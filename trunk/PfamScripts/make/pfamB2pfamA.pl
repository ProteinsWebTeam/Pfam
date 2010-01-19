#! /software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::Config;
use Log::Log4perl qw(:easy);
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

#A script to build pfamA families from pfamB families.


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

my ($from, $to, $rel, $no_pfamA, $author, $help);


my $rel_dir = "/lustre/pfam/pfam/Production/Pfam/RELEASES/";
my $archive = "/lustre/pfam/pfam/Archive/Pfam/RELEASES/";


&GetOptions('from=i' => \$from,
	    'to=i' => \$to,
	    "rel=s" => \$rel,
	    "no_pfamA=i" => \$no_pfamA,
	    "author=s" => \$author,
	    "help" => \$help);


#Check input parameters are sensible
unless( ( ($from and $to and ($from <= $to)) or ($no_pfamA) ) and $rel) {
    help()
}

if($no_pfamA) {
    if($from and $to and $from > $to) {
	help();
    }
    $from = "0" unless($from);
    $to = "1000000000" unless($to);
}

unless($rel =~ /\.0$/) {
    $rel .= ".0";
}

#Check the Pfam-B file exists
my $pfamB_file;
if(-s "$rel_dir/$rel/Pfam-B") {
    $pfamB_file = "$rel_dir/$rel/Pfam-B";
}
elsif(-s "$archive/$rel/Pfam-B") {
    $pfamB_file = "$archive/$rel/Pfam-B";
}
else {
  $logger->logdie ("Can't find Pfam-B file, it's not in $rel_dir/$rel/ or $archive/$rel/ !");
}


#If no_pfamA option has been chosen, query rdb to find out which Pfam-B families have not associated Pfam-A families
my %no_pfamA;
if($no_pfamA) {
    no_pfamA(\%no_pfamA);
}   


#Start building!
my $c;
$/="//\n";
open(PFAMB, $pfamB_file) or $logger->logdie("Couldn't open $pfamB_file $!");
while(<PFAMB>) {

    if(/\#\=GF\s+AC\s+(PB(\d\d\d\d\d\d))/) {
	my $pfamb_num = $1;
	my $num = $2;

	if($no_pfamA) {
	    next unless(exists($no_pfamA{$pfamb_num}));
	}
 
	$num = int($num);


	next if(-d $num);

	next if($num < $from);

	last if($num > $to);

	$logger->info("Building Pfam-B_$num");

        my @lines = split(/\n/, $_);


	mkdir($num, 0775) or $logger->logdie("Couldn't make dir $num $!");
	chdir($num) or $logger->logdie("Couldn't chdir into $num $!");

	create_desc($author, "Pfam-B_$num (release $rel)");

	my $original_seed = "SEED.original";

	open(SEED, ">$original_seed") or $logger->logdie("Couldn't open $original_seed $!");

	foreach my $line (@lines) {
	    unless($line =~ /^\#/) {
		print SEED "$line\n";
	    }
	}
	close SEED;

	tidy_seed($original_seed);

	system("pfbuild -withpfmake -makeEval 0.1") and $logger->logdie("Error running pfbuild");

	chdir("../") or $logger->logdie("Couldn't chdir up, $!");

	if($no_pfamA) {
            $c++;
	    last if($c >= $no_pfamA);
	}

    }
}
close PFAMB;


sub help {

   print STDERR <<EOF;
A script to build pfamA families from pfamB families.

Usage: 

   To build from pfamB family x to pfamB family y:
   -----------------------------------------------

      $0 -from <x> -to <y> -rel <num> 
      e.g. $0 -from 1 -to 100 -rel 24.0


   or to build x pfamB families which have no pfamA families:
   ----------------------------------------------------------

     $0 -no_pfamA <x> -rel <num>  (-from and -to flags are optional)
     e.g. $0 -no_pfamA 100 -rel 24.0

Additonal options:

  -h                      : show this help
  -author <author list>   : author name to put in DESC file
  e.g. $0 -no_pfamA 100 -rel 24.0 -author "Mistry J"

EOF
  exit;


}



sub create_desc {

    local $/="\n";

    my ($author, $seed_source) = @_;
    $author = "Who RU" unless($author);
    $seed_source = "Where did the seed come from" unless($seed_source);

    my $io = Bio::Pfam::FamilyIO->new;
 

  
    my %desc = ( DE    => "Family description",
                 AU    => $author,
                 SE    => $seed_source,
                 CUTGA => { seq => "27.00", dom => "27.00" },
                 CUTNC => { seq => "27.00", dom => "27.00" },
                 CUTTC => { seq => "27.00", dom => "27.00" },
                 BM    => "hmmbuild  -o /dev/null HMM SEED",
                 SM    => "hmmsearch -Z ".$io->{config}->dbsize." -E 1000 HMM pfamseq",
                 TP    => 'Family' );


    my $desc = Bio::Pfam::Family::DESC->new(%desc);
    $io->writeDESC($desc);

}


sub no_pfamA {
    my ($hash) = @_;

    #Query rdb for all sequences in pfamB families that have no pfamA match
    $logger->info("Running the query on pfamlive to find out which pfamB families contain only sequences that have no pfamA matches (this usually takes approximately 35 minutes to run)");

    my $config = Bio::Pfam::Config->new;
    my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
    
    my $dbh = $pfamDB->getSchema->storage->dbh;


    #This query takes approximately 35 minutes to run
    my $sth = $dbh->prepare("select pfamB_acc, number_regions from (pfamB_reg, pfamB) left join pfamA_reg_full_significant on pfamB_reg.auto_pfamseq = pfamA_reg_full_significant.auto_pfamseq where auto_pfamA is NULL and pfamB_reg.auto_pfamB = pfamB.auto_pfamB");
    
    $sth->execute or $logger->logdie("Failed to query rdb for pfamB data ".$sth->errstr."\n");
    
    my $array_ref = $sth->fetchall_arrayref();
    

    $logger->info("Finished running query");
    
    
    #Put data in hash
    my %pfamB;
    foreach my $element (@$array_ref) {
	my ($pfamB_acc, $number_reg) = ($$element[0], $$element[1]);
	$pfamB{$pfamB_acc}{"number_reg"}=$number_reg;
	$pfamB{$pfamB_acc}{"non_overlap"}+=1;
    }
    

    
    #Identify which pfamBs are made purely of sequences which have no pfamA regions
    foreach my $pfamB_acc (keys %pfamB) {
	if($pfamB{$pfamB_acc}{"number_reg"} == $pfamB{$pfamB_acc}{"non_overlap"} ) {
	    $$hash{$pfamB_acc}=1;
	}
    }
    
}




sub tidy_seed {

    my ($oldseed) = @_;

    local $/ = "\n";

    system("create_alignment.pl -fasta $oldseed -m > aln.$$") and $logger->logdie("Couldn't run create_alignment.pl on $oldseed $!");

    open(ALN, "aln.$$") or die "Couldn't open aln.$$ $!";
    my $aln = new Bio::Pfam::AlignPfam;
    while(<ALN>) {
        if( /^(\S+)\/(\d+)-(\d+)\s+(\S+)\s*/ ) {
            my ($name, $start, $end, $seq) = ($1, $2, $3, $4);

            $aln->add_seq(Bio::Pfam::SeqPfam->new('-seq'=>$seq,
                                                  '-id'=>$name,
                                                  '-start'=>$start,
                                                  '-end'=>$end,
                                                  '-type'=>'aligned'));


        }
    }
    close ALN;

    #Trim alignment
    my @columns = $aln->first_last_ungapped_column(20);   
    $aln = $aln->trimmed_alignment($columns[0], $columns[-1]);

    open(SEED, ">SEED.$$") or $logger->logdie("Couldn't open SEED.$$ for writing $!");
    $aln->write_Pfam(\*SEED);
    close SEED;

    #Make 80% non-redundant and remove partial seq 
    system("belvu -n 80 -o mul SEED.$$ > SEED");

    unlink "aln.$$";
    unlink "SEED.$$";
}
