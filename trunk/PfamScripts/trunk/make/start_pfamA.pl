#! /software/bin/perl -w

#Generic script to build pfams from fasta files or SEED alignments, or to iterate a family and perform overlap checks.

use strict;
use Getopt::Long;
use Cwd;
use Log::Log4perl qw(:easy);
use File::Copy;

use Bio::Pfam::AlignPfam;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

my ($directory, $list, $family, $fasta, $seed, $help, $max_iterations, $extend); 
my $seed_source = "";
my $author = "";


&GetOptions ( "directory=s"      => \$directory,
	      "list=s"           => \$list,
	      "family=s"         => \$family,
              "fasta=s"          => \$fasta,
              "seed=s"           => \$seed,
              "seed_source=s"    => \$seed_source,
	      "author=s"         => \$author,
              "max_iterations=i" => \$max_iterations,
	      "extend"           => \$extend,
              "help"             => \$help);



#See if input parameters are sensible
unless($directory or ($list and -s $list) or $family) {
    help();
}
if( ($directory and $list) or ($directory and $family) or ($list and $family) ) {
    $logger->logdie("You cannot choose more than one of the following options:-directory, -file, -family");
}
if($fasta and $seed) {
    $logger->logdie("You cannot use the -fasta and -SEED option together");
}


unless($max_iterations) {
  $max_iterations = 2;
}


my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{$config->pfamlive} );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Prepare a statement for rdb query - this is used if the 'extend' option has been chosen
my $st = $dbh->prepare("select seq_start, seq_end from pfamseq as p, pfamA_reg_full_significant as a where p.auto_pfamseq = a.auto_pfamseq and p.pfamseq_acc = ? and in_full = \"1\" "); 

my $cwd = getcwd;


#Get list of directories to work on
my @dirs;
if($directory) {
    opendir(DIR, $directory) or $logger->logdie("Can't open dir $directory $!");
    @dirs = grep { ! /^\./ } readdir DIR;  
    closedir DIR;
}
elsif($list) {
    open(LIST, $list) or $logger->logdie("Couldn't open $list $!");
    while(<LIST>) {
	if(/^(\S+)/) {
	    push(@dirs, $1);
	}
    }
    close LIST;
}
else {
    push(@dirs, $family);
}

#Make the Overlaps and Built directories if need be
mkdir("Overlaps", 0775) unless(-d "Overlaps") ; 
mkdir("Built", 0775) unless(-d "Built") ;


#Loop through each dir and either do a pfbuild or move dir to Overlaps or Built directory
foreach my $dir (@dirs) {
    chdir "$cwd" or $logger->logdie("Couldn't chdir to $cwd $!");

    next if($dir eq "Overlaps" or $dir eq "Built");
    next unless(-d $dir);

    $logger->info("Working on $dir");

    chdir "$dir" or $logger->logdie("Couldn't chdir to $dir $!");

    if(-s "ALIGN" and -s "PFAMOUT" and (-M "PFAMOUT" > -M "ALIGN") ) {
	iterate($dir, $cwd, $seed_source, $author, $extend);
    }
    elsif($fasta) { 
          unless(-s $fasta)  {
              $fasta = grep_file($fasta);
          }
	  unless(-s $fasta) {
              $logger->info("No fasta file [$dir/$fasta] - skipping family\n");
              next;
	  }
	  print "[$dir, $fasta]\n";
          create_seed($dir, $fasta);
          create_desc($author, $seed_source);

	  system("pfbuild.pl -withpfmake -makeEval 0.1");
    }
    elsif($seed) {    
          unless(-s "SEED")  {
              $seed = grep_file($seed);
              rename ($seed, "SEED");
          }
          unless(-s "SEED") {
	      $logger->info("No seed file [$dir/SEED], skipping family");
              next;
          }
	  create_desc($author, $seed_source);
	  system("pfbuild.pl -withpfmake -makeEval 0.1");

    }
    else {
	$logger->info("$dir looks like it hasn't finished searching");
    }
}


#################
#  Subroutines  #
#################


sub help {
print STDERR << "EOF";

This script can be used to build a Pfam-A family.  It can be run from
one of two starting points:

(1) a fasta file or a seed alignment - the script will run pfbuild on
    the seed alignment, or a seed alignment that it generates from a
    fasta file.

(2) a 'Pfam-A' like directory which contains a SEED, PFAMOUT and ALIGN
    file - the script will perform an overlap check and, if there are
    no overlaps, it will iterate the family by generating a new SEED
    file from the ALIGN file and running pfbuild.  If the 'extend'
    option is chosen, the script will try and extend the alignment as
    much as possible in both the N and C direction without allowing
    overlaps between the envelope co-odinates of any Pfam-A families.

    You should run the script multiple times on the same directories
    and eventually the script will move each directory into an
    'Overlaps' directory, or to a 'Built' directory, both of which
    will be created in the current working directory.  The family will
    need to undergo a set number of iterations (default of 2), in
    addition to having no overlaps, before it gets moved to the Built
    directory.  If there are no overlaps, and the iteration process
    has not increased the number of members in the ALIGN file, no
    further iterations will be performed and the directory is moved to
    the Built directory.  You should keep running the script
    periodically until all families are either in the 'Built' or
    'Overlaps' directory.

USAGE: 

     $0 (-directory <directory containing families> or -list <list file of directories> or -family <single directory>)

REQUIRED OPTIONS for starting point (1) (Choose one of the following):
----------------------------------------      

     -fasta  <fasta_file_of_multiple_sequences>     create the seed from this fasta file - give either the 
                                                    filename here if it is the same for each directory, or 
                                                    the extension of the sequence file if it is not.
                                                    (e.g. -fasta "seqs.fasta" or -fasta ".fasta")

     -seed <name of SEED file>                      use this file as SEED file - give either the 
                                                    filename here if it is the same for each directory, or 
                                                    the extension of the sequence file if it is not.
                                                    (e.g. -SEED "seqs.msa" or -SEED ".msa")


ADDITIONAL OPTIONS for starting point (1):
------------------------------------------
     -author <author_list>                          list of authors to put in the DESC file
     -seed_source <seed_source>                     seed source to put in the DESC file


EXAMPLE for starting point (1):
   $0 -directory . -fasta ".fa" -author "TIGRFAMs, Finn RD" -seed_source "TIGRFAMS (release 2.0)" 
 


ADDITIONAL OPTIONS for starting point (2):
------------------------------------------

     -author <author_list>                          list of authors to put in the DESC file
     -seed_source <seed_source>                     seed source to put in the DESC file
     -max_iterations <number>                       maximum number of iterations to run (default is 2)
     -extend                                        extend the alignment in both N and C directions as much as possible 
                                                    without allowing overlaps with any Pfam-A families

EXAMPLE for starting point (2):
   $0 -directory . -max_iterations 3 
  

EOF

exit (0);
}


sub iterate {
    my ($dir, $cwd, $seed_source, $author, $extend) = @_;

    my $num;

    edit_desc($seed_source, $author) if($seed_source or $author);

    chdir("../") or $logger->logdie("Couldnt chdir $!");

    my $familyIO = Bio::Pfam::FamilyIO->new;
    my $famObj = $familyIO->loadPfamAFromLocalFile($dir, $cwd);
    my %ignore;
    my $overlaps = &Bio::Pfam::PfamQC::family_overlaps_with_db( $dir, \%ignore, undef, $pfamDB, $famObj );

    chdir "$dir" or $logger->logdie("Couldn't chdir into $dir $!");


    if(! -e "ALIGN.iteration_number") {
        open(TMP, ">ALIGN.iteration_number");
        print TMP "1";
        close TMP;
        $num=1;
    }

    #If it overlaps move to Overlaps directory
    if($overlaps) {   
        chdir("../");
	$logger->info("Moving family $dir to Overlaps directory");
        rename ("$dir","Overlaps/$dir") or $logger->logdie("Cannot move $dir to Overlaps/$dir $!");
        return;
    }
    
     #iterate family 
     unless($num) {
        open(TMP, "ALIGN.iteration_number");
        while(<TMP>) {
	    if(/(\d+)/) {
                $num = $1;
	    }
	}
        close TMP;
     }

     if($num>= $max_iterations) {  
         chdir "../";
         rename ("$dir","Built/$dir") or $logger->logdie("Cannot move $dir to Built/$dir $!");
         return;
     }

     unless(-s "ALIGN.lastIteration" and -M "ALIGN" < -M "ALIGN.lastIteration") {  #ALIGN will be younger than ALIGN.lastIteration if pfbuild failed
       $num++;
       unlink "ALIGN.iteration_number";
       open(TMP, ">ALIGN.iteration_number");
       print TMP "$num";
       close TMP;
     }
                    
     #Count members in this iteration and last iteration
     my ($wc_old, $wc_new) = ("0", "0");          
     if(-e "ALIGN.lastIteration") {
         $wc_old = word_count("ALIGN.lastIteration");
     }
     $wc_new = word_count("ALIGN");

     #Only iterate if new align file has more members than last iteration or only done 1 iteration              
     unless($num == "1" or $wc_old < $wc_new) { 
         $logger->info("Moving family $dir to Built directory"); 
         chdir "../";
         rename ("$dir","Built/$dir") or $logger->logdie("Cannot move $dir to Built/$dir $!");
         return;
     }

    copy("ALIGN","ALIGN.lastIteration") or $logger->logdie("Copying ALIGN failed $!");

    system("belvu -n 80 -o mul ALIGN | grep -v '//' > ALIGN.$$");  #Make 80% non-redundant


    if($extend) {
	create_extended_seed($dir, "ALIGN.$$", $cwd);
    }
    else {
	create_seed($dir, "ALIGN.$$");
    }

    unlink "ALIGN.$$";

    system("pfbuild.pl -withpfmake -makeEval 0.1");
}




sub create_seed {
    my ($dir, $fa) = @_;

    unless(-s $fa) {
        print STDERR "No fasta or alignment file [$dir/$fa] - skipping\n";
        return;
    }    

    system("create_alignment.pl -fasta $fa -mu | belvu -o mul - > aln.$$") and $logger->logdie("Couldn't run create_alignment $!");
    
    open(ALN, "aln.$$") or $logger->logdie("Couldn't open aln.$$ $!");
    my $aln = new Bio::Pfam::AlignPfam;
    while(<ALN>) {
	if( /^(\S+)\/(\d+)-(\d+)\s+(\S+)\s*/ ) {
	    my $name = $1;
	    my $start = $2;
	    my $end = $3;
	    my $seq = $4;
	    

            $aln->add_seq(Bio::Pfam::SeqPfam->new('-seq'=>$seq,
                                                  '-id'=>$name,
                                                  '-start'=>$start,
                                                  '-end'=>$end,
                                                  '-type'=>'aligned'));


	}
    }                 
    close ALN;

    my @columns = $aln->first_last_ungapped_column(20);
    $aln = $aln->trimmed_alignment($columns[0], $columns[-1]);

    unlink "SEED" if(-e "SEED");

    open(SEED, ">SEED.$$") or $logger->logdie("Couldn't open SEED.$$ for writing $!");
    $aln->write_Pfam(\*SEED);
    close SEED;

    #Make 80% non-redundant and remove partial seq 
    system("belvu -P -n 80 -o mul SEED.$$ > SEED");

    unlink "aln.$$";
    unlink "SEED.$$";
}


sub create_extended_seed {
    my ($dir, $fa, $cwd) = @_;

    unless(-s $fa) {
        print STDERR "No fasta or alignment file [$dir/$fa] - skipping\n";
        return;
    }    

    my $familyIO = Bio::Pfam::FamilyIO->new;
    my $famObj = $familyIO->loadPfamAFromLocalFile($dir, $cwd);


    my $n_ext=0;
    my $c_ext=0;
    my $n_seq ="";
    my $c_seq =""; 

    foreach my $seq ($famObj->ALIGN->each_seq) {
		
	$st->execute($seq->display_id) or die "Couldn't execute statement ".$st->errstr."\n";

	my $array_ref = $st->fetchall_arrayref();
	
	foreach my $row (@$array_ref) {
	    my ($start, $end) = ($$row[0], $$row[1]);
	    
	    if($end <= $seq->start) {
		my $tmp = $seq->start - $end - 1;
		if(!$n_ext or $tmp < $n_ext) {
		    $n_ext = $tmp;
		    $n_seq = $seq->display_id;
		}
	    }
	    elsif($start >= $seq->end) {
		my $tmp = $start - $seq->end - 1;
		if(!$c_ext or $tmp < $c_ext) {
		    $c_ext = $tmp;
		    $c_seq = $seq->display_id;
		}
	    }
	}
    }

  
    if($n_ext) {
	$logger->info("Extending alignment by $n_ext ($n_seq) residues at N terminal");
    }
    if($c_ext) {
	$logger->info("Extending alignment by $c_ext ($c_seq) residues at C terminal");
    }

    my $wholeseq;
    unless($n_ext or $c_ext) {
	$logger->info("No Pfam-A families found on sequences in ALIGN file, running wholeseq on alignment");
	$wholeseq=1;
    }

    if($wholeseq) {   
	system("wholeseq.pl -align $fa -mu > aln.$$") and $logger->logdie("Couldn't run wholeseq.pl $!");
    }
    else {
	system("extend.pl -n $n_ext -c $c_ext -align $fa -mu > aln.$$") and $logger->logdie("Couldn't run extend.pl $!");
    }

    open(ALN, "aln.$$") or $logger->logdie("Couldn't open aln.$$ $!");
    my $aln = new Bio::Pfam::AlignPfam;
    while(<ALN>) {
	if( /^(\S+)\/(\d+)-(\d+)\s+(\S+)\s*/ ) {
	    my $name = $1;
	    my $start = $2;
	    my $end = $3;
	    my $seq = $4;
	    

            $aln->add_seq(Bio::Pfam::SeqPfam->new('-seq'=>$seq,
                                                  '-id'=>$name,
                                                  '-start'=>$start,
                                                  '-end'=>$end,
                                                  '-type'=>'aligned'));


	}
    }                 
    close ALN;

    my @columns = $aln->first_last_ungapped_column(20);
    $aln = $aln->trimmed_alignment($columns[0], $columns[-1]);

    unlink "SEED" if(-e "SEED");

    open(SEED, ">SEED.$$") or $logger->logdie("Couldn't open SEED.$$ for writing $!");
    $aln->write_Pfam(\*SEED);
    close SEED;

    #Make 80% non-redundant and remove partial seq 
    system("belvu -P -n 80 -o mul SEED.$$ > SEED");

    unlink "aln.$$";
    unlink "SEED.$$";
}




sub create_desc {
    my ($seed_source, $author) = @_;

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



sub grep_file {
    my $ext = shift;

    my $f;
    opendir(D, ".") or $logger->logdie("Couldn't open directory $!");
    my @files = grep {$_ =~ /$ext$/} readdir(D);
    closedir(D);
    unless(scalar(@files)) { 
	print STDERR "No files with $ext extension\n";
        return;
    }
    unless(@files == "1") {
       print STDERR "More than one file with $ext extension, going to use $files[0] to build SEED\n";      
    }
    $f = $files[0];
    return $f;
}



sub word_count {
    my $file = shift;

    open(WC, "wc -l $file |") or $logger->logdie("Couldn't run wc on $file $!");
    my $wc = <WC>;
    $wc =~ /(\d+)/;
    $wc = $1;
    close WC;

    return $wc;
}


sub edit_desc {

    my ($seed_source, $author) = @_;


    rename("DESC", "DESC.$$") or $logger->logdie("Couldn't rename DESC $!");

    open(OLDDESC, "DESC.$$") or $logger->logdie("Couldn't open DESC.$$ file $!");
    open(DESC, ">DESC") or $logger->logdie("Couldn't open DESC file $!");
    while(<OLDDESC>) {
	if(/^SE/ and $seed) {
	    print DESC "SE   $seed_source\n";
	}
	elsif(/^AU/ and $author) {
	    print DESC "AU   $author\n";
	}
	else {
	    print DESC $_;
	}
    }
    close DESC;
    close OLDDESC;


}
