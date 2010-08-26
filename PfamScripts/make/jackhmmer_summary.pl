#! /software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Log::Log4perl qw(:easy);


#Script to loop over each directory of Jackhmmer searches and
#create a file called summary.txt in each direcotory with the
#following info:
#
#seq_id  totalseq:     <num seq>  overlaps:      <num overlaps>  Gain:      <gain>  Frac gained: <frac gained> <List of overlapping fam>
#
#The script concatenates all these files together in a file
#called alldata in the cwd.  It is intended for use after running
#the proteome_jackhmmer.pl script.



#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

my $directory;

&GetOptions ( "directory=s"  => \$directory);

unless($directory) {
    print STDERR "Need to specify directory (-directoy <dir>) to work from\n";
    &help;
}


#Get clan data
my %fam2clan;
clan_mapping(\%fam2clan);

my @dirs;
opendir(DIR, $directory) or $logger->logdie("Can't open dir $directory $!");
@dirs = grep { ! /^\./ } readdir DIR;  
closedir DIR;


#Loopt through dir and write file
foreach my $dir (@dirs) {
    my $full_dir = "$directory/$dir";   

    next unless(-d $full_dir);
    next if(-s "$full_dir/summary.txt");
    next if($full_dir =~ /Done$/);
   
    unless(-s "$full_dir/align") {
	$logger->info("Skipping $full_dir (no align file)");
	next;
    }

    $logger->info("Doing $full_dir");

    open(ALN, "$full_dir/align") or $logger->logdie("Couldn't open $full_dir/align, $!");
    my %align;

    while(<ALN>) {
	if(/^(\S+)\//) {
	    $align{$1}=1;
	}
    }
    close ALN;

    my $align = keys %align;

    next if($align eq 0); #No homologues found

    open(OVERLAP, "$full_dir/overlap") or $logger->logdie("Couldn't open $full_dir/overlap, $!");
    my %overlap;
    my %clan;
    my %fam;
    my %tmp;
    while(<OVERLAP>) {

	if(/\S+\s+\[(\S+)\]\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s+(\S+)/) {
	    my ($seq, $fam) = ($1, $2);

	    $overlap{$seq}=1;
	    next if(exists($tmp{"$seq:$fam"})); #Don't want to count same overlapping seq twice for a family
	    $fam{$fam}++;
	    $tmp{"$seq:$fam"}=1;
	}
    }
    close OVERLAP;
    
    foreach my $f (keys %fam) {
	my $c = $fam2clan{$f};
	$c = "No_clan" unless($c);
	$clan{$c} .= " $f($fam{$f})";
    }

    my $clan_fam;
    foreach my $clan (keys %clan) {
	$clan_fam .= $clan . ":" . $clan{$clan} . ", ";
    }

    if($clan_fam) {
	chop $clan_fam;#Remove final ", "
	chop $clan_fam;
    }
	
    my $overlap = keys %overlap;
    
    my $gain = $align - $overlap;

    my $frac_gain = $gain/$align;
    $frac_gain = sprintf("%.2f", $frac_gain);


    open(OUT, ">$full_dir/summary.txt") or $logger->logdie("Couldn't open fh to $full_dir/summary.txt, $!");

    print OUT sprintf ("%6s totalseq:%7s  overlaps:%7s  Gain: %7s  Frac gained:%4s  ", $dir, $align, $overlap, $gain, $frac_gain);

    if($clan_fam) {
	print OUT "$clan_fam\n";
    }
    else {
	print OUT "\n";
    }

    close OUT;
}

#Sort by overlaps, then by number gained
system("cat $directory/*/summary.txt | sort -k5n -k7nr > alldata") and die "Couldnt run |cat $directory/*/summary.txt | sort -k5n -k7nr > alldata| $!";





sub clan_mapping {

    my $hash = shift;
    $logger->info("Getting clan membership from rdb");
    my $config = Bio::Pfam::Config->new;

    my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

    my $array_ref = $pfamDB->getAllClanData;
    
    foreach my $clan (@$array_ref){
        my $membership_ref = $pfamDB->getClanMembership($clan->clan_acc);
        foreach my $pfamA (@$membership_ref){               
            $$hash{$pfamA->auto_pfama->pfama_id}=$clan->clan_acc;
        }
    }
    $logger->info("Got clan membership");
}


sub help {

print STDERR << "EOF";

This program should be run after proteome_jackhmmer.pl.  It loops over
each directory of Jackhmmer searches and creates a file called
summary.txt in each directory with the following info:

seq_id  totalseq:     <num seq>  overlaps:      <num overlaps>  Gain:      <gain>  Frac gained: <frac gained> <List of overlapping fam>

The script concatenates all these files together in a file called
alldata in the cwd.

Usage:
  $0 -directory <directory containing Jackhmmer searches>

EOF

exit (0);
}
