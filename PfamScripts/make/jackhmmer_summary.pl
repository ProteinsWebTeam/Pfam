#! /usr/bin/env perl 

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::CompositionalBias;
use LWP::UserAgent;
use HTTP::Request::Common;
use Log::Log4perl qw(:easy);


#Script to loop over each directory of Jackhmmer searches and
#create a file called summary.txt in each direcotory with the
#following info:
#
#seq_id  totalseq:     <num seq>  overlaps:      <num overlaps>  Gain:      <gain>  Frac gained: <frac gained>  Bias: <bias><List of overlapping fam>
#
#totalseq=number of sequences (not domains) in the align file
#overlaps=number of sequences (not domains) in the overlaps file
#gain=total_seq-overlaps
#frac gained=gain/totalseq*100
#bias=proportion (%) of residues that are predicted to be low complexity or disordered
#
#The list of overlapping families/clans is given if overlaps are present
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

  my $aln;
  if(-e "$full_dir/align") {
    $aln="align";
  }
  else {
    $aln="ALIGN";
  }

  unless(-s "$full_dir/$aln") {
    $logger->info("Skipping $full_dir (no $aln file)");
    next;
  }

  $logger->info("Doing $full_dir");

  open(ALN, "$full_dir/$aln") or $logger->logdie("Couldn't open $full_dir/aln, $!");
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

    next if(/SEED/);

    #Sequence [Q87XD2] overlap DUF4414 PF14377/44-57 (PF14377/44-57, 22.9 bits) FULL with Cys_rich_CWC PF14375/16-67 (PF14375/16-68, 52.80 bits) FULL
    #Sequence [A0A0A1T2E8] overlap NEW NEW/110-227 JALIGN with HsbA PF12296/26-141 (PF12296/25-142, 68.40 bits) FULL
    if(/\S+\s++\[(\S+)\]\s+\S+\s+\S+\s+\S+ JALIGN\s\S+\s+(\S+)/ or /\S+\s+\[(\S+)\]\s+\S+\s+\S+\s+\S+\s+\S+\s+\S+\s\S+\s+\S+\s+\S+\s+(\S+)/) {
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


  my $prop_bias;
  if(-s "$full_dir/prop_bias") {
    open(BIAS, "$full_dir/prop_bias") or die "Couldn't open fh to $full_dir/prop_bias, $!";
    while(<BIAS>) {
      if(/^(\S+)/) {
        $prop_bias=$1;
        last;
      }
    }
    close BIAS;
  }
  else {
    $prop_bias = Bio::Pfam::CompositionalBias::calculate_compositional_bias("$full_dir/$aln");
  }
  $prop_bias= sprintf("%.1f", $prop_bias);

  open(OUT, ">$full_dir/summary.txt") or $logger->logdie("Couldn't open fh to $full_dir/summary.txt, $!");

  print OUT sprintf ("%-10s totalseq:%7s  overlaps:%7s  Gain: %7s  Frac gained: %4s  Bias: %4s  ", $dir, $align, $overlap, $gain, $frac_gain, $prop_bias);

  if($clan_fam) {
    print OUT "$clan_fam\n";
  }
  else {
    print OUT "\n";
  }

  close OUT;
}

#Print header 
open(ALL, ">alldata") or die "Coudln't open fh to alldata, $!";
print ALL "#File is sorted by overlaps, then by number gained\n";
print ALL "#Only squences with at least one match in the sequence database are included in this file\n";
print ALL "#totalseq=number of sequences (not domains) in the align file\n";
print ALL "#overlaps=number of sequences (not domains) in the overlaps file\n";
print ALL "#gain=total_seq-overlaps\n";
print ALL "#frac gained=gain/totalseq\n";
print ALL "#bias=proportion (%) of residues in align file predicted to be disordered or low complexity\n";
print ALL "#Overlapping clans and families are given at the end of the line (if any)\n";
close ALL;

#Sort by overlaps, then by number gained
system("cat $directory/*/summary.txt | sort -k5n -k7nr >> alldata") and die "Couldnt run |cat $directory/*/summary.txt | sort -k5n -k7nr >> alldata| $!";





sub clan_mapping {

  my $hash = shift;

  my $config = Bio::Pfam::Config->new;

  if($config->location eq 'EBI') {
    $logger->info("Getting clan membership from rdb");
    my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

    my $array_ref = $pfamDB->getAllClanData;

    foreach my $clan (@$array_ref){
      my $membership_ref = $pfamDB->getClanMembership($clan->clan_acc);
      foreach my $pfamA (@$membership_ref){
        $hash->{$pfamA->pfama_acc->pfama_id}=$clan->clan_acc;
      }
    }
  }
  else {
    #Need to use cgi script to get clan info
    $logger->info("Getting clan membership from Pfam database");

    # Create a user agent object
    my $ua = LWP::UserAgent->new;
    $ua->agent("JackhmmerSummary/0.1 ");

    #Set any proxy
    if ( $config->proxy and $config->proxy =~ /http/ ) {
      print STDERR "Setting proxy\n";
      $ua->proxy( [ 'http', 'ftp', 'https' ], $config->proxy );
    }
    elsif ( $ENV{http_proxy} ) {
      $ua->env_proxy;
    }

    #A little insurance
    $ua->timeout(600);

    #Pass request to the user agent and get a response back
    my $res = $ua->request(
      POST 'https://xfamsvn.ebi.ac.uk/cgi-bin/clan_mapping.cgi',
      Content      => []
    );

    if ( $res->is_success ) {
      foreach my $l (split(/\n/, $res->content)){
        if($l =~ /(\S+)\s+(\S+)/) {
          $hash->{$1}=$2;
        }
      }
    }
    else {
      print $res->status_line, "\n";
    }
  }
  $logger->info("Got clan membership");
}

sub help {

  print STDERR << "EOF";

This program should be run after proteome_jackhmmer.pl.  It loops over
each directory of Jackhmmer searches and creates a file called
summary.txt in each directory with the following info:

seq_id  totalseq:     <num seq>  overlaps:      <num overlaps>  Gain:      <gain>  Frac gained: <frac gained>  Bias:  <bias> <List of overlapping fam>

The script concatenates all these files together in a file called
alldata in the cwd.

Usage:
  $0 -directory <directory containing Jackhmmer searches>

EOF

  exit (0);
}
