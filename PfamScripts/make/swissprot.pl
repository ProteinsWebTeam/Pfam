#! /usr/bin/perl -w

use strict;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Cwd;
use Getopt::Long;

# Jiffy script that attempts to identify Swiss-Prot sequences from a PFAMOUT file
# You run it in the Pfam family directory itself and it will look for the PFAMOUT file
# It also createa a file called arch, with swiss-prot protein Pfam matches, and 
# some top scoring TrEBML ones if insufficient Swiss-Prot proteins are found

# To do
# Fetch regions for each protein and display in sp file and in STDOUT
# Add option to fetch all architectures


my $help;
my $n = 5;
GetOptions('num=i' => \$n,
  'help'  => \$help);

if($help) {
  help();
}
unless(-s "PFAMOUT" and -s "scores"){
  warn "You need to run this script in a Pfam family directory with a PFAMOUT and scores file\n";
  help();
}

unless($n>0) {
  warn "The num specified on the command line [$n] needs to be greater than 0\n";
  exit;
}

# Read scores file and only report matches!
my %id;
open (SCORES, "scores") or die "Cannot open scores file, $!";
while(<SCORES>){
  if (/^\S+\s+(\S+)\.\d+/){
    $id{$1}=1;
  }
}
close SCORES;


# Find if any swiss-prots are found in PFAMOUT and prepare sp file
my %accmap;
my %sp; # Stores sps found
my %top; # Stores accs of top n scoring proteins.  Will give architecture of these if no swiss-prot are available
my @order; #To keep order of proteins in PFAMOUT, used to supplement the swiss prot proteins with trembl ones if there aren't enough
my $i=0;
unlink "arch";
open (FH, "PFAMOUT") or die "Cannot open fh to PFAMOUT file, $!";
open (OUT, ">sp") or die "Cannot write to sp file, $!";
while(<FH>){
  if (/^(\S+)\.\d+\s+(\S+)_\S+\s+(.*)/){
    my $acc=$1;
    my $id=$2;
    my $desc=$3;

    if ($i<$n){
      push(@order, $acc);
      $top{$acc}=$desc;
    }
    if ($acc ne $id and $id{$acc}){
      $sp{$acc}=$desc;
      print;
      print OUT;
    }
    $i++;
  }
}
close FH;
close OUT;

my $found = keys %sp;

# retrieve swissprot seq_info into sp.seq_info file
my (@info, $all_ids);
print STDERR "Fetching seq_info for ". scalar(%sp) . " matches, writing to sp.seq_info file\n";
foreach my $id (keys %sp){

    select(undef, undef, undef, 0.25); # This command makes a 250ms pause. So that UniProt server is not overloaded.

    open(PFETCH, "wget -q -O - \"https://www.uniprot.org/uniprot/$id.txt\" |") or warn "Cannot wget $id\n";
    my $fail=0;
    while (<PFETCH>){
        if (/</){
            $fail=1;
            print STDERR "Failed to fetch $id\n";
            next; # Skip out of loop. This seems to be an xml response.
        }
        if ($_ =~ /^(  )\s{3}/){ # Remove sequence lines
            next;
        } elsif(/^CC   ---/){
            next;
        } elsif (/^CC   Copyrighted/){
            next;
        } elsif (/^CC   Distributed/){
            next;
        } elsif (/ProtNLM/){
            push(@info, $_);
            next;
        } elsif (/^DT/){
            next;
        } elsif (! $fail){
            push(@info, $_);
        }
    }
    close (PFETCH);
}

open (SIO, ">sp.seq_info") or die "Cannot write to sp.seq_info file, $!";

foreach (@info){
    chomp ($_);
    print SIO "$_\n";
}
close SIO;

# enrich with trembl and prepare arch file
if($found==0) {
  %sp=%top;
  $found = keys %top;
  print STDERR "Found 0 Swiss-Prot sequences, getting architectures for $found TrEMBL proteins\n";
}
elsif($found < $n) { #Supplement swiss prot seqs with trembl seqs if less than $n swiss prot seqs were found
  my $added=0;
  my $to_add = $n - $found;
  foreach my $acc (@order) {
    next if(exists($sp{$acc}));
    $sp{$acc} = $top{$acc};
    $added++;
    last if($added == $to_add);
  }
  print STDERR "Found $found Swiss-Prot sequences, supplementing architectures with $added TrEMBL sequences\n";
}
else {
  print STDERR "Found $found Swiss-Prot sequences\n";
}

# Get live mapping of Pfam ids and accs data
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my @pfamA=$pfamDB->getSchema->resultset('PfamA')->search();
foreach my $pfamA (@pfamA) {
  $accmap{$pfamA->pfama_acc}=$pfamA->pfama_id;
}

#Get current dir name
my $cwd = getcwd;
my $dir = pop @{[split m|/|, $cwd]};

write_arch_file(\%sp, $pfamDB, $dir);


sub write_arch_file {

  my ($hash, $pfamDB, $new_fam_name) = @_;

  $new_fam_name = sprintf("%-26s", $dir); #To make it line up with the other text

  # Loop over swiss-Prots and write to arch file
  open (ARCH, "> arch") or die "Cannot write to file arch, $!";

  foreach my $sp (sort keys %$hash){

    print ARCH "$sp\t$hash->{$sp}\n";

    # Get regions from PFAMOUT and add to arch file
    open (PFAMOUT, "PFAMOUT") or die "Cannot read PFAMOUT file, $!";
    while(<PFAMOUT>){
      if (/^$sp\.\d+\s+(\d+)\s+(\d+)\s+\d+\s+\d+\s+\d+\s+\d+\s+(\S+)/){
        my ($st, $en, $bits) = ($1, $2, $3);
        $st = sprintf("%6s", $st);
        $en = sprintf("%6s", $en);
        $bits = sprintf("%8s", $bits);

        print ARCH "\t$new_fam_name\t$st\t$en\t$bits\n";
      }
    }
    close PFAMOUT;

    #Get regions from pfam_live
    my @reg_full=$pfamDB->getSchema->resultset('PfamARegFullSignificant')->search({ pfamseq_acc => $sp }, {order_by => 'seq_start ASC'});
    foreach my $row (@reg_full) {
      my $pfamA_id = $accmap{$row->pfama_acc->pfama_acc};
      $pfamA_id = sprintf("%-16s", $pfamA_id);

      my ($st, $en, $bits) = ($row->seq_start, $row->seq_end, $row->domain_bits_score);

      $st = sprintf("%6s", $st); #Allow for the maximum start/end in the db
      $en = sprintf("%6s", $en);
      $bits = sprintf("%8s", $bits); #Allows for the maximum bits score in the db

      print ARCH "\t".$row->pfama_acc->pfama_acc."\t$pfamA_id\t$st\t$en\t$bits\n";
    }
    print ARCH "\n";
  }
  close ARCH;
}

sub help {
  print<<EOF;

This script finds Swiss-Prot sequences in a PFAMOUT file. It requires a 
PFAMOUT and scores files in the current working directory. The script 
creates three files: sp, sp.seq_info and arch. sp contains the Swiss-Prot matches,
while sp.seq_info contains the full UniProt flatfile record for those matches.
The arch file contains the architectures of the Swiss-Prot Pfam matches. If no
Swiss-Prot matches are found, or if the number of Swiss-Prot matches is 
less than num (default num=5), the arch file will be supplemented with 
top scoring TrEMBL sequences until it contains num sequences. For example if 
num=5, and 3 Swiss-Prot sequences are found, the 2 highest scoring TrEMBL 
proteins will be added to the arch file to make it up to 5. The value of
num can be set on the command line (see below).

usage:

$0

Options:
  -num <integer>: If less than num Swiss-Prot proteins are found, the 
                  total number of proteins (Swiss-Prot + TrEMBL) to add 
                  to the arch file (default=5)
  -help         : Shows this help
EOF

exit;
}
