#!/software/bin/perl

use strict;
use warnings;

use Getopt::Long;

my $CMALIGN = '/software/rfam/bin/cmalign';
my $NUM_CPUS = 2;

my ( $acc, $data_dir, $fasta_filename, $help, $mpi, $tmp_dir, $verbose );

GetOptions( 
  'acc=s'    => \$acc,
  'data=s'   => \$data_dir,
  'h'        => \$help,
  'in=s'     => \$fasta_filename,
  'mpi:i'    => \$mpi,
  'tmpdir:s' => \$tmp_dir,
  'v'        => \$verbose
);

if ( $help ) {
  help();
  exit;
}

# make sure the Rfam accession is valid
die "ERROR: must supply an Rfam accession ('--acc')"
  unless $acc;
die "ERROR: not a valid Rfam accession ($acc)"
  unless $acc =~ m/^RF\d{5}$/;

# check the data dir
die "ERROR: must supply the location of the CM files ('--data')"
  unless $data_dir;
die "ERROR: couldn't find data directory ($data_dir)"
  unless -d $data_dir;

# check the tmp dir
die "ERROR: must supply the location of a temporary directory ('--tmpdir')"
  unless $tmp_dir;
die "ERROR: couldn't find temporary directory ($tmp_dir)"
  unless -d $tmp_dir;

# make sure we can find the sequence file
die "ERROR: must supply FASTA sequence file name ('--in')"
  unless $fasta_filename;
my $fasta_file = "$tmp_dir/$fasta_filename";
die "ERROR: couldn't find the FASTA-format file ($fasta_file)"
  unless -s $fasta_file;

# make sure we can find the CM file
my $cm = "$data_dir/$acc.Rfam.cm";
die "ERROR: couldn't find the CM for Rfam family '$acc'"
  unless -s $cm;

# should we use MPI ? If the "--mpi" flag is not give, $mpi will be undefined.
# In that case we don't use MPI. If "--mpi" is given but has no value, we use
# MPI and set the number of CPUs to the number hard-coded at the top of the
# script. If "--mpi" is given with an integer value, we use MPI with that
# specified number of CPUs.
my $cpus = $mpi if defined $mpi;
$cpus ||= $NUM_CPUS;
die "ERROR: must specify an integer number of CPUs to use (not '$cpus')"
  unless $cpus =~ m/^\d+$/;

# the command to run cmalign
my $cmd;
if ( defined $mpi ) {
  $cmd = "mpirun -n $cpus $CMALIGN --mpi -q $cm $fasta_file";
}
else {
  $cmd = "$CMALIGN -q $cm $fasta_file";
}

print STDERR "executing command: |$cmd|\n" if $verbose;

system( $cmd ) == 0
  or die "ERROR: failed to run cmalign command: $cmd";

exit;

sub help{
  print STDERR <<EOF_help;
  
usage:  $0 --acc <accession> --data <data dir> --in <fasta file> [--mpi [n]]

This script takes a FASTA-format file and an Rfam accession and aligns the
sequences to the family CM.

parameters:
  --acc | -a <accession>  Rfam family accession
  --data | -d <data dir>  directory containing individual Rfam CM files
  --in | -i <fasta file>  FASTA-format sequence file
  --mpi | -m <n>          flag indicating whether to use MPI. If an argument
                          is given, it's taken to be the number of CPUs to use
  
EOF_help
}

