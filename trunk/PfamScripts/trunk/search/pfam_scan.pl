#! /software/bin/perl -w

use strict;
use warnings;

use Bio::Pfam::Scan::PfamScan;
use Getopt::Long;

#Get the user options
my ( $outfile, $e_seq, $e_dom, $b_seq, $b_dom, $dir, $clan_overlap, $fasta, $align, $help, $as );
GetOptions( "outfile=s" => \$outfile,
            "e_seq=f"   => \$e_seq,
            "e_dom=f"   => \$e_dom,
            "b_seq=f"   => \$b_seq,
            "b_dom=f"   => \$b_dom,
            "dir=s"     => \$dir,
            "clan_overlap" => \$clan_overlap,
            "fasta=s"   => \$fasta,
            "align"     => \$align,
            "h"         => \$help,
            "as"        => \$as);

#Check the input parameters
die "FATAL: Can't find directory [$dir]\n" unless(-d $dir);
die "FATAL: Can't find file [$fasta]\n" unless(-s $fasta);
die "FATAL: Can't find Pfam-A.scan.dat in $dir\n" unless(-s "$dir/Pfam-A.scan.dat");
die "FATAL: Can't find Pfam_HMM and/or Pfam_HMM binaries in $dir\n" unless(-s "$dir/Pfam_HMM" and -s "$dir/Pfam_HMM.h3f" and -s "$dir/Pfam_HMM.h3i" and -s "$dir/Pfam_HMM.h3m" and -s "$dir/Pfam_HMM.h3p");
die "FATAL: Can't use e value and bit score threshold together" if( ($e_seq and ($b_seq||$b_dom)) or ($b_seq and ($e_seq||$e_dom)) or ($b_dom and $e_dom) );
    
my $ps = Bio::Pfam::Scan::PfamScan->new();

my $input = {
  -e_seq => $e_seq,
  -e_dom => $e_dom,
  -b_seq => $b_seq,
  -b_dom => $b_dom,
  -dir => $dir,
  -clan_overlap => $clan_overlap,
  -fasta => $fasta,
  -align => $align,
  -as => $as
};

$ps->search( $input );
$ps->write_results( $outfile );

exit(0);

#-------------------------------------------------------------------------------

=head2 help

  Title    : help
  Usage    : help()
  Function : Write usage of the script to STDERR
  Args     : None
  Returns  : Nothing, exits script
  
=cut

sub help {
  print STDERR <<EOF;

$0: search a fasta file against a library of Pfam HMMs

Usage: $0 -fasta <fasta_file> -dir <directory location of Pfam files>

Addional options:

        -h              : show this help
        -o <file>       : output file, otherwise send to STDOUT
        -clan_overlap   : show overlapping hits within clan member families
        -align          : show the HMM-sequence alignment for each match
        -e_seq <n>      : specify hmmscan evalue sequence cutoff (default Pfam definition)
        -e_dom <n>      : specify hmmscan evalue domain cutoff (default Pfam definition)
        -b_seq <n>      : specify hmmscan bit score sequence cutoff (default Pfam definition)
        -b_dom <n>      : specify hmmscan bit score domain cutoff (default Pfam definition)
        -as             : predict active site residues*

	*Active site residues are predicted using the method described in the publication: Mistry J, Bateman A, Finn RD.
	Predicting active site residue annotations in the Pfam database.   BMC Bioinformatics. 2007;8:298. PMID:17688688.



Output format is:
<seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc> <hmm start> <hmm end> <bit score> <E-value> <hmm name> <clan> <predicted_active_site_residues>

Example output (with -as option):

  Q16933.1     55    307     53    307  PF00069.1     3   260    231.8   5.3e-73  Pkinase          CL0016.9
  Q1JEP1.1     12    398     12    398  PF01640.5     1   394    746.6  3.9e-229  Peptidase_C10    CL0125.5   predicted_active_site[192,340]
  Q55U49.1    895   1146    895   1146  PF00621.1     1   180    118.4   4.2e-38  RhoGEF           No_clan
  Q55U49.1   1232   1309   1227   1310  PF00169.1    20   100      8.0   0.00046  PH               CL0266.5

EOF
  exit(1);

}

