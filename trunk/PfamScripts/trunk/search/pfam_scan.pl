#!/software/bin/perl

# This is pfam_scan.pl version 0.4a

# $Id: pfam_scan.pl,v 1.17 2009-06-05 15:36:27 jm14 Exp $


use strict;
use warnings;

use Bio::Pfam::Scan::PfamScan;
use Getopt::Long;

$|=1;


# get the user options
my ( $outfile, $e_seq, $e_dom, $b_seq, $b_dom, $dir, 
     $clan_overlap, $fasta, $align, $help, $as );
GetOptions( 'help'         => \$help,
            'outfile=s'    => \$outfile,
            'e_seq=f'      => \$e_seq,
            'e_dom=f'      => \$e_dom,
            'b_seq=f'      => \$b_seq,
            'b_dom=f'      => \$b_dom,
            'dir=s'        => \$dir,
            'clan_overlap' => \$clan_overlap,
            'fasta=s'      => \$fasta,
            'align'        => \$align,
            'h'            => \$help,
            'as'           => \$as );



help() if($help);
help() unless($dir and $fasta);


# check the input parameters
die qq(FATAL: must specify both "-dir" and "-fasta")
  unless ( defined $dir and defined $fasta );

die qq(FATAL: can't find directory "$dir")
  unless -d $dir;

die qq(FATAL: can't find file "$fasta")
  unless -s $fasta;

die qq(FATAL: can't find "Pfam-A.scan.dat" in "$dir")
  unless -s "$dir/Pfam-A.scan.dat";

die qq(FATAL: can't find "Pfam_HMM" and/or Pfam_HMM binaries in "$dir")
  unless ( -s "$dir/Pfam_HMM" and 
           -s "$dir/Pfam_HMM.h3f" and
           -s "$dir/Pfam_HMM.h3i" and
           -s "$dir/Pfam_HMM.h3m" and
           -s "$dir/Pfam_HMM.h3p" );

die qq(FATAL: can\'t use E-value and bit score threshold together)
  if ( ( $e_seq and ( $b_seq or $b_dom ) ) or 
       ( $b_seq and ( $e_seq or $e_dom ) ) or 
       ( $b_dom and $e_dom ) );



die qq(FATAL: outfile "$outfile" already exists)
    if($outfile and -s $outfile);


if($as) {
    die qq(FATAL: can\'t find "active_site.dat" in "$dir")
        unless( -s "$dir/active_site.dat");
}


# build the object
my $ps = Bio::Pfam::Scan::PfamScan->new(
  -e_seq        => $e_seq,
  -e_dom        => $e_dom,
  -b_seq        => $b_seq,
  -b_dom        => $b_dom,
  -dir          => $dir,
  -clan_overlap => $clan_overlap,
  -fasta        => $fasta,
  -align        => $align,
  -as           => $as,
);

# run the search
$ps->search;

# print the results
$ps->write_results( $outfile, $e_seq, $e_dom, $b_seq, $b_dom );

exit;

#-------------------------------------------------------------------------------

sub help {
  print STDERR <<EOF;

pfam_scan.pl: search a FASTA file against a library of Pfam HMMs

Usage: pfam_scan.pl -fasta <fasta_file> -dir <directory location of Pfam files>

Addional options:

  -h              : show this help
  -o <file>       : output file, otherwise send to STDOUT
  -clan_overlap   : show overlapping hits within clan member families
  -align          : show the HMM-sequence alignment for each match
  -e_seq <n>      : specify hmmscan evalue sequence cutoff (default Pfam defined)
  -e_dom <n>      : specify hmmscan evalue domain cutoff (default Pfam defined)
  -b_seq <n>      : specify hmmscan bit score sequence cutoff (default Pfam defined)
  -b_dom <n>      : specify hmmscan bit score domain cutoff (default Pfam defined)
  -as             : predict active site residues

EOF
  exit;

}

#-------------------------------------------------------------------------------

=head1 NAME

pfam_scan.pl -- Search protein sequences against the Pfam HMM library

=head1 SYNOPSIS

pfam_scan.pl [options] -fasta <fasta_file> -dir <Pfam_data_file_dir>

=head1 OPTIONS

=over

=item B<-dir> I<Pfam_data_file_dir>

Directory containing Pfam data files [required]

=item B<-fasta> I<fasta_file>

Filename of input file containing sequence(s) [required]

=item B<-outfile> I<output_file>

Write output to C<output_file> [default: STDOUT]

=item B<-e_seq>

Sequence E-value cut-off [default: HMMER3 default]

=item B<-e_dom> 

Domain E-value cut-off [default: HMMER3 default]

=item B<-b_seq>

Sequence bits score cut-off [default: HMMER3 default]

=item B<-b_dom>

Domain bits score cut-off [default: HMMER3 default]

=item B<-clan_overlap>

Allow sequences in different clans to overlap [default: false]

=item B<-align>

Show alignment snippets in results [default: false]

=item B<-as>

Search for active sites [default: false]

=item B<-h>

Display help message

=back

The input must be a FASTA-format file. The C<-fasta> and C<-dir> options are 
mandatory. You cannot specify both an E-value and bits score threshold.  

=head1 OVERVIEW

C<pfam_scan.pl> is a script for searching one or more protein sequences against the
library of HMMs from Pfam. It requires a local copy of the Pfam data files, which 
can be obtained from the Pfam FTP area:

  ftp://ftp.sanger.ac.uk/pub/database/Pfam/current_release/

You must also have the HMMER3 binaries installed and their locations given by your
C<PATH> environment variable. You can download the HMMER3 package at:

  ftp://selab.janelia.org/pub/software/hmmer3/

=head1 OUTPUT

The output format is:
<seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc> <hmm name> <type> <hmm start> <hmm end> <hmm length> <bit score> <E-value> <significance> <clan> <predicted_active_site_residues>

Example output (with -as option):

  Q1JEP1.1     12    398     12    398 PF01640   Peptidase_C10     Domain     1   394   394    746.6  4.3e-225 1 CL0125    predicted_active_site[192,340]
  Q69SA9.1    146    243    143    259 PF00085   Thioredoxin       Domain     5    95   104     59.3   1.9e-16 1 CL0172
  Q69SA9.1    292    466    277    466 PF07970   COPIIcoated_ERV   Family    52   219   219     90.6   5.8e-26 1 No_clan



=head1 REFERENCES

Active site residues are predicted using the method described in the publication: 

Mistry J., Bateman A., Finn R.D. "Predicting active site residue annotations in 
the Pfam database." BMC Bioinformatics. 2007;8:298. PMID:17688688.

=head1 AUTHORS

Jaina Mistry (jm14@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

=cut

=head1 COPYRIGHT

Copyright (c) 2009: Genome Research Ltd.

Authors: Jaina Mistry (jm14@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

