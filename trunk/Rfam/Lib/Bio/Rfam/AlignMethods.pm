package Bio::Rfam::AlignMethods;

#module calling alignment methods used in create_alignment.pl
#adapted from the Bio::Pfam::AlignmentMethods module: removed methods that are not currently used by Rfam for clarity.

use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;

=head1 help

Title:help
Function: Displays help message indicating which parameters can be used with the program
Usage: &help(@_);

=cut

sub help {
  my $prog = shift;

  print "\nUsage: $prog  <options>\n";

  if ( $prog =~ /create_alignment.pl/ ) {
    print STDERR
      "This program creates an alignment from fasta file and outputs to STDOUT.

Required OPTIONS: 

 -fasta <fasta_file> \n";
  }

  print STDERR ( "
One of the following alignment methods:

    -t		Use T-coffee alignment method \n
    -m          Use MAFFT alignment method \n
    -mu         Use muscle alignment method \n
    -mup        Use muscle progressive-alignment methods (quicker, less accurate) \n
    -cl         Use Clustalw \n
" );

  exit(0);
}

=head1 read_fasta

Title   : read_fasta
Function: Takes fasta filename and returns two array references.
	: (\@sequence,\@description)
Usage	: &readfasta($fasta_file, $bin_dir)

=cut


sub read_fasta {

  my ($fasta_file, $bin) = @_;
  open( FASTA, "$bin/esl-reformat fasta $fasta_file |") or die "Coudn't open fh to esl-reformat ($bin/esl-reformat fasta $fasta_file)  $!";

  my ( @sequence, @description, $i );

  $/ = ">";    # Change record seperator
  $i = 0;
  while (<FASTA>) {
      my @data = split('\n', $_);
      unless ($data[0] eq '>'){
	  my $num = @data;
	  $description[$i]=$data[0];
	  $sequence[$i]='';
	  for (my $j=1; $j<$num; $j++){
	      $sequence[$i] = $sequence[$i] . $data[$j];
	  }
	  $sequence[$i] =~ s/.- _//g;      # Remove spaces from sequence
	  $sequence[$i] =~ s/>//g;      # Remove > from sequence
	  $sequence[$i] =~ tr/a-z/A-Z/;    # Make sequence upper case
	  $i++;
      }
  }
  close(FASTA);

  $/ = "\n";                           # Change record seperator
#  print Dumper (\@sequence);
  return ( \@sequence, \@description );
}



=head2 create_alignment

	Title	: create_alignment
	Function: Takes array of sequences and aligns
	       	: $method = clustal , T_coffee, MAFFT, hmmt
	Returns : array of aligned sequences
	Usage   : &create_alignment($bin_dir, \@sequence,\@description,$method,$fasta_file,$pdb,$chain)

=cut

sub create_alignment {

  my ( $bin, $sequence, $description, $method, $fasta_file) = @_;


  # Create fasta file
  open( TMP, ">tmp$$" ) or die "Couldn't open fh to tmp$$ $!";;
  for ( my $i=0; $i< @$sequence; $i++) {
      print TMP ">$i~$description->[$i]\n$sequence->[$i]\n"; #need to insert $i at the beginning of each accession no. to ensure each one is unique
  }
  close(TMP);

  # Create fasta file
  if ( ( $method =~ m/clustal/i ) || ( $method =~ m/clustalw/i ) ) {
    my $optns = "-infile=tmp$$";
    system ("$bin/clustalw $optns > /dev/null") and die "Error running clustalw: $!";
    open(TMP, "$bin/esl-reformat selex tmp$$.aln |") or die "Couldn't open fh to esl-reformat ($bin/esl-reformat selex tmp$$.aln) $!";
  } #end of clustal block

  elsif ($method =~ m/t_coffee/i){
      system "$bin/t_coffee tmp$$ -output=aln > /dev/null" and die "Error running t_coffee: $!";
#add clustal header to output file in order to parse
      open( T,  "tmp$$.aln" )    || die "Could not open tmp$$.msf\n";
      open( T2, "> tmp$$.aln2" ) || die "Could not open tmp$$.msf2\n";
      while (<T>){
	  if ($_ =~ /^CLUSTAL/){
	      print T2 "CLUSTAL 2.0.12 multiple sequence alignment\n";
	  } else {
	      print T2 $_;
	  }
      }
      close (T);
      close (T2);
      open(TMP, "$bin/esl-reformat selex tmp$$.aln2 |") or die "Couldn't open fh to esl-reformat ($bin/esl-reformat selex tmp$$.aln2) $!";

  } #end of t_coffee block

  elsif ( $method =~ m/MAFFT/i ) {
      system("$bin/esl-reformat -u fasta tmp$$ > tmp$$.fa") and die "esl-reformat failed $!";

# V4 of MAFFT is much better. It now has a wrapper and a series of options. There seems to be
# no format issues now and my test show there is not need for the -/\. substitution.
    system ("$bin/mafft  --quiet --maxiterate 16 tmp$$.fa > tmp$$.mafft") and die "Error running MAFFT: $!";

    open(TMP, "$bin/esl-reformat selex tmp$$.mafft |") or die "Couldn't open fh to esl-reformat ($bin/esl-reformat selex tmp$$.mafft) $!";
  } #end of mafft block
  elsif ( $method =~ /muscle-pro/i ) {

# Note need to remain this way round for the statement to distinguish between the two
    system("$bin/muscle -in tmp$$ -out tmp$$.fa -quiet -maxiters 2") and die "Error running muscle: $!";
    open(TMP, "$bin/esl-reformat selex tmp$$.fa |") or die "Couldn't open fh to esl-reformat ($bin/esl-reformat selex tmp$$.fa) $!";
  } #end of muscle-pro block
  elsif ( $method =~ /muscle/i ) {

    system("$bin/muscle -in tmp$$ -out tmp$$.fa -quiet") and die "Error running muscle: $!";
    open(TMP, "$bin/esl-reformat selex tmp$$.fa |") or die "Couldn't open fh to esl-reformat ($bin/esl-reformat selex tmp$$.fa) $!";
  } #end of muscle block
  else {
    print "Cannot align with method: $method.\n";
  }

  my ( %hash, $ali_name, $sequence_s_e );

  # Put into mul format


  while (<TMP>) {
    if ( !(/^\#/) and ( !/^$/ ) and ( !/\/\// ) )  {    # Ignore blanks and lines beginning with #.
	/^(\S+)\s+(\S+)$/;                              # (name) (sequence).

      $ali_name     = $1;
      $sequence_s_e = $2;
      if ( defined $hash{$ali_name} ) {
        $hash{$ali_name} .= $sequence_s_e;    # append sequence.
      }
      else {                                  # if key doesn't exist make it.
        $hash{$ali_name} = $sequence_s_e;

      }
    }
  }

  close(TMP);

  my ( %newhash, $seq );

# Need to remove number inserted at the beginning of each line before accession number

  foreach my $temp ( keys %hash ) {
    $seq = $hash{$temp};
    $temp =~ s/^\d+~//;
    $newhash{$temp} = $seq;
  }

  my @tempfiles = glob "tmp$$*";
  foreach my $file (@tempfiles) {
    unlink $file;
  }

  return %newhash;
} #end of createalignment


=head2 print_alignment

	Title	: print_alignment
	Function: Prints aligned sequences
	Returns : 
	Usage   : print_alignment(@\description,\@aligned_sequence)

=cut

sub print_alignment {

  my $hash   = shift @_;
  my $method = shift;

  # first pass to get the longest id
  my $idlength = 1;
  foreach my $a ( keys %{$hash} ) {
    if ( ( length $a ) > $idlength ) {
      $idlength = length($a);
    }
  }

  my $line;
  my ( $key, $value );

  if ( $method eq "mask" ) {

    #need to trim alignment

    open( FILE, ">tmp$$.mul" ) or die "Can't open tmp$$.mul $!";
    while ( ( $key, $value ) = each( %{$hash} ) ) {
      $line = sprintf( "%-" . $idlength . "s %s \n", $key, $value );
      print FILE "$line";
    }
    close FILE;
    open( FILE, "tmp$$.mul" ) or die "Can't open tmp$$.mul $!";
    &trim_align( \*FILE, 99 );
    close FILE;
    unlink "tmp$$.mul";
  }

  else {
    while ( ( $key, $value ) = each( %{$hash} ) ) {
      print sprintf( "%-" . $idlength . "s %s \n", $key, $value );
    }
  }

  return;
}



=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;

