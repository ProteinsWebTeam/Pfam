package Bio::Pfam::AlignMethods;

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::SeqFetch;

=head1 help

Title:help
Function: Displays help message indicating which parameters can be used with the program
Usage: &help(@_);

=cut

sub help {
  my $prog = shift;

  print "\nUsage: $prog  <options>\n";

  if ( $prog =~ /extend.pl/ ) {
    print STDERR
      "This program extends a preexisting alignment and outputs to STDOUT.

Required OPTIONS: 

 -align <alignment>    Alignment in mul format \n
 -n <integer>          Number of residues to extend at amino end \n
 -c <integer>          Number of residues to extend at carboxyl end \n";
  }

  elsif ( $prog =~ /wholeseq.pl/ ) {
    print STDERR
"This program takes an alignment in mul format and creates an alignment from the whole sequences of each member of the alignment and outputs to STDOUT.

Required OPTIONS: 

-align <alignment_file> \n";
  }

  elsif ( $prog =~ /create_alignment.pl/ ) {
    print STDERR
      "This program creates an alignment from fasta file and outputs to STDOUT.

Required OPTIONS: 

 -fasta <fasta_file> \n";
  }

  elsif ( $prog =~ /merge_alignment.pl/ ) {
    print STDERR
"This program creates an alignment from two fasta files and outputs to STDOUT.

Required OPTIONS: 

 -fasta <fasta_file> \n
 -fasta2 <fasta_file2> \n";
  }

  print STDERR ( "
One of the following alignment methods:

    -t		Use T-coffee alignment method \n
    -m          Use MAFFT alignment method \n
    -mu         Use muscle alignment method \n
    -mup        Use muscle progressive-alignment methods (quicker, less accurate) \n
    -cl         Use Clustalw \n
    -ma   -pdb <pdb identifier>   -chain <chain>         Use mask alignment method (chain is optional)\n               
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
  open( FASTA, "$bin/sreformat fasta $fasta_file |") or die "Coudn't open fh to sreformat ($bin/sreformat fasta $fasta_file)  $!";

  my ( @sequence, @description, $i );

  $/ = ">";    # Change record seperator
  $i = 0;
  while (<FASTA>) {
    if (/^\s?(\S+)\s?.*\n/) {
      $description[$i] = $1;
      $sequence[$i]    = $';    #'
      chop $sequence[$i];       # Remove > from end of sequence
      $sequence[$i] =~ s/\n//g;        # Remove newlines from sequence
      $sequence[$i] =~ s/.- _//g;      # Remove spaces from sequence
      $sequence[$i] =~ tr/a-z/A-Z/;    # Make sequence upper case
      $i++;
    }
  }
  close(FASTA);

  $/ = "\n";                           # Change record seperator
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

  my ( $bin, $sequence, $description, $method, $fasta_file, $pdb, $chain) = @_;


  # Create fasta file
  open( TMP, ">tmp$$" ) or die "Couldn't open fh to tmp$$ $!";;
  for ( my $i=0; $i< @$sequence; $i++) {
      print TMP ">$i~$description->[$i]\n$sequence->[$i]\n"; #need to insert $i at the beginning of each accession no. to ensure each one is unique
  }
  close(TMP);

  # Create fasta file
  if ( ( $method =~ m/clustal/i ) || ( $method =~ m/clustalw/i ) ) {
    my $optns = "-infile=tmp$$";
    system ("$bin/clustalw $optns -output=gcg > /dev/null") and die "Error running clustalw: $!";
    open(TMP, "$bin/sreformat selex tmp$$.msf |") or die "Couldn't open fh to sreformat ($bin/sreformat selex tmp$$.msf) $!";
  }
  elsif ( $method =~ m/t_coffee/i ) {

    #    print "\# Align with t_coffee\n";
    system "$bin/t_coffee tmp$$ -output=msf > /dev/null" and die "Error running t_coffee: $!";;

    # Reformat a bit
    open( T,  "tmp$$.msf" )    || die "Could not open tmp$$.msf\n";
    open( T2, "> tmp$$.msf2" ) || die "Could not open tmp$$.msf2\n";
    my $print;
    while (<T>) {
      if ($print) {
        print T2;
      }
      if (/\/\//) {
        $print = 1;
      }

    }
    close(T);
    close(T2);

    open(TMP, "$bin/sreformat selex tmp$$.msf2 |") or die "Couldn't open fh to sreformat ($bin/sreformat selex tmp$$.msf2) $!";
  }
  elsif ( $method =~ m/mask/i ) {

    # Make mask file
    open( MASK, ">maskfile" ) || die "Can't open maskfile $!";
    &pdb2mask( $pdb, *MASK, $chain );
    close(MASK);
    open( SLX, "> tmp$$.slx" ) || die "Couldn't open tmp$$.slx $!";;
    &clustal_mask( "maskfile", "tmp$$", *SLX );
    close(SLX);
    open(TMP, ">tmp$$.slx") or die "Couldn't open fh to tmp$$.slx $!";

  }
  elsif ( $method =~ m/MAFFT/i ) {
      system("$bin/sreformat -u fasta tmp$$ > tmp$$.fa") and die "sreformat failed $!";

# V4 of MAFFT is much better. It now has a wrapper and a series of options. There seems to be
# no format issues now and my test show there is not need for the -/\. substitution.
    system ("$bin/mafft  --quiet --maxiterate 16 tmp$$.fa > tmp$$.mafft") and die "Error running MAFFT: $!";

    open(TMP, "$bin/sreformat selex tmp$$.mafft |") or die "Couldn't open fh to sreformat ($bin/sreformat selex tmp$$.mafft) $!";
  }
  elsif ( $method =~ /muscle-pro/i ) {

# Note need to remain this way round for the statement to distinguish between the two
    system("$bin/muscle -in tmp$$ -out tmp$$.fa -quiet -maxiters 2") and die "Error running muscle: $!";
    open(TMP, "$bin/sreformat selex tmp$$.fa |") or die "Couldn't open fh to sreformat ($bin/sreformat selex tmp$$.fa) $!";
  }
  elsif ( $method =~ /muscle/i ) {

    system("$bin/muscle -in tmp$$ -out tmp$$.fa -quiet") and die "Error running muscle: $!";
    open(TMP, "$bin/sreformat selex tmp$$.fa |") or die "Couldn't open fh to sreformat ($bin/sreformat selex tmp$$.fa) $!";
  }
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
}

=head2 pdb2mask

 Title    : pdb2mask
 Function : takes pdb code and chain and outputs mask file for clustal
          : Returns zero if unsuccessful
 Usage    : &pdb2mask($id,*FILE, $chain);

=cut

sub pdb2mask {

  my (
    $id,                $chain,         $read,           $sequence,
    $dir,               $current_chain, $residue_number, $sec_structure,
    $old_sec_structure, $start,         $obj
  );

  $old_sec_structure = "B";    #arbitarily set

  $id    = shift;
  *FILE  = shift;
  $chain = shift;

  #    my $dssp_dir   = $Bio::Pfam::dssp_dir;
  $id = lc $id;

  if ($chain) {
    $chain =~ tr/a-z/A-Z/;     # Chains are upper case in dssp
  }

  # Retrieve DSSP info
  my $dssp = "DSSP";
  system("getz -f res '[dssp:$id]' > DSSP") and die "Error running getz: $!";

  open( DSSP, "$dssp" ) || do {
    warn "Could not open dssp file for $id\n";
    return 0;
  };

  # Need to write out dummy header
  print FILE "ID   $id\n";

  while (<DSSP>) {
    unless (/^#/) {

      if ($chain) {
        $current_chain = substr( $_, 11, 1 );
      }

      if ( !$chain || $current_chain eq $chain ) {

        $sequence .= substr( $_, 13, 1 );
        $residue_number = substr( $_, 1,  4 );
        $sec_structure  = substr( $_, 16, 1 );
        $sec_structure =~ s/[STBG]//g;    # Ignore non H or E.

        # Now need to write out FT lines
        # This is start of secondary structure

        if ( ( !$old_sec_structure || $old_sec_structure =~ /[^H]/ )
          && $sec_structure =~ /H/ )
        {
          $start = $residue_number;
        }
        if ( ( !$old_sec_structure || $old_sec_structure =~ /[^E]/ )
          && $sec_structure =~ /E/ )
        {
          $start = $residue_number;
        }

        # This is end of secondary structure
        if ( $old_sec_structure =~ /[H]/
          && ( $sec_structure ne $old_sec_structure ) )
        {
          print FILE "FT   HELIX       $start    ", $residue_number - 1, "\n";
        }
        if ( $old_sec_structure =~ /[E]/
          && ( $sec_structure ne $old_sec_structure ) )
        {
          print FILE "FT   STRAND      $start    ", $residue_number - 1, "\n";
        }
        $old_sec_structure = $sec_structure;

      }

    }
  }
  close(DSSP);

  print FILE "SQ\n";
  print FILE "$sequence\n";
  return ("1");
}

=head2 clustal_mask

# Title    : clustal_mask
# Function : uses mask to make alignment, writes file to $fasta.msf
# Usage    : &clustal_mask($mask,$fasta,*FILEHANDLE);

=cut

sub clustal_mask {
  my ( $i, $mask, $fasta, $bin );
  $mask  = shift;
  $fasta = shift;
  *FILE  = shift;
  $bin = shift;
  $i     = 0;

  # Do alignment
  system("$bin/clustalw -profile -profile1=$mask -profile2=$fasta -secstrout=both -sequences -output=gcg > /dev/null") and die "Error running clustalw: $!";

# Need to remove sequence of structure. May be duplicated or not exist in pfamseq!

  open( ALI, "sreformat selex $fasta.msf | sel2mul |" ) or die "Can't open Alignment file [$fasta.msf] $!";
  while (<ALI>) {
    if ( $i > 0 ) {
      print FILE "$_";
    }
    $i++;
  }

  return;

}

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

=head2 extend_alignment

	Title	: extend_alignment
	Function: Extends a set of sequences
	Returns : An extended set of sequences in fasta format
	Usage   : &extend_alignment($opt_align, $dbarg, $opt_n, $opt_c)

=cut

sub extend_alignment {

  my $opt_align = shift;
  my $db_arg     = shift;
  my $opt_n     = shift;
  my $opt_c     = shift;

  open( TEMP,  "> FA" ) or die "Can't open temp file $!";
  open( ALIGN, "$opt_align" ) or die "Can't open $opt_align $!";

  my ( %length, %sequence );
  my ( $id, $from, $to, $newfrom, $newto, $original_seqno, $retrieved_seqno );


  my (%nse, %seqList);

  my $fa_file = "FA.whole.$$";

  open(F, ">$fa_file") or die "Could not open $fa_file for writing :[$!]\n";
  while (<ALIGN>) {
    if (/^(\S+)\/(\d+)-(\d+)\s+(\S+)/) {
        my ($acc, $start, $end) = ($1, $2, $3);

	push(@{$nse{$acc}}, { start => $start, end => $end} );
	
	push(@{ $seqList{$acc} }, { whole => 1 });
    }
    elsif(/^\/\//) {
	next;
    }
    else {
	warn "Unrecognised line: [$_]\n";
    }
  }

  my $original = keys %seqList;
  my $retrieved =  Bio::Pfam::SeqFetch::fetchSeqs(\%seqList,$db_arg, \*F);
  close F;

  my $diff = $original - $retrieved;
  print STDERR "Extending $retrieved out the $original sequences in alignment.\n";
  if($diff != 0) {
      print STDERR "Warning - $diff of the $original sequences in the original alignment were not retrieved\n";
  }

  my $acc;
  open(F, "$fa_file") or die "Couldn't open $fa_file for reading :[$!]\n";
  while (<F>) {
      if (/^>(\S+)\/\d+\-(\d+)/) {
	  $acc = $1;
          $length{$acc}=$2;
      }
      elsif (/(.*)/) {
	  $sequence{$acc} .= $1;
      }
  } 
  close(F);
  unlink("$fa_file");  



  foreach my $acc (keys %length) {

      foreach my $se (@{$nse{$acc}}) {
	  $retrieved++;
	  my ($new_start, $new_end);

	  if( ($se->{start} - $opt_n) > 0) {
	      $new_start = $se->{start} - $opt_n;
	  }
	  else {
	      $new_start = 1;
	  }

	  if( ($se->{end} + $opt_c) > $length{$acc}) {
	      $new_end = $length{$acc};
	  }
	  else {
	      $new_end = $se->{end} + $opt_c;
	  }
	  my $extended_seq = substr( $sequence{$acc}, $new_start - 1, $new_end - $new_start + 1 );

	  print TEMP ">$acc/$new_start\-$new_end\n$extended_seq\n";
      }
  }

  close TEMP;

  return;
}

=head2 id2acc

	Title	: id2acc
	Function: Changes id number to accession number
	Returns : An accession number
	Usage   : &id2acc($id)

=cut

sub id2acc {
  my ( $seqid, $rdb ) = @_;

  unless ($seqid) {
    die "No sequence accession has been passed in\n";
  }

  unless ($rdb) {
    die "No database object was passed in!\n";
  }

  unless ( $rdb->isa("Bio::Pfam::PfamDBManager") ) {
    die "$rdb is not a Bio::Pfam::PfamDBManager\n";
  }

  #my $pfamseq = $rdb->getSchema.....

#@_ = $rdb->query("select pfamseq_acc, pfamseq_id from pfamseq where pfamseq_id=\"$seqid\"");

#if (!$acc) {
#   print STDERR "Warning $seqid has no corresponding accession number and will not be used in the alignment\n";
#}
#return ($acc);
}

=head2 trim_align

 Title   : trim_align
 Usage   : &trim_align(*FILEHANDLE, $percentage);
 Function: Given a file of an alignment this routine will
           trim from both N and C terminus to first column that is completely non-gap
 Returns: Nothing

=cut

sub trim_align {
  my ( $fh, $percent ) = @_;

  #Need to check that this is a glob
  unless ( ref($fh) eq "GLOB" ) {
    carp("Expecting a file handle to be passed in, got  $fh");
  }

  my $aln = new Bio::Pfam::AlignPfam;
  $aln->read_Pfam($fh);
  my @columns = $aln->first_last_ungapped_column($percent);
  my $new_aln = $aln->trimmed_alignment( $columns[0], $columns[-1] );
  $new_aln->write_Pfam(*STDOUT);

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

