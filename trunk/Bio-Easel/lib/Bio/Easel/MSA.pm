package Bio::Easel::MSA;

use strict;
use warnings;
use File::Spec;
use Carp;

=head1 NAME

Bio::Easel - The great new Bio::Easel!

=head1 VERSION

Version 0.01

=cut

#-------------------------------------------------------------------------------

our $VERSION = '0.01';

# Easel status codes, these must be consistent with #define's in Bio-Easel/src/easel/easel.h
our $ESLOK             = '0';     # no error/success
our $ESLFAIL           = '1';     # failure
our $ESLEOL            = '2';     # end-of-line (often normal)
our $ESLEOF            = '3';     # end-of-file (often normal)
our $ESLEOD            = '4';     # end-of-data (often normal)
our $ESLEMEM           = '5';     # malloc or realloc failed
our $ESLENOTFOUND      = '6';     # file or key not found
our $ESLEFORMAT        = '7';     # file format not correct
our $ESLEAMBIGUOUS     = '8';     # an ambiguity of some sort
our $ESLEDIVZERO       = '9';     # attempted div by zero
our $ESLEINCOMPAT      = '10';    # incompatible parameters
our $ESLEINVAL         = '11';    # invalid argument/parameter
our $ESLESYS           = '12';    # generic system call failure
our $ESLECORRUPT       = '13';    # unexpected data corruption
our $ESLEINCONCEIVABLE = '14';    # "can't happen" error
our $ESLESYNTAX        = '15';    # invalid user input syntax
our $ESLERANGE         = '16';    # value out of allowed range
our $ESLEDUP           = '17';    # saw a duplicate of something
our $ESLENOHALT        = '18';    # a failure to converge
our $ESLENORESULT      = '19';    # no result was obtained
our $ESLENODATA        = '20';    # no data provided, file empty
our $ESLETYPE          = '21';    # invalid type of argument
our $ESLEOVERWRITE     = '22';    # attempted to overwrite data
our $ESLENOSPACE       = '23';    # ran out of some resource
our $ESLEUNIMPLEMENTED = '24';    # feature is unimplemented
our $ESLENOFORMAT      = '25';    # couldn't guess file format
our $ESLENOALPHABET    = '26';    # couldn't guess seq alphabet
our $ESLEWRITE         = '27';    # write failed (fprintf, etc)

my $src_file      = undef;
my $typemaps      = undef;
my $easel_src_dir = undef;

BEGIN {
  $src_file = __FILE__;
  $src_file =~ s/\.pm/\.c/;

  my $file = __FILE__;
  ($easel_src_dir) = $file =~ /^(.*)\/blib/;
  $easel_src_dir = File::Spec->catfile( $easel_src_dir, 'src/easel' );

  $typemaps = __FILE__;
  $typemaps =~ s/\.pm/\.typemap/;
}

use Inline
  C        => "$src_file",
  VERSION  => '0.01',
  ENABLE   => 'AUTOWRAP',
  INC      => "-I$easel_src_dir",
  LIBS     => "-L$easel_src_dir -leasel",
  TYPEMAPS => $typemaps,
  NAME     => 'Bio::Easel::MSA';

=head1 SYNOPSIS

Multiple sequence alignment handling through inline C with Easel.

Perhaps a little code snippet.

    use Bio::Easel::MSA;

    my $foo = Bio::Easel::MSA->new({"fileLocation" => $alnfile});
    ...

=head1 EXPORT

No functions currently exported.

=head1 SUBROUTINES/METHODS
=cut

#-------------------------------------------------------------------------------

=head2 new 

  Title    : new
  Incept   : EPN, Thu Jan 24 09:28:54 2013
  Usage    : Bio::Easel::MSA->new
  Function : Generates a new Bio::Easel::MSA object.
           : Either <fileLocation> or <esl_msa> must be passed in.
  Args     : <fileLocation>: optional: file location of alignment
           : <esl_msa>:      optional: ptr to an Easel ESL_MSA object
           : <reqdFormat>:   optional: string defining requested/required format
           :                 valid format strings are: 
           :                 "unknown", "Stockholm", "Pfam", "UCSC A2M", "PSI-BLAST", 
           :                 "SELEX", "aligned FASTA", "Clustal", "Clustal-like", 
           :                 "PHLYIP (interleaved)", or "PHYLIP (sequential)"
           : <forceText>:    '1' to read the alignment in text mode
  Returns  : Bio::Easel::MSA object

=cut

sub new {
  my ( $caller, $args ) = @_;
  my $class = ref($caller) || $caller;
  my $self = {};

  bless( $self, $caller );

  # set flag to digitize, unless forceText passed in
  if ( defined $args->{forceText} && $args->{forceText}) { 
    $self->{digitize} = 0;
  }
  else { 
    $self->{digitize} = 1;
  }

  # First check that the file exists. If it exists, read it with
  # Easel and populate the object from the ESL_MSA object
  if ( defined $args->{fileLocation} && -e $args->{fileLocation} ) {
    eval {
      $self->{path}   = $args->{fileLocation};
      if(defined $args->{reqdFormat}) { 
        $self->{reqdFormat} = $args->{reqdFormat};
        $self->_check_reqd_format();
      }
      $self->read_msa();
    };    # end of eval
    if ($@) {
      if(defined $args->{reqdFormat}) { 
        confess("Error creating ESL_MSA from @{[$args->{fileLocation}]} if code 7 probably wrong format, $@\n");
      }
      else { 
        confess("Error creating ESL_MSA from @{[$args->{fileLocation}]}, $@\n");
      }
    }
  }
  elsif (defined $args->{esl_msa}) { 
    $self->{esl_msa} = $args->{esl_msa};
  }
  else {
    confess("Expected to receive an ESL_MSA or valid file location path (@{[$args->{fileLocation}]} doesn\'t exist)");
  }
  if ( defined $args->{aliType} ) {
    $self->{aliType} = $args->{aliType};
  }
  
  return $self;
}

#-------------------------------------------------------------------------------

=head2 msa

  Title    : msa
  Incept   : EPN, Tue Jan 29 09:06:30 2013
  Usage    : Bio::Easel::MSA->msa()
  Function : Accessor for msa: sets (if nec) and returns MSA.
  Args     : none
  Returns  : msa   

=cut

sub msa {
  my ($self) = @_;

  if ( !defined( $self->{esl_msa} ) ) {
    $self->read_msa();
  }
  return $self->{esl_msa};
}

#-------------------------------------------------------------------------------

=head2 path

  Title    : path
  Incept   : EPN, Tue Jan 30 15:42:30 2013
  Usage    : Bio::Easel::MSA->path()
  Function : Accessor for path, read only.
  Args     : none
  Returns  : string containing path to the SEED or undef.   

=cut

sub path {
  my ($self) = @_;

  return defined( $self->{path} ) ? $self->{path} : undef;
}

#-------------------------------------------------------------------------------

=head2 format

  Title    : format
  Incept   : rdf, Fri Jul 19 14:05:01 2013
  Usage    : $msaObject->format()
  Function : Gets the format of the MSA
  Args     : none
  Returns  : String of the format.

=cut

sub format {
  my ($self) = @_;
  
  if(!$self->{informat}){
    $self->read_msa;
  }

  return ($self->{informat});
}

#-------------------------------------------------------------------------------

=head2 is_digitized

  Title    : is_digitized
  Incept   : EPN, Wed Mar  5 09:14:34 2014
  Usage    : $msaObject->is_digitized()
  Function : Returns '1' is MSA is digitized, else returns '0'
  Args     : none
  Returns  : '1' if MSA is digitized, else returns '0'.

=cut

sub is_digitized {
  my ($self) = @_;
  
  return _c_is_digitized( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 read_msa

  Title    : read_msa
  Incept   : EPN, Mon Jan 28 09:26:24 2013
  Usage    : Bio::Easel::MSA->read_msa($fileLocation)
  Function : Opens $fileLocation, reads first MSA, sets it.
  Args     : <fileLocation>: file location of alignment, required unless $self->{path} already set
           : <reqdFormat>:   optional, required format of alignment file
           : <do_text>:      optional, TRUE to read alignment in text mode
  Returns  : void

=cut

sub read_msa {
  my ( $self, $fileLocation, $reqdFormat, $do_text ) = @_;

  if ($fileLocation) {
    $self->{path} = $fileLocation;
  }
  if ( !defined $self->{path} ) {
    croak "trying to read msa but path is not set";
  }

  if ($reqdFormat) { 
    $self->{reqdFormat} = $reqdFormat;
  }

  # default is to read in digital mode
  if (defined $do_text && $do_text) { 
    $self->{digitize} = 0;
  }
  elsif(! defined $self->{digitize}) { 
    $self->{digitize} = 1; 
  }

  my $informat = "unknown";
  if (defined $self->{reqdFormat}) {
    $informat = $self->{reqdFormat};
    $self->_check_reqd_format();
  }

  ($self->{esl_msa}, $self->{informat}) = _c_read_msa( $self->{path}, $informat, $self->{digitize});
  # Possible values for 'format', a string, derived from esl_msafile.c::eslx_msafile_DecodeFormat(): 
  # "unknown", "Stockholm", "Pfam", "UCSC A2M", "PSI-BLAST", "SELEX", "aligned FASTA", "Clustal", 
  # "Clustal-like", "PHYLIP (interleaved)", or "PHYLIP (sequential)".

  return;
}

#-------------------------------------------------------------------------------

=head2 nseq

  Title    : nseq
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : $msaObject->nseq()
  Function : Gets number of seqs in MSA
  Args     : none
  Returns  : number of sequences (esl_msa->nseq)

=cut

sub nseq {
  my ($self) = @_;

  $self->_check_msa();
  return _c_nseq( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 alen

  Title    : alen
  Incept   : EPN, Tue Jan 29 07:41:08 2013
  Usage    : $msaObject->alen()
  Function : Get alignment length.
  Args     : none
  Returns  : alignment length, number of columns (esl_msa->alen)

=cut

sub alen {
  my ($self) = @_;

  $self->_check_msa();
  return _c_alen( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 checksum

  Title    : checksum
  Incept   : EPN, Tue Mar  4 09:42:52 2014
  Usage    : $msaObject->checksum()
  Function : Determine the checksum for an MSA. Caution: the 
           : same MSA will give a different checksum depending
           : on whether it was read in text or digital mode.
  Args     : none
  Returns  : checksum as in integer.

=cut

sub checksum {
  my ($self) = @_;

  $self->_check_msa();
  return _c_checksum( $self->{esl_msa} );
}


#-------------------------------------------------------------------------------

=head2 has_rf

  Title    : has_rf
  Incept   : EPN, Tue Apr  2 19:44:21 2013
  Usage    : $msaObject->has_rf()
  Function : Does MSA have RF annotation?
  Args     : none
  Returns  : '1' if MSA has RF annotation, else returns 0

=cut

sub has_rf {
  my ($self) = @_;

  $self->_check_msa();
  return _c_has_rf( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 has_ss_cons

  Title    : has_ss_cons
  Incept   : EPN, Fri May 24 09:56:40 2013
  Usage    : $msaObject->has_ss_cons()
  Function : Does MSA have SS_cons annotation?
  Args     : none
  Returns  : '1' if MSA has SS_cons annotation, else returns 0

=cut

sub has_ss_cons {
  my ($self) = @_;

  $self->_check_msa();
  return _c_has_ss_cons( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 get_rf

  Title    : get_rf
  Incept   : EPN, Thu Nov 21 10:10:00 2013
  Usage    : $msaObject->get_rf()
  Function : Returns msa->rf if it exists, else dies via croak.
  Args     : None
  Returns  : msa->rf if it exists, else dies

=cut

sub get_rf { 
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  if(! $self->has_rf()) { croak "Trying to fetch RF from MSA but it does not exist"; }
  return _c_get_rf( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 set_rf

  Title    : set_rf
  Incept   : EPN, Tue Feb 18 09:54:20 2014
  Usage    : $msaObject->set_rf()
  Function : Sets msa->rf given a string.
  Args     : $rfstr: string that will become RF
  Returns  : void
  Dies     : if length($rfstr) != msa->alen

=cut

sub set_rf { 
  my ( $self, $rfstr ) = @_;

  $self->_check_msa();
  if(length($rfstr) != $self->alen) { croak "Trying to set RF with string of incorrect length"; }
  return _c_set_rf( $self->{esl_msa}, $rfstr );
}

#-------------------------------------------------------------------------------

=head2 get_ss_cons

  Title    : get_ss_cons
  Incept   : EPN, Fri May 24 10:03:41 2013
  Usage    : $msaObject->get_ss_cons()
  Function : Returns msa->ss_cons if it exists, else dies via croak.
  Args     : None
  Returns  : msa->ss_cons if it exists, else dies

=cut

sub get_ss_cons { 
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  if(! $self->has_ss_cons()) { croak "Trying to fetch SS_cons from MSA but it does not exist"; }
  return _c_get_ss_cons( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 set_ss_cons

  Title    : set_ss_cons
  Incept   : EPN, Tue Feb 18 10:15:25 2014
  Usage    : $msaObject->set_ss_cons()
  Function : Sets msa->ss_cons given a string.
  Args     : $ss_cons_str: string that will become msa->ss_cons
  Returns  : void
  Dies     : if length($ss_cons_str) != msa->alen

=cut

sub set_ss_cons { 
  my ( $self, $ss_cons_str ) = @_;

  $self->_check_msa();
  if(length($ss_cons_str) != $self->alen) { croak "Trying to set SS_cons with string of incorrect length"; }
  return _c_set_ss_cons( $self->{esl_msa}, $ss_cons_str );
}

#-------------------------------------------------------------------------------

=head2 set_blank_ss_cons

  Title    : set_blank_ss_cons
  Incept   : EPN, Tue Oct 22 10:38:39 2013
  Usage    : $msaObject->set_blank_ss_cons()
  Function : Sets msa->ss_cons as all '.' characters (zero basepairs).
  Args     : None
  Returns  : Nothing

=cut

sub set_blank_ss_cons { 
  my ( $self ) = @_;

  $self->_check_msa();
  return _c_set_blank_ss_cons( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 get_sqname

  Title    : get_sqname
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : $msaObject->get_sqname($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence $idx (esl_msa->sqname[$idx])
             ($idx runs 0..nseq-1)

=cut

sub get_sqname {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  return _c_get_sqname( $self->{esl_msa}, $idx );
}

#-------------------------------------------------------------------------------

=head2 get_sqidx

  Title    : get_seqidx
  Incept   : EPN, Mon Feb  3 14:20:07 2014
  Usage    : $msaObject->_get_sqidx($sqname)
  Function : Return the index of sequence $sqname in the MSA.
  Args     : $sqname: the sequence of interest
  Returns  : index of $sqname in $msa, or -1 if it does not exit
  Dies     : if msa->index is not setup
=cut

sub get_sqidx {
  my ( $self, $sqname ) = @_;

  $self->_check_msa();
  $self->_check_index();
  
  return _c_get_sqidx($self->{esl_msa}, $sqname);
}

#-------------------------------------------------------------------------------

=head2 set_sqname

  Title    : set_sqname
  Incept   : EPN, Mon Jan 28 09:48:42 2013
  Usage    : $msaObject->set_sqname($idx, $newName)
  Function : Returns nothing
  Args     : index of sequence, new sequence name. 
  Returns  : void

=cut

sub set_sqname {
  my ( $self, $idx, $newname ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  _c_set_sqname( $self->{esl_msa}, $idx, $newname );
  return;
}

#-------------------------------------------------------------------------------

=head2 get_sqwgt

  Title    : get_sqwgt
  Incept   : EPN, Fri May 24 10:47:03 2013
  Usage    : $msaObject->get_sqwgt($idx)
  Function : Returns weight of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : weight of sequence $idx (esl_msa->wgt[$idx])
             ($idx runs 0..nseq-1)

=cut

sub get_sqwgt {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  return _c_get_sqwgt( $self->{esl_msa}, $idx );
}

#-------------------------------------------------------------------------------

=head2 remove_sqwgts

  Title    : remove_sqwgts
  Incept   : EPN, Thu Feb 20 14:25:35 2014
  Usage    : $msaObject->remove_sqwgts()
  Function : Removes GF WT annotation from an MSA.
  Args     : none
  Returns  : void

=cut

sub remove_sqwgts {
  my ( $self ) = @_;

  $self->_check_msa();
  _c_remove_sqwgts( $self->{esl_msa} );
  return;
}

#-------------------------------------------------------------------------------

=head2 get_accession

  Title    : get_accession
  Incept   : EPN, Fri Feb  1 11:43:08 2013
  Usage    : $msaObject->get_accession()
  Function : Gets accession for MSA.
  Args     : none
  Returns  : the accession, a string

=cut

sub get_accession {
  my ($self) = @_;

  $self->_check_msa();
  return _c_get_accession( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 get_name

  Title    : get_name
  Incept   : EPN, Mon Jul  8 10:00:44 2013
  Usage    : $msaObject->get_name($name)
  Function : Gets name for MSA.
  Args     : none
  Returns  : the name, a string

=cut

sub get_name {
  my ($self) = @_;

  $self->_check_msa();
  return _c_get_name( $self->{esl_msa} );
}

#-------------------------------------------------------------------------------

=head2 set_accession

  Title    : set_accession
  Incept   : EPN, Fri Feb  1 11:11:05 2013
  Usage    : $msaObject->set_accession($acc)
  Function : Sets accession for MSA in <esl_msa>
  Args     : accession string to set
  Returns  : void

=cut

sub set_accession {
  my ( $self, $newname ) = @_;

  $self->_check_msa();
  my $status = _c_set_accession( $self->{esl_msa}, $newname );
  if ( $status != $ESLOK ) {
    croak "unable to set name (failure in C code)";
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 set_name

  Title    : set_name
  Incept   : EPN, Fri Feb  1 11:11:05 2013
  Usage    : $msaObject->set_name($acc)
  Function : Sets name for MSA in <esl_msa>
  Args     : name string to set
  Returns  : void

=cut

sub set_name {
  my ( $self, $newname ) = @_;

  $self->_check_msa();
  my $status = _c_set_name( $self->{esl_msa}, $newname );
  if ( $status != $ESLOK ) {
    croak "unable to set name (failure in C code)";
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 write_msa

  Title    : write_msa
  Incept   : EPN, Mon Jan 28 09:58:19 2013
  Usage    : $msaObject->write_msa($fileLocation)
  Function : Write MSA to a file
  Args     : $outfile: name of output file, if "STDOUT" output to stdout, not to a file
           : $format:  ('stockholm', 'pfam', 'afa', 'clustal', or 'fasta')
           :           if 'fasta', write out seqs in unaligned fasta.
  Returns  : void

=cut

sub write_msa {
  my ( $self, $outfile, $format ) = @_;

  my $status;
  $self->_check_msa();
  if ( !defined $format ) {
    $format = "stockholm";
  }
  if ($format eq "fasta") { # special case, write as unaligned fasta
    $status = _c_write_msa_unaligned_fasta( $self->{esl_msa}, $outfile );
  }
  elsif ( $format eq "stockholm"
          || $format eq "pfam"
          || $format eq "afa" 
          || $format eq "clustal" )
  {
    $status = _c_write_msa( $self->{esl_msa}, $outfile, $format );
  }
  else { 
    croak "format must be \"stockholm\" or \"pfam\" or \"afa\" or \"clustal\" or \"fasta\"";
  }
  if ( $status != $ESLOK ) {
    if ( $status == $ESLEINVAL ) {
      croak "problem writing out msa, invalid format $format";
    }
    elsif ( $status == $ESLFAIL ) {
      croak "problem writing out msa, unable to open output file $outfile for writing";
    }
    elsif ( $status == $ESLEMEM ) {
      croak "problem writing out msa, out of memory";
    }
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 write_single_unaligned_seq

  Title    : write_single_unaligned_seq
  Incept   : EPN, Mon Nov  4 09:53:50 2013
  Usage    : Bio::Easel::MSA->write_single_unaligned_seq($idx)
  Function : Writes out a single seq from MSA in FASTA format to a file.
  Args     : $idx:     index of seq in MSA to output
           : $outfile: name of file to create
  Returns  : void

=cut

sub write_single_unaligned_seq { 
  my ($self, $idx, $outfile) = @_;

  my $status;

  $self->_check_msa();
  $status = _c_write_single_unaligned_seq( $self->{esl_msa}, $idx, $outfile );
  if($status != $ESLOK) { 
    if   ($status == $ESLEINVAL) { 
      croak "problem writing out single seq idx $idx, idx out of bounds";
    }
    elsif($status == $ESLFAIL) { 
      croak "problem writing out single seq idx $idx, unable to open $outfile for writing"; 
    }
    elsif ( $status == $ESLEMEM ) {
      croak "problem writing out msa, out of memory";
    }
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 any_allgap_columns

  Title    : any_allgap_columns
  Incept   : EPN, Mon Jan 28 10:44:12 2013
  Usage    : Bio::Easel::MSA->any_allgap_columns()
  Function : Return TRUE if any all gap columns exist in MSA
  Args     : none
  Returns  : TRUE if any all gap columns, FALSE if not

=cut

sub any_allgap_columns {
  my ($self) = @_;

  $self->_check_msa();
  return _c_any_allgap_columns( $self->{esl_msa}, "-_.~" ); # gap string of "-_.~" only relevant if MSA is not digitized 
}

#-------------------------------------------------------------------------------

=head2 average_id

  Title    : average_id
  Incept   : EPN, Fri Feb  1 06:59:50 2013
  Usage    : $msaObject->average_id($max_nseq)
  Function : Calculate and return average fractional identity of 
           : all pairs of sequences in msa. If more than $max_nseq
           : sequences exist in the seed, an average is computed
           : over a stochastic sample (the sample and thus the 
           : result with vary over multiple runs).
  Args     : max number of sequences for brute force calculation
  Returns  : average percent id of all seq pairs or a sample
  
=cut

sub average_id {
  my ( $self, $max_nseq ) = @_;

  $self->_check_msa();
  if ( !defined $max_nseq ) {
    $max_nseq = 100;
  }

  # average percent id is expensive to calculate, so we set it once calc'ed
  if ( !defined $self->{average_id} ) {
    $self->{average_id} = _c_average_id( $self->{esl_msa}, $max_nseq );
  }
  return $self->{average_id};
}

#-------------------------------------------------------------------------------

=head2 get_sqstring_aligned

  Title    : get_sqstring_aligned
  Incept   : EPN, Fri May 24 11:02:21 2013
  Usage    : $msaObject->get_sq_aligned()
  Function : Return an aligned sequence from an MSA.
  Args     : index of sequence you want
  Returns  : aligned sequence index idx

=cut

sub get_sqstring_aligned {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  return _c_get_sqstring_aligned( $self->{esl_msa}, $idx );
}

#-------------------------------------------------------------------------------

=head2 get_sqstring_unaligned

  Title    : get_sqstring_unaligned
  Incept   : EPN, Fri May 24 11:02:21 2013
  Usage    : $msaObject->get_sqstring_unaligned()
  Function : Return an unaligned sequence from an MSA.
  Args     : index of sequence you want
  Returns  : unaligned sequence, index idx

=cut

sub get_sqstring_unaligned {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  return _c_get_sqstring_unaligned( $self->{esl_msa}, $idx );
}

#-------------------------------------------------------------------------------

=head2 get_sqlen

  Title    : get_sqlen
  Incept   : EPN, Fri Feb  1 16:56:24 2013
  Usage    : $msaObject->get_sqlen()
  Function : Return unaligned sequence length of 
           : sequence <idx>.
  Args     : index of sequence you want length of
  Returns  : unaligned sequence length of sequence idx

=cut

sub get_sqlen {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  return _c_get_sqlen( $self->{esl_msa}, $idx );
}

#-------------------------------------------------------------------------------

=head2 get_column

  Title    : get_column
  Incept   : EPN, Tue Feb 18 09:22:05 2014
  Usage    : $msaObject->get_column($apos)
  Function : Return a string that is column $apos of the alignment,
           : where apos runs 1..alen. So to get the first column
           : of the alignment pass in 0 for $apos, pass in 1 for the
           : second column, etc.
  Args     : $apos: [1..alen] the desired column of the alignment 
  Returns  : $column: column $apos of the alignment, as a string

=cut

sub get_column {
  my ( $self, $apos ) = @_;

  $self->_check_msa();
  $self->_check_ax_apos($apos);

  return _c_get_column( $self->{esl_msa}, $apos );
}
#-------------------------------------------------------------------------------

=head2 count_residues

  Title    : count_residues
  Incept   : March 5, 2013
  Usage    : $msaObject->count_residues()
  Function : Calculate and return total sequence length
           : in the MSA.
  Args     : none
  Returns  : total sequence length

=cut

sub count_residues
{
  my ($self) = @_;

  $self->_check_msa();
  if(!defined $self->{nresidue})
  {
    $self->{nresidue} = _c_count_residues($self->{esl_msa});
  }
  return $self->{nresidue};
}

#-------------------------------------------------------------------------------

=head2 average_sqlen

  Title    : average_sqlen
  Incept   : EPN, Fri Feb  1 06:59:50 2013
  Usage    : $msaObject->average_sqlen()
  Function : Calculate and return average unaligned sequence length
           : in the MSA.
  Args     : none
  Returns  : average unaligned sequence length

=cut

sub average_sqlen {
  my ($self) = @_;

  $self->_check_msa();

  # this could be expensive to calculate if nseq is very high, so we store it
  if ( !defined $self->{average_sqlen} ) {
    $self->{average_sqlen} = _c_average_sqlen( $self->{esl_msa} );
  }
  return $self->{average_sqlen};
}

#-------------------------------------------------------------------------------

=head2 calc_and_write_bp_stats

  Title    : calc_and_write_bp_stats
  Incept   : EPN, Fri Feb  1 10:29:11 2013
  Usage    : $msaObject->calc_and_write_bp_stats($fileLocation)
  Function : Calculate per-basepair stats for an RNA alignment
           : with SS_cons information and output it.
  Args     : name of requested output file 
  Returns  : void

=cut

sub calc_and_write_bp_stats {
  my ( $self, $fileLocation ) = @_;

# TODO: get this working with errbuf, I couldn't get this to work though:
# my $errbuf = "";
#my $status = _c_calc_and_write_bp_stats($self->{esl_msa}, $fileLocation, $errbuf);
  $self->_check_msa();
  my $status = _c_calc_and_write_bp_stats( $self->{esl_msa}, $fileLocation );
  if ( $status != $ESLOK ) {
    croak "ERROR: unable to calculate and write bp stats";
  }

  return;
}

#-------------------------------------------------------------------------------

=head2 rfam_qc_stats

  Title    : rfam_qc_stats
  Incept   : EPN, Fri Feb  1 10:29:11 2013
  Usage    : $msaObject->rfam_qc_stats($fam_outfile, $seq_outfile, $bp_outfile)
  Function : Calculate per-family, per-sequence and per-basepair stats for a
           : SS_cons annotated RNA alignment and output it. 
           : See 'Purpose' section of _c_rfam_qc_stats() C function in
           : MSA.c for more information.
  Args     : fam_outfile: name of output file for per-family stats
           : seq_outfile: name of output file for per-sequence stats
           : bp_outfile:  name of output file for per-basepair stats
  Returns  : void

=cut

sub rfam_qc_stats {
  my ( $self, $fam_outfile, $seq_outfile, $bp_outfile ) = @_;

  $self->_check_msa();
  my $status = _c_rfam_qc_stats( $self->{esl_msa}, $fam_outfile, $seq_outfile, $bp_outfile);
  if ( $status != $ESLOK ) {
    croak "ERROR: unable to calculate rfam qc stats";
  }

  return;
}

#-------------------------------------------------------------------------------

=head2 addGF

  Title    : addGF
  Incept   : EPN, Fri Feb  1 17:43:38 2013
  Usage    : $msaObject->addGF($tag, $value)
  Function : Add GF tag/value to a C ESL_MSA object.
  Args     : $tag:   two letter tag 
           : $value: text for the line
  Returns  : void

=cut

sub addGF {
  my ( $self, $tag, $value ) = @_;

  $self->_check_msa();
  my $status = _c_addGF( $self->{esl_msa}, $tag, $value );
  if ( $status != $ESLOK ) { croak "ERROR: unable to add GF annotation"; }
  return;
}

#-------------------------------------------------------------------------------

=head2 addGS

  Title    : addGS
  Incept   : EPN, Fri Feb  1 17:43:38 2013
  Usage    : $msaObject->addGF($tag, $value)
  Function : Add GS tag/value for a specific sequence 
           : to a C ESL_MSA object.
  Args     : $tag:   two letter tag 
           : $value: text for the line
           : $sqidx: seq index to add GS for
  Returns  : void

=cut

sub addGS {
  my ( $self, $tag, $value, $sqidx ) = @_;

  $self->_check_msa();
  my $status = _c_addGS( $self->{esl_msa}, $sqidx, $tag, $value );
  if ( $status != $ESLOK ) { croak "ERROR: unable to add GS annotation"; }
  return;
}

#-------------------------------------------------------------------------------

=head2 addGC_identity

  Title    : addGC_identity
  Incept   : EPN, Fri Nov  8 09:31:00 2013
  Usage    : $msaObject->addGCidentity($use_)
  Function : Add GC annotation to an ESL_MSA with
           : tag 'ID' with a '*' indicating columns
           : for which all sequences have an identical
           : residue, or instead of '*' use the
           : residue itself, if $use_res is 1.
  Args     : $use_res: '1' to use residue for marking 100% 
           :          identical columns, '0' to use '*'.
  Returns  : void

=cut

sub addGC_identity {
  my ( $self, $use_res ) = @_;

  $self->_check_msa();
  my $status = _c_addGC_identity( $self->{esl_msa}, $use_res );
  if ( $status != $ESLOK ) { croak "ERROR: unable to add GC ID annotation"; }
  return;
}

#-------------------------------------------------------------------------------

=head2 weight_GSC

  Title    : weight_GSC
  Incept   : EPN, Fri May 24 10:42:44 2013
  Usage    : $msaObject->weight_GSC($tag, $value)
  Function : Compute and annotate MSA with sequence weights using the 
           : GSC algorithm. Dies with croak if something goes wrong.
  Args     : none
  Returns  : void

=cut

sub weight_GSC {
  my ( $self ) = @_;

  $self->_check_msa();
  my $status = _c_weight_GSC( $self->{esl_msa} );
  if ( $status != $ESLOK ) { croak "ERROR: unable to calculate GSC weights"; }
  return;
}

#-------------------------------------------------------------------------------

=head2 free_msa

  Title    : free_msa
  Incept   : EPN, Mon Jan 28 11:06:18 2013
  Usage    : $msaObject->free_msa()
  Function : Frees an MSA->{$esl_msa} object
  Args     : name of output file 
  Returns  : void

=cut

sub free_msa {
  my ($self) = @_;

  # don't call _check_msa, if we don't have it, that's okay
  _c_free_msa( $self->{esl_msa} );
  return;
}

#-------------------------------------------------------------------------------

=head2 revert_to_original

  Title    : revert_to_original
  Incept   : EPN, Sat Feb  2 14:53:27 2013
  Usage    : $msaObject->revert_to_original()
  Function : Frees current $msaObject->{esl_msa} object
             and rereads it from $msaObject->{path}
  Args     : none
  Returns  : void

=cut

sub revert_to_original {
  my ($self) = @_;

  if ( !defined $self->{path} ) {
    croak "trying to revert_to_original but path not set";
  }
  $self->free_msa;
  $self->read_msa;
  return;
}

#-------------------------------------------------------------------------------

=head2 weight_id_filter

  Title     : weight_id_filter
  Incept    : March 1, 2013
  Usage     : $msaObject->weight_id_filter($idf)
  Function  : performs id weight filtering on the msa object
  Args      : Identity floating point from 0 to 1, id ratio above which will be filtered
  Returns   : void

=cut

sub weight_id_filter
{
  my($self, $idf) = @_;
  
  my $msa_in = $self->{esl_msa};
  my $msa_out = _c_msaweight_IDFilter($msa_in, $idf);
  
  $self->{esl_msa} = $msa_out;
  
  _c_free_msa($msa_in);
  
  return;
}

#-------------------------------------------------------------------------------

=head2 filter_msa_subset_target_nseq

  Title     : filter_msa_subset_target_nseq
  Incept    : EPN, Thu Nov 21 13:40:52 2013
  Usage     : $msaObject->filter_msa_subset_target_nseq($idf)
  Function  : Filter a subset of sequences in an MSA such that no
            : two sequences in the filtered subset are more than $idf
            : fractionally identical, where $idf is the maximum value
            : that results in <= $nseq sequences (within 0.01, that is, 
            : $idf + 0.01 gives > $nseq sequences).
            : $idf is found by a binary search.
  Args      : $usemeAR:  [0..$i..$self->nseq]: '1' if we should consider sequence $i, else ignore it.
            :            set to all '1' to consider all sequences in the msa.
            : $nseq:     fractional identity threshold no pair of seqs in $keepmeAR will exceed
            : $keepmeAR: [0..$i..$self->nseq]: '1' if seq $i survives the filtering.
            :            note that $keepmeAR->[$i] can only be '1' if $usemeAR->[$i] is also '1'.
  Returns   : $idf:   the fractional identity used to fill keepmeAR
            : $nkeep: number of '1' in keepmeAR

=cut

sub filter_msa_subset_target_nseq
{
  my($self, $usemeAR, $nseq, $keepmeAR) = @_;
  
  # binary search for max fractional id ($f_cur) that results in $nseq sequences
  # we'll filter the alignment such that no two seqs are more than $f_cur similar to each other
  # (or as close as we can get to $nseq by minimal change of 0.01)
  # initializations
  my $f_min = 0.01;
  my $f_opt = 0.01;
  my $f_prv = 1.0;
  my $f_cur = $f_min;
  my ($i, $n);
  my $diff = abs($f_prv - $f_cur);
  while($diff > 0.00999) { # while abs($f_prv - $f_cur) > 0.00999
    # filter based on percent identity
    $n = $self->filter_msa_subset($usemeAR, $f_cur, $keepmeAR);
          
    $f_prv = $f_cur;
    # adjust $f_cur for next round based on how many seqs we have
    if($n > $nseq) { # too many seqs, lower $f_cur
      $f_cur -= ($diff / 2.); 
    }
    else { # too few seqs, raise $f_cur
      if($f_cur > $f_opt) { $f_opt = $f_cur; }
      $f_cur += ($diff / 2.); 
    }
          
    # round to nearest percentage point (0.01)
    $f_cur = (int(($f_cur * 100) + 0.5)) / 100;
          
    if($f_cur < $f_min) { croak "filter_msa_subset_target_nseq: couldn't reach $nseq sequences, with fractional id > $f_min\n"; }
    $diff = abs($f_prv - $f_cur);
  }    
  # $f_opt is our optimal fractional id, the max fractional id that gives <= $nseq seqs
  # call filter_msa_subset once more, to redefine keepmeA
  $n = $self->filter_msa_subset($usemeAR, $f_opt, $keepmeAR);

  return($f_opt, $n);
}

#-------------------------------------------------------------------------------

=head2 filter_msa_subset

  Title     : filter_msa_subset
  Incept    : EPN, Thu Nov 21 13:30:15 2013
  Usage     : $nkept = $msaObject->filter_msa_subset($usmeAR, $idf, $keepmeAR)
  Function  : Filter a subset of sequences in an MSA such that no
            : two sequences in the filtered subset are more than $idf
            : fractionally identical.
            : Similar to weight_id_filter() except does not create a new
            : MSA of only the filtered set, and this function is flexible
            : to only considering a subset of the passed in alignment.
  Args      : $usemeAR:  [0..$i..$self->nseq]: '1' if we should consider sequence $i, else ignore it.
            : $idf:      fractional identity threshold no pair of seqs in $keepmeAR will exceed
            : $keepmeAR: [0..$i..$self->nseq]: '1' if seq $i survives the filtering.
            :            note that $keepmeAR->[$i] can only be '1' if $usemeAR->[$i] is also '1'.
  Returns   : Number of sequences that are '1' in $keepmeAR upon exit.

=cut

sub filter_msa_subset
{
  my($self, $usemeAR, $idf, $keepmeAR) = @_;
  
  my ($i, $j, $pid);  # counters and a pid (pairwise identity) value
  my $nseq = $self->nseq;
  my $nkeep = 0;

  # copy usemeA to keepmeA
  for($i = 0; $i < $nseq; $i++) { 
    $keepmeAR->[$i] = $usemeAR->[$i]; 
    if($keepmeAR->[$i]) { $nkeep++; }
  }
  
  for($i = 0; $i < $nseq; $i++) { 
    if($keepmeAR->[$i]) { # we haven't removed it yet
      for($j = $i+1; $j < $nseq; $j++) { # for every other seq that ... 
        if($keepmeAR->[$j]) { # ... we haven't removed yet
          $pid = _c_pairwise_identity($self->{esl_msa}, $i, $j); # get fractional identity
          if($pid > $idf) { 
            $keepmeAR->[$j] = 0; # remove it
            $nkeep--;
          }
        }
      }
    }
  }

  return $nkeep;
}

#-------------------------------------------------------------------------------

=head2 alignment_coverage

  Title     : alignment_coverage_id
  Incept    : March 5, 2013
  Usage     : $msaObject->alignment_coverage_id()
  Function  : determine coverage ratios of msa
  Args      : None
  Returns   : Success:
                Array from 0 to msa->alen, contains decimals from 0 to 1
                representing coverage ratio of that msa position
              Failure:
                Nothing

=cut

sub alignment_coverage
{
  my ($self, $idf) = @_;
  
  my $msa_in = $self->{esl_msa};
  
  my @output = _c_percent_coverage($msa_in);
  
  return @output;
}

#-------------------------------------------------------------------------------

=head2 count_msa

  Title     : count_msa
  Incept    : EPN, Fri Jul  5 13:46:51 2013
  Usage     : $msaObject->count_msa()
  Function  : Count residues and basepairs in an MSA
  Args      : None
  Returns   : 

=cut

sub count_msa
{
  my ($self) = @_;

  $self->_check_msa();
  $self->_c_count_msa($self->{esl_msa}, 0, 0);

  return;
}

#-------------------------------------------------------------------------------

=head2 pairwise_identity

  Title     : pairwise_identity
  Incept    : EPN, Wed Aug 21 10:19:07 2013
  Usage     : $msaObject->pairwise_identity($i, $j)
  Function  : Return fractional identity between seqs $i and $j in the MSA
  Args      : None
  Returns   : fraction identity of i and j

=cut

sub pairwise_identity
{
  my ($self, $i, $j) = @_;

  $self->_check_msa();
  return _c_pairwise_identity($self->{esl_msa}, $i, $j);
}

#-------------------------------------------------------------------------------

=head2 check_if_prefix_added_to_sqnames

  Title     : check_if_prefix_added_to_sqnames
  Incept    : EPN, Fri Nov  1 15:15:12 2013
  Usage     : $msaObject->check_if_prefix_added_to_sqnames
  Function  : Return '1' if it appears that prefixes were added
            : to all sqnames in the MSA, by Easel to avoid two
            : sequences having an identical name.
  Args      : None
  Returns   : '1' of '0'

=cut

sub check_if_prefix_added_to_sqnames
{
  my ($self) = @_;

  $self->_check_msa();

  # we'll return TRUE only if the following 2 criteria are satisfied:
  # 1) all seqs begin with numerical prefix: \d+\|, e.g. "0001|"
  # 2) at least one duplicated seq name exists AFTER removing numerical prefixes
  my @nameA = ();       # we'll keep track of all names we've seen thus far, to check for dups with
  my $prefix_added = 1; # we'll set to FALSE once we find a counterexample
  my $found_dup    = 0; # we'll set to TRUE once we find a dup
  for(my $i = 0; $i < $self->nseq; $i++) { 
    my $sqname = $self->get_sqname($i);
    if($sqname !~ m/^\d+\|/) { 
      $prefix_added = 0;
      last;
    }
    $sqname =~ s/^\d+\|//;
    if(! $found_dup) { # check if we have a duplicate of this name
      foreach my $sqname2 (@nameA) { 
        if($sqname eq $sqname2) { 
          $found_dup = 1;
          last;
        }
      }
      push(@nameA, $sqname);
    }
  }

  my $ret_val = ($prefix_added && $found_dup) ? 1 : 0;
  return $ret_val;
}

#-------------------------------------------------------------------------------

=head2 remove_prefix_from_sqname

  Title     : remove_prefix_from_sqname
  Incept    : EPN, Fri Nov  1 15:25:27 2013
  Usage     : $msaObject->remove_prefix_from_sqname
  Function  : Remove a numerical prefix from a sequence name.
            : Only meant to be used for MSAs for which 
            : check_if_prefix_added_to_sqnames returned
            : TRUE (which had numerical prefixes added to
            : sqnames to avoid duplicate names).
  Args      : $sqname
  Returns   : $sqname with numerical prefix removed.
  Dies      : If $sqname does not have a numerical prefix,
            : this means caller does not know what its doing.
=cut

sub remove_prefix_from_sqname
{
  my ($self, $sqname) = @_;

  if($sqname !~ m/^\d+\|/) { die "ERROR trying to remove numerical prefix from $sqname, but it doesn't have one"; }
  $sqname =~ s/^\d+\|//;

  return $sqname;
}

#-------------------------------------------------------------------------------

=head2 clone_msa

  Title     : clone_msa
  Incept    : EPN, Thu Nov 21 09:38:23 2013
  Usage     : $newmsaObject = $msaObject->clone_msa()
  Function  : Creates a new MSA, a duplicate of $self.
  Args      : None
  Returns   : $new_msa: a new Bio::Easel::MSA object, a duplicate of $self

=cut

sub clone_msa
{
  my ($self) = @_;

  $self->_check_msa();

  my $new_esl_msa = _c_clone_msa($self->{esl_msa});

  my $new_msa = Bio::Easel::MSA->new({
    esl_msa => $new_esl_msa,
  });

  return $new_msa;
}

#-------------------------------------------------------------------------------

=head2 reorder_all

  Title     : reorder_msa_all
  Incept    : EPN, Mon Feb  3 14:32:59 2014
  Usage     : $msaObject->reorder_all
  Function  : Reorder all sequences in an MSA by swapping pointers.
  Args      : $nameorderAR: [0..i..nseq-1] ref to array with names
            :               of sequences in preferred order.
  Returns   : void
  Dies      : If not all sequences are listed exactly once in $nameorderAR.
=cut

sub reorder_all
{
  my ($self, $nameorderAR) = @_;

  $self->_check_msa();
  $self->_check_index();

  my $nseq = $self->nseq();
  if(scalar(@{$nameorderAR}) != $nseq) { croak "ERROR, reorder_all() wrong num seqs in input array"; }

  # initialize idxorderA
  my @idxorderA = ();
  my @coveredA  = (); # coveredA[$i] is '1' if seq $i has already been reordered, else '0'
  my $i;
  for($i = 0; $i < $nseq; $i++) { $coveredA[$i] = 0; }
  for($i = 0; $i < $nseq; $i++) { 
    my $seqidx = _c_get_sqidx($self->{esl_msa}, $nameorderAR->[$i]);
    if($seqidx == -1)           { croak "ERROR, reorder_all() unable to find sequence $nameorderAR->[$i]"; }
    if($coveredA[$seqidx] != 0) { croak "ERROR, reorder_all() has sequence $nameorderAR->[$i] listed twice"; }
    $idxorderA[$i] = $seqidx;
  }

  _c_reorder($self->{esl_msa}, \@idxorderA);

  return;
}

#-------------------------------------------------------------------------------

=head2 sequence_subset

  Title     : sequence_subset
  Incept    : EPN, Thu Nov 14 10:24:50 2013
  Usage     : $newmsaObject = $msaObject->sequence_subset($usemeAR)
  Function  : Create a new MSA containing a subset of the
            : sequences in a passed in MSA. 
            : Keep any sequence with index i if 
            : usemeAR->[i] == 1, else remove it.
            : All gap columns will not be removed from the MSA,
            : caller may want to do that immediately with
            : remove_all_gap_columns().
  Args      : $usemeAR: [0..i..nseq-1] ref to array with value
            :           '1' to keep seq i, '0' to remove it
  Returns   : $new_msa: a new Bio::Easel::MSA object, with 
            :           a subset of the sequences in $self.
=cut

sub sequence_subset
{
  my ($self, $usemeAR) = @_;

  $self->_check_msa();

  my $new_esl_msa = _c_sequence_subset($self->{esl_msa}, $usemeAR);

  # create new Bio::Easel::MSA object from $new_esl_msa
  my $new_msa = Bio::Easel::MSA->new({
    esl_msa => $new_esl_msa,
  });

  return $new_msa;
}


#-------------------------------------------------------------------------------

=head2 sequence_subset_given_names

  Title     : sequence_subset_given_names
  Incept    : EPN, Tue Feb  4 09:17:37 2014
  Usage     : $newmsaObject = $msaObject->sequence_subset_given_names($nameAR)
  Function  : Create a new MSA containing a subset of the
            : sequences in a passed in MSA. 
            : Keep any sequence with name listed in @{$nameAR}
            : All gap columns will not be removed from the MSA,
            : caller may want to do that immediately with
            : remove_all_gap_columns().
  Args      : $nameAR: ref to array with list of names of seqs to keep
  Returns   : $new_msa: a new Bio::Easel::MSA object, with 
            :           a subset of the sequences in $self.
=cut

sub sequence_subset_given_names
{
  my ($self, $nameAR) = @_;

  $self->_check_msa();
  $self->_check_index();

  # step 1: determine which sequences to keep
  my @usemeA = ();
  my $orig_nseq = $self->nseq();
  my $sub_nseq  = scalar(@{$nameAR});
  my $i;
  for($i = 0; $i < $orig_nseq; $i++) { $usemeA[$i] = 0; }
  for($i = 0; $i < $sub_nseq;  $i++) { 
    my $seqidx = _c_get_sqidx($self->{esl_msa}, $nameAR->[$i]);    
    if($seqidx == -1)         { croak "ERROR, sequence_subset_and_reorder() unable to find sequence $nameAR->[$i]"; }
    if($usemeA[$seqidx] != 0) { croak "ERROR, sequence_subset_and_reorder() has sequence $nameAR->[$i] listed twice"; }
    $usemeA[$seqidx] = 1; 
  }
  my $new_esl_msa = _c_sequence_subset($self->{esl_msa}, \@usemeA);

  # create new Bio::Easel::MSA object from $new_esl_msa
  my $new_msa = Bio::Easel::MSA->new({
    esl_msa => $new_esl_msa,
  });

  return $new_msa;
}

#-------------------------------------------------------------------------------

=head2 sequence_subset_and_reorder

  Title     : sequence_subset_and_reorder
   Incept    : EPN, Tue Feb  4 08:54:13 2014
  Usage     : $newmsaObject = $msaObject->sequence_subset_and_reorder($nameorderAR)
  Function  : Create a new MSA containing a subset of the
            : sequences in a passed in MSA reordered to
            : the order in @{$nameorderAR}. 
            : @{$nameorderAR} contains a subset of the sequences
            : in the MSA with no duplicates.
            : All gap columns will not be removed from the MSA,
            : caller may want to do that immediately with
            : remove_all_gap_columns().
  Args      : $nameorderAR: names of sequences to put in subset in desired order.
  Returns   : $new_msa: a new Bio::Easel::MSA object, with 
            :           a subset of the sequences in $self,
            :           reordered to the order in @{$nameorderAR}.
=cut

sub sequence_subset_and_reorder
{
  my ($self, $nameorderAR) = @_;

  my $new_msa = $self->sequence_subset_given_names($nameorderAR);
  $new_msa->reorder_all($nameorderAR);

  return $new_msa;
}
#-------------------------------------------------------------------------------

=head2 column_subset

  Title     : column_subset
  Incept    : EPN, Thu Nov 14 10:24:50 2013
  Usage     : $msaObject->column_subset($usemeAR)
  Function  : Remove a subset of columns from an MSA.
            : If the column for one half of a SS_cons basepair is 
            : removed but not the other half, the basepair
            : will be removed from SS_cons.
  Args      : $usemeAR: [0..i..alen-1] ref to array with value
            :           '1' to keep column i, '0' to remove it
  Returns   : void
=cut

sub column_subset
{
  my ($self, $usemeAR) = @_;

  $self->_check_msa();

  _c_column_subset($self->{esl_msa}, $usemeAR);

  return;
}

#-------------------------------------------------------------------------------

=head2 remove_all_gap_columns

  Title     : remove_all_gap_columns
  Incept    : EPN, Thu Nov 14 13:39:42 2013
  Usage     : $msaObject->remove_all_gap_columns
  Function  : Remove all gap columns from an MSA.
            : If the column for one half of a SS_cons basepair is 
            : removed but not the other half, the basepair
            : will be removed from SS_cons.
  Args      : $consider_rf: '1' to not delete any nongap RF column, else '0'
  Returns   : void
  Dies      : upon an error with croak
=cut

sub remove_all_gap_columns
{
  my ($self, $consider_rf) = @_;

  $self->_check_msa();

  _c_remove_all_gap_columns($self->{esl_msa}, $consider_rf);

  return;
}

#-------------------------------------------------------------------------------

=head2 remove_rf_gap_columns

  Title     : remove_rf_gap_columns
  Incept    : EPN, Thu Nov 21 10:07:07 2013
  Usage     : $msaObject->remove_rf_gap_columns
  Function  : Remove any column from an MSA that is a gap (exists in $gapstr)
            : in the GC RF annotation of the MSA.
  Args      : $gapstring: string of characters to consider as gaps,
            :             if undefined we use '.-~'
  Returns   : void
  Dies      : upon an error with croak
=cut
    
sub remove_rf_gap_columns
{
  my ($self, $gapstring) = @_;

  $self->_check_msa();
  if(! defined $gapstring) { $gapstring = ".-~"; }
  
  if(! $self->has_rf) { croak "Trying to remove RF gap columns, but no RF annotation exists in the MSA"; }
  my $rf = $self->get_rf;
  my @rfA = split("", $rf);
  my $rflen = scalar(@rfA);
  if($self->alen != $rflen) { croak "RF length $rflen not equal to alignment length"; }

  my @usemeA = ();
  for(my $apos = 0; $apos < $rflen; $apos++) { 
    $usemeA[$apos] = ($rfA[$apos] =~ m/[\Q$gapstring\E]/) ? 0 : 1;
  }      
  
  _c_column_subset($self->{esl_msa}, \@usemeA);
  
  return;
}

#-------------------------------------------------------------------------------

=head2 find_divergent_seqs_from_subset

  Title     : find_divergent_seqs_from_subset
  Incept    : EPN, Thu Nov 21 08:41:06 2013
  Usage     : $msaObject->find_divergent_seqs_from_subset
  Function  : Given a subset of sequences, find all other sequences
            : not in the subset that are <= $id_thr fractionally
            : identical to *all* seqs in the subset.
  Args      : $subsetAR: [0..$i..$msa->nseq-1] '1' if sequence i is in the subset, else 0
            : $id_thr:   fractional identity threshold
            : $divAR:    FILLED HERE: [0..$i..$msa->nseq-1] ref to array, '1'  if a sequence
            :            is <= $id_thr fractionally identical to all seqs in subset, else 0
            : $nnidxAR:  FILLED HERE: [0..$i..$msa->nseq-1] ref to array, index of nearest
            :            neighbor (index that is '1' in subsetAR) if $divAR->[$i] == 1, else -1
            : $nnfidAR:  FILLED HERE: [0..$i..$msa->nseq-1] ref to array, index of fractional
            :            identity to nearest neighbor, if $divAR->[$i] == 1, else 0.
            : Example: if sequence idx 5 is $id_thr or less fractionally identical to all
            :          sequences in the subset, but closest to sequence index 11 at 0.73,
            :          then $divAR->[5] = 1, $nnidxAR->[5] = 11, $nnfidAR->[5] = 0.73.
  Returns   : Number of divergent seqs found. This will also be the size of @{$divAR}, @{$nnidxAR} and @{$nnfidAR}
  Dies      : if no sequences exist in $subsetAR, or any indices are invalid
            : with croak
=cut

sub find_divergent_seqs_from_subset
{
  my ($self, $subsetAR, $id_thr, $divAR, $nnidxAR, $nnfidAR) = @_;

  $self->_check_msa();

  my $nseq = $self->nseq;
  my $ndiv = 0;
  my $nsubset = 0;
  for(my $i = 0; $i < $self->nseq; $i++) { 
    my $iamdivergent = 0;
    my $maxid  = -1.;
    my $maxidx = -1;
    if(! $subsetAR->[$i]) { 
      $iamdivergent = 1; # until proven otherwise
      for(my $j = 0; $j < $self->nseq; $j++) {
        if($subsetAR->[$j]) { 
          my $id = _c_pairwise_identity($self->{esl_msa}, $i, $j);
          if($id > $id_thr) { $iamdivergent = 0; $j = $self->nseq+1; } # setting j this way breaks us out of the loop
          if($id > $maxid)  { $maxidx = $j; $maxid = $id; }
        }
      }
    }
    else { 
      $nsubset++; 
    }
    if($iamdivergent) { 
      if(defined $divAR)   { $divAR->[$i]   = 1; }
      if(defined $nnidxAR) { $nnidxAR->[$i] = $maxidx; }
      if(defined $nnfidAR) { $nnfidAR->[$i] = $maxid;  }
      $ndiv++;
    }
    else { 
      if(defined $divAR)   { $divAR->[$i]   = 0;  }
      if(defined $nnidxAR) { $nnidxAR->[$i] = -1; }
      if(defined $nnfidAR) { $nnfidAR->[$i] = 0.; }
    }
  }

  if($nsubset == 0) { die "ERROR in find_most_divergent_seq_from_subset(), no seqs in subset"; }
  return $ndiv;
}


#-------------------------------------------------------------------------------

=head2 find_most_divergent_seq_from_subset

  Title     : find_most_divergent_seq_from_subset
  Incept    : EPN, Thu Nov 21 08:41:06 2013
  Usage     : $msaObject->find_most_divergent_seq_from_subset
  Function  : Given a subset of sequences, find all other sequences
            : not in the subset that are <= $id_thr fractionally
            : identical to *all* seqs in the subset.
  Args      : $subsetAR: [0..$i..$msa->nseq-1] '1' if sequence i is in the subset, else 0
  Returns   : $idx: Index of sequence in $msa that is most divergent from all seqs in
            :       subsetAR, that is, the sequence for which the fractional identity
            :       to its closest neighbor in subsetAR is minimized.
            : $fid: fractional identity of idx to its closest neighbor in msa
            : $nnidx: idx of <$idx>s nearest neighbor in $msa
  Dies      : if no sequences exist in $subsetAR, or any indices are invalid
            : with croak
=cut

sub find_most_divergent_seq_from_subset
{
  my ($self, $subsetAR) = @_;

  $self->_check_msa();
  my $nsubset = 0;

  my $nseq = $self->nseq;
  my $min_max_id = 1.0;
  my $ret_idx = -1;
  my $ret_nnidx = -1;
  for(my $i = 0; $i < $self->nseq; $i++) { 
    if(! $subsetAR->[$i]) { 
      my $max_id = 0.;
      my $max_idx = -1;
      for(my $j = 0; $j < $self->nseq; $j++) {
        if($subsetAR->[$j]) { 
          my $id = _c_pairwise_identity($self->{esl_msa}, $i, $j);
          if($id > $min_max_id) { $j = $self->nseq+1; } # setting j this way breaks us out of the loop
          if($id > $max_id)     { $max_id = $id; $max_idx = $j; }
        }
      }
      if($max_id < $min_max_id) { 
        $ret_idx    = $i;
        $min_max_id = $max_id;
        $ret_nnidx  = $max_idx;
      }
    }
    else { 
      $nsubset++;
    }
  }

  if($nsubset == 0) { die "ERROR in find_most_divergent_seq_from_subset(), no seqs in subset"; }

  return ($ret_idx, $min_max_id, $ret_nnidx);
}

#-------------------------------------------------------------------------------

=head2 avg_min_max_pid_to_seq

  Title    : avg_min_max_pid_to_seq
  Incept   : EPN, Fri Nov 22 08:53:56 2013
  Usage    : $msaObject->avg_min_max_pid_to_seq($i);
  Function : Calculates the average, minimum and maximum 
           : fractional identity of seq $idx to all other seqs.
           : If optional parameter, $usemeAR is passed in
           : then only consider sequences $j for which
           : $usemeAR->[$j] is '1' (except $idx even if
           : $usemeAR->[$idx] is '1').
  Args     : $idx:     index of sequence we want avg/min/max pid to
           : $usemeAR: OPTIONAL: if defined: 
           :           [0..$j..nseq-1]: '1' if we should consider
           :           seq $j in calculation of avg/min/max.
  Returns  : $avg_pid: average fractional id b/t $idx and all other seqs
           : $min_pid: minimum fractional id b/t $idx and all other seqs
           : $min_idx: index of seq that gives $min_pid to $idx
           : $max_pid: minimum fractional id b/t $idx and all other seqs
           : $max_idx: index of seq that gives $max_pid to $idx

=cut

sub avg_min_max_pid_to_seq {
  my ($self, $idx, $usemeAR) = @_;
  
  my $pid;
  my $avg_pid = 0.;
  my $n = 0;
  my $min_pid = 1.1;
  my $min_idx = -1;
  my $max_pid = -1.;
  my $max_idx = -1;

  $self->_check_msa();

  for(my $i = 0; $i < $self->nseq; $i++) { 
    if($i != $idx && (! defined $usemeAR || $usemeAR->[$i])) { 
      $pid = _c_pairwise_identity($self->{esl_msa}, $idx, $i); # get fractional identity
      if($pid < $min_pid) { $min_pid = $pid; $min_idx = $i; }
      if($pid > $max_pid) { $max_pid = $pid; $max_idx = $i; }
      $avg_pid += $pid;
      $n++;
    }
  }
  if($n == 0) { croak "ERROR Bio::Easel::MSA::avg_min_max_pid_to_seq(): no sequences to compare seq $idx to"; }
  $avg_pid /= $n;

  return ($avg_pid, $min_pid, $min_idx, $max_pid, $max_idx);
}


#-------------------------------------------------------------------------------

=head2 create_from_string

  Title     : create_from_string
  Incept    : EPN, Fri Nov 29 07:03:12 2013
  Usage     : $newmsaObject = Bio::Easel::MSA::create_from_string($msa_str, $format)
  Function  : Create a new MSA from a string that is a properly 
            : formatted MSA in format <$format>. If $format is undefined
            : we will use Easels format autodetection capability.
            : If <$do_digitize> we will digitize the alignment before
            : returning, if this is undefined, we do it anyway since
            : most of the BioEasel MSA code requires a digitized MSA.
            : 
  Args      : $msa_str:     the string that is an MSA
            : $format:      string defining format of $msa_str.
            :               valid format strings are: 
            :               "stockholm", "pfam", "a2m", "phylip", "phylips", "psiblast",
            :               "selex", "afa", "clustal", "clustallike", "unknown", or undefined
            : $abc:         string defining alphabet of MSA, valid options:
            :               "amino", "rna", "dna", "coins", "dice", "custom"
            : $do_digitize: '1' to digitize alignment, '0' not do, use '1' unless you know 
            :               what you are doing.
  Returns   : $new_msa: a new Bio::Easel::MSA object, created
            :           from $msa_str
=cut

sub create_from_string
{
  my ($msa_str, $format, $abc, $do_digitize) = @_;

  if(! defined $format) { 
    $format = "unknown";
  }
  if(! defined $abc) { 
    croak "ERROR, alphabet is undefined in create_from_string()"; 
  }
  if($abc ne "amino" && $abc ne "rna" && $abc ne "dna" && $abc ne "coins" && $abc ne "dice" && $abc ne "custom") { 
    croak ("ERROR, alphabet $abc is invalid, valid options are \"amino\", \"rna\", \"dna\", \"coins\", \"dice\", and \"custom\"");
  }
  if(! defined $do_digitize) { # default to TRUE
    $do_digitize = 1; 
  }

  my $new_esl_msa = _c_create_from_string($msa_str, $format, $abc, $do_digitize);

  # create new Bio::Easel::MSA object from $new_esl_msa
  my $new_msa = Bio::Easel::MSA->new({
    esl_msa => $new_esl_msa,
  });

  return $new_msa;
}

#-------------------------------------------------------------------------------

=head2 is_residue

  Title     : is_residue
  Incept    : EPN, Fri Nov 29 17:05:44 2013
  Usage     : $msaObject->is_residue($sqidx, $apos)
  Function  : Return '1' if alignment position $apos of sequence $sqidx
            : is a residue, and '0' if not.
  Args      : $sqidx:   sequence index in MSA
            : $apos:    alignment position [1..alen]
  Returns   : '1' if position $apos of $sqidx is a residue, else '0'
  Dies      : with 'croak' if sequence $sqidx or apos $apos is invalid
=cut

sub is_residue
{
  my ($self, $sqidx, $apos) = @_;

  $self->_check_msa();
  $self->_check_sqidx($sqidx);
  $self->_check_ax_apos($apos);

  return _c_is_residue($self->{esl_msa}, $sqidx, $apos);
}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

=head2 capitalize_based_on_rf

  Title     : capitalize_based_on_rf
  Incept    : EPN, Tue Feb 18 10:44:11 2014
  Usage     : $msaObject->capitalize_based_on_rf
  Function  : Set all residues in nongap RF columns as uppercase,
            : and all gap characters ('.-_') to '-'. Set all residues
            : in gap RF columns to lowercase and all gap characters
            : '.-_' to '.'.
  Args      : none
  Returns   : void
  Dies      : if RF annotation does not exist, or alignment is not in text mode.
=cut

sub capitalize_based_on_rf
{
  my ($self) = @_;

  _c_capitalize_based_on_rf($self->{esl_msa});

  return;
}

#-------------------------------------------------------------------------------

=head2 DESTROY

  Title    : DESTROY
  Incept   : EPN, Mon Jan 28 10:09:55 2013
  Usage    : $msaObject->DESTROY()
  Function : Frees an MSA object
  Args     : none
  Returns  : void

=cut

sub DESTROY {
  my ($self) = @_;

  _c_destroy( $self->{esl_msa} );
  return;
}


#############################
# Internal helper subroutines
#############################

#-------------------------------------------------------------------------------

=head2 _check_msa

  Title    : _check_msa
  Incept   : EPN, Sat Feb  2 13:42:27 2013
  Usage    : Bio::Easel::MSA->_check_msa()
  Function : Reads msa only if it is currently undefined
  Args     : none
  Returns  : void

=cut

sub _check_msa {
  my ($self) = @_;

  if ( !defined $self->{esl_msa} ) {
    $self->read_msa();
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 _check_sqidx

  Title    : _check_seqidx
  Incept   : EPN, Sat Feb  2 13:46:08 2013
  Usage    : $msaObject->_check_sqidx($idx)
  Function : Check if $idx is in range 0..nseq-1,
             if not, croak.
  Args     : $idx
  Returns  : void

=cut

sub _check_sqidx {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  my $nseq = $self->nseq;
  if ( $idx < 0 || $idx >= $nseq ) {
    croak (sprintf("invalid sequence index %d (must be [0..%d])", $idx, $nseq));
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 _check_ax_apos

  Title    : _check_ax_apos
  Incept   : EPN, Fri Nov 29 17:09:11 2013
  Usage    : $msaObject->_check_ax_apos($apos)
  Function : Check if $apos is in range 1..alen,
             if not, croak.
  Args     : $apos
  Returns  : void

=cut

sub _check_ax_apos {
  my ( $self, $apos ) = @_;

  $self->_check_msa();
  my $alen = $self->alen;
  if ( $apos < 1 || $apos > $alen ) {
    croak (sprintf("invalid alignment position %d (must be [1..%d])", $apos, $alen));
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 _check_aseq_apos

  Title    : _check_aseq_apos
  Incept   : EPN, Fri Nov 29 17:09:11 2013
  Usage    : $msaObject->_check_aseq_apos($apos)
  Function : Check if $apos is in range 0..alen-1,
             if not, croak.
  Args     : $apos
  Returns  : void

=cut

sub _check_aseq_apos {
  my ( $self, $apos ) = @_;

  $self->_check_msa();
  my $alen = $self->alen;
  if ( $apos < 0 || $apos >= $alen ) {
    croak (sprintf("invalid alignment position %d (must be [0..%d])", $apos, $alen-1));
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 _check_reqd_format

  Title    : _check_reqd_format
  Incept   : EPN, Thu Jul 18 11:06:02 2013
  Usage    : $msaObject->_check_reqd_format()
  Function : Check if $self->{reqdFormat} is a valid format or 'unknown'
           : if not, croak. Also returns fine if self->{reqdFormat} is not
           : defined.
  Args     : none
  Returns  : void

=cut

sub _check_reqd_format { 
  my ( $self ) = @_;

  if(defined $self->{reqdFormat}) { 
    if($self->{reqdFormat} ne "unknown") { # unknown is valid, so we don't actually do the C check 
      _c_check_reqd_format($self->{reqdFormat}); 
    }
  }
  return;
}

#-------------------------------------------------------------------------------

=head2 _check_index

  Title    : _check_index
  Incept   : EPN, Mon Feb  3 15:17:21 2014
  Usage    : $msaObject->_check_index()
  Function : Check if an MSA has a hash and if not, create one.
  Args     : none
  Returns  : void
  Dies     : If no index exists and unable to create one.

=cut

sub _check_index { 
  my ( $self ) = @_;

  _c_check_index($self->{esl_msa});

  return;
}

#-------------------------------------------------------------------------------

=head2 _c_read_msa
=head2 _c_write_msa
=head2 _c_nseq
=head2 _c_alen
=head2 _c_get_accession
=head2 _c_set_accession
=head2 _c_get_sqname
=head2 _c_set_sqname
=head2 _c_any_allgap_columns
=head2 _c_average_id
=head2 _c_get_sqlen
=head2 _c_average_sqlen
=head2 _c_addGF
=head2 _c_addGS
=head2 _c_count_msa
=head2 _c_bp_is_canonical
=head2 _c_calc_and_write_bp_stats
=head2 dl_load_flags

=head1 AUTHORS

Eric Nawrocki, C<< <nawrockie at janelia.hhmi.org> >>
Jody Clements, C<< <clementsj at janelia.hhmi.org> >>
Rob Finn, C<< <finnr at janelia.hhmi.org> >>
William Arndt, C<< <arndtw at janelia.hhmi.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bio-easel at rt.cpan.org>.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bio::Easel::MSA


=head1 ACKNOWLEDGEMENTS

Sean R. Eddy is the author of the Easel C library of functions for
biological sequence analysis, upon which this module is based.

=head1 LICENSE AND COPYRIGHT

Copyright 2013 Eric Nawrocki.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1;
