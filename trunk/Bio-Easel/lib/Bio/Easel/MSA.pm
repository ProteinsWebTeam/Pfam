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

=head2 new 

  Title    : new
  Incept   : EPN, Thu Jan 24 09:28:54 2013
  Usage    : Bio::Easel::MSA->new
  Function : Generates a new Bio::Easel::MSA object
  Args     : <fileLocation>: file location of alignment
           : <reqdFormat>:   optional: string defining requested/required format
           :                 valid format strings are: 
           :                 "unknown", "Stockholm", "Pfam", "UCSC A2M", "PSI-BLAST", 
           :                 "SELEX", "aligned FASTA", "Clustal", "Clustal-like", 
           :                 "PHLYIP (interleaved)", or "PHYLIP (sequential)"
           :                 
  Returns  : Bio::Easel::MSA object

=cut

sub new {
  my ( $caller, $args ) = @_;
  my $class = ref($caller) || $caller;
  my $self = {};

  bless( $self, $caller );

  # First check that the file exists. If it exists, read it with
  # Easel and populate the object from the ESL_MSA object
  if ( -e $args->{fileLocation} ) {
    eval {
      $self->{path}   = $args->{fileLocation};
      if(defined $args->{reqdFormat}) { 
        $self->{reqdFormat} = $args->{reqdFormat};
        $self->_check_reqd_format();
      }
      $self->read_msa();
    };    # end of eval

    if ($@) {
      confess("Error creating ESL_MSA from @{[$args->{fileLocation}]}, $@\n");
    }
  }
  else {
    confess("Expected to receive a valid file location path (@{[$args->{fileLocation}]} doesn\'t exist)");
  }
  if ( defined $args->{aliType} ) {
    $self->{aliType} = $args->{aliType};
  }
  return $self;
}

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

=head2 read_msa

  Title    : read_msa
  Incept   : EPN, Mon Jan 28 09:26:24 2013
  Usage    : Bio::Easel::MSA->read_msa($fileLocation)
  Function : Opens $fileLocation, reads first MSA, sets it.
  Args     : <fileLocation>: optional, file location of alignment
           : <reqdFormat>:   optional, required format of alignment file
  Returns  : void

=cut

sub read_msa {
  my ( $self, $fileLocation, $reqdFormat ) = @_;

  if ($fileLocation) {
    $self->{path} = $fileLocation;
  }
  if ( !defined $self->{path} ) {
    croak "trying to read msa but path is not set";
  }

  if ($reqdFormat) { 
    $self->{reqdFormat} = $reqdFormat;
  }

  my $informat = "unknown";
  if (defined $self->{reqdFormat}) {
    $informat = $self->{reqdFormat};
    $self->_check_reqd_format();
  }

  ($self->{esl_msa}, $self->{informat}) = _c_read_msa( $self->{path}, $informat);

  # Possible values for 'format', a string, derived from esl_msafile.c::eslx_msafile_DecodeFormat(): 
  # "unknown", "Stockholm", "Pfam", "UCSC A2M", "PSI-BLAST", "SELEX", "aligned FASTA", "Clustal", 
  # "Clustal-like", "PHYLIP (interleaved)", or "PHYLIP (sequential)".

  return;
}

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

=head2 write_msa

  Title    : write_msa
  Incept   : EPN, Mon Jan 28 09:58:19 2013
  Usage    : $msaObject->write_msa($fileLocation)
  Function : Write MSA to a file
  Args     : name of output file 
           : format ('stockholm', 'pfam', 'afa' or 'clustal')
  Returns  : void

=cut

sub write_msa {
  my ( $self, $outfile, $format ) = @_;

  $self->_check_msa();
  if ( !defined $format ) {
    $format = "stockholm";
  }
  if ( $format ne "stockholm"
    && $format ne "pfam"
    && $format ne "afa" 
    && $format ne "clustal" )
  {
    croak "format must be \"stockholm\" or \"pfam\" or \"afa\" or \"clustal\"";
  }
  my $status = _c_write_msa( $self->{esl_msa}, $outfile, $format );
  if ( $status != $ESLOK ) {
    if ( $status == $ESLEINVAL ) {
      croak "problem writing out msa, invalid format $format";
    }
    elsif ( $status == $ESLFAIL ) {
      croak "problem writing out msa, unable to open output file $outfile for writing";
    }
  }
  return;
}

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
  return _c_any_allgap_columns( $self->{esl_msa} );
}

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

=head2 get_sqstring_unaligned

  Title    : get_sqstring_unaligned
  Incept   : EPN, Fri May 24 11:02:21 2013
  Usage    : $msaObject->get_sqstring_unaligned()
  Function : Return an unaligned sequence from an MSA.
  Args     : index of sequence you want
  Returns  : unaligned sequence index idx

=cut

sub get_sqstring_unaligned {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  $self->_check_sqidx($idx);
  return _c_get_sqstring_unaligned( $self->{esl_msa}, $idx );
}

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

=head2 _check_sqidx

  Title    : _check_seqidx
  Incept   : EPN, Sat Feb  2 13:46:08 2013
  Usage    : $msaObject->_check_sqidx($idx)
  Function : Check if $idx is in range 0..nseq-1,
             if not, croak.
  Args     : none
  Returns  : void

=cut

sub _check_sqidx {
  my ( $self, $idx ) = @_;

  $self->_check_msa();
  my $nseq = $self->nseq;
  if ( $idx < 0 || $idx >= $nseq ) {
    croak "invalid sequence index %d (must be [0..%d])", $idx, $nseq;
  }
  return;
}

=head2 _check_reqd_format

  Title    : _check_reqd_format
  Incept   : EPN, Thu Jul 18 11:06:02 2013
  Usage    : $msaObject->_check_reqd_format()
  Function : Check if $self->{reqd_format} is a valid format or 'unknown'
           : if not, croak. Also returns fine if self->{reqd_format} is not
           : defined.
  Args     : none
  Returns  : void

=cut

sub _check_reqd_format { 
  my ( $self ) = @_;

  if(defined $self->{reqd_format}) { 
    if($self->{reqd_format} ne "unknown") { # unknown is valid, so we don't actually do the C check 
      _c_check_reqd_format($self->{reqd_format}); 
    }
  }
  return;
}

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