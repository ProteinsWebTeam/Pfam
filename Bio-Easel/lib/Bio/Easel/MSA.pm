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

my $src_file      = undef;
my $typemaps      = undef;
my $easel_src_dir = undef;

BEGIN {
    $src_file = __FILE__;
    $src_file =~ s/\.pm/\.c/;

    my $file = __FILE__;
    ($easel_src_dir) = $file =~ /^(.*)\/blib/;
    $easel_src_dir = File::Spec->catfile($easel_src_dir, 'src/easel');

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
  Usage    : Bio::EslMSA->new
  Function : Generates a new Bio::EslMSA object
  Args     : <fileLocation>: file location of alignment
           : <nseq>: number of seqs
  Returns  : Bio::EslMSA object
  
=cut

sub new {
    my( $caller, $args) = @_;
    my $class = ref($caller) || $caller;
    my $self = {};

    bless( $self, $caller );

    # First check that the file exists. If it exists, read it with 
    # Easel and populate the object from the ESL_MSA object
    if(-e $args->{fileLocation}){
	eval{
	    $self->{path} = $args->{fileLocation};
	    $self->read_msa();
	    # should I populate the object here? Or wait until specific values are 
	    # requested by caller, e.g. 'nseq' is requested which is actually a 
	    # function that sets nseq if it's not already set? 
	    # $self->{nseq} = new_sizeof_msa($self); # or should this be just a call to sizeof_msa() which defines $self->{nseq} if it's not already defined?

	}; # end of eval
	
	if($@) {
	    confess("Error creating ESL_MSA from @{[$args->{fileLocation}]}, $@\n");
	}
    } 
    else {
	confess("Expected to recieve a valid file location path (@{[$args->{fileLocation}]} doesn\'t exist)");
    }
    return $self;
}

=head2 read_msa

  Title    : read_msa
  Incept   : EPN, Mon Jan 28 09:26:24 2013
  Usage    : Bio::Easel::MSA->read_msa($fileLocation)
  Function : Opens $fileLocation and updates 
  Args     : <fileLocation>: file location of alignment
  Returns  : ESL_MSA *C* object
  
=cut

sub read_msa { 
    my ($self, $fileLocation) = @_;

    if($fileLocation) { 
	$self->{path} = $fileLocation;
    }
    if(! defined $self->{path}) { 
	die "ERROR: path not set in Bio::Easel::MSA object"; 
    }

    $self->{esl_msa} = c_read_msa($self->{path}, $self->{esl_abc});
    return;
}

=head2 nseq

  Title    : nseq
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : Bio::Easel::MSA->nseq()
  Function : Sets (if nec) and returns number of sequences in MSA.
  Args     : none
  Returns  : number of sequences (esl_msa->nseq)
  
=cut

sub nseq { 
    my ($self) = @_;
    
    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if (! defined $self->{nseq}) { 
	$self->{nseq} = c_nseq($self->{esl_msa});
    }
    return $self->{nseq};
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
    my($self) = @_;

    if(! defined ($self->{esl_msa})) { 
	$self->read_msa();
    }

    return $self->{esl_msa};
}

=head2 alen

  Title    : alen
  Incept   : EPN, Tue Jan 29 07:41:08 2013
  Usage    : Bio::Easel::MSA->alen()
  Function : Sets (if nec) and returns alignment length.
  Args     : none
  Returns  : alignment length, number of columns (esl_msa->alen)
  
=cut

sub alen { 
    my ($self) = @_;
    if (! defined $self->{alen}) { 
	$self->{alen} = c_alen($self->{esl_msa});
    }
    return $self->{alen};
}

=head2 get_sqname_idx

  Title    : get_sqname_idx
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : $msaObject->get_sqname_idx($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence $idx (esl_msa->sqname[$idx])
  
=cut

sub get_sqname_idx { 
    my ($self, $idx) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    return c_get_sqname_idx($self->{esl_msa}, $idx);
}

=head2 set_sqname_idx

  Title    : set_sqname_idx
  Incept   : EPN, Mon Jan 28 09:48:42 2013
  Usage    : $msaObject->set_sqname_idx($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence index $idx 
  
=cut

sub set_sqname_idx { 
    my ($self, $idx, $newname) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    c_set_sqname_idx($self->{esl_msa}, $idx, $newname);
    return;
}

=head2 write_msa

  Title    : write_msa
  Incept   : EPN, Mon Jan 28 09:58:19 2013
  Usage    : $msaObject->write_msa($fileLocation)
  Function : Write MSA to a file
  Args     : name of output file 
  Returns  : void
  
=cut

sub write_msa { 
    my ($self, $outfile) = @_;
    c_write_msa($self->{esl_msa}, $outfile);
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

    # EPN should I do error checking in c_any_allgap_columns()? 
    return c_any_allgap_columns($self->{esl_msa});
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
    c_free_msa($self->{esl_msa});
    return;
}

=head2 destroy

  Title    : destroy
  Incept   : EPN, Mon Jan 28 10:09:55 2013
  Usage    : $msaObject->destroy()
  Function : Frees an MSA object
  Args     : name of output file 
  Returns  : void
  
=cut

sub destroy { 
    my ($self) = @_;
    c_destroy($self->{esl_msa}, $self->{esl_abc});
    return;
}
1;
