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

Quick summary of what the module does.

Perhaps a little code snippet.

    use Bio::Easel::MSA;

    my $foo = Bio::Easel::MSA->new($fileLocation);
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

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

    HERE HERE HERE, fill me in

  Title    : new
  Incept   : EPN, Thu Jan 24 09:28:54 2013
  Usage    : Bio::Easel::MSA->new
  Function : Generates a new Bio::Easel::MSA object
  Args     : <fileLocation>: file location of alignment
           : <nseq>: number of seqs
  Returns  : Bio::EslMSA object
  
=cut

sub read_msa { 
    my ($self, $fileLocation) = @_;
    if($fileLocation) { 
	$self->{path} = $fileLocation;
    }
    if(! defined $self->{path}) { 
	die "ERROR: path not set in Bio::Easel::MSA object"; 
    }

    $self->{esl_msa} = c_read_msa($self->{path});
    return $self->{esl_msa};
}

sub nseq { 
    my ($self) = @_;
    if (! defined $self->{nseq}) { 
	$self->{nseq} = c_nseq($self->{esl_msa});
    }
    return $self->{nseq};
}

sub get_sqname_idx { 
    my ($self, $idx) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    return c_get_sqname_idx($self->{esl_msa}, $idx);
}

sub set_sqname_idx { 
    my ($self, $idx, $newname) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    c_set_sqname_idx($self->{esl_msa}, $idx, $newname);
    return;
}

sub write_msa { 
    my ($self, $outfile) = @_;
    c_write_msa($self->{esl_msa}, $outfile);
    return;
}
1;

__DATA__
__C__
#include "easel.h"
#include "esl_msa.h"
#include "esl_msafile.h"

SV *c_read_msa (char *infile) 
{
    int           status;     /* Easel status code */
    ESLX_MSAFILE *afp;        /* open input alignment file */
    ESL_MSA      *msa;        /* an alignment */

    /* open input file */
    if ((status = eslx_msafile_Open(NULL, infile, NULL, eslMSAFILE_UNKNOWN, NULL, &afp)) != eslOK)
      eslx_msafile_OpenFailure(afp, status);

    /* read_msa */
    status = eslx_msafile_Read(afp, &msa);
    if(status != eslOK) esl_fatal("Alignment file %s read failed with error code %d\n", infile, status);

    printf("read %d seqs\n", msa->nseq);
    
    return perl_obj(msa, "ESL_MSA");
}    

void c_write_msa (ESL_MSA *msa, char *outfile) 
{
    FILE         *ofp;        /* open output alignment file */

    if ((ofp  = fopen(outfile, "w"))  == NULL) esl_fatal("Failed to open output file %s\n", outfile);
    eslx_msafile_Write(ofp, msa, eslMSAFILE_STOCKHOLM);

    return;
}

I32 c_nseq (ESL_MSA *msa)
{
    return msa->nseq;
}   

char *c_get_sqname_idx (ESL_MSA *msa, I32 idx)
{
    /* should this check if idx is valid? perl func that calls it already does... is that proper? */
    return msa->sqname[idx];
}

char *c_set_sqname_idx (ESL_MSA *msa, I32 idx, char *newname)
{

    /* should this check if idx is valid? perl func that calls it already does... is that proper? */
    if(msa->sqname[idx]) free(msa->sqname[idx]);
    esl_strdup(newname, -1, &(msa->sqname[idx]));

    return;
}   
