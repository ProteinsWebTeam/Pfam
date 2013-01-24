package Bio::Easel::Align;

use Inline C => DATA =>
    INC  => '-I/groups/eddy/home/nawrockie/notebook/13_0116_rfam_esl_align_module/infernal-1.1rc2/easel -I/groups/eddy/home/nawrockie/notebook/13_0116_rfam_esl_align_module ' =>
    LIBS => '-L/groups/eddy/home/nawrockie/notebook/13_0116_rfam_esl_align_module/infernal-1.1rc2/easel -leasel ';
    TYPEMAPS => "/groups/eddy/home/nawrockie/notebook/13_0116_rfam_esl_align_module/typemap";

use strict;
use warnings;

use Getopt::Long;
use IO::File;
use Sys::Hostname;
use Cwd;
use File::Copy;
use File::stat;

#-------------------------------------------------------------------------------

=head2 new 

  Title    : new
  Incept   : EPN, Thu Jan 24 09:28:54 2013
  Usage    : Bio::EslAlign->new
  Function : Generates a new Bio::EslAlign object
  Args     : <fileLocation>: file location of alignment
           : <nseq>: number of seqs
  Returns  : Bio::EslAlign object
  
=cut

sub new {
    my( $caller, $fileLocation ) = @_;
    my $class = ref($caller) || $caller;
    my $self = {};

    bless( $self, $caller );

    # First check that the file exists. If it exists, read it with 
    # Easel and populate the object from the ESL_MSA object
    if(-e $fileLocation){
	eval{
	    $self->{path} = $fileLocation;
	    $self->read_msa();
	    # should I populate the object here? Or wait until specific values are 
	    # requested by caller, e.g. 'nseq' is requested which is actually a 
	    # function that sets nseq if it's not already set? 
	    # $self->{nseq} = new_sizeof_msa($self); # or should this be just a call to sizeof_msa() which defines $self->{nseq} if it's not already defined?

	}; # end of eval
	
	if($@){
	    confess("Error creating ESL_MSA from $fileLocation, $@\n"); 
	}
    } 
    else {
	confess("Expected to recieve a valid file location path ($fileLocation doesn\'t exist)");  
    }
    return $self;
}

sub read_msa { 
    my ($self, $fileLocation) = @_;
    if($fileLocation) { 
	$self->{path} = $fileLocation;
    }
    if(! defined $self->{path}) { 
	die "ERROR: path not set in Bio::EslAlign object"; 
    }

    my $self->{esl_msa} = c_read_msa($self->{path});
    return $self->{esl_msa};
}

# TODO: define naming convention for subroutines that set/get variable like nseq, and those that just get one like sqname_idx
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
    return c_set_sqname_idx($self->{esl_msa}, $idx, $newname);
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
#include "tw_inline_functions.h"

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
