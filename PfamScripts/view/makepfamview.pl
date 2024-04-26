#!/usr/bin/env perl

=head1 NAME

makepfamview - makes view files on the farm.  Although the same in name, this is very different to
the old makepfamview.pl script.

=head1 SYNOPSIS

makepfamview.pl -family <pfam-id> -id <Data::UUID>

=head1 DESCRIPTION

The view files which are made are as follows:
   
  SEED.ann   - SEED with stockholm markup above it
  ALIGN.ann  - ALIGN with stockholm markup above it
  HMM.ann - ls mode HMM with Stockholm markup

=cut

#Core Perl Modules
use strict;
use warnings;
use DDP;

use Bio::Pfam::ViewProcess;

#-------------------------------------------------------------------------------
#Set up the view process object and coonfigure...
my $view = Bio::Pfam::ViewProcess->new;
#Get the user options
$view->processOptions;
help() if($view->options->{help});
$view->logger->debug("Got commandline options");

#Get the view job
$view->getJob;
$view->job->run;

#Make sure there are no files.
$view->cleanUp();

#-------------------------------------------------------------------------------
#Now we have start the work
$view->getPfamObj;
$view->logger->debug("Family " . $view->pfam->pfama_acc);
$view->logger->debug("Checking if family has hits");
#Check whether family has hits (need to know this later when checking if ALIGN file has size)
my $pfamA=$view->pfamdb->getSchema->resultset('PfamA')->find( { pfama_acc => $view->pfam->pfama_acc  } );
if($pfamA->num_full == 0) {
  $view->logger->debug("Family has no hits in pfamseq");
  $view->{noALIGN}=1;
}

$view->logger->debug("Getting all files");
$view->getAllFiles;
$view->resetStats;
#Run a md5 checksum on the "raw" files and compared to the release versions......!
#TODO - check this over, probably put on row?
$view->versionFiles;

#Get the generic per file annotations - #=GF stockholm lines
my $GFAnn = $view->getGFAnnotations;

#Active site prediction object
$view->initiateActiveSiteObj();
$view->processALIGN( $GFAnn );
$view->processSEED( $GFAnn );
$view->processHMMs( );
#-------------------------------------------------------------------------------
#Now generate the species tree data.
$view->logger->debug("Building JSON string for species tree");
$view->makeSpeciesJsonString();
$view->pfamProteomes();
$view->pfamTaxDepth();

#-------------------------------------------------------------------------------
#Run the HMM against the shuffled database.
$view->searchShuffled();

#-------------------------------------------------------------------------------
#Change the job status to done
$view->job->done;

#Now Initiate an Ancillary job.
# $view->initiateAncillaryViewProcess;

$view->cleanUp();
$view->logger->debug("Finished");
exit;


sub help {
  
  print <<EOF

usage: $0 -id <UUID> -family <pfamA_id>

Complete list of options:

family : name of the pfam family to be processed
id     : the unique identifier assigned to the job
fasta  : threshold for making the Pfam-A.fasta file. Default is set to 90% identity.
       : Note, this will automatically modifiy if too stringent.
noclean: Do not remove files at the end of the job
h      : prints this help statement

This script will take the input job and convert the unformated Pfam files and convert them to
formated versions ready for the release. This script tries to take all information from the database,
thereby detaching Pfam from the RCS/SVN system. If everything works, this should produce the following files:

SEED.ann    - SEED with stockholm markup above it 
ALIGN.ann   - ALIGN (FULL) with stockholm markup above it
HMM.ann  - ls mode HMM with Stockholm markup
SEED.html   - SEED alignment coloured according to clustalx colouring scheme
ALIGN.html  - FULL alignment coloured according to clustalx colouring scheme
SEED.tree   - NJ tree based on the SEED alignment
ALIGN.tree  - NJ tree based on the FULL alignment
hmmLogo.png - The HMM logo
family.fa   - Non-redundant version the ALIGN file

If errors are detected the user will normally be mailed with a short message.

EOF
}
