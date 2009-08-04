#!/usr/local/bin/perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess;

-------------------------------------------------------------------------------
#Initial things to start up - database connections and logger

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

#Lets get a new Pfam config object
my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}
$logger->debug("Got pfamlive database connection");





#-------------------------------------------------------------------------------
# Now update the VERSION table with the number of PfamA

my $noPfama = $pfamDB->getSchema->resultset('Pfama')->search({})->count;
my $version = $pfamDB->getSchema->resultset('Version')->find({});
$version->update({ number_families => $noPfama });

#-------------------------------------------------------------------------------
#Find when this job was last run



#-------------------------------------------------------------------------------
#Select all the families that have a changed date more recent that when the last jobs was started



#Now get a list of all sequences belongs to these families 

#Generate the storables for each sequence

#Look to see if the architecture is known for this sequence.  If not add it to the architecture table.

#Foreach family update the number of architectures

#Foreach clan update the number of architectures

# Now add in Kristofers bit to add the domain order

#-------------------------------------------------------------------------------
#Now on to the profile:profile comparisons
#
# Run hhsearch.

# 