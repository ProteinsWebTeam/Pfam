#!/usr/bin/perl -T
#
# Author:        jaina
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       July 17, 2020
# Last Modified: $Date$

#This needs to be configured in apache ideally, but it works and so I am going with this......
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamLib';
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata';
use lib '/nfs/production/xfam/pfam/perl5/lib/perl5';
use lib '/nfs/production/xfam/pfam/perl5/lib/perl5/x86_64-linux-thread-multi';

use strict;
use warnings;
$ENV{'PFAM_CONFIG'} = '/nfs/production/xfam/pfam/software/Conf/pfam_svn.conf';  
use CGI;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamQC;

my $q = CGI->new;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(%{ $config->pfamlive });

my $clan_data;
my $array_ref = $pfamDB->getAllClanData;

foreach my $clan (@$array_ref){
  my $membership_ref = $pfamDB->getClanMembership($clan->clan_acc);
  foreach my $pfamA (@$membership_ref){
    $clan_data.=$pfamA->pfama_acc->pfama_id."\t".$clan->clan_acc."\n";
  }
}

#Return lines clan data
print $q->header(), $clan_data;

exit;
