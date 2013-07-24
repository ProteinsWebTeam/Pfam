#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::QC;
use Cwd;

my $pwd = getcwd;

#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn

my $config = Bio::Rfam::Config->new;

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my $familyIO = Bio::Rfam::FamilyIO->new;
my $familyObj = $familyIO->loadRfamFromLocalFile( '.', $pwd );
my $error = Bio::Rfam::QC::checkCM($familyObj);
if($error){
  warn "Format error with CMs!\n";
}
