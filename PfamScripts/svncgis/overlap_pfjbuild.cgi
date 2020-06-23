#!/usr/bin/perl -T
#
# Author:        jaina
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       June 19, 2020
# Last Modified: $Date$

#This needs to be configured in apache ideally, but it works and so I am going with this......
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamLib';
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata';
use lib '/nfs/production/xfam/pfam/Bio-HMM-Logo-master/blib/arch';
use lib '/nfs/production/xfam/pfam/Bio-HMM-Logo-master/blib/lib';
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

my $fh = $q->upload('file');

bail( 'failed to retrieve uploaded file' ) unless defined $fh;

my $filename = $q->param('file');
my $tmp_file = $q->tmpFileName($filename);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(%{ $config->pfamlive });


my %regions;

open ( UPLOAD, $tmp_file ) or bail( 'failed to open temporary file' );
while(<UPLOAD>) {
  next if(/^#/ or /\/\//);
  if(/(\S+)\/(\d+)-(\d+)/) { #Alignments contain alignment co-ordinates
    my ($acc, $st, $en) = ($1, $2, $3);
    if($acc =~ /(\S+)\.\d+/) {
      $acc=$1;
    }

    push(@{ $regions{ $acc } },
      {
        ali_from  => $st,
        ali_to    => $en,
        family    => 'NEW',
        ali       => 'JALIGN',
        family_id => 'NEW' });
  }
}
close(UPLOAD);

my $ignore_ref; 
my ($numOverlaps, $overlapArray) = &Bio::Pfam::PfamQC::findOverlapsDb(\%regions, $ignore_ref, $pfamDB, "", "", "", "");


my $all_lines .= join("", @{$overlapArray});

#Return lines all the overlap lines
print $q->header(), $all_lines;


exit;

sub bail {
  my $msg = shift;
  print $q->header( -status => '400 Bad Request' ), $msg;
  exit 1;
}
