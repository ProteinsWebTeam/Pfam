#!/usr/bin/perl -T
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       May 13, 2010
# Last Modified: $Date$

#This needs to be configured in apache ideally, but it works and so I am going with this......
use lib '/nfs/production/xfam/pfam/software/Modules/PfamLib';
use lib '/nfs/production/xfam/pfam/software/Modules/PfamSchemata';
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

my $q = CGI->new;

my $fh = $q->upload('file');

bail( 'failed to retrieve uploaded file' ) unless defined $fh;

my $filename = $q->param('file');
my $tmp_file = $q->tmpFileName($filename);

my ($clan) = $q->param('clan') =~ /^(CL\d{4})$/;
my ($ignore) = $q->param('ignore') =~ /^([PF\d+]*)$/; 
my ($nest) = $q->param('nest') =~ /^([PF\d+]*)$/;

open ( UPLOAD, $tmp_file )
  or bail( 'failed to open temporary file' );
#my $upload = join '', <UPLOAD>;
my (%regions, %overlaps);

while(<UPLOAD>){
  print STDERR "$_\n";
  if(/^(SEED)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S+)\s+([A-Za-z0-9\_\-]{0,16})/){
    my ($align, $seqId, $start, $end, $acc, $id) = ($1, $2, $3, $4, $5, $6);
    
    push(@{$regions{$seqId}}, {  
                               from      => $start,
                               to        => $end,
                               family    => $acc,
                               family_id => $id,
                               ali       => $align
                              });
    
  }
  elsif(/^(FULL)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+([A-Za-z0-9\_\-]{0,16})\s+(\S+)/){
    my ($align, $seqId, $ali_start, $ali_end, $start, $end, $acc, $id, $score) = ($1, $2, $3, $4, $5, $6, $7, $8, $9);
    push(@{$regions{$seqId}}, {                  
                               from      => $start,
                               to        => $end,
                               ali_from  => $ali_start,
                               ali_to    => $ali_end,
                               family    => $acc,
                               family_id => $id,
                               ali       => $align,
                               score     => $score
                              });
    
  }
  else{
    bail('Uploaded file contains unrecognised format');
  }
}
close UPLOAD;


my $config = Bio::Pfam::Config->new;
my $connect = $config->pfamlive;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{ $connect }
  );

my $ignore_ref;
#User defined ignores
while($ignore){
  my $acc = substr( $ignore, 0, 7, "" );
  $$ignore_ref{ $acc }++;
}

#Ignore nested families and if they are part of clans, their related families.
while($nest){
  my $acc = substr($nest,0,7,"");
  $$ignore_ref{ $acc }++;
  my $clanO = $pfamDB->getClanDataByPfam( $acc );
  if ( $clanO and $clanO->auto_clan->clan_acc ) {
    my $clanMem = $pfamDB->getClanMembership( $clanO->auto_clan->clan_acc );
    foreach my $fam (@$clanMem) {
      $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
    }
  }  
} 

#If this is part of a clan, ignore other clan families and if those families
#have nested domains, then also ignore those nested families.
if($clan){
  my $clanMem = $pfamDB->getClanMembership( $clan );
    foreach my $fam (@$clanMem) {
      $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
      my $nestedRef =
        $pfamDB->getNestedDomain( $fam->auto_pfama->pfama_acc );

      if ($nestedRef) {
        foreach my $n (@$nestedRef) {
          $$ignore_ref{$n}++;

          my $clanO = $pfamDB->getClanDataByPfam($n);
          if ( $clanO and $clanO->auto_clan->clan_acc ) {
            my $clanMem = $pfamDB->getClanMembership( $clanO->auto_clan->clan_acc );
            foreach my $fam (@$clanMem) {
              $$ignore_ref{ $fam->auto_pfama->pfama_acc }++;
            }
          }
        }
      }
    }
}

#Query the database for all overlaps.
$pfamDB->getOverlapingFullPfamRegions( \%regions, \%overlaps );
$pfamDB->getOverlapingSeedPfamRegions( \%regions, \%overlaps );

my(%seen, $numberOverlaps, $allLines);

#Record which families have been ignored during this overlap check.
foreach my $iFam (keys %{$ignore_ref}){
  $allLines .= "Ignoring $iFam\n";
} 

#Now iterate through all overlaps and ignore any overlap that is permitted by the
#ignore hash.
$numberOverlaps = 0;
  foreach my $seqAcc ( keys %overlaps ) {
    foreach
      my $region ( sort { $a->{from} <=> $b->{from} } @{ $overlaps{$seqAcc} } )  {
      foreach my $overRegion ( @{ $region->{overlap} } ) {
        next if ( $$ignore_ref{ $overRegion->{family} } );

	my $line;
 	if($region->{ali} eq 'SEED') {
	  $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{from}."-".$region->{to}." ".$region->{ali}." with ";
	}
	else {
	  $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{ali_from}."-".$region->{ali_to}." (".
	    $region->{family}."/".$region->{from}."-".$region->{to}.", ".$region->{score}." bits) ".$region->{ali}." with ";
	}

	if($overRegion->{ali} eq 'SEED') {
	  $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}." ".$overRegion->{ali}."\n";
	}
	else {
	  $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{ali_from}."-".$overRegion->{ali_to}." (".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}
	    .", ".$overRegion->{score}." bits) ".$overRegion->{ali}."\n";
	}

        next if ( $seen{$line} );
        $seen{$line}++;
        $numberOverlaps++;
        $allLines .= $line;
      }
    }
  }

#Now return the overlaps
print $q->header(), $allLines;

exit;

sub bail {
  my $msg = shift;
  print $q->header( -status => '400 Bad Request' ), $msg;
  exit 1;
}
