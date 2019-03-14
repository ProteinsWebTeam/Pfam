#!/usr/bin/perl -T
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       May 13, 2010
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

my ($clan) = $q->param('clan') =~ /^(CL\d{4})$/;
my ($ignore) = $q->param('ignore') =~ /^([PF\d+]*)$/; 
my ($nest) = $q->param('nest') =~ /^([PF\d+]*)$/;
my ($filter) = $q->param('filter');
my ($no_sigP) = $q->param('no_SigP');
my ($compete) = $q->param('compete');

open ( UPLOAD, $tmp_file )
  or bail( 'failed to open temporary file' );
#my $upload = join '', <UPLOAD>;
my (%regions, %overlaps, $numFull);

my $family_acc;
while(<UPLOAD>){
  print STDERR $_;
  if(/^(SEED)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\S+)\s+([A-Za-z0-9\_\-]{0,16})/){
    my ($align, $seqId, $start, $end, $acc, $id) = ($1, $2, $3, $4, $5, $6);
    
    push(@{$regions{$seqId}}, {  
                               from      => $start,
                               to        => $end,
                               family    => $acc,
                               family_id => $id,
                               ali       => $align
                              });
    $family_acc=$acc unless($family_acc);
  }
  elsif(/^(FULL)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+([A-Za-z0-9\_\-]{0,16})\s+(\S+)\s+(\S+)/){
    my ($align, $seqId, $ali_start, $ali_end, $start, $end, $acc, $id, $score, $evalue) = ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);
    push(@{$regions{$seqId}}, {                  
                               from      => $start,
                               to        => $end,
                               ali_from  => $ali_start,
                               ali_to    => $ali_end,
                               family    => $acc,
                               family_id => $id,
                               ali       => $align,
                               score     => $score,
			       evalue    => $evalue
                              });
    $numFull++;
    
  }
  else{
    bail('Uploaded file contains unrecognised format');
  }
}
close UPLOAD;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(%{ $config->pfamlive });

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
  if ( $clanO and $clanO->clan_acc->clan_acc ) {
    my $clanMem = $pfamDB->getClanMembership( $clanO->clan_acc->clan_acc );
    foreach my $fam (@$clanMem) {
      $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
    }
  }  
} 

#If this is part of a clan, ignore other clan families and if those families
#have nested domains, then also ignore those nested families.
if($clan){
  my $clanMem = $pfamDB->getClanMembership( $clan );
    foreach my $fam (@$clanMem) {
      $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
      my $nestedRef =
        $pfamDB->getNestedDomain( $fam->pfama_acc->pfama_acc );

      if ($nestedRef) {
        foreach my $n (@$nestedRef) {
          $$ignore_ref{$n}++;

          my $clanO = $pfamDB->getClanDataByPfam($n);
          if ( $clanO and $clanO->clan_acc->clan_acc ) {
            my $clanMem = $pfamDB->getClanMembership( $clanO->clan_acc->clan_acc );
            foreach my $fam (@$clanMem) {
              $$ignore_ref{ $fam->pfama_acc->pfama_acc }++;
            }
          }
        }
      }
    }
}

#Record which families have been ignored during this overlap check.
my $allLines;
foreach my $iFam (keys %{$ignore_ref}){
  $allLines .= "Ignoring $iFam\n";
}

#Find overlaps
if($filter) {
  $allLines .= "Filtering overlaps\n";
}
else {
  $allLines .= "Overlaps will not be filtered\n";
}
my ($numOverlaps, $summary, $overlapArray);
($numOverlaps, $overlapArray, $summary) = &Bio::Pfam::PfamQC::findOverlapsDb(\%regions, $ignore_ref, $pfamDB, $clan, $compete, $filter, $numFull);

$allLines .= join("", @{$overlapArray});
$allLines .= $summary if($summary);
$allLines .= "End of overlaps\n"; #This signifies the end of the overlaps in the output, and is used in a reg ex in the pqc-overlap-cgi.pl script

#Look for sigP
unless($no_sigP) {
    my (%sigP_seed, %sigP_full);
    foreach my $acc (keys %regions) {
      foreach my $r (@{$regions{$acc}}) { 
        next if($r->{from} > 120);

        if($r->{ali} eq 'SEED') {
          if(exists($sigP_seed{$acc})) {
            next unless($r->{from} < $sigP_seed{$acc}{start});
          }
          $sigP_seed{$acc}{start}=$r->{from};
          $sigP_seed{$acc}{end}=$r->{to};
        }
        else {
          if(exists($sigP_full{$acc})) {
            next unless($r->{from} < $sigP_full{$acc}{start});
          }   
          $sigP_full{$acc}{start}=$r->{from};
          $sigP_full{$acc}{end}=$r->{to};
        }
      }   
    }
    my $sigP_seed_overlaps = $pfamDB->getSignalPeptideRegion(\%sigP_seed);
    my $sigP_full_overlaps = $pfamDB->getSignalPeptideRegion(\%sigP_full);

    my ($sigP_total_count, $sigP_seed_count, $sigP_full_count) = (0,0,0);
    foreach my $acc (keys %{$sigP_seed_overlaps}) {
      $allLines .= "Sequence $acc/" . $sigP_seed{$acc}{start} . "-". $sigP_seed{$acc}{end} . " in SEED overlaps with signal peptide ".$sigP_seed_overlaps->{$acc}."\n";
      $sigP_total_count++;
      $sigP_seed_count++;
    }
    foreach my $acc (keys %{$sigP_full_overlaps}) {
      $allLines .= "Sequence $acc/" . $sigP_full{$acc}{start} . "-". $sigP_full{$acc}{end} . " in ALIGN overlaps with signal peptide ".$sigP_full_overlaps->{$acc}."\n";
      $sigP_total_count++;
      $sigP_full_count++;
    }

    if($sigP_total_count) {
      $allLines .= "$family_acc: there are $sigP_total_count signal peptide overlaps, $sigP_seed_count in SEED and $sigP_full_count in ALIGN\n";
    }
    else {
      $allLines .= "$family_acc: found 0 signal peptide overlaps\n";
    }
}

#Return lines all the overlap lines
print $q->header(), $allLines;


exit;

sub bail {
  my $msg = shift;
  print $q->header( -status => '400 Bad Request' ), $msg;
  exit 1;
}
