#!/usr/local/bin/perl
#
# Tests to make sure that the HMM modules are doing as they are supposed to
#

use strict;
use warnings;
#
use Test::More tests => 19;
use Test::Warn;
use Test::Moose;
use Cwd;

#We need to read the config as we will need to run hmmer
use_ok( 'Bio::Pfam::Config');
my $conf = Bio::Pfam::Config->new;



my $module = 'Bio::Pfam::SeqFetch';
use_ok( $module);


#Test parseGDF

can_ok($module, 'parseGDF');
my %seqsGDF;
open( GDF, "data/seqs.GDF");
my $noSeqsGDF = Bio::Pfam::SeqFetch::parseGDF(\*GDF, \%seqsGDF);
is( $noSeqsGDF, 5, "Got the right number of sequences" );
close( GDF );

#Test parseScores
can_ok($module, 'parseScores');
my %seqsScores;
open( S, "data/seqs.scores");
my $noSeqsScores = Bio::Pfam::SeqFetch::parseScores(\*S, \%seqsScores);
is( $noSeqsScores, 5, "Got the right number of sequences" );
close( S );
is_deeply(\%seqsScores, \%seqsGDF, "Got same data strcutre from parseGDF and parseScores" );

#Test parseList
can_ok($module, 'parseList');
my %seqsList;
open( L, "data/seqs.list");
my $noSeqsList = Bio::Pfam::SeqFetch::parseList(\*L, \%seqsList);
is( $noSeqsList, 5, "Got the right number of sequences" );
close( L );
is_deeply(\%seqsList, \%seqsGDF, "Got same data strcutre from parseGDF and parseList" );

#Add another sequence
#A8FX08.1
can_ok($module, 'addSeq');
Bio::Pfam::SeqFetch::addSeq('A8FX08.1', undef, undef, \%seqsList);
my $noSeqsAdd = scalar( keys %seqsList );
is( $noSeqsAdd, 6, "Got the right number of sequences" );

#Now Fetch the sequences
can_ok($module, 'fetchSeqs');
open(SEQ, ">/tmp/fetchedSeqs");
my $fetchedNoSeqs = Bio::Pfam::SeqFetch::fetchSeqs(\%seqsList, $conf->pfamseqLoc."/pfamseq", \*SEQ);
is( $fetchedNoSeqs, 6, "Fetched the right number of sequences" );
close( SEQ );
ok(-s "/tmp/fetchedSeqs", "fetched sequences okay!");

#We now want to verify the sequences!
open(SEQ, "/tmp/fetchedSeqs");
can_ok($module, 'addSeqToVerify');
my %verifySeq;
my($acc, $s, $e, $seq);
while(<SEQ>){
   if(/\>(\S+)\/(\d+)\-(\d+)/){
      if($acc and $seq){
        Bio::Pfam::SeqFetch::addSeqToVerify($acc, $s, $e, $seq, \%verifySeq); 
        $seq = undef;
      } 
      $acc = $1;
      $s   = $2;
      $e   = $3;
   }elsif(/(\S+)/){
      $seq .= $1; 
   }else{
      print STDERR "Can not parse $_"; 
   }
}
Bio::Pfam::SeqFetch::addSeqToVerify($acc, $s, $e, $seq, \%verifySeq);
my $noSeqsToVerify = scalar( keys %verifySeq );
is($noSeqsToVerify, 6, "No seqs to verify\n"); 

can_ok($module, 'verifySeqs');
my $noSeqsVerified = Bio::Pfam::SeqFetch::verifySeqs( \%verifySeq, $conf->pfamseqLoc()."/pfamseq");
is($noSeqsVerified, 6, 'Verified sequences okay');