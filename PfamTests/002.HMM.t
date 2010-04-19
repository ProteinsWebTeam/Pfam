#!/usr/local/bin/perl
#
# Tests to make sure that the HMM modules are doing as they are supposed to
#

use strict;
use warnings;
#
use Test::More tests => 91;
use Test::Warn;
use Test::Exception;
use Test::Moose;
use Cwd;

#We need to read the config as we will need to run hmmer
use_ok( 'Bio::Pfam::Config');
my $conf = Bio::Pfam::Config->new;


my $moduleZ = 'Bio::Pfam::HMM::HMM';
use_ok( $moduleZ);

my $moduleY = 'Bio::Pfam::HMM::HMMIO';
use_ok( $moduleY );











my $module1 = 'Bio::Pfam::HMM::HMMUnit';
use_ok( $module1);

can_ok( $module1, 'new' );
my $hmmUnit = $module1->new( { seqFrom => 1,
                               seqTo   => 100,
                               hmmFrom => 1,
                               hmmTo   => 90,
                               bits    => 25.0,
                               evalue  => 0.01,
                               name    => 'P01234' });
isa_ok( $hmmUnit, $module1 );

foreach my $attr_name (qw(bits evalue name seqEvalue domain seqFrom seqTo domEvalue)){
  has_attribute_ok($hmmUnit, $attr_name, "$module1 has attribute $attr_name");
}

my $module2 = 'Bio::Pfam::HMM::HMMSequence';
use_ok( $module2) or exit;

can_ok( $module2, 'new' );
my $hmmSeq = $module2->new({   numberHits => 1,
                               bits       => 25.0,
                               evalue     => 0.01,
                               name       => 'P01234',
                               desc       => 'Protein of unknown function' });
isa_ok( $hmmSeq, $module2 );

foreach my $attr_name (qw(bits evalue name desc numberHits bias sumScore hmmUnits)){
  has_attribute_ok($hmmSeq, $attr_name, "$module2 has attribute $attr_name");
}

ok($hmmSeq->addHMMUnit($hmmUnit), 'Added HMMunit');
my $hmmSeqRef = $hmmSeq->hmmUnits;
ok(ref($hmmSeqRef) eq 'ARRAY', 'Array of hmmUnits');
ok(scalar(@{ $hmmSeqRef }) == 1, 'Got one HMMUnit');
isa_ok($hmmSeqRef->[0], $module1);
warning_like(sub{ $hmmSeq->addHMMUnit('Wibble') } , qr/is not a Bio::Pfam::HMM::HMMUnit/, 'Added not HMMUnit' );


my $module3 = 'Bio::Pfam::HMM::HMMResults';
use_ok( $module3) or exit;

can_ok( $module3, 'new' );
my $hmmRes = $module3->new;
isa_ok( $hmmRes, $module3 );

foreach my $attr_name (qw(hmmerVersion hmmName seqDB hmmLength thisFile seedName seqs units domThr seqThr evalueThr domTC seqTC domNC seqNC)){
  has_attribute_ok($hmmRes, $attr_name, "$module3 has attribute $attr_name");
}


my $module = 'Bio::Pfam::HMM::HMMResultsIO';

use_ok( $module) or exit;

can_ok( $module, 'new' );
my $hmmResIO = $module->new;
isa_ok( $hmmResIO, $module );

foreach my $attr_name (qw(align outfile pfamout)){
  has_attribute_ok($hmmResIO, $attr_name, "$module has attribute $attr_name");
}

#Can we parse stored HMMER3 output
ok($hmmResIO->parseHMMER3("data/OUTPUT.3.mini"), 'Parse HMM output');
warning_is(sub{ $hmmResIO->parseHMMER3("data/OUTPUT.3.mini") }, undef, "No warning in parseHMMER3"); # orno_warnings
my $hmmResTest = $hmmResIO->parseHMMER3("data/OUTPUT.3.mini");
isa_ok($hmmResTest, $module3) ;

#Test bad files 
dies_ok(sub{ $hmmResIO->parseHMMER3("dgkrngati") }); 

#Test parsing the alignment section
my $hmmResWithAlign = $hmmResIO->parseHMMER3("data/OUTPUT.3.mini");
$hmmResIO->align(0);
isa_ok($hmmResWithAlign, $module3) ;
$hmmResIO->align(1);

#Can we parse newly generate HMMER3 output
SKIP: {
    eval {
      system($conf->hmmer3bin."/hmmsearch --seed 24 --seqZ 5323441 data/HMM.3 data/minidb.fa > /tmp/OUTPUT.mini") and die "Failed to run hmmsearch:[$!]";
    };
    if($@){
      print STDERR "\n\n\n ***** Failed to run hmmsearch, bad things could be going on! *****\n\n";
      skip ' ***** failed to run HMMER:['.$@.'] *****', 2;
    }
    my $hmmResTest2;
    warning_is(sub{$hmmResTest2 = $hmmResIO->parseHMMER3("/tmp/OUTPUT.mini")}, undef, "No warning parseHMMER3 on new output");
    is_deeply($hmmResTest2, $hmmResTest, "Same objects produced from stored and calculated hmmer searches");
};

can_ok($module3, 'eachHMMSeq' );
my $arrayRef = $hmmResTest->eachHMMSeq;
ok(ref($arrayRef) eq 'ARRAY', 'Got array ref from eachHMMSeq');

my $noSeqs = scalar(@{$arrayRef});
my $noSeqsExpect = 30;
is( $noSeqs, $noSeqsExpect, "Number of sequences okay"); 


#write_scores_file

can_ok($module, 'writeScoresFile');
my $filename = "data/scores";
$hmmResIO->scores( $filename );
$hmmResIO->writeScoresFile( $hmmResTest );
ok(-s $filename, "Got scores file with size okay");
dies_ok(sub{ $hmmResIO->writeScoresFile }, 'Died in writeScoresFile with no params passed');
dies_ok(sub{ $hmmResIO->writeScoresFile('jibberish') }, 'Died in writeScoresFile with non Bio::Pfam::HMM::HMMResults object');


my $evalue = "0.001";
can_ok($module3, 'domainBitsCutoffFromEvalue');
my $bits = $hmmResTest->domainBitsCutoffFromEvalue($evalue);
my $expBits = 25.0; 
is($bits, $expBits, "Bit score from evalue"); 

$hmmResTest->domThr($expBits);
$hmmResTest->seqThr($expBits);

can_ok($module3, 'lowestTrue');
my ($sTC, $dTC) = $hmmResTest->lowestTrue;
my $expSTC = 233.8;  
my $expDTC = 232.5;
is($sTC, $expSTC, "Sequence Trusted cut-off okay\n");
is($dTC, $expDTC, "Doman Trusted cut-off okay\n");  
ok($sTC >= $expBits, "Lowest sequence true is less than threshold");
ok($dTC >= $expBits, "Lowest domain true is less than threshold");

#$hmmResTest 
can_ok($module3, 'highestNoise');
my ($sNC, $dNC) = $hmmResTest->highestNoise;
my $expSNC = 17.6;  
my $expDNC = 17.5;
is($sNC, $expSNC, "Sequence Noise cut-off okay\n");
is($dNC, $expDNC, "Doman Noise cut-off okay\n");

ok($sNC < $expBits, "Noise sequence cut-off domain is greater than threshold");
ok($dNC < $expBits, "Noise domain cut-off domain is greater than threshold");


#Parse HMMER2 output
can_ok( $module, 'parseHMMER2' );
open(H2, 'data/OUTPUT.2.mini' ) || die "Could not open data/OUTPUT.2.mini";
my $hmmResH2 = $hmmResIO->parseHMMER2(\*H2);
close(H2);

#Parse HMMER1 output
can_ok( $module, 'parseHMMER1' );
open(H1, 'data/OUTPUT.1.mini' ) || die "Could not open data/OUTPUT.1.mini";
my $hmmResH1 = $hmmResIO->parseHMMER1(\*H1);
close(H1);

#Write PFAMOUT file
can_ok( $module, 'writePFAMOUT' );
$hmmResIO->pfamout("/tmp/PFAMOUT");
$hmmResIO->writePFAMOUT($hmmResTest);
ok(-s $hmmResIO->pfamout, "PFAMOUT written okay");
#unlink($hmmResIO->pfamout);

dies_ok(sub{ $hmmResIO->writePFAMOUT }, 'Died in writePFAMOUT with no params passed');
dies_ok(sub{ $hmmResIO->writePFAMOUT('jibberish') }, 'Died in writePFAMOUT with non Bio::Pfam::HMM::HMMResults object');


#Parse PFAMOUT
can_ok( $module, 'parsePFAMOUT' );
$hmmResIO->pfamout("data/PFAMOUT.3");
my $hmmResPfam = $hmmResIO->parsePFAMOUT( $hmmResIO->pfamout );
ok($hmmResPfam , "PFAMOUT parsed okay");

dies_ok(sub{ $hmmResIO->parsePFAMOUT('grjgkr') }, 'Died on parsePFAMOUT with non existent file');


unlink($hmmResIO->scores);

#
dies_ok(sub{ $hmmResIO->pfamout("/tmp/someOutthereincyberspace/PFAMOUT"); 
              $hmmResIO->writePFAMOUT($hmmResTest) },
              'Died trying to write PFAMOUT to bogus file');
dies_ok(sub{ $hmmResIO->scores("/tmp/someOutthereincyberspace/scores"); 
              $hmmResIO->writeScoresFile($hmmResTest) },
              'Died trying to write scores to bogus file');            
             
          
