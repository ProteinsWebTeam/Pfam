#!/usr/local/bin/perl

use strict;
use warnings;


use Test::More tests => 34;
use Test::Warn;
#use Test::Exception;

#We need to have the external loaction set as environment variable
ok($ENV{PFAM_CONFIG}, 'The pfam config file location is not set');

my $conf = $ENV{PFAM_CONFIG};
ok(-s $conf );

BEGIN { use_ok( 'Bio::Pfam::Config' ); }

require_ok( 'Bio::Pfam::Config' );

my $config = Bio::Pfam::Config->new;
isa_ok($config, "Bio::Pfam::Config");

my $config2 = $config->new;
isa_ok($config2, "Bio::Pfam::Config");


ok($config->location, 'Failed to find a Pfam location from the config');
like($config->location, qr/WTSI|JFRC/, 'Unknown location '.$config->location);

ok($config->hmmer2bin, 'Failed to get a hmm2location');
ok(-d $config->hmmer2bin, 'The hmmer2bin specified in the config does not exist');
 
ok($config->hmmer3bin, 'Failed to get a hmm3location');
ok(-d $config->hmmer3bin, 'The hmmer3bin specified in the config does not exist'); 

ok($config->pfamseqLoc, 'Failed to get the pfamseqLoc from the config');
ok(-d $config->pfamseqLoc, 'The pfamseqLoc does not exist');

#

ok($config->dbsize, 'Failed to get dbsize');

ok($config->farm, 'Got farm config');
my $farm = $config->farm;
isa_ok($farm, "HASH", 'Have not got a HASH response from Bio::Pfam::Config->farm');
 
#Now check that we have the HMMER2 binaries
foreach my $e (qw(hmmbuild hmmcalibrate hmmsearch hmmalign)){
  ok(-s $config->hmmer2bin."/".$e, "Find $e in the hmmer2bin");  
}

#Now check that we have the HMMER3 binaries
foreach my $e (qw(hmmbuild hmmsearch hmmalign)){
  ok(-s $config->hmmer3bin."/".$e, "Find $e in the hmmer3bin");  
}

foreach my $sub (qw(location farm pfamseqLoc hmmer3bin hmmer2bin dbsize)){ 
  warnings_like( sub{ $config->$sub('wibble') }, qr/Passed variable to ro config/, "Modified $sub without warning");
}


foreach my $f(qw(pfamseq.xpd pfamseq.xpi pfamseq.xps pfamseq.xpt)){
 ok(-s $config->pfamseqLoc."/".$f, "Check pfamseq indexed, found $f");  
}