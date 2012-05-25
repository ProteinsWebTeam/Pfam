
use strict;
use warnings;

# use Test::More qw( no_plan );
use Test::More tests => 34;

use HTTP::Headers;
use HTTP::Request::Common;

BEGIN { 
  $ENV{RFAMWEB_CONFIG} = 't/test.conf';
  use_ok 'Catalyst::Test', 'RfamWeb';
}

use_ok 'DBI' or die 'Must have DBI';

my $server  = 'http://localhost';
my $search  = "$server/search";

my $test_seq = 'AGTTACGGCCATACCTCAGAGAATATACCGTATCCCGTTCGATCTGCGAAGTTAAGCTCTGAAGGGCGTCGTCAGTACTATAGTGGGTGACCATATGGGAATACGACGTGCTGTAGCTT';

my $rfam_test_db = 'rfam_test.db';
my $wu_test_db   = 'wu_test.db';

unlink $rfam_test_db;
unlink $wu_test_db;

diag( 'Creating test databases...' );
my ( $rfam_dbh, $wu_dbh ) = create_databases( $rfam_test_db, $wu_test_db );

my $req = GET( "$search" );
my $res = request( $req );

# basic page
ok( $res = request( $req ), 'Basic request to search page' );
ok( $res->is_success, 'Search page successful' );
is( $res->content_type, 'text/html', 'HTML Content-Type' );

like( $res->content, qr/Search Rfam/, "Contains the words 'Search Rfam'" );

like( $res->content, qr|$server/static/css/rfam.css|, 'Link to "rfam.css" generated correctly' );

# submit a sequence
$req = POST( "$search/sequence?seq=$test_seq" );
ok( $res = request( $req ), 'POST request' );
ok( $res->is_success, 'Sequence submission POST succeeded' );
is( $res->content_type, 'text/html', 'Response has expected HTML Content-Type' );

my @row = $wu_dbh->selectrow_array( q|SELECT `id`, `job_id`, `status`, `options`, `estimated_time`, `opened`, `closed`, `started`, `job_type`, `email` FROM job_history WHERE status != 'DONE'| );

like( $row[1], qr/^[a-f0-9]{8}(\-[a-f0-9]{4}){3}\-[a-f0-9]{12}$/, 'Server-generated UUID looks sensible' );

# try to retrieve results for a running job
$req = GET( "$search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de?content-type=application%2Fjson" );
$res = request( $req );
ok( $res->is_success, 'Request to retrieve incomplete results succeeded' );
is( $res->code, 202, 'Got response code 202' );

# flip status for dummy job
$wu_dbh->do( 'UPDATE job_history SET status = "DONE" WHERE job_id="b8ffe1b4-04e9-47d4-8342-21ac5cdf05de";' );

# retrieve results
$req = GET( "$search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de" );
$res = request( $req );
ok( $res->is_success, 'Request to retrieve results succeeded' );
is( $res->content_type, 'text/html', 'Response has expected HTML Content-Type' );
like( $res->content, qr|/search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de|, 'Found correct link in response' );

# different results formats

# JSON
SKIP: {
  eval { require JSON };
  skip 'JSON not installed', 4 if $@;

  $req = GET( "$search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de?content-type=application%2Fjson" );
  $res = request( $req );
  ok( $res->is_success, 'Request to retrieve results in JSON format succeeded' );
  is( $res->content_type, 'application/json', 'Response has expected "application/json" Content-Type' );

  my $json = JSON->new;
  ok( my $decoded_data = $json->decode( $res->content ), 'JSON decodes successfully' );
  is( $res->content, '{"closed":"2012-05-17 15:07:15","searchSequence":"AGTTACGGCCATACCTCAGAGAATATACCGTATCCCGTTCGATCTGCGAAGTTAAGCTCTGAAGGGCGTCGTCAGTACTATAGTGGGTGACCATATGGGAATACGACGTGCTGTAGCTT","hits":{"5S_rRNA":[{"P":"4.708e-28","score":"99.52","E":"2.072e-26","acc":"RF00001","end":119,"alignment":{"user_seq":"         1 AGUUACGGCCAUACCUCAGAGAAUAUACCGUAUCCCGUUC-GAUCUGCGAAGUUAAGCUCUGAAGGGCGUCGUCaGUACUAUAGUGGGUGACCAUAUGGGAAuACGACGU-GCUGUAGCUU 119       ","hit_seq":"         1 gcuggCggccAUAgcaggguggAaaCACCcGauCCCAUccCGaACuCgGAAGuUAAGcacccuagcgCcgaggu.GuACuggGgUggGugAccacaUGgGAa.AcuagGucgccGccagcu 119       ","ss":"           (((((((((,,,,<<-<<<<<---<<--<<<<<<_______>>-->>>>-->>---->>>>>-->><<<-<<--.-<-<<-----<<____>>----->>->.->>->>>,))))))))):           ","match":"           :: ::C:GCCAUA:C ::G:G+A A ACC: AUCCC U C GA CU :GAA UUAAGC:C:: AG:GC: :G+  GUACU++ GUGGGUGACCA+AUGGGAA AC:A:GU GC:G:: ::U           "},"strand":"+","id":"5S_rRNA","GC":"49","start":1}]},"opened":"2012-05-17 15:07:12","numHits":1,"started":"2012-05-17 15:07:13","jobId":"b8ffe1b4-04e9-47d4-8342-21ac5cdf05de"}', 'JSON matches expected string' );
}

# tab-separated values
$req = GET( "$search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de?content-type=text%2Fplain" );
$res = request( $req );
ok( $res->is_success, 'Request to retrieve results in plain text format succeeded' );
is( $res->content_type, 'text/plain', 'Response has expected "text/plain" Content-Type' );
like( $res->content, qr|^# job ID: b8ffe1b4-04e9-47d4-8342-21ac5cdf05de, |ms, 'Response has expected job ID' );
like( $res->content, qr|^5S_rRNA\s+RF00001|ms, 'Response has expected family hit' );

# tab-separated values
$req = GET( "$search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de?content-type=text%2Fxml" );
$res = request( $req );
ok( $res->is_success, 'Request to retrieve results in XML format succeeded' );
is( $res->content_type, 'text/xml', 'Response has expected "text/xml" Content-Type' );
like( $res->content, qr|^<\?xml version="1.0" encoding="UTF-8"\?>|ms, 'Looks like XML' );
like( $res->content, qr|<results job_id="b8ffe1b4-04e9-47d4-8342-21ac5cdf05de">|ms, 'Response has expected job ID' );
like( $res->content, qr|<match id="5S_rRNA" accession="RF00001" type="Rfam">|ms, 'Response has expected family hit' );
my $num_matches = grep m/<match /, split m/\n/, $res->content;
is( $num_matches, 1, 'Got expected number of matches' );

# GFF
$req = GET( "$search/sequence/b8ffe1b4-04e9-47d4-8342-21ac5cdf05de?content-type=application%2Fx-gff3" );
$res = request( $req );
ok( $res->is_success, 'Request to retrieve results in GFF format succeeded' );
like( $res->content, qr|^##gff-version 3|ms, 'Looks like GFF' );
like( $res->content, qr|^5S_rRNA\s+rfam_scan.pl|ms, 'Response has expected family hit' );
$num_matches = grep ! m/^\#\#/, split m/\n/, $res->content;
is( $num_matches, 1, 'Got expected number of matches' );

unlink $rfam_test_db;
unlink $wu_test_db;

exit;

sub create_databases {
  my ( $rfam_test_db, $wu_test_db ) = @_;

  my $rfam_dbh = DBI->connect( "dbi:SQLite:$rfam_test_db" ) 
    or die "Cannot connect to SQLite DB '$rfam_test_db': $DBI::errstr";

  $rfam_dbh->do( "CREATE TABLE `VERSION` (
                    `rfam_release` double(4,1) NOT NULL,
                    `rfam_release_date` date NOT NULL,
                    `number_families` int(10) NOT NULL,
                    `embl_release` tinytext NOT NULL 
                  )" );

  $rfam_dbh->do( "INSERT INTO `VERSION` VALUES (10.1,'2011-06-01',1973,'100');" );

  $rfam_dbh->do( "CREATE TABLE `rfam` (  `auto_rfam` INTEGER PRIMARY KEY AUTOINCREMENT,  `auto_wiki` int(10),  `rfam_acc` varchar(7),  `rfam_id` varchar(40),  `description` varchar(100) DEFAULT NULL,  `author` tinytext,  `seed_source` tinytext,  `alignment_method` tinytext,  `gathering_cutoff` double(5,2) DEFAULT NULL,  `trusted_cutoff` double(5,2) DEFAULT NULL,  `noise_cutoff` double(5,2) DEFAULT NULL,  `comment` longtext,  `previous_id` tinytext,  `cmbuild` tinytext,  `cmcalibrate` tinytext,  `cmsearch` tinytext,  `num_seed` bigint(20) DEFAULT NULL,  `num_full` bigint(20) DEFAULT NULL,  `type` varchar(50) DEFAULT NULL,  `structure_source` tinytext,  `number_of_states` mediumint(8) DEFAULT NULL,  `number_of_nodes` mediumint(8) DEFAULT NULL,  `number_of_species` bigint(20) DEFAULT NULL,  `taxonomic_domain` mediumtext,  `taxonomic_root` mediumtext,  `full_structure` longtext,  `reference_structure` longtext,  `reference_sequence` longtext,  `structure_annotations` longtext );" );

  $rfam_dbh->do( "INSERT INTO `rfam` ( `rfam_acc`, `rfam_id` ) VALUES ('RF00001','5S_rRNA');" );

  #---------------------------------------

  my $wu_dbh = DBI->connect( "dbi:SQLite:$wu_test_db" ) 
    or die "Cannot connect to SQLite DB '$wu_test_db': $DBI::errstr";

  $wu_dbh->do( qq|CREATE TABLE `job_history` (
                  `id` INTEGER PRIMARY KEY AUTOINCREMENT,
                  `job_id` varchar(40) NOT NULL,
                  `status` varchar(5) NOT NULL DEFAULT '',
                  `options` varchar(255) DEFAULT '',
                  `estimated_time` int(3) DEFAULT NULL,
                  `opened` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
                  `closed` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
                  `started` datetime NOT NULL DEFAULT '0000-00-00 00:00:00',
                  `job_type` varchar(50) NOT NULL DEFAULT '',
                  `email` varchar(255) DEFAULT NULL
                )| );

  $wu_dbh->do( qq|INSERT INTO "job_history" VALUES(1,'b8ffe1b4-04e9-47d4-8342-21ac5cdf05de','RUN',NULL,3,'2012-05-17 15:07:12','2012-05-17 15:07:15','2012-05-17 15:07:13','rfam',NULL);| );

  $wu_dbh->do( qq|CREATE TABLE `job_stream` (
                  `id` bigint(20) NOT NULL DEFAULT '0',
                  `stdin` longtext NOT NULL,
                  `stdout` mediumblob,
                  `stderr` longtext
                );| );

  my $stdout = join '', <DATA>;
  $wu_dbh->do( qq|INSERT INTO "job_stream" VALUES(1,'> UserSeq
AGTTACGGCCATACCTCAGAGAATATACCGTATCCCGTTCGATCTGCGAAGTTAAGCTCTGAAGGGCGTCGTCAGTACTATAGTGGGTGACCATATGGGAATACGACGTGCTGTAGCTT','$stdout',NULL);| );

  return ( $rfam_dbh, $wu_dbh );
}

__DATA__
# cmsearch :: search a sequence database with an RNA CM
# INFERNAL 1.0 (January 2009)
# Copyright (C) 2009 HHMI Janelia Farm Research Campus
# Freely distributed under the GNU General Public License (GPLv3)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# command:    cmsearch --tabfile /tmp/3604.res --ga /tmp/3604.cm /tmp/3604.seq
# date:       Thu May 17 15:54:56 2012
# num seqs:   1
# dbsize(Mb): 0.000238
#
# Pre-search info for CM 1: 5S_rRNA
#
#                                  cutoffs            predictions     
#                            -------------------  --------------------
# rnd  mod  alg  cfg   beta     E value   bit sc     surv     run time
# ---  ---  ---  ---  -----  ----------  -------  -------  -----------
    1  hmm  fwd  loc      -       0.690     0.78   0.4755  00:00:00.00
    2   cm  cyk  loc  1e-07       0.013     3.92   0.0091  00:00:00.02
    3   cm  ins  loc  1e-15     6.1e-05    16.00  2.9e-05  00:00:00.00
# ---  ---  ---  ---  -----  ----------  -------  -------  -----------
  all    -    -    -      -           -        -        -  00:00:00.02
#

CM: 5S_rRNA
>UserSeq/1-119

  Plus strand results:

 Query = 1 - 119, Target = 1 - 119
 Score = 99.52, E = 2.072e-26, P = 4.708e-28, GC =  49

           (((((((((,,,,<<-<<<<<---<<--<<<<<<_______>>-->>>>-->>---->>>
         1 gcuggCggccAUAgcaggguggAaaCACCcGauCCCAUccCGaACuCgGAAGuUAAGcac 60      
           :: ::C:GCCAUA:C ::G:G+A A ACC: AUCCC U C GA CU :GAA UUAAGC:C
         1 AGUUACGGCCAUACCUCAGAGAAUAUACCGUAUCCCGUUC-GAUCUGCGAAGUUAAGCUC 59      

           >>-->><<<-<<--.-<-<<-----<<____>>----->>->.->>->>>,)))))))))
        61 ccuagcgCcgaggu.GuACuggGgUggGugAccacaUGgGAa.AcuagGucgccGccagc 118     
           :: AG:GC: :G+  GUACU++ GUGGGUGACCA+AUGGGAA AC:A:GU GC:G:: ::
        60 UGAAGGGCGUCGUCaGUACUAUAGUGGGUGACCAUAUGGGAAuACGACGU-GCUGUAGCU 118     

           :
       119 u 119     
           U
       119 U 119     


#
# Post-search info for CM 1: 5S_rRNA
#
#                              number of hits       surv fraction  
#                            -------------------  -----------------
# rnd  mod  alg  cfg   beta    expected   actual  expected   actual
# ---  ---  ---  ---  -----  ----------  -------  --------  -------
    1  hmm  fwd  loc      -       0.690        2    0.4755   1.0000
    2   cm  cyk  loc  1e-07       0.013        1    0.0091   0.5000
    3   cm  ins  loc  1e-15     6.1e-05        1   2.9e-05   0.5000
#
# expected time    actual time
# -------------  -------------
    00:00:00.02    00:00:01.00
//
# Tabular version of hit list saved in file /tmp/3604.res.
#
# CPU time: 1.01u 0.00s 00:00:01.01 Elapsed: 00:00:01

