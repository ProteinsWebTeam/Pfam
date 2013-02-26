
use strict;
use warnings;

use Test::More qw( no_plan );
# use Test::More tests => 120;

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { use_ok 'Catalyst::Test', 'PfamWeb' }

my $url = 'http://localhost/families';
my $acc    = 'PF02171';
my $id     = 'Piwi';
my $desc   = 'Piwi domain';

my $req = GET($url);
my $res = request($req);

# basic page
ok( $res = request( $req ), 'Basic request to list-of-families page' );
ok( $res->is_success, 'Basic page request successful' );
is( $res->content_type, 'text/html', 'HTML Content-Type' );

like( $res->content, qr/This is a list of all \d+ Pfam-A families/, 'Found explantory text' );
like( $res->content, qr/$acc/, "Contains accession for 'Piwi'" );
like( $res->content, qr/$id/, "Contains ID for 'Piwi'" );
like( $res->content, qr/$desc/, "Contains description for 'Piwi'" );

# text output
$req = GET( $url . '?output=text' );
ok( $res = request( $req ), 'List of-families in plain text' );
ok( $res->is_success, 'Text list request successful' );
is( $res->content_type, 'text/plain', 'text/plain Content-Type' );

like( $res->content, qr/\# list of all \d+ Pfam-A families, generated:/, 'Found description text' );
like( $res->content, qr/PF02171\s+Piwi\s+Piwi domain/, "Contains line for 'Piwi'" );

# PfamAlyzer-style output
$req = GET( $url . '?output=pfamalyzer' );
ok( $res = request( $req ), 'List of-families for PfamAlyzer' );
ok( $res->is_success, 'PfamAlyzer-format list request successful' );
is( $res->content_type, 'text/plain', 'text/plain Content-Type' );

like( $res->content, qr/Piwi\s+PF02171\s+CL0219/, "Contains line for 'Piwi'" );

# Interpro-style output
$req = GET( $url . '?output=interpro' );
ok( $res = request( $req ), 'List of-families for Interpro' );
ok( $res->is_success, 'Interpro-format list request successful' );
is( $res->content_type, 'text/xml', 'text/xml Content-Type' );

like( $res->content, qr/<signature-library-release/, 'Starts with correct tag' );
like( $res->content, qr/<signature ac="PF02171" name="Piwi" type="family" desc="Piwi domain">/, "Contains record for 'Piwi'" );

