
use strict;
use warnings;

# use Test::More qw( no_plan );
use Test::More tests => 25;

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { 
  $ENV{RFAMWEB_CONFIG} ||= '../PfamConfig/RfamWeb/rfamweb.conf';
  use_ok 'Catalyst::Test', 'RfamWeb';
}

my $server = 'http://localhost';
my $clan   = "$server/clan";
my $acc    = 'CL00002';
my $id     = 'RNaseP';
my $desc   = 'The RNaseP clan contains the RNA families RNaseP_nuc';

my $req = GET( "$clan/$id" );
my $res = request( $req );

# basic page
ok( $res = request( $req ), 'Basic request to clan page' );
ok( $res->is_success, 'Clan page successful' );
is( $res->content_type, 'text/html', 'HTML Content-Type' );

like( $res->content, qr/$desc/, "Contains the words '$desc'" );
like( $res->content, qr/$acc/, "Contains the word '$acc'" );

like( $res->content, qr|$server/static/css/rfam.css|, 'Link to "rfam.css" generated correctly' );

# different access methods

# using an accession
$req = GET( "$clan/$acc" );
$res = request( $req );
ok( $res->is_success, 'Access using accession as a URL argument' );
like( $res->content, qr/$desc/, "Contains the words '$desc'" );

# methods that will redirect
$req = GET( "$clan?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using accession as a parameter' );
like( $res->content, qr|This item has moved <a href="http://localhost/clan/$acc">|, 'Redirected correctly' );

$req = GET( "$clan?id=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using ID as a parameter' );
like( $res->content, qr|This item has moved <a href="http://localhost/clan/$id">|, 'Redirected correctly' );

$req = GET( "$clan?entry=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an accession' );
like( $res->content, qr|This item has moved <a href="http://localhost/clan/$acc">|, 'Redirected correctly' );

$req = GET( "$clan?entry=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an ID' );
like( $res->content, qr|This item has moved <a href="http://localhost/clan/$id">|, 'Redirected correctly' );

$req = GET( "$clan?entry=$id&x=y" ); # legacy URL with extra param
$res = request( $req );
like( $res->content, qr|This item has moved <a href="http://localhost/clan/$id\?x=y">|, 'Redirect carries extra params' );

# bad accession/ID
$req = GET( "$clan/wibble" );
ok( $res = request( $req ), 'Request with bad ID still succeeds' );
like( $res->content, qr/No valid Rfam clan accession or ID/, 'Got "no valid clan" message' );

# page components

# structures
$req = GET( "$clan/$acc/structures" );
ok( $res = request( $req ), 'Structures request succeeds' );
like( $res->content, qr/id="structuresTable"/, 'Got structure mapping table' );
my $num_rows_in_mapping_table = grep ! m/\<tr\>/, split m/\n/, $res->content;
ok( $num_rows_in_mapping_table > 1, 'Got multiple rows in mapping table' );

$req = GET( "$clan/structures?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access mapping using accession as a parameter' );
like( $res->content, qr|This item has moved <a href="http://localhost/clan/$acc/structures">|, 'Redirected correctly' );

