
use strict;
use warnings;

# use Test::More qw( no_plan );
use Test::More tests => 19;

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { 
  $ENV{RFAMWEB_CONFIG} ||= '../PfamConfig/RfamWeb/rfamweb.conf';
  use_ok 'Catalyst::Test', 'RfamWeb';
}

my $jump = 'http://localhost/search/jump';

# family accession
ok( my $res = request( GET( "$jump?entry=RF00360" ) ), 'Basic jump request' );
ok( $res->is_success, 'Jump successful' );
like( $res->content, qr|/family/RF00360|, 'Returns a URL with the family accession' );

$res = request( GET( "$jump?entry=rf00360" ) );
like( $res->content, qr|/family/rf00360|, "Search doesn't care about case" );

$res = request( GET( "$jump?entry=snoZ107_R87" ) );
like( $res->content, qr|/family/snoZ107_R87|, 'Returns a URL with the family ID' );

$res = request( GET( "$jump?entry=7SK" ) );
like( $res->content, qr|/family/7SK|, 'Returns a family URL where clan exists with same name' );

# dead family
$res = request( GET( "$jump?entry=RF00098" ) );
like( $res->content, qr|/family/RF00098|, 'Returns a URL with the dead family acc' );

# clan acc
$res = request( GET( "$jump?entry=CL00002" ) );
like( $res->content, qr|/clan/CL00002|, 'Returns a URL with the clan acc' );

$res = request( GET( "$jump?entry=RNaseP" ) );
like( $res->content, qr|/clan/RNaseP|, 'Returns a URL with the clan ID' );
# (can't use CL00001/tRNA because there's a family called "tRNA" and the jump
# goes there by default, rather than to the clan.)

# sequence
$res = request( GET( "$jump?entry=AB000684" ) );
like( $res->content, qr|/sequence/AB000684|, 'Returns a URL with the sequence acc' );

# genome
$res = request( GET( "$jump?entry=AY805074.1" ) );
like( $res->content, qr|/genome/AY805074.1|, 'Returns a URL with the genome acc' );

$res = request( GET( "$jump?entry=328672" ) );
like( $res->content, qr|/genome/328672|, 'Returns a URL with the NCBI tax ID' );

# (can't find a genome test case with an ensembl ID in 10.1)

# error cases

# no entry
$res = request( GET( $jump ) );
is( $res->code, 400, 'Returns status 400 when entry is omitted' );
is( $res->content, 'No valid accession or ID', "Returns error message when 'entry' param is missing" );

# empty "entry" param
$res = request( GET( "$jump?entry=" ) );
is( $res->code, 400, 'Returns status 400 when entry is omitted' );
is( $res->content, 'No valid accession or ID', "Returns error message when 'entry' param is empty" );

# no guess
$res = request( GET( "$jump?entry=wibble" ) );
is( $res->code, 400, 'Returns status 400 when guessing fails' );
is( $res->content, "Couldn't guess entry", 'Returns error message when guessing fails' );

