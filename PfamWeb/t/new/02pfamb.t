
use strict;
use warnings;

use Test::More qw( no_plan );

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { use_ok 'Catalyst::Test', 'PfamWeb' }

my $server = 'http://localhost';
my $pfamb  = "$server/pfamb";
my $id     = 'Pfam-B_34907';
my $acc    = 'PB034907';
my $acc2   = 'PB141703';

my $req = GET( "$pfamb/$acc" );
my $res = request( $req );

# basic page
ok( $res = request( $req ), 'Basic request to family page' );
ok( $res->is_success, 'Family page successful' );
is( $res->content_type, 'text/html', 'HTML Content-Type' );
like( $res->content, qr/PfamB $acc/, 'Looks like the page for $acc' );

# different access methods

# using an ID
$req = GET( "$pfamb/$id" );
$res = request( $req );
ok( $res->is_success, 'Access using ID as a URL argument' );
like( $res->content, qr/PfamB $acc/, 'Looks like the page for $acc' );

# methods that will redirect
$req = GET( "$pfamb?entry=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an accession' );
like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/pfamb\/$acc">/, 'Redirected correctly' );

$req = GET( "$pfamb?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using accession as a parameter' );
like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/pfamb\/$acc">/, 'Redirected correctly' );

$req = GET( "$pfamb?id=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using ID as a parameter' );
like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/pfamb\/$id">/, 'Redirected correctly' );

$req = GET( "$pfamb?entry=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an ID' );
like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/pfamb\/$id">/, 'Redirected correctly' );

# structures
$req = GET( "$pfamb/structures?entry=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access structures using a parameter' );
like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/pfamb\/$acc\/structures">/, 'Redirected correctly' );

# format
$req = GET( "$pfamb/alignment/download/format?acc=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style alignment format request' );
ok( $res->is_redirect, 'Old-style request alignment successful' );

$req = GET( "$pfamb/$acc/alignment/format" );
ok( $res = request( $req ), 'Basic alignment format request' );
ok( $res->is_success, 'Basic alignment format request successful' );
like( $res->content, qr/# STOCKHOLM 1.0/, 'Looks like a Stockholm file' );
like( $res->content, qr/\#=GS /, 'Has a sequence in it' );

# gaps param can be default, dashes, dot or none  
note( "The following 'format' tests are dependent on particular sequences being in the full or seed alignment for $acc" );

$req = GET( "$pfamb/$acc2/alignment/format?gaps=dashes" );
$res = request( $req );
like( $res->content, qr/-----QKIDY------------RGVRF/, 'Appears to have dashes' );

$req = GET( "$pfamb/$acc2/alignment/format?gaps=dots" );
$res = request( $req );
like( $res->content, qr/.....QKIDY............RGVRF/, 'Appears to have dots' );

$req = GET( "$pfamb/$acc2/alignment/format?gaps=none" );
$res = request( $req );
like( $res->content, qr/QKIDYRGVRF/, 'Appears to have no gaps' );

# order param can be tree or alphabetical (default is tree, "a" for alphabetical)
$req = GET( "$pfamb/$acc2/alignment/format?order=a" );
$res = request( $req );
foreach ( split m/\n/, $res ) {
  next unless m/^\w+\/\d+-\d+/;
  like( $_, qr/C3XV22_BRAFL/, 'Got correct sequence first for alphabetical order' );
  last;
}

$req = GET( "$pfamb/$acc2/alignment/format?order=t" );
$res = request( $req );
foreach ( split m/\n/, $res ) {
  next unless m/^\w+\/\d+-\d+/;
  like( $_, qr/Q6KHX9_MYCMO/, 'Got correct sequence first for tree order' );
  last;
}

# format param can be one of pfam, stockholm, fasta or MSF
$req = GET( "$pfamb/$acc2/alignment/format?format=pfam" );
$res = request( $req );
like( $res->content, qr/^Q6KHX9_MYCMO/, 'Could be Pfam format...' );

$req = GET( "$pfamb/$acc2/alignment/format?format=fasta" );
$res = request( $req );
like( $res->content, qr/^>Q6KHX9_MYCMO/, 'Could be FASTA format...' );

$req = GET( "$pfamb/$acc2/alignment/format?format=msf" );
$res = request( $req );
like( $res->content, qr/Align   MSF/, 'Could be MSF format...' );

# "download" parameter true sets Content-disposition header
is( $res->header( 'Content-Disposition' ), undef, 'No "download" param, no "Content-Disposition" header' );

$req = GET( "$pfamb/$acc2/alignment/format?download=1" );
$res = request( $req );
like( $res->header( 'Content-Disposition' ), qr/$acc2.txt/, '"download" param sets "Content-Disposition" header' );

