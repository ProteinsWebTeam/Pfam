
use strict;
use warnings;

# use Test::More qw( no_plan );
use Test::More tests => 122;
use Test::Exception;

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { use_ok 'Catalyst::Test', 'PfamWeb' } #1

my $server = 'http://localhost';
my $family = "$server/family";
# my $family = "/family";
my $acc    = 'PF02171';
my $id     = 'Piwi';
my $desc   = 'Piwi domain';
my $small_acc = 'PF01097';

my $req = GET( "$family/$id" );
my $res = request( $req );

# basic page
ok( $res = request( $req ), 'Basic request to family page' ); #2
ok( $res->is_success, 'Family page successful' ); #3
is( $res->content_type, 'text/html', 'HTML Content-Type' ); #4

like( $res->content, qr/$desc/, "Contains the words '$desc'" ); #5
like( $res->content, qr/$acc/, 'Contains the word "$acc"' ); #6

# check dynamically generated link
# like( $res->content, qr|$server/static/css/pfam.css|, 'Link to "pfam.css" is correct' ); #7
like( $res->content, qr|/static/css/pfam.css|, 'Link to "pfam.css" is correct' ); #7

# different access methods

# using an accession
$req = GET( "$family/$acc" );
$res = request( $req );
ok( $res->is_success, 'Access using accession as a URL argument' ); #8
like( $res->content, qr/$desc/, "Contains the words '$desc'" ); #9

# using an accession with a version number
$req = GET( "$family/$acc.1" );
$res = request( $req );
ok( $res->is_success, 'Access using accession with a version as a URL argument' ); #10
like( $res->content, qr/$desc/, "Contains the words '$desc'" ); #11

# methods that will redirect
$req = GET( "$family?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using accession as a parameter' ); #12
# like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/family\/$acc">/, 'Redirected correctly' ); #13
like( $res->content, qr/This item has moved <a href=".*?\/family\/$acc">/, 'Redirected correctly' ); #13

$req = GET( "$family?acc=$acc.1" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using accession with a version as a parameter' ); #14
# like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/family\/$acc">/, 'Redirected correctly' ); #15
like( $res->content, qr/This item has moved <a href=".*?\/family\/$acc">/, 'Redirected correctly' ); #15

$req = GET( "$family?id=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using ID as a parameter' ); #16
# like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/family\/$id">/, 'Redirected correctly' ); #17
like( $res->content, qr/This item has moved <a href=".*?\/family\/$id">/, 'Redirected correctly' ); #17

$req = GET( "$family?entry=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an accession' ); #18
# like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/family\/$acc">/, 'Redirected correctly' ); #19
like( $res->content, qr/This item has moved <a href=".*?\/family\/$acc">/, 'Redirected correctly' ); #19

$req = GET( "$family?entry=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an ID' ); #20
# like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/family\/$id">/, 'Redirected correctly' ); #21
like( $res->content, qr/This item has moved <a href=".*?\/family\/$id">/, 'Redirected correctly' ); #21

# bad accession/ID
$req = GET( "$family/wibble" );
ok( $res = request( $req ), 'Request with bad ID still succeeded' ); #22
like( $res->content, qr/No valid Pfam family accession or ID/, 'Got "no valid family" message' ); #23

# accession/ID conversions
$req = GET( "$family/$id/acc" );
$res = request( $req );
is( $res->content, $acc, 'Convert from accession to ID' ); #24

$req = GET( "$family/$acc/id" );
$res = request( $req );
is( $res->content, $id, 'Convert from ID to accession' ); #25

# page redirects to dead family
$req = GET( "$family/PF00065" );
$res = request( $req );
like( $res->content, qr/now a dead family/, 'Got "now a dead family" message' ); #25

# XML rendering of the family page
$req = GET( "$family/$acc?output=xml" );
$res = request( $req );
is( $res->content_type, 'text/xml', 'XML Content-Type' ); #27
like( $res->content, qr/^<\?xml version="1.0"/, 'Looks like XML' ); #28

SKIP: {
  eval { require XML::LibXML };
  skip 'XML::LibXML not installed', 2 if $@;
  my $dom;
  lives_ok { $dom = XML::LibXML->load_xml( string => $res->content ) } 'Parsed XML successfully'; #29
  isa_ok( $dom, 'XML::LibXML::Document' ); #30
}

$req = GET( "$family?acc=$acc&output=xml" );
$res = request( $req );
ok( $res->is_redirect, 'Access XML using the "acc" parameter with an accession' ); #31
# like( $res->content, qr/This item has moved <a href="http:\/\/localhost\/family\/$acc\?output=xml">/, 'Redirected request for XML correctly' ); #32
like( $res->content, qr/This item has moved <a href=".*?\/family\/$acc\?output=xml">/, 'Redirected request for XML correctly' ); #31

# page components

# domain graphics
$req = GET( "$server/domaingraphics/$acc" );
ok( $res = request( $req ), 'Domain graphics request' ); #33
ok( $res->is_success, 'Domain graphics request successful' ); #34
like( $res->content, qr/start of graphics row \d/, 'Contains a domain graphic row' ); #35
like( $res->content, qr/There (is|are) \d+ sequences/, 'Got at least one architecture' ); #36

# logo
$req = GET( "$family/$id/logo" );
ok( $res = request( $req ), 'Logo HTML request' ); #37
ok( $res->is_success, 'Logo HTML request successful' ); #38
like( $res->content, qr/<img .*?logo_image/, 'Contains link to image' ); #39

$req = GET( "$family/logo_image?entry=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style logo image request' ); #40
ok( $res->is_redirect, 'Old-style logo image redirects' ); #41

$req = GET( "$family/$id/logo_image" );
ok( $res = request( $req ), 'Logo image request' ); #42
ok( $res->is_success, 'Logo image request successful' ); #43
is( $res->content_type, 'image/png', 'Got a PNG image' ); #44

# trees
$req = GET( "$family/tree?acc=$acc&alnType=seed" ); # legacy URL
ok( $res = request( $req ), 'Old-style seed tree HTML request' ); #45
ok( $res->is_redirect, 'Old-style seed tree HTML request successful' ); #46

$req = GET( "$family/$id/tree/html" );
ok( $res = request( $req ), 'Seed tree HTML request' ); #47
ok( $res->is_success, 'Seed tree HTML request successful' ); #48
like( $res->content, qr/seed_tree/, 'Seed tree HTML request successful' ); #49

$req = GET( "$family/$id/tree/download" );
ok( $res = request( $req ), 'Seed tree download request' ); #50
ok( $res->is_success, 'Seed tree download request successful' ); #51
like( $res->content, qr/^\(.*?\)\;/, 'Seed tree download request successful' ); #52

# tree images
$req = GET( "$family/$id/tree/image" );
ok( $res = request( $req ), 'Seed tree image request' ); #53
ok( $res->is_success, 'Seed tree image request successful' ); #54
is( $res->content_type, 'image/gif', 'Got a GIF image' ); #55
ok( length($res->content) > 62, 'Image has sensible size' ); #56

#************ fix these to check that /family/Piwi/seed/html works and /family/Piwi/full/html gives a warning
$req = GET( "$family/$id/tree/seed/html" );
ok( $res = request( $req ), 'Old-style seed tree HTML request' ); #57
ok( $res->is_redirect, 'Request is redirected' ); #58
like( $res->content, qr/This item has moved <a href=".*?\/family\/$acc\/tree\/html">/, 'Redirected request for seed HTML correctly' ); #59

# $req = GET( "$family/$id/tree/full/image" );
# ok( $res = request( $req ), 'Full tree image request' );
# ok( $res->is_success, 'Full tree image request successful' );
# is( $res->content_type, 'image/gif', 'Got a GIF image' );
# ok( length($res->content) > 62, 'Image has sensible size' );

# HMM file
$req = GET( "$family/hmm?entry=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style HMM request' );
ok( $res->is_redirect, 'Old-style HMM request redirects' );

$req = GET( "$family/$id/hmm" );
ok( $res = request( $req ), 'HMM request' );
ok( $res->is_success, 'HMM request successful' );
like( $res->content, qr/NAME  $id/, 'Looks like an HMM' );

# PDB image block
$req = GET( "$family/structures?acc=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style PDB image block request' );
ok( $res->is_redirect, 'Old-style PDB image block request redirects' );

$req = GET( "$family/$id/structures" );
ok( $res = request( $req ), 'PDB image block request' );
ok( $res->is_success, 'PDB image block request successful' );
like( $res->content, qr/start of pdb image block/, 'Got PDB image block' );

# structure mapping
$req = GET( "$family/structures/mapping?acc=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style structures table request' );
ok( $res->is_redirect, 'Old-style structures table request redirects' );

$req = GET( "$family/$id/mapping" );
ok( $res = request( $req ), 'Structures table request' );
ok( $res->is_success, 'Structures table request successful' );
like( $res->content, qr/structuresTable/, 'Got structures table' );

# alignments

# raw Stockholm-format alignment
$req = GET( "$family/$id/alignment/seed" );
ok( $res = request( $req ), 'Raw seed alignment request' );
ok( $res->is_success, 'Raw seed alignment request successful' );
like( $res->content, qr/\#=GF ID   $id/, 'Looks like a Stockholm file' );

$req = GET( "$family/$id/alignment/full" );
ok( $res = request( $req ), 'Raw full alignment request' );
ok( $res->is_success, 'Raw full alignment request successful' );
like( $res->content, qr/\#=GF ID   $id/, 'Looks like a Stockholm file' );

# gzipped
$req = GET( "$family/alignment/download/gzipped?acc=$acc&alnType=seed" ); # legacy URL
ok( $res = request( $req ), 'Old-style gzipped seed alignment request' );
ok( $res->is_redirect, 'Old-style gzipped seed alignment request successful' );

$req = GET( "$family/$id/alignment/seed/gzipped" );
ok( $res = request( $req ), 'Seed alignment (gzipped) request' );
ok( $res->is_success, 'Seed alignment (gzipped) request successful' );
my $seed = Compress::Zlib::memGunzip( $res->content );
ok( $seed, 'Uncompressing alignment successful' );
like( $seed, qr/\#=GF ID   $id/, 'Uncompressed content looks like a Stockholm file' );

# html
$req = GET( "$family/alignment/download/html?acc=$acc&alnType=seed" ); # legacy URL
ok( $res = request( $req ), 'Old-style gzipped seed alignment request' );
ok( $res->is_redirect, 'Old-style gzipped seed alignment request successful' );

$req = GET( "$family/$id/alignment/seed/html" );
ok( $res = request( $req ), 'HTML seed alignment request' );
ok( $res->is_success, 'HTML seed alignment request successful' );
like( $res->content, qr/Seed sequence alignment for $acc/, 'HTML seed alignment looks good' );

# heatmap
$req = GET( "$family/alignment/download/heatmap?acc=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style heatmap alignment request' );
ok( $res->is_redirect, 'Old-style heatmap alignment request successful' );

$req = GET( "$family/$small_acc/alignment/full/heatmap" );
ok( $res = request( $req ), 'Heatmap alignment request' );
ok( $res->is_success, 'Heatmap alignment request successful' );
like( $res->content, qr/Full heatmap alignment for $small_acc/, 'Heatmap alignment looks good' );

$req = GET( "$family/$small_acc/alignment/rp15/heatmap" );
ok( $res = request( $req ), 'RP15 heatmap alignment request' );
ok( $res->is_success, 'RP15 heatmap alignment request successful' );
like( $res->content, qr/Rp15 heatmap alignment for $small_acc/, 'RP15 heatmap alignment looks good' );

$req = GET( "$family/$small_acc/alignment/seed/heatmap" );
ok( $res = request( $req ), 'Bad heatmap alignment request' );
ok( $res->is_success, 'Missing heatmap request successful' );
like( $res->content, qr/We do not have the seed heatmap alignment for $small_acc/, 'Heatmaps not always available' );

# DAS alignment
$req = GET( "$family/alignment/dasviewer?acc=$acc&alnType=full" ); # legacy URL
ok( $res = request( $req ), 'DAS alignment viewer download request (legacy URL)' );
ok( $res->is_redirect, 'Old-style request for DAS alignment viewer successful' );

$req = GET( "$family/$id/alignment/seed/dasviewer" );
ok( $res = request( $req ), 'DAS viewer download request' );
like( $res->content, qr/Pfam alignment viewer/, 'Got a Pfam alignment viewer window' );

# format
$req = GET( "$family/alignment/download/format?acc=$acc" ); # legacy URL
ok( $res = request( $req ), 'Old-style alignment format request' );
ok( $res->is_redirect, 'Old-style request alignment successful' );

$req = GET( "$family/$id/alignment/seed/format" );
ok( $res = request( $req ), 'Basic alignment format request' );
ok( $res->is_success, 'Basic alignment format request successful' );
like( $res->content, qr/# STOCKHOLM 1.0/, 'Looks like a Stockholm file' );
like( $res->content, qr/\#=GS /, 'Has a sequence in it' );

# gaps param can be default, dashes, dot or none  
note( "The following 'format' tests are dependent on particular sequences being in the full or seed alignment for $id" );

$req = GET( "$family/$id/alignment/seed/format?gaps=dashes" );
$res = request( $req );
like( $res->content, qr/CIIVVLQS-KNSDI-YMTVK/, 'Appears to have dashes' );

$req = GET( "$family/$id/alignment/seed/format?gaps=dots" );
$res = request( $req );
like( $res->content, qr/CIIVVLQS.KNSDI.YMTVK/, 'Appears to have dots' );

$req = GET( "$family/$id/alignment/seed/format?gaps=none" );
$res = request( $req );
like( $res->content, qr/CIIVVLQSKNSDIYMTVK/, 'Appears to have no gaps' );

# case param can be u or l
$req = GET( "$family/$id/alignment/full/format?case=u" );
$res = request( $req );
like( $res->content, qr/KDKPVVNKDLT/, 'Appears to be uppercase' );

$req = GET( "$family/$id/alignment/full/format?case=l" );
$res = request( $req );
like( $res->content, qr/KDkpvvnkdlt/, 'Appears to be lowercase' );

# order param can be tree or alphabetical (default is tree, "a" for alphabetical)
$req = GET( "$family/$id/alignment/full/format?order=a" );
$res = request( $req );
foreach ( split m/\n/, $res ) {
  next unless m/^\w+\/\d+-\d+/;
  like( $_, qr/AGO1_SCHPO/, 'Got correct sequence first for alphabetical order' );
  last;
}

$req = GET( "$family/$id/alignment/full/format?order=t" );
$res = request( $req );
foreach ( split m/\n/, $res ) {
  next unless m/^\w+\/\d+-\d+/;
  like( $_, qr/YQ53_CAEEL/, 'Got correct sequence first for tree order' );
  last;
}

# format param can be one of pfam, stockholm, fasta or MSF
$req = GET( "$family/$id/alignment/seed/format?format=pfam" );
$res = request( $req );
like( $res->content, qr/^TAG76_CAEEL/, 'Could be Pfam format...' );

$req = GET( "$family/$id/alignment/seed/format?format=fasta" );
$res = request( $req );
like( $res->content, qr/^>TAG76_CAEEL/, 'Could be FASTA format...' );

$req = GET( "$family/$id/alignment/seed/format?format=msf" );
$res = request( $req );
like( $res->content, qr/Align   MSF/, 'Could be MSF format...' );

# "download" parameter true sets Content-disposition header
is( $res->header( 'Content-Disposition' ), undef, 'No "download" param, no "Content-Disposition" header' );

$req = GET( "$family/$id/alignment/seed/format?download=1" );
$res = request( $req );
like( $res->header( 'Content-Disposition' ), qr/${acc}_seed.txt/, '"download" param sets "Content-Disposition" header' );

