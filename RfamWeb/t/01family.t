
use strict;
use warnings;

# use Test::More qw( no_plan );
use Test::More tests => 106;

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { 
  $ENV{RFAMWEB_CONFIG} ||= '../PfamConfig/RfamWeb/rfamweb.conf';
  use_ok 'Catalyst::Test', 'RfamWeb';
}

my $server = 'http://localhost';
my $family = "$server/family";
my $acc    = 'RF00360';
my $id     = 'snoZ107_R87';
my $desc   = 'Small nucleolar RNA Z107/R87';

my $req = GET( "$family/$id" );
my $res = request( $req );

# basic page
ok( $res = request( $req ), 'Basic request to family page' );
ok( $res->is_success, 'Family page successful' );
is( $res->content_type, 'text/html', 'HTML Content-Type' );

like( $res->content, qr/$desc/, "Contains the words '$desc'" );
like( $res->content, qr/$acc/, "Contains the word '$acc'" );

like( $res->content, qr|$server/static/css/rfam.css|, 'Link to "rfam.css" generated correctly' );

# different access methods

# using an accession
$req = GET( "$family/$acc" );
$res = request( $req );
ok( $res->is_success, 'Access using accession as a URL argument' );
like( $res->content, qr/$desc/, "Contains the words '$desc'" );

# methods that will redirect
$req = GET( "$family?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using accession as a parameter' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc">|, 'Redirected correctly' );

$req = GET( "$family?id=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using ID as a parameter' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$id">|, 'Redirected correctly' );

$req = GET( "$family?entry=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an accession' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc">|, 'Redirected correctly' );

$req = GET( "$family?entry=$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an ID' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$id">|, 'Redirected correctly' );

$req = GET( "$family?entry=$id&x=y" ); # legacy URL with extra param
$res = request( $req );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$id\?x=y">|, 'Redirect correctly carries extra params' );

# bad accession/ID
$req = GET( "$family/wibble" );
ok( $res = request( $req ), 'Request with bad ID still succeeded' );
like( $res->content, qr/No valid Rfam family accession or ID/, 'Got "no valid family" message' );

# accession/ID conversions
$req = GET( "$family/$id/acc" );
$res = request( $req );
is( $res->content, $acc, 'Convert from accession to ID' );

$req = GET( "$family/$acc/id" );
$res = request( $req );
is( $res->content, $id, 'Convert from ID to accession' );
 
# try to convert bad accession/ID
$req = GET( "$family/wibble/acc" );
$res = request( $req );
is( $res->code, 404, '"404" when trying to convert bad ID to accession' );

$req = GET( "$family/wibble/id" );
$res = request( $req );
is( $res->code, 404, '"404" when trying to convert bad accession to ID' );

# page components

# varna applet
$req = GET( "$family/$acc/varna" );
$res = request( $req );
ok( $res->is_success, 'Applet tool page request successful' );
like( $res->content, qr/new VarnaControl/, 'Contains a call to the varna JS object' );

$req = GET( "$family/varna/$id" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Access using the entry parameter with an ID' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$id/varna">|, 'Redirected correctly' );

# SS images
$req = GET( "$family/$acc/image/normal" );
$res = request( $req );
ok( $res->is_success, 'SS image request successful' );
like( $res->header('Content-Type'), qr|image/png|, 'Returns response with correct Content-Type header' );

$req = GET( "$family/$acc/image/con" ); # check just one other type, to make sure the first wasn't a fluke
$res = request( $req );
ok( $res->is_success, '"con" SS image request successful' );
like( $res->header('Content-Type'), qr|image/png|, 'Correct Content-Type header' );

$req = GET( "$family/$acc/image/wibble" ); # check that a non-existent type still returns an image
$res = request( $req );
ok( $res->is_success, 'Bad SS image request still successful' );
like( $res->header('Content-Type'), qr|image/png|, 'Still returns response with correct Content-Type header' );

# CM
$req = GET( "$family/$acc/cm" );
$res = request( $req );
ok( $res->is_success, 'CM request successful' );
like( $res->header('Content-Disposition'), qr|filename=$acc.cm|, 'Returns correct filename' );
like( $res->content, qr|^INFERNAL.*?^NAME|ms, 'Looks like a CM' );
# like( $res->content, qr|^BCOM.*?^CCOM.*?^SCOM|ms, 'Looks like a CM' ); # only valid for Infernal 1.0 CMs
like( $res->content, qr|NAME\s+$id\nACC      $acc|m, 'Looks like the right CM' );

$req = GET( "$family/$acc/cm/1.0" );
$res = request( $req );
ok( $res->is_success, 'CM request with version successful' );
like( $res->header('Content-Disposition'), qr|filename=$acc.cm|, 'Still returns correct filename' );
like( $res->content, qr|^INFERNAL.*?^NAME|ms, 'Looks like a CM' );
# like( $res->content, qr|^BCOM.*?^CCOM.*?^SCOM|ms, 'Looks like a CM' ); # only valid for Infernal 1.0 CMs
like( $res->content, qr|NAME\s+$id\nACC      $acc|m, 'Still looks like the right CM' );

$req = GET( "$family/cm?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old CM URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/cm/">|, 'Redirected correctly' );

# regions
$req = GET( "$family/$acc/regions" );
$res = request( $req );
ok( $res->is_success, 'Regions request successful' );
like( $res->content, qr|Rfam regions for family $id|, 'Looks like a regions file' );

$req = GET( "$family/regions?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old regions URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/regions">|, 'Redirected correctly' );

# trees
$req = GET( "$family/$acc/tree/full/label/species/map?content-type=text/html" );
# NOTE: need to add "content-type" param, otherwise, because the LWP user agent
# doesn't specify HTML as its preferred content type, the RESTful interface
# issues an error saying it only makes sense to return HTML...
$res = request( $req );
ok( $res->is_success, 'Tree map request successful' );
like( $res->content, qr|id="species_full_img".*?id="species_full_map"|ms, 'Looks like a tree map HTML snippet' );

$req = GET( "$family/$acc/tree/full/label/species/image" );
$res = request( $req );
ok( $res->is_success, 'Tree image request successful' );
ok( length( $res->content ) > 100, 'Image has sensible size' );
like( $res->header('Content-Type'), qr|image/gif|, 'Returns response with correct Content-Type header' );

$req = GET( "$family/tree?acc=$acc&alnType=full&label=acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old tree HTML snippet URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/tree/full/label/acc/map">|, 'Redirected correctly' );

$req = GET( "$family/$acc/tree/seed" );
$res = request( $req );
ok( $res->is_success, 'Tree data request successful' );
like( $res->content, qr|^\(.*?\:\d+\.\d+,.*?\)|, 'Looks like tree data' );

$req = GET( "$family/tree/download?acc=$acc&alnType=full" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old tree data URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/tree/full">|, 'Redirected correctly' );

# structure mapping - needs a different test case (RF00002 has structure mappings)
$req = GET( "$family/RF00002/structures" );
$res = request( $req );
ok( $res->is_success, 'Structure mapping request successful' );
like( $res->content, qr|\<table class="details" id="structuresTable"\>|ms, 'Looks like a structure mapping table' );
my $num_mapping_rows = () = $res->content =~ m/\<tr\>/g; # see "perldoc perlfaq4", "How can I count the number of occurrences of a substring within a string?"
ok( $num_mapping_rows > 1, 'Got at least one mapping row' );

# $req = GET( "$family/RF00002/structures" );
$req = GET( "$family/RF00002/structures?content-type=text/plain" );
$res = request( $req );
like( $res->content, qr|\# Rfam structure mapping for family 5_8S_rRNA \(RF00002\)|ms, 'Looks like a structure mapping in plain text' );
my $num_rows_in_text_mapping = grep ! m/^#/, split m/\n/, $res->content;
ok( $num_rows_in_text_mapping == $num_mapping_rows, 'Got correct number of mapping rows' );

$req = GET( "$family/structures/mapping?acc=$acc" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old structure mapping URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/structures">|, 'Redirected correctly' );

# alignments

# jalview
$req = GET( "$family/$acc/alignment/seed/jalview" );
$res = request( $req );
ok( $res->is_success, 'Jalview alignment request successful' );
like( $res->content, qr|<applet code="jalview.bin.JalviewLite"|, 'Found jalview applet tag' );

$req = GET( "$family/alignment/jalview?acc=$acc&alnType=seed&nseLabels=0&viewer=jalview" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old jalview alignment URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/alignment/seed/jalview\?nseLabels=0">|, 'Redirected correctly' );

# HTML blocks
$req = GET( "$family/$acc/alignment/seed/html" );
$res = request( $req );
ok( $res->is_success, 'HTML alignment block request successful' );
like( $res->content, qr|\<h1\>Seed sequence alignment for $acc\</h1\>|ms, 'Looks like an HTML alignment page' );
$num_mapping_rows = () = $res->content =~ m|\<span class="alignment_seq"\>|g;
ok( $num_mapping_rows > 1, 'Got at least one alignment row' );

$req = GET( "$family/alignment/html?acc=$acc&alnType=seed&viewer=html" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old HTML alignment URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/alignment/seed/html">|, 'Redirected correctly' );

# colorstock
$req = GET( "$family/$acc/alignment/seed/colorstock" );
$res = request( $req );
ok( $res->is_success, 'Colorstock HTML alignment block request successful' );
like( $res->content, qr|\<!DOCTYPE html|ms, 'Looks like a colorstock HTML alignment page' );

$req = GET( "$family/$acc/alignment/seed/colorstock?gzip=1" );
$res = request( $req );
ok( $res->is_success, 'Compressed colorstock HTML alignment block request successful' );
is( $res->content_type, 'application/x-gzip', 'gzip Content-Type' );
my $uncompressed_content = Compress::Zlib::memGunzip( $res->content );
like( $uncompressed_content, qr|\<!DOCTYPE html|ms, 'Looks like a colorstock HTML alignment page' );

$req = GET( "$family/alignment/download/gzipped?acc=$acc&alnType=seed&cs=1" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old colorstock alignment URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/alignment/seed/colorstock">|, 'Redirected correctly' );

# stockholm format
$req = GET( "$family/$acc/alignment/seed/stockholm?download=1" );
$res = request( $req );
ok( $res->is_success, 'Stockholm alignment block request successful' );
like( $res->content, qr|# STOCKHOLM 1.0\n#=GF ID    $id\n#=GF AC    $acc|ms, 'Looks like a stockholm alignment' );
my $num_seqs_in_seed = () = $res->content =~ m/^\#=GS /msg;
like( $res->header('Content-Disposition'), qr|filename=${acc}_seed.stockholm.txt|, 'Returns correct filename' );

$req = GET( "$family/$acc/alignment/full/stockholm" );
$res = request( $req );
my $num_seqs_in_full = () = $res->content =~ m/^\#=GS /msg;
ok( $num_seqs_in_full > $num_seqs_in_seed, "More sequences in full alignment than seed in Stockholm file for $acc ($num_seqs_in_full > $num_seqs_in_seed)" );

$req = GET( "$family/alignment/download/gzipped?acc=$acc&alnType=seed" ); # legacy URL
$res = request( $req );
ok( $res->is_redirect, 'Old "gzipped" alignment URL redirects' );
like( $res->content, qr|This item has moved <a href="http://localhost/family/$acc/alignment/seed/stockholm\?gzip=1">|, 'Redirected correctly' );

# view rather than download
$req = GET( "$family/$acc/alignment/full/stockholm" );
$res = request( $req );
is( $res->header('Content-Disposition'), undef, 'No "Content-Disposition" header when not requesting download' );

# no alignment type specified
$req = GET( "$family/$acc/alignment" );
$res = request( $req );
ok( $res->is_success, 'Alignment request with no alignment type or format specified' );
like( $res->content, qr|# STOCKHOLM 1.0\n#=GF ID    $id\n#=GF AC    $acc|ms, 'Looks like a stockholm alignment' );
my ( $sq ) = $res->content =~ m/\#=GF SQ\s+(\d+)/;
is( $sq, $num_seqs_in_seed, 'Correct number of rows for seed alignment; default alignment type is seed' );

# no format specified
$req = GET( "$family/$acc/alignment/seed" );
$res = request( $req );
ok( $res->is_success, 'Alignment request with alignment type but no format specified' );
like( $res->content, qr|# STOCKHOLM 1.0\n#=GF ID    $id\n#=GF AC    $acc|ms, 'Looks like a stockholm alignment' );
( $sq ) = $res->content =~ m/\#=GF SQ\s+(\d+)/;
is( $sq, $num_seqs_in_seed, "Correct number of rows for seed alignment" );

# gzipped
$req = GET( "$family/$acc/alignment/seed?gzip=1" );
$res = request( $req );
ok( $res->is_success, 'Compressed Stockholm alignment request successful' );
is( $res->content_type, 'application/x-gzip', 'gzip Content-Type' );
$uncompressed_content = Compress::Zlib::memGunzip( $res->content );
like( $uncompressed_content, qr|# STOCKHOLM 1.0\n#=GF ID    $id\n#=GF AC    $acc|ms, 'Looks like a stockholm alignment' );

# pfam
$req = GET( "$family/$acc/alignment/seed/pfam" );
$res = request( $req );
ok( $res->is_success, 'Pfam alignment request successful' );
like( $res->content, qr|# STOCKHOLM 1.0\n#=GF ID $id\n#=GF AC $acc|ms, 'Has a stockholm header' );
my $label = ( $res->content =~ m/^\#=GS ([\w\.]+)/msg )[0];
my $num_label_occurences = () = $res->content =~ m/^$label/msg;
is( $num_label_occurences, 1, 'One row per sequence' );

# FASTA
$req = GET( "$family/$acc/alignment/seed/fasta" );
$res = request( $req );
ok( $res->is_success, 'FASTA alignment block request successful' );
like( $res->content, qr|^>O.sativa.\d|, 'Looks like a FASTA alignment' );

$req = GET( "$family/$acc/alignment/seed/FASTA" );
$res = request( $req );
like( $res->content, qr|^>O.sativa.\d|, 'Format request still generates FASTA when format is capitalised' );

# FASTAU
$req = GET( "$family/$acc/alignment/seed/fastau" );
$res = request( $req );
ok( $res->is_success, 'FASTA alignment block request successful' );
my @content = split m/\n/, $res->content;
like( $content[1], qr|^[ACGU]+$|, 'Looks like an ungapped FASTA alignment' );

