
use strict;
use warnings;

# use Test::More qw( no_plan );
use Test::More tests => 15;

use HTTP::Headers;
use HTTP::Request::Common;
use Data::Dump qw( dump );
use Compress::Zlib;

BEGIN { 
  $ENV{RFAMWEB_CONFIG} ||= '../PfamConfig/RfamWeb/rfamweb.conf';
  use_ok 'Catalyst::Test', 'RfamWeb';
}

my $server   = 'http://localhost';
my $genome   = "$server/genome";
my $acc      = 'AM270980.1';
my $id       = 5061;
my $desc     = 'Aspergillus niger';
my $chrom_id = 4133;

my $req = GET( "$genome/$id" );
my $res = request( $req );

# basic page
ok( $res = request( $req ), 'Basic request to genome page' );
ok( $res->is_success, 'Genome page successful' );
is( $res->content_type, 'text/html', 'HTML Content-Type' );

like( $res->content, qr/$desc/, "Contains the words '$desc'" );
like( $res->content, qr/$acc/, "Contains the word '$acc'" );

like( $res->content, qr|$server/static/css/rfam.css|, 'Link to "rfam.css" generated correctly' );

# bad accession/ID
$req = GET( "$genome/wibble" );
ok( $res = request( $req ), 'Request with bad ID still succeeded' );
like( $res->content, qr/No chromosomes found for that genome identifier/, 'Got "no chromosomes found" message' );

# using an accession
$req = GET( "$genome/$acc" );
$res = request( $req );
ok( $res->is_success, 'Access using accession as a URL argument' );
like( $res->content, qr/$desc/, "Contains the words '$desc'" );

# page components

my $num_chromosomes = grep ! m/^\s*<tr class="(odd|even)">/, split m/\n/, $res->content;
ok( $num_chromosomes > 1, 'Found some chromosomes' );

# downloads

# GFF
# http://web-vm-rfam.internal.sanger.ac.uk:3000/genome/5061/gff/4133
$req = GET( "$genome/$acc/gff/4133" );
$res = request( $req );
ok( $res->is_success, 'GFF request successful' );
like( $res->content, qr|##gff-version 3\n.*?\n##sequence-region AM270980.1|ms, 'Looks like a GFF file' );
like( $res->header('Content-Disposition'), qr|filename=${acc}.gff|, 'Returns correct filename' );

