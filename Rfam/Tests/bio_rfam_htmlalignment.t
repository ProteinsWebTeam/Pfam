
use strict;
use warnings;

use Test::More;
use Test::Exception;
use FindBin;
use FileHandle;

use Bio::Rfam::Config;

BEGIN {
  use_ok( 'Bio::Rfam::HtmlAlignment' );
}

my $dir = $FindBin::Bin;
my $tax_labels_filename = $dir . '/data/RF00504/taxonomy_labels.stockholm';
my $nse_labels_filename = $dir . '/data/RF00504/nse_labels.stockholm';
my $rfam_acc = 'RF00504';

# suck in the stockholm file without the object, so we've something we can compare 
# to the contents that we read into the object
open( FILE, "< $tax_labels_filename" )
  or die "ERROR: couldn't read Stockholm file: $!";
my $stockholm_file_contents = join '', <FILE>;
close FILE;

# get a database connection from the configuration
my $config = Bio::Rfam::Config->new;
my $schema = $config->rfamlive;

# open the Stockholm file with taxonomy labels
my $fh = FileHandle->new;
$fh->open("< $tax_labels_filename");

my $ha = Bio::Rfam::HtmlAlignment->new( stockholm => $fh, schema => $schema, rfam_acc => $rfam_acc );
isa_ok( $ha, 'Bio::Rfam::HtmlAlignment' );

$fh->open("< $tax_labels_filename"); # have to re-open to read it again
lives_ok { $ha->stockholm( $fh ) } 'can read Stockholm file from FileHandle';

is( join( '', @{ $ha->stockholm } ), $stockholm_file_contents, 'loaded file contents matches original' );

lives_ok { $ha->stockholm( $tax_labels_filename ) } 'can read Stockholm file from filename';
lives_ok { $ha->stockholm( \$stockholm_file_contents ) } 'can read Stockholm file from scalar reference';

is( scalar @{$ha->sequences}, 44, 'read correct number of sequences' );

my $expected_sequence_hash = {
  id       => 'Mycobacterium_tuberc.1',
  start    => 1,
  end      => 215,
  strand   => 1,
  sequence => '.....UGCCU.CUGGAAAGCGGUGGC..............GACCCCUGGCGGUC.......................CUCACC.CGCCGAUGGGGAAA.GGCGAUU.............................CACC.......UGACGGUGGACAGAGUCGCCGAAUCUCUCAGGCGCCUGGCGUGCAGGU..GAA.GACAGAGGGAGAGGG',
  type     => 'aligned',
};

is_deeply( $ha->sequences->[0], $expected_sequence_hash, 'got correct sequence for first row' );

# check that we can correctly parse a file with nse labels on the sequence rows
lives_ok { $ha = Bio::Rfam::HtmlAlignment->new( stockholm => $nse_labels_filename, schema => $schema, rfam_acc => $rfam_acc ) } 
  'can parse Stockholm file with taxonomy labels';

$expected_sequence_hash = {
  id       => 'BX842577.1',
  start    => 344457,
  end      => 344587,
  strand   => 1,
  type     => 'aligned',
  sequence => '.....UGCCU.CUGGAAAGCGGUGGC..............GACCCCUGGCGGUC.......................CUCACC.CGCCGAUGGGGAAA.GGCGAUU.............................CACC.......UGACGGUGGACAGAGUCGCCGAAUCUCUCAGGCGCCUGGCGUGCAGGU..GAA.GACAGAGGGAGAGGG',
};

is_deeply( $ha->sequences->[0], $expected_sequence_hash, 'got correct sequence for first row' );

$ha->build_html;

like( $ha->html_blocks->[0], qr(^<div id="html_alignment">)s, 'looks like an HTML-formatted block' );

is( $ha->count_html_blocks, 2, 'generated correct number of HTML blocks' );
is( scalar ( grep { m/class="alignment_nse/ } split "\n", $ha->html_blocks->[0] ), 30, 'correct number of labels in first block' );
is( scalar ( grep { m/class="alignment_nse/ } split "\n", $ha->html_blocks->[1] ), 14, 'correct number of labels in second block' );

done_testing();

