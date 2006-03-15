use lib 't';
use strict;
use warnings;

use Test;
use TestUtils;

use Bio::Pfam::SeqPfam;
use Bio::SimpleAlign;

$| = 1;

plan tests => 7;

# 1 - compiles

ok(1);

# 2 - make new SeqPfam object

my $seq = Bio::Pfam::SeqPfam -> new( '-id'    => 'FUCK_ECOLI',
				     '-acc'   => 'P11553',
				     '-seq'   => "MELWRQCTHWLIQCRVLPPSHRVTWDGAQVCELAQALRDGVLLCQLLNNLLPHAINLREV",
				     '-start' => 21,
				     '-end'   => 80 );
ok( $seq->isa('Bio::Pfam::SeqPfam') );

# 3 - check the id
ok( $seq->id(), "FUCK_ECOLI" );

# 4 - check the acc
ok( $seq->acc(), "P11553" );

# 5 - find the length
ok( $seq->length(), 60 );

# 6 - check the crc64
ok( $seq->crc64, '7A206DEA6C7A3767' );

# 7 - check that we're allowed in alignment objects
my $aln = Bio::SimpleAlign->new();
$aln -> add_seq( $seq );
ok( $aln->each_seq(), 1 );

