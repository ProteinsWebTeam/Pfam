use strict;
use warnings;

use Test::More tests => 12;

# 1 - compiles

BEGIN { use_ok('Bio::Pfam::SeqPfam'); use_ok('Bio::SimpleAlign') }

require_ok('Bio::Pfam::SeqPfam');

#make new SeqPfam object
my $seq = Bio::Pfam::SeqPfam->new(
  '-id'        => 'FUCK_ECOLI',
  '-acc'       => 'P11553',
  '-rdb_index' => 438257,
  '-seq'     => "MELWRQCTHWLIQCRVLPPSHRVTWDGAQVCELAQALRDGVLLCQLLNNLLPHAINLREV",
  '-start'   => 21,
  '-desc'    => "test Seq",
  '-end'     => 80,
  '-version' => 1,
  '-current' => 2
);

isa_ok( $seq, "Bio::Pfam::SeqPfam" );
my $seq2 = $seq->new(
  '-id'    => 'FUCK_ECOLI',
  '-acc'   => 'P11553',
  '-seq'   => "MELWRQCTHWLIQCRVLPPSHRVTWDGAQVCELAQALRDGVLLCQLLNNLLPHAINLREV",
  '-start' => 21,
  '-end'   => 80,
  'seq_version' => 10,
  '-organism'   => 'Escherichia coli'
);

isa_ok( $seq2, "Bio::Pfam::SeqPfam" );

# 3 - check the id
ok( $seq->id(), "FUCK_ECOLI" );

# 4 - check the acc
ok( $seq->acc() eq "P11553" );

# 5 - find the length
ok( $seq->length == 60 );

# 6 - check the crc64
ok( $seq->crc64 eq '7A206DEA6C7A3767' );

# 7 - check that we're allowed in alignment objects
my $aln = Bio::SimpleAlign->new();
$aln->add_seq($seq);
ok( $aln->each_seq() == 1 );

my $seq3 = $seq->new(
  '-id'    => 'FUCK_ECOLI',
  '-acc'   => 'P11553',
  '-seq'   => "MELWRQCTHWLIQCRVLPPSHRVTWDGAQVCELAQALRDGVLLCQLLNNLLPHAINLREV",
  '-start' => 21,
  '-end'   => 80
);
isa_ok( $seq3, "Bio::Pfam::SeqPfam" );

use_ok('Bio::Pfam::OtherRegion');


$seq3->set_active_site( "...............................*.........................*..");
$seq3->set_sprot_pred_active_site( "...............................*.........................*..");
$seq3->set_pfam_pred_active_site( "...............................*.........................*..");
