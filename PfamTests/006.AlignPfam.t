
use strict;
use warnings;

use Test::More tests => 4;
use Test::Exception;

use Bio::Pfam::AlignPfam;

# 1 - compiles

#ok(require "Bio::Pfam::AlignPfam");# 'Loaded Bio::Pfam::AlignPfam okay') or exit;

# 2 - make object

my $aln = Bio::Pfam::AlignPfam->new();
ok( $aln->isa('Bio::Pfam::AlignPfam') );

# 3 - read a pfam alignment

open( A, "data/AlignPfam.aln" );
$aln->read_Pfam( \*A );
close A;
ok( $aln->each_seq() == 57 );

# 4 - read an annotated pfam alignment

my $aln2 = Bio::Pfam::AlignPfam->new();
open( A, "data/AlignPfam.ann" );
$aln2->read_stockholm( \*A );
close A;
ok( $aln2->each_seq() == 57 );

# 5 - retrieve a sequence

my( $seq ) = $aln->each_seq();
ok( $seq->isa('Bio::Pfam::SeqPfam') );
