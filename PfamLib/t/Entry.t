use lib 't';
use strict;
use warnings;

use Test;
use TestUtils;
use Bio::Pfam;
$| = 1;

plan tests => 18;

# 1 - compiles
ok(1);

# 2 - get a default db object
my $db = &Bio::Pfam::default_db();
ok( $db->isa('Bio::Pfam::Root'));

# 3 - get an entry object
ok(my $en = $db->get_EntryA_by_acc('PF00076'));

# 4 - Check id
ok(my $id = $en->id(), "rrm");

# 5 - Check acc is the same
ok(my $acc = $en->acc(), "PF00076");

# 6 - Check that DE line load
ok(my $desc = $en->description());

# 7 - Check Authors
ok(my $author = $en->author(), "Eddy SR, Birney E");

# 8 - Check GA
ok(my $ga = $en->gathering_cutoff());

# 9 - Check TC
ok(my $tc = $en->trusted_cutoff());

# 10 - Check NC
ok(my $nc = $en->noise_cutoff());

# 11 - Check seed source
ok( $en->source(), "Published_alignment");

# 12 - check the comment is a Bio::Annotation::Comment object
my( $firstcom ) = $en->ann->get_Annotations('comment');
ok( $firstcom->isa('Bio::Annotation::Comment') );

# 13 - get the first reference
my( $firstref ) = $en->ann()->get_Annotations('reference');
ok( $firstref->isa('Bio::Annotation::Reference') );

# 14 - reference has authors
ok( $firstref->authors() );

# 15 - add a reference comment
$firstref->comment( "a reference comment!" );
ok( $firstref->comment(), "a reference comment!" );

# 16 - Check that something has been written
open(ENTRY, ">entry.test") || die;
$en->write_std_desc_tag("","   ",\*ENTRY,1);
close(ENTRY);
ok((-s "entry.test"));
unlink("entry.test");

# 17 - get seed alignment object
my $seed = $en->seed();
ok( $seed->isa('Bio::Pfam::AlignPfam') );

# 18 - get align alignment object
my $full = $en->full();
ok( $full->isa('Bio::Pfam::AlignPfam') );


