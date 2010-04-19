use strict;
use warnings;

use Test::More tests => 13;
use Test::Warn;
use Test::Exception;

my $m = 'Bio::Pfam::AnnotatedRegion';

use_ok( $m );
can_ok( $m, 'new');

my $annReg1 = new $m;
isa_ok($annReg1, $m);

can_ok($m, 'seq_name');
$annReg1->seq_name('FUCK_ECOLI');
is($annReg1->seq_name, 'FUCK_ECOLI', 'Got seq_id back');
can_ok($m, 'from');
$annReg1->from(3);
is($annReg1->from, 3,  'Got from back');
can_ok($m, 'to');
$annReg1->to(30);
is($annReg1->to, 30,  'Got to back');
can_ok($m, 'annotation');
$annReg1->annotation('Test Annotation Region');
is($annReg1->annotation, 'Test Annotation Region',  'Got annotation back');
my $annReg2 = Bio::Pfam::AnnotatedRegion->new( ( '-seq_id' => 'FUCK_ECOLI',
                                                 '-from'   => 3,
                                                 '-to'     => 30,
                                                 '-annotation' => 'Test Annotation Region' ));
                    
is_deeply($annReg2, $annReg1, 'Made object the same, but in two different ways');
  
my $annReg3 = Bio::Pfam::AnnotatedRegion->new( ( '-SEQ_ID' => 'FUCK_ECOLI',
                                                 '-FROM'   => 3,
                                                 '-TO'     => 30,
                                                 '-ANNOTATION' => 'Test Annotation Region' ));

is_deeply($annReg3, $annReg1, 'Made object the same, but in two different ways');
  