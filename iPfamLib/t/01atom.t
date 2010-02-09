
# 01atom.t
# jt6 20080130 WTSI
#
# $Id: 01atom.t,v 1.9 2008-02-01 17:35:57 jt6 Exp $

use strict;
use warnings;

use Test::More tests => 32;

use Test::Warn;
use Test::Output;
use Data::Dump qw( dump );

use Log::Log4perl qw(get_logger :levels);
BEGIN {
    # Explicit initialisation if we can't find the conf file
    Log::Log4perl->init( \<<EOF
log4perl.rootLogger=ERROR, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d{yyyy-MM-dd HH:mm:ss}: line %4L, %M: %m%n
EOF
    );
} 

get_logger( 'Bio::iPfam::Structure::Atom'    )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Entity'  )->level( $INFO );

my $module = 'Bio::iPfam::Structure::Atom';

# does the module load ?
use_ok( $module ) or exit;

# is it what we think it is ?
can_ok( $module, 'new' );
my $a = $module->new;
isa_ok( $a, $module );

# can we initialise an empty object ?
ok( my $empty_atom = $module->new, 'got an empty Atom' );

# can we initialise with an Atom ?
ok( my $another_atom = $empty_atom->new, 'got an Atom from an Atom' );

# what do we get when name is undefined ?
is( $a->name, '', 'name is the empty string' );

# can we initialise an Atom with arguments ?
ok( $a = $module->new( 
      {
        serial        => 1,
        name          => ' C  ',
        x             => 1.0,
        y             => 2.0,
        z             => 3.0,
        tempFactor    => 10.0,
      }
    ),  
    'constructor with parameters' );

# can we initialise it from an ATOM record ?
my $record = "ATOM    145  N   VAL A  25      32.433  16.336  57.540  1.00 11.92           N  \n";
ok( $a = $module->new( $record ), 'constructor with ATOM record' );

my $expected_contents = {
  hetatm     => 0,
  primary    => 0,
  serial     => '145',
  name       => ' N  ',
  realName   => 'N',
  altLoc     => '',
  resName    => 'VAL',
  chainID    => 'A',
  resSeq     => 25,
  iCode      => '',
  x          => '32.433',
  y          => '16.336',
  z          => '57.540',
  occupancy  => '1.00',
  tempFactor => '11.92',
  element    => 'N',
  charge     => ''
};
my $atom_contents;
foreach my $key ( keys %$expected_contents ) {
  $atom_contents->{$key} = $a->$key;
}
is_deeply( $atom_contents, $expected_contents,
           'check atom contents' );

# write ATOM to STDOUT
stdout_is( sub { $a->write }, $record,
           'write ATOM record to STDOUT' );

# write ATOM to filehandle
stderr_is( sub { $a->write( \*STDERR ) }, $record,
           'write ATOM record to filehandle' );

# can we set all properties from a record ?
warnings_are( sub { $a->new( $record ) }, [],
              'set properties from record' );

# test the trickier corners of the record
my $tricky_record = "ATOM    145  N  AVAL A  25B     32.433  16.336  57.540  1.00 11.92           N+1\n";
my $b = $module->new( $tricky_record );
$expected_contents = {
  hetatm     => 0,
  primary    => 0,
  serial     => '145',
  name       => ' N  ',
  realName   => 'N',
  altLoc     => 'A',
  resName    => 'VAL',
  chainID    => 'A',
  resSeq     => 25,
  iCode      => 'B',
  x          => '32.433',
  y          => '16.336',
  z          => '57.540',
  occupancy  => '1.00',
  tempFactor => '11.92',
  element    => 'N',
  charge     => '+1'
};
my $tricky_atom_contents;
foreach my $key ( keys %$expected_contents ) {
  $tricky_atom_contents->{$key} = $b->$key;
}
is_deeply( $tricky_atom_contents, $expected_contents,
           'check trickier atom contents' );

# check the catch for an invalid record
warnings_like( sub { $a->new( 'HEADER    MUSCLE PROTEIN                          02-JUN-93   1MYS                                   ' ) },
               qr/not an accepted/,
               'catch bad record type' );

# check the catch for a short record
warnings_like( sub { $a->new( 'ATOM    145  N   VAL A  25      32.433  16.336  57.540  1.00 11.92' ) },
               qr/short record/,
               'catch short record' );

# parse a HETATM
my $hetatm_record = "HETATM 1407  CA  BLE P   1      14.625  32.240  14.151  1.09 16.76           C  \n";
my $hetatm;
ok( $hetatm = $module->new( $hetatm_record ), 'parse a HETATM record' );
is( $hetatm->hetatm, 1, 'hetatm flag set correctly' );

# can we set and get properties using the accessors ?
ok( $a->serial( 2 ), 'set atom number' );
is( $a->serial, 2, 'get atom number' );

# set atom name and "real atom name"
warnings_like( sub { $a->name( 'N' ) },
               qr/setting a "raw" atom name/,
               'got warning about atom name' );
is( $a->name, ' N  ', 'name shifted to correct column' ); 
is( $a->realName, 'N', 'realName correct' ); 

# can we set all coordinates in one call ?
ok( $a->xyz( 4.0, 5.0, 6.0 ), 'set all coords' );
is_deeply( [ $a->xyz ], 
           [ 4.0, 5.0, 6.0 ],
           'get all coords' );

# check that we catch setter calls with less than three coordinates
warnings_like( sub { $a->xyz( 1.0, 2.0 ) },
               qr/must supply/,
               'warn unless all new coords set' );
is_deeply( [ $a->xyz ], 
           [ 4.0, 5.0, 6.0 ],
           'coords unchanged after failed set' );

# check that the distance calculation works
my $a1 = $module->new( { x => 1.0,
                         y => 1.0,
                         z => 1.0 } );
my $a2 = $module->new( { x => 2.0,
                         y => 2.0,
                         z => 2.0 } );
is( sprintf( '%5.3f', $a1->distance( $a2 ) ), 1.732,
    'distance calculation correct' );

# check the angle calculation
$a1 = $module->new( { x => 0,
                      y => 0,
                      z => 0 } );
$a2 = $module->new( { x => 1.0,
                      y => 0,
                      z => 0 } );
my $a3 = $module->new( { x => 1.0,
                         y => 1.0,
                         z => 0 } );
is( $a1->angle( $a2, $a3 ), 45.0,
    'angle calculation correct' );
    
# check the rotation matrix method    
$a1 = $module->new( { x => 2.001,
                      y => 1.000,
                      z => 1.000 } );
                    
my $matrix = [ [ 1.0, 0.0, 0.0, 0.0 ], 
               [ 0.0, 1.0, 0.0, 0.0 ], 
               [ 0.0, 0.0, 1.0, 0.0 ] ];
ok($a1->transform($matrix), "applied matrix");
 
is_deeply( [ $a1->xyz ],
           [ 2.001, '1.000', '1.000' ],
           'coords unchanged after applying unity matrix');

$matrix = [ [ -0.500,  0.866,  0.000,  75.270 ], 
            [ -0.860, -0.500,  0.000, 130.370 ], 
            [  0.000,  0.000,  1.000,   0.000 ] ];
$a1->transform($matrix);
is_deeply( [ $a1->xyz ],
           [ 75.135, 128.149, '1.000' ],
           'coords changed properly after applying matrix');

my $bad_matrix = [ [ -0.500,  0.866,  0.000,  75.270 ], 
                   [ -0.860, -0.500,  0.000, 130.370 ], 
                   [  0.000,  0.000,  0.000          ] ];

warnings_like( sub { $a1->transform( $bad_matrix ) },
               qr/transformation matrix incomplete/,
               'warn unless transformation matrix complete' );
