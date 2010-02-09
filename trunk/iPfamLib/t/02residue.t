
# 02residue.t
# jt6 20080130 WTSI
#
# $Id: 02residue.t,v 1.7 2008-02-01 17:36:18 jt6 Exp $

use strict;
use warnings;

use CGI;

use Test::More tests => 26;
use Test::Warn;
use Test::Output;

use Log::Log4perl qw(get_logger :levels);
BEGIN {
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
get_logger( 'Bio::iPfam::Structure::Residue' )->level( $INFO );

my $module = 'Bio::iPfam::Structure::Residue';

# does the module load ?
use_ok( $module ) or exit;

# is it what we think it is ?
can_ok( $module, 'new' );
my $r = $module->new;
isa_ok( $r, $module );

# can we initialise an empty object ?
ok( my $empty_residue = $module->new, 'got an empty Residue' );

# can we initialise with an Atom ?
ok( my $another_residue = $empty_residue->new, 'got a Residue from an Residue' );

# can we initialise it with arguments ?
ok( $r = $module->new( {
      resName => 'ALA',
      chainID => 'A',
      resSeq  => 1,
      iCode   => 'A',
    } ),
    'constructor with parameters' );

# can we initialise it from an ATOM record ?
my $record = "ATOM    145  N   VAL A  25I     32.433  16.336  57.540  1.00 11.92           N  \n";
ok( $r = $module->new( $record ), 'constructor with ATOM record' );
my $expected_contents = {
  resName => 'VAL',
  chainID => 'A',
  resSeq  => 25,
  iCode   => 'I',
};
my $residue_contents;
foreach my $key ( keys %$expected_contents ) {
  $residue_contents->{$key} = $r->$key;
}
is_deeply( $residue_contents, $expected_contents,
           'check atom contents' );

# check the catch for an invalid record
warnings_like( sub { $r = $module->new( 'HEADER    HYDROLASE' ) },
               qr/not an accepted/,
               'catch bad record type' );

$r = $module->new;

# try add_atom with undef
warnings_like( sub { $r->add_atom( undef ) },
               qr/not an Atom object/,
               'catch add_atom with undef' );

# try to delete an atom with undef
warnings_like( sub { $r->delete_atom( undef ) },
               qr/can't delete atom/,
               'catch delete_atom with undef' );

# try add_atom with something other than an Atom
warnings_like( sub { $r->add_atom( CGI->new ) },
               qr/not an Atom object/,
               'catch add_atom with non-Atom object' );

# try delete_atom with something other than an Atom
warnings_like( sub { $r->delete_atom( CGI->new ) },
               qr/not an Atom object/,
               'catch delete_atom with non-Atom object' );

# these tests are all dependent on us actually being able to instantiate 
# one or more Atom objects
my $atom = Bio::iPfam::Structure::Atom->new( $record );

die q(couldn't instantiate an Atom) 
  unless defined $atom;
  
# add a real Atom
ok( $r->add_atom( $atom ), 'add an Atom' );

# make sure it was counted...
is( $r->atom_count, 1, 'atom count correct' );

# write residue contents to STDOUT
stdout_is( sub { $r->write }, $record,
           'write residue to STDOUT' );

# write residue contents to filehandle
stderr_is( sub { $r->write( \*STDERR ) }, $record,
           'write residue to filehandle' );

# check that we can cascade the chain ID down to the atoms
ok( $r->chainID( 'Z' ), 'set chain ID on residue' );
is( $r->chainID, 'Z', 'residue has correct chain ID' );
is( $atom->chainID, 'Z', 'atom has correct chain ID' );

# get another Atom, so that we have two in the residue. Needed to exercise
# a false condition in $r->delete_atom. We can use the same ATOM record for 
# this though, but we need to change the name to avoid a warning
my $atom2 = Bio::iPfam::Structure::Atom->new( $record );
$atom2->name( ' CA  ' );
ok( $r->add_atom( $atom2 ), 'add a second Atom' );

# make sure the second Atom was counted...
is( $r->atom_count, 2, 'atom count correct' );

# can we retrieve the atom by name ?
isa_ok( $r->get_atom( 'N' ), 'Bio::iPfam::Structure::Atom',
        'retrieve atom by name' );

# delete a real Atom. Need to delete the second one, so that we hit the false
# condition as well as the true one
ok( $r->delete_atom( $atom2 ), 'delete an Atom' );

# add the second atom again
warnings_like( sub { $r->add_atom( $atom2 ) },
               qr/already exists/,
               'catch for duplicate atom' );
  
# set primary = 1 on an atom and see if we retrieve it. Again, set the flag
# on the second atom, so that we exercise both true and false
$atom2->primary( 1 );
is( $r->get_primary_atom, $atom2, 'retrieve primary atom' );
