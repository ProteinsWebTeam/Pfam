
# 03chain.t
# jt6 20080130 WTSI
#
# $Id: 03chain.t,v 1.4 2008-02-01 16:40:42 jt6 Exp $

use strict;
use warnings;

use CGI;

use Test::More tests => 32;
use Test::Warn;
use Test::Output;

use Bio::iPfam::Structure::Residue;

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
get_logger( 'Bio::iPfam::Structure::Chain'   )->level( $INFO );

my $module = 'Bio::iPfam::Structure::Chain';

# does the module load ?
use_ok( $module ) or exit;

# is it what we think it is ?
can_ok( $module, 'new' );
my $chain = $module->new;
isa_ok( $chain, $module );

#----------------------------------------

# can we initialise an empty object ?
ok( my $empty_chain = $module->new, 'got an empty Chain' );

# can we initialise with a Chain ?
ok( my $another_chain = $empty_chain->new, 'got an Chain from a Chain' );

# can we initialise it with arguments ?
ok( $chain = $module->new( { chainID => 'A' } ), 'constructor with parameters' );

# can we initialise it from an ATOM record ?
my $record = "ATOM    145  N   VAL A  25I     32.433  16.336  57.540  1.00 11.92           N  ";
ok( $chain = $module->new( $record ), 'constructor with ATOM record' );
my $expected_contents = {
  chainID => 'A',
};
my $residue_contents;
foreach my $key ( keys %$expected_contents ) {
  $residue_contents->{$key} = $chain->$key;
}
is_deeply( $residue_contents, $expected_contents,
           'check atom contents' );
           
# check the catch for an invalid record
warnings_like( sub { $chain = $module->new( 'HEADER    HYDROLASE' ) },
               qr/not an accepted/,
               'catch bad record type' );

#----------------------------------------

$chain = $module->new( $record );

# check auto-generated getters/setters
ok( $chain->internal_chain_id( 'MYCHAIN' ), 'set valid internal chain ID' );
is( $chain->internal_chain_id, 'MYCHAIN', 'get valid internal chain ID' );

# hand-crafted getters/setters
ok( $chain->chainID( 'X' ), 'set valid chain ID' );
is( $chain->chainID, 'X', 'get valid chain ID' );

is( $chain->type, undef, q(try to get type before it's set) ); # before we set it...
ok( $chain->type( 'protein' ), 'set valid type' ); # set it
is( $chain->type, 'protein', 'get valid type' );   # after we set it...

warnings_like( sub { $chain->type( 'foo' ) },
               qr/not an allowed chain type/,
               'try invalid chain type' );

#----------------------------------------

# try add_monomer with undef
warnings_like( sub { $chain->add_monomer( undef ) },
               qr/not a Monomer object/,
               'catch add_monomer with undef' );

# try to delete a residue with undef
warnings_like( sub { $chain->delete_monomer( undef ) },
               qr/not a Monomer object/,
               'catch delete_residue with undef' );

# try add_monomer with something other than a Residue
warnings_like( sub { $chain->add_monomer( CGI->new ) },
               qr/not a Monomer object/,
               'catch add_atom with non-Monomer object' );

# try delete_monomer with something other than a Residue
warnings_like( sub { $chain->delete_monomer( CGI->new ) },
               qr/not a Monomer object/,
               'catch delete_monomer with non-Monomer object' );

#----------------------------------------

# these tests are all dependent on us actually being able to instantiate 
# one or more Atom objects and one or more Residues
my $N = Bio::iPfam::Structure::Atom->new( $record );
die q(couldn't instantiate an Atom) unless defined $N;

# build a chain with a single residue and a single atom
my $residue = Bio::iPfam::Structure::Residue->new( $record );
die q(couldn't instantiate a Residue) unless defined $residue;
$residue->add_atom( $N );

# make sure we can add a residue to the chain
ok( $chain->add_monomer( $residue ), 'add a Residue' );

# write chain contents to STDOUT
stdout_like( sub { $chain->write }, qr/^$record/,
             'write chain to STDOUT' );

# write chain contents to filehandle
stderr_like( sub { $chain->write( \*STDERR ) }, qr/^$record/,
             'write chain to filehandle' );

# check that we can cascade the chain ID down to the atoms
$chain->chainID( 'Z' );
is( $chain->chainID, 'Z', 'chain has correct chain ID' );
is( $residue->chainID, 'Z', 'residue has correct chain ID' );
is( $N->chainID, 'Z', 'atom has correct chain ID' );

# put the chain ID back...
$chain->chainID( 'A' );

#----------------------------------------

# build a residue with several atoms
my $CA = Bio::iPfam::Structure::Atom->new( $record );
my $C = Bio::iPfam::Structure::Atom->new( $record );
my $O = Bio::iPfam::Structure::Atom->new( $record );
$CA->name( ' CA ' );
$C->name( ' C  ' );
$O->name( ' O  ' );

$residue->add_atom( $_ ) for ( $CA, $C, $O );

# make sure the atom count is correct too
is( $chain->atom_count, 4, 'atom count correct' );

#----------------------------------------

# get another Residue, so that we have two in the chain. Needed to exercise
# a false condition in $r->delete_chain. We can use the same atoms to build
# the new residue though
my $residue2 = Bio::iPfam::Structure::Residue->new( $record );
$residue->resSeq( $residue->resSeq + 1 );
$residue2->add_atom( $_ ) for ( $N, $CA, $C, $O );

ok( $chain->add_monomer( $residue2 ), 'add a second Residue' );

# make sure it was counted
is( $chain->monomer_count, 2, 'residue count correct' );

# actually delete the second residue
ok( $chain->delete_monomer( $residue2 ), 'delete a residue' );
  
# get one more new residue, this time to try deleting with a Residue that's 
# not part of the Chain
my $residue3 = Bio::iPfam::Structure::Residue->new;
$residue3->add_atom( $_ ) for ( $N, $CA, $C, $O );
is( $chain->delete_monomer( $residue2 ), undef, 'delete residue not in chain' );
