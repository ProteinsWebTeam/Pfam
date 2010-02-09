
# 05chainset.t
# jt6 20080130 WTSI
#
# $Id: 05chainset.t,v 1.4 2008-02-01 17:36:32 jt6 Exp $

use strict;
use warnings;

use Test::More tests => 9;

use Test::Warn;
use Test::Output;

use Bio::iPfam::Structure::Atom;
use Bio::iPfam::Structure::Residue;
use Bio::iPfam::Structure::Chainset;

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

get_logger( 'Bio::iPfam::Structure::Atom'       )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Residue'    )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Ligand'     )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Chain'      )->level( $INFO );
get_logger( 'Bio::iPfam::Structure::Chainset'   )->level( $INFO );

my $module = 'Bio::iPfam::Structure::Chainset';

# does the module load ?
use_ok( $module ) or exit;

# is it what we think it is ?
can_ok( $module, 'new' );
my $f = $module->new;
isa_ok( $f, $module );

my $record = "ATOM    145  N   VAL A  25I     32.433  16.336  57.540  1.00 11.92           N  \n";
my $chainset;
ok( $chainset = $module->new( $record ), 'new chainset from ATOM record' );

#----------------------------------------

# these tests are all dependent on us actually being able to instantiate 
# one or more Atom objects and one or more Residues
my $N = Bio::iPfam::Structure::Atom->new( $record );
die q(couldn't instantiate an Atom) unless defined $N;

my $residue = Bio::iPfam::Structure::Residue->new( $record );
die q(couldn't instantiate a Residue) unless defined $residue;

# build a chain with a single residue and a single atom
$residue->add_atom( $N );
my $chain = Bio::iPfam::Structure::Chain->new( $record );
$chain->add_monomer( $residue );

ok( $chainset->add_chain( $chain ), 'add chain' );
isa_ok( $chainset->get_chain( 'A' ), 'Bio::iPfam::Structure::Chain',
        'get chain using chain ID' );

isa_ok( $chainset->get_monomer( 'A25I' ), 'Bio::iPfam::Structure::Residue',
        'get residue with string' );

isa_ok( $chainset->get_monomer( [ 'A', 25, 'I' ] ), 'Bio::iPfam::Structure::Residue',
        'get residue with array ref' );

isa_ok( $chainset->get_monomer( { chainID => 'A', resSeq => 25, iCode => 'I' } ), 
        'Bio::iPfam::Structure::Residue',
        'get residue with hash ref' );


