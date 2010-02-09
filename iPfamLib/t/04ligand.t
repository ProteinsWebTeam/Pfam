
# 04ligand.t
# jt6 20080130 WTSI
#
# $Id: 04ligand.t,v 1.1 2008-01-30 15:41:11 jt6 Exp $

use strict;
use warnings;

use Test::More tests => 11;

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
get_logger( 'Bio::iPfam::Structure::Ligand'  )->level( $INFO );

my $module = 'Bio::iPfam::Structure::Ligand';

# does the module load ?
use_ok( $module ) or exit;

# is it what we think it is ?
can_ok( $module, 'new' );
my $l = $module->new;
isa_ok( $l, $module );

# check new setters and getters
ok( $l->hetID( 'ATP' ), 'set het ID' );
is( $l->hetID, 'ATP', 'get het ID' );
ok( $l->numHetAtoms( 10 ), 'set numHetAtoms' );
is( $l->numHetAtoms, 10, 'get numHetAtoms' );
ok( $l->description( 'het description' ), 'set description' );
is( $l->description, 'het description', 'get description' );
ok( $l->formula( 'het formula' ), 'set formula' );
is( $l->formula, 'het formula', 'get formula' );
