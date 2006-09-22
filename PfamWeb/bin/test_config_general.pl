#!/nfs/team71/pfam/jt6/server/perl/bin/perl

use strict;
use warnings;

use Config::General;

use Data::Dump qw(dump);

die "No such file" unless( $ARGV[0] and -f $ARGV[0] );

my $conf = new Config::General( $ARGV[0] );
my %config = $conf->getall;

dump( \%config );

