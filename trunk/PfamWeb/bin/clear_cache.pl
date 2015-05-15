#!/nfs/team71/pfam/jt6/server/perl/bin/perl
#
# # Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;

use Getopt::Std;

use Cache::Memcached;

my @development = qw( 
  ves-ebi-b5.ebi.ac.uk:11211
  ves-ebi-b7.ebi.ac.uk:11211
  ves-ebi-b8.ebi.ac.uk:11211
);
my @production = qw(
  ves-hx-b5.ebi.ac.uk:11211
  ves-hx-b7.ebi.ac.uk:11211
  ves-hx-b8.ebi.ac.uk:11211
  ves-oy-b5.ebi.ac.uk:11211
  ves-oy-b6.ebi.ac.uk:11211
  ves-oy-b7.ebi.ac.uk:11211
  ves-oy-b8.ebi.ac.uk:11211
  ves-oy-b9.ebi.ac.uk:11211
  ves-pg-b5.ebi.ac.uk:11211
  ves-pg-b6.ebi.ac.uk:11211
  ves-pg-b7.ebi.ac.uk:11211
  ves-pg-b8.ebi.ac.uk:11211
  ves-pg-b9.ebi.ac.uk:11211
);

my %options;
getopt( 'adpqh', \%options ) or usage();

my @servers;

if( $options{h} ) {
  usage();
} elsif( exists $options{a} ) {
  print STDERR "clearing all caches:\n" unless exists $options{q};
  @servers = ( @development, @production );
} elsif( exists $options{p} ) {
  print STDERR "clearing production cache:\n" unless exists $options{q};
  @servers = @production;
} else {
  print STDERR "clearing development cache:\n" unless exists $options{q};
  @servers = @development;
}

unless( exists $options{q} ) {
  foreach ( @servers ) {
    print "$_\n";
  }
}

my $cache = new Cache::Memcached(
                                  { servers => \@servers }
                                );
$cache->flush_all;

exit;

#-------------------------------------------------------------------------------

sub usage {
  print <<EOF_help;
usage: $0 [-adph]

 -a : clear all caches
 -d : clear development cache only
 -p : clear production cache only
 -h : prints this message
 
EOF_help

  exit;
}
 
