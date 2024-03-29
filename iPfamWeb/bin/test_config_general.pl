#!/Users/rdf/Work/Server/perl/bin/perl
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

use strict;
use warnings;

use Config::General;

use Data::Dump qw(dump);

die "No such file" unless( $ARGV[0] and -f $ARGV[0] );

my $conf;
eval {
  $conf = new Config::General( $ARGV[0] );
};
if( $@ ) {
  die "error: problem parsing configuration file:\n$@";
}

my %config;
eval {
  %config = $conf->getall;
};
if( $@ ) {
  die "error: problem retrieving configuration from file:\n$@";
}

dump( \%config );

