#!/nfs/team71/pfam/jt6/server/perl/bin/perl
#
# dump a list of error messages for the last hour
# jt6 20060918 WTSI
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

use lib qw( /nfs/team71/pfam/jt6/server/PfamWeb/lib 
			/nfs/team71/pfam/jt6/server/PfamSchemata );

use DateTime;
use WebUser;

my $dsn  = "dbi:mysql:web_user:pfam:3306";
my $user = "web_user";
my $pass = "web_user";
my %dbiParams = ();

my $schema = WebUser->connect( $dsn, $user, $pass, \%dbiParams);

my $rs = $schema->resultset( "ErrorLog" );

my $check = DateTime->now->subtract( DateTime::Duration->new( hours => 1 ) );
print "checking for errors since $check...\n";

my $count = 0;
foreach my $e ( $rs->all ) {
  my( $y, $m, $d, $h, $min, $s ) = $e->last =~ /^(\d{4})\-(\d{2})\-(\d{2})\s(\d{2})\:(\d{2})\:(\d{2})$/;

  my $dt = DateTime->new( year      => $y,
						  month     => $m,
						  day       => $d,
						  hour      => $h,
						  minute    => $min,
						  second    => $s);

  print $e->message . " (" . $e->last . ") x " . $e->num . "\n" and $count++
	if DateTime->compare_ignore_floating( $dt, $check ) > 0;

}

print "found $count error" . ( $count == 1 ? "\n" : "s\n" );

