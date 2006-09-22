#!/nfs/team71/pfam/jt6/server/perl/bin/perl

# dump a list of error messages for the last hour
# jt6 20060918 WTSI

use strict;
use warnings;

use lib qw( /nfs/team71/pfam/jt6/server/PfamWeb/lib );

use DateTime;

use PfamWeb::Schema::WebUser;

my $dsn  = "dbi:mysql:web_user:pfam:3306";
my $user = "web_user";
my $pass = "web_user";
my %dbiParams = ();

my $schema = PfamWeb::Schema::WebUser->connect( $dsn, $user, $pass, \%dbiParams);

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

