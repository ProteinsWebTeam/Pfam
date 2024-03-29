#!/software/bin/perl

# a jiffy script, intended to be run as a cron job, which checks the web_user
# database and flags a problem if the number of pending jobs in the job_history
# table exceeds some value. This script doesn't send a mail, but only generates
# output if the limit is exceeded, relying on cron to send a mail to the user.
#
# jt6 20100831 WTSI
#
# $Id$

use strict;
use warnings;

use Getopt::Long;
use Config::General;
use DateTime;
use DBI;

# find the config file
my $config_file = 'conf/wiki.conf';
GetOptions( 'config=s' => \$config_file );
die "ERROR: couldn't read config from '$config_file': $!"
  unless -e $config_file;

# get the DB connection parameters
my $cg = Config::General->new($config_file);
my %config = $cg->getall;
die "ERROR: failed to extract and configuration from '$config_file'"
  unless keys %$cg;
my $conf = $config{Model}{WebUser};

my $DB_DSN  = "dbi:mysql:database=$conf->{database};host=$conf->{host};port=$conf->{port}";
my $DB_USER = $conf->{user};
my $DB_PASS = $conf->{password};
my $MAX_PENDING = 50;

# connect to the web_user database
my $dbh = DBI->connect( $DB_DSN,
                        $DB_USER,
                        $DB_PASS,
                        { RaiseError => 1,
                          PrintError => 0 } )
  or die "ERROR: couldn't connect to database: $DBI::errstr\n";

# get the date string for yesterday midnight
my $dt = DateTime->now
                 ->subtract( days => 1 );
my $date = $dt->ymd . ' 00:00:00';

# set up the query
my $sth = $dbh->prepare( 'SELECT COUNT(*) FROM job_history WHERE status = "PEND" AND opened > ?' );
$sth->bind_param( 1, $date );
$sth->execute;

my $row = $sth->fetchall_arrayref;
my $num_rows = $row->[0]->[0];

# only generate output if the number of pending jobs is large enough
print "\nWARNING: there are currently $num_rows pending rows in the web_user database\n\n"
  if $num_rows > $MAX_PENDING;

