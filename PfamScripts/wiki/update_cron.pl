#!/usr/bin/env perl

# This script updates the "wikipedia_revision" column in the
# "wiki_approve.wikipedia" table. It uses the wikipedia API to get the latest
# revision number for all of the articles in the table. If the retrieved
# version number is different from the one in the table, the row is updated.
#
# jt6 20100420 WTSI
#
# $Id$

use strict;
use warnings;

use Getopt::Long;
use Config::General;
use WikiApprove;
use Bio::Pfam::Wiki::Updater;
use DateTime;
use Log::Log4perl qw(get_logger :levels);

#-------------------------------------------------------------------------------
# set up logging
my $logger_conf = q(
  log4perl.logger                   = INFO, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger();
#-------------------------------------------------------------------------------

# handle the command-line options
my $config_file = 'conf/wiki.conf';
my $verbosity = 0;
GetOptions ( 'config=s' => \$config_file,
             'v+'       => \$verbosity );

# increase the amount of logging by one level for each "v" switch added
$log->more_logging(1) while $verbosity-- > 0;

# find the config file
$log->logdie( "ERROR: couldn't read config from '$config_file': $!" )
  unless -e $config_file;

#-------------------------------------------------------------------------------

# get the DB connection parameters
my $cg = Config::General->new($config_file);
my %config = $cg->getall;
$log->logdie( "ERROR: failed to extract and configuration from '$config_file'" )
  unless keys %$cg;
my $conf = $config{WikiApprove};

# connect to the wiki_approve database
my $dsn = "dbi:mysql:$conf->{db_name}:$conf->{db_host}:$conf->{db_port}";
my $wa = WikiApprove->connect( $dsn, $conf->{username}, $conf->{password} );

# get an Updater and update all entries
my $u = Bio::Pfam::Wiki::Updater->new( schema => $wa );
$u->update_all;

my $num_checked    = $u->num_checked;
my $num_updated    = $u->num_updated;
my $num_redirected = $u->num_redirected;
my $num_approved = $u->num_auto_approved;

if ( $num_redirected ) {
  foreach my $redirect ( @{ $u->redirected_articles } ) {
    my @mappings =  $redirect->{row}->article_mappings;
    if ( scalar @mappings ) {
      $log->warn(  q(") . $redirect->{from}
                    . q(" has been redirected to ")
                    . $redirect->{to} . q(")
                    . ', used by '
                    . ucfirst $_->db . ' '
                    . $_->accession . ' ') for @mappings;
    } else {
      $log->warn(  q(") . $redirect->{from}
                    . q(" has been redirected to ")
                    . $redirect->{to} . q(")
                    . " NOT USED" );
    }
  }
}

my $now = DateTime->now;
$log->info( "$now: updated last revision IDs for $num_updated "
              ."out of $num_checked checked articles. "
              ."Found $num_redirected redirected article(s) "
              ."Approved $num_approved article(s).");

# if given in the configuration, append a snippet of text to the output. This is
# to allow URLs, etc., to be added to the cron output
$log->info($config{update_email_message}) if $config{update_email_message};
