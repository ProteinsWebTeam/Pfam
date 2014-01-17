#!/usr/bin/env perl

use strict;
use warnings;

use Data::Printer;
use Log::Log4perl qw(get_logger :levels);
use File::Basename;

use Bio::Pfam::Config;
use Bio::Pfam::Queues::EbiQueueManager;

#-------------------------------------------------------------------------------
# set up logging

my $logger_conf = q(
  log4perl.logger                   = DEBUG, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
);

Log::Log4perl->init( \$logger_conf );

my $log = get_logger();

$log->level($DEBUG) if $ENV{DEQUEUER_DEBUG};

#-------------------------------------------------------------------------------
# which queue are we running here ?

my $config = Bio::Pfam::Config->new;

my $queue = shift;

$log->logdie( 'no queue name given' ) 
  unless defined $queue;

$log->logdie( 'not a valid queue name' ) 
  unless $queue =~ m/^\w+$/;

$log->info( "starting queue '$queue'" );

#-------------------------------------------------------------------------------

my $eqm = Bio::Pfam::Queues::EbiQueueManager->new( config => $config, queue => $queue );

$eqm->daemonise unless $ENV{DEQUEUER_DEBUG};
 
$eqm->start_polling;

exit;

