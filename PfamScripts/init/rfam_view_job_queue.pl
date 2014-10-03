#!/usr/bin/env perl

# init script for the dequeuer that runs the Rfam view process jobs
# jgt 20141003 EBI

use strict;
use warnings;

# this is to enable this init script to find Daemon::Control
use lib qw( /nfs/production/xfam/pfam/software/init/perl5/lib/perl5 );

use Daemon::Control;

my $PFAM_INIT_DIR = '/nfs/production/xfam/pfam/software/init';
my $RFAM_INIT_DIR = '/nfs/production/xfam/rfam/production_software/rfam_production';

my @PERL5LIB = (
  "$PFAM_INIT_DIR/perl5/lib/perl5",
  "/nfs/production/xfam/rfam/production_software/perl5/lib/perl5",
  "$RFAM_INIT_DIR/Bio-Easel/blib/arch",
  "$RFAM_INIT_DIR/Bio-Easel/blib/lib",
  "$RFAM_INIT_DIR/Rfam/Lib",
  "$RFAM_INIT_DIR/Rfam/Schemata",
  "$PFAM_INIT_DIR/PfamLib",
  "$PFAM_INIT_DIR/PfamSchemata"
);

# and this is setting up the environment that's passed onto the dequeuer itself
$ENV{PERL5LIB}       = join ':', @PERL5LIB;
$ENV{PFAM_CONFIG}    = '/nfs/production/xfam/pfam/software/Conf/pfam_svn.conf';
$ENV{RFAM_CONFIG}    = '/nfs/production/xfam/rfam/production_software/rfam_production/Rfam/Conf/rfam.conf';

my $options = {
  name         => 'rfam_view_job_queue',
  lsb_start    => '$syslog $remote_fs',
  lsb_stop     => '$syslog',
  lsb_sdesc    => 'Dequeuer for running Rfam view process jobs',
  lsb_desc     => 'A queue management daemon that runs the Rfam view process queue',

  program      => "$RFAM_INIT_DIR/Rfam/Scripts/view/job_dequeuer.pl",
  program_args => [ qw( -j family -view family ) ],

  pid_file     => "$PFAM_INIT_DIR/run/rfam_view_job_queue.pid",

  fork         => 2,
  user         => 'xfm_adm',
  group        => 'xfm_pub',
};

if ( not $ENV{DEBUG_DEQUEUER} ) {
  $options->{stdout_file} = '/dev/null';
  $options->{stderr_file} = '/dev/null';
}

my $daemon = Daemon::Control->new( %$options );
$daemon->run; 

