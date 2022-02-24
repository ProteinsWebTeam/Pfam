#!/usr/bin/env perl

# init script for the dequeuer that runs pfam interactive sequence searches against the Pfam
# maq

use strict;
use warnings;

# this is to enable this init script to find Daemon::Control
use lib qw( /srv/data/perllib/lib/perl5/
            /srv/data/perllib/lib/perl5/x86_64-linux-thread-multi );

use Daemon::Control;

my $INIT_DIR = '/srv/data/pfam-prod';
my $PF_HOME = '/srv/data/pfam-prod/pfam';

my @PERL5LIB = (
  "/srv/data/perllib/lib/perl5/",
  "/srv/data/perllib/lib/perl5/x86_64-linux-thread-multi",
  "$PF_HOME/PfamLib",
  "$PF_HOME/PfamSchemata"
);

# and this is setting up the environment that's passed onto the dequeuer itself
$ENV{PERL5LIB}       = join ':', @PERL5LIB;
$ENV{PATH}          .= ":$INIT_DIR/exe/bin:$PF_HOME/PfamBackend/scripts:$PF_HOME/PfamScripts/search";
$ENV{PFAM_CONFIG}    = "$INIT_DIR/conf/pfam_svn.conf";

$ENV{DEBUG_DEQUEUER} = 1;
$ENV{BG_DEQUEUER}    = 0; # don't fork in the dequeuer; let Daemon::Control take care of that

my $options = {
  name         => 'pfam_interactive_queue',
  lsb_start    => '$syslog $remote_fs',
  lsb_stop     => '$syslog',
  lsb_sdesc    => 'Dequeuer for running Pfam interactive sequence searches',
  lsb_desc     => 'A queue management daemon that runs jobs that run interative sequence searches against pfam',

  program      => "$PF_HOME/PfamScripts/search/hmmer_dequeuer.pl",
  program_args => [ 'pfama' ],

  pid_file     => "$INIT_DIR/monit-files/pfam_interactive_queue.pid",

  fork         => 2,
  user         => 'xfm_adm',
  group        => 'xfm_pub',

  stdout_file  => ['>>', '/tmp/pfam_interactive_out.log'],
  stderr_file  => ['>>', '/tmp/pfam_interactive_error.log']
};

if ( not $ENV{DEBUG_DEQUEUER} ) {
  $options->{stdout_file} = '/dev/null';
  $options->{stderr_file} = '/dev/null';
}

my $daemon = Daemon::Control->new( %$options );
$daemon->run; 

