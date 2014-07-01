#!/usr/bin/env perl

# init script for the dequeuer that runs the Rfam interactive searches
# jgt 20140417 EBI

use strict;
use warnings;

# this is to enable this init script to find Daemon::Control
use lib qw( /nfs/production/xfam/pfam/software/init/perl5/lib/perl5
            /nfs/production/xfam/pfam/software/init/perl5/lib/perl5/x86_64-linux-thread-multi );

use Daemon::Control;

my $INIT_DIR = '/nfs/production/xfam/pfam/software/init';

my @PERL5LIB = (
  "$INIT_DIR/perl5/lib/perl5",
  "$INIT_DIR/perl5/lib/perl5/x86_64-linux-thread-multi",
  "$INIT_DIR/PfamLib",
  "$INIT_DIR/PfamSchemata"
);

# and this is setting up the environment that's passed onto the dequeuer itself
$ENV{PERL5LIB}       = join ':', @PERL5LIB;
$ENV{PFAM_CONFIG}    = '/nfs/production/xfam/pfam/software/Conf/pfam_svn.conf';

$ENV{DEQUEUER_DEBUG} = 1;
$ENV{BG_DEQUEUER}    = 0; # don't fork in the dequeuer; let Daemon::Control take care of that

my $options = {
  name         => 'rfam_queue',
  lsb_start    => '$syslog $remote_fs',
  lsb_stop     => '$syslog',
  lsb_sdesc    => 'Dequeuer for running Rfam interactive searches',
  lsb_desc     => 'A queue management daemon that runs the Rfam interactive search queue',

  program      => "$INIT_DIR/PfamScripts/search/ebi_dequeuer.pl",
  program_args => [ 'rfam' ],

  pid_file     => "$INIT_DIR/run/rfam_queue.pid",

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

