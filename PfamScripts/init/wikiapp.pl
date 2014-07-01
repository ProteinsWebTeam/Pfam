#!/usr/bin/env perl

use strict;
use warnings;

# this is to enable this init script to find Daemon::Control
use lib qw( /nfs/production/xfam/pfam/software/init/perl5/lib/perl5
            /nfs/production/xfam/pfam/software/init/perl5/lib/perl5/x86_64-linux-thread-multi );

use Daemon::Control;

my $INIT_DIR = '/nfs/production/xfam/pfam/software/init';
my $APP_DIR  = '/nfs/production/xfam/pfam/software/wiki_approvals_app';

# note that the WikiApp environment is using the modules and perl5 library
# that's set up in the wiki_approvals_app area, rather than the one set up for
# the other init scripts
my @PERL5LIB= (
  "$APP_DIR/perl5/lib/perl5",
  "$APP_DIR/perl5/lib/perl5/x86_64-linux-thread-multi",
  "$APP_DIR/PfamSchemata",
  "$APP_DIR/PfamLib",
);

# and this is setting up the environment that's passed onto the dequeuer itself
$ENV{PERL5LIB}       = join ':', @PERL5LIB;
$ENV{WIKIAPP_CONFIG} = "$APP_DIR/WikiApp/wiki_app.conf";
 
my $daemon = Daemon::Control->new(
  name         => 'wikiapp',
  lsb_start    => '$syslog $remote_fs',
  lsb_stop     => '$syslog',
  lsb_sdesc    => 'Wikipedia approvals application',
  lsb_desc     => 'A catalyst application to coordinate wikipedia update approvals.',

  program      => "$APP_DIR/WikiApp/script/wikiapp_server.pl",

  pid_file     => "$INIT_DIR/run/wikiapp.pid",
  stdout_file  => '/dev/null',
  stderr_file  => '/dev/null',

  fork         => 2,
  user         => 'xfm_adm',
  group        => 'xfm_pub',
);

$daemon->run; 

