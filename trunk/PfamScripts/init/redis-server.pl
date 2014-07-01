#!/usr/bin/env perl

use strict;
use warnings;

use lib qw( /nfs/production/xfam/pfam/software/init/perl5/lib/perl5
            /nfs/production/xfam/pfam/software/init/perl5/lib/perl5/x86_64-linux-thread-multi );

use Daemon::Control;

my $REDIS_CONFIG = '/nfs/production/xfam/pfam/software/Conf/PfamConfig/PfamRepos/pfam.redis.conf';
 
my $daemon = Daemon::Control->new(
  name         => 'redis-server',
  lsb_start    => '$syslog $remote_fs',
  lsb_stop     => '$syslog',
  lsb_sdesc    => 'Pfam redis server',
  lsb_desc     => 'A redis server used as part of the Pfam curation pipeline.',

  program      => '/nfs/production/xfam/pfam/software/bin/redis-server',
  program_args => [ $REDIS_CONFIG ],

  pid_file     => '/nfs/production/xfam/pfam/software/init/run/redis-server.pid',
  stdout_file  => '/dev/null',
  stderr_file  => '/dev/null',

  fork         => 2,
  user         => 'xfm_adm',
  group        => 'xfm_pub',
);

$daemon->run; 

