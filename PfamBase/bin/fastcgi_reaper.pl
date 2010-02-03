#!/usr/bin/perl
#
# fastcgi_reader.pl
# jt6 20100111 WTSI
#
# A simple script for killing processes whose virtual memory size exceeds
# the given limit. This is intended to be run as a cron job that will 
# reap Catalyst FastCGI processes that might be growing too large due to
# "copy-on-write".
#
# Example: every thirty minutes, kill of all pfamweb FastCGI processes
#          that have a virtual memory size of greater than 100Mb
#
# */30 * * * * /path/to/fastcgi_reaper `cat /var/run/pfamweb/pfamweb.pid` 104857600
#
# Copied almost verbatim from http://www.catalystframework.org/calendar/2007/18
#
# $Id: fastcgi_reaper.pl,v 1.1 2010-01-13 14:36:46 jt6 Exp $

my ( $ppid, $max_bytes ) = @ARGV;

die "Usage: $0 PPID ram_limit_in_bytes\n"
  unless ( $ppid and $max_bytes );

my $process_list;

exit unless open( $process_list, "/bin/ps -o pid=,vsz= --ppid $ppid|" );

my @large_processes;

while ( <$process_list> ) {
  chomp;
  my ( $pid, $virtual_memory_size ) = split;

  $virtual_memory_size *= 1024; # ps shows Kb but we want bytes.

  if ( $virtual_memory_size >= $max_bytes ) {
     push @large_processes, $pid;
  }
}

close $process_list;

if ( @large_processes ) {
  # kill them slowly, so that all connection serving
  # children don't suddenly die at once.
  foreach my $fastcgi_process ( @large_processes ) {
    kill 'HUP', $fastcgi_process;
    sleep 10;
  }
}

