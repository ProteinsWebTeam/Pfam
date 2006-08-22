
# Pfetch.pm
# jt6 20060731 WTSI
#
# A model to retrieve data from the WTSI pfetch server.
#
# $Id: Pfetch.pm,v 1.2 2006-08-22 13:37:37 rdf Exp $

package PfamWeb::Model::Pfetch;

use strict;
use warnings;

use IO::Socket;
use Sys::Hostname;

my %serverConfig;

BEGIN {

  # use settings from the environment if possible
  $serverConfig{PFETCH_SERVER}  = $ENV{PFETCH_SERVER}  if defined $ENV{PFETCH_SERVER};
  $serverConfig{PFETCH_PORT}    = $ENV{PFETCH_PORT}    if defined $ENV{PFETCH_PORT};
  $serverConfig{PFETCH_TIMEOUT} = $ENV{PFETCH_TIMEOUT} if defined $ENV{PFETCH_TIMEOUT};

  # try getting them from the catalyst configuration, if available
  eval {
	$serverConfig{PFETCH_SERVER}  ||= PfamWeb->config->{pfetchServerIP};
	$serverConfig{PFETCH_PORT}    ||= PfamWeb->config->{pfetchServerPort};
	$serverConfig{PFETCH_TIMEOUT} ||= PfamWeb->config->{pfetchServerTimeout};
  };
  if( $@ ) {
	die "Pfetch::BEGIN: error when setting config: $!";
  }
}

# (do we even need a constructor when we're inside catalyst ? Don't
# think so)

sub new {
  my $invocant = shift;
  my $class = ref( $invocant ) || $invocant;
  my $this = {};
  bless $this, $class;
  return $this;
}

# "get me something"

sub retrieve {
  my( $this, $commands ) = @_;

  # make sure we got some sort of query
  return unless scalar keys( %$commands );

  my $result;

  # the bones of this bit are taken from perldoc -f alarm
  eval {

	# catch the alarm signal and die with a message that we can check
	# for outside of the eval
	local $SIG{ALRM} = sub { die "alarm\n" }; # NB: \n required

	# ask the kernel to send a SIGALRM to this process after 15 seconds
	alarm 15;

	# try retrieving the file from the server
	$result = $this->getData( $commands );

	# and turn off the alarm
	alarm 0;
  };

  # according to the camel book, there's a small chance of a race
  # condition here, if the alarm signal comes in after the file has
  # been retrieved but before we've turned off the alarm. Supposedly
  # this second "alarm 0" avoids the race condition
  alarm 0;

  # check for errors in the eval
  if ($@) {

	die "problem retrieving data from server"
	  unless $@ eq "alarm\n";   # propagate unexpected errors

	# timed out
  }

  return $result;
}

sub getData {
  my( $this, $commands ) = @_;

  # open the socket connection to the server
  my $s = IO::Socket::INET->new( PeerAddr => $serverConfig{PFETCH_SERVER},
								 PeerPort => $serverConfig{PFETCH_PORT},
								 Timeout  => $serverConfig{PFETCH_TIMEOUT},
								 Proto    => "tcp",
								 Type     => SOCK_STREAM,
							   );

  # oops
  return unless $s;

  $s->autoflush(1);

  # build the request string.
  my $request;
  while( my( $command, $param ) = each( %$commands ) ) {
	$request .= "$command $param ";
  }
  $request .= "--client " . hostname();

  # push the request down the pipe...
  print $s "$request\n";

  # and read the PDB file back from the pipe
  my @results;
  while( <$s> ) {
	push @results, $_;
  }
  close $s;

  return \@results;
}

1;
