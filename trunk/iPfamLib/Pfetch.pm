# Pfetch.pm
# jt6 20060731 WTSI
#
# A model to retrieve data from the WTSI pfetch server.
#
# $Id: Pfetch.pm,v 1.3 2009-05-18 15:24:37 jt6 Exp $

package Pfetch;

use strict;
use warnings;
use Log::Log4perl qw(:easy);
Log::Log4perl->easy_init($ERROR);
my $logger = get_logger(__PACKAGE__);
use IO::Socket;
use Sys::Hostname;

my %serverConfig;

BEGIN {
  # use settings from the environment if possible
  $serverConfig{PFETCH_SERVER}  = $ENV{PFETCH_SERVER}   ||= '172.18.62.3';
  $serverConfig{PFETCH_PORT}    = $ENV{PFETCH_PORT}     ||= 22400;
  $serverConfig{PFETCH_TIMEOUT} = $ENV{PFETCH_TIMEOUT}  ||= 30;
}


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
  $logger->debug("Going to get pdb, based on:".$serverConfig{PFETCH_TIMEOUT}." time out,".
                  $serverConfig{PFETCH_SERVER}." server,".$serverConfig{PFETCH_PORT}." port");
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
  $logger->debug("Going to get data");
  # open the socket connection to the server
  my $s = IO::Socket::INET->new( PeerAddr => $serverConfig{PFETCH_SERVER},
                                                                 PeerPort => $serverConfig{PFETCH_PORT},
                                                                 Timeout  => $serverConfig{PFETCH_TIMEOUT},
                                                                 Proto    => "tcp",
                                                                 Type     => SOCK_STREAM,
                                                           );

  # oops
  return unless $s;
  $logger->debug("Got socket connect");
  $s->autoflush(1);

  # build the request string.
  my $request;
  while( my( $command, $param ) = each( %$commands ) ) {
        $request .= "$command $param ";
  }
  $request .= "--client " . hostname();
  $logger->debug("Going to run the following request:|$request|");
  # push the request down the pipe...
  print $s "$request\n";

  # and read the PDB file back from the pipe
  my @results;
  #while( <$s> ) {
  #      push @results, $_;
  #}
  #close $s;
  
  #return \@results;
  return $s;
}

1;