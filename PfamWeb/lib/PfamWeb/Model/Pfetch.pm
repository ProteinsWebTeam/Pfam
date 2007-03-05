
# Pfetch.pm
# jt6 20060731 WTSI
#
# $Id: Pfetch.pm,v 1.5 2007-03-05 13:23:39 jt6 Exp $

=head1 NAME

PfamWeb::Model::Pfetch - retrieves data from the WTSI pfetch server

=cut

package PfamWeb::Model::Pfetch;

=head1 DESCRIPTION

A simple wrapper around a call to the Sanger pfetch server.

=head1 SYNOPSIS

  $c->model( "Pfetch" )->retrieve( "--pdb" => "1abc" );

=cut

use strict;
use warnings;

use IO::Socket;
use Sys::Hostname;

# nothing else to base it on really...
use base "Catalyst::Model";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 retrieve

Retrieves the specified data from the pfetch server. The arguments should
encapsulate a valid pfetch command, otherwise it will all go horribly
wrong.

=cut

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

  	die "problem retrieving data from server ($@)"
  	  unless $@ eq "alarm\n";   # propagate unexpected errors
  
  	# timed out
  }

  return $result;
}

#-------------------------------------------------------------------------------

=head2 getData

Actually retrieves data from the server. The first obstacle is getting a
socket connection to the server; we fetch server details from the 
class configuration in the catalyst config files and then try to connect.
If the connection fails, we're done. If we get a connection, the results 
are stuffed into an array and the reference is returned.

=cut

sub getData {
  my( $this, $commands ) = @_;

  # retrieve the Pfetch server connection parameters from the config
	my( $server, $port, $timeout );
	( $server  ) = $this->{pfetchServerIP}      =~ /^((\d{1,3}\.){3}\d{1,3})$/;
	( $port    ) = $this->{pfetchServerPort}    =~ /^(\d+)$/;
	( $timeout ) = $this->{pfetchServerTimeout} =~ /^(\d+)$/;

  # open the socket connection to the server
  my $s = IO::Socket::INET->new( PeerAddr => $server,
                								 PeerPort => $port,
                								 Timeout  => $timeout,
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

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
