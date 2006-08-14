
# PdbFile.pm
# jt6 20060731 WTSI
#
# A model to return the specified PDB file. Currently this will
# retrieve the file from the WTSI pfetch server, but it could also
# read the file from local disk or by FTP from the PDB servers.
#
# $Id: PdbFile.pm,v 1.1 2006-08-14 10:34:02 jt6 Exp $

package PfamWeb::Schema::PfamDB::PdbFile;

use strict;
use warnings;

use IO::Socket;
use Sys::Hostname;

sub new {
  my $invocant = shift;
  my $class = ref( $invocant ) || $invocant;
  my $this = {};
  bless $this, $class;
  return $this;
}

sub retrieve {
  my( $this, $rawId ) = @_;

  my $id;
  ( $id ) = $rawId =~ /^(\d\w{3})$/;

  return unless $id;

  my $pdb;

  # the bones of this bit are taken from perldoc -f alarm
  eval {

	# catch the alarm signal and die with a message that we can check
	# for outside of the eval
	local $SIG{ALRM} = sub { die "alarm\n" }; # NB: \n required

	# ask the kernel to send a SIGALRM to this process after 30 seconds
	alarm 30;

	# try retrieving the file from the server
	$pdb = $this->getFile( $id );

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

	die "problem reading PDB file from server"
	  unless $@ eq "alarm\n";   # propagate unexpected errors

	# timed out
  }

  return $pdb;
}

sub getFile {
  my( $this, $id ) = @_;

  # open the socket connection to the server
  my $s = IO::Socket::INET->new( PeerAddr => PfamWeb->config->{pfetchServerIP},
								 PeerPort => PfamWeb->config->{pfetchServerPort},
								 Timeout  => PfamWeb->config->{pfetchTimeout},
								 Proto    => "tcp",
								 Type     => SOCK_STREAM,
							   );
  # oops
  return unless $s;

  $s->autoflush(1) if $s;

  # push the request down the pipe...
  my $request = "--pdb $id --client " . hostname();
  print $s "$request\n";

  # and read the PDB file back from the pipe
  my @pdb;
  while( <$s> ) {
	push @pdb, $_;
  }
  close $s;

  return \@pdb;
}

1;
