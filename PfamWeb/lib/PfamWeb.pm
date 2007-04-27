
# PfamWeb.pm
# jt 20060316 WTSI
#
# $Id: PfamWeb.pm,v 1.31 2007-04-27 16:17:31 jt6 Exp $

=head1 NAME

PfamWeb - application class for the Pfam website

=cut

package PfamWeb;

=head1 DESCRIPTION

This is the main class for the Pfam website catalyst application. It
handles configuration of the application classes and error reporting
for the whole application.

$Id: PfamWeb.pm,v 1.31 2007-04-27 16:17:31 jt6 Exp $

=cut

use strict;
use warnings;

# set flags and add plugins for the application
use Catalyst qw/
				PfamConfigLoader
				Prototype
				HTML::Widget
				Email
				Session
				Session::Store::FastMmap
				Session::State::Cookie
				Cache::FileCache
				/;

#				-Debug
#				Compress::Deflate
#				PageCache

# some other plugins that could be used...

# a cache backend. This one won't work when we're using multiple servers
#				Cache::FastMmap

# user authentication. Not used currently, largely because it doesn't really work
#				Authentication
#				Authentication::Store::DBIC
#				Authentication::Credential::Password

our $VERSION = '0.01';

#-------------------------------------------------------------------------------

=head1 CONFIGURATION

Configuration is done through a series of external "Apache-style"
configuration files.

=cut

# grab the location of the configuration file from the environment and
# detaint it. Doing this means we can configure the location of the
# config file in httpd.conf rather than in the code
my( $conf ) = $ENV{PFAMWEB_CONFIG} =~ /([\d\w\/-]+)/;

__PACKAGE__->config( file => $conf );
__PACKAGE__->setup;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 reportError : Private

Records site errors in the database. Because we could be getting
failures constantly, e.g. from SIMAP web service, we want to avoid
just mailing admins about that, so instead we insert an error message
into a database table.

The table has four columns:

=over 8

=item message

the raw message from the caller

=item num

the number of times this precise message has been seen

=item first

the timestamp for the first occurrence of the message

=item last

the timestamp for that most recent occurence of the
message. Automatically updated on insert or update

=back

An external script or plain SQL query should then be able to retrieve
error logs when required and we can keep track of errors without being
deluged with mail.

=cut

sub reportError : Private {
  my( $this, $c ) = @_;

  my $el = $c->model( "WebUser::ErrorLog" );
  foreach my $e ( @{$c->error} ) {

  	$c->log->debug( "PfamWeb::reportError: reporting a site error: |$e|" );
 
    # see if we can access the table at all - basically, see if the DB is up 
    my $rs; 
	  eval {
    	$rs = $el->find( { message => $e } );
    };
	  if( $@ ) {
  		# really bad; an error while reporting an error...
  		$c->log->error( "PfamWeb::reportError: couldn't create a error log: $@" );
	  }
  
    # if we can get a ResultSet, try to add a message
  	if( $rs ) {

      # we've seen this error before; update the error count
  	  eval {
    	  $rs->update( { num => $rs->num + 1 } );
  	  };
  	  if( $@ ) {
    		# really bad; an error while reporting an error...
    		$c->log->error( "PfamWeb::reportError: couldn't create a error log: $@" );
  	  }

  	} else {

      # no log message like this has been registered so far; add the row 
  	  eval {
    		$el->create( { message => $e,
  										num     => 1,
  										first   => [ "CURRENT_TIMESTAMP" ] } );
  	  };
  	  if( $@ ) {
    		# really bad; an error while reporting an error...
    		$c->log->error( "PfamWeb::reportError: couldn't create a error log: $@" );
  	  }

  	}
  }

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
