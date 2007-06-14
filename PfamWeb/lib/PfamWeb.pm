
# PfamWeb.pm
# jt 20060316 WTSI
#
# $Id: PfamWeb.pm,v 1.35 2007-06-14 21:31:19 jt6 Exp $

=head1 NAME

PfamWeb - application class for the Pfam website

=cut

package PfamWeb;

=head1 DESCRIPTION

This is the main class for the Pfam website catalyst application. It
handles configuration of the application classes and error reporting
for the whole application.

$Id: PfamWeb.pm,v 1.35 2007-06-14 21:31:19 jt6 Exp $

=cut

use strict;
use warnings;

# a useful trick to get Catalyst to confess errors on startup, rather than
# simply dying with a cryptic error about barewords
#use Carp; $SIG{__DIE__} = \&Carp::confess;

# set flags and add plugins for the application
use Catalyst qw/
                 ConfigLoader
                 Prototype
                 HTML::Widget
                 Email
                 Cache::Memcached
                 Session
                 Session::Store::FastMmap
                 Session::State::Cookie
               /;
               
#        -Debug
#        Compress::Deflate
#        PageCache

# some other plugins that could be used...

# a cache backend. This one won't work when we're using multiple servers
#        Cache::FastMmap

# sessions via cookies
#                 Session
#                 Session::Store::FastMmap
#                 Session::State::Cookie


# user authentication. Not used currently, largely because it doesn't really work
#        Authentication
#        Authentication::Store::DBIC
#        Authentication::Credential::Password

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

=head2 setup_plugins

Overrides the default C<setup_plugins> to add the PageCache Catalyst plugin 
iff we're running in debug mode.

=cut

#sub setup_plugins {
#  my( $c, $plugins ) = @_;
#  push @$plugins, 'PageCache' if not $c->debug;
#  $c->NEXT::setup_plugins($plugins);
#} 

#-------------------------------------------------------------------------------

=head2 finalize_config

Overrides the empty C<finalize_config> method from the ConfigLoader plugin, 
turning it into a dumb by-pass of the perl taint checking on configuration
parameters.

=cut

sub finalize_config {
  my $c = shift;
  my $v = Data::Visitor::Callback
            ->new( plain_value => sub {
               return unless defined $_;
               /^(.*)$/;
               $_ = $1;
            }
          );
  $v->visit( $c->config );
}

#-------------------------------------------------------------------------------

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

Note that this method does NOT clear_errors. It's up to the caller to decide
whether that's required or not.

=cut

sub reportError : Private {
  my( $this, $c ) = @_;

  my $el = $c->model( "WebUser::ErrorLog" );
  foreach my $e ( @{$c->error} ) {

    $c->log->error( "PfamWeb::reportError: reporting a site error: |$e|" );
 
    # see if we can access the table at all - basically, see if the DB is up 
    my $rs; 
    eval {
      $rs = $el->find( { message => $e } );
    };
    if( $@ ) {
      # really bad; an error while reporting an error...
      $c->log->error( "PfamWeb::reportError: couldn't create a error log; " .
                      "couldn't read error table: $@" );
    }
  
    # if we can get a ResultSet, try to add a message
    if( $rs ) {

      # we've seen this error before; update the error count
      eval {
        $rs->update( { num => $rs->num + 1 } );
      };
      if( $@ ) {
        # really bad; an error while reporting an error...
        $c->log->error( "PfamWeb::reportError: couldn't create a error log; " .
                        "couldn't increment error count: $@" );
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
        $c->log->error( "PfamWeb::reportError: couldn't create a error log; " .
                        "couldn't create a new error record : $@" );
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
