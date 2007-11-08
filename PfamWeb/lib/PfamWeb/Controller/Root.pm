
# Root.pm
# jt 20061003 WTSI
#
# $Id: Root.pm,v 1.22 2007-11-08 16:59:27 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Root - main class for the PfamWeb application

=cut

package PfamWeb::Controller::Root;

=head1 DESCRIPTION

This is the root class for the Pfam website catalyst application. It
installs global actions for the main site index page and other top-level
functions.

$Id: Root.pm,v 1.22 2007-11-08 16:59:27 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Controller';

# sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
__PACKAGE__->config->{namespace} = '';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Checks the request parameters for a "tab" parameter and sets the
appropriate tab name in the stash. That will be picked up by the
tab_layout.tt view, which will use it to figure out which tab to show
by default.

Also adds the Pfam version data to the stash, so that it's accessible 
throughout the site.

Checks for the existence of various files that act as switches, enabling
different behaviours...
=cut

sub auto : Private {
  my( $this, $c ) = @_;

  # pick a tab
  my $tab;
  ($tab) = $c->req->param('tab') =~ /^(\w+)$/
    if defined $c->req->param('tab');

  $c->stash->{showTab} = $1 if defined $tab;

  # see if we can get a DB ResultSet, which is effectively a test of whether we
  # can connect to the DB. If we can't, thrown an error and let the end action
  # clean up
  my $releaseData;
  eval {
    # stash some details of the Pfam release
    $releaseData = $c->model( 'PfamDB::Version' )
                     ->find( {} );
  };
  if( $@ ) {
    $c->error( 'There appears to be a problem with the Pfam database.' );
    $c->forward('/reportError');
  }
  
  $c->stash->{relData} = $releaseData if $releaseData;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the main site index page.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # set the page to be cached for one week
  $c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'home';

  $c->log->debug('PfamWeb::index: generating site index');
}

#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

fortnight = 1209600

=cut

sub new_features : Local {
  my( $this, $c ) = @_;

  # get the cookie that stores the state and retrieve the data as a hash with
  # key/value pairs storing feature timestamps and last-seen dates 
  my $cookie = $c->req->cookie( 'features' );
  my %last_seen = ();
  %last_seen = split /,/, $cookie->value if defined $cookie;
  
  # the available changelog entries
  my $changelog = $c->config->{changelog}->{entries};
  my @entries = sort keys %$changelog;
  
  # work out the range of entries that we should consider
  my $first = scalar @entries - $c->config->{changelog}->{show_last};
  $first = 0 if $first < 0;
  my $last  = scalar @entries - 1;

  my @features;
  my $body = '<ul>';
  foreach my $feature_time ( reverse @entries[$first..$last] ) {
    $c->log->debug( "Root::new_features: checking changelog entry: |$feature_time|..." )
      if $c->debug;

    # find out if and when the user last saw this message
    my $saw_feature_at = $last_seen{$feature_time} || 0;

    # flag to say that we've decided to show this entry
    my $show = 0;

    # if it's less than, say, two weeks since we saw this entry, or if the
    # feature is previously unseen, show it now
    if( time - $saw_feature_at < $c->config->{changelog}->{show_for} or
        $feature_time > $saw_feature_at ) {
      $c->log->debug( 'Root::new_features: showing this feature' ) if $c->debug;
      $body .= '<li>' . $changelog->{$feature_time} . '</li>';
    }
      
    # set a cookie saying "we saw feature X at time Y"
    push @features, $feature_time, 
                    $saw_feature_at > 0 ? $saw_feature_at : time;

  }
  $body .= '</ul>';

  $c->res->body( $body );

  # turn the features "hash" into a cookie value
  my $value = join ',', @features;
  $c->res->cookies->{features} = { value   => $value,
                                   expires => '+3M' };
}

#-------------------------------------------------------------------------------

=head2 default : Private

Generates a '404' page.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  # first, figure out where the broken link was, internal or external
  my $ref = ( defined $c->req->referer ) ? $c->req->referer : '';
  $c->stash->{where} = ( $ref =~ /sanger/ ) ? 'internal' : 'external';

  # record the error
  $c->error( 'Found a broken '
             . $c->stash->{where}
             . q( link: ')
             . $c->req->uri
             . q(', referer: )
             . ( $ref eq '' ? 'unknown' : qq('$ref') ) );

  # report it...
  $c->forward('/reportError');

  # ...and clear the errors before we render the page
  $c->clear_errors;

  # set the HTTP status and point at the 404 page
  $c->res->status(404);
  $c->stash->{template} = 'pages/404.tt';
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

  my $el = $c->model( 'WebUser::ErrorLog' );
  foreach my $e ( @{$c->error} ) {

    $c->log->error( "PfamWeb::reportError: reporting a site error: |$e|" );
    # see if we can access the table at all - basically, see if the DB is up 
    my $rs; 
    eval {
      $rs = $el->find( { message => $e } );
    };
    if( $@ ) {
      # really bad; an error while reporting an error...
      $c->log->error( "PfamWeb::reportError: couldn't create a error log; "
                      . "couldn't read error table: $@" );
    }
  
    # if we can get a ResultSet, try to add a message
    if( $rs ) {

      # we've seen this error before; update the error count
      eval {
        $rs->update( { num => $rs->num + 1 } );
      };
      if( $@ ) {
        # really bad; an error while reporting an error...
        $c->log->error( "PfamWeb::reportError: couldn't create a error log; "
                        . "couldn't increment error count: $@" );
      }

    } else {

      # no log message like this has been registered so far; add the row 
      eval {
        $el->create( { message => $e,
                       num     => 1,
                       first   => [ 'CURRENT_TIMESTAMP' ] } );
      };
      if( $@ ) {
        # really bad; an error while reporting an error...
        $c->log->error( "PfamWeb::reportError: couldn't create a error log; "
                        . "couldn't create a new error record : $@" );
      }

    }
  }

}

#-------------------------------------------------------------------------------

=head2 robots : Path

Serve a "robots.txt" file. We try to retrieve the file from the configuration
and fall back onto a restrictive generic version that just disallows all robots 
to all URLs.

=cut

sub robots : Path( '/robots.txt' ) {
  my( $this, $c ) = @_;
  
  # try to get the file from config
  my $r = $c->config->{robots};
  
  # fall back on a generic, restrictive version
  $r ||= <<'EOF_default_robots';
User-agent: *
Disallow: /
EOF_default_robots

  # put the file into the response and we're done
  $c->res->content_type( 'text/plain' );
  $c->res->body( $r );
}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the index page for the site by default, but the default template can be 
overridden by setting it in an action (eg for a 404 template). Patterned on
the "end" method from the DefaultEnd plugin.

=cut

sub end : Private {
  my( $this, $c ) = @_;
  
  # were there any errors ? If so, render the error page into the response
  if( scalar @{ $c->error } ) {
    $c->log->debug( 'Root::end: found some errors from previous methods' );
    $c->stash->{errorMsg} = $c->error;
    $c->stash->{template} = 'pages/error.tt';
    
    # make sure the error page isn't cached
    $c->res->header( 'Pragma'        => 'no-cache' );
    $c->res->header( 'Expires'       => 'Thu, 01 Jan 1970 00:00:00 GMT' );
    $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'.
                                        'post-check=0, pre-check=0, max-age=0' );
    
    $c->forward( 'PfamWeb::View::TT' );
    $c->clear_errors;
  }

  # don't render anything else if the response already has content
  return 1 if $c->response->body;
  return 1 if $c->response->status =~ /^3\d\d$/;

  # set the content type to the default of "text/html", unless it's already set
  $c->response->content_type( 'text/html; charset=utf-8' )
    unless $c->response->content_type;

  # finally, default to the index page template and we're done
  $c->stash->{template} ||= 'pages/index.tt';
  $c->forward( 'PfamWeb::View::TT' );
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
