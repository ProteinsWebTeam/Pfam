
# Root.pm
# jt 20080226 WTSI
#
# $Id: Root.pm,v 1.14 2010-01-19 09:59:09 jt6 Exp $

=head1 NAME

PfamBase::Controller::Root - base class for the Xfam Root classes

=cut

package PfamBase::Controller::Root;

=head1 DESCRIPTION

This is the base class for the Xfam website catalyst applications. It's 
intended to be sub-classed to build the specific site Root.pm classes.

$Id: Root.pm,v 1.14 2010-01-19 09:59:09 jt6 Exp $

=cut

use strict;
use warnings;

use XML::Feed;
use DateTime;

use base 'Catalyst::Controller';
use LWP::Simple;

# set the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
__PACKAGE__->config->{namespace} = '';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Checks the request parameters for a "tab" parameter and sets the
appropriate tab name in the stash. That will be picked up by the
tab_layout.tt view, which will use it to figure out which tab to show
by default.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  # pick a tab
  my $tab;
  ($tab) = $c->req->param('tab') =~ /^(\w+)$/
    if defined $c->req->param('tab');

  $c->stash->{showTab} = $1 if defined $tab;

  return 1;
}

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the main site index page.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'home';

  $c->log->debug('PfamBase::index: generating site index') if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 announcements : Local

Returns a snippet of HTML showing summaries of the most recent set of blog posts.
Intended to be called only via an AJAX request from the homepage.

=cut

sub announcements : Local {
  my ( $this, $c ) = @_;

  # see whether we're returning announcements or features
  my $type = $c->req->param('type');

  # see if there's a "hide" cookie
  my $cookie = $c->req->cookie("hide_posts");
  
  #----------------------------------------
  
  my $entries;  

  # retrieve the blog content
  my $ua = LWP::UserAgent->new;
  $ua->timeout(10);
  $ua->env_proxy;

  my $response = $ua->get( $this->{blog_uri} );
  unless ( $response->is_success ) {
    $c->log->warn( "Root::announcements: couldn't retrieve blog content from |"
                    . $this->{blog_uri} . "|" ) if $c->debug;
    $c->res->status( 204 );
    return;
  }
  my $blog_content = $response->decoded_content;

  # parse the XML and turn it into an XML::Feed object
  my $feed = XML::Feed->parse( \$blog_content );
  unless ( defined $feed ) {
    $c->log->warn( "Root::announcements: couldn't parse blog content" )
      if $c->debug;
    $c->res->status( 204 );
    return;
  }

  # check the timestamp on each entry and decide if we should show it or not  
  foreach my $entry ( $feed->entries ) {
    my $issued = $entry->issued->epoch;

    if ( defined $cookie ) {
      # $c->log->debug( "Root::announcements: cookie shows timestamp "
      #                 . $cookie->value . '; entry issued at '
      #                 . $issued ) if $c->debug;
      
      if ( $issued > $cookie->value ) {
        # $c->log->debug( "Root::announcements: post is newer than cookie; showing" )
        #   if $c->debug;
        # $entries->{$issued} = $entry;
      }
      # else {
      #   $c->log->debug( "Root::announcements: cookie is newer than post; NOT showing" )
      #     if $c->debug;
      # }
      
    }
    else { 
      # $c->log->debug( "Root::announcements: no $type cookie found; adding post "
      #                 . $entry->id )
      #   if $c->debug;
      $entries->{$issued} = $entry;
    }

  }
   
  #----------------------------------------

  my $i = 0;
  foreach my $issued ( reverse sort keys %$entries ) {
    my $entry = $entries->{$issued};
    
    $c->log->debug( "Root::announcements: adding post $i" )
      if $c->debug;
    
    # TODO should make the maximum number of posts into a configuration value
    if ( $i >= 3 ) { 
      $c->log->debug( "Root::announcements: reached limit for posts" )
        if $c->debug;
      last;
    }

    $c->stash->{entries}->{$issued} = $entry; 
    $i++;
  }
  
  if ( scalar keys %$entries ) {
    $c->log->debug( "Root::announcements: found some posts; handing off to template" )
      if $c->debug;
    $c->stash->{template} = 'pages/announcements.tt';
  }
  else {
    $c->log->debug( "Root::announcements: found NO posts; returning 204" )
      if $c->debug;
    $c->res->status( 204 );
  }
    
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
  my ( $this, $c ) = @_;

  return unless $this->{reportErrors};

  my $el = $c->model( 'WebUser::ErrorLog' );
  foreach my $e ( @{$c->error} ) {

    $c->log->error( "PfamBase::reportError: reporting a site error: |$e|" );
    # see if we can access the table at all - basically, see if the DB is up 
    my $rs; 
    eval {
      $rs = $el->find( { message => $e } );
    };
    if ( $@ ) {
      # really bad; an error while reporting an error...
      $c->log->error( "PfamBase::reportError: couldn't create a error log; "
                      . "couldn't read error table: $@" );
    }
  
    # if we can get a ResultSet, try to add a message
    if ( $rs ) {

      # we've seen this error before; update the error count
      eval {
        $rs->update( { num => $rs->num + 1 } );
      };
      if ( $@ ) {
        # really bad; an error while reporting an error...
        $c->log->error( "PfamBase::reportError: couldn't create a error log; "
                        . "couldn't increment error count: $@" );
      }

    }
    else {

      # no log message like this has been registered so far; add the row 
      eval {
        $el->create( { message => $e,
                       num     => 1,
                       first   => [ 'CURRENT_TIMESTAMP' ] } );
      };
      if ( $@ ) {
        # really bad; an error while reporting an error...
        $c->log->error( "PfamBase::reportError: couldn't create a error log; "
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

=head2 favicon : Path

Redirect requests for "favicon.ico" to the actual file.

=cut

sub favicon : Path( '/favicon.ico' ) {
  my ( $this, $c ) = @_;
  
  # set the status to 301 "Moved permanently" too.
  $c->res->redirect( $c->uri_for( '/static/images/favicon.png' ), 301 );
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
    $c->log->warn( 'Root::end: found some errors from previous methods' )
      if $c->debug;
    $c->stash->{errorMsg} = $c->error;
    $c->stash->{template} = 'pages/error.tt';
    
    # make sure the error page isn't cached
    $c->res->header( 'Pragma'        => 'no-cache' );
    $c->res->header( 'Expires'       => 'Thu, 01 Jan 1970 00:00:00 GMT' );
    $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'.
                                        'post-check=0, pre-check=0, max-age=0' );
    
    $c->forward( $c->view('TT') );
    $c->clear_errors;
  }

  # don't render anything else if the response already has content or if we're
  # specifically not returning content
  return 1 if $c->res->body;
  return 1 if $c->res->status =~ /^3\d\d$/;
  return 1 if $c->res->status == 204;

  # set the content type to the default of "text/html", unless it's already set
  $c->response->content_type( 'text/html; charset=utf-8' )
    unless $c->res->content_type;

  # finally, default to the index page template and we're done
  $c->stash->{template} ||= 'pages/index.tt';
  $c->forward( $c->view('TT') );
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
