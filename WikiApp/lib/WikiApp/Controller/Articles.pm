
# Articles.pm
# jt6 20100422 WTSI
#
# $Id$

=head1 NAME

WikiApp::Controller::Articles - shows wikipedia articles

=cut

package WikiApp::Controller::Articles;

=head1 DESCRIPTION

This controller handles the display and approval of wikipedia articles.

=cut

use Moose;
use namespace::autoclean;
use MediaWiki::Bot;
use Bio::Pfam::Wiki::Updater;
use Data::Dump qw(dump);
use URI::Escape;

BEGIN {extends 'Catalyst::Controller'; }

#-------------------------------------------------------------------------------

=head1 ACTIONS

=head2 articles : Global

Shows the list of articles to be updated.

=cut

sub articles : Global {
  my ( $this, $c, $type ) = @_;

  $c->stash->{template} = 'articles.tt';

  # build the search terms; check if we need to return articles for only one
  # database
  my $search_terms = { approved_revision => { '!=', \'wikipedia_revision' } };
  if ( defined $type and $this->{handled_article_types}->{$type} ) {
    $c->log->debug( "Articles::articles: looking for |$type| articles" )
      if $c->debug;

    $search_terms->{ $this->{handled_article_types}->{$type} } = { '!=' => 'inactive' };

    $c->stash->{handled_article_types} = $c->forward('type_list');
    $c->stash->{type} = $type;
  }

  # build the search attributes; see how many rows to return
  my $search_attributes = {};
  if ( defined $c->req->param('rows') ) {

    if ( $c->req->param('rows') =~ m/^(\d+)$/ ) {
      $c->log->debug( "Articles::articles: showing |$1| rows" )
        if $c->debug;
      $search_attributes->{rows} = $1;
    }
    elsif ( $c->req->param('rows') eq 'all' ) {
      $c->log->debug( "Articles::articles: showing ALL rows" )
        if $c->debug;
    }

  }
  else {
    $c->log->debug( "Articles::articles: showing 20 rows (default)" )
      if $c->debug;
    $search_attributes->{rows} = 20;
  }
  $c->stash->{rows} = $search_attributes->{rows};

  # decide which page to return
  if ( defined $c->req->param('page') and
       $c->req->param('page') =~ m/^(\d+)$/ ) {
    $c->log->debug( "Articles::articles: showing page |$1|" )
      if $c->debug;
    $c->stash->{page} = $1;
    $search_attributes->{page} = $1;
  }
  else {
    $search_attributes->{page} = 1;
  }

  # run the search...
  my $articles = $c->model( 'WikiAppDB::Wikipedia' )
                   ->search( $search_terms, $search_attributes );
  $c->log->debug( 'Articles::articles: got ' . $articles->count . ' articles' )
    if $c->debug;
  
  $c->stash->{articles} = $articles;

  # stash the pager too, so that we can work out where we are in the page list
  $c->stash->{pager} = $articles->pager;
}

#-------------------------------------------------------------------------------

=head2 article : Chained('/') : PathPart('article')

Retrieves the details of the specified article.

=cut

sub article : Chained('/') PathPart('article') CaptureArgs(1) {
  my ( $this, $c, $encoded_title ) = @_;

  my $title = uri_unescape( $encoded_title );

  die qq[invalid article title ("$title")] if $title =~ m/[\%\`\#\;\?\&]/;

  $c->log->debug( "Articles::article: got article title |$title|" )
    if $c->debug;
  
  my $rs = $c->model( 'WikiAppDB::Wikipedia' )
             ->find( { title => $title } );

  $c->stash->{article} = $rs;
}

#-------------------------------------------------------------------------------

=head2 approve : Chained('article') : PathPart('approve')

Updates the approval status of the specified article. If a revision ID is given
as an argument, that is used as the approved version. If no revision ID is
given, the wikipedia revision from the database is used instead.

=cut

sub approve : Chained('article') PathPart('approve') Args(0) {
  my ( $this, $c ) = @_;

  unless ( $c->user_exists ) {
    $c->res->status( 401 ); # Not authorized
    $c->res->body( 'You are not authorised to perform that operation. Please login first.' );
    return;
  }

  my $revid       = $c->req->params->{revid};       # revision to use as approved version
  my $approved_by = $c->req->params->{approved_by}; # username of approver
  my $updated     = $c->req->params->{updated};     # timestamp for the update. Leave as
                                                    # undef to use "NOW()"

  unless ( defined $revid and $revid =~ m/^\d+$/ ) {
    $c->log->debug( 'Approve::approve: no valid revision ID' )
      if $c->debug;
    $c->res->status( 400 ); # Bad request
    $c->res->body( 'Must supply a valid revision ID' );
    return;
  }

  unless ( defined $approved_by and $approved_by =~ m/^\w+$/ ) {
    $c->log->debug( 'Approve::approve: no valid username' )
      if $c->debug;
    $c->res->status( 400 ); # Bad request
    $c->res->body( 'Must supply a valid user name for approver' );
    return;
  }

  $c->log->debug( "Articles::approve: user $approved_by is approving revision $revid for article " 
                  . $c->stash->{article}->title . " at $updated" ) if $c->debug;

  eval {
    $c->stash->{article}->update_approval( $revid, $approved_by, $updated );
  };
  if ( $@ ) {
    # something dire happened when trying to get the model to approve
    $c->res->status( 500 ); # Internal server error
    $c->res->body( "Failed to approve article: $@" );
    return;
  }

  $c->res->status( 204 );
  $c->res->body( 'OK' );
}

#-------------------------------------------------------------------------------

=head2 update : Local

Updates the approved_revision for the specified article. Detaches to the
controller that shows the list of all unapproved articles.

=cut

sub update : Local {
  my ( $this, $c, $titles ) = @_;

	my $u = Bio::Pfam::Wiki::Updater->new( schema => $c->model('WikiAppDB')->schema );

	if ( $titles ) {
		unless ( $titles =~ m/[\/%\`\#;?&,]/ ) {
			$c->log->debug( "Articles::update: invalid titles: |$titles|" )
				if $c->debug;
			$c->res->status( 400 ); # Bad request
			$c->res->body( "Invalid title(s)" );
			return;
		}

		$c->log->debug( "Articles::update: updating specified titles: |$titles|" )
			if $c->debug;
		$u->update( split /,/, $titles );
	}
	else {
		$c->log->debug( 'Articles::update: no titles specified; updating all' )
			if $c->debug;
		$u->update_all;
	}

	my %updated_articles = map { $_ => 1 } @{ $u->updated_articles };

	$c->stash->{updateCount}     = $u->num_updated;
	$c->stash->{updatedArticles} = \%updated_articles;

	$c->detach( $c->controller('Articles')->action_for('articles') );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 type_list : Private

Returns the list of handled article types. This is simply copied straight from 
the config for this controller, and the point of the method is to make the list
available in, for example, the index action on the Root controller.

=cut

sub type_list : Private {
  my ( $this, $c ) = @_;

  return [ sort keys %{ $this->{handled_article_types} } ];
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
         Paul Gardner (pg5@sanger.ac.uk)

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

__PACKAGE__->meta->make_immutable;

1;
