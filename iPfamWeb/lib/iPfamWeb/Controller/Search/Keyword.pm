
# Keyword.pm
# pg6 20092512 WTSI
#
# $Id: Keyword.pm,v 1.6 2009-12-02 13:13:34 pg6 Exp $
#

=head1 NAME

PfamWeb::Controller::Search::Keyword - a keyword search engine for the site

=cut

package iPfamWeb::Controller::Search::Keyword;

=head1 DESCRIPTION

This controller reads a list of search plugins from the application
configuration and forwards to each of them in turn, collects the
results and hands off to a template to format them as a results page.

$Id: Keyword.pm,v 1.6 2009-12-02 13:13:34 pg6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'iPfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Retrieves the input and sends it back to the browser for making ajax calls.

=cut
sub begin: Private {
  my( $this, $c, $arg ) = @_;

   unless ( $c->req->param('query') ) {
    $c->stash->{kwSearchError} = 'You did not supply a query term.';

    $c->log->debug( 'Search::Keyword::textSearch: no query terms supplied' )
      if $c->debug;

    return;
  }
  
  # get the query
  my( $terms ) = $c->req->param('query') =~ /^([\w\:\;\-\.\s]+)/;

  # we're done here unless there's a query specified
  $c->log->warn( 'Search::Keyword::begin: no query terms supplied' ) and return
    unless defined $terms;

  $c->log->debug( 'Search::Keyword::begin: running query prasad with: |' 
                  . $terms . '|' );

  
  # stash the de-tainted terms so we can safely display them later
  $c->stash->{rawQueryTerms} = $terms;
  
  #now format the terms for querying the database;
  $c->forward( 'formatTerms');
  
}

#-------------------------------------------------------------------------------
=head2 error :Path ('/search/keyword/error' )

Method to handle cases where no results are retireved for search.

=cut

sub error: Path( '/search/keyword/error') {
  my ( $this, $c ) = @_;
  
  $c->log->debug( "Search::Keyword::error: This is called as there are no results" );
  # set the page template and we're done
  $c->stash->{template} = 'pages/search/keyword/error.tt';
}

#-------------------------------------------------------------------------------
#
sub keyword : Path {
  my ( $this, $c ) = @_;
  
  $c->log->debug( "Search::Keyword::process: This does nothing and usually child classes overwrite this method" );
  # set the page template and we're done
  $c->stash->{template} = 'pages/search/keyword/results.tt' unless( $c->stash->{kwSearchError} ); 
}  


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