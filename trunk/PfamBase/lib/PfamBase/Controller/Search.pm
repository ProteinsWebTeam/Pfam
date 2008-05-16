
# Search.pm
# jt6 20080314 WTSI
#
# $Id: Search.pm,v 1.4 2008-05-16 14:58:22 jt6 Exp $

=head1 NAME

PfamBase::Controller::Search - perform various searches

=cut

package PfamBase::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running searches.

$Id: Search.pm,v 1.4 2008-05-16 14:58:22 jt6 Exp $

=cut

use strict;
use warnings;

use Module::Pluggable;

use base 'PfamBase::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'search' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract the query terms from the URL and de-taint them.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'search';
  
  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;
  
  #----------------------------------------
  
  # see if we should pre-fill the sequence field, based on the sequence that
  # was handed to us by the referring site
  
  # detaint it, naturally, and accept only sequences that are shorter than some
  # limit, which is set in the config
  if ( defined $c->req->param('preseq') and 
       $c->req->param('preseq') =~ m/^([A-Z]+)$/ and
       length( $1 ) < $this->{maxPrefillLength} ) {
    $c->log->debug( 'Search::begin: found a sequence with which to pre-fill the search form' )
      if $c->debug;

    # stash it and let the template stuff the value into the form    
    $c->stash->{preseq} = $1;
  }
  
  #----------------------------------------

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  $c->stash->{output_xml} = ( defined $c->req->param('output') and
                              $c->req->param('output') eq 'xml' );

}

#-------------------------------------------------------------------------------

=head2 formatTerms : Private

Inherited by the search plugins, this action formats the query terms to add 
wildcard and fulltext operators to each word in the list. This base 
implementation just prepends a "+" (require that the word is present in every 
returned row; necessary for "IN BOOLEAN MODE" queries) and appends a "*" 
(wildcard) to each term.

This method should be over-ridden by plugin search classes if they
need some other processing to be performed on the search terms.

=cut

sub formatTerms : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
    join " ", map { $_ = "+$_*" } split /\s+|\W|\_/, $c->stash->{rawQueryTerms};

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
