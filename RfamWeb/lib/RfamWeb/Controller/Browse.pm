
# Browse.pm
# jt6 20080314 WTSI
#
# $Id: Browse.pm,v 1.2 2008-06-17 09:17:15 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Browse - controller to build the "browse" pages

=cut

package RfamWeb::Controller::Browse;

=head1 DESCRIPTION

Retrieves the data for the various "browse" pages.

Generates a B<full page>.

$Id: Browse.pm,v 1.2 2008-06-17 09:17:15 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Caches the page and notifies the navbar of the location.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # stash the parameters, after checking that they're valid
  ( $c->stash->{$_} ) = $c->req->param($_) || '' =~ m/^(\w)$/
    for qw( to from top20 numbers );

  # set the page to be cached for one week
  #$c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'browse';
}

#-------------------------------------------------------------------------------

=head2 browse : Global

Show an index page for the various "browse" pages.

=cut

sub browse : Global {
  my ( $this, $c ) = @_;

  if ( defined $c->stash->{from} and
       defined $c->stash->{to} ) {
    $c->log->debug( 'Browse::browse_families: browsing by first letter' )
      if $c->debug;
    $c->forward( 'browse_range' );
  }
  
  elsif( defined $c->stash->{numbers} ) {
    $c->log->debug( 'Browse::browse_families: browsing by number' )
      if $c->debug;

    $c->stash->{from} = 0;
    $c->stash->{to}   = 9;
    $c->forward( 'browse_range' );
  }
  
  elsif( defined $c->stash->{top20} ) {
    $c->log->debug( 'Browse::browse_families: browsing top twenty families' )
      if $c->debug;
    $c->forward( 'browse_top20' );
  }

  $c->stash->{template} ||= 'pages/browse/index.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub browse_range : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Browse::browse_range: |' . $c->stash->{from}
                  . '|' . $c->stash->{to} . '|' ) if $c->debug;

  # sort the "letters" so that we know what we're getting from the loop
  my ( $from, $to ) = sort ( $c->stash->{from}, $c->stash->{to} );

  my @families;
  foreach my $char ( $from .. $to ) {
    my @rs = $c->model('RfamDB::Rfam')
                 ->search( { rfam_id => { 'LIKE', qq($char%) } },
                           { order_by => 'rfam_id' } );
    
    $c->log->debug( 'Browse::browse_range: found |' . scalar @rs
                    . '| families beginning with |' . $char . '|' )
      if $c->debug;
    
    push @families, @rs;
  } 
  $c->log->debug( 'Browse::browse_range: found |' . scalar @families
                  . '| families in total' ) if $c->debug;

  $c->stash->{families} = \@families;
  $c->stash->{template} = 'pages/browse/families.tt';

}

#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub browse_top20 : Private {
  my ( $this, $c ) = @_;
  
  my @rs = $c->model('RfamDB::Rfam')
             ->search( { },
                       { rows     => 20,
                         page     => 1,
                         order_by => 'num_full DESC' }
                       )->all;

  $c->log->debug( 'Browse::browse_top20: found |' . scalar @rs
                  . '| families in total' ) if $c->debug;

  $c->stash->{families} = \@rs;
  $c->stash->{template} = 'pages/browse/families.tt';
  
}

#-------------------------------------------------------------------------------
#- cargo area ------------------------------------------------------------------
#-------------------------------------------------------------------------------

#  my @res;
#
#  if( lc $c->req->param('browse') eq 'numbers' ) {
#    $c->log->debug( 'Browse::browseFamilies: browsing "numbers"...' );
#    $c->stash->{char} = 'numbers';
#
#    # run the query to get back all families starting with a number
#    @res = $c->model('PfamDB::Pfam')
#             ->search( { pfamA_id => { 'REGEXP', '^[0-9]' } },
#                       { order_by => 'pfamA_id ASC' } );
#
#  } elsif( lc $c->req->param('browse') eq 'top twenty' ) {
#    $c->log->debug( 'Browse::browseFamilies: browsing "top twenty"...' );
#    $c->stash->{char} = 'top twenty';
#
#    # retrieve the top twenty largest families, ordered by number of
#    # sequences in the family
#    @res = $c->model('PfamDB::Pfam')
#            ->search( { },
#                      { rows     => 20,
#                        page     => 1,
#                        order_by => 'num_full DESC' }
#                      )->all;
#
#  } else {
#
#    $c->log->debug( 'Browse::browseFamilies: not a number, not "top twenty"...' );
#
#    # see if we should load the page for families starting with a given letter
#    my $char;
#    ( $char ) = $c->req->param('browse') =~ /^(\w{1})$/;
#
#    if( defined $char ) {
#      $c->log->debug( "Browse::browseFamilies: browsing for a character: |$char|" );
#      $c->stash->{char} = uc $char;
#  
#      # run the query to get back all families starting with the
#      # specified letter, ordered by ID
#      @res = $c->model('PfamDB::Pfam')
#               ->search( { pfamA_id => { 'LIKE', qq($char%) } },
#                         { order_by => 'pfamA_id' } );
#
#    } else {
#
#      # either "new" specified, or no starting letter specified, so default 
#      # to new families anyway
#      $c->log->debug( 'Browse::browseFamilies: browsing new entries' );
#      $c->stash->{char} = 'new';
#  
#      @res = $c->model('PfamDB::Pfam')
#               ->search( { change_status => 'NEW' },
#                         {order_by => 'pfamA_id ASC' } );
#
#    }
#
#  }
#
#  # stash the results for the template
#  $c->stash->{browse} = \@res if scalar @res;
#
#  # set the template and let the default end action from Section
#  # render it for us
#  if( $c->req->param('list') ) {
#    # we want to return just the list of Pfam IDs, as a snippet of HTML.
#    # This is used in the domain query search form
#    $c->stash->{template} = 'pages/browse/ids.tt';
#  } else {
#    # just render as a regular 'browse' page
#    $c->stash->{template} = 'pages/browse/families.tt';
#  }
#}
#
#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
