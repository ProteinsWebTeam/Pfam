
# PfamViewer.pm
# jt6 20060601 WTSI
#
# $Id: PfamViewer.pm,v 1.8 2009-10-07 10:26:29 jt6 Exp $

=head1 NAME

PfamWeb::Controller::PfamViewer - viewing sequence alignments

=cut

package PfamWeb::Controller::PfamViewer;

=head1 DESCRIPTION

An HTML-based sequence alignment viewer.
 
$Id: PfamViewer.pm,v 1.8 2009-10-07 10:26:29 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use JSON;
use Data::Pageset;
use Data::Dump qw( dump );

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 showPfamViewer : Private

This is the way into the Pfam sequence alignment viewer.

Hands straight off to a template that generates a "tool" page containing the 
necessary hooks to load the interactive alignment viewer.

The viewer requires a set of input parameters, given in the stash as a hash.
This hash must contain at least the B<source> key, the value of which should
be one of the enumeration in the config for the viewer. The B<source> setting
tells the viewer which controller to forward to when it needs to retrieve the
alignment.

Another, optional, parameter is B<title>, which will be used as an HTML 
heading for the tool window that shows the alignment. This should be kept
fairly short.

=cut

sub showPfamViewer : Private {
  my ( $this, $c ) = @_;

  # these settings come directly from another controller, so they haven't
  # have been exposed to the user and don't need detainting, but we'll
  # escape them anyway
  foreach ( keys %{ $c->stash->{params} } ) {
    $c->stash->{escapedParams}->{$_} = uri_escape( $c->stash->{params}->{$_} );
  }
  
  $c->stash->{paramString} = to_json( $c->stash->{escapedParams} );
  $c->log->debug( 'PfamViewer::showPfamViewer: paramString: |'
                  . $c->stash->{paramString} . '|' ) if $c->debug;

  # hand off to the tool window template
  $c->log->debug( 'PfamViewer::showPfamViewer: handing off to alignmentTool.tt' )
    if $c->debug;
  $c->stash->{template} = 'components/tools/pfamviewer/alignmentTool.tt';
}

#-------------------------------------------------------------------------------

=head2 view : Local

This is the main action for the viewer. This action is responsible for 
coordinating the calculation of which page to show, retrieving the alignment
and marking it up as HTML, before handing off to the template that renders
the alignment fragment.

=cut

sub view : Local {
  my ( $this, $c ) = @_;

  # the parameters for this action were handed to us by the caller and although 
  # they *should* have been specified by the original caller, they've also been 
  # exposed to the user, so we need to detaint them before storing 
  
  # we also need to take care to store and pass on the input parameters, 
  # so that we keep any that the "getAlignment" method needs to do its job
  
  my %params;
  foreach my $param ( keys %{ $c->req->params } ) {
    my $escapedValue   = $c->req->param($param);
    my $unescapedValue = uri_unescape( $escapedValue );

    next unless $unescapedValue =~ m/^([A-Za-z0-9\-\s]+)$/;    
    
    $c->log->debug( "PfamViewer::view: stashing parameter: |$param|$unescapedValue|" )
      if $c->debug;
    
    $c->stash->{$param} = $unescapedValue;
    $params{$param} = $escapedValue;
  }
  # note that the values in the params hash are still escaped, but the stash
  # has unescaped values

  # this is now the record of the input parameters that we'll pass on
  $c->stash->{paramString} = to_json( \%params );
  $c->log->debug( 'PfamViewer::view: build a paramString: |'
                  . $c->stash->{paramString} . '|' ) if $c->debug;

  # set up the paging
  $c->forward( 'setPage' );

  # forward to the action on the caller that will:
  #   stash the number of rows in the alignment
  #   return an alignment fragment 

  # look up the class that we need to forward to
  unless ( defined $c->req->param('source') and 
           $c->req->param('source') =~ /^([A-Za-z]+)$/ ) { 
    $c->error( 'The alignment source must be set.' );
    return;
  }
  my $source = $1;
  my $class  = $this->{sources}->{$source};
  $c->log->debug( "PfamViewer::view: class: |$class|" ) if $c->debug;

  # make sure we can actually use the source to get an alignment
  unless ( $class->can( 'getAlignment' ) ) {
    $c->error( "'$source' is not a valid alignment source." );
    return;
  }

  # forward to that class
  $c->log->debug( "PfamViewer::view: forwarding to 'getAlignment' on '$class'" )
    if $c->debug;
  $c->forward( $class, 'getAlignment' );
  
  # shortcut to the hash with the details of the alignments that were returned
  my $alignments = $c->stash->{alignments};

  # mark up the alignments in HTML  
  my @markedUpAlignments;
  for ( my $i = 0; $i < length @{ $alignments->{rawAlignments} }; $i++ ) {
    my $alignment = $alignments->{rawAlignments}->[$i];
    my $consensus = $alignments->{consensus}->[$i];
    push @markedUpAlignments,
      Bio::Pfam::ColourAlign::markupAlignSeparate( $alignment, $consensus );
  }

  # stash the marked up alignments
  $c->stash->{alignments}->{alignments} = \@markedUpAlignments;

  # hand off to the template that will render the alignment fragment
  $c->stash->{template} = 'components/tools/pfamviewer/alignmentFragment.tt';
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 setPage : Private

Calculates which page of the alignment to show. 

=cut

# Stashed values used by this action:
#
#  numRowsInAlignment - total number of rows in the alignment
#
# Parameters used by this action:
#
#  scrollValue - horizontal scroll position
#  numRows     - number of rows of alignment to be displayed
#  page        - jump to the specified page in the alignment
#
# Values stashed by this actions:
#
#  scrollValue - scroll position
#  pager       - the Data::Pageset object
#  rows        - e.g. "1 - 100"
#  pagesBefore \ arrays with a list of pages before and after this one
#  pagesAfter  /

sub setPage : Private {
  my ( $this, $c ) = @_;
  
  # get the scroll position
  if ( defined $c->req->param('scrollValue') and
       $c->req->param('scrollValue') =~ m/^(\d+\.\d+)$/ ) {
    $c->stash->{scrollValue} = $1;
  }
  else {
    $c->stash->{scrollValue} = 0;
  }
  $c->log->debug( 'PfamViewer::setPage: set scroll value to |'
                  . $c->stash->{scrollValue} . '|' ) if $c->debug;

  #----------------------------------------

  # get the number of alignment lines to display
  my ( $numRowsToShow ) =
    $c->req->param('numRows') || $this->{defaultRows} =~ m/^(\d+)$/;

  # if the number of rows to show is less than the total number of rows in the
  # alignment, reset it
  $numRowsToShow = $c->stash->{numRowsInAlignment}
    if $c->stash->{numRowsInAlignment} < $numRowsToShow;

  $c->log->debug( "PfamViewer::view: showing |$numRowsToShow| rows" )
    if $c->debug;

  #----------------------------------------

  # use a Data::Pageset object to keep track of all this...
  my $pager = Data::Pageset->new( { total_entries    => $c->stash->{numRowsInAlignment}, 
                                     entries_per_page => $numRowsToShow,
                                     pages_per_set    => 11,
                                     mode             => 'slide' } );
  $c->stash->{pager} = $pager;

  # find out what page we want for this request
  my $page;
  if ( defined $c->req->param('page') ) {
    ( $page ) = $c->req->param('page') =~ m/^(\d+)$/;
    $c->log->debug( "PfamViewer::view: requested page number |$page|" )
      if $c->debug;
  }
  
  elsif( defined $c->req->param('next') ) {
    $page = $pager->next;
    $c->log->debug( 'PfamViewer::view: requested next page' ) if $c->debug;
  }
  
  elsif( defined $c->req->param('prev') ) {
    $page = $pager->prev;
    $c->log->debug( 'PfamViewer::view: requested previous page' ) if $c->debug;
  }
  
  $page ||= 1;
  $c->log->debug( "PfamViewer::view: showing page |$page|" ) if $c->debug;
  
  $pager->current_page( $page ); 

  #----------------------------------------

  # decide which actual rows we need to use now
  $c->stash->{rows} = [ $pager->first, $pager->last ];
  $c->log->debug( 'PfamViewer::view: rows: |'
                  . $c->stash->{rows}->[0] . '| to |'
                  . $c->stash->{rows}->[1] . '|' )
    if $c->debug;

  # store the lists of page numbers before and after the current one, for use
  # by the template in generating the list of available pages
  $c->stash->{pagesBefore} = [         1 .. $page - 1         ] if $page > 1;
  $c->stash->{pagesAfter}  = [ $page + 1 .. $pager->last_page ] if $page < $pager->last_page;
  
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
