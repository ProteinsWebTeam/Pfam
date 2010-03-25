
# Alignment.pm
# jt6 20070725 WTSI
#
# $Id: Alignment.pm,v 1.1 2008-07-25 13:26:50 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Family::Alignment - a base class for alignment handling

=cut

package RfamWeb::Controller::Family::Alignment;

=head1 DESCRIPTION

This is intended as the basis for alignment-related code.

$Id: Alignment.pm,v 1.1 2008-07-25 13:26:50 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'RfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 jalview : Local

This is the way into the JalView alignment viewer applet.

Hands straight off to a template that generates a "tool" page containing the 
JalView applet.

=cut

sub jalview : Local {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'components/tools/jalview.tt';
}

#-------------------------------------------------------------------------------

=head2 html : Path

Retrieves the HTML alignment and dumps it to the response. We first try to 
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub html : Local {
  my ( $this, $c ) = @_;

  # get all of the blocks  
  my @rs = $c->stash->{rfam}->search_related( 'html_alignments',
                                              { type => $c->stash->{alnType} } );

  unless ( scalar @rs ) {
    $c->log->debug( 'Family::Alignment::html: failed to retrieve an alignment block' )
      if $c->debug;
    $c->stash->{errorMsg} = 'We could not extract the requested alignment block for '
                            . $c->stash->{acc};
    return;
  }

  $c->log->debug( 'Family::Alignment::html: found ' . scalar @rs . ' blocks' )
    if $c->debug;

  # decide which block to show
  my ( $block_num ) = $c->req->param('block') =~ m/^(\d+)$/;
  $block_num ||= 0;

  $c->log->debug( "Family::Alignment::html: showing block $block_num" )
    if $c->debug;

  # gunzip the html
  my $gzipped_html = $rs[$block_num]->html;
  my $block = Compress::Zlib::memGunzip( $gzipped_html );
  unless ( defined $block ) {
    $c->log->debug( 'Family::Alignment::html: failed to gunzip the alignment block' )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem uncompressing an alignment block for '
                            . $c->stash->{acc};
    return;
  }
 
  # $c->log->debug( "Family::Alignment::html: block code: : |$block|" )
  #  if $c->debug;

  # stash the stuff that's used for paging in the template
  $c->stash->{alignment_block}   = $block;
  $c->stash->{current_block_num} = $block_num;
  $c->stash->{last_block_num}    = scalar @rs - 1;

  $c->stash->{template} = 'components/tools/html_alignment.tt';
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
