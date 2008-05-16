
# Domain.pm
# jt6 20061108 WTSI
#
# $Id: Domain.pm,v 1.4 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Domain - perform domain architecture searches

=cut

package PfamWeb::Controller::Search::Domain;

=head1 DESCRIPTION

Searches for sequence architectures with the specified set of Pfam domains.

$Id: Domain.pm,v 1.4 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw( thaw );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 domainSearch : Path

Executes a domain query.

=cut

sub domainSearch : Path {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Domain::domainSearch: executing a domain search' );

  # point at the template right away
  $c->stash->{template} = 'components/allArchitectures.tt';

  my $list = '';
  if( defined $c->req->param( 'have' ) ) {
    $c->log->debug( 'Search::Domain::domainSearch: must have:     |' . $c->req->param('have') . '|' );
    foreach ( split /\s+/, $c->req->param('have') ) {
      next unless /(PF\d{5})/;
      $list .= "+$1 ";
    }
  }
  if( defined $c->req->param( 'not' ) ) {
    $c->log->debug( 'Search::Domain::domainSearch: must not have: |' . $c->req->param('not') . '|' );
    foreach ( split /\s+/, $c->req->param('not') ) {
      next unless /(PF\d{5})/;
      $list .= "-$1 ";
    }
  }

  $c->log->debug( "Search::Domain::domainSearch: list: |$list|" );

  return unless $list;

  my @architectures = $c->model('PfamDB::Architecture')
                        ->search( {},
                                  { join     => [ qw( storable ) ],
                                    prefetch => [ qw( storable ) ],
                                    order_by => "no_seqs DESC" } )
                        ->search_literal( 'MATCH( architecture_acc ) ' .
                                          'AGAINST( ? IN BOOLEAN MODE )',
                                          $list );

  my $sum = 0;
  foreach my $arch ( @architectures ) {
    $sum += $arch->no_seqs;
  }

  $c->log->debug( 'Search::Domain::domainSearch: found '
                  . scalar @architectures
                  . ' rows, with a total of $sum sequences' );

  $c->stash->{numRows} = scalar @architectures;
  $c->stash->{numSeqs} = $sum;

  # if there are too many results, bail here and let the TT just
  # display the text summary, plus an admonition to the user to
  # restrict their search a bit
  return if scalar @architectures > 500;

  # build the mappings that we'll need to interpret all this...
  my( @seqs, %seqInfo );
  foreach my $arch ( @architectures ) {

    # thaw out the sequence object for this architecture
    push @seqs, thaw( $arch->annseq_storable );
  
    # work out which domains are present on this sequence
    my @domains = split m/\~/, $arch->architecture;
    $seqInfo{$arch->pfamseq_id}{arch} = \@domains;
  
    # store a mapping between the sequence and the auto_architecture
    $seqInfo{$arch->pfamseq_id}{auto_arch} = $arch->auto_architecture;
  
    # if this is a call to retrieve all of the architectures, we don't
    # have an auto_architecture, so this won't work
    $seqInfo{$arch->pfamseq_id}{num} = $arch->no_seqs unless $c->stash->{auto_arch};
  }
  $c->log->debug( 'found ' . scalar @seqs . ' storables' );

  if( scalar @seqs ) {
    my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
    
    $layout->layout_sequences_with_regions_and_features( \@seqs, { PfamA      => 1,
                                                                   PfamB      => 1,
                                                                   noFeatures => 1 } );
    
    my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
    $imageset->create_images( $layout->layout_to_XMLDOM );
  
    $c->stash->{images} = $imageset;
    $c->stash->{seqInfo}  = \%seqInfo;
  }

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
