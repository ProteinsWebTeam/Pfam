
# Domain.pm
# jt6 20061108 WTSI
#
# $Id: Domain.pm,v 1.7 2009-10-27 14:16:32 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Domain - perform domain architecture searches

=cut

package PfamWeb::Controller::Search::Domain;

=head1 DESCRIPTION

Searches for sequence architectures with the specified set of Pfam domains.

$Id: Domain.pm,v 1.7 2009-10-27 14:16:32 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Pfam::Drawing::Layout::LayoutManager;

use Storable qw( thaw );
use Data::Dump qw(dump);

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 domain_search : Path

Executes a domain query.

=cut

sub domain_search : Path {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::Domain::domain_search: executing a domain search' )
    if $c->debug;

  $c->stash->{template} = 'components/allArchitectures.tt';

  unless ( $c->forward( 'get_data' ) ) {
    $c->log->debug( 'Search::Domain::domain_search: problem getting data' )
        if $c->debug;
    
    $c->stash->{domainSearchError} = $c->stash->{errorMessage}
                                     || 'There was a problem running your query.'; 
   
    return; 
  }

  unless ( $c->forward( 'build_layout' ) ) {
    $c->log->debug( 'Search::Domain::domain_search: problem getting data' )
        if $c->debug;
    
    $c->stash->{domainSearchError} = $c->stash->{errorMessage}
                                     || 'There was a problem running your query.'; 
   
    return; 
  }



}

sub get_data : Private {
  my ( $this, $c ) = @_;
  
  my $list = '';
  if ( defined $c->req->param( 'have' ) ) {
    $c->log->debug( 'Search::Domain::domain_search: must have:     |' 
                    . $c->req->param('have') . '|' ) if $c->debug;
    foreach ( split /\s+/, $c->req->param('have') ) {
      next unless /(PF\d{5})/;
      $list .= "+$1 ";
    }
  }
  if ( defined $c->req->param( 'not' ) ) {
    $c->log->debug( 'Search::Domain::domain_search: must not have: |' . $c->req->param('not') . '|' )
      if $c->debug;
    foreach ( split /\s+/, $c->req->param('not') ) {
      next unless /(PF\d{5})/;
      $list .= "-$1 ";
    }
  }

  $c->log->debug( "Search::Domain::domain_search: list: |$list|" )
    if $c->debug;

  unless ( $list ) {
    $c->log->debug( "Search::Domain::get_data: no query terms" ) 
        if $c->debug;

    $c->stash->{errorMessage} = 'You did not supply any query terms';
    
    return 0; 
  }

  my @architectures = $c->model('PfamDB::Architecture')
                        ->search( {},
                                  { join     => [ qw( storable ) ],
                                    prefetch => [ qw( storable ) ],
                                    order_by => "no_seqs DESC" } )
                        ->search_literal( 'MATCH( architecture_acc ) ' .
                                          'AGAINST( ? IN BOOLEAN MODE )',
                                          $list );

  my $sum = 0;
  map { $sum += $_->no_seqs } @architectures;
  $c->log->debug( "Search::Domain::get_data: sum using map: $sum" )
      if $c->debug;

  $sum = 0;
  foreach my $arch ( @architectures ) {
    $sum += $arch->no_seqs;
  }
  $c->log->debug( "Search::Domain::get_data: sum using for: $sum" )
      if $c->debug;

  $c->stash->{numRows} = scalar @architectures;
  $c->stash->{numSeqs} = $sum;

  $c->log->debug( 'Search::Domain::domain_search: found ' . scalar @architectures
                  . " rows, with a total of $sum sequences" ) if $c->debug;

  # if there are too many results, bail here and let the TT just
  # display the text summary, plus an admonition to the user to
  # restrict their search a bit
  if ( scalar @architectures > 500 ) {
    $c->log->debug( 'Search::Domain::get_data: too many sequences: ' . scalar @architectures )
        if $c->debug;

    $c->stash->{errorMessage} = 'Your query returned too many sequences';
    
    return 0;
  }
  
  my ( @seqs, %seqInfo, @ids );
  foreach my $row ( @architectures ) {

    # thaw out the sequence object for this architecture and get a handle on
    # the right DB object
    push @seqs, thaw( $row->storable->annseq_storable );

    # we're looking at all sequences, so we want just the type example
    my $seq = $row->type_example;

    # stash the sequence IDs for the type example in an array, so that we can 
    # access them in the right order in the TT, i.e. ordered by number of 
    # sequences with the given architecture)
    my $pfamseq_id = $seq->pfamseq_id;
    push @ids, $pfamseq_id;

    # work out which domains are present on this sequence
    my @domains = split m/\~/, $row->architecture;
    $seqInfo{$pfamseq_id}{arch} = \@domains;

    # how many sequences ?
    $seqInfo{$pfamseq_id}{num} = $row->no_seqs;

    # store a mapping between the sequence and the auto_architecture
    $seqInfo{$pfamseq_id}{auto_arch} = $row->auto_architecture;

    # store the sequence description, species name and length of each 
    # individual sequence
    $seqInfo{$pfamseq_id}{desc}    = $seq->description;
    $seqInfo{$pfamseq_id}{species} = $seq->species;
    $seqInfo{$pfamseq_id}{length}  = $seq->length;
  }

  $c->log->debug( 'DomainGraphics::get_family_data: retrieved '
                  . scalar @seqs . ' storables' ) if $c->debug;

  $c->stash->{seqs}    = \@seqs;
  $c->stash->{ids}     = \@ids;
  $c->stash->{seqInfo} = \%seqInfo;
}

sub build_layout : Private {
  my ( $this, $c ) = @_;

  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  my $pfama = $lm->_getRegionConfigurator('Pfama');
  my $pfamb = $lm->_getRegionConfigurator('Pfamb');
  
  if ( $c->req->param('ac') ) {

    # detaint the param by trying to decode it as JSON
    my $data;
    eval {
      $data = from_json( uri_unescape( $c->req->param('ac') ) );
    };
    if ( $@ or not defined $data ) {
      # decoding failed; don't try to use the data
      $c->log->warn( 'Search::Domain::build_layout: failed to detaint colours param' )
        if $c->debug;
    }
    else {
      # decoding worked; set these assigned colours on the layout manager
      # before generating the new layout
      my $colours_json = uri_unescape( $c->req->param('ac') );
      my $colours = from_json( $colours_json );
      
      my ( $pfama_colours, $pfamb_colours );
      # Note that we're not validating the colours here, because they're going
      # to be passed to the Moose objects, which all have strict type checking
      # in place. Any broken data will cause an exception when the Moose object
      # tries to use the data.
      
      # split the colours into Pfam-A and Pfam-B colours
      foreach ( keys %$colours ) {
        if ( m/^(PF\d{5})$/ ) {
          $pfama_colours->{$1} = $colours->{$1};
        }
        elsif ( m/^(PB\d{6})$/ ) {
          $pfamb_colours->{$1} = $colours->{$1};
        }
      }
  
      $c->log->debug( 'Search::Domain::build_layout: pfama_colours: ' 
                      . dump( $pfama_colours ) ) if $c->debug;
      $c->log->debug( 'Search::Domain::build_layout: pfamb_colours: ' 
                      . dump( $pfamb_colours ) ) if $c->debug;
      
      # and pre-assign the colours to the respective configurators
      if ( $pfama_colours ) {
        $pfama->assignedColours( $pfama_colours );
        $pfama->colourIndex( scalar( keys %{ $pfama->assignedColours } ) + 1 );
      }

      if ( $pfamb_colours ) {
        $pfamb->assignedColours( $pfamb_colours );
      }
    }
  }
  
  # let the layout manager build the domain graphics definition from the
  # sequence objects
  $lm->layoutSequences( $c->stash->{seqs} );

  $c->log->debug( 'Search::Domain::build_layout: pfama->assignedColours: ' 
                  . dump( $pfama->assignedColours ) ) if $c->debug;
  $c->log->debug( 'Search::Domain::build_layout: pfamb->assignedColours: ' 
                  . dump( $pfamb->assignedColours ) ) if $c->debug;

  # configure the JSON object to correctly stringify the layout manager output
  my $json = new JSON;
  # $json->pretty(1);
  $json->allow_blessed;
  $json->convert_blessed;

  # encode and stash the sequences as a JSON string
  $c->stash->{layout} = $json->encode( $c->stash->{seqs} );

  # stash the assigned colours from the layout manager. First we need to merge
  # the sets of colours from the two configurators
  if ( defined $pfama and defined $pfamb ) {

    my $valid_colours;
    my $pfama_colours = $pfama->assignedColours;
    foreach ( keys %$pfama_colours ) {
      next unless $_;
      $valid_colours->{$_} = $pfama_colours->{$_};
    }
    my $pfamb_colours = $pfamb->assignedColours;
    foreach ( keys %$pfamb_colours ) {
      next unless $_;
      $valid_colours->{$_} = $pfamb_colours->{$_};
    }

    $c->stash->{assignedColours} = $json->encode( $valid_colours );
    
    $c->log->debug( 'Search::Domain::build_layout: assigned colours: ' 
                    . dump( $c->stash->{assignedColours} ) )
      if $c->debug;
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
