
# Proteome.pm
# rdf 20060821 WTSI
#
# $Id: Proteome.pm,v 1.13 2009-01-09 12:59:24 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Proteome - controller for the proteome section

=cut

package PfamWeb::Controller::Proteome;

=head1 DESCRIPTION

Controller to build the main Pfam Proteome page.

$Id: Proteome.pm,v 1.13 2009-01-09 12:59:24 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'proteome' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract an NCBI taxonomy ID from the parameters and retrieves the 
details of the proteome with that tax ID.

=cut

sub begin : Private {
  my( $this, $c, $entry_arg ) = @_;

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') and
       $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
    $c->res->content_type('text/xml');    
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('id') ||
                      $c->req->param('taxId') ||
                      $c->req->param('ncbi_code')     ||
                      $c->req->param('entry')     ||
                      $entry_arg                  ||
                      '';

  # we're only accepting an NCBI tax ID  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^(\d+)$/;
    $c->stash->{errorMsg} = 'Invalid NCBI taxonomy ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No NCBI taxonomy ID specified';
  }

  # retrieve data for this entry
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;


  if( defined $c->req->param('taxId') and 
      $c->req->param('taxId') =~ m/^(\d+)$/i ) {
    
    $c->log->info( "Proteome::begin: found tax ID |$1|" );
    $c->stash->{taxId} = $1;
    
    $c->stash->{proteomeSpecies} = $c->model('PfamDB::Proteome_species')
                                     ->find( { ncbi_code => $1},
                                             { join      => [ qw( ncbi_tax ) ],
                                               prefetch  => [ qw( ncbi_tax ) ] } 
                                           );

    $c->forward( 'getSummaryData' );
    $c->forward( 'getStats');
  }
  
  # this controller could be handed a parameter "pfamAcc", which is a Pfam-A
  # accession. We steer clear of using the standard parameter "acc" because 
  # that's used interchangably throughout the app to represent an accession for
  # whatever type of entity we're dealing with, from Pfam-A to clan to sequence.
  # In this case that would imply at "acc" represents a proteome, but in fact 
  # that's represented in the parameter "taxId", hence the use of "pfamAcc"
  # instead...
  
  if ( defined $c->req->param('pfamAcc') and
       $c->req->param('pfamAcc') =~ m/^(PF\d{5})$/ ) {
    $c->log->debug( "Proteome::begin: found a Pfam-A accession: |$1|" );

    $c->stash->{pfamAcc} = $1;
    $c->stash->{pfam} = $c->model('PfamDB::Pfam')
                          ->find( { pfamA_acc => $1 } );
  }

  # Note: I'm no longer sure what this last DB lookup is for...
  # jt6 20081219 WTSI

}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 stats : Local

Builds a table showing the domain composition of the proteome. Intended to be 
called via AJAX and builds a page B<fragment>.

=cut

sub stats : Local {
  my( $this, $c ) = @_;
  
  $c->stash->{template} = 'components/blocks/proteome/statsTable.tt';  
}

#-------------------------------------------------------------------------------

=head2 graphics : Local

Generates domain graphics for each of the sequences containing the specified
Pfam-A domain.

=cut

sub graphics : Local {
  my( $this, $c ) = @_;
  
  $c->stash->{template} = 'components/blocks/proteome/graphicsTool.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieve data for this proteome.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  my $rs = $c->model('PfamDB::Proteome_species')
             ->find( { ncbi_code => $entry },
                     { join      => [ 'ncbi_tax' ],
                       prefetch  => [ 'ncbi_tax' ] } );
  
  unless ( defined $rs ) {
    $c->stash->{errorMsg} = 'No valid NCBI taxonomy ID found';
    return;
  }
  
  $c->stash->{taxId} = $entry;
  
  $c->log->debug( 'Proteome::get_data: got a proteome entry' ) if $c->debug;
  $c->stash->{proteomeSpecies} = $rs;
  
  # only add extra data to the stash if we're actually going to use it later
  if ( not $c->stash->{output_xml} and 
       ref $this eq 'PfamWeb::Controller::Proteome' ) {
    
    $c->log->debug( 'Proteome::get_data: adding extra info' )
      if $c->debug;
    
    $c->forward('getSummaryData');
    $c->forward('getStats');
  }
  
}

#-------------------------------------------------------------------------------
=head2 getSummaryData : Private

Just gets the data items for the overview bar.

=cut

sub getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  #----------------------------------------

  # number of architectures
  my $rs = $c->model( 'PfamDB::Pfamseq' )
             ->find( { ncbi_code  => $c->stash->{proteomeSpecies}->ncbi_code,
                       genome_seq => 1 },
                     { select     => [
                                       {
                                         count => [
                                                    { distinct => [ qw( auto_architecture ) ] }
                                                  ]
                                       }
                                     ],
                       as         => [ qw( numArch ) ] } );

  $summaryData{numArchitectures} = $rs->get_column( 'numArch' );

  #----------------------------------------

  # number of sequences
  $summaryData{numSequences} = $c->stash->{proteomeSpecies}->total_genome_proteins;

  #----------------------------------------

  # number of structures
  $rs = $c->model( 'PfamDB::Pdb_pfamA_reg' )
          ->find( { 'pfamseq.ncbi_code'  => $c->stash->{proteomeSpecies}->ncbi_code,
                    'pfamseq.genome_seq' => 1 },
                    { select => [
                                  {
                                    count => [
                                               { distinct => [ qw( auto_pdb ) ] }
                                             ]
                                  }
                                ],
                      as     => [ qw( numPdb ) ],
                      join   => [ qw( pfamseq ) ] } );

  $summaryData{numStructures}  = $rs->get_column( 'numPdb' );

  #----------------------------------------

  # number of species. As we are dealing with a proteome, this is 1
  $summaryData{numSpecies} = 1;

  #----------------------------------------

  # number of interactions - For now set to zero
  $summaryData{numInt} = 0;

  #----------------------------------------

  $c->stash->{summaryData} = \%summaryData;
}

#-------------------------------------------------------------------------------

=head2 getStats : Private

Just gets the data items for the stats Page. This is really quick

=cut

sub getStats : Private {
  my( $this, $c ) = @_;
  $c->log->debug( 'Proteome::getStatus: getting domain statistics...' );

  my @rs = $c->model("PfamDB::Proteome_seqs")
             ->search( { ncbi_code => $c->stash->{proteomeSpecies}->ncbi_code },
                       { join      => [ qw( pfam ) ],
                         select    => [ qw( pfam.pfamA_id
                                            pfam.pfamA_acc
                                            pfam.description
                                            me.auto_pfamA ), 
                                        { count => 'auto_pfamseq' }, 
                                        { sum   => 'me.count' } ],
                         as        => [ qw( pfamA_id 
                                            pfamA_acc 
                                            description 
                                            auto_pfamA 
                                            numberSeqs 
                                            numberRegs ) ],
                         group_by => [ qw( me.auto_pfamA ) ],
                         order_by => \'sum(me.count) DESC', 
                         #prefetch => [ qw( pfam ) ]
                       }
                     );
  $c->log->debug( 'Proteome::getStats: found |' . scalar @rs . '| rows' );

  $c->stash->{statsData} = \@rs;
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

