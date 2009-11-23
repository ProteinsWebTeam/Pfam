
# Structure.pm
# jt6 20060706 WTSI
#
# $Id: Structure.pm,v 1.21 2009-11-23 13:05:49 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure - controller for structure-related
sections of the site

=cut

package PfamWeb::Controller::Structure;

=head1 DESCRIPTION

This is intended to be the base class for everything related to 3-D
structure across the site. The L<begin|/"begin : Private"> method will
try to extract a PDB ID from the captured URL and then try to load a
Pdb object from the model into the stash.

This is also the controller that handles the structure section of the
site, so it includes an action to capture a URL like

=over

=item http://localhost:3000/structure?id=1abc

=back

Generates a B<tabbed page>.

$Id: Structure.pm,v 1.21 2009-11-23 13:05:49 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'structure' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract a PDB ID from the URL and gets the row in the Pdb table
for that entry. Accepts various formats of URL:

=over

=item * http://localhost:3000/structure/I<..>?id=1abc

=item * http://localhost:3000/structure/I<..>?entry=1abc

=item * http://localhost:3000/structure/I<..>/1abc

=item * http://localhost:3000/structure/I<..>/1abc.pdb

=back

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||  
                      $c->req->param('id')    ||  
                      $c->req->param('entry') ||
                      $entry_arg              ||  
                      '';

  # when called from the AstexViewer tool window, the GetPdbFile method
  # will be forced to use a filename rather than a raw PDB ID, i.e. we'll
  # get "1abc.pdb" rather than "1abc", because AstexViewer expects to load
  # a real file. Here we'll just trim off the ".pdb" suffix before trying
  # to detaint
  $tainted_entry =~ s/^(.*?)\.pdb$/$1/;
  
  my $entry;
  if ( $tainted_entry ) { 
    ( $entry ) = $tainted_entry =~ m/^([0-9][A-Z0-9]{3})$/i;
    $c->stash->{errorMsg} = 'Invalid Pfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }
 
  my $pdb = $c->model('PfamDB::Pdb')
              ->search( { pdb_id => $entry } )
              ->single;

  # we're done here unless there's an entry specified
  unless( defined $pdb ) {

    # see if this was an internal link and, if so, report it
    my $b = $c->req->base;
    if( defined $c->req->referer and $c->req->referer =~ /^$b/ ) {
  
      # report the error as a broken internal link
      $c->error( q|Found a broken internal link; no valid PDB ID |
                 . qq|("$entry") in "| . $c->req->referer . q|"| );
      $c->forward( '/reportError' );
  
      $c->clear_errors;
    }
  
    $c->stash->{errorMsg} = 'No valid PDB ID';
  
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn( "Structure::begin: couldn't retrieve data for PDB ID |$entry|" );
  
    return;
  }

  $c->log->debug( "Structure::begin: successfully retrieved pdb object for |$entry|" )
    if $c->debug;

  # stash the PDB object and ID
  $c->stash->{pdb}   = $pdb;
  $c->stash->{pdbId} = $entry;

  # get the icon summary data, but only if we're in this top-level class, 
  # i.e. the one that generates the structure page rather than the sub-classes
  # that build page components
  if( ref $this eq 'PfamWeb::Controller::Structure' ) {
    $c->forward( 'get_summary_data' );
    $c->forward( 'get_authors' );
  }
  
  # add the mapping between structure, sequence and family. We need this for 
  # more or less all of the sub-classes, so always do this
  $c->forward( 'add_mapping' );

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Gets the data items for the overview bar

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $id = $c->stash->{pdb}->pdb_id;

  my $cache_key = "${id}_summary";
  my $summary = $c->cache->get( $cache_key );
  if ( defined $summary ) { 
    $c->log->debug( 'Structure::get_summary_data: retrieved summary from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Structure::get_summary_data: failed to retrieve summary from cache; going to DB' )
      if $c->debug;

    $summary = {};

    # number of sequences in the structure - count the number of chains.
    my $rs = $c->model('PfamDB::PdbResidueData')
               ->search( { pdb_id=> $id},
                         { select   => [
                                         {
                                           count => [ { distinct => [ 'chain' ] } ]
                                         }
                                       ],
                           as       => [ 'numChains' ] } )
               ->single;
    $summary->{numSequences} = $rs->get_column( 'numChains' );
  
    # number of species should be one, but get the species for the sequences
    $rs = $c->model('PfamDB::PdbResidueData')
            ->search( { pdb_id => $id },
                      { join     => [ 'pfamseqs' ],
                        select   => [
                                      {
                                        count => [ { distinct => [ 'pfamseqs.species' ] } ]
                                      }
                                    ],
                        as       => [ 'numSpecies' ] } )
            ->single;
    $summary->{numSpecies} = $rs->get_column( 'numSpecies' );
  
    # number architectures
    $rs = $c->model('PfamDB::PdbResidueData')
            ->search( { pdb_id => $id },
                      { join     => [ 'pfamseqs' ],
                        select   => [
                                      {
                                       count => [ { distinct => [ 'pfamseqs.auto_architecture' ] } ]
                                      }
                                    ],
                        as       => [ 'numArch' ] } )
            ->single;
    $summary->{numArchitectures} = $rs->get_column( 'numArch' );

    # number of interactions.
    #$rs = $c->model('PfamDB::Interactions')
    #        ->find( { auto_pdb => $autoPdb },
    #                { select   => [
    #                                {
    #                                  count => [ { distinct => [ 'auto_int_pfamAs' ] } ]
    #                                }
    #                              ],
    #                  as       => [ qw( numInts ) ] } );
    #$summaryData{numInt} = $rs->get_column( 'numInts' );
    $summary->{numInt} = 0;
  
    # number of structures is one
    $summary->{numStructures} = 1;

    $c->cache->set( $cache_key, $summary ) unless $ENV{NO_CACHE};

    $c->log->debug( "Structure::get_summary_data: caching summary data for $id" )
      if $c->debug;
  }

  $c->stash->{summaryData} = $summary;
}

#-------------------------------------------------------------------------------

=head2 getAuthors : Private

Add the list of authors to the stash.

=cut

sub get_authors : Private {
  my ( $this, $c ) = @_;

  # get the authors list
  my @authors = $c->model('PfamDB::PdbAuthor')
                  ->search( { pdb_id => $c->stash->{pdb}->pdb_id },
                            { order_by => 'author_order ASC' } );

  $c->stash->{authors} = \@authors;
}

#-------------------------------------------------------------------------------

=head2 add_mapping : Private

Adds the structure-to-UniProt mapping to the stash.

=cut

sub add_mapping : Private {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Structure::add_mapping: adding mappings for PDB entry '
          . $c->stash->{pdb}->pdb_id ) if $c->debug;

  # add the structure-to-UniProt mapping to the stash
  my @unpMap = $c->model('PfamDB::PdbPfamaReg')
                 ->search( { pdb_id          => $c->stash->{pdb}->pdb_id, 
                             'pdb_res_start' => { '!=' => 'pdb_res_end' } },
                           { prefetch => [ qw( auto_pfama auto_pfamseq ) ],
                             order_by => 'chain ASC' } );

  $c->log->debug( 'Structure::add_mapping: found ' . scalar @unpMap . ' mappings' )
    if $c->debug;
  $c->stash->{mapping} = \@unpMap;

  # build a little data structure to map PDB chains to uniprot IDs and
  # then cache that for the post-loaded graphics component
  my ( $chains, $chain );
  foreach my $row ( @unpMap ) {
    $chain = ( defined $row->chain ) ? $row->chain : ' ';
    # TODO Need to think more about the consequences of setting null
    # chain ID to " "...
  
    $chains->{$row->auto_pfamseq->pfamseq_id}->{$chain} = '';
  }
  $c->stash->{chainsMapping} = $chains;

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
