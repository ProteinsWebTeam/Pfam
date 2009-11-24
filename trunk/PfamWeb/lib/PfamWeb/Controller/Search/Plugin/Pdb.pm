
# Pdb.pm
# pg6 20091123 WTSI
# 
# $Id: Pdb.pm,v 1.8 2009-11-24 16:39:48 pg6 Exp $

=head1 Name

iPfamWeb::Controller::Searches::Plugin::Pdb - Search plugin for the PDB table.

=cut

package iPfamWeb::Controller::Search::Plugin::Pdb;

=head1 Description


Performs a MySQL "fulltext" query of the pdb table, on the
following columns:

=over

=item o pdb_id

=item o header

=item o title

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: Pdb.pm,v 1.8 2009-11-24 16:39:48 pg6 Exp $

=cut

use strict;
use warnings;
use Data::Dump qw( dump );

use base 'iPfamWeb::Controller::Search';

=head1 Methods

=head2 process : Private 

performs the MySQL 'full text' search

=cut
#
#sub process: Private {
#  my( $self, $c ) = @_;
#  
#  $c->log->debug( 'Search::Plugin::Pdb::process: text querying table pdb' )
#    if $c->debug;
#  
#  # first get the list of pdb accessions for the input search term
#  my $rs = $c->model( 'iPfamDB::Pdb' )
#             ->search()
#             ->search_literal( 'MATCH( pdb_id, header, title ) ' .
#                               'AGAINST( ? IN BOOLEAN MODE )',
#                               $c->stash->{terms} );
#  my $results = [];
#  
#  while (my $r  = $rs->next ){
#    my $pdb_id  = $r->get_column( 'pdb_id' );
#    my $desc    = $r->get_column( 'header' ); 
#    my $rs1 = $c->model( 'iPfamDB::PdbChainData')->search( { 'pdb_id'  =>  $pdb_id } );
#    my $seen    = {};
#    
#    while( my $protein = $rs1->next ){
#      unless( $seen->{ $protein } ){
#        
#        push @$results, {
#          'acc'         =>  $pdb_id,
#          'protein_id'  =>  $protein->get_column( 'internal_chain_accession' ),
#          'type'        =>  'structure',
#          'desc'        =>  $desc
#        }; 
#        $seen->{ $protein }++;     
#      } # end of unless $acc
#    } # end of while
#  } # end of pdb
#  
#  #$c->log->debug( "Search::Plugin::process:dump of the results is ".dump( $results ) );
#  $c->log->debug( "the size of the results is ".scalar( @$results ) );
#  return $results;
#}

sub process: Private {
  my( $self, $c ) = @_;
  
  $c->log->debug( 'Search::Plugin::Pdb::process: text querying table pdb' )
    if $c->debug;
  
  # first get the list of pdb accessions for the input search term
  my $rs = $c->model( 'iPfamDB::PdbChainData' )
             ->search({},
                      { prefetch  =>  [ qw/pdbtable/ ]}  )
             ->search_literal( 'MATCH( pdbtable.pdb_id, pdbtable.header, pdbtable.title ) ' .
                               'AGAINST( ? IN BOOLEAN MODE )',
                               $c->stash->{terms} );
  my $results = [];
  
  while (my $r  = $rs->next ){
    my $pdb_id  = $r->pdbtable->get_column( 'pdb_id' );
    my $desc    = $r->pdbtable->get_column( 'header' ); 
    my $protein = $r->get_column( 'internal_chain_accession' );
    $c->log->debug( "Search::Plugin::Pdb:process: the protein id is $protein");
    my $seen    = {};
    
      unless( $seen->{ $protein } ){
        
        push @$results, {
          'acc'         =>  $pdb_id,
          'protein_id'  =>  $protein,
          'type'        =>  'structure',
          'desc'        =>  $desc
        }; 
        $seen->{ $protein }++;     
      } # end of unless $acc
  } # end of pdb
  
  #$c->log->debug( "Search::Plugin::process:dump of the results is ".dump( $results ) );
  $c->log->debug( "the size of the results is ".scalar( @$results ) );
  return $results;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

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