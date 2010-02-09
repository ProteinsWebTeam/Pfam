
# Ligand.pm
# pg6 20091123 WTSI
# 
# $Id: Ligand.pm,v 1.1 2009-11-27 11:40:34 pg6 Exp $

=head1 Name

iPfamWeb::Controller::Searches::Keyword::Ligand - Search Keyword for the ligand table.

=cut

package iPfamWeb::Controller::Search::Keyword::Ligand;

=head1 Description


Performs a MySQL "fulltext" query of the ligand_chemistry table, on the
following columns:

=over

=item o three_letter_code

=item o name

=item o systematic_name

=item o category

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: Ligand.pm,v 1.1 2009-11-27 11:40:34 pg6 Exp $

=cut

use strict;
use warnings;
use Data::Dump qw( dump );
use base 'iPfamWeb::Controller::Search::Keyword';

=head1 Methods

=head2 process : Path 

performs the MySQL 'full text' search

=cut

sub process : Path {
  my ( $self, $c ) = @_;
  
  $c->log->debug( 'Search::Keyword::Ligand::process: text querying table ligandChemistry' )
    if $c->debug;
 
  my $rs = $c->model( 'iPfamDB::LigandChemistry' )
             ->search()
             ->search_literal( 'MATCH( name, systematic_name, category, formula ) '.
                               'AGAINST(? IN BOOLEAN MODE)',
                               $c->stash->{ terms }
                             );
  my $results = [];my $seen = {};
  my $total_hits = 0;
  while(my $r = $rs->next ){
    
    #increment the hits coutner when we see the results;
    $total_hits++;
    
    my $acc   = $r->get_column( 'three_letter_code' );
    next if( exists $seen->{ $acc } ); 
    
    my $desc = $r->get_column( 'name');
    push @$results, {
      'acc'               =>  $acc,
      'name'              =>  $desc,
      'lig_code'          =>  $r->get_column( 'lig_code' ),
      'formula'           =>  $r->get_column( 'formula'),
      'type'              =>  'ligand'
    };
    $seen->{ $acc }++;
  } # end of while 
  $c->stash->{ hits } = $total_hits;
  $c->log->debug( "Search::Keyword::ligand::process: the total number of results are  ".scalar( @$results ) );
  $c->stash->{ results } = $results;
  $c->stash->{ template } = 'components/blocks/search/keyword/ligand.tt';                            
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