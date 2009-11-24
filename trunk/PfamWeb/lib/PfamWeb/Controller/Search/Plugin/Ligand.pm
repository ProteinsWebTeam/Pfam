
# Ligand.pm
# pg6 20091123 WTSI
# 
# $Id: Ligand.pm,v 1.1 2009-11-24 16:39:48 pg6 Exp $

=head1 Name

iPfamWeb::Controller::Searches::Plugin::Ligand - Search plugin for the ligand table.

=cut

package iPfamWeb::Controller::Search::Plugin::Ligand;

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

$Id: Ligand.pm,v 1.1 2009-11-24 16:39:48 pg6 Exp $

=cut

use strict;
use warnings;
use Data::Dump qw( dump );
use base 'iPfamWeb::Controller::Search';

=head1 Methods

=head2 process : Private 

performs the MySQL 'full text' search

=cut

sub process : Private {
  my ( $self, $c ) = @_;
  
  $c->log->debug( 'Search::Plugin::Ligand::process: text querying table ligandChemistry' )
    if $c->debug;
 
  my $rs = $c->model( 'iPfamDB::LigandChemistry' )
             ->search()
             ->search_literal( 'MATCH( name, systematic_name, category, formula ) '.
                               'AGAINST(? IN BOOLEAN MODE)',
                               $c->stash->{ terms }
                             );
  my $results = [];
  while( my $r = $rs->next ){
    push @$results, {
      'acc'               =>  $r->get_column( 'three_letter_code' ),
      'name'              =>  $r->get_column( 'name' ),
      'lig_code'          =>  $r->get_column( 'lig_code' ),
      'type'              =>  'ligand'
    };
  } # end of while 
  #$c->log->debug( "Search::Plugin::process:dump of the results is ".dump( $results ) );
  $c->log->debug( "the size of the resutls is ".scalar( @$results ) );  
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