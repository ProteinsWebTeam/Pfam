
# Structures.pm
# jt6 20070823 WTSI
#
# $Id: Structures.pm,v 1.5 2009-10-07 10:40:14 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Proteome::Structures - structure/sequence mapping for proteomes

=cut

package PfamWeb::Controller::Proteome::Structures;

=head1 DESCRIPTION

Controller to build the mapping between sequence and structure for the 
proteome section

$Id: Structures.pm,v 1.5 2009-10-07 10:40:14 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Proteome';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 get_mapping : Path

Just gets the data items for the structure mapping table.

=cut

sub get_mapping : Path {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Proteome::get_mapping: getting structure mapping...' )
    if $c->debug;
  
  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { 'auto_pfamseq.ncbi_taxid' => $c->stash->{taxId},
                              'auto_pfamseq.genome_seq' => 1 },
                            { prefetch => [ qw( auto_pfamseq pdb_id ) ] } );
    
  $c->stash->{pfamMaps} = \@mapping;

  # NB ! Handing off to a family template
  $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  
  # cache the template output for one week
  #$c->cache_page( 604800 );
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

