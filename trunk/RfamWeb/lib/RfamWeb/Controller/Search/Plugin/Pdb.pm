
# Pdb.pm
# jt6 20081210 WTSI
#
# $Id: Pdb.pm,v 1.1 2009-01-06 11:32:43 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Searches::pdb - search plugin for PDB IDs

=cut

package RfamWeb::Controller::Search::Plugin::Pdb;

=head1 DESCRIPTION

Performs a simple search against the "pdb_rfam_reg" table, looking for the 
supplied PDB ID.

$Id: Pdb.pm,v 1.1 2009-01-06 11:32:43 jt6 Exp $

=cut

# based loosely on this original query:
# SELECT *
# FROM   rfam r, 
#        pdb_rfam_reg p
# WHERE  r.auto_rfam = p.auto_rfam
# AND    pdb_id=?;

use strict;
use warnings;

use base 'RfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

A simple search of the "pdb_rfam_reg" table.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Plugin::Pdb::process: querying PDB ID' )
    if $c->debug;

  my $results = $c->model('RfamDB::PdbRfamReg')
                  ->search( { pdb_id => $c->stash->{terms} },
                            { join => [ 'auto_rfam' ] } );

  return $results;
}

#-------------------------------------------------------------------------------

=head2 formatTerms : Private

Keep only the first query term and don't add the usual operators that are needed 
by boolean searches.

=cut

sub formatTerms : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
    ( split /\s+|\W|\_/, $c->stash->{rawQueryTerms} )[0];

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
