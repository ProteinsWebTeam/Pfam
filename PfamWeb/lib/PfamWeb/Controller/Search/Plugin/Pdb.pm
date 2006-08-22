
# Pdb.pm
# jt6 20060810 WTSI
#
# $Id: Pdb.pm,v 1.1 2006-08-22 15:16:23 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Pdb - search plugin for the pdb table

=cut

package PfamWeb::Controller::Search::Plugin::Pdb;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the pdbmap table, on the
following columns:

=over

=item o pdb_id

=item o header

=item o title

=back

This query requires an explicit join against the pfamA table, so that
we can retrieve pfam accession, ID and description from there, and
another join against the pdb table for the actual text-query against
the PDB ID.

$Id: Pdb.pm,v 1.1 2006-08-22 15:16:23 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Search";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::Plugin::Pdb::process: text querying table pdb" );

  my $results = $c->model("PfamDB::PdbMap")
	->search(
			 {
			  pfam_region => 1
			 },
			 {
			  join        => [ qw/pfamA pdb/ ],
			  prefetch    => [ qw/pfamA/ ]
			 }
			)
	  ->search_literal( "MATCH( pdb_id, header, title ) " .
						"AGAINST( ? IN BOOLEAN MODE )",
						$c->stash->{terms} );

  return $results;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
