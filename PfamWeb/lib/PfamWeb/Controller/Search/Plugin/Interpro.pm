
# Interpro.pm
# jt6 20060816 WTSI
#
# $Id: Interpro.pm,v 1.1 2006-08-22 15:16:22 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Plugin::Pfam - search plugin for the interpro table

=cut

package PfamWeb::Controller::Search::Plugin::Interpro;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the interpro gene_ontology table, on the
following columns:

=over

=item o interpro_id

=item o abstract

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: Interpro.pm,v 1.1 2006-08-22 15:16:22 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Search";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

Executes the query.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::Plugin::Interpro::process: text querying table interpro" );

  my $results = $c->model("PfamDB::Interpro")
	->search(
			 {},
			 {
			  join     => [ qw/pfamA/ ],
			  prefetch => [ qw/pfamA/ ]
			 }
			)
	  ->search_literal( "MATCH( interpro_id, abstract ) " .
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
