
# GO.pm
# jt6 20060816 WTSI
#
# $Id: GO.pm,v 1.1 2006-08-22 15:16:22 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Plugin::GO - search plugin for the gene_ontology table

=cut

package PfamWeb::Controller::Search::Plugin::GO;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the gene_ontology table, on the
following columns:

=over

=item o go_id

=item o term

=back

There's an explicit join against the pfamA table, so that we can
retrieve pfamA accession, ID and description.

$Id: GO.pm,v 1.1 2006-08-22 15:16:22 jt6 Exp $

=cut

# based loosely on this original query:
#SELECT category,
#       term,
#       go_id
#FROM   pfamA AS p,
#       gene_ontology AS go
#WHERE  p.pfamA_acc = ?
#AND    p.auto_pfamA = go.auto_pfamA

use strict;
use warnings;

use base "PfamWeb::Controller::Search";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::Plugin::GO::process: text querying table gene_ontology" );

  my $results = $c->model("PfamDB::GO")
	->search(
			 {},
			 {
			  join     => [ qw/pfamA/ ] ,
			  prefetch => [ qw/pfamA/ ]
			 }
			)
	  ->search_literal( "MATCH( go_id, term ) " .
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
