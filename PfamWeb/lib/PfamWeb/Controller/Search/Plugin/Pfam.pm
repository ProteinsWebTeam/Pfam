
# Pfam.pm
# jt6 20060810 WTSI
#
# $Id: Pfam.pm,v 1.2 2006-10-03 14:31:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Plugin::Pfam - search plugin for the pfamA table

=cut

package PfamWeb::Controller::Search::Plugin::Pfam;

=head1 DESCRIPTION

Performs a MySQL "fulltext" query of the pfamA table, searching
against the following columns:

=over

=item o pfamA_acc

=item o pfamA_id

=item o description

=item o comment

=item o description

=item o previous_id

=back

$Id: Pfam.pm,v 1.2 2006-10-03 14:31:28 jt6 Exp $

=cut

# based loosely on this original query:
# SELECT pfamA_acc   AS acc,
#        pfamA_id    AS id,
#        description AS descr
# FROM   pfamA
# WHERE  MATCH( pfamA_acc, pfamA_id, description, comment, previous_id )
# AGAINST( ? IN BOOLEAN MODE )

use strict;
use warnings;

use base "PfamWeb::Controller::Search";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::Plugin::Pfam::process: text querying table pfamA" );

  my $m = $c->model("PfamDB::Pfam");

  # do a full blown query...
  my $results =
	$m->search(
			   {},
			   {}
			  )
	  ->search_literal( "MATCH( pfamA_acc, pfamA_id, description, comment, previous_id ) " .
						"AGAINST( ? IN BOOLEAN MODE )",
						$c->stash->{terms} );

  # do a simple lookup for the ID or accession...
  my $lookup =
	$m->search( [ { pfamA_acc => $c->stash->{rawQueryTerms} },
				  { pfamA_id  => $c->stash->{rawQueryTerms} } ] );

  return $results, $lookup;
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
