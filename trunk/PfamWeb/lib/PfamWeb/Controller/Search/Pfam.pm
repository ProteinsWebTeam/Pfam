
# Pfam.pm
# jt6 20060810 WTSI
#
# $Id: Pfam.pm,v 1.1 2006-08-14 10:50:18 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Pfam - search plugin for the pfamA table

=cut

package PfamWeb::Controller::Search::Pfam;

=head1 DESCRIPTION

Search the pfamA table

$Id: Pfam.pm,v 1.1 2006-08-14 10:50:18 jt6 Exp $

=cut

# based on this original query:
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

  $c->log->debug( "Search::Pfam::process: text querying table pfamA" );

  my $results = $c->model("PfamDB::Pfam")
	->search( {},	{} )
	  ->search_literal( "MATCH( pfamA_acc, pfamA_id, description, comment, previous_id ) " .
						"AGAINST( ? IN BOOLEAN MODE )",
						$c->stash->{terms} );

  $c->stash->{queryResults} = $results;

  $c->log->debug( "Search::Pfam::process: stashed " . $results->count . " results" );
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
