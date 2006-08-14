
# Pdb.pm
# jt6 20060810 WTSI
#
# $Id: Pdb.pm,v 1.1 2006-08-14 10:50:18 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Searches::Pdb - search plugin for the pdb table

=cut

package PfamWeb::Controller::Search::Pdb;

=head1 DESCRIPTION

Search the PDB table

$Id: Pdb.pm,v 1.1 2006-08-14 10:50:18 jt6 Exp $

=cut

# based on this original query:
# SELECT DISTINCT( pf.pfamA_acc ) AS acc,
#        pf.pfamA_id AS id,
#        pf.description AS descr
# FROM   pdb    AS p,
#        pdbmap AS pm,
#        pfamA  AS pf
# WHERE  p.auto_pdb = pm.auto_pdb
# AND    pm.auto_pfam = pf.auto_pfamA
# AND    MATCH( p.pdb_id, p.header, p.title ) AGAINST ( ? IN BOOLEAN MODE )

use strict;
use warnings;

use base "PfamWeb::Controller::Search";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Private

=cut

sub process : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "Search::Pdb::process: text querying table pdb" );

  my $results = $c->model("PfamDB::Pdb")
	->search( {},	{} )
	  ->search_literal( "MATCH( pdb_id, header, title ) " .
						"AGAINST( ? IN BOOLEAN MODE )",
						$c->stash->{terms} );

  $c->stash->{queryResults} = $results;

  $c->log->debug( "Search::Pdb::process: stashed " . $results->count . " results" );
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
