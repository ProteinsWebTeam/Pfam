
# Keyword.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Search::Keyword - role containing keyword search-related actions

=cut

package RfamWeb::Roles::Search::Keyword;

=head1 DESCRIPTION

A role to add keyword search-related methods to the main search controller.

Methods in this role read the terms that they need to search from the stash
key "rawQueryTerms" and return their results as a DBIC L<ResultSet>. Methods
are enabled in the config:

  <Controller Search>
    # these are the search "plugins" that can be called by the search system
    <plugins>
      Rfam       "Text fields for Rfam entries"
      Wikipedia  "Wikipedia annotations"
      Literature "Literature references"
      Pdb        "PDB structures"
      Clan       "Clan details"
    </plugins>

    # these are sets of searches. They can be used to group searches, so that, 
    # for example the "textSearches" list can contain those search methods that
    # perform fulltext queries
    <searchSets>
      # full text queries
      textSearches Rfam
      textSearches Wikipedia
      textSearches Literature
      textSearches Pdb
      textSearches Clan
    </searchSets>
  </Controller>

The labels specified in the "plugins" list are converted into action names
something like:

  my $method_name = 'process_' . lc $search_name;

and L<forward>ed to in the order given in the config.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use Data::Dump qw( dump );

with 'PfamBase::Roles::Search::Keyword';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process_pdb : Private

A simple search of the "pdb_rfam_reg" table.

=cut

sub process_pdb : Private {
  my( $this, $c ) = @_;

  # use a slightly different regex to convert the raw query terms into the 
  # true search terms
  $c->stash->{terms} = ( split /\s+|\W|\_/, $c->stash->{rawQueryTerms} )[0];

  $c->log->debug( 'Roles::Search::Keyword::process_pdb: querying PDB ID' )
    if $c->debug;

  my $results = $c->model('RfamDB::PdbRfamReg')
                  ->search( { pdb_id => $c->stash->{terms} },
                            { join => [ 'auto_rfam' ] } );

  return $results;
}

#-------------------------------------------------------------------------------

=head2 process_wikipedia : Private

Keyword search the wikipedia text.

=cut

sub process_wikipedia : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
     join " ", map { $_ = "+$_*" } split /[^A-Za-z0-9_-]/, $c->stash->{rawQueryTerms};  

  $c->log->debug( 'Roles::Search::Keyword::process_wikipedia: text querying Rfam wikipedia annotations' )
    if $c->debug;

  my $results = $c->model('RfamDB::RfamKeywords')
                  ->search( {},
                            {} )
                  ->search_literal( 'MATCH( wiki ) ' .
                                    'AGAINST( ? IN BOOLEAN MODE )',
                                    $c->stash->{terms} );

  return $results;
}

#-------------------------------------------------------------------------------

=head2 process_rfam : Private

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process_rfam : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
     join " ", map { $_ = "+$_*" } split /[^A-Za-z0-9_-]/, $c->stash->{rawQueryTerms};  

  $c->log->debug( 'Roles::Search::Keyword::process_rfam: text querying Rfam-specific data using: |' .
                  $c->stash->{terms} . '|' ) if $c->debug;

  my $m = $c->model('RfamDB::RfamKeywords');

  # do a full blown query...
  my $results =
    $m->search( {},
                {} )
      ->search_literal( 'MATCH( rfam_acc, rfam_id, description, rfam_general ) ' .
                        'AGAINST( ? IN BOOLEAN MODE )',
                        $c->stash->{terms} );

  return $results;
}

#-------------------------------------------------------------------------------

=head2 process_literature : Private

Executes the query. Performs a fulltext query on the pfamA table,
searching the accession, ID, description, comment and "previous_id"
columns.

=cut

sub process_literature : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
     join " ", map { $_ = "+$_*" } split /[^A-Za-z0-9_-]/, $c->stash->{rawQueryTerms};  

  $c->log->debug( 'Roles::Search::Keyword::process_literature: text querying Rfam literature' )
    if $c->debug;

  my $m = $c->model('RfamDB::RfamKeywords');

  # do a full blown query...
  my $results =
    $m->search( {},
                {} )
      ->search_literal( 'MATCH( literature ) ' .
                        'AGAINST( ? IN BOOLEAN MODE )',
                        $c->stash->{terms} );

  return $results;
}

#-------------------------------------------------------------------------------

=head2 process_clan : Private

Executes the query. Performs a fulltext query on the clans table,
searching the accession, ID, description, comment and author
columns.

=cut

sub process_clan : Private {
  my( $this, $c ) = @_;

  $c->stash->{terms} =
     join " ", map { $_ = "+$_*" } split /[^A-Za-z0-9_-]/, $c->stash->{rawQueryTerms};  

  $c->log->debug( 'Roles::Search::Keyword::process_clan: text querying Rfam clan' )
    if $c->debug;

  # my $m = $c->model('RfamDB::Clans');

  # # do a full blown query...
  # my $results =
  #   $m->search( {},
  #               {} )
  #     ->search_literal( 'MATCH( literature ) ' .
  #                       'AGAINST( ? IN BOOLEAN MODE )',
  #                       $c->stash->{terms} );

  # return $results;
}

#-------------------------------------------------------------------------------

=head2 lookup_term : Private

Does a quick look-up to see if the search term matches a Pfam ID
or accession.

=cut

sub lookup_term : Private {
  my ( $this, $c ) = @_;

   my $rs = $c->model('RfamDB::Rfam')
              ->search( [ { rfam_acc => $c->stash->{rawQueryTerms} },
                          { rfam_id  => $c->stash->{rawQueryTerms} } ] );

  # we're going to assume that there's only one hit here... we're in
  # trouble if there's more than one, certainly
  my $hit = $rs->next;
  $c->stash->{lookupHit} = $hit if $hit;
}

#-------------------------------------------------------------------------------

=head2 merge

Merges results from plugins. Merging requires that each row of the C<ResultSet>
has access to an Rfam accession. We'll try to get it using

  $rs->rfam_acc

or

  $rs->auto_rfam->rfam_acc

but if neither method works, the row is skipped.

=cut

sub merge : Private {
  my ( $this, $c, $pluginName, $rs ) = @_;

  ROW: while ( my $row = $rs->next ) {

    my ( $acc, $hit );

    TRY: {

      # first try accessing the accession on the table row directly
      eval {
        $acc = $row->rfam_acc;
      };
      if ( $@ ) {
        $c->log->debug( 'Search::Keyword::merge: caught an exception when trying '
                       . " \$row->rfam_acc for plugin |$pluginName|: $@" )
          if $c->debug;
      }

      if ( defined $acc ) {
        $hit = $c->stash->{results}->{$acc} ||= {};
        $hit->{dbObj} = $row;
        last TRY;
      }

      # we couldn't find the accession on the row itself, so try walking down one
      # possible relationship, "auto_rfam", and see if that gets us to another
      # table, which hopefully does store the details of the Rfam family
      #
      # This is all a bit convoluted, but it means that we shouldn't need to add
      # proxy columns indiscriminately throughout the model, just so that text
      # searching can work
      eval {
        $acc = $row->auto_rfam->rfam_acc;
      };
      if ( $@ ) {
        $c->log->debug( 'Search::Keyword::merge: caught an exception when trying '
                       . " \$row->auto_rfam->rfam_acc for plugin |$pluginName|: $@" )
          if $c->debug;
      }

      if ( defined $acc ) {
        $hit = $c->stash->{results}->{$acc} ||= {};
        $hit->{dbObj} = $row->auto_rfam;
        # last TRY;
      }

    }

    $hit->{query}->{$pluginName} = 1;
    $c->stash->{pluginHits}->{$pluginName} += 1;

    # score this hit
    $this->_computeScore( $c, $hit );
  }
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012 Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk)

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
