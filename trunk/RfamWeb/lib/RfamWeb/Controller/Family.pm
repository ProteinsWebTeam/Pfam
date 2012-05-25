
# Family.pm
# jt6 20080306 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Controller::Family - controller to build the main Rfam family page

=cut

package RfamWeb::Controller::Family;

=head1 DESCRIPTION

This is the controller for everything related to Rfam families.

Generates a B<tabbed page>.

$Id$

=cut

use Moose;
use namespace::autoclean;

use MIME::Base64;
use JSON;
use Data::Dump qw( dump );

BEGIN {
  extends 'Catalyst::Controller::REST';
}

# roles that add the bulk of the functionality to this controller. When applying
# the "Section" role from PfamBase, we specifically exclude the "section" action,
# otherwise the "/family?acc=blah" redirect action doesn't get registered.
with 'PfamBase::Roles::Section' => { -excludes => 'section' },
     'RfamWeb::Roles::Family::Methods',
     'RfamWeb::Roles::Family::TreeMethods',
     'RfamWeb::Roles::Family::AlignmentMethods',
     'RfamWeb::Roles::Family::StructureMethods';

# set up the list of content-types that we handle via REST
__PACKAGE__->config(
  'default' => 'text/html',
  'map'     => {
    'text/html'        => [ 'View', 'TT' ],
    'text/xml'         => [ 'View', 'TT' ],
    'text/plain'       => [ 'View', 'TT' ],
    'application/json' => 'JSON',
  }
);

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Extracts values from the parameters. Accepts "acc", "id" and "entry", in lieu
of having them as path components.

=cut

# using "after" because there's a begin method in the C::C::REST controller that
# we're extending
after 'begin' => sub {
  my ( $this, $c ) = @_;
  
  # get a handle on the entry, if supplied as params, and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      '';
  
  if ( $tainted_entry ) {
    $c->log->debug( 'Family::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w-]+)$/;
  }

  # should we use the live or release databases ?
  if ( $c->req->param('live') ) {
    $c->log->debug( 'Family::begin: using "live" database' )
      if $c->debug;
    $c->stash->{live} = 1;
    $c->stash->{db} = $c->model('RfamLive');
  }
  else {
    $c->log->debug( 'Family::begin: using "release" database' )
      if $c->debug;
    $c->stash->{db} = $c->model('RfamDB');
  }
};

#-------------------------------------------------------------------------------

=head2 family : Chained('/') PathPart('family') CaptureArgs(1)

Mid-point of a chain handling family-related data. Retrieves family information
from the DB.

=cut

sub family : Chained( '/' )
             PathPart( 'family' )
             CaptureArgs( 1 ) {

  my ( $this, $c, $entry_arg ) = @_;

  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
                      '';
  
  $c->log->debug( "Family::family: tainted_entry: |$tainted_entry|" )
    if $c->debug;

  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w-]+)$/;
    $c->stash->{rest}->{error} = 'Invalid Rfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{rest}->{error} = 'No Rfam family accession or ID specified';
  }

  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------

=head2 family_page : Chained('family') PathPart('') Args(0) ActionClass('REST')

End-point of a chain handling family data. Generates a family page (HTML) 
or an XML output. Implemented using C::C::REST; accepts GET requests only.

=cut

sub family_page : Chained( 'family' )
                  PathPart( '' )
                  Args( 0 )
                  ActionClass( 'REST::ForBrowsers' ) { }

# RESTful interface: builds a family page (HTML).
sub family_page_GET_html : Private {
  my ( $this, $c ) = @_;

  # fail fast if there was a problem retrieving family data
  unless ( $c->stash->{rfam} ) {
    $c->res->status(500); # Internal server error

    $c->stash->{rest}->{error} ||= 'We could not find the data for Rfam family ' . $c->stash->{acc} . '.';
    $c->stash->{template} = 'components/blocks/family/error.tt';

    return;
  }

  # load the data for all regions, provided the number of regions is less than
  # the limit set in the config
  if ( $c->stash->{rfam}->num_full <= $this->{regionsLimits}->{showAll} ) {
    $c->log->debug( 'Family::family_page: num_full <= showAll limit; retrieving regions' )
      if $c->debug;
    $c->stash->{showAll} = 1;
    $c->forward( 'get_regions_data' );
  }
  elsif ( $c->stash->{rfam}->num_full <= $this->{regionsLimits}->{showText} ) {
    $c->log->debug( 'Family::family_page: num_full <= showText limit; retrieving regions later' )
      if $c->debug;
    $c->stash->{showText} = 1;
  }

  # add the clan details, if any
  my $clan = $c->stash->{db}->resultset('Clans')
               ->search( { 'clan_memberships.auto_rfam' => $c->stash->{rfam}->auto_rfam },
                         { prefetch => [ qw(clan_memberships) ] } )
               ->first;
  
  if ( $clan and defined $clan->clan_acc ) {
    $c->log->debug( 'Family::family_page: adding clan info' ) if $c->debug;
    $c->stash->{clan} = $clan;
  }

  $c->cache_page( 43200 ); # cache for 12 hours
  
  #---------------------------------------

  $c->log->debug( 'Family::family_page: adding summary info' ) 
    if $c->debug;
  $c->forward( 'get_summary_data' );

  $c->log->debug( 'Family::family_page: adding wikipedia info' ) 
    if $c->debug;
  $c->forward( 'get_wikipedia' );

  #---------------------------------------

  $c->log->debug( 'Family::family_page: emitting HTML' )
    if $c->debug;

  $c->stash->{pageType} = 'family';
  $c->stash->{template} = 'pages/layout.tt';
}

#---------------------------------------

# RESTful interface: emits family data in non-HTML format.
sub family_page_GET : Private {
  my ( $this, $c ) = @_;

  # there was a problem retrieving family data
  unless ( $c->stash->{rfam} ) {
    $c->log->debug( 'Family::family_page_GET: problem retrieving family data' ) 
      if $c->debug;

    $c->stash->{rest}->{error} ||= 'Could not retrieve family data.';
    $c->stash->{template} = 'rest/family/error_xml.tt';

    return;
  }

  # for XML output...
  if ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' ) {

    $c->log->debug( 'Family::family_page_GET: emitting XML' ) 
      if $c->debug;

    $c->stash->{template} = $c->stash->{rfam}
                          ? 'rest/family/rfam.tt'
                          : 'rest/family/error_xml.tt';
  }
  else {

    $c->log->debug( 'Family::family_page_GET: emitting something other than XML or HTML' ) 
      if $c->debug;

    # populate a data structure with family data that should go into, say, a JSON
    # rendering of the home page data
    my $r = $c->stash->{rfam}; # shortcut...
    my $data = {
      rfam => {
        release => {
          number => $c->stash->{relData}->rfam_release,
          date => $c->stash->{relData}->rfam_release_date,
        },
        id => $r->rfam_id,
        acc => $r->rfam_acc,
        description => $r->description,
        comment => $r->comment,
        clan => { 
          id => undef,
          acc => undef,
        },
        curation => {
          author => $r->author,
          seed_source => $r->seed_source,
          num_seed => $r->num_seed,
          num_full => $r->num_full,
          num_species => $r->number_of_species,
          type => $r->type,
          structure_source => $r->structure_source,
        },
        cm => {
          build_command => $r->cmbuild,
          calibrate_command => $r->cmcalibrate,
          search_command => $r->cmsearch,
          cutoffs => {
            gathering => $r->gathering_cutoff,
            trusted => $r->trusted_cutoff,
            noise => $r->noise_cutoff,
          }
        }
      }
    };

    # TODO populate the clan data...

    # report success.
    $this->status_ok(
      $c,
      entity => $data
    );

  }
}

#---------------------------------------

=head2 old_family : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_family : Path( '/family' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_family: redirecting to "family"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  if ( $c->stash->{param_entry} ) {
    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, $c->req->params ) );
  }
  else {
    $c->log->debug( 'Family::old_family: no entry specified' ) 
      if $c->debug;

    $c->forward( 'families' );
  }
}

#-------------------------------------------------------------------------------

=head2 families : Chained('/') PathPart('family') Args(0)

Returns the list of all families.

=cut

# sub families : Chained( '/' )
#                PathPart( 'family' )
#                Args( 0 ) {
#   my ( $this, $c ) = @_;
# 
#   my @families = $c->stash->{db}->resultset('Rfam')
#                    ->search( {}, {} );
# 
#   # map { push @{ $c->stash->{rest}->{families} }, $_->rfam_acc } @families;
# }

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves family data for the given entry. Accepts the entry ID or accession as
the first argument. Does not return any value but drops the L<ResultSet> for
the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # check for a family
  my $rfam = $c->stash->{db}->resultset('Rfam')
               ->search( [ { rfam_acc => $entry },
                           { rfam_id  => $entry } ] )
               ->single;
                         
  unless ( defined $rfam ) {
    $c->log->debug( 'Family::get_data: no row for that accession/ID' )
      if $c->debug;
  
    $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                          ? 'rest/family/error_xml.tt'
                          : 'components/blocks/family/error.tt';

    $this->status_not_found( $c, message => 'No valid Rfam family accession or ID' );

    return;
  }  

  $c->log->debug( 'Family::get_data: got a family' )
    if $c->debug;

  $c->stash->{rfam}      = $rfam;
  $c->stash->{acc}       = $rfam->rfam_acc;
  $c->stash->{entryType} = 'R';
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the Rfam object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $summaryData = {};

  # number of sequences in full alignment
  $summaryData->{numSequences} = $c->stash->{rfam}->num_full;

  # number of structures known for the domain
  my $rs = $c->stash->{db}->resultset('PdbRfamReg')
             ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam },
                       {} );

  $summaryData->{numStructures} = $rs->count;

  # Number of species
  $summaryData->{numSpecies} = $c->stash->{rfam}->number_of_species;

  # number of interactions
  $summaryData->{numInt} = 0;

  $c->stash->{summaryData} = $summaryData;

  # get tree curation data. Limit retrieved columns to avoid pulling down
  # alignments and tree data until we really need them.
  my $full_tree_data = $c->stash->{db}->resultset( 'AlignmentsAndTrees' )
                         ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                                   type      => 'full' },
                                 { columns => [ qw{ type 
                                                    treemethod 
                                                    average_length 
                                                    percent_id 
                                                    number_of_sequences 
                                                    most_unrelated_pair } ] } );

  my $seed_tree_data = $c->stash->{db}->resultset( 'AlignmentsAndTrees' )
                         ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                                   type      => 'seed' },
                                 { columns => [ qw{ type 
                                                    treemethod 
                                                    average_length 
                                                    percent_id 
                                                    number_of_sequences 
                                                    most_unrelated_pair } ] } );

  $c->stash->{alignment_info}->{full} = $full_tree_data;
  $c->stash->{alignment_info}->{seed} = $seed_tree_data;
}

#-------------------------------------------------------------------------------

=head2 get_regions_data : Private

Retrieves sequence data for the family.

=cut

sub get_regions_data : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::get_regions_data: family has |'
                  . $c->stash->{rfam}->num_full . '| regions' ) if $c->debug;

  my @regions = $c->stash->{db}->resultset('RfamRegFull')
                  ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam },
                            { join      => { 'auto_rfamseq' => 'ncbi_id' },
                              '+select' => [ qw( auto_rfamseq.rfamseq_acc
                                                 auto_rfamseq.description
                                                 ncbi_id.species
                                                 ncbi_id.ncbi_id
                                                 auto_rfamseq.length ) ],
                              '+as'     => [ qw( rfamseq_acc
                                                 description
                                                 species
                                                 ncbi_taxid
                                                 length ) ],
                              order_by  => [ 'bits_score DESC' ] } );
                                            
  $c->stash->{region_rows} = \@regions;

  $c->log->debug( 'Family::get_regions_data: added |' . scalar @regions
                  . '| regions to stash' ) if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_wikipedia : Private

Retrieves the wikipedia content, if any, for this family.

=cut

sub get_wikipedia : Private {
  my ( $this, $c ) = @_;

  my $article = $c->model('WebUser::ArticleMapping')
                  ->search( { accession => $c->stash->{acc} },
                            { join     => [ 'wikitext' ],
                              prefetch => [ 'wikitext' ] } )
                  ->next;
  
  return unless ( $article and $article->wikitext );

  # $c->log->debug( 'Family::get_wikipedia: got wiki title: |'
  #                 . $article->title . '|' )
  #   if $c->debug;

  $c->stash->{article} = $article;
}

#-------------------------------------------------------------------------------  

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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

__PACKAGE__->meta->make_immutable;

1;

