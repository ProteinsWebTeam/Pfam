
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
use Compress::Zlib;
use Text::Wrap qw( $columns wrap );

$Text::Wrap::columns = 60;

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
     'RfamWeb::Roles::Family::StructureMethods',
     'PfamBase::Roles::SunburstMethods';

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

=head2 auto : Private

Extracts values from the parameters. Accepts "acc", "id" and "entry", in lieu
of having them as path components.

We're using C<auto> to do all of this, rather than C<begin>, because of problems
with adding method modifiers when the C<begin> method has the C<Deserialize> 
action class added, e.g. C<sub begin : ActionClass('Deserialize') { }>. Ideally
we'd just do this stuff in an C<after> method, but it seems that method modifiers
just don't work when the C<Deserialize> C<ActionClass> is applied..

=cut

sub auto : Private {
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
  # if ( $c->req->param('live') ) {
  #   $c->log->debug( 'Family::begin: using "live" database' )
  #     if $c->debug;
  #   $c->stash->{live} = 1;
  #   $c->stash->{db} = $c->model('RfamLive');
  # }
  # else {
    $c->log->debug( 'Family::begin: using "release" database' )
      if $c->debug;
    $c->stash->{db} = $c->model('RfamDB');
  # }

  # 'auto' has to return true or the dispatch short-circuits straight to 'end'
  return 1;
}

#-------------------------------------------------------------------------------

=head2 end : ActionClass( 'Serialize' )

The L<Section> base class sets the C<end> action to use the L<RenderView>
C<ActionClass>, but that screws up the RESTful serialisation. Reset C<end> here
to use a serialiser to render the output, and rely on the mapping between 
MIME type and serialiser to do the right thing.

=cut

sub end : ActionClass( 'Serialize' ) { } 

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

  #---------------------------------------

  # should we decline to show the sunburst for this family ?
  if ( $this->{no_sunburst}->{ $c->stash->{acc} } ) {
    $c->log->debug( 'Family::family_page_GET_html: not showing sunburst for '
                    . $c->stash->{acc} )
      if $c->debug;
    $c->stash->{no_sunburst} = $this->{no_sunburst}->{ $c->stash->{acc} };
  }

  #---------------------------------------

  # add the clan details, if any
  my $clan = $c->stash->{db}->resultset('Clan')
               ->search( { rfam_acc => $c->stash->{acc} },
                         { prefetch => [ qw(clan_memberships) ] } )
               ->first;

  if ( $clan ) {
    my $members = $c->stash->{db}->resultset('ClanMembership')
                    ->search( { clan_acc => $clan->clan_acc },
                              { join => [ 'rfam_acc' ],
                                order_by => [ 'rfam_id' ] } );
    
    if ( $clan and defined $clan->clan_acc ) {
      $c->log->debug( 'Family::family_page: adding clan info' ) if $c->debug;
      $c->stash->{clan} = $clan;
      $c->stash->{clan_members} = $members;
    }
  }

  #---------------------------------------

  $c->log->debug( 'Family::family_page: adding summary info' ) 
    if $c->debug;
  $c->forward( 'get_summary_data' );

  $c->log->debug( 'Family::family_page: adding wikipedia info' ) 
    if $c->debug;
  $c->forward( 'get_wikipedia' );

  $c->log->debug( 'Family::family_page: adding references info' )
    if $c->debug;
  $c->forward( 'get_references' );

  $c->log->debug( 'Family::family_page: adding motif matches info' )
    if $c->debug;
  $c->forward( 'get_motif_matches' );

  #---------------------------------------

  $c->log->debug( 'Family::family_page: emitting HTML' )
    if $c->debug;

  $c->cache_page( 43200 ) unless $ENV{NO_CACHE}; # cache for 12 hours
  
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

sub old_family : Chained( '/' )
                 PathPart( 'family' )
                 Args( 0 ) {
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

    $c->stash->{rest}->{error} = 'No valid family accession/ID';
    $c->forward( 'family_page' );
  }
}

#-------------------------------------------------------------------------------

=head2 sunburst

Stub to add a "sunburst" pathpart. All methods from the L<SunburstMethods> Role
will be hung off this stub.

=cut

sub sunburst : Chained( 'family' )
               PathPart( 'sunburst' )
               CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;

  # specify the queue to use when submitting sunburst-related jobs
  $c->stash->{alignment_job_type} = 'rfalign';
}

#-------------------------------------------------------------------------------

=head2 sequences

=cut

sub sequences : Chained( 'family' )
                PathPart( 'sequences' )
                Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = "components/blocks/family/sequences_tab.tt";

  $c->forward('get_regions_data');
}

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
  my $rfam = $c->stash->{db}->resultset('Family')
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
  my $rs = $c->model('RfamDB::PdbFullRegion')->search( { rfam_acc => $c->stash->{acc} },{});
  $summaryData->{numStructures} = $rs->count;

  # Number of species
  $summaryData->{numSpecies} = $c->stash->{rfam}->number_of_species;

  # number of interactions
  $summaryData->{numInt} = 0;

  $c->stash->{summaryData} = $summaryData;

  # get tree curation data. Limit retrieved columns to avoid pulling down
  # alignments and tree data until we really need them.
  my $seed_tree_data = $c->stash->{db}->resultset( 'AlignmentAndTree' )
                         ->find( { rfam_acc => $c->stash->{acc},
                                   type     => 'seed' },
                                 { columns => [ qw{ type 
                                                    treemethod 
                                                    average_length 
                                                    percent_id 
                                                    number_of_sequences } ] } );

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

  $c->stash->{region_rs} = 
    $c->stash->{db}->resultset('FullRegion')
      ->search( { rfam_acc => $c->stash->{acc} },
                { join      => { 'rfamseq_acc' => 'ncbi' },
                  '+select' => [ qw( rfamseq_acc.rfamseq_acc
                                     rfamseq_acc.description
                                     ncbi.species
                                     ncbi.ncbi_id
                                     rfamseq_acc.length ) ],
                  '+as'     => [ qw( rfamseq_acc
                                     description
                                     species
                                     ncbi_taxid
                                     length ) ],
                  order_by  => [ 'bit_score DESC' ] } );
                                            
  $c->stash->{limits}  = $this->{refseqRegionsLimits};

$c->log->debug('Family::get_regions_data: added regions to stash')
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_wikipedia : Private

Retrieves the wikipedia content, if any, for this family.

=cut

sub get_wikipedia : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('WebUser::ArticleMapping')
                  ->search( { accession => $c->stash->{acc} },
                            { join     => [ 'wikitext' ],
                              prefetch => [ 'wikitext' ] } );
  my $article = $rs->next;
  
  return unless ( $article and $article->wikitext );

  $c->stash->{article} = $article;
}

#-------------------------------------------------------------------------------

=head2 get_references : Private

Retrieves the literature references content, if any, for this family.

=cut

sub get_references : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('RfamDB::LiteratureReference')
                  ->search( { 'family_literature_references.rfam_acc' => $c->stash->{acc} },
                            { join     => 'family_literature_references',
                              order_by => 'family_literature_references.order_added' }
  );

  my $match_count = $rs->count;

  if ($match_count > 0) {$c->stash->{motif_count} = 1}
  else {$c->stash->{motif_count} = 0}

  $c->stash->{family_literature} = $rs;

}
#-------------------------------------------------------------------------------

=head2 get_motif_matches : Private

Retrieves the motifs which match the family, if any

=cut

sub get_motif_matches : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('RfamDB::MotifFamilyStat')
                  ->search( { 'rfam_acc' => $c->stash->{acc} }, { order_by => { -asc => 'motif_acc' }}
  );

  $c->stash->{family_matches} = $rs;

}


#-------------------------------------------------------------------------------


=head2 build_fasta : Private

Builds a FASTA-format sequence file containing the region sequences for the
supplied accessions. This is "required" by the L<SunburstMethods> role.

Takes one argument, a reference to a list of accessions to retrieve.

=cut

sub build_fasta : Private {
  my ( $this, $c, $accessions ) = @_;

  my $fasta = '';
  foreach ( @$accessions ) {
    next unless m/^[\w\.]+$/;
    my $rs = $c->model( 'RfamDB::FullRegion' )
               ->search( { rfamseq_acc => $_,
                           rfam_acc    => $c->stash->{acc} },
                         { columns => [ qw( rfamseq_acc
                                            seq_start 
                                            seq_end ) ] } );
    while ( my $row = $rs->next ) {
      $fasta .= '>' . 
                $row->get_column('rfamseq_acc') . '/' .
                $row->seq_start . '-' . $row->seq_end . "\n";
    }
  }

  return $fasta;
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

