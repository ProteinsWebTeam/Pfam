# Family.pm
# jt6 20060411 WTSI
#
# $Id: Family.pm,v 1.54 2010-01-13 14:44:53 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main Pfam family
page

=cut

package PfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Pfam
families across the site. The L<begin|/"begin : Private"> method tries
to extract a Pfam ID or accession from the captured URL and tries to
load a Pfam object from the model.

Generates a B<tabbed page>.

$Id: Family.pm,v 1.54 2010-01-13 14:44:53 jt6 Exp $

=cut

use strict;
use warnings;

use LWP::UserAgent;
use XML::LibXML;
use Image::Size;
use Compress::Zlib;
use treefam::nhx_plot;
use Image::Size;
use Compress::Zlib;
use Text::Wrap;
use Bio::Pfam::AlignPfam;
use URI::Escape;
use JSON;
use Data::UUID;
use Bio::Pfam::ColourAlign;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

my %allowed_alignment_types = ( full => 1,
                                seed => 1,
                                ncbi => 1,
                                meta => 1,
                                long => 1 );

#-------------------------------------------------------------------------------

=head1 ACTIONS

=head2 begin : Private

Extracts the Pfam family ID or accession from parameters on the URL. Handles
any of three parameters:

=over

=item acc

a valid Pfam accession, either for an A or B entry

=item id

a valid Pfam accession

=item entry

either an ID or accession

=back

This is the way in when there are parameters but for "RESTful" URLs, the
chained actions pick up.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') ) {
    if ( $c->req->param('output') eq 'xml' ) {
      $c->stash->{output_xml} = 1;
      $c->res->content_type('text/xml');
    }
    elsif ( $c->req->param( 'output' ) eq 'pfamalyzer' ) {
      $c->stash->{output_pfamalyzer} = 1;
      $c->res->content_type('text/plain');
    }
  }

  # see if the entry is specified as a parameter
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->query_keywords || # accept getacc-style params
                      '';

  if ( $tainted_entry ) {
    $c->log->debug( 'Family::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w-]+)(\.\d+)?$/;
  }
}

#-------------------------------------------------------------------------------
#- main family actions ---------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 family : Chained

Tries to get a row from the DB for the family. This is the entry point for
URLs with the accession/ID given like "/family/piwi".

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

  # although these next checks might fail and end up putting an error message
  # into the stash, we don't "return", because we might want to process the
  # error message using a template that returns XML rather than simply HTML

  my $entry;
  if ( $tainted_entry ) {
    # strip off family version numbers, if present
    ( $entry ) = $tainted_entry =~ m/^([\w-]+)(\.\d+)?$/;
    $c->stash->{errorMsg} = 'Invalid Pfam family accession or ID'
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }

  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------

=head2 family_page : Chained

End point of a chain, which captures the URLs for the family page.

=cut

sub family_page : Chained( 'family' )
                  PathPart( '' )
                  Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # if we don't have an entry to work with by now, we're done
  return unless $c->stash->{pfam};

  #----------------------------------------

  # detect DUFs
  $c->stash->{is_duf} = ( $c->stash->{pfam}->pfama_id =~ m/^DUF\d+$/ );

  #----------------------------------------

  # dead families are a special case...
  if ( defined $c->stash->{entryType} and
       $c->stash->{entryType} eq 'D' ) {

    $c->log->debug( 'Family::family_page: got a dead family; setting a refresh URI' )
      if $c->debug;

    if ( $c->stash->{pfam}->forward_to ) {
      $c->stash->{refreshUri} =
        $c->secure_uri_for( '/family', $c->stash->{pfam}->forward_to );
    }
    else {
      $c->stash->{refreshUri} = $c->secure_uri_for( '/' );

      # the default delay for redirecting is 5 seconds, but that's maybe
      # too short to allow the user to read the message telling them that
      # they're going to be redirected to the home page. Pause for a little
      # longer in this case
      $c->stash->{refreshDelay} = 20;
    }

    # set the template. This will be overridden below if we're emitting XML
    $c->stash->{template} = 'pages/dead.tt';

    return;
  }

  #----------------------------------------

  # use a redirect page if the ID of the family has changed
  if ( defined $c->stash->{entryType} and
       $c->stash->{entryType} eq 'R' ) {

    $c->log->debug( 'Family::family_page: arrived at a family using a previous ID; setting a refresh URI' )
      if $c->debug;

    $c->stash->{refreshUri} =
      $c->secure_uri_for( '/family', $c->stash->{acc} );

    # set the template for the intermediate page
    $c->stash->{template} = 'pages/moved.tt';

    return;
  }

  #----------------------------------------

  # output is specific to PfamAlyzer
  if ( $c->stash->{output_pfamalyzer} ) {

    $c->log->debug( 'Family::family_page: outputting plain text for PfamAlyzer' )
      if $c->debug;

    $c->stash->{template} = 'rest/family/entry_pfamalyzer.tt';
  }

  # output is in XML format
  elsif ( $c->stash->{output_xml} ) {

    $c->log->debug( 'Family::family_page: outputting XML' )
      if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Family::family_page: there was an error: |' .
                      $c->stash->{errorMsg} . '|' ) if $c->debug;
      $c->stash->{template} = 'rest/family/error_xml.tt';
      return;
    }

    # decide on the output template, based on the type of family that we have
    if ( $c->stash->{entryType} eq 'A' or
         $c->stash->{entryType} eq 'R' ) {
      # we'll use the same XML template to handle familes that were arrived at
      # using a "previous ID"
      $c->log->debug( 'Family::family_page: got data for a Pfam-A' ) if $c->debug;
      $c->stash->{template} = 'rest/family/pfama_xml.tt';
    }
    elsif( $c->stash->{entryType} eq 'B' ) {
      $c->log->debug( 'Family::family_page: got data for a Pfam-B' ) if $c->debug;
      $c->stash->{template} = 'rest/family/pfamb_xml.tt';
    }
    elsif( $c->stash->{entryType} eq 'D' ) {
      $c->log->debug( 'Family::family_page: got data for a dead family' ) if $c->debug;
      $c->stash->{template} = 'rest/family/dead_xml.tt';
    }
    else {
      $c->log->debug( 'Family::family_page: got an error' ) if $c->debug;
      $c->stash->{template} = 'rest/family/error_xml.tt';
    }

  }

  # output is a web page
  else {

    $c->log->debug( 'Family::family_page: outputting HTML' )
      if $c->debug;

    $c->log->debug( 'Family::family_page: adding extra family info' ) if $c->debug;

    # add the clan details, if any
    my $clans = $c->model('PfamDB::ClanMembership')
                          ->search( { 'pfama_acc' => $c->stash->{pfam}->pfama_acc },
                                    { join     => [ qw(clan_acc) ],
                                      prefetch => [ qw(clan_acc) ] } )->first;

    if ( $clans and defined $clans->clan_acc->clan_acc ) {
      $c->log->debug( 'Family::family_page: adding clan info' ) if $c->debug;
      $c->stash->{clan} = $clans->clan_acc->clan_acc;
    }

    $c->forward( 'get_summary_data' );
    $c->forward( 'get_db_xrefs' );
    $c->forward( 'get_interactions' );
    #$c->forward( 'get_pseudofam' );
    $c->forward( 'get_wikipedia_title_role' );

    return;

  } # end of "else"

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
    $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, $c->req->params ) );
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }
}

#-------------------------------------------------------------------------------
#- HMM logos -------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 HMM LOGO ACTIONS

=head2 logo : Chained

Returns the HMM logo image for this family. This is subject to a check on the
size of the image and the type of browser that is requesting it. Since there
are known problems with firefox and large PNGs, we don't return the image
immediately in that case. The template takes care of showing a bit of text
and providing a link to load the image anyway.

=cut

sub logo : Chained( 'family' )
           PathPart( 'logo' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  my $logo = $c->forward( 'get_logo' );
  return if $c->stash->{errorMsg};

  my ( $logo_x, $logo_y ) = imgsize( \$logo );

  if ( defined $logo_x and defined $logo_y and
       ( $logo_x > $this->{image_size_limit} or
         $logo_y > $this->{image_size_limit} ) and
           $c->req->user_agent =~ m/Gecko/ and
       not $c->req->user_agent =~ m/WebKit/ ) {

    $c->log->debug( 'Family::FamilyActions::logo: browser is Gecko-based and image is large'
                    . " ($logo_x x $logo_y)" )
      if $c->debug;

    $c->stash->{logo_x} = $logo_x;
    $c->stash->{logo_y} = $logo_y;

    $c->stash->{large_logo} = 1;
  }

  $c->stash->{template} = 'components/logo.tt';
}

#---------------------------------------

=head2 old_logo : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_logo : Path( '/family/logo' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family::FamilyActions::old_logo: redirecting to "logo"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'logo', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 logo_image : Chained

Returns the HMM logo image for this family.

=cut

sub logo_image : Chained( 'family' )
                 PathPart( 'logo_image' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->log->debug( 'Family::FamilyActions::logo_image: returning raw image' )
    if $c->debug;

  my $logo = $c->forward( 'get_logo' );

  return if $c->stash->{errorMsg};

  if ( $c->req->param('dl') ) {
    my $filename = $c->stash->{acc} . '_logo.png';

    $c->log->debug( 'Family::FamilyActions::logo_image: forcing download of logo as '
                    . $filename ) if $c->debug;

    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'image/png' );
  $c->res->body( $logo );
}

#---------------------------------------

=head2 old_logo_image : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_logo_image : Path( '/family/logo_image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_logo_image: redirecting to "logo_image"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'logo_image', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- RESTful utility actions -----------------------------------------------------
#-------------------------------------------------------------------------------

=head1 RESTFUL UTILITY ACTIONS

=head2 hmm : Chained

Serve the contents of the HMM for a Pfam-A entry from the database. Requires
the "mode" parameter to be set either to "ls" or "fs".

=cut

sub hmm : Chained( 'family' )
          PathPart( 'hmm' )
          Args( 0 ) {
  my ( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # cache page for 1 week
  $c->cache_page( 604800 );

  my $cacheKey = 'hmm' . $c->stash->{acc};
  my $hmm      = $c->cache->get( $cacheKey );

  if ( defined $hmm ) {
    $c->log->debug( 'Family::gethmm: extracted HMM from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::gethmm: failed to extract HMM from cache; going to DB' )
      if $c->debug;

    my $rs = $c->model('PfamDB::PfamaHmm')
               ->find( $c->stash->{pfam}->pfama_acc );

    unless ( $rs ) {
      $c->log->warn( 'Family::FamilyActions::hmm: failed to find row' )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not find the HMM for '
                              . $c->stash->{acc};
      $c->res->status( 500 );
      return;
    }

    $hmm = $rs->hmm;

    unless ( $hmm ) {
      $c->log->warn( 'Family::FamilyActions::hmm: failed to retrieve HMM from row' )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the HMM for '
                              . $c->stash->{acc};

      $c->res->status( 500 );
      return;
    }

    # cache the raw HMM
    $c->cache->set( $cacheKey, $hmm ) unless $ENV{NO_CACHE};
  }

  # build a name for the file that will be downloaded
  my $filename = $c->stash->{pfam}->pfama_id . '.hmm';

  # set the response headers
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # at this point we should have the HMM in hand, so spit it out to the
  # response and we're done
  $c->res->body( $hmm );

  # the RenderView action on the end method in Section.pm will spot that there's
  # content in the response and return without trying to render any templates
}

#---------------------------------------

=head2 old_hmm : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_hmm : Path( '/family/hmm' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_hmm: redirecting to "hmm"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'hmm', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 id : Chained

Returns the ID for this family as a single, plain text string. Returns 404 if
there's no family to work on.

=cut

sub id : Chained( 'family' )
         PathPart( 'id' )
         Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  if ( defined $c->stash->{pfam} ) {
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfama_id );
    }
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#---------------------------------------

=head2 old_id : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_id : Path( '/family/id' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_id: redirecting to "id"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'id', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 acc : Chained

Returns the accession for this family as a single, plain text string. Returns
404 if there's no family to work on.

=cut

sub acc : Chained( 'family' )
          PathPart( 'acc' )
          Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  if ( defined $c->stash->{pfam} ) {
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/family/entry_xml.tt';
    }
    else {
      $c->res->content_type( 'text/plain' );
      $c->res->body( $c->stash->{pfam}->pfama_acc );
    }
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#---------------------------------------

=head2 old_acc : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_acc : Path( '/family/acc' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_acc: redirecting to "acc"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'acc', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 desc : Chained

Returns the description string for a family. If the "output_pfamalyzer"
parameter is set, the output returns more family information.

=cut

sub desc : Chained( 'family' )
           PathPart( 'desc' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  if ( defined $c->stash->{pfam} ) {

    $c->res->content_type( 'text/plain' );

    if ( $c->stash->{output_pfamalyzer} ) {
      $c->res->body(
        $c->stash->{pfam}->pfama_acc   . "\t" .
        $c->stash->{pfam}->author      . "\t" .
        $c->stash->{pfam}->type        . "\t" .
        $c->stash->{pfam}->num_seed    . "\t" .
        $c->stash->{pfam}->num_full    . "\t" .
        $c->stash->{pfam}->description . "\t" .
        $c->stash->{pfam}->comment
      );
    }
    else {
      $c->res->body( $c->stash->{pfam}->description );
    }
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#-------------------------------------------------------------------------------
#- structure/family stuff ------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 STRUCTURE ACTIONS

=head2 structures : Chained

Retrieves the list of PDB entries for this family. If a PDB ID is specified,
the method also retrieves the row of the "pdb" table for that entry.

=cut

sub structures : Chained( 'family' )
                 PathPart( 'structures' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # see if we were handed a valid PDB ID and, if so, just stash it
  if ( defined $c->req->param('pdbId') and
       $c->req->param('pdbId') =~ /^(\d\w{3})$/ ) {

    $c->log->debug( "Family::structures: got PDB ID: |$1|" )
      if $c->debug;

    $c->stash->{pdb_id} = $1;
  }

  # retrieve the PDB entries for this family
  my @regions;
  if ( defined $c->stash->{pfam}->pfama_acc ) {
    $c->log->debug( 'Family::structures: got an pfama_acc: '
                    . $c->stash->{pfam}->pfama_acc ) if $c->debug;
    @regions = $c->model('PfamDB::PdbPfamaReg')
                 ->search( { 'me.pfama_acc' => $c->stash->{pfam}->pfama_acc },
                           { prefetch => [ qw( pdb_id pdb_image pfama_acc ) ] } );
    $c->log->debug( 'Family::structures: got '
                    . scalar @regions . ' regions' ) if $c->debug;
  }

  # don't render the template unless we need to
  unless ( scalar @regions ) {
    $c->log->debug( 'Family::structures: no structure image; not rendering template' )
      if $c->debug;
    $c->res->status( 204 );
    return;
  }

  my $pdb_unique = {};
  my $colours = {};
  foreach my $region ( @regions ) {
    my $id = $region->pdb_id->pdb_id;
    $pdb_unique->{$id} = $region;
    $colours->{$id}->{$region->hex_colour} = $region->pfama_acc->pfama_id;
  }

  $c->stash->{pdb_unique} = $pdb_unique;
  $c->stash->{colours}    = $colours;

  # my %pdb_unique = map{ $_->pdb_id->pdb_id => $_ } @regions;
  # $c->stash->{pdb_unique} = \%pdb_unique;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = 'components/blocks/family/familyStructures.tt';

  # cache the template output for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_structures : Path( '/family/structures' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'structures', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 mapping : Chained

Renders a table showing the mapping between Pfam family, UniProt region and
PDB residues.

=cut

sub mapping : Chained( 'family' )
              PathPart( 'mapping' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->log->debug( 'Family::FamilyActions::mapping: acc: |'
                  . $c->stash->{acc}  . '|' .  $c->stash->{entryType}. '|' )
    if $c->debug;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc },
                            { join       => [ qw( pdb_id pfamseq_acc ) ],
                              columns    => [ qw( pfamseq_acc.pfamseq_id
                                                  seq_start
                                                  seq_end
                                                  pdb_id.pdb_id
                                                  chain
                                                  pdb_res_start
                                                  pdb_res_end ) ] } );

  $c->stash->{pfamMaps} = \@mapping;
  $c->log->debug( 'Family::FamilyActions::mapping: found |' . scalar @mapping . '| rows' )
    if $c->debug;

  unless ( scalar @mapping ) {
    $c->log->debug( 'Family::FamilyActions::mapping: no rows; returning 204' )
      if $c->debug;

    $c->res->status( 204 );

    return;
  }

  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Family::FamilyActions::mapping: emitting XML' ) if $c->debug;
    $c->stash->{template} = 'rest/family/structures_xml.tt';
  }
  else {
    $c->log->debug( 'Family::FamilyActions::mapping: emitting HTML' ) if $c->debug;
    $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  }

  # cache the template output for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_mapping : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_mapping : Path( '/family/structures/mapping' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::FamilyActions::old_mapping: redirecting to "mapping"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'mapping', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- tree-related actions --------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 TREE ACTIONS

=head2 tree : Chained

Stub to set the type of alignment that we want.

=cut

sub tree : Chained( 'family' )
           PathPart( 'tree' )
           CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->stash->{alnType} = 'seed';

  if ( defined $aln_type and
       exists $allowed_alignment_types{ $aln_type } ) {
    $c->stash->{alnType} = $aln_type;
  }
}

#-------------------------------------------------------------------------------

=head2 tree_html : Chained

Plots the tree and hands off to a template that builds HTML for the image and
associated image map.

=cut

sub tree_html : Chained( 'tree' )
                PathPart( 'html' )
                Args( 0 ) {
  my ( $this, $c, $aln_type ) = @_;

  # stash the tree object
  $c->forward( 'get_tree' );

  # bail unless we actually got a tree
  unless ( defined $c->stash->{tree} ) {
    $c->res->status( 204 );
    return;
  }

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core;

  # set up the TT view
  $c->stash->{template} = 'components/blocks/family/treeMap.tt';

  # cache the page (fragment) for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_tree : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_tree : Path( '/family/tree' ) {
  my ( $this, $c, $action ) = @_;

  my $aln_type = 'seed';

  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  if ( ( $action || '' ) eq 'image' ) {
    $c->log->debug( 'Family::Tree::old_tree: redirecting to "image"' )
      if $c->debug;
    $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "tree/$aln_type/image", $c->req->params ) );
  }
  elsif ( ( $action || '' ) eq 'download' ) {
    $c->log->debug( 'Family::Tree::old_tree: redirecting to "download"' )
      if $c->debug;
    $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "tree/$aln_type/download", $c->req->params ) );
  }
	else {
    $c->log->debug( 'Family::Tree::old_tree: redirecting to "tree"' )
      if $c->debug;
    $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "tree/$aln_type", $c->req->params ) );
  }
}

#-------------------------------------------------------------------------------

=head2 image : Local

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub image : Chained( 'tree' )
            PathPart( 'image' )
            Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # stash the tree object
  $c->forward( 'get_tree' );

  if ( defined $c->stash->{tree} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{tree}->plot_core( 1 )->gif );
  }
    else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->secure_uri_for( '/shared/images/blank.gif' ) ) if $c->debug;
  }

}

#-------------------------------------------------------------------------------

=head2 download : Chained

Serves the raw tree data as a downloadable file.

=cut

sub download : Chained( 'tree' )
               PathPart( 'download' )
               Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->log->debug( 'Family::Tree::download: dumping tree data to the response' )
    if $c->debug;

  # stash the raw tree data
  $c->forward( 'get_tree_data' );

  return unless defined $c->stash->{treeData};

  my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType} . '.nhx';

  $c->log->debug( "Family::Tree::download: tree filename: |$filename|" )
    if $c->debug;

  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $c->stash->{treeData} );
}

#-------------------------------------------------------------------------------
#- alignment-related actions ---------------------------------------------------
#-------------------------------------------------------------------------------

=head1 ALIGNMENT ACTIONS

=head2 alignment : Chained

Start of a chain for the other methods in this controller. Sets the alignment
type in the stash, based on the "alnType" parameter.

=cut

sub alignment : Chained( 'family' )
                PathPart( 'alignment' )
                CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->stash->{alnType} = 'seed';

  if ( defined $aln_type and
       exists $allowed_alignment_types{ $aln_type } ) {
    $c->stash->{alnType} = $aln_type;
  }

  $c->log->debug( 'Family::alignment: aln_type: |' . $c->stash->{alnType} .'|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 alignment_link : Chained

An endpoint for the /family/ACC/alignment chain that doesn't need the alignment
type specified.

=cut

sub alignment_link : Chained( 'family' )
                     PathPart( 'alignment' )
                     CaptureArgs( 0 ) { }

#-------------------------------------------------------------------------------

=head2 raw_alignment : Chained

Dumps the raw alignment to the response as plain text.

=cut

sub raw_alignment : Chained( 'alignment' )
                    PathPart( '' )
                    Args( 0 ) {
	my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  $c->res->content_type( 'text/plain' );
  $c->res->body( $c->stash->{alignment} );
}

#-------------------------------------------------------------------------------

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family.

=cut

sub gzipped : Chained( 'alignment' )
              PathPart( 'gzipped' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  my ( $alignment, $filename );

  if ( $c->stash->{alnType} eq 'long' ) {
    $c->log->debug( 'Family::gzipped: building full length sequence FASTA' )
      if $c->debug;

    # build the alignment file
    my $rs = $c->model('PfamDB::PfamaRegFullSignificant')
               ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                           in_full    => 1 },
                         { prefetch => [ qw( pfamseq_acc ) ],
                           columns  => [ qw( pfamseq_acc.pfamseq_id
                                             pfamseq_acc.pfamseq_acc
                                             pfamseq_acc.sequence ) ] } );
    my $sequences = '';
    while ( my $seq_row = $rs->next ) {
      $Text::Wrap::columns = 60;
      $sequences .= '>' . $seq_row->pfamseq_id . ' (' . $seq_row->pfamseq_acc . ")\n";
      $sequences .= wrap( '', '', $seq_row->sequence ) . "\n";
    }

    # compress it
    $alignment = Compress::Zlib::memGzip( $sequences );

    # build a filename for it
    $filename = $c->stash->{acc} . '_full_length_sequences.fasta.gz';
  }
  else {
    # retrieve the alignment
     my $rs = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single();

    $alignment = $rs->alignment;
    $filename  = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.gz';
  }

  unless ( defined $alignment ) {
    $c->log->warn( 'Family::gzipped: failed to retrieve alignment for '
                    . $c->stash->{acc} ) if $c->debug;

    $c->res->status( 204 ); # "no content"

    return;
  }

  # set the filename on the HTTP headers, so that the browser will offer to
  # download and save it
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # ... and dump it straight to the response
  $c->res->content_type( 'application/x-gzip' );
  $c->res->body( $alignment );
}

#---------------------------------------

=head2 old_gzipped : Path

This is used by the form in the Pfam family page. The form is currently
submitted by the browser directly, so there's no javascript to intervene
and convert the parameters into URL arguments. This action will accept
the parameters and redirect to the Chained action above.

=cut

sub old_gzipped : Path( '/family/alignment/download/gzipped' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_gzipped: redirecting to "gzipped"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/gzipped", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 format : Chained

Serves the plain text (no markup) alignment. Also applies various
changes to the alignment before hand, such as changing the gap style
or sequence order.

By default this method will generate Stockholm format, but the
following formats are also available and can be specified with the
C<format> parameter:

=over

=item * pfam

Essentially Stockholm with markup lines removed

=item * fasta

Vanilla FASTA format

=item * msf

No idea...

=item * stockholm (default)

Standard Stockholm format

=back

The exact styles (other than C<pfam>) are defined by BioPerl.

=cut

sub format : Chained( 'alignment' )
             PathPart( 'format' )
             Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  if ( defined $c->stash->{alignment_rows} ) {
    $c->log->debug( 'Family::format: successfully retrieved an alignment' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::format: failed to retrieve an alignment' )
      if $c->debug;
  }

  # drop it into an AlignPfam object
  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $c->stash->{alignment_rows} );
  };
  if ( $@ ) {
    $c->log->debug( "Family::format: problem reading stockholm data: $@" )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if ( $c->req->param('gaps') ) {
    $c->log->debug( 'Family::format: handling gaps parameter' )
      if $c->debug;

    if ( $c->req->param('gaps') =~ m/^n\w*/ ) {
      $pfamaln->map_chars('-', '');
      $pfamaln->map_chars('\.', '');
    }
    elsif ( $c->req->param('gaps') =~ m/^do\w*$/ ) {
      $pfamaln->map_chars('-', '.');
    }
    elsif ( $c->req->param('gaps') =~ m/^da\w*$/ ) {
      $pfamaln->map_chars('\.', '-');
    }
  }

  # case param can be u or l
  if ( $c->req->param('case') and $c->req->param('case') =~ m/^u\w*$/ ) {
    $c->log->debug( 'Family::format: uppercasing alignment' )
      if $c->debug;
    $pfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if ( $c->req->param('order') and $c->req->param('order') =~ m/^a\w*$/ ) {
    $c->log->debug( 'Family::format: sorting alphabetically' )
      if $c->debug;
    $pfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if ( $c->req->param( 'format' ) ) {
    if ( $c->req->param( 'format' ) =~ m/^p\w*$/ ) {
      $c->log->debug( 'Family::format: writing Pfam format' )
        if $c->debug;
      $output = $pfamaln->write_Pfam;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^f\w*$/ ) {
      $c->log->debug( 'Family::format: writing FASTA format' )
        if $c->debug;
      $output = $pfamaln->write_fasta;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^m\w*$/ ) {
      $c->log->debug( 'Family::format: writing MSF format' )
        if $c->debug;
      $output = $pfamaln->write_MSF;
    }
  }

  # default to writing stockholm
  $output ||= $pfamaln->write_stockholm;

  # are we downloading this or just dumping it to the browser ?
  if ( $c->req->param( 'download' ) ) {
    $c->log->debug( 'Family::format: sending alignment as download' )
      if $c->debug;

    my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.txt';

    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'text/plain' );
  $c->res->body( join '', @$output );
}

#---------------------------------------

=head2 old_format : Path

This is used by the form in the Pfam family page. The form is currently
submitted by the browser directly, so there's no javascript to intervene
and convert the parameters into URL arguments. This action will accept
the parameters and redirect to the Chained action above.

=cut

sub old_format : Path( '/family/alignment/download/format' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_format: redirecting to "format"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/format", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 html : Chained

Retrieves the HTML alignment and dumps it to the response. We first try to
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub html : Chained( 'alignment' )
           PathPart( 'html' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';

  my $cacheKey = 'jtml' . $c->stash->{acc} . $c->stash->{alnType};

  my $jtml = $c->cache->get( $cacheKey );
  if ( defined $jtml ) {
    $c->log->debug( 'Family::html: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::html: failed to extract HTML from cache; going to DB' )
      if $c->debug;

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} },
                           { columns    => [ qw( jtml ) ] } )
                ->single;

    # final check...
    unless ( defined $row->jtml ) {
      $c->log->debug( 'Family::html: failed to retrieve JTML' )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $jtml = Compress::Zlib::memGunzip( $row->jtml );
    unless ( defined $jtml ) {
      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::html: retrieved HTML from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $jtml ) unless $ENV{NO_CACHE};
  }

  # stash the HTML
  $c->stash->{html_alignment} = $jtml;

}

#---------------------------------------

=head2 old_html : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_html : Path( '/family/alignment/download/html' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';

  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_html: redirecting to "html"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/html", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 heatmap : Chained

Retrieves the HTML "heatmap" coloured alignment and dumps it to the response.
We first try to extract the HTML from the cache or, if that fails, we retrieve
it from the DB.

=cut

sub heatmap : Chained( 'alignment' )
              PathPart( 'heatmap' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # heatmaps are only available for full alignments
  if ( $c->stash->{alnType} ne 'full' ) {
    $c->stash->{errorMsg} = 'Heatmaps are only available for full alignments';
    return;
  }

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';
  $c->stash->{alnType}  = 'heatmap';

  my $cacheKey = 'heatmap' . $c->stash->{acc};

  my $hm = $c->cache->get( $cacheKey );
  if ( defined $hm ) {
    $c->log->debug( 'Family::heatmap: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::heatmap: failed to extract HTML from cache; going to DB' )
      if $c->debug;

    # retrieve the HTML from the DB
    my $row = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                            type       => 'full' },
                          { columns    => [ qw( post ) ] } )
                ->single;

    # final check...
    unless ( defined $row->post ) {
      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress the row to get the raw HTML
    $hm = Compress::Zlib::memGunzip( $row->post );
    unless ( defined $hm ) {
      $c->stash->{errorMsg} = 'We could not extract the heatmap alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'Family::heatmap: retrieved HTML from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $hm ) unless $ENV{NO_CACHE};
  }

  # stash the HTML
  $c->stash->{html_alignment} = $hm;
}

#---------------------------------------

=head2 old_heatmap : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_heatmap : Path( '/family/alignment/download/heatmap' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_heatmap: redirecting to "heatmap"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'alignment/full/heatmap', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 jalview : Chained

This is the way into the JalView alignment viewer applet.

Hands straight off to a template that generates a "tool" page containing the
JalView applet.

=cut

sub jalview : Chained( 'alignment' )
              PathPart( 'jalview' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->stash->{template} = 'components/tools/jalview.tt';
}

#---------------------------------------

=head2 old_jalview : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_jalview : Path( '/family/alignment/jalview' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_jalview: redirecting to "jalview"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/jalview", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 das_viewer : Chained

=cut

sub dasviewer : Chained( 'alignment' )
                PathPart( 'dasviewer' )
                Args( 0 ) {
  my ( $self, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # build a "title" string, which will be used as the heading for the
  # alignment tool window
  my $title = 'Pfam ' . $c->stash->{alnType} . ' alignment for '
              . $c->stash->{acc};

  # find out how many rows are in the alignment
  my $num_rows = ( $c->stash->{alnType} eq 'seed' )
                 ? $c->stash->{pfam}->num_seed
                 : $c->stash->{pfam}->num_full;

  my $das_source = $c->stash->{ alnType } eq 'seed'
                 ? 'Pfam_Seed_Alignments'
                 : 'Pfam_Full_Alignments' ;

  $c->log->debug( 'Family::dasviewer: setting up for get_das_alignment' )
    if $c->debug;

  $c->stash->{params} = { source             => 'family',
                          dasSource          => $das_source,
                          title              => $title,
                          acc                => $c->stash->{acc},
                          alnType            => $c->stash->{alnType},
                          numRowsInAlignment => $num_rows };

  # now stash the template which will have the necessary params to
  $c->stash->{ template } = 'components/tools/pfamviewer/dasviewer.tt';
}

#---------------------------------------

=head2 old_dasviewer : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

# http://pfam.sanger.ac.uk/family/alignment/dasviewer?acc=PF02171&alnType=full&viewer=viewer

sub old_dasviewer : Path( '/family/alignment/dasviewer' ) {
  my ( $this, $c ) = @_;

  my $aln_type = 'seed';
  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  $c->log->debug( 'Family::old_dasviewer: redirecting to "dasviewer"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, "alignment/$aln_type/dasViewer", $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 build : Path

Builds a sequence alignment from the specified sequences. The "jobId" parameter
in this action refers to the list of accessions that were previously stored in
the species_collection table, rather than the job that we run to align them.

=cut

sub build : Chained( 'alignment_link' )
            PathPart( 'build' )
            Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->log->debug( 'Family::build: checking for sequences' )
    if $c->debug;

  unless ( $c->req->param('jobId') ) {
    $c->log->debug( 'Family::build: no job ID supplied' )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was no job ID for this alignment..';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # validate the UUID
  my $collection_id = $c->req->param('jobId');
  unless ( $collection_id =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'Family::build: bad job id' )
      if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # retrieve the sequences
  $c->stash->{fasta} = $c->forward( '/utils/get_sequences',
                                    [ $collection_id, $c->stash->{pfam} ] );

  # make sure we got something...
  unless ( length $c->stash->{fasta} ) {
    $c->log->debug( 'Family::build: failed to get a FASTA sequence' )
      if $c->debug;
    $c->stash->{errorMsg} = 'We failed to get a FASTA format sequence file for your selected sequences.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # submit the job to actually build the alignment
  my $submissionStatus = $c->forward( 'queueAlignment' );

  # and see if we managed it...
  if ( $submissionStatus < 0 ) {
    $c->log->debug( 'Family::build: problem with submission; returning error page' )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was an error when submitting your sequences to be aligned.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
  }
  else {
    $c->log->debug( 'Family::build: alignment job submitted; polling' )
      if $c->debug;
    $c->stash->{template} = 'components/tools/seqViewAlignmentPolling.tt';
  }
}

#---------------------------------------

=head2 old_build : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_build : Path( '/family/alignment/builder' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_build: redirecting to "build"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'alignment/build', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 view : Local

Retrieves the sequence alignment that we generated.

=cut

sub view : Chained( 'alignment_link' )
           PathPart( 'view' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # retrieve the job results
  my ( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/i;
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );

  unless ( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::view: no results found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # count the number of rows in the alignment. The raw alignment includes
  # the consensus string as the last line
  my @rows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  my $numRowsInAlignment = scalar @rows - 1;
  $c->log->debug( "Family::view: alignment has |$numRowsInAlignment| rows" )
    if $c->debug;

  # configure the viewer...
  $c->stash->{params} = { source             => 'species',
                          title              => 'Alignment for selected sequences',
                          jobId              => $jobId,
                          numRowsInAlignment => $numRowsInAlignment };

  # and hand off to it
  $c->forward( 'PfamViewer', 'showPfamViewer' );
}

#---------------------------------------

=head2 old_view : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_view : Path( '/family/alignment/builder/view' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_view: redirecting to "view"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'alignment/view', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- general family-related private actions --------------------------------------
#-------------------------------------------------------------------------------

=head1 PRIVATE ACTIONS

=head2 get_data : Private

Retrieves family data for the given entry. Accepts the entry ID or accession
as the first argument. Does not return any value but drops the L<ResultSet>
for the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;

  # check for a Pfam-A
  my $rs = $c->model('PfamDB::Pfama')
             ->search( [ { pfama_acc => $entry },
                         { pfama_id  => $entry } ],
                       { join     => [ { clan_memberships => 'clan_acc' },
                                       "interpros",
                                       "pfama_species_trees" ], } );
                          prefetch => [ qw( interpros pfama_species_trees ) ] } );

  my $pfam = $rs->first if defined $rs;

  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a Pfam-A [$entry]' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfama_acc;
    $c->stash->{entryType} = 'A';

    # GO data are used by both the XML and HTML outputs, so always get those
    $c->forward( 'get_go_data' );

    return; # unless $c->stash->{output_xml};

  } # end of "if pfam..."

  #----------------------------------------
  # check for a dead Pfam-A

  if ( not $pfam ) {
    $pfam = $c->model('PfamDB::DeadFamily')
              ->search( [ { pfama_acc => $entry },
                          { pfama_id  => $entry } ] )
              ->single;

    if ( $pfam ) {
      $c->log->debug( 'Family::get_data: got a dead family' ) if $c->debug;
      $c->stash->{pfam}      = $pfam;
      $c->stash->{acc}       = $pfam->pfama_acc;
      $c->stash->{entryType} = 'D';
      return;
    }
  }

  #----------------------------------------
  # check for a previous ID

  if ( not $pfam ) {
    $pfam = $c->model('PfamDB::Pfama')
              ->find( { previous_id => { like => "%$entry;%" } } );

    # make sure the entry matches a whole ID, rather than just part of one
    # i.e. make sure that "6" doesn't match "DUF456"
    if ( $pfam ) {
      my $previous_id = $pfam->previous_id;
      if ( $previous_id =~ m/(^|.*?;\s*)$entry\;/ ) { # same pattern used in Jump.pm
        $c->log->debug( 'Family::get_data: got a family using a previous ID' )
          if $c->debug;
        $c->stash->{pfam}      = $pfam;
        $c->stash->{acc}       = $pfam->pfama_acc;
        $c->stash->{entryType} = 'R';
        return;
      }
    }
  }

  #----------------------------------------
  # there's a problem... by this point we really should have retrieved a
  # row and returned

  $c->stash->{errorMsg} = 'No valid Pfam family accession or ID';
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the PfamA object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $summaryData = {};

  # number of architectures....
  $summaryData->{numArchitectures} = $c->stash->{pfam}->number_archs;

  # number of sequences in full alignment
  $summaryData->{numSequences} = $c->stash->{pfam}->num_full;

  # number of structures known for the domain
  $summaryData->{numStructures} = $c->stash->{pfam}->number_structures;

  # Number of species
  $summaryData->{numSpecies} = $c->stash->{pfam}->number_species;

  # number of interactions
  my $pfama_acc = $c->stash->{pfam}->pfama_acc;
  my $rs = $c->model('PfamDB::PfamaInteractions')
             ->search( { pfama_acc_a => $pfama_acc },
                       { select => [ { count => 'pfama_acc_a' } ],
                         as     => [ qw( numInts ) ] } )
             ->first;
  $summaryData->{numInt} = $rs->get_column( 'numInts' );

  $c->stash->{summaryData} = $summaryData;
}

#-------------------------------------------------------------------------------

=head2 get_db_xrefs : Private

Retrieve the database cross-references for the family.

=cut

sub get_db_xrefs : Private {
  my ( $this, $c ) = @_;

  my $xRefs = {};

  # stuff in the accession and ID for this entry
  $xRefs->{entryAcc} = $c->stash->{pfam}->pfama_acc;
  $xRefs->{entryId}  = $c->stash->{pfam}->pfama_id;

  # Interpro
  my $i = $c->model('PfamDB::Interpro')
            ->find( $c->stash->{pfam}->pfama_acc );

  push @{ $xRefs->{interpro} }, $i if defined $i;

  # PDB
  $xRefs->{pdb} = keys %{ $c->stash->{pdbUnique} }
    if $c->stash->{summaryData}{numStructures};


  # PfamA relationship based on SCOOP
  my @ataSCOOP = $c->model('PfamDB::Pfama2pfamaScoopResults')
                   ->search( { pfama_acc_1 => $c->stash->{pfam}->pfama_acc,
                               score       => { '>', 10.0 } },
                             { join        => [ qw( pfamA1 pfamA2 ) ],
                               select      => [ qw( pfamA1.pfama_id
                                                    pfamA2.pfama_id
                                                    pfamA1.pfama_acc
                                                    pfamA2.pfama_acc
                                                    score ) ],
                               as          => [ qw( l_pfama_id
                                                    r_pfama_id
                                                    l_pfama_acc
                                                    r_pfama_acc
                                                    score ) ]
                             } );



  my @ataSCOOP2 = $c->model('PfamDB::Pfama2pfamaScoopResults')
                   ->search( { pfama_acc_2 => $c->stash->{pfam}->pfama_acc,
                               score       => { '>', 10.0 } },
                             { join        => [ qw( pfamA2 pfamA1 ) ],
                               select      => [ qw( pfamA2.pfama_id
                                                    pfamA1.pfama_id
                                                    pfamA2.pfama_acc
                                                    pfamA1.pfama_acc
                                                    score ) ],
                               as          => [ qw( l_pfama_id
                                                    r_pfama_id
                                                    l_pfama_acc
                                                    r_pfama_acc
                                                    score ) ]
                             } );



  foreach my $ref ( @ataSCOOP, @ataSCOOP2 ) {
    if ( $ref->get_column('l_pfama_acc') ne $ref->get_column('r_pfama_acc') ) {
      push @{ $xRefs->{scoop} }, $ref;
    }
  }

  # PfamA to PfamB links based on ADDA
  my %atobPRODOM;
  foreach my $xref ( $c->stash->{pfam}->pfama_database_links ) {
    if ( $xref->db_id eq 'PFAMB' ) {
      $atobPRODOM{$xref->db_link} = $xref;
    }
    else {
      push @{ $xRefs->{$xref->db_id} }, $xref;
    }
  }

  # PfamA to PfamA links based on HHsearch
  my @atoaHH = $c->model('PfamDB::Pfama2pfamaHhsearchResults')
                 ->search( { 'pfama_acc_1.pfama_acc' => $c->stash->{pfam}->pfama_acc },
                           { join     => [ qw( pfama_acc_1 pfama_acc_2 ) ],
                             select   => [ qw( pfama_acc_1.pfama_id
                                               pfama_acc_1.pfama_acc
                                               pfama_acc_2.pfama_id
                                               pfama_acc_2.pfama_acc
                                               evalue ) ],
                             as       => [ qw( l_pfama_id
                                               l_pfama_acc
                                               r_pfama_id
                                               r_pfama_acc
                                               evalue ) ],
                             order_by => 'pfama_acc_2.pfam_acc_2 ASC'
                           } );

  $xRefs->{atoaHH} = [];
  foreach ( @atoaHH ) {
    if ( $_->get_column( 'evalue' ) <= 0.001 and
         $_->get_column( 'l_pfama_id' ) ne $_->get_column( 'r_pfama_id' ) ) {
      push @{ $xRefs->{atoaHH} }, $_;
    }
  }

  $c->stash->{xrefs} = $xRefs;
}

#-------------------------------------------------------------------------------

=head2 get_go_data : Private

Retrieves the gene ontology (GO) data for the family.

=cut

sub get_go_data : Private {
  my ( $this, $c ) = @_;

  my @goTerms = $c->model('PfamDB::GeneOntology')
                  ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc } );

  $c->stash->{goTerms} = \@goTerms;
}

#-------------------------------------------------------------------------------

=head2 get_interactions : Private

Retrieves details of the interactions between this family and others.

=cut

sub get_interactions : Private {
  my ( $this, $c ) = @_;

  my @interactions = $c->model('PfamDB::PfamaInteractions')
                       ->search( { pfama_acc_a => $c->stash->{pfam}->pfama_acc },
                                 { join     => [ qw( pfama_acc_b ) ],
                                   prefetch => [ qw( pfama_acc_b ) ] } );

  $c->stash->{interactions} = \@interactions;
}

#-------------------------------------------------------------------------------

=head2 get_pseudofam : Private

Retrieves details of the interactions between this family and others.

=cut

sub get_pseudofam : Private {
  my ( $this, $c ) = @_;

  my $cache_key = 'pseudofam_families';
  $c->stash->{pseudofam_accessions} = $c->cache->get( $cache_key );

  if ( defined $c->stash->{pseudofam_accessions} ) {
    $c->log->debug( 'Family::get_pseudofam: retrieved pseudofam families list from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::get_pseudofam: failed to retrieve pseudofam families list from cache; going to pseudofam web service' )
      if $c->debug;

    # get the accessions list from the web service
    $c->forward( 'get_pseudofam_accessions' );

    if ( defined $c->stash->{pseudofam_accessions} ) {
      $c->log->debug( 'Family::get_pseudofam: got pseudofam_accessions' )
        if $c->debug;
      $c->cache->set( $cache_key, $c->stash->{pseudofam_accessions} ) unless $ENV{NO_CACHE};
    }
    else {
      $c->log->debug( 'Family::get_pseudofam: failed to retrieve pseudofam accessions' )
        if $c->debug;
      return;
    }
  }

  # for convenience, pull the URL prefix out of the accessions hash and stash
  # it individually
  $c->stash->{pseudofam_prefix_url} = $c->stash->{pseudofam_accessions}->{_prefix};

  $c->log->debug( 'Family::get_pseudofam: got a hash of '
                  . scalar( keys %{$c->stash->{pseudofam_accessions} } )
                  . ' accessions' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_pseudofam_accessions : Private

Parses the XML containing the list of pseudofam accessions.

=cut

sub get_pseudofam_accessions : Private {
  my ( $this, $c ) = @_;

  # go to the web service and retrieve the XML
  $c->forward( 'retrieve_pseudofam_xml' );

  # did we manage to retrieve it ?
  unless ( defined $c->stash->{pseudofam_xml} ) {
    $c->log->warn( 'Family::get_pseudofam_accessions: failed to retrieve pseudofam families list from the pseudofam web service: '
                   . $c->stash->{error} ) if $c->debug;
    return;
  }

  # now parse the XML to extract the list of families, which we then store
  # as hash keys
  my $xml_parser = XML::LibXML->new();
  my $xml_document;
  my $xml_root;
  eval {
    $xml_document = $xml_parser->parse_string( $c->stash->{pseudofam_xml} );
    $xml_root     = $xml_document->documentElement();
  };
  if ( $@ ) {
    $c->log->warn( "Family::get_pseudofam_accessions: failed to parse pseudofam xml: $@" )
      if $c->debug;
    return;
  }

  # get the family accessions
  my @accession_nodes = $xml_root->findnodes( '/pseudofam/families/accession' );
  $c->log->debug( 'Family::get_pseudofam_accessions: found '
                  . scalar @accession_nodes . ' accessions in file' )
    if $c->debug;

  my %accessions = map { $_->textContent => 1 } @accession_nodes;

  # get the URL prefix from the XML and drop that into the accessions hash
  my $prefix_url = $xml_root->find( '/pseudofam/families' )
                            ->shift()
                            ->getAttribute('urlPrefix');
  $accessions{_prefix} = $prefix_url;

  $c->stash->{pseudofam_accessions} = \%accessions;
}

#-------------------------------------------------------------------------------

=head2 retrieve_pseudofam_xml : Private

Retrieves the XML containing the list of pseudofam accessions from their web
service.

=cut

sub retrieve_pseudofam_xml : Private {
  my ( $this, $c ) = @_;

  if ( not defined $this->{_ua} ) {
    $c->log->debug( 'Family::retrieve_pseudofam_xml: building a new user agent' )
      if $c->debug;
    $this->{_ua} = LWP::UserAgent->new;
    $this->{_ua}->timeout(10);
    $this->{_ua}->env_proxy;
  }

  my $response = $this->{_ua}->get( $this->{pseudofam_ws_url} );

  if ( $response->is_success ) {
    $c->log->debug( 'Family::retrieve_pseudofam_xml: successful response from web service' )
      if $c->debug;
    $c->stash->{pseudofam_xml} = $response->decoded_content;
  }
  else {
    $c->log->debug( 'Family::retrieve_pseudofam_xml: got an error from web service' )
      if $c->debug;
    $c->stash->{error} = $response->status_line;
  }
}

#-------------------------------------------------------------------------------

=head2 get_wikipedia : Private

Retrieves the wikipedia content, if any, for this family.

=cut

sub get_wikipedia : Private {
  my ( $this, $c ) = @_;

  my @articles = $c->model('WebUser::ArticleMapping')
                   ->search( { accession => $c->stash->{acc} },
                             { join     => [ 'wikitext' ],
                               prefetch => [ 'wikitext' ] } );

  return unless scalar @articles;

  $c->log->debug( 'Family::get_wikipedia: found ' . scalar @articles . ' articles' )
    if $c->debug;

  $c->stash->{articles} = \@articles;
}

=head2 get_wikipedia_title_role : Private

Retrieves the wikipedia title, if any, for this family.

=cut

sub get_wikipedia_title_role : Private {
  my ( $this, $c ) = @_;

  my @articles = $c->model('WebUser::ArticleMapping')
                   ->search( { accession => $c->stash->{acc} },
                             { join     => [ 'wikitext' ],
                               prefetch => [ 'title' ] } );

  return unless scalar @articles;

  $c->log->debug( 'Family::get_wikipedia_title_role: found ' . scalar @articles . ' articles' )
    if $c->debug;

  $c->stash->{titles} = \@articles;
}
#-------------------------------------------------------------------------------

=head2 get_logo : Private

Retrieves the HMM logo for this family, either from cache or the DB. Returns
the logo, if found.

=cut

sub get_logo : Private {
  my ( $this, $c ) = @_;

  my $cache_key = 'logo' . $c->stash->{acc};
  my $logo = $c->cache->get( $cache_key );

  if ( defined $logo ) {
    $c->log->debug( 'Family::FamilyActions::logo: extracted logo from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::FamilyActions::logo: failed to extract logo from cache; going to DB' )
      if $c->debug;

    my $rs = $c->model('PfamDB::PfamaHmm')
               ->find( $c->stash->{pfam}->pfama_acc );
    $logo = $rs->logo;

    unless ( defined $logo ) {
      $c->log->debug( 'Family::FamilyActions::logo: failed to retrieve logo from DB' )
        if $c->debug;

      $c->res->status( 204 );

      return;
    }

    # cache the LOGO
    $c->cache->set( $cache_key, $logo ) unless $ENV{NO_CACHE};
  }

  return $logo;
}

#-------------------------------------------------------------------------------
#- tree-related private actions ------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_tree : Private

Builds the TreeFam tree object for the specified family and alignment type
(seed or full). We first check the cache for the pre-built tree object and
then fall back to the database if it's not already available in the cache.

=cut

sub get_tree : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'tree' . $c->stash->{acc} . $c->stash->{alnType};
  my $tree     = $c->cache->get( $cacheKey );

  if ( defined $tree ) {
    $c->log->debug( 'Family::Tree::get_tree: extracted tree from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Tree::get_tree: failed to extract tree from cache; going to DB' )
      if $c->debug;

    # get a new tree object...
    $tree = treefam::nhx_plot->new( -width => 600,
                                    -skip  => 14 );

    # retrieve the tree from the DB
    $c->forward( 'get_tree_data' );
    return unless defined $c->stash->{treeData};

    # parse the data
    eval {
      $tree->parse( $c->stash->{treeData} );
    };
    if( $@ ) {
      $c->log->error( 'Family::Tree::get_tree: ERROR: failed to parse '
                      . $c->stash->{alnType} . ' tree for '
                      . $c->stash->{acc} . ": $@" );
      return;
    }

    # and now cache the populated tree object
    $c->cache->set( $cacheKey, $tree ) unless $ENV{NO_CACHE};
  }

  $c->stash->{tree} = $tree;
}

#-------------------------------------------------------------------------------

=head2 get_tree_data : Private

Retrieves the raw tree data. We first check the cache and then fall back to the
database.

=cut

sub get_tree_data : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'treeData' . $c->stash->{acc} . $c->stash->{alnType};
  my $treeData = $c->cache->get( $cacheKey );

  if( defined $treeData ) {
    $c->log->debug( 'Family::Tree::get_tree_data: extracted tree data from cache' )
      if $c->debug;
  } else {
    $c->log->debug( 'Family::Tree::get_tree_data: failed to extract tree data from cache; going to DB' )
      if $c->debug;

    # retrieve the tree from the DB
    my $rs = $c->model('PfamDB::AlignmentAndTree')
               ->search( { pfama_acc  => $c->stash->{pfam}->pfama_acc,
                           type       => $c->stash->{alnType} } );

    return unless defined $rs;

    my $row = $rs->first;
    return unless defined $row;

    my $tree = $row->tree;
    return unless defined $tree;

    # make sure we can uncompress it
    $treeData = Compress::Zlib::memGunzip( $tree );
    unless ( defined $treeData ) {
      $c->log->error( 'Family::Tree::get_tree_data: ERROR: failed to uncompress '
                      . $c->stash->{alnType} . ' tree data for '
                      . $c->stash->{acc} );
      return;
    }

    # and now cache the populated tree data
    $c->cache->set( $cacheKey, $treeData ) unless $ENV{NO_CACHE};
  }

  # stash the uncompressed tree
  $c->stash->{treeData} = $treeData;
}

#-------------------------------------------------------------------------------

=head2 get_das_alignment : Private

Retrieves a family alignment, seed or full, from the DAS sources.

=cut

sub get_das_alignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Family::get_das_alignment: retrieving alignment' )
    if $c->debug;

  # set the DAS dsn based on the alignment type parameter
  my $dsn = ( $c->stash->{alnType} eq 'seed' )
            ? $this->{urls}->{seed}
            : $this->{urls}->{full};

  if ( $c->debug ) {
    $c->log->debug( 'Family::get_das_alignment: dsn:  |' . $dsn . '|' );
    $c->log->debug( 'Family::get_das_alignment: acc:  |' . $c->stash->{acc} . '|' );
    $c->log->debug( 'Family::get_das_alignment: rows: |' . $c->stash->{rows} . '|' );
  }

  # retrieve the DasLite client from the base model class and hand it the DSN
  my $dl = $c->model('PfamDB')->getDasLite;
  $dl->dsn( $dsn );

  # put the rows specification into the right format for DAS
  my $rows = $c->stash->{rows}->[0] . '-' . $c->stash->{rows}->[1];

  # retrieve the raw alignment fragment and associated features via DAS and
  # generate the consensus sequence

  # retrieve the raw alignment from the DAS source
  my $raw_alignment  = $dl->alignment( { query => $c->stash->{acc},
                                         rows  => $rows } );

  # build the marked-up alignment
  my ( $alignment, $alignment_lengths ) = reconstruct_alignment( $raw_alignment );

  # retrieve the features
  my $features_hash = $dl->features( $c->stash->{acc} );

  my ( $source, $features ) = each %$features_hash;
  my $label = $features->[0]->{feature_label};

  # build the consensus string
  my $consensus = [ Bio::Pfam::ColourAlign::parseConsensus( $label ) ];

  # stash the arrays of alignments, alignment lengths and consensus strings
  $c->stash->{alignments}->{rawAlignments} = $alignment;
  $c->stash->{alignments}->{lengths}       = $alignment_lengths;
  $c->stash->{alignments}->{consensus}     = $consensus;
}

#-------------------------------------------------------------------------------

=head2 get_alignment_from_db : Private

Retrieves the complete alignment ("seed", "full", etc.) from the database.

=cut

sub get_alignment_from_db : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the raw alignment from the cache first
  my $cacheKey  = 'alignment' . $c->stash->{acc} . $c->stash->{alnType};
  my $alignment = $c->cache->get( $cacheKey );

  if ( defined $alignment ) {
    $c->log->debug( 'Family::get_alignment_from_db: extracted alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::get_alignment_from_db: failed to extract alignment from cache; going to DB' )
      if $c->debug;

    # retrieve the alignment from the DB
    my $row = $c->model('PfamDB::AlignmentAndTree')
                ->search( { pfama_acc  => $c->stash->{pfam}->pfama_acc,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single;

    unless ( defined $row and defined $row->alignment ) {

      $c->log->warn( 'Family::get_alignment_from_db: failed to retrieve '
        . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress it
    $alignment = Compress::Zlib::memGunzip( $row->alignment );
    unless ( defined $alignment ) {

      $c->log->warn( 'Family::get_alignment_from_db: failed to uncompress '
        . $c->stash->{alnType} . ' alignment for ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # cache the raw alignment
    $c->cache->set( $cacheKey, $alignment ) unless $ENV{NO_CACHE};
  }

	# store the raw text alignment
	$c->stash->{alignment} = $alignment;

  # we need the alignment as an array ref, so...
  my @alignment = split /\n/, $alignment;
  $c->stash->{alignment_rows} = \@alignment;

  $c->log->debug( 'Family::get_alignment_from_db: got '
                  . scalar @alignment . ' rows in alignment' ) if $c->debug;
}

#-------------------------------------------------------------------------------
#- alignment-related private actions -------------------------------------------
#-------------------------------------------------------------------------------

=head2 getAlignment : Private

Builds an alignment of the selected sequences.

=cut

sub getAlignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Family::getAlignment: retrieving alignment...' )
    if $c->debug;

  # first get a job ID. The call to retrieve results will get the job ID for
  # itself, but we'll need it here anyway
  my( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/i;

  unless( defined $jobId ) {
    $c->log->debug( 'Family::getAlignment: no job ID found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No job ID found for the sequence alignment job.';
    return;
  }

  # retrieve the job results
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::getAlignment: no results found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    return;
  }

#  $c->log->debug( 'Family::etAlignment: job results: |'
#                  . $c->stash->{results}->{$jobId}->{rawData} . '|' );

  # the rawData is just a string containing the alignment lines
  my @alignmentRows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};

  # the consensus string is the last row of the alignment
  my $consensusString = pop @alignmentRows;
  $consensusString =~ s/^ConSeq\s+(\S+)$/$1/;

  # take a slice of that array, based on the "rows" setting from PfamViewer.
  # Rows are numbered from 1, not zero, so we need to offset the row values
  my $from = $c->stash->{rows}->[0] - 1;
  my $to   = $c->stash->{rows}->[1] - 1;
  #$c->log->debug( 'Family::getAlignment: showing rows |'
  #                . "$from| to |$to|" );

  my %alignment;
  my $length;
  foreach ( @alignmentRows[ $from .. $to ] ) {
    next unless m|(\S+/\d+\-\d+)\s+(\S+)|;
    $alignment{$1} = $2;
    $length++;
  }

  # parse the consensus string
  my $consensus = Bio::Pfam::ColourAlign::parseConsensus( $consensusString );

  # stash everything
  $c->stash->{alignments}->{rawAlignments} = [ \%alignment ];
  $c->stash->{alignments}->{lengths}       = [ $length ];
  $c->stash->{alignments}->{consensus}     = [ $consensus ];
}

#-------------------------------------------------------------------------------

=head2 queueAlignment : Private

Queues the job that will actually generate the sequence alignment.

=cut

sub queueAlignment : Private {
  my($this, $c) = @_;

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # set the options
  my $opts = '-acc ' . $c->stash->{acc} . '.' . $c->stash->{pfam}->version;

  # guesstimate the time it will take to build the alignment
  my $estimatedTime = int( 1 + ( $c->stash->{numRows} / 100 ) );

  # add this job to the tracking tables
  my $jobHistory = $c->model('WebUser::JobHistory')
                     ->create( { options        => $opts,
                                 job_type       => 'align',
                                 estimated_time => $estimatedTime,
                                 job_id         => $jobId,
                                 opened         => \'NOW()',
                                 status         => 'PEND' } );

  my $jobStream = $c->model('WebUser::JobStream')
                    ->create( { id    => $jobHistory->id,
                                stdin => $c->stash->{fasta} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $jobHistory->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side. Because the queuing system allows
  # multiple Jobs in one page, the jobStatus JSON string needs to be an array
  # of hashes, each of which gives details of a separate job
  my $jobStatus = [
                    {
                      checkURI      => $c->secure_uri_for( '/jobmanager/checkStatus' ),
                      doneURI       => $c->secure_uri_for( '/family/'.$c->stash->{acc}.'/alignment/view' ),
                      estimatedTime => $estimatedTime,
                      interval      => $this->{pollingInterval},
                      jobId         => $jobId,
                      name          => 'Sequence alignment',
                      jobClass      => 'alignment',
                      opened        => $historyRow->opened,
                    }
                  ];
  $c->stash->{jobStatusJSON} = to_json( $jobStatus );

  $c->log->debug( 'Family::queueAlignment: job status: ',
                  dump( $jobStatus ) ) if $c->debug;
  $c->log->debug( 'Family::queueAlignment: submitted job '
                  . "|$jobId| at |" . $historyRow->opened . '|' ) if $c->debug;

  return 0;
}

#-------------------------------------------------------------------------------
#- regular perl methods (not actions) ------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 reconstruct_alignment

Reconstructs a blocked alignment from a raw alignment.

=cut

sub reconstruct_alignment {
  my $ali = shift;
  my ( $source, $aliData) = each %$ali;

  my ( @alignments, @alignmentLengths );

  for ( my $i = 0; $i < scalar( @$aliData ); $i++ ) {
    my %aliObjects =
      map{ $_->{alignobject_intObjectId} => $_ } @{ $aliData->[$i]->{alignobject} };

    push @alignmentLengths, $aliData->[$i]->{alignment_max};

    foreach my $block ( sort { $a->{block_blockOrder} <=> $b->{block_blockOrder} }
                             @{$aliData->[$i]->{block} } ) {
      my %ali;
      foreach my $bseqRef (@{ $block->{segment} } ) {

        my $key = $bseqRef->{segment_intObjectId} . '/' .
                  $bseqRef->{segment_start}       . '-' .
                  $bseqRef->{segment_end};

        $ali{$key} = get_alignment_string($bseqRef, \%aliObjects);
      }
      push @alignments, \%ali;
    }
  }
  return \@alignments, \@alignmentLengths;
}

#-------------------------------------------------------------------------------

=head2 get_alignment_string

Gets the alignment string from the alignment.

=cut

sub get_alignment_string {
  my ( $bseqRef, $aliObjectsRef ) = @_;

  my $seqStr = $aliObjectsRef->{ $bseqRef->{segment_intObjectId} }->{sequence};

  my $seq = substr( $seqStr,
                    $bseqRef->{segment_start} - 1,
                    $bseqRef->{segment_end} - $bseqRef->{segment_start} + 1 );

  return cigar_to_alignment( $bseqRef->{cigar}, $seq );
}

#-------------------------------------------------------------------------------

=head2 cigar_to_alignment

Converts a cigar string into an alignment row.

=cut

sub cigar_to_alignment {
  my $cigar = shift;

  $cigar =~ s/\"//g;

  my $seq = shift;
  my $tmp = $cigar;
  my $start = 0;
  my $len = length($seq);

  $tmp =~ s/(\d+)D/'-'x$1/eg;
  $tmp =~ s/D/\-/g;
  $tmp =~ s/(\d+)I/'.'x$1/eg;
  $tmp =~ s/I/\./g;

  $tmp =~ s/(\d{0,5})M/if($1){$start+=$1,($start<=$len)?substr($seq,$start-$1,$1):'~'x$1}else{$start+=1,($start<=$len)?substr($seq,$start-1,1):'~'}/eg;

  return $tmp;
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
