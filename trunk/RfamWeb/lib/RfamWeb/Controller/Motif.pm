
# Motif.pm
# jt6 20100302 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Controller::Motif - controller to build the main Rfam Motif page

=cut

package RfamWeb::Controller::Motif;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Rfam
motifs

Generates a B<tabbed page>.

$Id$

=cut

use Moose;
use namespace::autoclean;
use LWP::Simple;
#use Bio::Pfam::Wiki::Scraper;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Section' => { -excludes => 'section' },
     'RfamWeb::Roles::Motif::Methods',
     'RfamWeb::Roles::Motif::AlignmentMethods';

# set the name of the section
__PACKAGE__->config( SECTION => 'motif' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Extracts values from the parameters. Accepts "acc", "id" and "entry", in lieu
of having them as path components.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;
  
  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') and
       $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
    $c->res->content_type('text/xml');
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      '';

  if ( $tainted_entry ) {
    $c->log->debug( 'Motif::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w\._-]+)$/;
  }

  $c->stash->{db} = $c->model('RfamDB');

}

#-------------------------------------------------------------------------------

=head2 motif : Chained('/') PathPart('clan') CaptureArgs(1)

Starting point of a chain handling motif-related data. Retrieves motif information
from the DB. This is the way in if the motif acc/ID is given as an argument.

=cut

sub motif : Chained( '/' )
            PathPart( 'motif' )
            CaptureArgs( 1 ) {
  my ( $this, $c, $entry_arg ) = @_;
  
  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
                      '';

  $c->log->debug( "Motif::motif: tainted_entry: |$tainted_entry|" )
    if $c->debug;
  
  my ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;

  unless ( defined $entry ) {
    $c->log->debug( 'Motif::begin: no valid Rfam motif accession or ID' )
      if $c->debug;

    $c->stash->{errorMsg} = 'No valid Rfam motif accession or ID';

    return;
  }
  
  # retrieve data for the motif
  $c->forward( 'get_data', [ $entry ] );
}

#-------------------------------------------------------------------------------

sub motif_page : Chained( 'motif' )
                PathPart( '' )
                Args( 0 ) {
  my ( $this, $c ) = @_;

  # there was a problem retrieving motif data
  unless ( $c->stash->{motif} ) {
    $c->log->debug( 'Motif::begin: problem retrieving motif data' )
      if $c->debug;

    $c->stash->{errorMsg} ||= 'We could not find the data for the Rfam motif ' . $c->stash->{acc} .'.';
  }

  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Motif::begin: emitting XML' ) if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Motif::begin: there was an error: |' .  $c->stash->{errorMsg} . '|' )
        if $c->debug;

      $c->stash->{template} = 'rest/motif/error_xml.tt';

      return;
    }
    else {
      $c->stash->{template} = 'rest/motif/motif.tt'
    }
  
  }
  else {
    $c->log->debug( 'Motif::begin: emitting HTML; retrieving summary data' )
      if $c->debug;

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Motif::begin: there was an error: |' .  $c->stash->{errorMsg} . '|' )
        if $c->debug;

      $c->stash->{template} = 'components/blocks/motif/error.tt';

      return;

    }
  }


  $c->log->debug( 'Moitf::motif_page: adding summary info' )
    if $c->debug;
  $c->forward( 'get_summary_data' );


  $c->log->debug( 'Motif::motif_page: adding wikipedia info' ) 
    if $c->debug;
#  $c->forward( 'get_wikipedia' );
  
  $c->log->debug( 'Motif::motif_page: adding literature info' )
    if $c->debug;
  $c->forward( 'get_references' );

  $c->log->debug( 'Motif::motif_page: adding curation info' )
    if $c->debug;
  $c->forward( 'get_curation' );

  $c->log->debug( 'Motif::motif_page: adding families that match info' )
    if $c->debug;
  $c->forward( 'get_matches' );

  $c->log->debug( 'Motif::motif_page: adding structure info' )
    if $c->debug;
  $c->forward( 'get_motif_structures' );


  $c->cache_page( 604800 );
}


#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves motif data for the given entry. Accepts the entry ID or accession
as the first argument. Does not return any value but drops the L<ResultSet>
for the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # check for a motif
  my $rs = $c->model('RfamDB::Motif')
             ->search( [ { motif_acc => $entry },
                         { motif_id  => $entry } ] );
  
  my $motif = $rs->first if defined $rs;
  
  unless ( defined $motif ) {
    $c->log->debug( 'Motif::get_data: no row for that accession/ID' )
      if $c->debug;
  
    $c->stash->{errorMsg} = 'No valid Rfam motif accession or ID';

    return;
  }  

  $c->log->debug( 'Motif::get_data: got a motif' ) if $c->debug;

  $c->stash->{motif} = $motif;
  $c->stash->{acc}  = $motif->motif_acc;
  $c->stash->{entryType}  = 'C';

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
  
  my $rs = $c->model('RfamDB::MotifPdb')->search( { motif_acc => $c->stash->{acc} },{});
  $summaryData->{numStructures} = $rs->count;

  $c->stash->{summaryData} = $summaryData;

}


#-------------------------------------------------------------------------------

=head2 get_wikipedia : Private

Retrieves the wikipedia content, if any, for this motif

=cut

sub get_wikipedia : Private {
  my ( $this, $c ) = @_;

  my $scraper = new Bio::Pfam::Wiki::Scraper;
 
  #$scraper->{wiki_root} = 'http://en.wikipedia.org/w/index.php?action=raw&title=';  

   my $wikitext = $scraper->scrape('Tetraloop');

   return unless ( $wikitext );

   $c->stash->{wikitext} = $wikitext;

} 


#-------------------------------------------------------------------------------

=head2 get_references : Private

Retrieves the references, if any, for this motif

=cut

sub get_references : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('RfamDB::LiteratureReference')
                  ->search( { 'motif_literature_references.motif_acc' => $c->stash->{acc} },
                            { join     => 'motif_literature_references',
                              order_by => 'motif_literature_references.order_added' }
  );

  $c->stash->{motif_literature} = $rs;

}

#-------------------------------------------------------------------------------



=head2 get_curation : Private

Retrieves the curation data for this motif

=cut

sub get_curation : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('RfamDB::Motif')
                  ->search( { 
                    'motif_acc' => $c->stash->{acc} 
                  },)->next;

  $c->stash->{motif_curation} = $rs;

}

#-------------------------------------------------------------------------------

=head2 get_matches : Private

Retrieves the references, if any, for this motif

=cut

sub get_matches : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('RfamDB::MotifFamilyStat')
                  ->search( { 'motif_acc' => $c->stash->{acc} }, { order_by => { -asc => 'rfam_acc' }}
  );

  $c->stash->{motif_matches} = $rs;

}

#-------------------------------------------------------------------------------

=head2 get_motif structures : Private

Retrieves the references, if any, for this motif

=cut

sub get_motif_structures : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model('RfamDB::MotifPdb')
                  ->search( { 'motif_acc' => $c->stash->{acc} },
  );

  $c->stash->{motif_structures} = $rs;

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

1;
