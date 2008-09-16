
# PfamGraphicsTools.pm
# jt 20070402 WTSI
#
# $Id: PfamGraphicsTools.pm,v 1.6 2008-09-16 11:10:48 jt6 Exp $

=head1 NAME

PfamWeb::Controller::PfamGraphicsTools - tools for drawing Pfam graphics

=cut

package PfamWeb::Controller::PfamGraphicsTools;

=head1 DESCRIPTION

A couple of utility methods for generating Pfam graphics from a user-supplied
XML file and for displaying the XML that builds the graphic for a specified 
UniProt entry.

$Id: PfamGraphicsTools.pm,v 1.6 2008-09-16 11:10:48 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );
use File::Temp qw( tempfile );

use XML::LibXML;

use Storable qw(thaw);
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generateGraphic : Global

Just displays the upload form.

=cut

sub generateGraphic : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamGraphicsTools::generateGraphic: generating upload form' );
  $c->stash->{template} = 'pages/uploadXml.tt';

}

#-------------------------------------------------------------------------------

=head2 generateUniprotGraphic : Global

Displays the graphic for a particular UniProt entry.

=cut

sub generateUniprotGraphic : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamGraphicsTools::generateUniprotGraphic: generating form' );
  $c->stash->{template} = 'pages/uploadUniprotEntry.tt';
  
}

#-------------------------------------------------------------------------------

=head2 renderXML : Global

Reads the uploaded XML file and tries to render the Pfam graphic that it
describes.

=cut

sub renderXML : Global {
  my( $this, $c ) = @_;

  # see if the upload worked, whatever that means
  my $upload;
  unless( $upload = $c->request->upload( 'XMLupload' ) ) {
    $c->log->error( 'PfamGraphicsTools::renderXML: upload failed' );
    $c->stash->{error} = 'There was a problem accepting your upload.';
    $c->stash->{template} = 'pages/uploadXml.tt';
    return 0;
  }
  $c->log->debug( 'PfamGraphicsTools::renderXML: 1) we retrieved the upload from the request' );
   
  #----------------------------------------

  # copy the file to our temp area. Generate a temporary filehandle and
  # and filename, rather than using the user specified one
  ( my $dir ) = $this->{uploadDir} =~ m/^(.*)/;
  my($fh, $filename) = tempfile( 'uploadedPfamGraphicXXXXXXXXXXXX',
                                 SUFFIX => '.xml',
                                 DIR    => $this->{uploadDir} );

  $c->log->debug( "PfamGraphicsTools::renderXML: copying uploaded file to |$filename|" );

  unless( $upload->copy_to( $filename ) ) {
    $c->log->error( "PfamGraphicsTools::renderXML: couldn't copy the uploaded file" );
    $c->stash->{error} = 'There was a problem accepting your upload.';
    $c->stash->{template} = 'pages/uploadXml.tt';
    return 0;
  }
  $c->log->debug( 'PfamGraphicsTools::renderXML: 2) successfully copied the uploaded file' );

  #----------------------------------------

  # ok; the upload seems to have worked

  # try validating the uploaded file against our schema
  if( not $c->forward( 'validateXML', [ $filename ] ) ) {
    $c->log->error( 'PfamGraphicsTools::renderXML: there were validation errors.' );
    $c->stash->{template} = 'pages/uploadXml.tt';
    return 0;  
  }
  $c->log->debug( 'PfamGraphicsTools::renderXML: 3) the uploaded XML validated successfully' );

  # validation will drop the XML document into the stash
  
  #----------------------------------------

  # at this point we should have a valid XML file
  
  # render the graphic
  my $imageSet = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageSet->create_images( $c->stash->{xmlDocument}, 1 );

  if( not defined $imageSet ) {
    $c->log->error( 'PfamGraphicsTools::renderXML: image generation failed' );
    $c->stash->{error} = 'There was a problem generating the Pfam graphic from your XML.';
    $c->stash->{template} = 'pages/uploadXml.tt';
    return 0;
  }
  $c->log->debug( 'PfamGraphicsTools::renderXML: 4) we *might* have generated an image...' );

  $c->stash->{xml}      = $c->stash->{xmlDocument}->toString( 1 );
  $c->stash->{imageSet} = $imageSet;

  # hand off to another action, which will decide whether to return the raw image
  # or to render the specified template
  $c->stash->{template} = 'pages/generatedGraphic.tt';
  $c->forward( 'returnGraphic' );
}

#-------------------------------------------------------------------------------

=head2 renderUniprotGraphic : Global

Renders the Pfam graphic for a specified UniProt sequence entry.

=cut

sub renderUniprotGraphic : Global {
  my( $this, $c ) = @_;

  my $p;

  # first, make sure this is a valid UniProt ID or accession
  if( $c->req->param( 'entry' ) =~ m/^([OPQ]\d[A-Z0-9]{3}\d)$/i ) {
    $c->log->debug( "PfamGraphicsTools::renderUniprotGraphic: looks like a sequence accession: |$1|" );
    $p = $c->model('PfamDB::Pfamseq')
           ->find( { pfamseq_acc => $1 } );

    # if we got a result there, so much the better...
    unless( defined $p ) {
  
      # ... otherwise, see if this is really a secondary accession
      $p = $c->model('PfamDB::Secondary_pfamseq_acc')
        ->find( { secondary_acc => $1 },
                { join          => [ qw( pfamseq ) ],
  			        	prefetch      => [ qw( pfamseq ) ] } );
    }

  } elsif( $c->req->param( 'entry' ) =~ m/^(\w+_\w+)$/ ) {
    $c->log->debug( "PfamGraphicsTools::renderUniprotGraphic: looks like a sequence ID: |$1|" );
    $p = $c->model('PfamDB::Pfamseq')
           ->find( { pfamseq_id => $1 } );

  } else {
    $c->log->debug( 'PfamGraphicsTools::renderUniprotGraphic: no valid sequence identifier specified' );
    $c->stash->{error} = 'No valid UniProt accession or ID given.';
    $c->stash->{template} = 'pages/uploadUniprotEntry.tt';
    return 0;
  }
  
  # now we should have a Pfamseq object
  
  # stash the pfamseq object for the template
  $c->stash->{pfamseq} = $p;

  # start generating a graphic for it...
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  
  # retrieve the Storable containing the annotated sequence, thaw it
  # and hand it off to the layout manager
  my $annseq;
  eval {
    $annseq = thaw( $p->annseq->annseq_storable );
  };
  if ($@) {
    $c->log->error( 'PfamGraphicsTools::renderUniprotGraphic: ERROR: failed to thaw annseq: $@' );
    $c->stash->{error} = 'There was a problem getting the sequence information for that entry.';
    $c->stash->{template} = 'pages/uploadUniprotEntry.tt';
    return 0;
  }

  $layoutPfam->layout_sequences_with_regions_and_features( [ $annseq ],
                                                           { PfamA      => 1,
                                                             PfamB      => 1,
                                                             noFeatures => 0 } );

  # and build an imageset
  my $imageSet = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageSet->create_images( $layoutPfam->layout_to_XMLDOM );
  $c->stash->{xml} = $layoutPfam->layout_to_XMLDOM->toString( 1 );
  $c->stash->{imageSet} = $imageSet;

  $c->stash->{template} = 'pages/generatedUniprotGraphic.tt';
  $c->forward( 'returnGraphic' );

}

#-------------------------------------------------------------------------------

=head2 end : Private

Hands off to TT to render the template that was specified earlier.

=cut

sub end : ActionClass('RenderView') {}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 returnGraphic : Private

Checks the "image_only" parameter and decides whether to hand back the raw
image or to render a page.

=cut

sub returnGraphic : Private {
  my( $this, $c ) = @_;

  # print the image. That is, write it to disk
  my( $image )= $c->stash->{imageSet}->each_image;
  $image->print_image;

  # should we return just the image ?
  if( $c->req->param( 'image_only' ) ) {
    my $imageFile = $c->config->{'View::TT'}->{CONSTANTS}->{tmp}
                    . '/' . $image->file_location;
    my $imageURI = $c->uri_for( "/$imageFile" );
    $c->log->debug("PfamGraphicsTools::returnGraphic: returning image URI: |$imageURI|" )
      if $c->debug;
    $c->res->redirect( $imageURI, 301 );
    return 1;
  } else {
    $c->stash->{image} = $image;
  }

}

#-------------------------------------------------------------------------------

=head2 validateXML : Private

Validates an XML file against the schema. Returns true if the schema validates
and false otherwise. Adds error messages to the stash if there's a problem.

Stashes the L<XML::LibXML::Document|Document> object on the way past.

=cut

sub validateXML : Private {
  my( $this, $c, $xmlFile ) = @_;
  
  $c->log->debug( 'PfamGraphicsTools::validateXML: retrieving schema from: |'
                  . $this->{schemaFile} . '|' ) if $c->debug;

  # parse the schema
  # TODO cache the Schema object
  my $schema;
  eval {
    $schema = XML::LibXML::Schema->new( location => $this->{schemaFile} );
  };
  if( $@ or not defined $schema ) {
    $c->log->error( "PfamGraphicsTools::validateXML: couldn't get a valid schema object..." );
    $c->stash->{error} = 'There was a problem validating your XML.';
    return 0;
  }
  $c->log->debug( 'PfamGraphicsTools::validateXML: got a Schema object' );  
  
  $c->log->debug( "PfamGraphicsTools::validateXML: loading XML from |$xmlFile|" )
    if $c->debug;

  # parse the XML Document and stash it so we can use it later without re-parsing
  my $parser = XML::LibXML->new;
  eval {
    $c->stash->{xmlDocument} = $parser->parse_file( $xmlFile );

    $schema->validate( $c->stash->{xmlDocument} );
  };
  if( $@ ) {
    $c->log->error( "PfamGraphicsTools::validateXML: validation errors: |$@|" );
    $c->stash->{error} = 'Your XML was not valid.';
    return 0;
  }
  
  $c->log->debug( 'PfamGraphicsTools::validateXML: XML document was valid' );
  return 1;
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
