
# Metaseq.pm
# jt6 20071008 WTSI
#
# $Id: Metaseq.pm,v 1.7 2010-01-13 14:44:53 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Metaseq - controller to build pages for meta-genomics
sequences

=cut

package PfamWeb::Controller::Metaseq;

=head1 DESCRIPTION

Generates a page set for a metagenomics sequence.

$Id: Metaseq.pm,v 1.7 2010-01-13 14:44:53 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'metaseq' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the metaseq entry.

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

  $c->cache_page( 604800 );

  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') and
       $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
    $c->res->content_type('text/xml');    

    # enable CORS (see http://www.w3.org/wiki/CORS_Enabled)
    $c->res->header( 'Access-Control-Allow-Origin' => '*' );
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';

  # although these next checks might fail and end up putting an error message
  # into the stash, we don't "return", because we might want to process the 
  # error message using a template that returns XML rather than simply HTML
  # (XML output not yet implemented for metaseq data
  # jt6 20080603 WTSI.)
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)$/;
    $c->stash->{errorMsg} = 'Invalid metaseq accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No metaseq accession or ID specified';
  }

  # retrieve data for this entry
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;

  # if we need the domain graphic, which we don't for XML output, submit
  # the sequence for a sequence search
  if ( not $c->stash->{output_xml} ) {
    $c->stash->{data}->{altoutput} = 2;
    $c->stash->{data}->{seq}       = $c->stash->{sequence};
    $c->forward( qw/ PfamWeb::Controller::Search::Sequence search / );
  }
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves data for the given metagenomics identifier.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # use "esl-sfetch" to try to retrieve a sequence from the NCBI sequence
  # file
  my $opened = open( META, join ' ', ( $this->{sfetchBinary}, $this->{metaSeqFile}, $entry, '|' ) );

  unless ( $opened ) {
    $c->log->debug( "Metaseq::get_data: problem running esl-sfetch: $!" )
      if $c->debug;

    $c->stash->{errorMsg} = 'Could not open the metagenomics sequence file';

    return;
  }
    
  my @metaseq = <META>;
  close META;
 
  unless ( scalar @metaseq ) {
    $c->log->debug( 'Metaseq::get_data: no sequence found with given ID' )
      if $c->debug;

    $c->stash->{errorMsg} = 'No valid metagenomics sequence identifier';

    return;
  }
  
  $c->log->debug( 'Metaseq::get_data: got a metaseq entry' )
    if $c->debug;

  $c->stash->{id} = $entry;

  # parse the FASTA header to get the secondary accession and description
  my $header = shift @metaseq;
  $c->stash->{sequence} = join '', @metaseq;
  $c->stash->{sequence} =~ s/\n//g;

  ( $c->stash->{desc} ) = $header =~ m/^>\d+\s+\'(.*?)\'/;

  # add summary data for the icons, such as it is
  $c->stash->{summaryData} = {
    numSequences => 1,
    numArchitectures => 1,
    numSpecies => 0,
    numStructures => 0,
    numInt => 0,
  };

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

Andy Jenkinson, C<aj5@sanger.ac.uk>

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
