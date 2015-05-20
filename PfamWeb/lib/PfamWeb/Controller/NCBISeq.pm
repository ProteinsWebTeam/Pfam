
# NCBISeq.pm
# jt6 20071010 WTSI
#
# $Id: NCBISeq.pm,v 1.6 2009-12-07 22:27:57 jt6 Exp $

=head1 NAME

PfamWeb::Controller::NCBISeq - controller to build pages for sequences with
NCBI identifiers

=cut

package PfamWeb::Controller::NCBISeq;

=head1 DESCRIPTION

Generates a B<tabbed page>.

$Id: NCBISeq.pm,v 1.6 2009-12-07 22:27:57 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Section';

__PACKAGE__->config( SECTION => 'ncbiseq' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the entry.

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

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
                      $c->req->param('gi')    ||
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
    $c->stash->{errorMsg} = 'Invalid ncbiseq accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No ncbiseq accession or ID specified';
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

Retrieves data for the given NCBI GI number.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # use "esl-sfetch" to try to retrieve a sequence from the NCBI sequence
  # file
  my $opened = open( NCBI, join ' ', ( $this->{sfetchBinary}, $this->{ncbiSeqFile}, $entry, '|' ) );

  unless ( $opened ) {
    $c->log->debug( "NCBISeq::get_data: problem running esl-sfetch: $!" )
      if $c->debug;

    $c->stash->{errorMsg} = 'Could not open the NCBI sequence file';

    return;
  }
    
  my @ncbiseq = <NCBI>;
  close NCBI;
 
  unless ( scalar @ncbiseq ) {
    $c->log->debug( 'NCBISeq::get_data: no sequence found with given GI' )
      if $c->debug;

    $c->stash->{errorMsg} = 'No valid ncbiseq accession or ID';

    return;
  }
  
  $c->log->debug( 'Ncbiseq::get_data: got a ncbiseq entry' )
    if $c->debug;

  $c->stash->{gi} = $entry;

  # parse the FASTA header to get the secondary accession and description
  my $header = shift @ncbiseq;
  $c->stash->{sequence} = join '', @ncbiseq;
  $c->stash->{sequence} =~ s/\n//g;

  ( $c->stash->{secondary_acc}, $c->stash->{desc} ) = 
    ( $header =~ m/^>\s*\d+\s+ref\|(.*?)\s+(.*)/ );

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
