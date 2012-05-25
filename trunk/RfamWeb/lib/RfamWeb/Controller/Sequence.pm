
# Sequence.pm
# jt6 20081205 WTSI
#
# $Id: Sequence.pm,v 1.2 2009-01-06 11:52:29 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Sequence- controller to build the page for a sequence

=cut

package RfamWeb::Controller::Sequence;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Rfam
sequences.

$Id: Sequence.pm,v 1.2 2009-01-06 11:52:29 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Section' => { -excludes => 'section' };

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 sequence_search : Chained PathPart Args

The entry point for a sequence search. We check for a parameter, "lookup", which
we detaint and stash. We then forward straight to L<sequence_page>, which will
retrieve and present the results.

Start and end of a chain.

=cut

sub sequence_search : Chained( '/' ) 
                      PathPart( 'sequence' ) 
                      Args( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Sequence::sequence_search: searching for Rfam hits to a sequence' )
    if $c->debug;

  unless ( defined $c->req->param('entry') ) { 
    $c->log->debug( 'Sequence::sequence_search: no sequence accession found' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'You must give a sequence accession';
    
    return;
  }

  my ( $entry ) = $c->req->param('entry') =~ m/^(\w+)(\.\d+)?$/;
  
  unless ( defined $entry ) { 
    $c->log->debug( 'Sequence::sequence_search: no valid sequence accession found' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'You must give a valid sequence accession';
    
    return;
  }

  $c->forward( 'sequence_page', [ $entry ] );
}

#-------------------------------------------------------------------------------

=head2 sequence_page : Chained PathPart Args

Action to build a page showing all Rfam hits for a given sequence entry. Takes
the sequence accession/ID from the first argument.

Start and end of a chain.

=cut

sub sequence_page : Chained( '/' ) 
                    PathPart( 'sequence' ) 
                    Args( 1 ) {
  my ( $this, $c, $tainted_entry ) = @_;
  
  $c->log->debug( 'Sequence::sequence_page: building page of hits for a sequence' )
    if $c->debug;
    
  my ( $entry ) = $tainted_entry =~ m/^(\w+)(\.\d+)?$/;

  unless ( defined $entry ) {
    $c->log->debug( 'Sequence::sequence_page: no valid sequence accession found' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'You must supply a valid sequence accession';
    
    return;
  }

  $c->log->debug( "Sequence::sequence_page: looking up hits for |$entry|" )
    if $c->debug;
    
  $c->stash->{entry} = $entry;
  $c->forward( 'get_data' );
  
  $c->stash->{template} = 'pages/search/sequence/lookup.tt';
}

#-------------------------------------------------------------------------------

=head2 sequence_link : Chained PathPart CaptureArgs

Captures the first argument as a sequence accession/ID.

Part of a chain.

=cut

sub sequence_link : Chained( '/' ) 
                    PathPart( 'sequence' ) 
                    CaptureArgs( 1 ) {
  my ( $this, $c, $tainted_entry ) = @_;
  
  $c->log->debug( 'Sequence::sequence_link: checking sequence ID/acc' )
    if $c->debug;
    
  my ( $entry ) = $tainted_entry =~ m/^(\w+)(\.\d+)?$/;

  unless ( defined $entry ) {
    $c->log->debug( 'Sequence::sequence_link: no valid sequence accession found' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'You must provide a valid sequence accession';
    
    return;
  }

  $c->log->debug( "Sequence::sequence_link: looking up hits for |$entry|" )
    if $c->debug;
    
  $c->stash->{entry} = $entry;
}

#-------------------------------------------------------------------------------

=head2 sequence_table : Chained PathPart Args

Forwards to a private action which retrieves the Rfam hits for the (stashed)
sequence entry. Builds just the 

End of a chain. 

=cut

sub sequence_table : Chained( 'sequence_link' ) 
                     PathPart( 'hits' ) 
                     Args( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Sequence::sequence_table: looking up Rfam hits to a sequence' )
    if $c->debug;
    
  $c->forward( 'get_data' );                  

  $c->stash->{template} = 'pages/search/sequence/lookup_table.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves Rfam hits for the (stashed) sequence entry.

=cut

sub get_data : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Sequence::get_data: retrieving Rfam hits for ' . $c->stash->{entry} )
    if $c->debug;
    
  unless ( defined $c->stash->{entry} ) {
    $c->log->debug( 'Sequence::get_data: no valid sequence accession found in stash' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'You must use a valid sequence accession';
    
    return;
  }

  my @hits = $c->model('RfamDB::RfamRegFull')
               ->search( { 'auto_rfamseq.rfamseq_acc' => $c->stash->{entry} },
                         { 
                           join      => [ 'auto_rfam',
                                           { 'auto_rfamseq' => 'ncbi_id' } ],
                           '+select' => [ qw( auto_rfam.rfam_id
                                              auto_rfam.rfam_acc
                                              auto_rfam.description
                                              auto_rfamseq.rfamseq_acc
                                              auto_rfamseq.description
                                              ncbi_id.tax_string
                                              ncbi_id.species ) ],
                           '+as'     => [ qw( rfam_id
                                              rfam_acc
                                              rfam_desc
                                              rfamseq_acc
                                              rfamseq_desc
                                              rfamseq_taxonomy
                                              rfamseq_species ) ],
                           order_by  => 'seq_start'
                         } );

  $c->log->debug( 'Sequence::get_data: found |' . scalar @hits. '| hits for |'
                  . $c->stash->{entry} . '|' ) if $c->debug;
                  
  $c->stash->{results}  = \@hits;
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
