# MetaDataset.pm
# jt6 20080508 WTSI
#
# $Id: MetaDataset.pm,v 1.1 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::MetaDataset - a controller to handle metaseq datasets

=cut

package PfamWeb::Controller::MetaDataset;

=head1 DESCRIPTION

Controller overview.

$Id: MetaDataset.pm,v 1.1 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'metadataset' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Description...

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;
  
  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  if ( defined $c->req->param('output') and
       $c->req->param('output') eq 'xml' ) {
    $c->stash->{output_xml} = 1;
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('entry') ||
                      $entry_arg              ||
                      '';
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w-]+)$/;
    $c->stash->{errorMsg} = 'Invalid metaseq dataset name' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No metaseq dataset name specified';
  }
  
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves the data for the specified metaseq dataset.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  my $rs = $c->model('Metagenomics_refs')
             ->find( { source => $entry } );
             
  if ( not defined $rs ) {
    $c->log->debug( "MetaDataset::get_data: no such source '$entry'" )
      if $c->debug;
    return;
  }
  
  $c->log->debug( "MetaDataset::get_data: found source '$entry'" )
    if $c->debug;

  $c->stash->{entry} = $rs;  
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