
# Dead.pm
# jt6 20070830 WTSI
#
# $Id: Dead.pm,v 1.1 2007-08-30 09:26:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Dead - a simple controller for dead families

=cut

package PfamWeb::Controller::Dead;

=head1 DESCRIPTION

A controller to deal with dead Pfam-A families.

$Id: Dead.pm,v 1.1 2007-08-30 09:26:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Picks up the accession for the dead family and retrieves that row of the table.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  my $rs;
  if( defined $c->req->param('acc') and
      $c->req->param('acc') =~ m/^(PF\d{5})$/i ) {

    $c->log->debug( "Dead::begin: found a Pfam-A accession ($1)" );
    $rs = $c->model('PfamDB::Dead_families')
            ->find( { pfamA_acc => $1 } );

  } elsif( defined $c->req->param('id') and
           $c->req->param('id') =~ /^([\w_-]+)$/ ) {
  
    $c->log->debug( "Dead::begin: found a Pfam-A ID ($1)" );
    $rs = $c->model('PfamDB::Dead_families')
            ->find( { pfamA_id => $1 } );

  } elsif( defined $c->req->param('entry') ) {
    
    $c->log->debug( "Dead::begin: found a Pfam-A entry ($1)" );
    if( $c->req->param('entry') =~ m/^(PF\d{5})$/i ) {

    $c->log->debug( "Dead::begin: entry looks like an accession ($1)" );
      $rs = $c->model('PfamDB::Dead_families')
              ->find( { pfamA_acc => $1 } );
              
    } elsif( $c->req->param('entry') =~ /^([\w_-]+)$/ ) {

    $c->log->debug( "Dead::begin: entry looks like an ID ($1)" );
      $rs = $c->model('PfamDB::Dead_families')
              ->find( { pfamA_id => $1 } );         
    }  
  }

  if( defined $rs ) {
    $c->log->debug( 'Dead::begin: got a row' );
    $c->stash->{pfam} = $rs;
  }
}

#-------------------------------------------------------------------------------

=head2 dead : Path

A simple action that presents a page with a link to redirect to the family
that replaces this one.

=cut

sub dead : Path {
  my( $this, $c ) = @_;
  
  # before handing off to the template, set a refresh URI that will be picked 
  # up by head.tt and used in a meta refresh element
  if( defined $c->stash->{pfam} and
      $c->stash->{pfam}->forward_to ) {
    
    $c->log->debug( 'Dead::dead: refresh URI pointing to |'
                    . $c->stash->{pfam}->forward_to . '|' );

    $c->stash->{refreshUri} =
      $c->uri_for( '/family', { acc => $c->stash->{pfam}->forward_to } );

  } else {
    $c->log->debug( 'Dead::dead: refresh URI pointing to /' );
    $c->stash->{refreshUri} = $c->uri_for( '/' );
  }

  # hand off to the template
  $c->stash->{template} = 'pages/dead.tt';
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;