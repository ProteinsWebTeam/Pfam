
# GetPDB.pm
# pg6 20091112
# Controller which gets the ligand pdb file from the ftp site
# 
# $Id: GetPDB.pm,v 1.2 2009-11-12 16:19:54 pg6 Exp $

=head1 Name

iPfamWeb::Controller::Ligand::GetPDB - Retrieves the PDB file of the ligand from the FTP site of MSD.

=cut

package iPfamWeb::Controller::Ligand::GetPDB;

use strict;
use warnings;

use LWP::Simple;
use Data::Dump qw( dump );

use base 'iPfamWeb::Controller::Ligand';


=head2 get_ligand_pdb

This action retrieves the ligand pdb from FTP site of MSD

=cut

sub getpdb: Path {
  my( $self, $c ) = @_;
  
  unless( defined $c->req->param( 'id') ){
    $c->log->debug( "Ligand:GetPDB::getPDB: we did some mistake as three_letter_code of the ligand is not found." );
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
    return;
  }
  
  my ( $pdb_id ) = $c->req->param( 'id' ) =~ /(\w+[.]pdb)/;
  $c->log->debug( "Ligand:GetPDB::getPdb: The input ligand pdb id is $pdb_id" );
  $c->log->debug( "Ligand:GetPDB::getPdb: The dump of the object is ".dump( $self ) );
  
  unless( defined $pdb_id ){
    $c->log->debug( "Ligand:GetPDB::getPDB: input id contains invalid characters");
    $c->stash->{ errorMsg } ||= 'Input ligand PDB id contains invalid characters';
    return;
  }
  
  my$uri = $self->{lig_uri};
  
  unless( defined $uri ){
    $c->log->debug( "Ligand:GetPDB::getPDB: ligand uri could not be found in config");
    $c->stash->{ errorMsg } ||= 'Please add the ligand uri to the config file';
    return;
  }
  
  $uri .=$pdb_id;
  
  $c->stash->{ ligPDB } = get( $uri );
  
  if( defined $c->stash->{ ligPDB } ){
    $c->res->body( $c->stash->{ ligPDB } );
  } else{
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
  }
  
}



1;