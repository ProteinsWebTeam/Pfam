
# Jump.pm
# jt6 20060807 WTSI
#
# $Id: Jump.pm,v 1.2 2009-10-06 11:05:34 pg6 Exp $

=head1 NAME

PfamWeb::Controller::Jump - guess the type of entry that the user wants to see

=cut

package iPfamWeb::Controller::Search::Jump;

=head1 DESCRIPTION

$Id: Jump.pm,v 1.2 2009-10-06 11:05:34 pg6 Exp $

=cut

use strict;
use warnings;

use URI;

use base 'iPfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 jump : Local

Re-directs to one of the main page type controllers, such as Family, Clan, etc.,
which can handle directly the entry ID or accession.

=cut

sub jump : Path {
  my( $this, $c ) = @_;

  # de-taint the entry ID or accession
  my $entry = '';
  ( $entry ) = $c->req->param('entry') =~ /^([\w\-_\s()\.]+)$/;
  $c->log->debug( "Search::Jump::jump: called with entry |$entry|" );

  # strip off leading and trailing whitespace
  $entry =~ s/^\s*(.*?)\s*$/$1/;
  $c->log->debug( "Search::Jump::jump: trimmed entry to |$entry|" );

  # we've detainted and trimmed whitespace from the "entry" parameter, so
  # override the original version with our cleaned up one
  my $params = $c->req->params;
  $params->{entry} = $entry;
    
  # bail immediately if there's no entry given
  unless( $entry ) {
    if( defined $c->req->referer )  {

      # build a new URI for the referrer. We want to append a parameter to tell
      # the jump box that we couldn't find the entry, but there could already
      # *be* parameters, so we use URI to tack the new one on, just to be safe      
      my $uri = new URI( $c->req->referer );
      $uri->query_form( $uri->query_form,
                        jumpErr => 1 );
      $c->log->debug( "Search::Jump::jump: redirecting to referer ($uri)" );
      $c->res->redirect( $uri );

    } else {
      $c->log->debug( "Search::Jump::jump: couldn't guess entry type and no referer; redirecting to home page" );
      $c->res->redirect( $c->uri_for( '/' ) );
    }
    return 1;
  }

  # now we know we have an entry. De-taint the ID type
  my $entryType;
  ( $entryType ) = $c->req->param('type') || ''  =~ /^(\w+)/;

  # see if that entry type is valid and, if so, redirect to it
  if( $entryType and defined $this->{jumpTargets}->{$entryType} ) {
    
    my $action = '/' . $this->{jumpTargets}->{$entryType};
    $c->log->debug( "Search::Jump::jump: called on entry type |$entryType|; redirecting to |$action|" );
    
    # again, we've cleaned up "entryType", so override that parameter in our
    # parameters list
    $params->{type} = $entryType;

    # and actually redirect
    $c->res->redirect( $c->uri_for( $action, $params ) );
    return 1;
  }
  
  # if we get to here we have an entry accession or ID, but we have no idea what 
  # kind of entry it refers to

  # let's guess !
  $c->log->debug( 'Search::Jump::jump: no valid entry type specified; guessing instead' );
  my $action = $c->forward( 'guess', [ $entry ] );

  if( $action ) {
  
    $c->log->debug( "Search::Jump::jump: we've made a guess; redirecting to |$action|" );
    #$c->res->redirect( $c->uri_for( "/$action", $params ) );
    $c->stash->{ url } = $c->uri_for( "/$action", $params );
  
  } else {
    $c->log->debug( "Search::Jump::jump: couldn't guess entry type..." );
    
    # set the error message differently according to whether we were trying to
    # "look up" and entry or guess the type
    $c->stash->{error} = $entryType ? 'Entry not found' : "Couldn't guess entry";
  }

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 guess : Private

Tries to guess what kind of entity is referred to by the argument. It's assumed
that the entry is already de-tainted before it's passed in here.

Returns the action to which the entry maps, or undef if guessing fails.

=cut

sub guess : Private {
  my( $this, $c, $entryUnknownCase ) = @_;
  
  # make sure we know the entry is upper case
  my $entry = uc( $entryUnknownCase );

  $c->log->debug( "Search::Jump::guess: guessing target for |$entry|" );

  # see if we can figure out what kind if ID or accession we've been handed
  my( $action, $found );  
  
  # now check for pfamA's , uniprot id's and pdb id's,
  # if we find anything, return at that point;
  # i am not enforcing any regex check, because i am  
  # just interested in look up.
  
  # 1) checking for pfama's
  
  my $rs1 =  $c->model( 'iPfamDB::Pfama' )
              ->search ( [
                { pfama_id  =>  $entry },
                { pfama_acc =>  $entry },
              ] );
    
  if( defined $rs1->first ){
    $c->log->debug( "Search::Jump::guess: found a pfamA family for the entry |$entry|\nhence returning....." );
    return 'family';
  }
  
  # explicitly using undef asking perl to clear this variable's memory immediately, rather than 
  # waiting for garbage collection because, though we dint get any result, the object still exists ;
  
  undef $rs1;
  
  # 2) now check for any protein sequence and id's 
  
  my $rs2 =  $c->model( 'iPfamDB::Protein' )
              ->search( [
                { accession =>  $entry },
                { id  =>  $entry }
              ] );
  
  if( defined $rs2->first ){
    $c->log->debug( "Search::Jump::guess: found a protein entry for |$entry| ");
    return 'protein';
  }
  undef $rs2;
  
  # 3) now check for pdb's 
  
  my $rs3 = $c->model( 'iPfamDB::Pdb')
              ->search( 
                { pdb_id  =>  $entry }
              );
  if( defined $rs3->first ){
    $c->log->debug( "Search::Jump::guess: found a pdb entry for |$entry|" );
    return 'structure';
  }
  undef $rs3;
  
  # 4) now check for the ligand, its very rare that user knows the ligand id,
  # so presuming that the user would enter the three letter code of the ligand
  
  my $rs4 = $c->model( 'iPfamDB::LigandChemistry')->search(
              { three_letter_code =>  $entry }
            );
  if( defined $rs4->first ){
    $c->log->debug( "Search::Jump::guess: found a ligand for |$entry|" );
    return 'ligand';
  }
  undef $rs4;
  
# all these can be removed as pfamBs wont be present in the iPfam database;
#  
#  # first, see if it's a PfamA family
#  if( $entry =~ /^(PF\d{5})(\.\d+)?$/ ) {
#    
#    $found = $c->model('iPfamDB::Pfama')
#               ->find( { pfamA_acc => $1 } );
#  
#    if( defined $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a PfamA family (from accession)' );
#      $action = 'family';
#
#    }   
#  }
#  
#  # or a PfamB accession ?
#  if( not $action and $entry =~ /^(PB\d{6})$/ ) {
#
#    $found = $c->model('PfamDB::PfamB')
#              ->find( { pfamB_acc => $1 } );      
#  
#    if( defined $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a PfamB (from accession)' );
#      $action = 'pfamb';
#    }
#  }
#  
#  # maybe a PfamB ID ?
#  if( not $action and $entry =~ /^(Pfam-B_\d+)$/ ) {
#
#    $found = $c->model('PfamDB::PfamB')
#              ->find( { pfamB_id => $1 } );      
#  
#    if( defined $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a PfamB (from ID)' );
#      $action = 'pfamb';
#    }
#  }
#  # now check for uniprot sequence;  
#  
#  # how about a sequence entry ?
#  if( not $action and $entry =~ /^([OPQ]\d[A-Z0-9]{3}\d)(\.\d+)?$/ ) {
#  
#    $found = $c->model('PfamDB::Pfamseq')
#               ->find( { pfamseq_acc => $1 } );
#  
#    if( defined $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a sequence entry' );
#      $action = 'protein';
#    } else {
#      # see if it's a secondary accession
#      $found = $c->model('PfamDB::Secondary_pfamseq_acc')
#                 ->find( { secondary_acc => $1 },
#                         { join =>     [ qw( pfamseq ) ],
#                           prefetch => [ qw( pfamseq ) ] } );
#      if ( defined $found ) {
#        $c->log->debug( 'Search::Jump::guess: found a secondary sequence entry' );
#        $action = 'protein';
#      }
#    }
#  }
#  
#  # see if it's a protein sequence ID (e.g. CANX_CHICK)
#  if( not $action and $entry =~ /^([A-Z0-9]+\_[A-Z0-9]+)$/ ) {
#  
#    $found = $c->model('iPfamDB::Pfamseq')
#               ->find( { pfamseq_id => $1 } );
#  
#    if( $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a sequence entry (from ID)' );
#      $action = 'protein';
#    }
#  
#  }
#  
#  # maybe a structure ?
#  if( not $action and $entry =~ /^([0-9][A-Za-z0-9]{3})$/ ) {
#  
#    $found = $c->model('iPfamDB::Pdb')
#               ->find( { pdb_id => $1 } );
#  
#    if( defined $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a structure' );
#      $action = 'structure';
#    }
#    
#  }
#  
#  # finally, see if it's some other sort of ID
#
#  # a Pfam family ID ?
#  if( not $action ) {
#
#    $found = $c->model('iPfamDB::Pfama')
#               ->find( { pfamA_id => $entry } );
#               
#    if( $found ) {
#      $c->log->debug( 'Search::Jump::guess: found a Pfam family (from ID)' );
#      $action = 'family';
#
#    } 
#  }
#
#  return $action;
}

#-------------------------------------------------------------------------------

=head2 end : Private

Returns either a text/plain response with the guessed URL in the body, or an
error response (status 400) with the error message in the body. 

If the C<redirect> parameter is set to 1, rather than simply returning the URL 
or an error message, we set the redirect header and the user is redirected 
straight to the guessed URL or, if we couldn't guess a URL, to the index page.

=cut

sub end : Private {
  my ( $this, $c ) = @_;

  $c->res->content_type( 'text/plain' );
  
  if( $c->stash->{error} ) {

    if ( $c->req->param('redirect') ) {
      $c->log->debug( 'Search::Jump::end: jump error; redirecting to home page' )
        if $c->debug;
      $c->res->redirect( $c->uri_for( '/' ), 301 );
    }
    else {
      $c->res->body( $c->stash->{error} );
      $c->res->status( 400 );
    }

  }
  else {
    
    if ( defined $c->req->param('redirect') and
         $c->req->param('redirect') == 1 ) {
      $c->log->debug( 'Search::Jump::end: redirect parameter set; redirecting to guessed URL' )
        if $c->debug;
      $c->res->redirect( $c->stash->{url}, 301 );
    }
    else {
      $c->res->body( $c->stash->{url} );
    }

  }
  
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
