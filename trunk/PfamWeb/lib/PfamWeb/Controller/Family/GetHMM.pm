
# GetHMM.pm
# jt6 20061003 WTSI
#
# $Id: GetHMM.pm,v 1.9 2007-08-02 15:24:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::GetHMM - retrieve an HMM

=cut

package PfamWeb::Controller::Family::GetHMM;

=head1 DESCRIPTION

Retrieves the raw HMM for a specified Pfam family.

Generates a B<text file>.

$Id: GetHMM.pm,v 1.9 2007-08-02 15:24:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getHMM : Private

Serve the contents of the HMM for a Pfam-A entry from the database. Requires 
the "mode" parameter to be set either to "ls" or "fs".

=cut

sub getHMM : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # find out which HMM we want...
  my( $mode ) = $c->req->param( 'mode' ) =~ /^(fs|ls)$/;
  if( not defined $mode ) {
    $c->stash->{errorMsg} = 'There was no HMM type specified. &quot;mode&quot; '
                            . 'should be either &quot;ls&quot; or &quot;fs&quot;';
    return;
  }

  my $cacheKey = 'hmm' . $c->stash->{acc} . $mode;
  my $hmm      = $c->cache->get( $cacheKey );

  if( defined $hmm ) {
    $c->log->debug( 'Family::GetHMM::getHMM: extracted HMM from cache' );
  } else {
    $c->log->debug( 'Family::GetHMM::getHMM: failed to extract HMM from cache; going to DB' );
     
    # retrieve the HMM from the appropriate table
    if( $mode eq 'ls' ) {
      my $rs = $c->model('PfamDB::PfamA_HMM_ls')
                 ->find( $c->stash->{pfam}->auto_pfamA );
  
      unless( $rs ) {
        $c->stash->{errorMsg} = "We could not find the &quot;ls&quot; HMM file for " 
                                . $c->stash->{acc};
        return;
      }
  
      $hmm = $rs->hmm_ls;
  
    } elsif( $mode eq 'fs' ) {
      my $rs = $c->model('PfamDB::PfamA_HMM_fs')
                 ->find( $c->stash->{pfam}->auto_pfamA );
  
      unless( $rs ) {
        $c->stash->{errorMsg} = "We could not find the &quot;fs&quot; HMM file for " 
                                . $c->stash->{acc};
        return;
      }    
  
      $hmm = $rs->hmm_fs;
    }  

    unless( $hmm ) {
      $c->stash->{errorMsg} = "We could not find the $mode HMM file for " 
                              . $c->stash->{acc};
      return;
    }

    # cache the raw HMM
    $c->cache->set( $cacheKey, $hmm );
  }

  # build a name for the file that will be downloaded
  my $filename = $c->stash->{pfam}->pfamA_id."_$mode.hmm";

  # set the response headers
  $c->res->content_type( 'text/plain' );
  $c->res->headers->header( 'Content-disposition' => "attachment; filename=$filename" );

  # at this point we should have the HMM in hand, so spit it out to the 
  # response and we're done
  $c->res->body( $hmm );

  # the RenderView action on the end method in Section.pm will spot that there's
  # content in the response and return without trying to render any templates
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
