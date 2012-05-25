
# Utils.pm
# jt6 20080104 WTSI
#
# $Id: Utils.pm,v 1.1 2008-06-24 08:47:25 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Utils - a set of utility actions

=cut

package RfamWeb::Controller::Utils;

=head1 DESCRIPTION

These are a few actions that can be used in various places around the code. 

$Id: Utils.pm,v 1.1 2008-06-24 08:47:25 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Utils';

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 retrieve_ids : Private

Retrieves a list of IDs/accessions from the DB, given a "job ID" for the list.
This action doesn't attempt to validate the job ID, it just assumes it will
work. The ID should be checked before calling this action.

=cut

sub retrieve_ids : Private {
  my( $this, $c, $job_id ) = @_;
  
  # retrieve the IDs/accessions for that job ID
  my $rs = $c->model('WebUser::Species_collection')
             ->find( $job_id );
  unless( defined $rs ) {
    $c->log->debug( 'Utils::retrieve_ids: no accessions for that job ID' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence accessions found for that job ID';
    return;
  }

  $c->log->debug( 'Utils::retrieve_ids: accessions_list: |'
                   . $rs->id_list . '|' ) if $c->debug;

  # get the individual accessions
  my @seqAccs;
  foreach ( split /\s+/, $rs->id_list ) {
    push @seqAccs, $_ if m/^\w+$/;
  }
  $c->log->debug( 'Utils::retrieve_ids: found |' . scalar @seqAccs
                  . '| valid sequence accessions' ) if $c->debug;

  return \@seqAccs;
}

#-------------------------------------------------------------------------------


=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk),
         Paul Gardner, C<pg5@sanger.ac.uk>, Jennifer Daub, C<jd7@sanger.ac.uk>

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
