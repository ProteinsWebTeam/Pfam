
# SpeciesTree.pm
# jt6 20060410 WTSI
#
# $Id: SpeciesTree.pm,v 1.2 2008-06-24 08:47:08 jt6 Exp $

=head1 NAME

RfamWeb::Controller::SpeciesTree - controller to build a representation
of the species tree

=cut

package RfamWeb::Controller::SpeciesTree;

=head1 DESCRIPTION

This controller subclasses its namesake in PfamBase, to generate either an 
interactive or a text representation of the species tree for an Rfam family.

Generates a B<page fragment>.

$Id: SpeciesTree.pm,v 1.2 2008-06-24 08:47:08 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::SpeciesTree';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Retrieves the row from the primary table for the specified entry.

=cut

sub begin : Private {
  my( $this, $c, $entry_arg ) = @_;

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Rfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Rfam family accession or ID specified';
  }
  
  #  find out what type of alignment we need, seed, full, etc
  $c->stash->{alnType} = 'seed';
  if( defined $c->req->param('alnType') ) {
    $c->stash->{alnType} = $c->req->param( 'alnType' ) eq 'full' ? 'full'
                         :                                         'seed';
  }
  
  $c->log->debug( 'Family::begin: setting alnType to ' . $c->stash->{alnType} )
    if $c->debug;
  
  # hard-wire the entry type to Rfam
  $c->stash->{entryType} = 'R';
  
  # retrieve the family data
  my $rs = $c->model('RfamDB::Rfam')
             ->search( [ { rfam_acc => $entry },
                         { rfam_id  => $entry } ] );
  my $rfam = $rs->first if defined $rs;
  
  unless ( defined $rfam ) {
    $c->stash->{errorMsg} = 'No valid Pfam family accession or ID found';
    return;
  }

  $c->stash->{rfam}       = $rfam;
  $c->stash->{acc}        = $rfam->rfam_acc;

  # see if we should override the "too many species" check
  $c->stash->{loadTree} = defined $c->req->param('loadTree');

  # see if we're serving to Internet Exploder...
  $c->stash->{isIE} = ( defined $c->req->param('ie') and
                        $c->req->param('ie') eq 'true' ) ? 1 : 0;
  
  $c->log->debug( 'SpeciesTree::begin: isIE: |' . $c->stash->{isIE} . '|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 auto : Private

Actions that build text representation of the tree might want to use the
release date, so we retrieve it here. We need to use an "auto" method
because C<relData> is only dropped into the stash in the C<Root:auto>.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  # see if we can get the release version
  $c->stash->{release_data} = ''; 
  if ( $c->stash->{relData}->rfam_release ) {
    $c->stash->{release_data} = '# Generated from Rfam version ' .
                                $c->stash->{relData}->rfam_release . "\n";
  }
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getDataByType : Private

Retrieves species data for the specified family. 

=cut

sub getDataByType : Private {
  my( $this, $c ) = @_;
  
  # get the species information for the full alignment
  my @regions = $c->model('RfamDB::RfamRegFull')
                  ->search( { 'auto_rfam.rfam_acc' => $c->stash->{acc} },
                            { join              => [ qw( auto_rfamseq auto_rfam ) ],
                              prefetch          => [ qw( auto_rfamseq ) ] } );

  $c->stash->{regions} = \@regions;

  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @regions . '| full regions' ) if $c->debug;

  # get the species information for the seed alignment
  my @resultsSeed = $c->model('RfamDB::RfamRegSeed')
                      ->search( { 'auto_rfam.rfam_acc' => $c->stash->{acc} },
                                { join              => [ qw( auto_rfamseq auto_rfam ) ],
                                  prefetch          => [ qw( auto_rfamseq ) ] } );
  $c->log->debug( 'SpeciesTree::getFamilyData:: found |'
                  . scalar @resultsSeed . '| seed regions' ) if $c->debug;
                
  # hash the seed info so we can easily look up whether a sequence is 
  # found in the seed alignment
  my %inSeed;
  foreach my $region ( @resultsSeed ) {
    $inSeed{ $region->rfamseq_acc}++;
  }

  $c->stash->{inSeed}  = \%inSeed;  
}

#-------------------------------------------------------------------------------

=head2 countSpecies : Private

Retrieves or calculates the number of species in the entry. For Pfam-As and 
clans we can look this up directly in the DB but for Pfam-B we actually have
to count it up.

Again, for Pfam-Bs, since we need to get all regions here, we'll stash them
and they can be used if we go ahead and build the tree, rather than repeating
the query in the C<getData> methods.

=cut

sub countSpecies : Private {
  my( $this, $c ) = @_;

  # for Pfam-As or clans we can just look up the number of species in the 
  # main table, via the "entry" that was put in the stash by C<begin>, but
  # for Pfam-Bs we'll actually have to count the number of species

#  if( $c->stash->{entryType} eq 'B' ) {
#    
#    my @regions = $c->model('PfamDB::PfamB_reg')
#                    ->search( { auto_pfamB => $c->stash->{entry}->auto_pfamB },
#                              { join       => [ qw( pfamseq ) ],
#                                prefetch   => [ qw( pfamseq ) ] } );
#
#    # as we're retrieving them here anyway, stash the regions, so we don't need
#    # to get them again later
#    $c->stash->{regions} = \@regions;
#
#    my %species_unique = map {$_->species => 1} @regions;
#    $c->stash->{numSpecies} = scalar( keys %species_unique );
#
#  } else {
#    $c->stash->{numSpecies} = $c->stash->{entry}->number_species;
#  }

  # TODO fix this; currently hard coding numSpecies to 0...
  $c->stash->{numSpecies} = 0;

  $c->log->debug( 'SpeciesTree::countSpecies: numSpecies: |'
                  . $c->stash->{numSpecies} . '|' )
    if $c->debug;
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
