
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Family.pm,v 1.33 2007-08-30 09:26:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main Pfam family
page

=cut

package PfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Pfam
families across the site. The L<begin|/"begin : Private"> method tries
to extract a Pfam ID or accession from the captured URL and tries to
load a Pfam object from the model.

Generates a B<tabbed page>.

$Id: Family.pm,v 1.33 2007-08-30 09:26:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

This is the guts of this controller. It's function is to extract
the Pfam family ID or accession from the URL and get the row
in the Pfam table for that entry. Expects one of three parameters:

=over

=item acc

a valid Pfam accession, either for an A or B entry

=item id

a valid PfamA accession

=item entry

either an ID or accession

=back

If C<entry> is specified, we'll try to guess if it's an A or B accession entry
or an ID and redirect back to this class with the appropriate parameter (either
C<acc> or C<id> specified.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  if( defined $c->req->param('acc') and 
      $c->req->param('acc') =~ m/^(P([FB])\d{5,6})$/i ) {

    $c->log->debug( "Family::begin: found accession |$1|, family A / B ? |$2|" );
    
    # is it a Pfam-A ?
    if( $c->req->param('acc') =~ m/^(PF\d{5})$/i ) {

      $c->stash->{pfam} = $c->model('PfamDB::Pfam')
                            ->find( { pfamA_acc => $1 } );

      if( defined $c->stash->{pfam} ) {
        $c->log->debug( "Family::begin: found a Pfam-A: |$1|" );
        $c->stash->{entryType} = 'A';
        $c->stash->{acc}       = $c->stash->{pfam}->pfamA_acc;

      } else {
        $c->log->debug( 'Family::begin: testing for a dead family by accession' );
        
        my $rs = $c->model('PfamDB::Dead_families')
                   ->find( { pfamA_acc => $1 } );
        if( defined $rs ) {
          $c->log->debug( "Family::begin: |$1| is dead; redirecting" );
          $c->res->redirect( $c->uri_for( '/dead', { acc => $1 } ) );
          return;
        }
      }
          
    } elsif( $c->req->param('acc') =~ m/^(PB\d{6})$/i ) {
      # no; could be a Pfam-B

      $c->stash->{pfam} = $c->model('PfamDB::PfamB')
                            ->find( { pfamB_acc => $1 } );

      if( defined $c->stash->{pfam} ) {
        $c->log->debug( "Family::begin: found a Pfam-B: |$1|" );
        $c->stash->{entryType} = 'B';
        $c->stash->{acc}       = $c->stash->{pfam}->pfamB_acc;
      }

    }

  } elsif( defined $c->req->param('id') and
           $c->req->param('id') =~ /^([\w_-]+)$/ ) {
    $c->log->debug( "Family::begin: found an ID |$1|" );
    my $id = $1;
  
    # but now we need to decide if it's a Pfam-A or a Pfam-B ID...
    if( $id =~ /^(Pfam-B_\d+)$/ ) {
      
      $c->stash->{pfam} = $c->model('PfamDB::PfamB')
                            ->find( { pfamB_id => $1 } );

      if( defined $c->stash->{pfam} ) {
        $c->log->debug( "Family::begin: found a Pfam-B: |$1|" );
        $c->stash->{entryType} = 'B';
        $c->stash->{acc}       = $c->stash->{pfam}->pfamB_acc;
      }

    } else {
      
      # must be a Pfam-A ID (?)
      $c->stash->{pfam} = $c->model('PfamDB::Pfam')
                            ->find( { pfamA_id => $1 } );

      if( defined $c->stash->{pfam} ) {
        $c->stash->{entryType} = 'A';
        $c->stash->{acc}       = $c->stash->{pfam}->pfamA_acc;

      } else {
        $c->log->debug( 'Family::begin: testing for a dead family by ID' );
        
        my $rs = $c->model('PfamDB::Dead_families')
                   ->find( { pfamA_id => $1 } );
        if( defined $rs ) {
          $c->log->debug( "Family::begin: |$1| is dead; redirecting" );
          $c->res->redirect( $c->uri_for( '/dead', { id => $1 } ) );
          return;
        }
      }
    }
    
  } elsif( defined $c->req->param( 'entry' ) ) {

    if( $c->req->param('entry') =~ /^(PF\d{5})$/i ) {

      # looks like a PfamA accession; redirect to this action, appending the accession
      $c->log->debug( "Family::begin: looks like a Pfam-A accession ($1); redirecting internally" );
      $c->req->param( 'acc' => $1 );
      $c->detach( 'begin' );
      return 1;

    } elsif( $c->req->param( 'entry' ) =~ /^(PB\d{6})$/i ) {

      # looks like a Pfam-B accession; redirect to this action, appending the accession
      $c->log->debug( "Family::begin: looks like a Pfam-B accession ($1); redirecting" );
      $c->res->redirect( $c->uri_for( '/pfamb', { acc => $1 } ) );
      return 1;

    } elsif( $c->req->param( 'entry' ) =~ /^(Pfam-B_\d+)$/i ) {

      # looks like a Pfam-B ID; redirect to this action, appending the accession
      $c->log->debug( "Family::begin: looks like a Pfam-B id ($1); redirecting" );
      $c->res->redirect( $c->uri_for( '/pfamb', { id => $1 } ) );
      return 1;

    } elsif( $c->req->param( 'entry' ) =~ /^([\w_-]+)$/ ) {

      # looks like an ID; redirect to this action, appending the ID
      $c->log->debug( 'Family::begin: might be a Pfam ID; redirecting internally' );
      $c->req->param( 'id' => $1 );
      $c->detach( 'begin' );
      return 1;
    }

  }

  #----------------------------------------

  # find out what type of tree to draw, seed or full
  if( defined $c->req->param('alnType') ) {
    $c->stash->{alnType} = ( $c->req->param( 'alnType' ) eq 'seed' ) ? 'seed' : 'full';
    $c->log->debug( 'Family::begin: setting alnType to |' . 
                    $c->stash->{alnType} . '|' );
  } else {
    $c->stash->{alnType} = 'seed';
    $c->log->debug( 'Family::begin: no alnType parameter; defaulting to "seed"' );
  }    

  #----------------------------------------

  # we're done here unless there's an entry specified
  unless( defined $c->stash->{pfam} ) {

    # de-taint the accession or ID
    my $input = $c->req->param('acc')
      || $c->req->param('id')
      || $c->req->param('entry')
      || '';
    $input =~ s/^(\w+)/$1/;
    
    # see if this was an internal link and, if so, report it
    my $b = $c->req->base;
    if( defined $c->req->referer and $c->req->referer =~ /^$b/ ) {
  
      # this means that the link that got us here was somewhere within
      # the Pfam site and that the accession or ID which it specified
      # doesn't actually exist in the DB
  
      # report the error as a broken internal link
      $c->error( "Found a broken internal link; no valid Pfam family accession or ID "
                 . "(\"$input\") in \"" . $c->req->referer . "\"" );
      $c->forward( '/reportError' );
  
      # now reset the errors array so that we can add the message for
      # public consumption
      $c->clear_errors;
  
    }

    # the message that we'll show to the user
    $c->stash->{errorMsg} = 'No valid Pfam family accession or ID';

    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn( 'Family::begin: no valid Pfam family ID or accession' );

    return;
  }

  $c->log->debug( 'Family::begin: successfully retrieved a pfam object' );

  #----------------------------------------

  # if this is a PfamA then we need to get more stuff out of the
  # database for it...

  if( $c->stash->{entryType} eq 'A' ) {

    # add the clan details, if any
    $c->stash->{clan} = $c->model('PfamDB::Clans')
                          ->find( { clan_acc => $c->stash->{pfam}->clan_acc } )
        if defined $c->stash->{pfam}->clan_acc;
        
    # if this request originates at the top level of the object hierarchy,
    # i.e. if it's a call on the "default" method of the Family object,
    # then we'll need to do a few extra things

    if( ref $this eq 'PfamWeb::Controller::Family' ) {
      
      # add extra data to the stash
      $c->forward( 'getSummaryData' );
      $c->forward( 'getDbXrefs' );
      $c->forward( 'getGoData' );
      $c->forward( 'getInteractions' );
        
    }
  }
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getSummaryData : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the PfamA object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub getSummaryData : Private {
  my( $this, $c ) = @_;

  my $summaryData = {};

  # number of architectures....
  $summaryData->{numArchitectures} = $c->stash->{pfam}->number_archs;

  # number of sequences in full alignment
  $summaryData->{numSequences} = $c->stash->{pfam}->num_full;

  # number of structures known for the domain
  $summaryData->{numStructures} = $c->stash->{pfam}->number_structures;

  # Number of species
  $summaryData->{numSpecies} = $c->stash->{pfam}->number_species;

  # number of interactions
  my $auto_pfamA = $c->stash->{pfam}->auto_pfamA;
  my $rs = $c->model('PfamDB::PfamA_interactions')
             ->find( { auto_pfamA_A => $auto_pfamA },
                     { select => [ { count => 'auto_pfamA_A' } ],
                       as     => [ qw( numInts ) ]
                     } );
  $summaryData->{numInt} = $rs->get_column( 'numInts' );

  $c->stash->{summaryData} = $summaryData;
}

#-------------------------------------------------------------------------------

=head2 getDbXrefs : Private

Retrieve the database cross-references for the family.

=cut

sub getDbXrefs : Private {
  my( $this, $c ) = @_;

  my $xRefs = {};

  # stuff in the accession and ID for this entry
  $xRefs->{entryAcc} = $c->stash->{pfam}->pfamA_acc;
  $xRefs->{entryId}  = $c->stash->{pfam}->pfamA_id;

  # Interpro
  push @{ $xRefs->{interpro} }, $c->stash->{pfam}->interpro_id
    if $c->stash->{pfam}->interpro_id;

  # PDB
  $xRefs->{pdb} = keys %{ $c->stash->{pdbUnique} }
    if $c->stash->{summaryData}{numStructures};


  # PfamA relationship based on SCOOP
  push @{ $xRefs->{scoop} },
       $c->model('PfamDB::PfamA2pfamA_scoop_results')
         ->search( { auto_pfamA1 => $c->stash->{pfam}->auto_pfamA,
                     score       => { '>', 50.0 } },
                   { join        => [ qw( pfamA1 pfamA2 ) ],
                     select      => [ qw( pfamA1.pfamA_id pfamA2.pfamA_id score ) ],
                     as          => [ qw( l_pfamA_id r_pfamA_id score ) ]
                   } );

  # PfamA to PfamB links based on PRODOM
  my %atobPRODOM;
  foreach my $xref ( $c->stash->{pfam}->pfamA_database_links ) {
    if( $xref->db_id eq 'PFAMB' ) {
      $atobPRODOM{$xref->db_link} = $xref;
    } else {
      push @{ $xRefs->{$xref->db_id} }, $xref;
    }
  }

  # PfamA to PfamA links based on PRC
  my @atoaPRC = $c->model('PfamDB::PfamA2pfamA_PRC_results')
                  ->search( { 'pfamA1.pfamA_acc' => $c->stash->{pfam}->pfamA_acc },
                            { join               => [ qw( pfamA1 pfamA2 ) ],
                              select             => [ qw( pfamA1.pfamA_id pfamA2.pfamA_id evalue ) ],
                              as                 => [ qw( l_pfamA_id r_pfamA_id evalue ) ],
                              order_by           => 'pfamA2.auto_pfamA ASC'
                            } );

  $xRefs->{atoaPRC} = [];
  foreach ( @atoaPRC ) {
    push @{ $xRefs->{atoaPRC} }, $_ if $_->get_column( 'evalue' ) <= 0.001;
  }

  # $xRefs{atoaPRC} = \@atoaPRC if scalar @atoaPRC;

  # PfamB to PfamA links based on PRC
  my @atobPRC = $c->model('PfamDB::PfamB2pfamA_PRC_results')
                  ->search( { 'pfamA.pfamA_acc' => $c->stash->{pfam}->pfamA_acc, },
                            { join      => [ qw( pfamA pfamB ) ],
                              prefetch  => [ qw( pfamA pfamB ) ]
                            } );

  # find the union between PRC and PRODOM PfamB links
  my %atobPRC;
  foreach ( @atobPRC ) {
    $atobPRC{$_->pfamB_acc} = $_ if $_->evalue <= 0.001;
  }
  # we should be able to filter the results of the query according to
  # evalue using a call on the DBIx::Class object, but for some reason
  # it's broken, hence that last loop rather than this neat map...
  # my %atobPRC = map { $_->pfamB_acc => $_ } @atobPRC;

  my %atobBOTH;
  foreach ( keys %atobPRC, keys %atobPRODOM ) {
    $atobBOTH{$_} = $atobPRC{$_}
      if( exists( $atobPRC{$_} ) and exists( $atobPRODOM{$_} ) );
  }

  # and then prune out those accessions that are in both lists
  foreach ( keys %atobPRC ) {
    delete $atobPRC{$_} if exists $atobBOTH{$_};
  }

  foreach ( keys %atobPRODOM ) {
    delete $atobPRODOM{$_} if exists $atobBOTH{$_};
  }

  # now populate the hash of xRefs;
  my @atobPRC_pruned;
  foreach ( sort keys %atobPRC ) {
    push @atobPRC_pruned, $atobPRC{$_};
  }
  $xRefs->{atobPRC} = \@atobPRC_pruned if scalar @atobPRC_pruned;

  my @atobPRODOM;
  foreach ( sort keys %atobPRODOM ) {
    push @atobPRODOM, $atobPRODOM{$_};
  }
  $xRefs->{atobPRODOM} = \@atobPRODOM if scalar @atobPRODOM;

  my @atobBOTH;
  foreach ( sort keys %atobBOTH ) {
    push @atobBOTH, $atobBOTH{$_};
  }
  $xRefs->{atobBOTH} = \@atobBOTH if scalar @atobBOTH;

  $c->stash->{xrefs} = $xRefs;
}

#-------------------------------------------------------------------------------

=head2 getGoData : Private

Retrieves the gene ontology (GO) data for the family.

=cut

sub getGoData : Private {
  my( $this, $c ) = @_;

  my @goTerms = $c->model('PfamDB::GO')
                  ->search( { 'me.auto_pfamA' => $c->stash->{pfam}->auto_pfamA } );

  $c->stash->{goTerms} = \@goTerms;
}

#-------------------------------------------------------------------------------

=head2 getInteractions : Private

Retrieves details of the interactions between this family and others.

=cut

sub getInteractions : Private {
  my( $this, $c ) = @_;
  
  my @interactions = $c->model('PfamDB::PfamA_interactions')
                       ->search( { auto_pfamA_A => $c->stash->{pfam}->auto_pfamA },
                                 { join     => [ qw( pfamA_B ) ],
                                   prefetch => [ qw( pfamA_B ) ] } );

  $c->stash->{interactions} = \@interactions;
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
