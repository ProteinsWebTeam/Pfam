
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Family.pm,v 1.38 2008-04-25 13:10:27 jt6 Exp $

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

$Id: Family.pm,v 1.38 2008-04-25 13:10:27 jt6 Exp $

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

a valid Pfam accession

=item entry

either an ID or accession

=back

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
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $entry_arg              ||
                      '';
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\._-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Pfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }
  
  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
  
  #----------------------------------------
  
  # dead families are a special case...
  if ( $c->stash->{entryType} eq 'D' ) {
    
    $c->log->debug( 'Family::begin: got a dead family; setting a refresh URI' ) 
      if $c->debug;

    if ( $c->stash->{pfam}->forward_to ) {
      $c->stash->{refreshUri} =
        $c->uri_for( '/family', { acc => $c->stash->{pfam}->forward_to } );
    }
    else {
      $c->stash->{refreshUri} = $c->uri_for( '/' );
    }
    
    # set the template. This will be overridden below if we're emitting XML
    $c->stash->{template} = 'pages/dead.tt';
  }

  #----------------------------------------

  # if we're outputting HTML, we're done here
  return unless $c->stash->{output_xml};

  #----------------------------------------
  # from here on we're handling XML output

  # if there was an error...
  if ( $c->stash->{errorMsg} ) {
    $c->log->debug( 'Family::begin: there was an error: |' .
                    $c->stash->{errorMsg} . '|' ) if $c->debug;
    $c->stash->{template} = 'rest/family/error_xml.tt';
    $c->res->content_type('text/xml');
    return;
  }
  
  # decide on the output template, based on the type of family that we have
  if ( $c->stash->{entryType} eq 'A' ) {
    $c->log->debug( 'Family::begin: got data for a Pfam-A' ) if $c->debug;
    $c->stash->{template} = 'rest/family/pfama_xml.tt';
    $c->res->content_type('text/xml');
  }
  elsif( $c->stash->{entryType} eq 'B' ) {
    $c->log->debug( 'Family::begin: got data for a Pfam-B' ) if $c->debug;
    $c->stash->{template} = 'rest/family/pfamb_xml.tt';
    $c->res->content_type('text/xml');
  }
  elsif( $c->stash->{entryType} eq 'D' ) {
    $c->log->debug( 'Family::begin: got data for a dead family' ) if $c->debug;
    $c->stash->{template} = 'rest/family/dead_xml.tt';
    $c->res->content_type('text/xml');
  }
  else {
    $c->log->debug( 'Family::begin: got an error' ) if $c->debug;
    $c->stash->{template} = 'rest/family/error_xml.tt';
    $c->res->content_type('text/xml');
  }

}

#-------------------------------------------------------------------------------
#- public actions --------------------------------------------------------------
#-------------------------------------------------------------------------------

=pod

The main action in this package is the one that generates the family page. For 
that action we need to add extra data to the stash, such as summary values for 
the icons. For most other actions that are related to families, we don't need 
all of that extra stuff.

The actions that are associated with families are all in the 
L<FamilyActions|PfamWeb::Controller::Family::FamilyActions> controller, so that 
we can distinguish them from the actions that need summary data, and just avoid 
the overhead of adding it for no reason. 

=cut

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves family data for the given entry. Accepts the entry ID or accession
as the first argument. Does not return any value but drops the L<ResultSet>
for the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # check for a Pfam-A
  my $rs = $c->model('PfamDB::Pfam')
             ->search( [ { pfamA_acc => $entry },
                         { pfamA_id  => $entry } ] );
  my $pfam = $rs->first if defined $rs;
  
  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a Pfam-A' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfamA_acc;
    $c->stash->{entryType} = 'A';
    
    # GO data are used by both the XML and HTML outputs, so always get those
    $c->forward( 'get_go_data' );

    unless ( $c->stash->{output_xml} ) {
      
      # if this request originates at the top level of the object hierarchy,
      # i.e. if it's a call on the "default" method of the Family object,
      # then we'll need to do a few extra things
      if ( ref $this eq 'PfamWeb::Controller::Family' ) {
        $c->log->debug( 'Family::get_data: adding extra family info' ) if $c->debug;

        # add the clan details, if any
        if ( defined $pfam->clan_acc ) {
          $c->log->debug( 'Family::get_data: adding clan info' ) if $c->debug;
          $c->stash->{clan} = $c->model('PfamDB::Clans')
                                ->find( { clan_acc => $pfam->clan_acc } );
        }
        
        $c->forward( 'get_summary_data' );
        $c->forward( 'get_db_xrefs' );
        $c->forward( 'get_interactions' );
      }

      #  find out what type of tree to draw, seed or full
      if( defined $c->req->param('alnType') ) {
        $c->stash->{alnType} = ( $c->req->param( 'alnType' ) eq 'seed' ) ? 'seed' : 'full';
      }
      else {
        $c->stash->{alnType} = 'seed';
      }

    } # end of "unless XML..."
    
    return;

  } # end of "if pfam..."

  #----------------------------------------
  # check for a Pfam-B

  $rs = $c->model('PfamDB::PfamB')
          ->search( [ { pfamB_acc => $entry },
                      { pfamB_id  => $entry } ] );
  $pfam = $rs->first if defined $rs;
    
  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a Pfam-B' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfamB_acc;
    $c->stash->{entryType} = 'B';
    return;
  }

  #----------------------------------------
  # check for a dead Pfam-A
  
  $pfam = $c->model('PfamDB::Dead_families')
            ->find( { pfamA_acc => $entry } );
  
  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a dead family' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfamA_acc;
    $c->stash->{entryType} = 'D';
    return;
  }

  #----------------------------------------
  # there's a problem... by this point we really should have retrieved a
  # row and returned
  
  $c->stash->{errorMsg} = 'No valid Pfam family accession or ID';
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the PfamA object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

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

=head2 get_db_xrefs : Private

Retrieve the database cross-references for the family.

=cut

sub get_db_xrefs : Private {
  my ( $this, $c ) = @_;

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
    if ( $xref->db_id eq 'PFAMB' ) {
      $atobPRODOM{$xref->db_link} = $xref;
    }
    else {
      push @{ $xRefs->{$xref->db_id} }, $xref;
    }
  }

  # PfamA to PfamA links based on PRC
  my @atoaPRC = $c->model('PfamDB::PfamA2pfamA_PRC_results')
                  ->search( { 'pfamA1.pfamA_acc' => $c->stash->{pfam}->pfamA_acc },
                            { join               => [ qw( pfamA1 pfamA2 ) ],
                              select             => [ qw( pfamA1.pfamA_id 
                                                          pfamA1.pfamA_acc
                                                          pfamA2.pfamA_id 
                                                          pfamA2.pfamA_acc 
                                                          evalue ) ],
                              as                 => [ qw( l_pfamA_id 
                                                          l_pfamA_acc 
                                                          r_pfamA_id 
                                                          r_pfamA_acc 
                                                          evalue ) ],
                              order_by           => 'pfamA2.auto_pfamA ASC'
                            } );

  $xRefs->{atoaPRC} = [];
  foreach ( @atoaPRC ) {
    if ( $_->get_column( 'evalue' ) <= 0.001 and
         $_->get_column( 'l_pfamA_id' ) ne $_->get_column( 'r_pfamA_id' ) ) {
      push @{ $xRefs->{atoaPRC} }, $_;
    } 
  }

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
      if ( exists( $atobPRC{$_} ) and exists( $atobPRODOM{$_} ) );
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

=head2 get_go_data : Private

Retrieves the gene ontology (GO) data for the family.

=cut

sub get_go_data : Private {
  my ( $this, $c ) = @_;

  my @goTerms = $c->model('PfamDB::GO')
                  ->search( { auto_pfamA => $c->stash->{pfam}->auto_pfamA } );

  $c->stash->{goTerms} = \@goTerms;
}

#-------------------------------------------------------------------------------

=head2 get_interactions : Private

Retrieves details of the interactions between this family and others.

=cut

sub get_interactions : Private {
  my ( $this, $c ) = @_;
  
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
