
# Family.pm
# jt6 20060411 WTSI
#
# $Id: Family.pm,v 1.53 2009-11-16 16:05:30 jt6 Exp $

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

$Id: Family.pm,v 1.53 2009-11-16 16:05:30 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

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
    $c->res->content_type('text/xml');    
  }
  
  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->query_keywords || # accept getacc-style params
                      $entry_arg              ||
                      '';
  
  # although these next checks might fail and end up putting an error message
  # into the stash, we don't "return", because we might want to process the 
  # error message using a template that returns XML rather than simply HTML
  
  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Pfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam family accession or ID specified';
  }
  
  # strip off family version numbers, if present
  $entry =~ s/\.\d+$//;
  
  #  find out what type of alignment we need, seed, full, ncbi, etc
  $c->stash->{alnType} = 'seed';
  my %allowed_alignment_types = ( full => 1,
                                  seed => 1,
                                  ncbi => 1,
                                  meta => 1,
                                  long => 1 );
  if ( defined $c->req->param('alnType') and
       exists $allowed_alignment_types{ $c->req->param('alnType') } ) {
    $c->stash->{alnType} = $c->req->param( 'alnType' );
  }
  
  $c->log->debug( 'Family::begin: setting alnType to ' . $c->stash->{alnType} )
    if $c->debug;
  
  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
  
  #----------------------------------------
  
  # dead families are a special case...
  if ( $c->stash->{entryType} eq 'D' ) {
    
    $c->log->debug( 'Family::begin: got a dead family; setting a refresh URI' ) 
      if $c->debug;

    if ( $c->stash->{pfam}->forward_to ) {
      $c->stash->{refreshUri} =
        $c->uri_for( '/family', $c->stash->{pfam}->forward_to );
    }
    else {
      $c->stash->{refreshUri} = $c->uri_for( '/' );
    }
    
    # set the template. This will be overridden below if we're emitting XML
    $c->stash->{template} = 'pages/dead.tt';
  }

  #----------------------------------------
  
  # use a redirect page if the ID of the family has changed 
  if ( $c->stash->{entryType} eq 'R' ) {
    
    $c->log->debug( 'Family::begin: arrived at a family using a previous ID; setting a refresh URI' ) 
      if $c->debug;

    $c->stash->{refreshUri} =
      $c->uri_for( '/family', $c->stash->{acc} );
    
    # set the template for the intermediate page
    $c->stash->{template} = 'pages/moved.tt';
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
    return;
  }
  
  # decide on the output template, based on the type of family that we have
  if ( $c->stash->{entryType} eq 'A' or
       $c->stash->{entryType} eq 'R' ) {
    # we'll use the same XML template to handle familes that were arrived at 
    # using a "previous ID"
    $c->log->debug( 'Family::begin: got data for a Pfam-A' ) if $c->debug;
    $c->stash->{template} = 'rest/family/pfama_xml.tt';
  }
  elsif( $c->stash->{entryType} eq 'B' ) {
    $c->log->debug( 'Family::begin: got data for a Pfam-B' ) if $c->debug;
    $c->stash->{template} = 'rest/family/pfamb_xml.tt';
  }
  elsif( $c->stash->{entryType} eq 'D' ) {
    $c->log->debug( 'Family::begin: got data for a dead family' ) if $c->debug;
    $c->stash->{template} = 'rest/family/dead_xml.tt';
  }
  else {
    $c->log->debug( 'Family::begin: got an error' ) if $c->debug;
    $c->stash->{template} = 'rest/family/error_xml.tt';
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
  my $rs = $c->model('PfamDB::Pfama')
             ->search( [ { pfama_acc => $entry },
                         { pfama_id  => $entry } ],
                       { join     => [ qw( interpros ) ],
                         prefetch => [ qw( interpros ) ] } );
  my $pfam = $rs->first if defined $rs;
  
  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a Pfam-A' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfama_acc;
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
        my $clans = $c->model('PfamDB::Clans')
                      ->search( { 'clan_memberships.auto_pfama' => $pfam->auto_pfama },
                                { join     => [ qw(clan_memberships) ],
                                  prefetch => [ qw(clan_memberships) ] } )
                      ->first;
        
        if ( $clans and  defined $clans->clan_acc ) {
          $c->log->debug( 'Family::get_data: adding clan info' ) if $c->debug;
          $c->stash->{clan} = $clans;
        }
        
        $c->forward( 'get_summary_data' );
        $c->forward( 'get_db_xrefs' );
        $c->forward( 'get_interactions' );
      }

    } # end of "unless XML..."
    
    return;

  } # end of "if pfam..."

  #----------------------------------------
  # check for a Pfam-B
    
  $rs = $c->model('PfamDB::Pfamb')
          ->search( [ { pfamb_acc => $entry },
                      { pfamb_id  => $entry } ] );
  $pfam = $rs->first if defined $rs;
    
  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a Pfam-B' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfamb_acc;
    $c->stash->{entryType} = 'B';
    return;
  }

  #----------------------------------------
  # check for a dead Pfam-A
  
  $pfam = $c->model('PfamDB::DeadFamilies')
            ->search( [ { pfama_acc => $entry },
                        { pfama_id  => $entry } ] )
            ->single;
  
  if ( $pfam ) {
    $c->log->debug( 'Family::get_data: got a dead family' ) if $c->debug;
    $c->stash->{pfam}      = $pfam;
    $c->stash->{acc}       = $pfam->pfama_acc;
    $c->stash->{entryType} = 'D';
    return;
  }

  #----------------------------------------
  # check for a previous ID
  
  $pfam = $c->model('PfamDB::Pfama')
            ->find( { previous_id => { like => "%$entry;%" } } );
  
  # make sure the entry matches a whole ID, rather than just part of one
  # i.e. make sure that "6" doesn't match "DUF456" 
  if ( $pfam ) {
    my $previous_id = $pfam->previous_id;
    if ( $previous_id =~ m/(^|.*?;\s*)$entry\;/ ) { # same pattern used in Jump.pm
      $c->log->debug( 'Family::get_data: got a family using a previous ID' )
        if $c->debug;
      $c->stash->{pfam}      = $pfam;
      $c->stash->{acc}       = $pfam->pfama_acc;
      $c->stash->{entryType} = 'R';
      return;
    }
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
  my $auto_pfamA = $c->stash->{pfam}->auto_pfama;
  my $rs = $c->model('PfamDB::PfamaInteractions')
             ->search( { auto_pfama_a => $auto_pfamA },
                       { select => [ { count => 'auto_pfama_a' } ],
                         as     => [ qw( numInts ) ] } )
             ->first;
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
  $xRefs->{entryAcc} = $c->stash->{pfam}->pfama_acc;
  $xRefs->{entryId}  = $c->stash->{pfam}->pfama_id;

  # Interpro
  my $i = $c->model('PfamDB::Interpro')
            ->find( $c->stash->{pfam}->auto_pfama, 
                    { key => 'UQ_interpro_1' } );

  push @{ $xRefs->{interpro} }, $i if defined $i;

  # PDB
  $xRefs->{pdb} = keys %{ $c->stash->{pdbUnique} }
    if $c->stash->{summaryData}{numStructures};


  # PfamA relationship based on SCOOP
  my @ataSCOOP = $c->model('PfamDB::Pfama2pfamaScoopResults')
                   ->search( { auto_pfama1 => $c->stash->{pfam}->auto_pfama,
                               score       => { '>', 50.0 } },
                             { join        => [ qw( pfamA1 pfamA2 ) ],
                               select      => [ qw( pfamA1.pfama_id 
                                                    pfamA2.pfama_id
                                                    pfamA1.pfama_acc
                                                    pfamA2.pfama_acc
                                                    score ) ],
                               as          => [ qw( l_pfama_id
                                                    r_pfama_id
                                                    l_pfama_acc
                                                    r_pfama_acc
                                                    score ) ]
                             } );
  
  foreach my $ref ( @ataSCOOP ) {
    if ( $ref->get_column('l_pfama_acc') ne $ref->get_column('r_pfama_acc') ) {
      push @{ $xRefs->{scoop} }, $ref;
    }
  }

  # PfamA to PfamB links based on ADDA
  my %atobPRODOM;
  foreach my $xref ( $c->stash->{pfam}->pfama_database_links ) {
    if ( $xref->db_id eq 'PFAMB' ) {
      $atobPRODOM{$xref->db_link} = $xref;
    }
    else {
      push @{ $xRefs->{$xref->db_id} }, $xref;
    }
  }

  # PfamA to PfamA links based on PRC
  my @atoaHH = $c->model('PfamDB::Pfama2pfamaHhsearchResults')
                 ->search( { 'auto_pfama1.pfama_acc' => $c->stash->{pfam}->pfama_acc },
                           { join     => [ qw( auto_pfama1 auto_pfama2 ) ],
                             select   => [ qw( auto_pfama1.pfama_id 
                                               auto_pfama1.pfama_acc
                                               auto_pfama2.pfama_id 
                                               auto_pfama2.pfama_acc 
                                               evalue ) ],
                             as       => [ qw( l_pfama_id 
                                               l_pfama_acc 
                                               r_pfama_id 
                                               r_pfama_acc 
                                               evalue ) ],
                             order_by => 'auto_pfama2.auto_pfama ASC'
                           } );
                           
  $xRefs->{atoaHH} = [];
  foreach ( @atoaHH ) {
    if ( $_->get_column( 'evalue' ) <= 0.001 and
         $_->get_column( 'l_pfama_id' ) ne $_->get_column( 'r_pfama_id' ) ) {
      push @{ $xRefs->{atoaHH} }, $_;
    } 
  }

#  # PfamA to PfamA links based on PRC
#  my @atoaPRC = $c->model('PfamDB::Pfama2pfamaPrcResults')
#                  ->search( { 'pfamA1.pfama_acc' => $c->stash->{pfam}->pfama_acc },
#                            { join               => [ qw( pfamA1 pfamA2 ) ],
#                              select             => [ qw( pfamA1.pfama_id 
#                                                          pfamA1.pfama_acc
#                                                          pfamA2.pfama_id 
#                                                          pfamA2.pfama_acc 
#                                                          evalue ) ],
#                              as                 => [ qw( l_pfama_id 
#                                                          l_pfama_acc 
#                                                          r_pfama_id 
#                                                          r_pfama_acc 
#                                                          evalue ) ],
#                              order_by           => 'pfamA2.auto_pfama ASC'
#                            } );
#
#  $xRefs->{atoaPRC} = [];
#  foreach ( @atoaPRC ) {
#    if ( $_->get_column( 'evalue' ) <= 0.001 and
#         $_->get_column( 'l_pfama_id' ) ne $_->get_column( 'r_pfama_id' ) ) {
#      push @{ $xRefs->{atoaPRC} }, $_;
#    } 
#  }

  # PfamB to PfamA links based on PRC
  my @atobPRC = $c->model('PfamDB::Pfamb2pfamaPrcResults')
                  ->search( { 'auto_pfama.pfama_acc' => $c->stash->{pfam}->pfama_acc, },
                            { join      => [ qw( auto_pfama auto_pfamb ) ],
                              prefetch  => [ qw( auto_pfama auto_pfamb ) ]
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

  my @goTerms = $c->model('PfamDB::GeneOntology')
                  ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama } );

  $c->stash->{goTerms} = \@goTerms;
}

#-------------------------------------------------------------------------------

=head2 get_interactions : Private

Retrieves details of the interactions between this family and others.

=cut

sub get_interactions : Private {
  my ( $this, $c ) = @_;
  
  my @interactions = $c->model('PfamDB::PfamaInteractions')
                       ->search( { auto_pfama_a => $c->stash->{pfam}->auto_pfama },
                                 { join     => [ qw( auto_pfama_b ) ],
                                   prefetch => [ qw( auto_pfama_b ) ] } );

  $c->stash->{interactions} = \@interactions;
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
