
# Protein.pm
# jt6 20060427 WTSI
#
# $Id: Protein.pm,v 1.25 2007-04-20 15:34:07 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family - controller to build the main protein
page

=cut

package PfamWeb::Controller::Protein;

=head1 DESCRIPTION

This is intended to be the base class for everything related to
UniProt entries across the site. 
Generates a B<tabbed page>.

$Id: Protein.pm,v 1.25 2007-04-20 15:34:07 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use Storable qw(thaw);
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use Bio::Pfam::AnnotatedSequence;
use Bio::Pfam::AnnotatedRegion;
use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::OtherRegion;
use Bio::Pfam::SmartRegion;
use Bio::Pfam::ContextPfamRegion;
use Bio::Pfam::CATHRegion;
use Bio::Pfam::SCOPRegion;
use Bio::Pfam::SeqPfam;
use Bio::Pfam::HMMOtherRegion;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::SeqFeature::Generic;

use base "PfamWeb::Controller::Section";

__PACKAGE__->config( SECTION => "protein" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Get the data from the database for the UniProt entry.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  #----------------------------------------
  # get the accession or ID code

  my $p;
  if ( defined $c->req->param("acc") ) {

    $c->req->param( "acc" ) =~ m/^([OPQ]\d[A-Z0-9]{3}\d)$/i;
    $c->log->info( "Protein::begin: found a uniprot accession |$1|" );
  
    # try a lookup in the main pfamseq table first
    $p = $c->model("PfamDB::Pfamseq")->find( { pfamseq_acc => $1 } );
  
    # if we got a result there, so much the better...
    unless( defined $p ) {
  
      # ... otherwise, see if this is really a secondary accession
      $p = $c->model("PfamDB::Secondary_pfamseq_acc")
        ->find( { secondary_acc => $1 },
                { join          => [ qw/pfamseq/ ],
  				prefetch      => [ qw/pfamseq/ ] } );
    }
  
  } elsif ( defined $c->req->param("id") ) {

    $c->req->param("id") =~ m/^(\w+)$/;
    $c->log->info("Protein::begin: found a uniprot ID |$1|");
  
    # try a lookup in the main pfamseq table first
    $p = $c->model("PfamDB::Pfamseq")->find( { pfamseq_id => $1 } );
  
  } elsif ( defined $c->req->param("entry") ) {

    # we don't know if this is an accession or an ID; try both
  
    if ( $c->req->param("entry") =~ m/^([OPQ]\d[A-Z0-9]{3}\d)$/i ) {
  
      # looks like an accession; redirect to this action, appending the accession
      $c->log->debug(
             "Protein::begin: looks like a uniprot accession ($1); redirecting");
      $c->res->redirect( $c->uri_for( "/protein", { acc => $1 } ) );
      return 1;
  
    } elsif ( $c->req->param("entry") =~ m/^(\w+_\w+)$/ ) {
  
      # looks like an ID; redirect to this action, appending the ID
      $c->log->debug("Protein::begin: looks like a uniprot ID; redirecting");
      $c->res->redirect( $c->uri_for( "/protein", { id => $1 } ) );
      return 1;
    }
  }

  # we're done here unless there's an entry specified
  unless ( defined $p ) {

    # de-taint the accession or ID
    my $input = $c->req->param("acc")
      || $c->req->param("id")
      || $c->req->param("entry")
        || "";
    $input =~ s/^(\w+)/$1/;
  
    # see if this was an internal link and, if so, report it
    my $b = $c->req->base;
    if ( $c->req->referer =~ /^$b/ ) {
  
      # this means that the link that got us here was somewhere within
      # the Pfam site and that the accession or ID which it specified
      # doesn't actually exist in the DB
  
      # report the error as a broken internal link
      $c->error(
          "Found a broken internal link; no valid UniProt accession or ID "
          . "(\"$input\") in \""
          . $c->req->referer
          . "\"" );
      $c->forward("/reportError");
  
      # now reset the errors array so that we can add the message for
      # public consumption
      $c->clear_errors;
  
    }
  
    # the message that we'll show to the user
    $c->stash->{errorMsg} = "No valid UniProt accession or ID";
  
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn("Family::begin: no valid UniProt ID or accession");
  
    return;
  }

  $c->log->debug("Protein::begin: successfully retrieved a pfamseq object");
  $c->stash->{pfamseq} = $p;
  
  #----------------------------------------
  # add extra data to the stash

  $c->forward("_generatePfamGraphic");
  $c->forward("_getDasSources");
  $c->forward("_getMapping");
  $c->forward("_getSummaryData");
  $c->forward("_getGenomeData");

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _getDasSources : Private

Retrieves DAS sources with the appropriate object type and coordinate system.

(Author aj5)

=cut

sub _getDasSources : Private {
  my ($this, $c) = @_;
  
  my @dasSources = $c->model("WebUser::Feature_das_sources")->search();
  my $keptSources = {};
  my ($baseCoord, $baseType) = ('UniProt', 'Protein Sequence');
  my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;
  my $dl = $c->model("PfamDB")->getDasLite;
  my %alignMatches;
  
  FEATURE: foreach my $f (@dasSources) {
    if ($f->sequence_type eq $baseType and $f->system eq $baseCoord) {
      push (@{ $keptSources->{$f->sequence_type}{$f->system} }, $f);
      next;
    }
    ALN: foreach my $a ($f->alignment_sources_to) {
      next ALN unless (defined $a);
      next ALN unless ($a->from_type eq $baseType and $a->from_system eq $baseCoord);
    
      # Find out if we have any alignments for this object type and co-ord system.
      unless (defined $alignMatches{$f->sequence_type}{$f->system}) {
        $dl->dsn( [$a->url] );
        my (undef, $alignments) = each %{ $dl->alignment( { 'query' => $seqAcc } ) };
        if (ref $alignments eq 'ARRAY' and scalar @{$alignments}) {
          $alignMatches{$f->sequence_type}{$f->system} = 1;
        } else {
          $alignMatches{$f->sequence_type}{$f->system} = 0;
        }
      }
      
      push @{ $keptSources->{$f->sequence_type}{$f->system} }, $f 
        if $alignMatches{$f->sequence_type}{$f->system};
      next FEATURE;
    }
  }
  
  my @keptSourcesArr = ();
  my @types = _sortWithPref($baseType, keys %{ $keptSources } );
  foreach my $type (@types) {
    my @systems = _sortWithPref($baseCoord, keys %{ $keptSources->{$type} } );
    foreach my $system (@systems) {
      my $id = $type.'_'.$system;
      $id =~ s/\s+/_/g;
        push (@keptSourcesArr, { type=>$type, system=>$system, servers=>$keptSources->{$type}{$system}, id=>$id } );
    }
  }
  $c->stash->{dasSourcesRs} = \@keptSourcesArr;

  $c->log->debug("Protein::begin: added DAS sources to the stash");
}

#-------------------------------------------------------------------------------

=head2 _sortWithPref

A regular Perl method implementing a sort

=cut

sub _sortWithPref {
  my $pref = shift;
  return sort {
    my $i = (lc $a) cmp (lc $b);
    return $i if $i == 0;
    if( $a eq $pref ) {
      $i = -1;
    } elsif( $b eq $pref ) {
      $i = 1;
    }
    return $i;
  } @_;
}

#-------------------------------------------------------------------------------

=head2 _getGenomeData : Private

Retrieves the genome information for a given sequence.

=cut

sub _getGenomeData : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model( "PfamDB::Genome_pfamseq")
             ->find( { auto_pfamseq => $c->stash->{pfamseq}->auto_pfamseq },
                     { join         => [ qw/genome_species/ ],
                       prefetch     => [ qw/genome_species/ ] } );

  $c->stash->{genomeCode} = $rs->ncbi_code if $rs;

  # Rob says we (he) might be able to improve this...
}

#-------------------------------------------------------------------------------

=head2 _getMapping : Private

Gets the structure mapping

=cut

sub _getMapping : Private {
  my ( $this, $c ) = @_;

  # get a layout manager and set the X scale

  my @mapping = $c->model("PfamDB::PdbMap")
    ->search( { "pfamseq.auto_pfamseq" => $c->stash->{pfamseq}->auto_pfamseq,
                "pfam_region"          => 1 },
              {  join                  => [ qw/pfamA pfamseq pdb/ ],
                 prefetch              => [ qw/pfamA pfamseq pdb/ ] } );

  $c->stash->{pfamMaps} = \@mapping;

  $c->log->debug("Protein::begin: added the structure mapping to the stash");
}

#-------------------------------------------------------------------------------

=head2 _generatePfamGraphic : Private

Generates the Pfam graphic

=cut

sub _generatePfamGraphic : Private {
  my ( $this, $c ) = @_;

  # get a layout manager and set the X scale
  my $layoutPfam = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layoutPfam->scale_x(1);
  $c->log->debug("Protein::begin: instantiated a layout manager");

  # retrieve the Storable containing the annotated sequence, thaw it
  # and hand it off to the layout manager
  my $annseq;
  eval {
    $annseq = thaw( $c->stash->{pfamseq}->annseq->annseq_storable );
  };
  if ($@) {
    $c->log->error("Protein::begin: ERROR: failed to thaw annseq: $@");
  }

  $layoutPfam->layout_sequences_with_regions_and_features( [$annseq],
                                                           { PfamA      => 1,
                                                             PfamB      => 1,
                                                             noFeatures => 0 } );

  # and build an imageset
  my $pfamImageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $pfamImageset->create_images( $layoutPfam->layout_to_XMLDOM );
  $c->log->debug("Protein::begin: created images");

  $c->stash->{pfamImageset} = $pfamImageset;

  $c->log->debug("Protein::begin: successfully generated an imageset object");
}

#-------------------------------------------------------------------------------

=head2 _getSummaryData : Private

Get the data items for the overview bar

=cut

sub _getSummaryData : Private {
  my ( $this, $c ) = @_;

  my %summaryData;

  # first, the number of sequences... pretty easy...
  $summaryData{numSequences} = 1;

  # also, the number of architectures
  $summaryData{numArchitectures} = 1;

  # number of species
  $summaryData{numSpecies} = 1;

  # number of structures
  my $rs = $c->model("PfamDB::Pdb_residue")
             ->find( { auto_pfamseq => $c->stash->{pfamseq}->auto_pfamseq },
			               { select       => [
							                           {
    								                       count => [ { distinct => [qw/auto_pdb/] } ]
							                           }
							                         ],
                       as           => [ qw/numberPdbs/ ] } );

  $summaryData{numStructures} = $rs->get_column("numberPdbs");

  # number of interactions
  $rs = $c->model("PfamDB::Interactions")
          ->find( { auto_pfamseq_A => $c->stash->{pfamseq}->auto_pfamseq },
                  { select         => [
								                        {
								                          count => [ { distinct => ["auto_int_pfamAs"] } ]
								                        }
								                      ],
			              as             => [ qw/numInts/ ] } );

  $summaryData{numInt} = $rs->get_column("numInts");

  $c->stash->{summaryData} = \%summaryData;

  $c->log->debug("Protein::begin: added the summary data to the stash");
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

Andy Jenkinson, C<aj5@sanger.ac.uk>

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
