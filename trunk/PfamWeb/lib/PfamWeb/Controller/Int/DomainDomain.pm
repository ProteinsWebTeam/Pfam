# Domain.pm
# rdf 20060818 WTSI
#
# Controller to build the main Domain page.
#
# $Id: DomainDomain.pm,v 1.1 2006-09-28 09:51:41 rdf Exp $

package PfamWeb::Controller::Int::DomainDomain;

use strict;
use warnings;
use Data::Dumper;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/int

sub generateDomainDomainIntSum : Path {
  my( $this, $c ) = @_;

  $c->log->debug("Int::Domain::generateDomainDomainIntSum: Hello");


  if( defined $c->req->param("acc1") ) {
    $c->req->param("acc1") =~ m/^(PF\d{5})$/i;
    $c->stash->{pfam_A} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
  }
  if( defined $c->req->param("acc2") ) {
    $c->req->param("acc2") =~ m/^(PF\d{5})$/i;
    $c->stash->{pfam_B} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
  }

  my @rs = $c->model("PfamDB::Interactions")->search({auto_pfamA_A => $c->stash->{pfam_A}->auto_pfamA,
						     auto_pfamA_B => $c->stash->{pfam_B}->auto_pfamA}
						   );
  my $structures ={};
  my $sequences = {};

  foreach my $int (@rs){
    $structures->{$int->auto_pdb}++;
    $sequences->{$int->auto_pfamseq_A}++;
    $sequences->{$int->auto_pfamseq_B}++;
  }

  my %summaryData;
  $summaryData{numSpecies} = 0;
  $summaryData{numArchitectures} = 0;
  $summaryData{numStructures}  = scalar(keys(%$structures));
  $summaryData{numSequences}   = scalar(keys(%$sequences));
  $summaryData{numInt}         = 1;
  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------
sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pfam object in the stash
  #return 0 unless defined $c->stash->{ligand};

  # check for errors
  if ( scalar @{ $c->error } ) {
        $c->stash->{errors}   = $c->error;
        $c->stash->{template} = "components/blocks/ipfam/errors.tt";
  } else {
        $c->log->debug("PfamWeb::Controller::Int::Domain - Handing off to layout");
        $c->stash->{pageType} = "iDomainDomain";
        $c->stash->{template} ||= "pages/layout.tt";
  }

  # and render the page
  $c->forward( "PfamWeb::View::TT" );

  # clear any errors
  $c->error(0);

}
1;
