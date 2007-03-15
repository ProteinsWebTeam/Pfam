# Domain.pm
# rdf 20060818 WTSI
#
# Controller to build the main Domain page.
#
# $Id: DomainDomain.pm,v 1.3 2007-03-15 14:06:11 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Int::DomainDomain - controller for iPfam domain-domain
interactions.

=cut

=head1 DESCRIPTION

Given two Pfam domain accessions or IDs, this Controller retrieves the
interactions between the two.

$Id: DomainDomain.pm,v 1.3 2007-03-15 14:06:11 jt6 Exp $

=cut

package PfamWeb::Controller::Int::DomainDomain;

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generateDomainDomainIntSum

Generates the summary data for a domain-domain interaction page.

=cut

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

  my @rs = $c->model("PfamDB::Interactions")
	->search( { auto_pfamA_A => $c->stash->{pfam_A}->auto_pfamA,
				auto_pfamA_B => $c->stash->{pfam_B}->auto_pfamA } );
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

=head2 end : Private

Overrides the default end to forward to the iPfam template.

=cut

# TODO: need to figure out if we really need to do this or if we can leave the
# rendering to the default end

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
