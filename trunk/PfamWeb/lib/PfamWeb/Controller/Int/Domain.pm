
# Domain.pm
# rdf 20060818 WTSI
#
# $Id: Domain.pm,v 1.6 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Int::Domain - controller for iPfam domains

=cut

package PfamWeb::Controller::Int::Domain;

=head1 DESCRIPTION

This is a Controller for domain interactions in the iPfam section.

Generates a B<tabbed page>.

$Id: Domain.pm,v 1.6 2008-05-16 15:29:28 jt6 Exp $

=cut

# TODO: complete this...

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# define the name of the section...
__PACKAGE__->config( SECTION => "domain" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generateDomainIntSum : Path

Generates the summary data for the domain interactions section of iPfam.

=cut

sub generateDomainIntSum : Path {
  my( $this, $c ) = @_;

  if( defined $c->req->param("acc") ) {
    $c->req->param("acc") =~ m/^(PF\d{5})$/i;
    $c->stash->{pfam} = $c->model("PfamDB::Pfam")->find( { pfamA_acc => $1 } );
  } elsif(defined $c->req->param("id") ) {

  } elsif(defined $c->req->param("entry") ) {

  }

  if($c->stash->{pfam}->pfamA_id){
    # Now that we have a pfam domain
    my @rs = $c->model("PfamDB::Int_pfamAs")
	  ->search( { auto_pfamA_A => $c->stash->{pfam}->auto_pfamA },
				{ join         => [ qw/pfamA_B/ ],
				  prefetch     => [ qw/pfamA_B/ ] } );
    if(scalar(@rs)){
      $c->stash->{domainInts} = \@rs;
      my $noSeqs = $c->model("PfamDB::Interactions")
		->find( { auto_pfamA_A => $c->stash->{pfam}->auto_pfamA },
				{ select       => { count => {distinct => "auto_pfamseq_A" } },
				  as           => [ qw/noSeqs/ ] } );
      $c->stash->{noSeqs} = $noSeqs->get_column("noSeqs");
      my $noStruc = $c->model("PfamDB::Interactions")
		->find( { auto_pfamA_A => $c->stash->{pfam}->auto_pfamA },
				{ select       => { count => {distinct => "auto_pdb" } },
				  as           => [ qw/noStrucs/ ] } );

      my %summaryData;
      $summaryData{numSpecies} = 0;
      $summaryData{numArchitectures} = 0;
      $summaryData{numStructures}  = $noSeqs->get_column("noSeqs");
      $summaryData{numSequences}   = $noStruc->get_column("noStrucs");
      $summaryData{numInt}         = scalar(@rs);
      $c->stash->{summaryData} = \%summaryData;

      $c->log->debug("Get ".$noSeqs->get_column("noSeqs")." sequences");
      $c->log->debug("Found ".scalar(@{$c->stash->{domainInts}})." domain interactions");
      $c->log->debug("Int::Domain::generateDomainIntSum: Got domain data for:");
    } else {
      $c->stash->{noiPfam} = 1;
    }
  } else {
    $c->stash->{noPfam} = 1;
  }

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

