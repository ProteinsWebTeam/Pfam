
# Proteome.pm
# rdf 20060821 WTSI
#
# Controller to build the main Pfam Proteome page.
#
# $Id: Proteome.pm,v 1.3 2006-11-13 15:32:10 rdf Exp $

package PfamWeb::Controller::Proteome;

use strict;
use warnings;
use Data::Dumper;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "proteome" );

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param( "ncbiCode" ) ) {

    my ($ncbiCode) = $c->req->param( "ncbiCode" ) =~ m/^(\d+)$/i;
    $c->log->info( "Proteome::begin: found ncbiCode |$ncbiCode|" );

    $c->stash->{proteomeSpecies} = $c->model("PfamDB::Genome_species")->find( { ncbi_code => $ncbiCode});

    $c->forward( "_getSummaryData" );
  }
}


#-------------------------------------------------------------------------------
# get the data items for the overview bar

sub _getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;
  #select distinct auto_architecture from pfamseq where ncbi_code=62977 and genome_seq=1;
  my $rs = $c->model("PfamDB::Pfamseq")->find({ ncbi_code  => $c->stash->{proteomeSpecies}->ncbi_code,
						genome_seq => 1},
					      { select => [
							   { count => [
								       { distinct => [ "auto_architecture" ] }
								      ]
							   }
							  ],
						as => [ qw/numArch/ ]
					      });
  $summaryData{numArchitectures} = $rs->get_column( "numArch" );;

  # number of sequences in proteome.
  $summaryData{numSequences} = $c->stash->{proteomeSpecies}->total_genome_proteins;


  # number of structures known for the domain
  #Release 21.0 need something like this
  
  #select distinct auto_pdb from pdb_pfamA_reg a, pfamseq s where s.auto_pfamseq=a.auto_pfamseq and ncbi_code=62977 and genome_seq=1;
  $rs = $c->model("PfamDB::Pdb_pfamA_reg")->find({ "pfamseq.ncbi_code"  => $c->stash->{proteomeSpecies}->ncbi_code,
						   "pfamseq.genome_seq" => 1},
						 { select => [
							      { count => [
									  { distinct => [ "auto_pdb" ] }
									 ]
							      }
							     ],
						   as => [ qw/numPdb/ ],
						   join => [qw/pfamseq/],
						 });
   $summaryData{numStructures}  = $rs->get_column( "numPdb" );

  # number of species
  #AS we are dealing with a proteome, this is 1
  $summaryData{numSpecies} = 1;
  # number of interactions - For now set to zero
  $summaryData{numInt} = 0;
  $c->stash->{summaryData} = \%summaryData;

}

1;

