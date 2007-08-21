
# Proteome.pm
# rdf 20060821 WTSI
#
# Controller to build the main Pfam Proteome page.
#
# $Id: Proteome.pm,v 1.9 2007-08-21 12:35:24 rdf Exp $

=head1 NAME

PfamWeb::Controller::Proteome- controller for the proteome section

=cut

package PfamWeb::Controller::Proteome;

=head1 DESCRIPTION

This is intended to be the base class for everything related to clans
across the site. The L<begin|/"begin : Private"> method will try to
extract a clan ID or accession from the captured URL and then try to
load a Clan object from the model into the stash.

Generates a B<tabbed page>.

$Id: Proteome.pm,v 1.9 2007-08-21 12:35:24 rdf Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'proteome' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract an NCBI code from the parameters and retrieves the appropriate
row for it.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param('taxId') and 
      $c->req->param('taxId') =~ m/^(\d+)$/i ) {
    
    $c->log->info( "Proteome::begin: found tax ID |$1|" );
    $c->stash->{taxId} = $1;
    
    $c->stash->{proteomeSpecies} = $c->model("PfamDB::Proteome_species")
                                     ->find( { ncbi_code => $1},
                                             { join      => [ qw( ncbi_tax ) ],
                                               prefetch  => [ qw( ncbi_tax ) ] } 
                                           );

    $c->forward( 'getSummaryData' );
    $c->forward( 'getStats');
  }
  
  # throw an error unless there's something in the stash
  unless( defined $c->stash->{proteomeSpecies} and 
          $c->stash->{proteomeSpecies}->ncbi_code  ) {

    # de-taint the ncbi code
    my $input = $c->req->param('taxId') || '';
    $input =~ s/^(\w+)/$1/;
    
    # see if this was an internal link and, if so, report it
    my $b = $c->req->base;
    if( defined $c->req->referer and $c->req->referer =~ /^$b/ ) {
  
      # this means that the link that got us here was somewhere within
      # the Pfam site and that the ncbi code which it specified
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
    $c->stash->{errorMsg} = 'NCBI taxonomy ID was either invalid or the proteome was not found';
    
    # log a warning and we're done; drop out to the end method which
    # will put up the standard error page
    $c->log->warn( 'Proteome::begin: tax ID invalid or not found' );
     
    return; 
  }
}


#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub stats : Local {
  my( $this, $c ) = @_;
  
  $c->stash->{template} = 'components/blocks/proteome/statsTable.tt';
  
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getSummaryData : Private

Just gets the data items for the overview bar.

=cut

sub getSummaryData : Private {
  my( $this, $c ) = @_;

  my %summaryData;

  #----------------------------------------

  # number of architectures
  my $rs = $c->model( 'PfamDB::Pfamseq' )
             ->find( { ncbi_code  => $c->stash->{proteomeSpecies}->ncbi_code,
                       genome_seq => 1 },
                     { select     => [
                                       {
                                         count => [
                                                    { distinct => [ qw( auto_architecture ) ] }
                                                  ]
                                       }
                                     ],
                       as         => [ qw( numArch ) ] } );

  $summaryData{numArchitectures} = $rs->get_column( 'numArch' );

  #----------------------------------------

  # number of sequences
  $summaryData{numSequences} = $c->stash->{proteomeSpecies}->total_genome_proteins;

  #----------------------------------------

  # number of structures
  $rs = $c->model( 'PfamDB::Pdb_pfamA_reg' )
          ->find( { 'pfamseq.ncbi_code'  => $c->stash->{proteomeSpecies}->ncbi_code,
                    'pfamseq.genome_seq' => 1 },
                    { select => [
                                  {
                                    count => [
                                               { distinct => [ qw( auto_pdb ) ] }
                                             ]
                                  }
                                ],
                      as     => [ qw( numPdb ) ],
                      join   => [ qw( pfamseq ) ] } );

  $summaryData{numStructures}  = $rs->get_column( 'numPdb' );

  #----------------------------------------

  # number of species. As we are dealing with a proteome, this is 1
  $summaryData{numSpecies} = 1;

  #----------------------------------------

  # number of interactions - For now set to zero
  $summaryData{numInt} = 0;

  #----------------------------------------

  $c->stash->{summaryData} = \%summaryData;
}

#-------------------------------------------------------------------------------

=head2 getStats : Private

Just gets the data items for the stats Page. This is really quick

=cut

sub getStats : Private {
  my( $this, $c ) = @_;
  $c->log->debug( 'Proteome::getStatus: getting domain statistics...' );

  my @rs = $c->model("PfamDB::Proteome_seqs")
             ->search( { ncbi_code => $c->stash->{proteomeSpecies}->ncbi_code },
                       { join      => [ qw( pfam ) ],
                         select    => [ qw( pfam.pfamA_id
                                            pfam.pfamA_acc
                                            pfam.description
                                            me.auto_pfamA ), 
                                        { count => 'auto_pfamseq' }, 
                                        { sum   => 'me.count' } ],
                         as        => [ qw( pfamA_id 
                                            pfamA_acc 
                                            description 
                                            auto_pfamA 
                                            numberSeqs 
                                            numberRegs ) ],
                         group_by => [ qw( me.auto_pfamA ) ],
                         order_by => \'sum(me.count) DESC', 
                         #prefetch => [ qw( pfam ) ]
                       }
                     );
  $c->log->debug( 'Proteome::getStats: found |' . scalar @rs . '| rows' );

  $c->stash->{statsData} = \@rs;
}
#-------------------------------------------------------------------------------


=head2 getStats : Private

Just gets the data items for the stats Page. This is really quick

=cut

sub getStructure : Local {
  my ($this, $c) = @_;
  
  $c->log->debug( 'Proteome::getStructure: getting structure mapping...' );
  
  my @mapping = $c->model("PfamDB::Pdb_PfamA_reg")
                ->search({ 'pfamseq.ncbi_code'  => $c->stash->{proteomeSpecies}->ncbi_code,
                           'pfamseq.genome_seq' => 1 },
                          { join => [ qw(pfamseq pdb) ],
                            prefetch => [ qw (pfamseq pdb ) ]});
    
  $c->stash->{pfamMaps} = \@mapping;

  $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  
  # cache the template output for one week
  $c->cache_page( 604800 );
  
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

