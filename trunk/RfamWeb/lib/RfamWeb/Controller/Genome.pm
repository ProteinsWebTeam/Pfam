
# Genome.pm
# jt6 20081127 WTSI
#
# $Id: Genome.pm,v 1.3 2009-06-10 15:05:46 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Genome - controller for the genome page

=cut

package RfamWeb::Controller::Genome;

=head1 DESCRIPTION

Controller to build the main Rfam genome page.

$Id: Genome.pm,v 1.3 2009-06-10 15:05:46 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );
use Compress::Zlib;

use base 'RfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'genome' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 genome

The start of a chain for genomes. Tries to process its argument as an NCBI 
taxonomy id.

=cut

sub genome : Chained( '/' )
             PathPart( 'genome' )
             CaptureArgs( 1 ) {
  my ( $this, $c, $tainted_entry ) = @_;
  
  $c->log->debug( 'Genome::genome: start of "genome" chain' )
    if $c->debug;

  my ( $entry ) = $tainted_entry =~ m/^(\w+)$/;
  unless ( defined $entry ) {
    $c->log->debug( 'Genome::genome: no valid tax ID found' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Invalid NCBI taxonomy ID';

    return;
  }
  
  # retrieve data for the genome
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------

=head2 genome_page

Generates the "genome" page. Processes the first argument as an NCBI taxonomy
ID and adds summary data to the stash. Start and end of a chain.

=cut

sub genome_page : Chained( '/' )
                  PathPart( 'genome' )
                  Args( 1 ) {
  my ( $this, $c, $tainted_entry ) = @_;

  $c->cache_page( 604800 );
  
  $c->log->debug( 'Genome::genome_page: building a "genome" page' )
    if $c->debug;

  my ( $entry ) = $tainted_entry =~ m/^(\w+)$/;
  unless ( defined $entry ) {
    $c->log->debug( 'Genome::genome_page: no valid tax ID found' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Invalid NCBI taxonomy ID';

    return;
  }
  
  # retrieve data for the genome
  $c->forward( 'get_data', [ $entry ] );

  #----------------------------------------
  
  unless ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Genome::get_data: NOT returning XML; adding extra info' ) 
        if $c->debug;
    
    # if this request originates at the top level of the object hierarchy,
    # i.e. if it's a call on the "default" method of the Family object,
    # then we'll need to do a few extra things
    if ( ref $this eq 'RfamWeb::Controller::Genome' ) {
      $c->log->debug( 'Genome::genome_page: adding extra genome info' ) if $c->debug;
      
      $c->forward( 'get_summary_data' );
    }

  } # end of "unless XML..."

}
    
#-------------------------------------------------------------------------------

=head2 gff

Treats its argument as an auto_genome number and uses it to retrieve the GFF for 
the specified genome segment. Chained to "genome". 

=cut

sub gff : Chained( 'genome' )
          PathPart( 'gff' )
          Args( 1 ) {
  my ( $this, $c, $tainted_ag ) = @_;
  
  $c->log->debug( 'Genome::genome_page: retrieving a GFF file' )
    if $c->debug;

  my ( $ag ) = $tainted_ag =~ m/^(\w+)$/;
  unless ( defined $ag ) {
    $c->log->debug( 'Genome::genome_page: no valid auto_genome found' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Invalid genome segment identifier';

    return;
  }
  
  my $rs = $c->model('RfamDB::GenomeGff')
             ->search( { 'me.auto_genome' => $ag },
                       { join => [ 'auto_genome' ] } );
          
  unless ( defined $rs ) {
    $c->log->debug( 'Genome::genome_page: no GFF found auto_genome for that segment' )
      if $c->debug;

    $c->stash->{errorMsg} = 'No GFF found for that genome segment';

    return;
  }

  my $gff = Compress::Zlib::memGunzip( $rs->first->gff3 );
  
  my $filename;
  my $ge = $rs->first->auto_genome;
  if ( defined $ge->genome_acc ) {
    $filename = $ge->genome_acc . '.gff';
    $c->log->debug( "Genome::gff: using 'genome_acc' for filename: |$filename|" )
      if $c->debug;
  }
  else {  
    $filename = $ge->ensembl_id . '.gff';
    $c->log->debug( "Genome::gff: using 'ensembl_id' for filename: |$filename|" )
      if $c->debug;
  }
  
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $gff );
}

#-------------------------------------------------------------------------------

=head2 all_gffs

Returns the concatenated contents of all of the available GFF files for this 
genome. Chained to "genome". 

=cut

#sub all_gffs : Chained( 'genome' )
#               PathPart( 'gff' )
#               Args( 0 ) {
#  my ( $this, $c ) = @_;
#  
#  $c->log->debug( 'Genome::genome_page: retrieving all GFF files for this genome' )
#    if $c->debug;
#
#  my @ags = ();
#  foreach my $ag ( sort keys %{ $c->stash->{hits} } ) {
#    $c->log->debug( "Genome::all_gffs: auto_genome, hits: |$ag|"
#                    . $c->stash->{hits}->{$ag}->{count} . '|' )
#      if $c->debug;
#    next unless $c->stash->{hits}->{$ag}->{count};
#    push @ags, $ag;
#  }
#
#  $c->log->debug( 'Genome::all_gffs: auto_genomes which have hits: |' 
#                  . join( ', ', @ags ) . '|' )
#    if $c->debug;
#
#  my @gffs = $c->model('RfamDB::GenomeGff')
#               ->search( { auto_genome => { 'IN', \@ags } },
#                         { order_by => [ 'auto_genome' ] } );
#
#  $c->log->debug( 'Genome::all_gffs: found |' . scalar @gffs . '| GFF files' ) 
#    if $c->debug;
#             
#  foreach my $gff ( @gffs ) {
#    $c->log->debug( 'Genome::all_gffs: gff auto_genome:|' . $gff->auto_genome . '|' ) 
#      if $c->debug;
#  }    
#             
#}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves genome data for the given entry.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # get the genome breakdown
  my @genomes = $c->model('RfamDB::GenomeEntry')
                  ->search( {
                              -or =>  [ { ncbi_id    => $entry },
                                        { genome_acc => $entry },
                                        { ensembl_id => $entry } ],
                              -and => { 'regions.genome_start' => { '!=', undef }, 
                                        'regions.genome_end'   => { '!=', undef } }
                            },
                            {
                              join      => { 'regions' => 'auto_rfam' },
                              order_by  => [ qw( auto_genome
                                                 regions.genome_start ) ],
                              '+select' => [ qw( regions.auto_rfam
                                                 regions.genome_start
                                                 regions.genome_end
                                                 regions.bits_score
                                                 auto_rfam.rfam_id
                                                 auto_rfam.rfam_acc ) ],
                              '+as'     => [ qw( auto_rfam 
                                                 genome_start
                                                 genome_end
                                                 bits_score
                                                 rfam_id
                                                 rfam_acc ) ] 
                            } );

  unless ( @genomes ) {
    $c->log->debug( "Genome::get_data: failed to find a genome for '$entry'" )
      if $c->debug;
    
    $c->stash->{errorMsg} = "No genome for taxonomy ID $entry";

    return;
  }

  #----------------------------------------

  $c->log->debug( 'Genome::get_data found ' . scalar @genomes . ' genome rows' )
    if $c->debug;

  # get the summary for this genome
  my $summary = $c->model('RfamDB::GenomeSummary')
                  ->find( { ncbi_id => $genomes[0]->ncbi_id } );

  unless ( defined $summary ) {
    $c->log->debug( "Genome::get_data: failed to find a genome summary for '$entry'" )
      if $c->debug;
    
    $c->stash->{errorMsg} = "No genome summary for taxonomy ID $entry";

    return;
  }

  $c->log->debug( 'Genome::get_data: got a genome summary' ) if $c->debug;
  $c->stash->{summary} = $summary;
  
  #----------------------------------------

  # hash the breakdown in different ways
  my $hits            = {};
  my $hit_chromosomes = {};
  foreach my $row ( @genomes ) {
    my $ag = $row->auto_genome;

    push @{ $hits->{$ag}->{rows} }, $row;

    $hit_chromosomes->{$ag} = 1;   

    $hits->{$ag}->{count}++    if defined $row->get_column('auto_rfam');
    $hits->{$ag}->{families}++ if defined $row->get_column('auto_rfam');

  }

  $c->stash->{hits}            = $hits;
  $c->stash->{hit_chromosomes} = $hit_chromosomes;
}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Just gets the data items for the overview bar.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $summaryData = {};

  # number of sequences in full alignment
  $summaryData->{numSequences} = scalar( keys %{$c->stash->{hit_chromosomes}} );

  # number of interactions
  $summaryData->{numInt} = 0;

  # Number of species
  $summaryData->{numSpecies} = 1;

  # number of structures known for the domain
  $summaryData->{numStructures} = 0;

  $c->stash->{summaryData} = $summaryData;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
