
# Genome.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Genome - role containing actions related to genomes

=cut

package RfamWeb::Roles::Genome;

=head1 DESCRIPTION

This is a role to add functionality to the genome controller. These methods,
however, aren't very useful. They were intended to allow us to serve data for
the UCSC genome browser via the website, but it turns out that that requires
its data served via FTP...

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 hub_txt : Path

Returns a "hub.txt".

=cut

sub hub_txt : Path('/genome/hub.txt') {
  my ( $this, $c ) = @_;

  $c->log->debug( dump $c->req->headers )
    if $c->debug;

  # $c->cache_page( 2419200 );

  $c->res->content_type( 'text/plain' );
  # $c->res->header( 'Content-disposition' => "attachment; filename=hub.txt" );
  $c->res->body( <<EOF_hub );
hub rfam
shortLabel rfam_ncRNA
longLabel Rfam non-coding RNA annotation
genomesFile genomes.txt
email rfam-help\@sanger.ac.uk
EOF_hub
}
  
#-------------------------------------------------------------------------------

=head2 genomes_txt : Path

Returns a "genomes.txt" file.

=cut

sub genomes_txt : Path('/genome/genomes.txt') {
  my ( $this, $c ) = @_;

  # $c->cache_page( 2419200 );

  my $rs = $c->model('GenomeBigbed')
             ->search( {}, {} );

  my $content = '';
  while ( my $genome = $rs->next ) {
    $content .= 'genome ' . $genome->code . "\n";
    $content .= 'trackDb ' . $c->uri_for( '/genome/' . $genome->ncbi_id . '/trackDb.txt' ) . "\n\n";
  }

  $c->res->content_type( 'text/plain' );
  # $c->res->header( 'Content-disposition' => "attachment; filename=genomes.txt" );
  $c->res->body( $content );
}
  
#-------------------------------------------------------------------------------

=head2 trackdb_txt : Chained

Returns a "trackDb.txt" file for the specified genome.

=cut

sub trackdb_txt : Chained('genome')
                  PathPart('trackDb.txt')
                  Args(0) {
  my ( $this, $c ) = @_;

  # $c->cache_page( 2419200 );

  my $bigbed = $c->uri_for( '/genome/' . $c->stash->{ncbi_id} . '/bigbed' );

  $c->res->content_type( 'text/plain' );
  # $c->res->header( 'Content-disposition' => "attachment; filename=trackDb.txt" );
  $c->res->body( <<EOF_trackdb );
track Rfam 
bigDataUrl $bigbed
shortLabel Rfam ncRNA
longLabel Rfam ncRNA annotations 
type bigBed 8
url http://rfam.sanger.ac.uk/family/\$\$
visibility 3
color 102,0,0
EOF_trackdb
}
  
#-------------------------------------------------------------------------------

=head2 bigbed : Chained

Returns the bigbed file for the specified genome, if it exists. 

=cut

sub bigbed : Chained( 'genome' )
             PathPart( 'bigbed' )
             Args( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Genome::bigbed: retrieving a bigbed file' )
    if $c->debug;
  
  my $rs = $c->model('RfamDB::GenomeBigbed')
             ->find( { ncbi_id => $c->stash->{ncbi_id} } );
          
  unless ( defined $rs ) {
    $c->log->debug( 'Genome::bigbed: no bigbed found for this genome' )
      if $c->debug;

    $c->res->status(404); # Not found

    return;
  }

  my $filename = $rs->code . '.bigBed';
  
  $c->res->content_type( 'application/octet-stream' );
  # $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $rs->bigbed );
}


#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012 Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk)

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


