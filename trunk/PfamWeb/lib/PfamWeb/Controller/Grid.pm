
# Root.pm
# jt 20061003 WTSI
#
# $Id: Grid.pm,v 1.2 2009-03-06 16:32:48 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Root - main class for the PfamWeb application

=cut

package PfamWeb::Controller::Grid;

=head1 DESCRIPTION

This is the root class for the Pfam website catalyst application. It
installs global actions for the main site index page and other top-level
functions.

$Id: Grid.pm,v 1.2 2009-03-06 16:32:48 jt6 Exp $

=cut

use strict;
use warnings;

use JSON;

use base 'Catalyst::Controller';

our $GRIDS = { yui       => 'pages/yui_grid.tt',
               prototype => 'pages/prototype_grid.tt',
               rico      => 'pages/rico_grid.tt',
               ext       => 'pages/ext_grid.tt' };

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 action : Attribute

Description...

=cut

sub grid : Global {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Grid::grid: rendering grid page' )
    if $c->debug;
  
  my $template = 'pages/prototype_grid.tt';
  if ( defined $c->req->param('grid') and
       exists $GRIDS->{ $c->req->param('grid') } ) {
    $template = $GRIDS->{ $c->req->param('grid') };
    $c->log->debug( "Grid::grid: using '$template' grid" )
      if $c->debug;
  }
  else {
    $c->log->warn( 'Grid::grid: not a known grid; using default' )
      if $c->debug;
  }
  
  $c->stash->{template} = $template;
    
  #$c->stash->{template} = 'pages/yui_grid.tt';
  #$c->stash->{template} = 'pages/prototype_grid.tt';
  #$c->stash->{template} = 'pages/rico_grid.tt';
  #$c->stash->{template} = 'pages/ext_grid.tt';
}

#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub ext_grid_data : Global {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Grid::ext_grid_data: returning sequence information' )
    if $c->debug;
  
  my $start   = $c->req->param('start') || 0;
  my $limit   = $c->req->param('limit') || 100;
  my $order   = $c->req->param('dir')   || 'ASC';
  my $sortcol = ( $c->req->param('sort')  eq 'acc_field' )
                ? 'pfamseq_acc'
                : 'sequence';
  
  my $rs = $c->model('PfamDB::Pfamseq')
             ->search( {},
                       { order_by => "$sortcol $order" } 
                     );
             
  $c->log->debug( 'Grid::ext_grid_data: found ' . $rs->count() . ' rows' )
    if $c->debug;

  my $data = { 
    response => {
      value => {
        version => 1,
        total_count => 5000,
        items   => []
      }
    }
  };  
  
  my $slice = $rs->slice( $start, $start + $limit );
  $c->log->debug( 'Grid::ext_grid_data: slice contains ' . $slice->count . ' rows' )
    if $c->debug;
  
  my $i = 0;
  foreach my $row ( $slice->all ) {
    push @{ $data->{response}->{value}->{items} }, 
      { id        => $i++,
        acc_field => $row->pfamseq_acc, 
        seq_field => '<span class="alignment">' . $row->sequence . '</span>' };
  }

  $c->res->content_type( 'application/json' );
  $c->res->body( to_json( $data ) );
}

#-------------------------------------------------------------------------------
# response format:
# {
#   "update_ui":"true",
#   "offset":"0",
#   "rowCount":"20",
#   "rows":[
#             {"id":"1","name":"Bob"},
#             {"id":"2","name":"Bill"}
#          ]
# }

sub rico_grid_data : Global {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Grid::rico_grid_data: returning sequence information' )
    if $c->debug;
  
  my $offset    = $c->req->param('offset')    || 0;
  my $page_size = $c->req->param('page_size') || 100;
  
  my $rs = $c->model('PfamDB::Pfamseq')
             ->search( {}, {} );
             
  $c->log->debug( 'Grid::rico_grid_data: found ' . $rs->count . ' rows' )
    if $c->debug;

  my $slice = $rs->slice( $offset, $offset + $page_size - 1 );
  $c->log->debug( 'Grid::rico_grid_data: slice contains ' . $slice->count . ' rows' )
    if $c->debug;
  
  my $i = 0;
  my $rows = [];
  foreach my $row ( $slice->all ) {
    push @{$rows}, [ $i++,
                     $row->pfamseq_acc, 
                     '<span class="alignment">' . $row->sequence . '</span>' ];
  }

  my $data = { 
    update_ui => 'true',
    offset    => $offset,
    rowCount  => $rs->count,
    rows      => $rows
  };
  
  $c->res->content_type( 'application/json' );
  $c->res->body( to_json( $data ) );
}

#-------------------------------------------------------------------------------

sub prototype_grid_data : Global {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Grid::prototype_grid_data: returning sequence information' )
    if $c->debug;
  
  my $offset    = $c->req->param('offset')    || 0;
  my $page_size = $c->req->param('page_size') || 100;
  
  my $rs = $c->model('PfamDB::Pfamseq')
             ->search( {}, {} );
             
  $c->log->debug( 'Grid::prototype_grid_data: found ' . $rs->count . ' rows' )
    if $c->debug;

  my $slice = $rs->slice( $offset, $offset + $page_size - 1 );
  $c->log->debug( 'Grid::prototype_grid_data: slice contains ' . $slice->count . ' rows' )
    if $c->debug;
  
  my $i = $offset;
  my $rows = [];
  foreach my $row ( $slice->all ) {
    push @{$rows}, { 
      index => $i++,
      acc   => $row->pfamseq_acc, 
      seq   => $row->sequence
    };
  }

  my $data = {
    offset    => $offset,
    totalrows => 2000,
    rowcount  => $page_size,
    rows      => $rows
  };
  
  $c->stash->{data} = $data;

  $c->res->content_type( 'text/xml' );  
  $c->stash->{template} = 'pages/prototype_slice.tt';
}

#-------------------------------------------------------------------------------

sub prototype_json_data : Global {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Grid::prototype_json_data: returning sequence information' )
    if $c->debug;
  
  my $offset    = $c->req->param('offset')    || 0;
  my $page_size = $c->req->param('page_size') || 100;
  
  my $rs = $c->model('PfamDB::Pfamseq')
             ->search( {}, {} );
             
  $c->log->debug( 'Grid::prototype_json_data: found ' . $rs->count . ' rows' )
    if $c->debug;

  my $slice = $rs->slice( $offset, $offset + $page_size - 1 );
  $c->log->debug( 'Grid::prototype_json_data: slice contains ' . $slice->count . ' rows' )
    if $c->debug;
  
  my $i = $offset;
  my $rows = [];
  foreach my $row ( $slice->all ) {
    push @{$rows}, { 
      index => $i++,
      acc   => $row->pfamseq_acc, 
      seq   => $row->sequence
    };
  }

  my $data = {
    offset    => $offset,
    totalrows => 2000,
    rowcount  => $page_size,
    rows      => $rows
  };
  
  $c->stash->{json} = $data;
}

#----------------------------------------

sub end : Private {
  my ( $this, $c ) = @_;

  if ( defined $c->stash->{json} ) {
    $c->log->debug( 'Grid::end: forwarding to the JSON view' )
      if $c->debug;

    $c->forward('PfamWeb::View::JSON');
  }
  else {
    $c->forward('PfamWeb::View::TT');
  }
}

#-------------------------------------------------------------------------------

sub yui_grid_data : Global {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Grid::yui_grid_data: returning sequence information' )
    if $c->debug;
  
  my $offset    = $c->req->param('offset')    || 0;
  my $page_size = $c->req->param('page_size') || 100;
  
  my $rs = $c->model('PfamDB::Pfamseq')
             ->search( {}, {} );
             
  $c->log->debug( 'Grid::yui_grid_data: found ' . $rs->count . ' rows' )
    if $c->debug;

  my $slice = $rs->slice( $offset, $offset + $page_size - 1 );
  $c->log->debug( 'Grid::yui_grid_data: slice contains ' . $slice->count . ' rows' )
    if $c->debug;
  
  my $i = $offset;
  my $rows = [];
  foreach my $row ( $slice->all ) {
    push @{$rows}, { 
      index => $i++,
      acc   => $row->pfamseq_acc, 
      seq   => $row->sequence
    };
  }

  my $data = {
    rs => {
      offset    => $offset,
      totalrows => 20000,
      rowcount  => $page_size,
      rows      => $rows
    }
  };
  
  $c->res->content_type( 'application/json' );
  $c->res->body( to_json( $data ) );
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
