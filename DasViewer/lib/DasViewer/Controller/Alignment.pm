package DasViewer::Controller::Alignment;

use strict;
use warnings;
use parent 'Catalyst::Controller';
use Data::Dump qw(dump);
use JSON;

# set the name of the section
__PACKAGE__->config( SECTION => 'alignment_sources' );

=head1 NAME

DasViewer::Controller::Alignment - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=head2 view : Local

Method which gets the total alignment rows , for setting that in the page for the use of LiveGrid;

=cut

sub view : Local {
  my( $self, $c ) = @_;
  
  # this is the initial request so we have only acc and dsn_name;
  # create a hash_ref for the input and pass it to the datasource module;
  $c->log->debug( 'the dump of the object is '.dump( $self ) );
  my $args = {
    input_params  =>{
      acc      =>  $c->req->param('accession'),
	    dsn_name =>  $c->req->param('dsn_name'),  
    }
  };
  
  # get the datasource from the model;
  my $ds = $c->model('DasSource' )->getDataSource( $args );
  $c->log->debug('ALignment::View:we got the datasource object' );
  # get the alignment size;
  my $size = $ds->get_alignment_size();
  if( defined $size and $size != 0 ){
    $args->{ input_params }->{ size } = $size;  
  }else{
    $args->{ input_params }->{ size } = 0;  
    $c->stash->{ errorMsg } = "Invalid accession or No alignment found for ".$c->req->param('accession')." provided.";
  }
  
  $c->log->debug( "Alignment::View:the sizw returned is ".$args->{ input_params }->{ size } );
  $c->stash->{ data } = $args->{ input_params }; 
  $c->stash->{template} = 'components/alignment.tt';
  
}

#------------------------------------------------------------------------------

=head2 alignment : Local

Method which gets the alignment for the offset defined;

=cut

sub alignment : Local {
  my ( $self, $c ) = @_;
  
  # get the offset, pagesize and max_rows from the  request;
  my $offset    = $c->req->param('offset') || 1;
  my $page_size = $c->req->param('page_size') || 100;
  my $max_rows  = $c->req->param('max_rows');
  
  # using the values, get the row range to fetch the alignments;
  my $end = $offset + $page_size - 1;
  $c->log->debug("Alignment:alignment: rows to be fetched is |$offset|$page_size|$end|");
  if( $end > $max_rows ){
    $end = $max_rows ;
  }
  
  # set the rowrange;
  my $row_range = $offset .'-'. $end;
  $c->log->debug("Alignment:alignment: fetching the rows in the range |$row_range|");
  # create a hash_ref for generating the datasource object;
  my $args = {
    input_params  =>{
      acc      =>  $c->req->param('accession'),
	    dsn_name =>  $c->req->param('dsn_name'),
	    rows     =>  $row_range,  
    }
  };
  
  # get the datasource from the model;
  my $ds = $c->model('DasSource' )->getDataSource( $args );
  
  my ( $storealignment, $alignment )= $ds->get_alignment();
  
#  my $test = [ [1,2],[3,4],[5,6],[3,23],[23,54],[2,34],[4,65] ];
#  
#  $c->log->debug( 'the alignmnet is '.dump( $test ) );
  my $data = {
    offset    => $offset,
    rowcount  => $page_size,
    rows      => $alignment
  };
  
#  $c->stash->{json} = $data;
#  $c->stash->{ storeAlignments } = $storealignment;
  my $json = { 
    data  => $data,
    storeAlignments =>  $storealignment
  };
  
  $c->stash->{json} = to_json( $json );
#  $c->stash->{json}->{ data } = to_json( $data );
#  $c->stash->{json}->{ storeAlignments } = to_json( $storealignment );
  
  $c->res->content_type( 'application/json');
  $c->res->body( $c->stash->{ json } );  
#  #$c->log->debug( 'ALignment:alignment: the dump of the alignment is '.dump( $data ) );
#  $c->forward('DasViewer::View::JSON');
  
}

sub end : ActionClass( 'RenderView' ) {}

=head1 AUTHOR

Prasad Gunasekaran,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut


1;
