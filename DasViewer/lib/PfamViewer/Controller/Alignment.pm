package PfamViewer::Controller::Alignment;

use strict;
use warnings;
use parent 'Catalyst::Controller';
use Data::Dump qw(dump);
use JSON;

# set the name of the section
__PACKAGE__->config( SECTION => 'alignment_sources' );

=head1 NAME

PfamViewer::Controller::Alignment - Catalyst Controller

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
    $c->stash->{ errorMsg } = "Invalid accession: ".$c->req->param('accession')." provided.";
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
  
  my $alignment = $ds->get_alignment();
  
  
  my $data = {
    offset    => $offset,
    rowcount  => $page_size,
    rows      => $alignment 
  };
  
  $c->stash->{json} = $data;
  #$c->log->debug( 'ALignment:alignment: the dump of the alignment is '.dump( $data ) );
  $c->forward('PfamViewer::View::JSON');
  
}


=head1 AUTHOR

Prasad Gunasekaran,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut


1;
