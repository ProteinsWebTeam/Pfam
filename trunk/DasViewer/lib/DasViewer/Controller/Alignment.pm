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

# sub view : Local {
#   my( $self, $c ) = @_;
#   
#   # this is the initial request so we have only acc and dsn_name;
#   # create a hash_ref for the input and pass it to the datasource module;
#   $c->log->debug( 'the dump of the object is '.dump( $self ) )
#     if $c->debug;
#   my $args = {
#     input_params  =>{
#       acc      =>  $c->req->param('accession'),
# 	    dsn_name =>  $c->req->param('dsn_name'),  
#     }
#   };
#   
#   # get the datasource from the model;
#   my $ds = $c->model('DasSource' )->getDataSource( $args );
#   $c->log->debug('ALignment::View:we got the datasource object' )
#     if $c->debug;
#   # get the alignment size;
#   my $size = $ds->get_alignment_size();
#   if( defined $size and $size != 0 ){
#     $args->{ input_params }->{ size } = $size;  
#   }else{
#     $args->{ input_params }->{ size } = 0;  
#     $c->stash->{ errorMsg } = "Invalid accession or No alignment found for ".$c->req->param('accession')." provided.";
#   }
#   
#   $c->log->debug( "Alignment::View:the size returned is ".$args->{ input_params }->{ size } )
#     if $c->debug;
#   $c->stash->{ data } = $args->{ input_params }; 
#   $c->stash->{template} = 'components/alignment.tt';
#   
# }


#---------------------------------------------------------------------------------------------

=head2 getAlignmentViewer : Path 

=cut

sub getAlignmentViewer : Path( '/javascripts/AlignmentViewer.js'){
  my( $self, $c ) = @_;
  
  my $jsFile = $c->view( 'TT')->render( $c, 'components/AlignmentViewer.tt' );
  
  $c->res->content_type( 'text/javascript');
  $c->res->body( $jsFile );
}

#---------------------------------------------------------------------------------------------

=head2 alignment : Local

Method which gets the alignment for the offset defined;

=cut

sub alignment : Local {
  my ( $self, $c ) = @_;
  
  # detaint the damn parameters...
  foreach ( qw( offset page_size max_rows ) ) {
    unless ( $c->req->param($_) =~ m/^\d+/ ) {
      $c->res->status(400); # Bad request
      $c->res->body( "Parameter '$_' was not valid" );
      return;
    }
  }
  unless ( defined $c->req->param('accession') and
           $c->req->param('accession') =~ m/^[A-Za-z0-9]+$/ ) {
    $c->res->status(400); # Bad request
    $c->res->body( 'Not a valid accession' );
    return;
  }
  unless ( defined $c->req->param('dsn_name') and
           $c->req->param('dsn_name') =~ m/^\w+$/ ) {
    $c->res->status(400); # Bad request
    $c->res->body( 'Not a valid DSN name' );
    return;
  }

  # get the offset, pagesize and max_rows from the  request;
  my $offset    = $c->req->param('offset') || 1;
  my $page_size = $c->req->param('page_size') || 100;
  my $max_rows  = $c->req->param('max_rows');
  
  # using the values, get the row range to fetch the alignments;
  my $end = $offset + $page_size - 1;
  $c->log->debug("Alignment:alignment: rows to be fetched is |$offset|$page_size|$end|")
    if $c->debug;
  if( $end > $max_rows ){
    $end = $max_rows ;
  }
  
  # set the rowrange;
  my $row_range = $offset .'-'. $end;
  $c->log->debug("Alignment:alignment: fetching the rows in the range |$row_range|")
    if $c->debug;
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
  
  my $alignments;
  
  if( ref( \$storealignment ) ne 'SCALAR' ){
    
    my $data = {
      offset    => $offset,
      rowcount  => $page_size,
      rows      => $alignment
    };
  
    my $json = { 
               data  => $data,
               storeAlignments =>  $storealignment
               };
    
    $alignments = to_json( $json );
  } else{
    $alignments = "'$storealignment'";
  }  
  my $string = "var alignments = $alignments;";
  
  $c->res->content_type( 'application/json');
  $c->res->body( $string );
}

#------------------------------------------------------------------------------

sub end : ActionClass( 'RenderView' ) {}

=head1 AUTHOR

Prasad Gunasekaran,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut


1;
