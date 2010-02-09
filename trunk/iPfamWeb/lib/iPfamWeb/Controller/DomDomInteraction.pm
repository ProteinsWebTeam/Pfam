
# DomDomInt.pm
# pg6 20090901 WTSI
#
# $Id: DomDomInteraction.pm,v 1.2 2009-11-09 17:01:03 pg6 Exp $

=head1 Name

iPfamWeb::Controller::DomDomInt - controller to build the interaction between the families.

=cut

package iPfamWeb::Controller::DomDomInteraction;

=head1 Description 

This is the class which represents a specific domain-domain interaction.

=cut

use base 'iPfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION  =>  'domdomint' );

#----------------------------------------------------------------------------------------------

=head1 Methods

=head2 begin : Private 

This is the action responsible for parsing the input accessions and validate them.

=cut

sub begin: Private {
  my ( $self, $c ) = @_;
  
  # if we need to display the data in xml format then we woudl get the request in url 
  # as xml, but probably i would skip it for the time being as in most cases this class 
  # is called from family or ligand pages ;
  
  # if either of domains are not given, return stashing the error message;
  unless ( defined $c->req->param( "from" ) and defined $c->req->param( "to" ) ){
    $c->stash->{ errorMsg } = "Invalid input or No input accession";
    return;  
  }elsif ( $c->req->param( "from" ) =~ /\W+/ or $c->req->param( "to" ) =~ /\W+/ ){
    $c->stash->{ errorMsg } = "Input contains invalid characters";
    return;
  }
  
  # now get the accessions and parse it;
  my ( $domIntFrom )  = $c->req->param( "from" ) =~ /(\w+)/;
  my ( $domIntTo )    = $c->req->param( "to" ) =~ /(\w+)/;
  
#  if( $domIntFrom =~ /^PF\d+/ ){
#  
#    $c->log->debug( "DomDomInt::begin: we got pfam accession as input( from : $domIntFrom )");
#    $c->stash->{ from } = $domIntFrom;  
#  
#  }else{
  
    $c->log->debug( "DomDomInt::begin: we might have got pfamA_id as input ( from: $domIntFrom )" );
    # now querying the pfama table to look for corresponding pfama_acc for the input pfama_id;
    my $rs =  $c->model( 'iPfamDB::Pfama')->search( [ 
                { 'pfama_id'  =>  "$domIntFrom" },
                { 'pfama_acc' =>  "$domIntFrom" }
                ] );
    my $pfamFrom = $rs->first;
    
    # now look for whether we got a valid pfam accession, if not then return with error message;
    unless( defined $pfamFrom ){
      $c->log->debug( "DomDomInt:begin: user input $domIntFrom neither corresponds to pfamA_id nor pfamA_acc");
      $c->stash->{ errorMsg } = "Input value: $domIntFrom neither corresponds to pfamA_id nor pfamA_acc";
      return;
      
    } # end of unless   
    $c->stash->{ from } = $pfamFrom;
    
#  } # end of else
  
  # now parse the other input "to"
#  if( $domIntTo =~ /^PF\d+/ ){
#  
#    $c->log->debug( "DomDomInt::begin: we got pfam accession as input( to : $domIntTo )");
#    $c->stash->{ to } = $domIntTo;  
#  
#  }else{
#  
    $c->log->debug( "DomDomInt::begin: we might have got pfamA_id as input ( to: $domIntTo )" );
    # now querying the pfama table to look for corresponding pfama_acc for the input pfama_id;
    my $rs1 =  $c->model( 'iPfamDB::Pfama')->search( [ 
                { 'pfama_id'  =>  "$domIntTo" },
                { 'pfama_acc' =>  "$domIntTo" }
                ] );
    my $pfamTo = $rs1->first;
    
    # now look for whether we got a valid pfam accession, if not then return with error message;
    unless( defined $pfamTo ){
      $c->log->debug( "DomDomInt:begin: user input $domIntTo neither corresponds to pfamA_id nor pfamA_acc");
      $c->stash->{ errorMsg } = "Input value: $domIntTo neither corresponds to pfamA_id nor pfamA_acc";
      return;
      
    } # end of unless   
    $c->stash->{ to } = $pfamTo;
    
#  } # end of else
  
}


#----------------------------------------------------------------------------------------------

sub domdominteraction : Path('/domdominteraction'){
  
  my ( $self, $c ) = @_;
  $c->log->debug( "Domain::domain- it reaches inside the child class domain ");
  
  $c->forward( 'getSequences' );
  return;
}


#----------------------------------------------------------------------------------------------
sub getSequences : Local {
  my ( $self, $c ) = @_;
  
  $c->log->debug( "DomDomInt:getSequences: Inside the action getSequences" );
  my $from = $c->stash->{ from };
  my $to = $c->stash->{ to };
  
  $c->log->debug( "the dump is". $from->pfama_acc .'|'. $to->pfama_acc );
  
  my @sequences = $c->model( 'iPfamDB::Ddi' )->search( 
    { 'region_id_a.pfam_acc'  => $from->pfama_acc ,
      'region_id_b.pfam_acc'  => $to->pfama_acc
    },
    {
      prefetch  =>  [
        'region_id_a', 'region_id_b'
      ]      
    }
  );
  
  my @seqData;
  foreach my $seq ( @sequences ){
    push @seqData, {
      'domainA'  =>  $seq->region_id_a->get_column( 'pfam_acc' ),
      'domainB'  =>  $seq->region_id_b->get_column( 'pfam_acc' ),
      'proteinA' =>  $seq->region_id_a->get_column( 'protein_accession' ),
      'proteinB' =>  $seq->region_id_b->get_column( 'protein_accession' ),
      'startA'   =>  $seq->region_id_a->get_column( 'start' ),
      'endA'     =>  $seq->region_id_a->get_column( 'end' ),
      'startB'   =>  $seq->region_id_b->get_column( 'start' ),
      'endB'     =>  $seq->region_id_b->get_column( 'end' )
    };
    
  } # end of foreach
  $c->stash->{ seqData } = \@seqData;
  
  # debuggin to check whether the data is there or not
  foreach my $seq ( @seqData ){
    $c->log->debug( "DomDomInt:get_sequences: ".$seq->{ 'domainA' }.'|'.
      $seq->{ 'proteinA'}.'|'.$seq->{ 'startA'}.'|'. $seq->{ 'endA'}.'|'.
      $seq->{ 'domainB'}.'|'.$seq->{ 'proteinB'}.'|'.$seq->{ 'startB'}.'|'.$seq->{ 'endB'}
    );
  }
  
}


1;