#
# LigDomInt.pm
# pg6 20090902 WTSI
#
# $Id: LigDomInteraction.pm,v 1.2 2009-11-17 10:18:07 pg6 Exp $

=head1 Name

iPfameb::Controller::LigDomInt - Controller to build the interaction between a domain and a ligand.

=cut

package iPfamWeb::Controller::LigDomInteraction;

=head1 Description

This is the class which represents the specific Domain - Ligand interaction.

=cut

use base 'iPfamWeb::Controller::Section';

# set the name of the section it should read from the config 
__PACKAGE__->config( SECTION  =>  'ligdomint' );

#---------------------------------------------------------------------------------------------------------

=head1 Methods

=head2 begin : Private

This is a private method which parses the input parameters and validate it.

it accepts two parameters named:

=over 

=item from

It corresponds to either a pfam_accession or pfama_id or ligand_id, based on the param from the method change as either
Domain- Ligand interaction or Ligand - Domain interaction.

=item to

It depends on the former type.

=back

=cut

sub begin : Private {
  my ( $self, $c ) = @_;
  $c->log->debug( "enters inside the ligdominteraction ");
  # if we need to display the data in xml format then we woudl get the request in url 
  # as xml, but probably i would skip it for the time being as in most cases this class 
  # is called from family or ligand pages ;
  
  # now look for the input and if either of them misses then return stashing the error;
  
  if ( $c->req->param( "from" ) =~ /\W+/ or $c->req->param( "to" ) =~ /\W+/ ){
    $c->stash->{ errorMsg } = "Input contains invalid characters";
    return;
  }elsif ( not defined $c->req->param( "from" ) and not defined $c->req->param( "to" ) ){
    $c->stash->{ errorMsg } = "Invalid input or No input accession";
    return;  
  }# end of if
  
  # now get the accessions and parse it;
  
  my ( $from )  =   $c->req->param( 'from' ) =~ /(\w+)/;
  my ( $to )    =   $c->req->param( 'to' )  =~ /(\w+)/;
  
  # now type of the interaction is determined usign the input from. 
  if( $from =~ /^PF\d+/ ){
    
    $c->log->debug( "LigDomInt:begin: we got from: $from so it is domain-ligand interaction");
    $c->stash->{ from }->{ type } = 'Domain';
    $c->stash->{ from }->{ from } = $from;
    
  }else{
    
    # check for the input is pfama_id
    my $rs =  $c->model( 'iPfamDB::Pfama' )->search ( {
                'pfama_id'  => "$from"
              });
    my $pfam = $rs->first;
    
    unless ( defined $pfam ){
      $c->log->debug( "LigDomInt:begin: the input is not a pfama_id, so might be ligand_id");
      
      #specificall searching using 3 letter code because, family pages and ligand pages are created 
      # with three letter code, also the three letter code has number which conflicts with the 
      # ligand_id, to avoid those conflicts i am sticking to the three letter code search, 
      
      # now check for whether its a ligand id;
      my $lig_rs =  $c->model( 'iPfamDB::LigandChemistry')->search(
                      [
                        { three_letter_code =>  "$from" }                                              
                      ]
                    );
      if ( defined $lig_rs ){
        $c->log->debug( "LigDomInteraction::begin: input from is ligand so its ligand- domain interaction");
        $c->stash->{ from }->{ type } = 'Ligand';
        $c->stash->{ from }->{ from } = $from;
          
      } else{
        
        $c->log->debug( "LigDomInteraction::begin: invalid input: neither corresponds to pfama_id nor ligand_id" );
        $c->stash->{ errorMsg } = "Invalid Input: neither corresponds to pfam data nor ligand data";
        return;
        
      } # end of if defined $lig_rs;
                     
    } # end of unless
    else{
      $c->stash->{ from }->{ type } = 'Domain';
      $c->stash->{ from }->{ from } = $pfam->pfama_acc;
    }
     
  }# end of from check for pfam_acc
  
  # parse the input "to", now.
  if( $c->stash->{ from }->{ type } eq 'Domain' ){
    my $lig_rs =  $c->model( 'iPfamDB::LigandChemistry')->search(
                    [
                      { three_letter_code =>  "$to" }                                              
                    ]
                  );
    if ( defined $lig_rs ){
      $c->log->debug( "LigDomInteraction::begin: input to is ligand id or ligand three letter code");
      $c->stash->{ to }->{ to } = $to;
      $c->stash->{ to }->{ type } = 'Ligand';
        
    } else{
       
      $c->log->debug( "LigDomInteraction::begin: invalid input: from corresponds to pfam data and to should be an ligand_id" );
      $c->stash->{ errorMsg } = "Invalid Input: from corresponds to pfam data and to should be an ligand_id";
      return;
        
    } # end of if defined $lig_rs;
  } # end of from.type eq domain.
  elsif( $c->stash->{ from }->{ type } eq 'Ligand' ){
    
    if( $to =~ /^PF\d+/ ){
    
      $c->log->debug( "LigDomInt:begin: we got from: $to");
      $c->stash->{ to }->{ to } = $to;
      $c->stash->{ to }->{ type } = 'Domain';
    }else{
    
      # check for the input is pfama_id
      my $rs =  $c->model( 'iPfamDB::Pfama' )->search ( {
                  'pfama_id'  => "$to"
                });
      my $pfam = $rs->first;
      
      if( defined $pfam){
        $c->stash->{ to }->{ to } = $pfam->pfama_acc;
        $c->stash->{ to }->{ type } = 'Domain';
      }else{
        $c->log->debug( "LigDomInt::begin: invalid input, to should be a pfama_id or pfama_acc");
        $c->stash->{ errorMsg } = "LigDomINt::begin: from corresponds to Ligand and to has to be a pfam data" ;
        return;
      }
    }
  } # end of elsif from.type eq ligand.
  
}

#----------------------------------------------------------------------------------------------

sub ligdominteraction : Path( '/ligdominteraction' ){
  my ( $self, $c ) = @_;
  
  $c->forward( 'getSequences' );
  return;
} 

#----------------------------------------------------------------------------------------------
=head2 getSequences:Local

subroutine which gets the list of sequences which particpates in interaction with the ligand.

=cut

sub getSequences: Local {
  my ( $self, $c ) = @_;
  
  if( $c->stash->{ from }->{ type } eq 'Domain' ){
    $c->stash->{ domain } = $c->stash->{ from }->{ from };
    $c->stash->{ ligand } = $c->stash->{ to }->{ to };
  }else{
    $c->stash->{ domain } = $c->stash->{ to }->{ to };
    $c->stash->{ ligand } = $c->stash->{ from }->{ from };
  }
  
  # get the sequences contributing to the ligand domain interaction;
  my @sequences = $c->model( 'iPfamDB::Domain' )->search( 
    { 'pfam_acc' =>  $c->stash->{ domain } ,
      'three_letter_code' =>  $c->stash->{ ligand } 
    },
    {
      prefetch  =>[
        { 'dlis'  =>  
          { 'internal_ligand_id'  => "ligand_id" } }
      ]
    }
  );
  
  my @seqData;
  foreach my $seq ( @sequences ){
    push @seqData, {
      'domain'       =>  $seq->get_column( 'pfam_acc' ),
      'protein'      =>  $seq->get_column( 'protein_accession' ),
      'start'        =>  $seq->get_column( 'start' ),
      'end'          =>  $seq->get_column( 'end' ),
      'atom_start'   =>  $seq->dlis->first->internal_ligand_id->get_column( 'atom_start' ),
      'atom_end'     =>  $seq->dlis->first->internal_ligand_id->get_column( 'atom_end' ),
      'ligcode'      =>  $seq->dlis->first->internal_ligand_id->ligand_id->get_column( 'three_letter_code' ),
      'ligand_name'  =>  $seq->dlis->first->internal_ligand_id->ligand_id->get_column( 'name' )
    };
    
  } # end of foreach
  $c->stash->{ seqData } = \@seqData;
  $c->log->debug( "LigDomInt:get_sequences: now dumping the sequences contributing the interaction".scalar( @sequences) );
  foreach my $seq ( @seqData ){
    $c->log->debug( "LigDomInt:get_sequences: ".$seq->{ 'domain' }.'|'.
      $seq->{ 'protein'}.'|'.$seq->{ 'start'}.'|'. $seq->{ 'end'}.'|'.
      $seq->{ 'atom_start'}.'|'.$seq->{ 'atom_end'}.'|'.$seq->{ 'ligand_name'}.'|'.$seq->{ 'ligcode'}
    );
  }
  
} 


1;