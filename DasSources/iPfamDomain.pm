#########
# Author: rdf
# Maintainer: rdf
# Created: 2008-02-21
# Last Modified: $Date: 2009-09-22 08:59:15 $
# Builds simple DAS features from the domain:domain interactions for the ipfam database
# $Source: /nfs/team71/pfam/pg6/2009/CVS_Repository/Bio-Das-ProServer/lib/Bio/Das/ProServer/SourceAdaptor/iPfamDomain.pm,v $ - $Revision: 1.1.1.1 $

package Bio::Das::ProServer::SourceAdaptor::iPfamDomain;

use strict;
use base qw(Bio::Das::ProServer::SourceAdaptor);
use Data::Dump qw(dump);
use Data::Dump qw( dump );

my $DEBUG = 1;

sub init {
   my $self = shift;
   $self->{'capabilities'}{'interaction'} = '1.0';
}

#subroutine for building interaction

sub build_interaction {
   my ($self,$opts) = @_;   
   print STDERR  dump($opts)."\n";
   
#   #print error message when there is no input; print now but later change it as a subroutine
#   unless (defined @{$opts->{'interactors'}}){
#      $self->_build_interactor;
#      return;  
#   }
   my %pf_int;    #replace with pfam_interactor
   foreach(@{$opts->{'interactors'}}){
      next if($_ =~ /\W+/);
      $pf_int{ uc($_) } = $self->_get_Pfam($_);
   }
   
   print "the dump of the pf_int is ".dump( \%pf_int )."\n\n";
   
   my ($interactionRef, $interactorsRef) = $self->_build_interactor(\%pf_int);    
   
   #print STDERR dump($interactionRef), dump($interactorsRef);
   return {
        'interactors'  => [values %$interactorsRef],
        'interactions' => $interactionRef,
   };
}

#subroutine top get interaction data from database for input ids,
sub _get_Pfam {
    my ($self,$query_family) = @_;
    my $query;
	my $qfamily = $self->transport->dbh->quote($query_family);
		
	if($query_family =~ /PF\d\d\d\d\d/) {   #It's a pfamA_acc
	  
      $query  =   qq(select a.pfamA_id as pfamA_id_a, a.pfamA_acc as pfamA_acc_a , a.description as description_a, b.pfamA_id as pfamA_id_b, b.pfamA_acc as pfamA_acc_b, b.description as description_b 
                      from pfamA_interactions i, pfamA a, pfamA b 
                      where i.auto_pfamA_A=a.auto_pfamA 
                      and i.auto_pfamA_B=b.auto_pfamA 
                      and a.pfamA_acc=$qfamily);
    }
    else {  #Assume its a pfam_id
          
      $query  =   qq(select a.pfamA_id as pfamA_id_a, a.pfamA_acc as pfamA_acc_a , a.description as description_a, b.pfamA_id as pfamA_id_b, b.pfamA_acc as pfamA_acc_b, b.description as description_b 
                      from pfamA_interactions i, pfamA a, pfamA b 
                      where i.auto_pfamA_A=a.auto_pfamA 
                      and i.auto_pfamA_B=b.auto_pfamA 
                      and a.pfamA_id=$qfamily);
    }
   my $array_ref  = $self->transport->query($query);
   print "the array ref is ".dump( $array_ref )."\n\n";
   return [{'pfamA_id_a' => "Invalid ID $query_family"}] unless ($array_ref->[0]);
   return $array_ref;
}

#subroutin for generating data for xml

sub _build_interactor {
   #print STDERR "The total values passed to sub is ",scalar(@_), " the values is ",dump(@_);
   if(scalar(@_) == 1){
      my ($self) = @_;
      my $interactors = {
         'NO_ID' => {
            'id'           => "NO_ID",
            'label'        => "No Label",
            'dbSourceCvId' => "",
            'dbSource'     => "No Source",
            'dbCoordSys'   => "No Coords",
            'dbVersion'    => "No Version",
            'dbAccession'  => "No Accession",
            'details'      => [
               {
                  'property' => "description",
                  'details'  => "No Description"
               }
            ]
         }
      };
      return { 'interactors' => [ values %$interactors ] };
   }else{
      my ($self,$all_interactors) = @_;
      #print STDERR dump($all_interactors);
      my (%interactors,%chk_interaction);
      my @interactions;   
      foreach  my $key (keys %$all_interactors){
         my $value = $all_interactors->{$key};
         #print STDERR "value type is = $value\n\n";
         foreach my $row(@$value){
            my ($qname,$Source,$coords,$version,$accession,$label,$id_details);
	        if(defined $row->{pfamA_acc_a}){
	            $qname =  $row->{pfamA_acc_a};
                $qname .= "pfamA";
                $label = $row->{pfamA_id_a};
                $Source = "pfamA";
                $coords = "Pfam,Protein Sequence";
                $version = "22.0";
                $accession  = $row->{pfamA_acc_a};
                $id_details = $row->{description_a};                
	        }else{
	           $qname = $key;
	        }	        
	        unless($interactors{$qname}){
         	  	$interactors{$qname}{'id'} = $qname ;
               	$interactors{$qname}{'label'} = $label || "Invalid label" ;
               	$interactors{$qname}{'dbSourceCvId'} = "";
               	$interactors{$qname}{'dbSource'} = $Source || "Invalid Source";
               	$interactors{$qname}{'dbCoordSys'}=$coords || "Invalid Coords";
               	$interactors{$qname}{'dbVersion'} = $version || "Invalid Version";
               	$interactors{$qname}{'dbAccession'} = $accession || "Invalid Accession";
               	my %details;
               	$details{'property'} = "description";
               	$details{'details'} = $id_details || "Invalid_description";
               	push (@{$interactors{$qname}{'details'}}, \%details);
         	  }
             #checking to find only common interactors for all query;
             my $id = uc($row->{'pfamA_id_b'});
             my $acc = uc($row->{'pfamA_acc_b'});
             if (scalar(keys %$all_interactors)>1){
                next unless(exists($all_interactors->{$id}) or exists($all_interactors->{$acc}));
             }
             my $pname = $row->{pfamA_acc_b};
             $pname .= "pfamA";
       	     unless($interactors{$pname}){
                  	$interactors{$pname}{'id'} = $pname;
                  	$interactors{$pname}{'label'} = $row->{pfamA_id_b} ;
                  	$interactors{$pname}{'dbSourceCvId'} = "";
                  	$interactors{$pname}{'dbSource'} = "pfamA";
                  	$interactors{$pname}{'dbCoordSys'}="Pfam,Protein Sequence";
                  	$interactors{$pname}{'dbVersion'} = "22.0";
                  	$interactors{$pname}{'dbAccession'} = $row->{pfamA_acc_b};
                  	my %details;
                  	$details{'property'} = "description";
                  	$details{'details'} = $row->{description_b};
                  	push (@{$interactors{$pname}{'details'}}, \%details);
            	  }
          	  my %interaction;
         	  $interaction{'name'} = $row->{pfamA_acc_a}."-".$row->{pfamA_acc_b};
         	  $interaction{'dbSource'} = 'iPfam';
              $interaction{'dbVersion'} = '22.0';
              my $chk1 = $pname.$qname;
              my $chk2 = $qname.$pname;
              unless(exists($chk_interaction{$chk1}) or exists($chk_interaction{$chk2})){
                push (@{$interaction{'participants'}}, {'id' => $qname}, { 'id' => $pname} );
                push @interactions, \%interaction;
                $chk_interaction{$chk1} +=1;
                $chk_interaction{$chk2} +=1;
              }                
         }         
      } 
      #print STDERR dump(%interactors);
      return (\@interactions, \%interactors); 
   }   
}

1;
