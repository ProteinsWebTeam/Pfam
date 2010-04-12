#########
# Author        : pg6
# Maintainer    : $Id: Rfam.pm,v 1.3 2010-04-09 09:47:32 pg6 Exp $
# Created       : 2008-11-25
# Last Modified : $Date: 2010-04-09 09:47:32 $
# Version       : $Revision: 1.3 $
# Builds simple DAS features from the Rfam database
#

=head1 NAME

Rfam

=head1 DESCRIPTION

Rfam source adaptor that is specific to the Rfam MySQL database.  This have been heavily modified 
from the first release so that it actually error checks and conforms to bioSapiens ontology.  This 
adaptor has I<features> and I<types> capabilities.  This source adaptor is expected
to be used as part of ProServer, so no example usages will be given.

=head1 AUTHORS

Prasad Gunasekaran (pg6@sanger.ac.uk) & Rob Finn (rdf@sanger.ac.uk) & Paul Gardner (pg5@sanger.ac.uk)

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Prasad Gunasekaran (pg6@sanger.ac.uk) 

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

package Bio::Das::ProServer::SourceAdaptor::Rfam;
use strict;
use Bio::Das::ProServer::SourceAdaptor;
use Data::Dumper;
use base 'Bio::Das::ProServer::SourceAdaptor';
use Data::Dump qw( dump );

# RFAM STORES ONTOLOGIES IN THEIR DATABASE NOW;
our $METHOD      = 'Rfam';
our $METHOD_CVID = 'ECO:00000043';

#-------------------------------------------------------------------------------------------------------------------


sub init {
    my $self = shift;
    $self->{'capabilities'} = {
	'features' => '1.0',
	'types'	   => '1.0',
	};
	
}

#-------------------------------------------------------------------------------------------------------------------
#subroutine to fetch all features for rfamseq.

=head2  build_features

 Title		:	build_features
 Usage		:	$sa->build_features;
 Function	:	Returns all the features for valid segment and error message for invalid segment
 Example	:	$sa->build_features($opts);
 Returns	:	Array of Hashes
 Args		:	String

=cut

sub build_features {
    my ($self, $opts) = @_;
 	my $segment = $opts->{segment};
 	my $start = $opts->{start};		#currently start and end is not given by me ; used incase users input
 	my $end = $opts->{end};
 	my $qsegment = $self->transport->dbh->quote($segment);
 	my $test          = $self->transport->query(qq(SELECT rfamseq_acc
                                                    FROM   rfamseq
                                                    WHERE  rfamseq_acc = $qsegment));    
   # Test for valid segment 
    if(!$test->[0]){
    	my @features;
  		push @features, {
  	    	'label'  => "Invalid segment",
  	    	'start'  => "0",
  	    	'end'    => "0",	
  	    	'note'   => "Sequence cannot be retrieved for invalid segment"
  		};
  		return @features;
    }
    
    my $ref = $self->_get_Rfam_regions($start,$end,$qsegment);
    
    my $VERSION  =  "Infernal-1.0";
    my @features = ();
  	my $href = "http://rfam.sanger.ac.uk/";
    
  	for my $row (@{$ref}) {
      my $ptype = $row->{'type'};
      
  		my $id = $row->{'rfam_id'};
  		my $feature = $self->_feat_params($id,$row,$href."family?entry=".$row->{'rfam_acc'});
  		$$feature{'score'} = $row->{'bits_score'};
  		$$feature{'note'} = "$VERSION";
  		push (@features,$feature);
    }
   #print STDERR "the featuers are ".dump( \@features )."\n";
   return @features;   
}

#-------------------------------------------------------------------------------------------------------------------
#subroutine to fetch all types for rfamseq.


=head2  build_types

 Title		:	build_types
 Usage		:	$sa->build_types
 Function	:	Returns all the types for valid segment; error message for invalid segment and all types if called without segment
 Example	:	$sa->build_features($opts)	
 Returns	:	Array of Hashes
 Args		:	optional String

=cut

sub build_types{
  my ($self,$opts) = @_;
  # checks for segment  
	my @types;			
	if($opts){
    	my $segment       = $opts->{'segment'};
    	my $start         = $opts->{'start'};
    	my $end           = $opts->{'end'};
   		my $dsn           = $self->{'dsn'}; 
		  my $qsegment      = $self->transport->dbh->quote($segment);
    	my $test          = $self->transport->query(qq(SELECT rfamseq_acc FROM  rfamseq WHERE  rfamseq_acc = $qsegment));
		
		# checks valid segment or not
		if(!$test->[0]){
   		my @types;
			push @types, {
	    		'type'  => "Invalid segment: No type found",
	    		'method'=> 'No Method defined',
	    		'category' => 'No Evidence code'
			};
			return @types;
  	}else{
      # rfam regions 
			my $ref_A = $self->_get_Rfam_regions($start,$end,$qsegment);
			
			# add the db_id and the db_link to create the SO id;
			# create a hash to build the required values;
			my $typesFound = {};
			foreach my $rfam (@{$ref_A} ){
			  my $so_id = $rfam->{ db_id } . ':' . $rfam->{ db_link };

        unless( exists $typesFound->{ $so_id } ){
			    $typesFound-> { $so_id } = {
			      'type_cvid'  => $so_id,
			      'type'       => $rfam->{ comment },
			      'method'     => $METHOD,
			      'count'      => 1        
			    };  
			   
			  } # end of unless; 
			  else{
			    $typesFound->{ $so_id }->{ count }++;  
			  }
			  
			}
			
      foreach my $types ( keys %$typesFound ){
        push ( @types, $typesFound->{ $types } );
      }
      			
			# No types for valid segment
			if(!@types){
        		push @types, {
	    			'type'  	=> "No annotation",
	    			'method'   	=> "No method",
					  'category'	=> "No Evidence Code"
				    };					
      }
    }
	}else{    	
		
		# now load all the types, so query from the datbaase;
		my $typesQuery = $self->transport->query( qq( select db_id, db_link, comment, count( *) AS total from rfam_database_links where db_id = 'SO' group by db_link; ) );
		
		
		foreach my $type ( @{ $typesQuery } ){
		  #print "the dump fo the type is ".dump( $type );
		  push @types, {
		    'type_cvid' =>  $type->{ db_id } .':'. $type->{ db_link },
		    'type'      =>  $type->{ comment },
		    'count'     =>  $type->{ total } 
		  };
		}
      	
  }
    
	return @types;		
}

#-------------------------------------------------------------------------------------------------------------------
#subroutine returns Rfam features as a hash-ref for segment

=head2  _get_Rfam_regions

 Title		:	_get_RfamA_regions
 Usage		:	$Rfam_obj->_get_RfamA_regions;
 Function	:	Returns all the Rfam regions like Family, Domain, Repeat and Motif for input 
                segment
 Example	:	$Rfam_obj->_get_Rfam_regions($start,$end,$segment);
 Returns	:	Reference containing Rfam-A features as hash.
 Args		:	String 

=cut

sub _get_Rfam_regions {
	my($self,$start,$end,$qsegment) = @_;
	my $qbounds          = qq(AND seq_start <= '$end' AND seq_end >= '$start') if($start && $end);
    
    # Select the Rfam regions
    my $query         = qq(select rfam_id,rfam_acc, rfam.description, seq_start , seq_end , bits_score , rfam.type 
    					   , db_id, rfam_database_links.comment, db_link  
    					   from rfam, rfam_reg_full, rfamseq, rfam_database_links  
    					   where rfam.auto_rfam = rfam_reg_full.auto_rfam 
    					   and rfam_reg_full.auto_rfamseq = rfamseq.auto_rfamseq 
    					   and rfam.auto_rfam = rfam_database_links.auto_rfam 
    					   and rfam_database_links.db_id = 'SO' 
    					   and rfamseq_acc = $qsegment $qbounds);
     
    my $ref    = $self->transport->query($query);
    return $ref;
    
}

#subroutine returns feature information

=head2 _feat_params

 Title		:	_feat_params
 Usage		:	$pfam_obj->_feat_params
 Function	:	Returns ontology and various feature information like start,end,link,linktxt
 Example	:	$pfam_obj->_feat_params($type,$id,$start,$end,$link,$linktxt);
 Returns	:	Array of Hash
 Args		:	String 

=cut

sub _feat_params {
	#my ($self,$param,$id,$feat,$link) = @_;	
	my ($self,$id,$feat,$link) = @_;
	
	next unless($feat->{seq_start} =~ /^\d+/);
	next unless($feat->{seq_end} =~	/^\d+/);
	
	# to make the feature ids unique, i have added the start and end coordinates with the id;
	my $feature = {
		'id' 			=> 	$id.':'.$feat->{seq_start}.'-'.$feat->{seq_end},
		#'label'			=>	$id.":".$start."-".$end,
		'label'			=>	$feat->{rfam_acc}.": ".$feat->{rfam_id},
		'start'			=> 	$feat->{seq_start},
		'end'			  =>	$feat->{seq_end},
		'link'			=> 	$link,
		'linktxt'		=>	$id,
    'type'          => $feat->{ comment },
    'typetxt'       => $feat->{ comment },
    'type_cvid'     => $feat->{ db_id }.':'.$feat->{ db_link },
    'method'        => $METHOD,
    'method_label'  => $METHOD,
    'method_cvid'   => $METHOD_CVID
	};
	return $feature;
}


1;