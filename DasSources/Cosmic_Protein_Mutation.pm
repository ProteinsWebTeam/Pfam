#########
# Author        : pg6, rdf
# Maintainer    : $Id: Cosmic_Protein_Mutation.pm,v 1.2 2009-09-22 14:46:41 pg6 Exp $
# Created       : 2008-11-12
# Last Modified : $Date: 2009-09-22 14:46:41 $
# Version       : $Revision: 1.2 $
# Builds simple DAS features from the pfam database
#

=head1 NAME

Cosmic_Protein_Mutation

=head1 DESCRIPTION

Cosmic_Protein_Mutation source adaptor that is specific to the cosmic_uniprot MySQL database.  This is the 
first release and conforms to bioSapiens ontology.  This 
adaptor has I<features>, I<sequences> and I<types>, I<alignment>, capabilities.  This source adaptor is expected
to be used as part of ProServer, so no example usages will be given.

=head1 AUTHORS

Prasad Gunasekaran (pg6@sanger.ac.uk) & Rob Finn (rdf@sanger.ac.uk) 

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors:  Prasad Gunasekaran (pg6@sanger.ac.uk), Rob Finn (rdf@sanger.ac.uk)

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

package Bio::Das::ProServer::SourceAdaptor::Cosmic_Protein_Mutation;
use strict;
use Bio::Das::ProServer::SourceAdaptor;
use Data::Dumper;
use base 'Bio::Das::ProServer::SourceAdaptor';

our $type_ref = {
	'Unknown' => {
				'type'			=> 'SO:1000090',
				'typetxt'		=> 'mutation_causing_uncharacterised_change_of_translational_product',
				'method_cvid' => 'ECO:0000006',
				'method' 		=> 'Cosmic',
				'method_label'	=> 'Cosmic'
				},
	'Gene fusion' => {
					'type'			=> 'SO:1000181',
					'typetxt'		=> 'mutation_causing_gene_fusion',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Substitution - Nonsense' => {
					'type'			=> 'SO:1000062',
					'typetxt'		=> 'mutation_causing_nonsense_codon_change_in_transcript',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Substitution - Missense' => {
					'type'			=> 'SO:1000059',
					'typetxt'		=> 'mutation_causing_missense_codon_change_in_transcript',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Substitution - coding silent' => {
					'type'			=> 'SO:1000057',
					'typetxt'		=> 'mutation_causing_synonymous_codon_change_in_transcript',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Insertion - In frame' => {
					'type'			=> 'SO:1000096',
					'typetxt'		=> 'mutation_causing_amino_acid_insertion',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Insertion - Frameshift' => {
					'type'			=> 'SO:1000065',
					'typetxt'		=> 'frameshift_mutation',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Deletion - In frame' => {
					'type'			=> 'SO:1000097',
					'typetxt'		=> 'mutation_causing_amino_acid_deletion',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Deletion - Frameshift' => {
					'type'			=> 'SO:1000065',
					'typetxt'		=> 'frameshift_mutation',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Substitution - intronic' => {
					'type'			=> 'SO:1000071',
					'typetxt'		=> 'mutation_affecting_splicing',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Complex - compound substitution' => {
					'type'			=> 'SO:1000092',
					'typetxt'		=> 'mutation_causing_complex_change_of_translational_product',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Insertion - intronic' => {
					'type'			=> 'SO:1000071',
					'typetxt'		=> 'mutation_affecting_splicing',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Deletion - intronic' => {
					'type'			=> 'SO:1000071',
					'typetxt'		=> 'mutation_affecting_splicing',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Nonstop extension' => {
					'type'			=> 'SO:1000063',
					'typetxt'		=> 'mutation_causing_terminator_codon_change_in_transcript',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Complex - intronic' => {
					'type'			=> 'SO:1000071',
					'typetxt'		=> 'mutation_affecting_splicing',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Coding silent' => {
					'type'			=> 'SO:1000057',
					'typetxt'		=> 'mutation_causing_synonymous_codon_change_in_transcript',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Complex - deletion inframe' => {
					'type'			=> 'SO:1000092',
					'typetxt'		=> 'mutation_causing_complex_change_of_translational_product',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Complex - insertion inframe' => {
					'type'			=> 'SO:1000092',
					'typetxt'		=> 'mutation_causing_complex_change_of_translational_product',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'Complex - frameshift' => {
					'type'			=> 'SO:1000065',
					'typetxt'		=> 'frameshift_mutation',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					},
	'"Parent" fusion - genomic locali' => {
					'type'			=> 'SO:1000181',
					'typetxt'		=> 'mutation_causing_gene_fusion',
					'method_cvid' => 'ECO:0000006',
					'method' 		=> 'Cosmic',
					'method_label'	=> 'Cosmic'
					}
};

#-------------------------------------------------------------------------------------------------------------------

sub init {
    my $self = shift;
    
    $self->{'capabilities'} = {
	'features' => '1.0',
	'sequence' => '1.0',
	'alignment' => '1.0',
	'types'		=>	'1.0'
	};
	#print STDERR Dumper($self);
}

#-------------------------------------------------------------------------------------------------------------------

=head1 METHODS

=cut

=head2  Sequence

 Title		:	sequence
 Usage		:	$sa->sequence;
 Function	:	Returns sequence for a valid segment and error message for invalid segment.
 Example	:	$sa->sequence($opts);
 Returns	:	hash
 Args		:	String

=cut

sub sequence {
  my ($self, $opts) = @_;
  
  my $segment 	= $opts->{'segment'};
  my $qsegment	= $self->transport->dbh->quote($segment.'%');
  my $query 	= qq(SELECT sequence, md5
			 FROM target_sequence
			 WHERE target_id like $qsegment);
  
  my $row          = shift @{$self->transport->query($query)};
  unless ($row){
  	return {
		'moltype'	=>	'Unknown Segment',
		'version'	=>	'Unknown version'
	};
  }
  my $seq = $row->{'sequence'} || "";
  if(defined $opts->{'start'} && defined $opts->{'end'}) {
      $seq = substr($seq, $opts->{'start'}-1, $opts->{'end'}+1-$opts->{'start'});
      
  }
  my $version = $row->{'md5'};
  return {
      'seq'     => $seq,
      'moltype' => 'Protein',
      'version' => $version,
  };
}

#-------------------------------------------------------------------------------------------------------------------

sub segment_version {
  my ($self, $segment) = @_;
  #print STDERR $self->_load_segment_info($segment)->{'md5'}," in segment_version\n";
  my $seg_info = $self->_load_segment_info($segment);
  if($seg_info) {return $seg_info->{'md5'};} else {return 0;}
}

#-------------------------------------------------------------------------------------------------------------------

sub length {
  my ($self, $segment) = @_;
  #print STDERR $self->_load_segment_info($segment)->{'length'}," in length\n";
  #return $self->_load_segment_info($segment)->{'length'};
  my $seg_info = $self->_load_segment_info($segment);
  if($seg_info) {return $seg_info->{'length'};} else {return 0;}
}
#-------------------------------------------------------------------------------------------------------------------
#method for types

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
	my @types;
	if($opts){
		my $segment = $opts->{segment};
		my $start = $opts->{start};
		my $end = $opts->{end};
		my $dsn = $opts->{dsn};
		my $qsegment = $self->transport->dbh->quote($segment.'%');
		unless ( $segment =~ m/^([A-Z0-9]{6}|[A-Z0-9]{6}\.[A-Z0-9]{1})$/i ) {
	    	return { type  	=> "Invalid segment: No type found",
		    		method	=> 'No Method defined',
		    		category => 'No Evidence code'
		    		};
	    }	    
	    my $dbquery = qq(	SELECT qt.query_id, qt.target_id, class from 
	    				Mutation m, query_target qt
	    				WHERE m.auto_query_target = qt.auto_query_target
	    				AND qt.target_id like $qsegment    				
	    				);
	  	my $types = $self->transport->query($dbquery);
		#print STDERR Dumper($types);
		unless ($types->[0]){
			return {
				type  	=> "No type found for $segment",
		    	method	=> 'COSMIC',
		    	category => 'No Evidence code'
			};
		}
		my $types_found;
		foreach my $type (@$types){
			#print Dumper($type);
			$types_found->{$type->{class}}++;			
		}
		foreach my $type_name (keys %$types_found){
			push @types, {
				'type_cvid'   => $type_ref->{$type_name}->{'type'},
        'type'        => $type_ref->{$type_name}->{'typetxt'},
        'method'      => $type_ref->{$type_name}->{'method'},
        'count'       => $types_found->{$type_name},
#    
#				'type'		=> $type_ref->{$type_name}->{'type'},
#				'method'	=> $type_ref->{$type_name}->{'method'},
#				'category' 	=> $type_ref->{$type_name}->{'type_category'},
#				count		=>	$types_found->{$type_name}
			}
		} 
		
	}else{
		#loads all types 
		push @types,{
    		'type'		=> 'ID',
			'method'	=> 'Method used',
			'category' 	=> 'Evidence code Term(Evidence Code from the ECO)'
    		};
		foreach my $key (keys %{$type_ref}){
	    	push @types,{
    			'type_cvid'   => $type_ref->{$key}->{'type'},
          'type'        => $type_ref->{$key}->{'typetxt'},
          'method'      => $type_ref->{$key}->{'method'},
          #'category'    => $type_ref->{$param}->{'type_category'},
#          'type'		=> $type_ref->{$key}->{'type'},
#				  'method'	=> $type_ref->{$key}->{'method'},
#				  'category' 	=> $type_ref->{$key}->{'type_category'},
    		};	
		}		
	}	 
	return @types; 				
}

#-------------------------------------------------------------------------------------------------------------------
#method for alignemnt

=head2  build_alignment

 Title		:	build_alignment
 Usage		:	$sa->build_alignment
 Function	:	Returns all the alignment for valid segment; error message for invalid segment.
 Example	:	$sa->build_alignment($opts)	
 Returns	:	Array of Hashes
 Args		:	optional String

=cut



sub build_alignment {
	my ($self,$query) = @_;
	my $qsegment      = $self->transport->dbh->quote($query.'%');
    
    unless ( $query =~ m/^([A-Z0-9]{6}|[A-Z0-9]{6}\.[A-Z0-9]{1})$/i ) {
    	return { name => 'No Input query or unknown characters',
    		     type => 'unknown type',
    		     description => 'No description for Invalid UniprotID' };   	
    }

	my $dbquery = qq( SELECT qt.query_id , qt.target_id , target_cigar , query_cigar ,
   					t.md5 as t_md5, q.md5 as q_md5, comment , t.length as t_length , 
   					q.length as q_length, q.sequence as q_seq, 
   					t.sequence as t_seq, cosmic_version , uniprot_version from versions v, 
   					target_sequence t, query_sequence q, query_target qt 
   					where qt.target_id = t.target_id and 
   					qt.query_id = q.query_id and 
   					qt.target_id like $qsegment
					);
	my $alignment = $self->transport->query($dbquery);
	#print Dumper($alignment);
	unless($alignment->[0]){
		my @alignment;
		push @alignment, {
				name	=> 	"$query",
				type	=>	'No alignment for Human Uniprot ID',
				max		=>	0,				
			};
		return @alignment;
	}
	my @alignment;
	foreach my $align (@$alignment){
		my ($id) = $align->{ target_id } =~ m/(\w+)\.[0-9]+/ ;
		push @alignment, {
			 name 			=> 	"Cosmic Alignment",
			 type 			=>	'Uniprot_Cosmic',
			 description 	=>	"Pairwise alignment between Uniprot and cosmic sequences",
			 max 			=>	scalar(@$alignment),
			 alignObj 		=>	[
								{
								id 	=> $id,
								version 	=>	$align->{t_md5},
								type 		=>	'PROTEIN',
								dbSource    => 	'Uniprot',
                 				dbVersion   => 	$align->{uniprot_version },
                 				sequence	=>	$align->{t_seq}, 
                 				dbAccession	=>	$id,    
                 				dbCoordSys	=>	'Uniprot'            				
								},
								{
								id 	=> $align->{query_id},
								version 	=>	$align->{q_md5},
								type 		=>	'PROTEIN',
								dbSource    => 	'Cosmic',
                 				dbVersion   => 	$align->{cosmic_version },
                 				sequence	=>	$align->{q_seq}, 
                 				dbAccession	=> $align->{query_id},
                 				dbCoordSys	=>	'Cosmic'                				
								}								
								],
             score			=>	[{
             					method 	=> 	 'pmatch',
             					score 	=>	$align->{comment}
             					}],
             blocks   => [
                {
                 blockScore => $align->{comment},
                 segments   => [
                                {
                                 #id          => $align->{target_id}, # internal object ID
								 id			 =>	$id,
                                 cigar       => $align->{target_cigar},
                                },
                                {
                                 id          => $align->{query_id}, # internal object ID
                                 cigar       => $align->{query_cigar},
                                }
                               ],               	
                }
               ]               					
		};
	}
	return @alignment;
}

#-------------------------------------------------------------------------------------------------------------------

=head2  build_features

 Title		:	build_features
 Usage		:	$sa->build_features;
 Function	:	Returns all the Mutations for valid segment and error message for invalid segment
 Example	:	$sa->build_features($opts);
 Returns	:	Array of Hashes
 Args		:	String

=cut

sub build_features{
	my ($self, $opts) = @_;
    my $segment       = $opts->{'segment'};
    my $start         = $opts->{'start'};
    my $end           = $opts->{'end'};
    my $dsn           = $self->{'dsn'};   
    my $qsegment      = $self->transport->dbh->quote($segment.'%');
    return({
       'label'  => "No Input segment or unknown characters",
	   'start'  => "0",
	   'end'    => "0",	
	   'note'   => "Uniprot segment ID required"
    }) unless ( $segment =~ m/^([A-Z0-9]{6}|[A-Z0-9]{6}\.[A-Z0-9]{1})$/i );
   #change the table-name to Mutation_non for "non-continous display" and Mutation for "continuos dispaly"
   
   	my $query = qq(select qt.query_id,qt.target_id,m.qmut_start,
						m.qmut_end,m.tmut_start,m.tmut_end,
						m.m_count,m.class,m.mut_id,m.mut_label
						from Mutation m, query_target qt 
						where m.auto_query_target = qt.auto_query_target
						and qt.target_id like $qsegment
					);
	my $mut_ref = $self->transport->query($query);
	#print STDERR Dumper($mut_ref); 
	
	my $test 	= qq(SELECT sequence, md5
					 FROM target_sequence
					 WHERE target_id like $qsegment);
    my $test_row= shift @{$self->transport->query($test)};
	if(!$mut_ref->[0]){
    	my @features;
		if(!$test_row){    		
    		push @features, {
	    	'label'  => "Invalid segment",
	    	'start'  => "0",
	    	'end'    => "0",	
	    	'note'   => "Mutation cannot be retrieved for Invalid segment"
			};
			return @features;
    	}else{
    		push @features, {
	    	'label'  => "$segment",
	    	'start'  => "0",
	    	'end'    => "0",	
	    	'note'   => "No Mutation for segment:$segment"
			};
			return @features;    		
    	}    	
    }	
	my @mutation;
	for my $feat (@{$mut_ref}){
		#my ($id) = $feat->{'target_id'} =~ m/(\w+)\.[0-9]+/ ;
		my $feature = $self->_feat_params($feat->{class},$feat);
		push @mutation,  $feature;
	}	
  return @mutation;  
}

#-------------------------------------------------------------------------------------------------------------------

=head1 Internal Methods

=head2  _load_segment_info

 Title		:	_load_segment_info
 Usage		:	$cosmic_obj->_load_segment_info;
 Function	:	Loads md5 checksum and length for given segment
 Example	:	$cosmic_obj->_load_segment_info($segment);
 Returns	:	Reference
 Args		:	String

=cut

sub _load_segment_info {
  my ($self, $segment) = @_;
  $segment			.=	'%';
  my $qsegment         = $self->transport->dbh->quote($segment);
  my $ref              = $self->transport->query(qq(SELECT md5,length
                                                    FROM   target_sequence
                                                    WHERE  target_id like $qsegment));
  my @features = ();
  if($ref->[0]) {
     return $ref->[0];
  }else{
     return 0;
  }
}

#-------------------------------------------------------------------------------------------------------------------

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
	my ($self,$param,$feat) = @_;	
	
	next unless($feat->{tmut_start} =~ /^\d+/);
	next unless($feat->{tmut_end} =~	/^\d+/);
	next unless(exists $type_ref->{$param});
	
	# For Frameshifts, cosmic team prefered to have start coordinates in both start and end column.
	my $start =  $feat->{tmut_start};
	my ( $end,$note ) ;
	if( $feat->{ class } =~ m/Frameshift/i ) {
	  $end = $start;
	  $note = $feat->{mut_label}.": Protein is non-sense from position ".$start;
	}elsif( $feat->{mut_label} =~ m/\d+\*/i){
	  $end = $start;
	  $note = $feat->{mut_label}.": Protein is truncated after position ".$start;
	}else{
	  $end = $feat->{tmut_end};
	  $note= $feat->{mut_label}.": ".$feat->{class},
	}
	
	# to make the feature id unique, i am using the cosmic mut_id as a feature id;
	my $feature = {
		
			'id'			=>	$feat->{mut_id},	
			#'label'			=>	$id.":".$feat->{'wild_seq'}.$feat->{tmut_start}.$feat->{'mut_seq'},
			'label'			=>	$feat->{query_id}.":".$type_ref ->{$param}->{'typetxt'},
#			'start'			=>	$feat->{tmut_start},
#			'end'			=> 	$feat->{tmut_end},
      'start'     =>  $start,
      'end'     =>  $end,
			'score'			=>	$feat->{m_count},
			'target_id'		=>	$feat->{query_id},
			'target_start'	=>	$feat->{qmut_start},
			'target_stop'	=>	$feat->{qmut_end},
			'linktxt'		=> 	$feat->{mut_label},
			'link'			=>	"http://www.sanger.ac.uk/perl/genetics/CGP/cosmic?action=mut_summary&id=".$feat->{mut_id},
			#'note'			=> 	$feat->{mut_label}.": ".$feat->{class},
			'note'     =>  $note,
#			'type'   		=> 	$type_ref ->{$param}->{'type'},	
#			'typetxt'		=> 	$type_ref ->{$param}->{'typetxt'},
#			'type_category' => 	$type_ref ->{$param}->{'type_category'},
#			'method'		=>	$type_ref ->{$param}->{'method'},
#			'method_label'	=>	$type_ref ->{$param}->{'method_label'}		
		  
		  'type'          => $type_ref->{$param}->{'typetxt'},
      'typetxt'       => $type_ref->{$param}->{'typetxt'},
      'type_cvid'     => $type_ref->{$param}->{'type'},
      'method'        => $type_ref->{$param}->{'method'},
      'method_label'  => $type_ref->{$param}->{'method_label'},
      'method_cvid'   => $type_ref->{$param}->{'method_cvid'}, 
    
	};
	return $feature;
}


1;










