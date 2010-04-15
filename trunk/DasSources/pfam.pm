#########
# Author        : rdf, pg6
# Maintainer    : $Id: pfam.pm,v 1.2 2009-09-22 14:46:41 pg6 Exp $
# Created       : 2006-01-25
# Last Modified : $Date: 2009-09-22 14:46:41 $
# Version       : $Revision: 1.2 $
# Builds simple DAS features from the pfam database
#

=head1 NAME

pfam

=head1 DESCRIPTION

Pfam source adaptor that is specific to the Pfam MySQL database.  This have been heavily modified 
from the first release so that it actually error checks and conforms to bioSapiens ontology.  This 
adaptor has I<features>, I<sequences> and I<types> capabilities.  This source adaptor is expected
to be used as part of ProServer, so no example usages will be given.

=head1 AUTHORS

Rob Finn (rdf@sanger.ac.uk) & Prasad Gunasekaran (pg6@sanger.ac.uk)

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), Prasad Gunasekaran (pg6@sanger.ac.uk)

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

package Bio::Das::ProServer::SourceAdaptor::pfam;
use strict;
use Bio::Das::ProServer::SourceAdaptor;
use Data::Dumper;
use base 'Bio::Das::ProServer::SourceAdaptor';

#declare ontology as global variable
our $type_ref = {

  Family => {
    'type'          => 'SO:0100021',
    'typetxt'       => "polypeptide_conserved_region",
    'type_category' =>
      "inferred from reviewed computational analysis (ECO:0000053)",
    'method'       => "Pfam",
    'method_label' => "Pfam-A",
    'method_cvid'   => "ECO:0000053" 
  },

  Domain => {
    'type'          => 'SO:0000417',
    'typetxt'       => 'polypeptide_domain',
    'type_category' =>
      "inferred from reviewed computational analysis (ECO:0000053)",
    'method'       => "Pfam",
    'method_label' => "Pfam-A",
    'method_cvid'   => "ECO:0000053" 
  },
  Repeat => {
    'type'          => 'SO:0001068',
    'typetxt'       => 'polypeptide_repeat',
    'type_category' =>
      "inferred from reviewed computational analysis (ECO:0000053)",
    'method'       => "Pfam",
    'method_label' => "Pfam-A",
    'method_cvid'   => "ECO:0000053" 
  },
  Motif => {
    'type'          => 'SO:0100017',
    'typetxt'       => 'polypeptide_conserved_motif',
    'type_category' =>
      "inferred from reviewed computational analysis (ECO:0000053)",
    'method'       => "Pfam",
    'method_label' => "Pfam-A",
    'method_cvid'   => "ECO:0000053" 
  },

  'PfamB' => {
    'type'          => 'SO:0000839',
    'typetxt'       => "polypeptide_region",
    'type_category' => "inferred from electronic annotation (ECO:00000067)",
    'method'        => "Pfam",
    'method_label'  => "Pfam-B",
    'method_cvid'   => "ECO:0000067" 
  },

  'Active site' => {
    'type'          => "SO:0001104",
    'typetxt'       => "catalytic_residue",
    'method'        => "Uniprot",
    'method_label'  => "Uniprot",
    'type_category' => "inferred by curator (ECO:0000001)",
    'method_cvid'   => "ECO:0000001"
  },
  'Pfam predicted active site' => {
    'type'          => "SO:0001104",
    'typetxt'       => "catalytic_residue",
    'method'        => "Pfam",
    'method_label'  => "Pfam predicted active site",
    'type_category' => "inferred from motif similarity (ECO:0000028)",
    'method_cvid'   => "ECO:0000028" 
  },
  'Swiss-Prot predicted active site' => {
    'type'          => "SO:0001104",
    'typetxt'       => "catalytic_residue",
    'method'        => "Uniprot",
    'method_label'  => "Uniprot",
    'type_category' => "inferred by curator (ECO:0000001)",
    'method_cvid'   => "ECO:0000001" 
  },

  'disulfide' => {
    'type'          => "MOD:00689",
    'typetxt'       => "disulfide crosslinked residues",
    'type_category' => "inferred from electronic annotation (ECO:0000067)",
    'method'        => "UniProt",
    'method_label'  => "Uniprot",
    'method_cvid'   => "ECO:0000067" 
  },

  'ncoils' => {
    'type'          => "SO:0001080",
    'typetxt'       => "coiled_coil",
    'type_category' => "inferred from electronic annotation (ECO:0000067)",
    'method'        => "Pfam",
    'method_label'  => "Ncoil",
    'method_cvid'   => "ECO:0000067" 
  },

  'sig_p' => {
    'type'          => "SO:0000418",
    'typetxt'       => "signal_peptide",
    'type_category' => "inferred from electronic annotation (ECO:0000067)",
    'method'        => "Phobius",
    'method_label'  => "Phobius",
    'method_cvid'   => "ECO:0000067" 

  },

  'transmembrane' => {
    'type'          => "SO:0001077",
    'typetxt'       => "transmembrane_region",
    'type_category' => "inferred from electronic annotation (ECO:0000067)",
    'method'        => "Phobius",
    'method_label'  => "Phobius",
    'method_cvid'   => "ECO:0000067"
  },

  'seg' => {    #nochange
    'type'          => "SO:0001004",
    'typetxt'       => "low_complexity",
    'type_category' => "inferred from electronic annotation (ECO:0000067)",
    'method'        => "Pfam",
    'method_label'  => "Seg",
    'method_cvid'   => "ECO:0000067" 
    }

};

#-------------------------------------------------------------------------------------------------------------------

sub init {
  my $self = shift;
  $self->{'capabilities'} = {
    'features' => '1.0',
    'sequence' => '1.0',
    'types'    => '1.0',
  };

}

#-------------------------------------------------------------------------------------------------------------------

# subroutine returns the md5 checksum for segment

sub segment_version {
  my ( $self, $segment ) = @_;
  my $seg_info = $self->_load_segment_info($segment);
  if ($seg_info) { return $seg_info->{'md5'}; }
  else { return 0; }
}

#-------------------------------------------------------------------------------------------------------------------

#subroutine returns length of the sequence for segment.

=head1 METHODS

=cut 

=head2 length

=cut

sub length {
  my ( $self, $segment ) = @_;
  my $seg_info = $self->_load_segment_info($segment);
  if ($seg_info) {
    return $seg_info->{'length'};
  }
  else {
    return 0;
  }
}

#-------------------------------------------------------------------------------------------------------------------

=head2  build_types

 Title		:	build_types
 Usage		:	$sa->build_types
 Function	:	Returns all the types for valid segment; error message for invalid segment and all types if called without segment
 Example	:	$sa->build_features($opts)	
 Returns	:	Array of Hashes
 Args		:	optional String

=cut

#subroutine loads the types for the segment.

sub build_types {
  my ( $self, $opts ) = @_;
  my @types;

  # checks for segment
  if ($opts) {
    my $segment  = $opts->{'segment'};
    my $start    = $opts->{'start'};
    my $end      = $opts->{'end'};
    my $dsn      = $self->{'dsn'};
    my $qsegment = $self->transport->dbh->quote($segment);
    my $test     =
      $self->transport->query(
      qq(SELECT pfamseq_id FROM  pfamseq WHERE  pfamseq_acc = $qsegment));

    # checks valid segment or not
    if ( !$test->[0] ) {
      my @types;
      push @types,
        {
        'type'     => "Invalid segment: No type found",
        'method'   => 'No Method defined',
        'category' => 'No Evidence code '
        };
      return @types;
    }
    else {

      # PfamA regions
      my $ref_A = $self->_get_PfamA_regions( $start, $end, $qsegment );
      my $typesFound;
      foreach my $pfamA ( @{$ref_A} ) {
        $typesFound->{ $pfamA->{'type'} }++;
      }
      foreach my $typeName ( keys %$typesFound ) {
        push( @types,
          $self->_type_params( $typeName, $typesFound->{$typeName} ) );
      }

      # PfamB regions
      my $ref_B = $self->_get_PfamB_regions( $start, $end, $qsegment );
      push( @types, $self->_type_params( "PfamB", scalar( @{$ref_B} ) ) )
        if ( scalar( @{$ref_B} ) > 0 );

      # Active site
      my $act_site_ref = $self->_get_activesite( $start, $end, $qsegment );
      my $act_types;
      foreach my $activesite_feat ( @{$act_site_ref} ) {
        $act_types->{ $activesite_feat->{'label'} }++;
      }
      foreach my $act_type_name ( keys %{$act_types} ) {
        push( @types,
          $self->_type_params( $act_type_name, $act_types->{$act_type_name} ) );
      }

      #other features
      my $other_reg_ref = $self->_get_Other_regions( $start, $end, $qsegment );
      my $other_types;
      foreach my $other_feat ( @{$other_reg_ref} ) {
        $other_types->{ $other_feat->{'source_id'} }++
          unless ( $other_feat->{'source_id'} eq 'Phobius' );
        $other_types->{ $other_feat->{'type_id'} }++
          if ( $other_feat->{'source_id'} eq 'Phobius' );
      }
      foreach my $other_type_name ( keys %{$other_types} ) {
        push(
          @types,
          $self->_type_params(
            $other_type_name, $other_types->{$other_type_name}
          )
        );
      }

      # No types for valid segment
      if ( !@types ) {
        push @types,
          {
          'type'     => "No annotation",
          'method'   => "No method",
          'category' => "No Evidence Code"
          };
      }

    }
  }
  else {

    #loads all types
    push @types,
      {
      'type'     => 'ID',
      'method'   => 'Method used',
      'category' => 'Evidence code Term(Evidence Code from the ECO)'
      };
    foreach my $key ( keys %{$type_ref} ) {

      push @types,
        {
        'type_cvid'   => $type_ref->{$key}->{'type'},
        'type'        => $type_ref->{$key}->{'typetxt'},
        'method'      => $type_ref->{$key}->{'method'},
        #'category'    => $type_ref->{$param}->{'type_category'},
    
        };
    }
  }

  return @types;
}

#-------------------------------------------------------------------------------------------------------------------

=head2  build_features

 Title		:	build_features
 Usage		:	$sa->build_features;
 Function	:	Returns all the features for valid segment and error message for invalid segment
 Example	:	$sa->build_features($opts);
 Returns	:	Array of Hashes
 Args		:	String

=cut

# subroutine loads features for input segment

sub build_features {
  my ( $self, $opts ) = @_;
  my $segment  = $opts->{'segment'};
  my $start    = $opts->{'start'};
  my $end      = $opts->{'end'};
  my $qsegment = $self->transport->dbh->quote($segment);
  my $test     = $self->transport->query(
    qq(SELECT pfamseq_id FROM pfamseq WHERE  pfamseq_acc = $qsegment)
  );

  # Test for valid segment
  if ( !$test->[0] ) {
    my @features;
    push @features,
      {
      'label' => "Invalid segment",
      'start' => "0",
      'end'   => "0",
      'note'  => "Sequence cannot be retrieved for invalid segment"
      };
    return @features;
  }
  
  # validation fails if feature id is not unique.
  # for id to be unique, i can use pfamA_acc and pfamB_acc for 
  # pfamA's and B's respectively, inoreder to be consistent, i
  # am using the following pattern for all features
  # ID-START-END to make them unique,
  
  # Pfam-A
  my $ref = $self->_get_PfamA_regions( $start, $end, $qsegment );
  my $VERSION = $self->transport->query(qq(SELECT hmmer_version from VERSION)) if ($ref);
  my @features = ();
  my $href     = "http://pfam.sanger.ac.uk/";

  for my $row ( @{$ref} ) {
    my $ptype   = $row->{'type'};
    my $id      = $row->{'pfamA_id'};
    my $feature =
      $self->_feat_params( $ptype, $id, $row->{'seq_start'}, $row->{'seq_end'},
      $href . "family?entry=" . $row->{'pfamA_acc'}, $id );
    $$feature{'score'} = $row->{'domain_evalue_score'};
    $$feature{'note'}  = "HMMER Version: " . $VERSION->[0]->{hmmer_version};
    push( @features, $feature );

  }

  # Pfam-B
  my $ref = $self->_get_PfamB_regions( $start, $end, $qsegment );
  for my $row ( @{$ref} ) {
    my $id = $row->{'pfamB_id'};
    push(
      @features,
      $self->_feat_params(
        "PfamB", $id, $row->{'seq_start'}, $row->{'seq_end'},
        $href . "pfamb?entry=" . $row->{'pfamB_acc'}, $id
      )
    );

  }

  #Active sites
  my $act_site_ref = $self->_get_activesite( $start, $end, $qsegment );

  #print Dumper($act_site_ref);	#remove it later
  for my $row ( @{$act_site_ref} ) {
    my $id      = $row->{'label'};
    my $feature = $self->_feat_params(
      $id, $id, $row->{'residue'}, $row->{'residue'},
      $href . "protein?entry=" . $row->{'pfamseq_acc'},
      "active site:" . $row->{'residue'}
    );
    $$feature{note} = "Experimental Result" if ( $id eq "Active site" );
    push( @features, $feature );

  }

  #Disulphide bonds
  my $disul_ref = $self->_get_disulfide( $start, $end, $qsegment );
  for my $row ( @{$disul_ref} ) {
    my $id      = "disulfide";
    my $feature = $self->_feat_params(
      $id,
      $id,
      $row->{'bond_start'},
      $row->{'bond_end'},
      $href . "protein?entry=" . $row->{'pfamseq_acc'},
      "disulfide bond:" . $row->{'bond_start'} . "-" . $row->{'bond_end'}
    );

    if ( $row->{'bond_start'} eq $row->{'bond_end'} ) {
      $$feature{label}   .= " (interchain)";
      $$feature{linktxt} .= " (interchain)";
    }
    push( @features, $feature );
  }

  #Other regions - signalp, low complexity, coiled coils
  my $other_reg_ref = $self->_get_Other_regions( $start, $end, $qsegment );

  for my $row ( @{$other_reg_ref} ) {

    my $id = $row->{'type_id'};
    my ( $feature, $linktxt );
    $linktxt =
      $row->{'source_id'} . ":" . $row->{'seq_start'} . "-" . $row->{'seq_end'}
      if ( $row->{'source_id'} eq "ncoils" );
    $linktxt =
      $row->{'type_id'} . ":" . $row->{'seq_start'} . "-" . $row->{'seq_end'}
      if ( $row->{'source_id'} ne "ncoils" );
    $feature =
      $self->_feat_params( $id, $id, $row->{'seq_start'}, $row->{'seq_end'},
      $href . "protein?entry=" . $row->{'pfamseq_acc'}, $linktxt )
      if ( $row->{'source_id'} eq "Phobius" );
    $feature =
      $self->_feat_params( $row->{'source_id'}, $id, $row->{'seq_start'},
      $row->{'seq_end'}, $href . "protein?entry=" . $row->{'pfamseq_acc'},
      $linktxt )
      if ( $row->{'source_id'} ne "Phobius" );
    #$$feature{'id'} = $row->{'source_id'} if ( $row->{'source_id'} eq "seg" );
    push( @features, $feature );

  }

  # No features for valid segment
  if ( !@features ) {
    push @features,
      {
      'label' => "Segment",
      'note'  => "No features found for the segment"
      };
  }
  return @features;
}

#-------------------------------------------------------------------------------------------------------------------

=head2  sequence

 Title		:	sequence
 Usage		:	$sa->sequence
 Function	:	Returns sequence for a valid segment and error message for invalid segment.
 Example	:	$pfam_obj->sequence($opts);
 Returns	:	hash
 Args		:	String

=cut

#subroutine returns sequence for valid segment and error message for invalid segment.

sub sequence {
  my ( $self, $opts ) = @_;

  my $segment  = $opts->{'segment'};
  my $qsegment = $self->transport->dbh->quote($segment);
  my $query    = qq(SELECT sequence, md5
			 		FROM pfamseq
			      	WHERE pfamseq_acc=$qsegment);

  my $row = shift @{ $self->transport->query($query) };

  unless ($row) {
    return {
      'moltype' => 'Unknown segment',
      'version' => 'No Version',

    };
  }
  my $seq = $row->{'sequence'} || "";
  if ( defined $opts->{'start'} && defined $opts->{'end'} ) {
    $seq = substr(
      $seq,
      $opts->{'start'} - 1,
      $opts->{'end'} + 1 - $opts->{'start'}
    );

  }
  my $version = $row->{'md5'};
  return {
    'seq'     => $seq,
    'moltype' => 'Protein',
    'version' => $version,
  };
}

#-------------------------------------------------------------------------------------------------------------------

=head2  _load_segment_info

 Title		:	_load_segment_info
 Usage		:	$pfam_obj->_load_segment_info;
 Function	:	Loads md5 checksum and length for given segment
 Example	:	$pfam_obj->_load_segment_info($segment);
 Returns	:	Reference
 Args		:	String

=cut

#subroutine loads md5 and length of the segment

sub _load_segment_info {
  my ( $self, $segment ) = @_;
  my $qsegment = $self->transport->dbh->quote($segment);
  my $ref      = $self->transport->query(
    qq(SELECT md5,length
                                                    FROM   pfamseq
                                                    WHERE  pfamseq_acc = $qsegment)
  );

  #my @features = ();
  if ( $ref->[0] ) {
    return $ref->[0];
  }
  else {
    return 0;
  }
}

#-------------------------------------------------------------------------------------------------------------------

#subroutine returns Pfam-A features as a hash-ref for segment

=head2  _get_PfamA_regions

 Title		:	_get_PfamA_regions
 Usage		:	$pfam_obj->_get_PfamA_regions;
 Function	:	Returns all the Pfam-A regions like Family, Domain, Repeat and Motif for input 
                segment
 Example	:	$pfam_obj->_get_PfamA_regions($start,$end,$segment);
 Returns	:	Reference containing Pfam-A features as hash.
 Args		:	String 

=cut

sub _get_PfamA_regions {
  my ( $self, $start, $end, $qsegment ) = @_;
  my $qbounds = qq(AND seq_start <= '$end' AND seq_end >= '$start')
    if ( $start && $end );

  # Select the pfamA regions
  my $query =
qq(SELECT pfamA_id, pfamA_acc, pfamA.description, md5, seq_start, seq_end, domain_evalue_score,pfamA.type  
			   					FROM   pfamA, pfamA_reg_full_significant, pfamseq 
			  					WHERE  pfamA.auto_pfamA=pfamA_reg_full_significant.auto_pfamA
			  					AND pfamA_reg_full_significant.auto_pfamseq=pfamseq.auto_pfamseq 
			   					AND in_full=1
			   					AND pfamseq_acc=$qsegment $qbounds);

  my $ref = $self->transport->query($query);
  return $ref;

}

#-------------------------------------------------------------------------------------------------------------------

#subroutine returns Pfam-B features as a hash-ref for segment

=head2  _get_PfamB_regions

 Title		:	_get_PfamB_regions
 Usage		:	$pfam_obj->_get_PfamB_regions;
 Function	:	Returns all the Pfam-B regions 
 Example	:	$pfam_obj->_get_PfamB_regions($start,$end,$segment);
 Returns	:	Reference containing Pfam-B features as hash.
 Args		:	String 

=cut

sub _get_PfamB_regions {
  my ( $self, $start, $end, $qsegment ) = @_;
  my $qbounds = qq(AND seq_start <= '$end' AND seq_end >= '$start')
    if ( $start && $end );
  my $query = qq(SELECT pfamB_id, pfamB_acc, md5, seq_start, seq_end 
								FROM   pfamB, pfamB_reg, pfamseq 
								WHERE  pfamB.auto_pfamB=pfamB_reg.auto_pfamB
								AND pfamB_reg.auto_pfamseq=pfamseq.auto_pfamseq
								AND pfamseq_acc=$qsegment $qbounds);

  my $ref = $self->transport->query($query);
  return $ref;
}

#-------------------------------------------------------------------------------------------------------------------

#subroutine returns Activesite features as a hash-ref for segment

=head2  _get_activesite

 Title		:	_get_activesite
 Usage		:	$pfam_obj->_get_activesite
 Function	:	Returns all the predicted and experimentally determined active sites present in the 
                input segment
 Example	:	$pfam_obj->_get_activesite($start,$end,$segment);
 Returns	:	Reference containing active site features as hash.
 Args		:	String 

=cut

sub _get_activesite {
  my ( $self, $start, $end, $qsegment ) = @_;
  my $qbounds = "";
  $qbounds = qq(AND residue <= '$end' AND residue >= '$start')
    if ( $start && $end );

  my $act_site_query = qq(SELECT residue, label, annotation, md5, pfamseq_acc 
			  					 FROM   pfamseq_markup m, pfamseq s, markup_key u
			  					 WHERE  s.auto_pfamseq=m.auto_pfamseq
              					 AND    m.auto_markup=u.auto_markup 
			  					 AND    pfamseq_acc=$qsegment $qbounds);

  my $act_site_ref = $self->transport->query($act_site_query);
  return $act_site_ref;

}

#-------------------------------------------------------------------------------------------------------------------

#subroutine returns disulfide features as a hash-ref for segment

=head2  _get_disulfide

 Title		:	_get_disulfide
 Usage		:	$pfam_obj->_get_disulfide
 Function	:	Returns all the disulfide regions present in segment
 Example	:	$pfam_obj->_get_disulfide($start,$end,$segment)
 Returns	:	Reference containing disulfide features as hash.
 Args		:	String 

=cut

sub _get_disulfide {
  my ( $self, $start, $end, $qsegment ) = @_;
  my $qbounds = "";
  $qbounds =
qq(   (AND (bond_start >= $start  and bond_start <= $end ) or (bond_end >= $start and bond_end <= $end) ) )
    if ( $start && $end );

  my $disulphide_query = qq(SELECT bond_start, bond_end, pfamseq_acc 
			      				FROM   pfamseq_disulphide d, pfamseq s
			      				WHERE  s.auto_pfamseq=d.auto_pfamseq
			      				AND    pfamseq_acc=$qsegment $qbounds);

  my $disul_ref = $self->transport->query($disulphide_query);
  return $disul_ref;
}

#-------------------------------------------------------------------------------------------------------------------

#subroutine returns other features like sig-p, transmembrane, ncoil, lowcomplexity as a hash-ref for segment

=head2  _get_Other_regions

 Title		:	_get_Other_regions
 Usage		:	$pfam_obj->_get_Other_regions
 Function	:	Returns all the Other regions like sig-p, transmembrane, coiled coil, lowcomplexity in segment
 Example	:	$pfam_obj->_get_Other_regions($start,$end,$segment);
 Returns	:	Reference containing Other features as hash.
 Args		:	String 

=cut

sub _get_Other_regions {
  my ( $self, $start, $end, $qsegment ) = @_;
  my $qbounds = "";
  $qbounds =
qq( (AND (seq_start >= $start and seq_start <= $end ) or ( seq_end >= $start and seq_end <= $end ) or (seq_start >= $start and seq_end <= $end) ) )
    if ( $start && $end );

  my $other_reg_query =
    qq(SELECT  seq_start, seq_end, pfamseq_acc, type_id, source_id 
			      				FROM   other_reg o, pfamseq s
			      				WHERE  s.auto_pfamseq=o.auto_pfamseq
			      				AND    pfamseq_acc=$qsegment $qbounds);

  my $other_reg_ref = $self->transport->query($other_reg_query);
  return $other_reg_ref;
}

#-------------------------------------------------------------------------------------------------------------------

# subroutine returns ontology for features

=head2  _type_params

 Title		:	_type_params
 Usage		:	$pfam_obj->_type_params
 Function	:	Returns Ontology information for specific feature
 Example	:	$pfam_obj->_type_params($type,$count);
 Returns	:	Array of hash 
 Args		:	String 

=cut

sub _type_params {
  my ( $self, $param, $count ) = @_;
  my @type_feat;
  push @type_feat,
    {
#    'type'     => $type_ref->{$param}->{'type'},
#    'method'   => $type_ref->{$param}->{'method'},
#    'category' => $type_ref->{$param}->{'type_category'},
#    'count'    => $count
    'type_cvid'   => $type_ref->{$param}->{'type'},
    'type'        => $type_ref->{$param}->{'typetxt'},
    'method'      => $type_ref->{$param}->{'method'},
    #'category'    => $type_ref->{$param}->{'type_category'},
    'count'       => $count
    };
  return @type_feat;
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
  my ( $self, $param, $id, $start, $end, $link, $linktxt ) = @_;

  next unless ( $start =~ /^\d+/ );
  next unless ( $end   =~ /^\d+/ );
  next unless ( exists $type_ref->{$param} );

  my $feature = {
    'id'            => $id . ":" . $start . "-" . $end,
    'label'         => $id . ":" . $start . "-" . $end,
    'start'         => $start,
    'end'           => $end,
    'link'          => $link,
    'linktxt'       => $linktxt,
#    'type'          => $type_ref->{$param}->{'type'},
#    'typetxt'       => $type_ref->{$param}->{'typetxt'},
#    'type_category' => $type_ref->{$param}->{'type_category'},
#    'method'        => $type_ref->{$param}->{'method'},
#    'method_label'  => $type_ref->{$param}->{'method_label'}
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