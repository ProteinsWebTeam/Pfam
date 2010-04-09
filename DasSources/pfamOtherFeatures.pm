#########
# Author: rdf, jm14
# Maintainer: rdf, jm14
# Created: 2006-10-03
# Last Modified: 2006-10-03
# Builds simple DAS features from the pfam database other than Pfam domains
#

package Bio::Das::ProServer::SourceAdaptor::pfamOtherFeatures;

=head1 AUTHOR

Rob Finn <rdf@sanger.ac.uk>, Jaina Mistry <jm14@sanger.ac.uk>

Copyright (c) 2006 The Sanger Institute

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  See DISCLAIMER.txt for
disclaimers of warranty.

=cut

use strict;
use vars qw(@ISA);
use Bio::Das::ProServer::SourceAdaptor;
@ISA = qw(Bio::Das::ProServer::SourceAdaptor);

#MY BIT TO BE REMOVED
use Data::Dumper;

sub init {
    my $self = shift;
    
    $self->{'capabilities'} = {
	'features' => '1.0',
	};
}

sub _load_segment_info {
  my ($self, $segment) = @_;
  my $qsegment         = $self->transport->dbh->quote($segment);
  my $ref              = $self->transport->query(qq(SELECT md5,length
                                                    FROM   pfamseq
                                                    WHERE  pfamseq_acc = $qsegment));

  return $ref->[0];
}

sub segment_version {
  my ($self, $segment) = @_;
  return $self->_load_segment_info($segment)->{'md5'};
}

sub length {
  my ($self, $segment) = @_;
  return $self->_load_segment_info($segment)->{'length'};
}

sub build_features {
    my ($self, $opts) = @_;
    my $segment       = $opts->{'segment'};
    my $start         = $opts->{'start'};
    my $end           = $opts->{'end'};
    my $dsn           = $self->{'dsn'};
    
    my @features      = ();    
    
    my $qsegment      = $self->transport->dbh->quote($segment);
   
    
    # Perform queries like this:
    #select pfamseq_id, pfamseq_acc, md5, residue, label, annotation from pfamseq_markup m, pfamseq s, markup_key u where s.auto_pfamseq=m.auto_pfamseq and m.auto_markup=u.auto_markup and residue<100 and residue>50 limit 10;
   
  
    #Active sites
    my $qbounds       = "";
    $qbounds          = qq(AND residue <= '$end' AND residue >= '$start') if($start && $end);   
   
    my $act_site_query         = qq(SELECT residue, label, annotation, md5, pfamseq_acc 
			   FROM   pfamseq_markup m, pfamseq s, markup_key u
			   WHERE  s.auto_pfamseq=m.auto_pfamseq
                           AND    m.auto_markup=u.auto_markup 
			   AND    pfamseq_acc=$qsegment $qbounds);

    my $act_site_ref  = $self->transport->query($act_site_query);

    for my $row (@{$act_site_ref}) {

      my $feature = { 'id'     => $row->{'label'},
	              'label'  => $row->{'label'}.":".$row->{'residue'},
		      'type'   => "SO:0001104", 
		      'typetxt' => "catalytic_residue", 
		      'type_category' => "inferred from motif similarity (ECO:0000028)",
		      'start'  => $row->{'residue'},
		      'end'    => $row->{'residue'},
		      'link'   => "http://www.sanger.ac.uk/cgi-bin/Pfam/swisspfamget.pl?name=".$row->{'pfamseq_acc'},
		      'linktxt' => "active site:".$row->{'residue'}
		      };
      if($row->{'annotation'}){
	$$feature{label} .= ":".$row->{'annotation'} unless($row->{'annotation'} eq "NULL");
      }
      #Set the method
      if($row->{'label'} eq "Pfam predicted active site"){
	  $$feature{method} = "Pfam active site rule based(INSERTIONS)";
	  
	  #MY BIT TO BE REMOVED
	  $$feature{method_l} = "Pfam";
	  
      }else{
	$$feature{method} = "UniProt(INSERT1)";

      #MY BIT TO BE REMOVED
      $$feature{method_l} = "UniProt";

      }
      push(@features, $feature);
      print STDERR Dumper($feature);

    }



    #Disulphide bonds
    $qbounds       = "";
    $qbounds          = qq(   (AND (bond_start >= $start  and bond_start <= $end ) or (bond_end >= $start and bond_end <= $end) ) ) if($start && $end);

    my $disulphide_query = qq(SELECT bond_start, bond_end, pfamseq_acc 
			      FROM   pfamseq_disulphide d, pfamseq s
			      WHERE  s.auto_pfamseq=d.auto_pfamseq
			      AND    pfamseq_acc=$qsegment $qbounds);

    my $disul_ref = $self->transport->query($disulphide_query);
    for my $row (@{$disul_ref}) {
      my $feature = { 'id'     => "disulfide",
	              'label'  => "disulfide:".$row->{'bond_start'}."-".$row->{'bond_end'},
		      'type'   => "MOD:00689",
		      'typetxt'  => "disulfide crosslinked residues",
		      'type_category' => "inferred from curator (ECO:0000001)",
		      'start'  => $row->{'bond_start'},
		      'end'    => $row->{'bond_end'},
		      'link'   => "http://www.sanger.ac.uk/cgi-bin/Pfam/swisspfamget.pl?name=".$row->{'pfamseq_acc'},
		      'linktxt' => "disulfide bond:".$row->{'bond_start'}."-".$row->{'bond_end'},
                      'method' => "UniProt"
		      };
      if($row->{'bond_start'} eq $row->{'bond_end'}) {
         $$feature{label} .= " (interchain)"; 
         $$feature{linktxt} .= " (interchain)";
      }
      push(@features, $feature);
      print STDERR Dumper($feature);
  }
  


    #Other regions - signalp, low complexity, coiled coils


    $qbounds       = "";
    $qbounds  = qq( (AND (seq_start >= $start and seq_start <= $end ) or ( seq_end >= $start and seq_end <= $end ) or (seq_start >= $start and seq_end <= $end) ) ) if($start && $end);

    my $other_reg_query = qq(SELECT  seq_start, seq_end, pfamseq_acc, type_id, source_id 
			      FROM   other_reg o, pfamseq s
			      WHERE  s.auto_pfamseq=o.auto_pfamseq
			      AND    pfamseq_acc=$qsegment $qbounds);

    my $other_reg_ref = $self->transport->query($other_reg_query);
    for my $row (@{$other_reg_ref}) {
      if($row->{'source_id'} eq "ncoils") {
        my $feature = { 'id'     => $row->{'type_id'},
	              'label'  => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
		     # 'type'  => "Pfam other region ".$row->{'type_id'},
		      'type'   => "SO:0001080",	
		      'typetxt' => "coiled_coil",
		      'type_category' => "inferred from curator (ECO:0000001)",
		      'start'  => $row->{'seq_start'},
		      'end'    => $row->{'seq_end'},
		      'link'   => "http://www.sanger.ac.uk/cgi-bin/Pfam/swisspfamget.pl?name=".$row->{'pfamseq_acc'},
		      'linktxt' => $row->{'source_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
     		       'method' => $row->{'source_id'}
		      };
        push(@features, $feature); 
     	print STDERR Dumper($feature);
     }
      elsif($row->{'source_id'} eq "Phobius" && $row->{'type_id'} eq 'sig_p') {
        my $feature = { 'id'     => $row->{'type_id'},
	              'label'  => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
		     #'type'   => "Pfam other region " .$row->{'type_id'},
		      'type'   => "SO:0000418",
		      'typetxt' => "signal_peptide",
		      'type_category' => " inferred from electronic annotation (ECO:00000067)",
		      'start'  => $row->{'seq_start'},
		      'end'    => $row->{'seq_end'},
		      'link'   => "http://www.sanger.ac.uk/cgi-bin/Pfam/swisspfamget.pl?name=".$row->{'pfamseq_acc'},
		      'linktxt' => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
     		       'method' => $row->{'source_id'}
		      };
        push(@features, $feature); 
        print STDERR Dumper($feature); 
     }
     elsif($row->{'source_id'} eq "Phobius" && $row->{'type_id'} eq 'transmembrane') {
        my $feature = { 'id'     => $row->{'type_id'},
	              'label'  => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
		     #'type'   => "Pfam other region " .$row->{'type_id'},
		      'type'   => "SO:0001077",
		      'typetxt' => "transmembrane_region",
		      'type_category' => "inferred from curator (ECO:0000001)",
		      'start'  => $row->{'seq_start'},
		      'end'    => $row->{'seq_end'},
		      'link'   => "http://www.sanger.ac.uk/cgi-bin/Pfam/swisspfamget.pl?name=".$row->{'pfamseq_acc'},
		      'linktxt' => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
     		       'method' => $row->{'source_id'}
		      };
        push(@features, $feature); 
        print STDERR Dumper($feature); 
     }
      elsif($row->{'source_id'} eq "seg") {
        my $feature = { 'id'     => $row->{'source_id'},
	              'label'  => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
		      #'type'   => "Pfam other region"  .$row->{'type_id'},
		      'type'	=> "SO:0001004",
		      'typetxt' => "low_complexity",
		      'type_category' => "Evidence code (ECO:0000000)",
		      'start'  => $row->{'seq_start'},
		      'end'    => $row->{'seq_end'},
		      'link'   => "http://www.sanger.ac.uk/cgi-bin/Pfam/swisspfamget.pl?name=".$row->{'pfamseq_acc'},
		      'linktxt' => $row->{'type_id'}.":".$row->{'seq_start'}."-".$row->{'seq_end'},
     		       'method' => $row->{'source_id'}
		      };
        push(@features, $feature);
        print STDERR Dumper($feature);
     }
  }




  


    return @features;
}






1;
