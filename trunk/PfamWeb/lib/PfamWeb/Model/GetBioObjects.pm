package PfamWeb::Model::GetBioObjects;

use strict;
use warnings;

use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::OtherRegion;
use Bio::Pfam::SmartRegion;
use Bio::Pfam::ContextPfamRegion;
use Bio::Pfam::SeqPfam;
use Bio::SeqFeature::Generic;

sub getAnnseq {
    my ($seq_acc_ref, $regions) = @_;
    my $annseqs = &getSeqs($seq_acc_ref);
    
    &getPfamAFullRegions($annseqs) if($$regions{"pfama"});
    &getPfamBRegions($annseqs) if($$regions{"pfamb"});
    &getOtherRegions($annseqs) if($$regions{"transmembrane"}|| $$regions{"low_complexity"} ||$$regions{"transmembrane"}|| $$regions{"sig_p"}  );
    &getContextRegion($annseqs) if($$regions{"context"});;
    &getSmartRegions($annseqs) if($$regions{"smart"});
    &getDisulphide($annseqs); 
    &getActiveSite($annseqs);
    return $annseqs;
}

sub  getSeqs {
#First we want to get the sequence
    my $accs_ref = shift;
    my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
    
    #For faster code, make memory declarations here.
    my ($acc, $annSeq, @annSeqs);
    
    #Get all of the nested domains here......
    foreach $acc (@$accs_ref){
	#print "REF is ".ref($acc)."\n";
	my $pfamseq;
	$annSeq = $fac->createAnnotatedSequence();
	if(!ref($acc)){
#Get an object out of the database
	
	    my @rs = PfamWeb::Schema::PfamDB::Pfamseq->find( pfamseq_acc => $acc );
	    if(!scalar(@rs)){
		print STDERR "Looking up id\n";
		@rs = PfamWeb::Schema::PfamDB::Pfamseq->find( pfamseq_id => $acc ); 
	    }
	
	    if(!scalar(@rs)){
		print STDERR "Looking up secondary\n";
		@rs = PfamWeb::Schema::PfamDB::Pfamseq->find({ "secondary_accession.secondary_acc" => $acc },
						    { join => [qw/ secondary_accession /]}); 
	    }
	    $pfamseq = shift @rs;
	}else{
	    $pfamseq = $acc;
	}
	    
	    #Now populate the BioPerl sequence object.
	    #There should be more stuff here......
	    $annSeq->id( $pfamseq->pfamseq_id );
	    $annSeq->length( $pfamseq->length );
	    $annSeq->sequence(Bio::Pfam::SeqPfam->new('-seq' => $pfamseq->sequence,
						      '-start' => '1',
						      '-end' => $pfamseq->length,
						      '-id'=> $pfamseq->pfamseq_id,
						      '-acc' => $pfamseq->pfamseq_acc,
						      '-organism' => $pfamseq->species,
						      '-desc' => $pfamseq->description,
						  '-rdb_index' => $pfamseq->auto_pfamseq));
	push(@annSeqs, $annSeq)
	}
    return(\@annSeqs);
}



sub getPfamAFullRegions{
    my $annSeq_ref = shift;
    my $nestings = shift;
    #Now get the pfam full regions for all the sequense in the annSeq array reference.
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){	
	foreach my $region ( PfamWeb::Schema::PfamDB::PfamA_reg_full->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index,
								      in_full      => 1 )){
	    
	    $$annSeq_ref[$i]->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $region->pfamA_acc,
									     '-PFAM_ID' => $region->pfamA_id,
									     '-FROM' => $region->seq_start,
									     '-TO' => $region->seq_end,
									     '-MODEL_FROM' => $region->model_start,
									     '-MODEL_TO' => $region->model_end,
									     '-BITS' => $region->domain_bits_score,
									     '-EVALUE' => $region->domain_evalue_score,
									     '-ANNOTATION' => $region->description,
									     '-MODEL_LENGTH'=>$region->model_length,
									     '-TYPE' => "PfamA",
									     '-ANNOTATION' => $region->description,
									     '-REGION' => $region->type,
									     ));
	
	}
=head1
	#split nested_domains here
	my @pfamAs = sort{ $a->from <=> $b->from}$annSeq->eachAnnotatedRegion;
	if(scalar(@pfamAs)>1){
	    
	    for(my $i=0; $i<$#pfamAs; $i++){
		if($nests{$pfamAs[$i]->accession}){
		    my %nestings = map{$_=> 1}@{$nests{$pfamAs[$i]->accession}};
		    for(my $j = $i+1; $j<=$#pfamAs; $j++){
			if($pfamAs[$i]->to >= $pfamAs[$j]->from && $nestings{$pfamAs[$j]->accession}){
			    
			    my $feature = Bio::SeqFeature::Generic->new('-start' => $pfamAs[$j]->from-1,
									'-end' => $pfamAs[$j]->to+1,
									'-primary' => "nested");
			    
			    $feature->display_name("Discontinuous domain");
			    $annSeq->addFeature($feature);
#copy domain
			    
			    my $pfamA_split = Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $pfamAs[$i]->accession,
									 '-PFAM_ID' => $pfamAs[$i]->id,
									 '-FROM' => $pfamAs[$j]->to+1,
									 '-TO' => $pfamAs[$i]->to,
									 '-MODEL_FROM' => $pfamAs[$i]->model_from + 1,
									 '-MODEL_TO' => $pfamAs[$i]->model_to,
									 '-MODEL_LENGTH'=>$pfamAs[$i]->model_length,
									 '-BITS' => $pfamAs[$i]->bits_score,
									 '-EVALUE' => $pfamAs[$i]->evalue_score,
									 '-TYPE' => "PfamA",
									 '-ANNOTATION' => $pfamAs[$i]->annotation,
									 '-REGION' => $pfamAs[$i]->type 
									 );
			    $pfamAs[$i]->to($pfamAs[$j]->from-1);
			    $pfamAs[$i]->model_to($pfamAs[$i]->model_to-1);
			    $annSeq->addAnnotatedRegion($pfamA_split);
			    @pfamAs = sort{ $a->from <=> $b->from}$annSeq->eachAnnotatedRegion;
			    last;
			}
		    }
		}
	    }
	}
=cut

    }
    
}	

sub getPfamBRegions {  
    my $annSeq_ref = shift;
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	#Get the PfamB data
	foreach my $region ( PfamWeb::Schema::PfamDB::PfamB_reg->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index)){
	    $$annSeq_ref[$i]->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $region->pfamB_acc,
									     '-PFAM_ID' => $region->pfamB_id,
									     '-FROM' => $region->seq_start,
									     '-TO' => $region->seq_end,
									     '-TYPE' => "PfamB"));
	}
    }
}



sub getOtherRegions {
    my $annSeq_ref = shift;
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	#Get the transmembrane etc positions
	foreach my $region (PfamWeb::Schema::PfamDB::Other_reg->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index)){
	    $$annSeq_ref[$i]->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-FROM' => $region->seq_start,
									      '-TO' => $region->seq_end,
									      '-TYPE' => $region->type_id,
									      '-SOURCE' => $region->source_id,
									      '-SCORE' => $region->score,
									      '-ORIENTATION' => $region->orientation,
									      )); 	    
	}
    }
}



sub getContextRegion {
    my$annSeq_ref=shift;
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	#Get the context regions
	foreach my $region (PfamWeb::Schema::PfamDB::Context_pfam_regions->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index)){
	    $$annSeq_ref[$1]->addAnnotatedRegion( Bio::Pfam::ContextPfamRegion->new('-PFAM_ACCESSION' => $region->pfamA_acc,
										   '-PFAM_ID' => $region->pfamA_id,
										   '-FROM' => $region->seq_start,
										   '-TO' => $region->seq_end,
										   '-ANNOTATION' => $region->description,
										   '-DOMAIN_SCORE' => $region->domain_score,
										   '-TYPE' => "context"
										   ));
	}
    }	
}   	


sub getSmartRegions{  
    my$annSeq_ref=shift;
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	#Get SMART matches
	foreach my $region (PfamWeb::Schema::PfamDB::Smart_reg->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index)){
	    $$annSeq_ref[$1]->addAnnotatedRegion( Bio::Pfam::SmartRegion->new('-FROM' => $region->seq_start,
									      '-TO' => $region->seq_end,
									      '-TYPE' => "SMART",
									      '-smart_id' => $region->smart_id,
									      '-smart_accession' => $region->smart_acc,
									      ));
	}
    }
}

sub getDisulphide{
    my $annSeq_ref = shift;
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	foreach my $markup (PfamWeb::Schema::PfamDB::Pfamseq_disulphide->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index)){
	    my $feature = Bio::SeqFeature::Generic->new('-start' => $markup->bond_start,
							'-end' => $markup->bond_end,
							'-primary' => "disulphide");
	    if($markup->bond_start != $markup->bond_end){
		$feature->display_name("Disulphide, ".$markup->bond_start."-".$markup->bond_end);
	    }else{
		$feature->display_name("Intermolecular disulphide, ".$markup->bond_start);
	    }
	    $$annSeq_ref[$i]->addFeature($feature);
	}	       
    }
}

sub getActiveSite{
    my $annSeq_ref = shift;
    #Get the active site
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	foreach my $markup (PfamWeb::Schema::PfamDB::Pfamseq_markup->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index)){
	    my $feature = Bio::SeqFeature::Generic->new('-start' => $markup->residue,
							'-primary' => $markup->label);
	    my $res = substr($$annSeq_ref[$i]->sequence->seq, $markup->residue-1, 1);
	    $feature->display_name($markup->label.", $res".$markup->residue);
	    $$annSeq_ref[$i]->addFeature($feature);
	}
    }
}

1;
