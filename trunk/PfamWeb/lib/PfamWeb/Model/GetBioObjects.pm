package PfamWeb::Model::GetBioObjects;

use strict;
use warnings;

use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::Pfam::SeqPfam;

use PfamWeb::Model::BaseModel;
use PfamWeb::Model::Pfam;
use PfamWeb::Model::Pfamseq;
use PfamWeb::Model::PfamA_reg_full;

sub getAnnseq {
    my ($seq_acc_ref, $regions) = shift;
    my $annseqs = &getSeqs($seq_acc_ref);
    &getPfamAFullRegions($annseqs);
    
    return $annseqs;
}

sub  getSeqs {
#First we want to get the sequence
    my $accs_ref = shift;
    my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
    
    #For faster code, make memory declarations here.
    my ($acc, $annSeq, @annSeqs);

    foreach $acc (@$accs_ref){
	$annSeq = $fac->createAnnotatedSequence();
	#Get an object out of the database
	my @rs = PfamWeb::Model::Pfamseq->find( pfamseq_acc => $acc );
	if(!scalar(@rs)){
	    print STDERR "Looking up id\n";
	    @rs = PfamWeb::Model::Pfamseq->find( pfamseq_id => $acc ); 
	}
	
	if(!scalar(@rs)){
	    print STDERR "Looking up secondary\n";
	    @rs = PfamWeb::Model::Pfamseq->find({ "secondary_accession.secondary_acc" => $acc },
					      { join => [qw/ secondary_accession /]}); 
	}
	my $pfamseq = shift @rs;
	

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
    for (my $i = 0; $i < scalar(@$annSeq_ref); $i++){
	#Now get the pfam full regions.
	my @regions = PfamWeb::Model::PfamA_reg_full->search( auto_pfamseq => $$annSeq_ref[$i]->sequence->rdb_index, 
							    in_full      => 1 );
	foreach my $region (@regions){	
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
    }
}


1;
