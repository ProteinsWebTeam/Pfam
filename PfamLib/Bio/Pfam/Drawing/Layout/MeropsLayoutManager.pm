package Bio::Pfam::Drawing::Layout::MeropsLayoutManager;

use vars qw($AUTLOAD @ISA @EXPORT $VERSION);
use Exporter;
use Bio::Pfam::Web::PfamWWWConfig;
use Bio::Pfam::Drawing::Layout::LayoutManager;
@ISA = qw(Bio::Pfam::Drawing::Layout::LayoutManager);

sub _default_region_order {
  return qw (merops pfama);
}

sub scale_x {
  my ($self, $scale_x) = @_;
  if ($scale){
    $self->{'scale_x'} = $scale;
  }
  if(!$self->{'scale_x'}){
    $self->{'scale_x'} = 1.0;
  }
  return $self->{'scale_x'};
}

sub scale_y {
  my ($self, $scale_y) = @_;
  if ($scale){
    $self->{'scale_y'} = $scale;
  }
  if(!$self->{'scale_y'}){
    $self->{'scale_y'} = 1.0;
  }
  return $self->{'scale_y'};
}

sub resolve_external_region_overlaps{
    my ($self, $seq, $type, $done) = @_;
    
    my @region = sort{$a->start <=> $b->start}$seq->eachRegionOfType($type);
    foreach my $order ($self->region_order){
	next if($$done{$order} || $order eq $type);
	
	my @regs = sort{$a->start <=> $b->start}$seq->eachRegionOfType($order);
	foreach my $reg1 (@region){
	    #reg1 always wins
	    next if ($reg1->hidden);
	    foreach my $reg2 (@regs){
		
		next if ($reg2->hidden);
		#fudge to prevent Pfam pu domains poking out from behind Merops domains
		my $pfam = $reg1->BioAnnotatedRegion->pfam;
		my $accession = $reg2->BioAnnotatedRegion->accession;
		if($reg1->BioAnnotatedRegion->type eq 'merops' && $pfam =~ /$accession/) {
		    $reg2->hidden(1);

		}elsif($reg1->start <= $reg2->start && $reg1->end >= $reg2->end){
		    #reg1 completely covers region;
		    $reg2->hidden(1);
		    
		}elsif($reg2->start <= $reg1->start && $reg2->end >= $reg1->end){
		    #reg1 is within reg2;
		    #Therefore we need to split region
		    my $reg2a = $reg2->cloneRegion; #2a will become the C-terminal half 
		    
		    
		    #push(@regs, $reg2a);
		    $reg2->end($reg1->start - 1);
		    $reg2a->start($reg1->end + 1);
		    if($order =~ /pfama/i){
			$reg2->BioAnnotatedRegion->model_to($reg2->BioAnnotatedRegion->model_to - 1);
			$reg2a->BioAnnotatedRegion->model_from($reg2a->BioAnnotatedRegion->model_from + 1);
		    }
		    
		    $reg2a->hidden(1) if ($reg2a->end <= $reg2a->start);
		    $reg2a->hidden(1) if ((($reg2a->end - $reg2a->start)*$self->scale_x) <= 1);
		    
		    $seq->addRegion($reg2a);
		    push(@regs, $reg2a);
		}elsif($reg1->end >= $reg2->start && $reg1->start < $reg2->start ){
		    #N-term od reg2 overlaps with C-term of reg1
		     $reg2->start($reg1->end + 1);
		     if($order =~ /pfama/i){
			$reg2->BioAnnotatedRegion->model_from($reg2->BioAnnotatedRegion->model_from + 1);
		    }
		     
		}elsif($reg2->end >= $reg1->start && $reg2->start < $reg1->start ){
		    #C-term of reg2 overlaps with N-term of reg1
		    $reg2->end($reg1->start - 1);
		    if($order =~ /pfama/i){
			$reg2->BioAnnotatedRegion->model_to($reg2->BioAnnotatedRegion->model_to - 1);
		    }
		    
		}
		if ($reg2->end <= $reg2->start){
		    $reg2->hidden(1);
		}
		$reg2->hidden(1) if ((($reg2->end - $reg2->start)*$self->scale_x)  <=1);
	    }
	}
    }

    $$done{$type}++;
}
1;
