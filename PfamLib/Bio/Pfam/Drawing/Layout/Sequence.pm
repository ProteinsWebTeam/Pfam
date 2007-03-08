
=head1 NAME 

Bio::Pfam::Drawing::Layout::Sequence - The intermediated between an Bio::Pfam::AnnotatedSeq and an Image

=head1 SYNOPSIS




=head1 DESCRIPTION



=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...
package Bio::Pfam::Drawing::Layout::Sequence;
use vars qw($AUTOLOAD @ISA @EXPORT);
use strict;
use warnings;
use Exporter;
use Data::Dumper;

=head2 new


=cut

sub new {
    my $caller = shift;
    my $self = bless {}, ref($caller) || $caller;
    my %params = @_;

    $self->{ 'region' }  = [];
    $self->{ 'markup' }  = [];
    $self->{ 'name' }    = undef;
    $self->{ 'length' }  = undef;
    $self->{ 'info' }    = undef;
    $self->{ 'hidden'  } = undef;
    $self->{ 'colour1' } = undef;
    $self->{ 'colour2' } = undef;

    return $self;
}

#Now all of the get sets

=head2 addRegion

 Title   : addRegion
 Usage   : $sequence->addRegion($region_object);
 Function: adds a region object to the sequence object
 Returns : Nothing
 Args    : None

=cut

sub addRegion{
   my ($self, $region) = @_;
   # simply push the region onto the list;
   push(@{$self->{ 'region' }}, $region);
}

=head2 eachRegion

 Title   : eachRegion
 Usage   : @regions = $sequence->eachRegion();
 Function: Returns an array of Layout::Region objects for the sequence 
 Returns : Returns an array of region objects
 Args    : None

=cut

sub eachRegion{
   my ($self) = @_;
   return @{$self->{ 'region' }};
}



sub addMarkup{
   my ($self, $markup) = @_;
   push @{$self->{ 'Markup' }}, $markup;
}



sub eachMarkup {
   my ($self) = @_;
   if(scalar($self->{'Markup'})){
     return @{$self->{'Markup'}};
   }
}

sub name{
   my ($self, $name) = @_;
   if($name){
     $self->{'name'} = $name;
   }
   return $self->{'name'};
}

sub info{
   my ($self, $info) = @_;
   if($info){
     $self->{'info'} = $info;
   }
   return $self->{'info'};
}

sub hidden{
   my ($self, $hidden) = @_;
   if($hidden){
     $self->{'hidden'} = $hidden;
   }
   return $self->{'hidden'};
}

sub colour1{
   my ($self, $colour) = @_;
    if($colour){
      if($colour->isa("Bio::Pfam::Drawing::Colour::hexColour") || 
	 $colour->isa("Bio::Pfam::Drawing::Colour::rgbColour")){
	$self->{'colour1'} = $colour;
      }else {
        warn "$colour is not a Bio::Pfam::Drawing::Colour::xxxColour";
      }
    }
    return $self->{'colour1'};
}

sub colour2{
   my ($self, $colour) = @_;
    if($colour){
      if($colour->isa("Bio::Pfam::Drawing::Colour::hexColour") || 
	 $colour->isa("Bio::Pfam::Drawing::Colour::rgbColour")){
	$self->{'colour2'} = $colour;
      }else {
        warn "$colour is not a Bio::Pfam::Drawing::Colour::xxxColour";
      }
    }
    return $self->{'colour2'};
}

=head2 length

  Title   : length
  Usage   : $sequence->length( $theSequenceLength );
  Function: sets or returns the length of the sequence
  Returns : Interger of the sequence length
  Args    : A length (optional);

=cut

sub length{
  my ($self, $length) = @_;
  if($length){
    $self->{'length'} = $length;
  }else{
    return $self->{'length'};
  }
}

=head2 convert_seq

 Title   : convert_seq
 Usage   : $layoutsequence->convert_seq( $aBioSequence );
 Function: Populates a "layout sequence" from a Bio::Pfam::AnnotatedSeq object
         : Due to the modifications that the layoutManager performs on the regions, the
         : Bio::Pfam::AnnotaedRegions of the sequence are stored.
 Returns : ref to a Bio::Pfam::AnnotatedSeq object
 Args    : A Bio::Pfam::AnnotatedSeq

=cut

sub convert_seq {
  my ($self, $seq, $regionsAndFeatures) = @_;
  $self->length($seq->length);
  $self->name($seq->id);
  if($seq->sequence && $seq->sequence->organism){
      $self->info("[".$seq->sequence->organism."] ".$seq->sequence->desc);
  }

  if($regionsAndFeatures){
      foreach my $region ($seq->eachAnnotatedRegion){
	  if($$regionsAndFeatures{$region->type}){
	      my $l_reg = Bio::Pfam::Drawing::Layout::Region->new();
	      $self->addRegion($l_reg);
	      $l_reg->convert_reg($region);
	  }
      }
      
      if(!$$regionsAndFeatures{"noFeatures"}){
	  foreach my $feature ($seq->eachFeature){
	      my $markup = Bio::Pfam::Drawing::Layout::Markup->new();
	      $self->addMarkup($markup);
	      $markup->convert_feature($feature);
	  }
      }
  }else{
      foreach my $region ($seq->eachAnnotatedRegion){
	  my $l_reg = Bio::Pfam::Drawing::Layout::Region->new();
	  $self->addRegion($l_reg);
	  $l_reg->convert_reg($region);
      }
      
      foreach my $feature ($seq->eachFeature){
	  my $markup = Bio::Pfam::Drawing::Layout::Markup->new();
	  $self->addMarkup($markup);
	  $markup->convert_feature($feature);
      }
      
  }
  
}

sub convertDasSeqAndFeatures {
    my ($self, $seqSource, $sourceUrl, $features) = @_;
    
    while (my ($source,$seqs) = each %$seqSource){ 
		my $s = $seqs->[0];
		if($s->{'sequence'}){
		    $self->length(CORE::length($s->{'sequence'}));
		}elsif($s->{'start'} && $s->{'end'}){
		    $self->length(($s->{'end'} - $s->{'start'}) + 1 );
		}elsif($s->{'sequence_start'} && $s->{'sequence_end'}){
		    $self->length(($s->{'sequence_end'} - $s->{'sequence_start'}) + 1 );
		}else{
		    warn "Can not determine sequence length, this will probably prove fatal\n";
		}
		$self->name($s->{'sequence_id'});
		$self->info($sourceUrl);
		foreach my $feature (@$features){
	    		if ($feature->{'drawingType'} eq "Region"){
				my $region = Bio::Pfam::Drawing::Layout::Region->new();
				$self->addRegion($region);
				$region->convertDasRegion($feature, $source, $s->{'sequence_id'});
			} elsif( $feature->{'drawingType'} eq "Markup"){
				my $markup = Bio::Pfam::Drawing::Layout::Markup->new();
				$self->addMarkup($markup);
				$markup->convertDasMarkup($feature, $source, $s->{'sequence_id'});
			}
		}
    }
}




sub sequence2XMLDOM {
    my $self = shift;
    my $dom = shift;
    my $element = $dom->createElement("sequence");
    $element->setAttribute("name", $self->name);
    $element->setAttribute("length", $self->length);
    $element->setAttribute("display_data", $self->info);

    if($self->colour1){
      my $c1 = $dom->createElement("colour1");
      $element->appendChild($c1);
      my $colour = $self->colour1->colour2XMLDOM($dom);
      $c1->appendChild($colour);
    }
    if($self->colour1){
      my $c2 = $dom->createElement("colour2");
      $element->appendChild($c2);
      my $colour = $self->colour1->colour2XMLDOM($dom);
      $c2->appendChild($colour);
    }
    

    $element->setAttribute("hidden", 1) if($self->hidden);
    foreach my $reg ($self->eachRegion){
	  if(!$reg->hidden){
	    my $region_element = $reg->region2XMLDOM($dom);
	    $element->appendChild($region_element);
	  }
    }
	
    foreach my $reg ($self->eachMarkup){
	  if($reg){
	    my $markup_element = $reg->markup2XMLDOM($dom);
	    $element->appendChild($markup_element);
	  }
    }

    return $element;
}

sub eachRegionOfType{
    my ($self, $type) = @_;
    my @regs;
    foreach my $reg ($self->eachRegion){
	if ($reg->BioAnnotatedRegion->type){
	    if ($reg->BioAnnotatedRegion->type =~ /$type/i){
		push(@regs, $reg);
	    }
	}
    }
    @regs = sort{$a->start <=> $b->start}@regs;
    return @regs;
}

sub getKey {
    my $self = shift;
    my %key;
    foreach my $reg ($self->eachRegion){
	my ($st, $end, $url, $source, $label, $type, $colour, $hidden);
	#get the start,end, label, source and link url.
	#This is going to be eval, as not all region->BioAnnotatedRegion
	#have all of the methods;
	eval{
	    $type = $reg->BioAnnotatedRegion->type;
	    $st = $reg->BioAnnotatedRegion->from;
	    $end = $reg->BioAnnotatedRegion->to;
	    $label = $reg->label;
	    $colour = $reg->colour1->colour;
	    $url = $reg->url;
	    $hidden = $reg->hidden;
	    $source = $reg->BioAnnotatedRegion->source;
	};
	
	$key{"$label:$type:$st:$end"}{"source"} = $source if($source);
	$key{"$label:$type:$st:$end"}{"url"} = $url if($url);
	$key{"$label:$type:$st:$end"}{"start"} = $st;
	$key{"$label:$type:$st:$end"}{"end"} = $end;
	$key{"$label:$type:$st:$end"}{"label"} = $label;
	$key{"$label:$type:$st:$end"}{"type"} = $type;
	$key{"$label:$type:$st:$end"}{"colour"} = $colour;
	$key{"$label:$type:$st:$end"}{"hidden"} = $hidden;
    }
    return %key;
}



1;
