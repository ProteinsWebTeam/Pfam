
# AlignmentGenerator.pm
# jt6 20060601 WTSI
#
# ?
#
# $Id: AlignmentGenerator.pm,v 1.1 2006-06-01 16:25:41 jt6 Exp $

package PfamWeb::Controller::AlignmentGenerator;

use strict;
use warnings;

use Data::Dumper;
use Bio::Pfam::ColourAlign;

use base "PfamWeb::Controller::Family";

# pick up a URL like http://localhost:3000/alignmentgenerator?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  my @dsnList = ( "http://pfam1b.internal.sanger.ac.uk:9000/das/pfamAlign" );

  my $dl = PfamWeb::Model::Das_sources->getDasLite;
  $dl->dsn( \@dsnList );

  my $alignment = $dl->alignment( { query => $c->stash->{pfam}->pfamA_acc . ".full" } );
  my $features  = $dl->features( $c->stash->{pfam}->pfamA_acc . ".full" );
  my $consensus = getConsensus( $features );

  $c->stash->{alignment} = [ reconstructAli( $alignment ) ];
  $c->stash->{consensus} = Bio::Pfam::ColourAlign::parseConsensus( $consensus );

}

#-------------------------------------------------------------------------------
# override the end method from the Family class, so that we now hand
# off to a template that doesn't require the wrapper

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  foreach my $alignment ( @{$c->stash->{alignment}} ) {
	Bio::Pfam::ColourAlign::markupAlign( $alignment, $c->stash->{consensus} );
  }

}

#-------------------------------------------------------------------------------

sub reconstructAli {
    my $ali = shift;
    my ($source, $aliData) = each %$ali;

    my @alignments;
    #print "$source, $aliData\n";
    for (my $i = 0; $i < scalar(@$aliData); $i++){
	#print $aliData->[$i]->{'alignobject'}."\n";
	my %aliObjects = map{$_->{'alignobject_intObjectId'} => $_} @{$aliData->[$i]->{'alignobject'}};
	foreach my $block (sort{$a->{'block_blockOrder'} <=> $b->{'block_blockOrder'}} @{$aliData->[$i]->{'block'}}){
	    my %ali;
	    foreach my $bseqRef (@{$block->{'segment'}}){
		$ali{$bseqRef->{'segment_intObjectId'}."/".$bseqRef->{'segment_start'}."-".$bseqRef->{'segment_end'}} = &getAliString($bseqRef, \%aliObjects); 
		#printf("%-20s %s\n", $bseqRef->{'segment_intObjectId'}."/".$bseqRef->{'segment_start'}."-".$bseqRef->{'segment_end'}, $aliString);
	    }
	    push(@alignments, \%ali);
	}
    }
    return @alignments;
}

sub getAliString{
    my ($bseqRef, $aliObjectsRef) = @_;
    my $seq = substr($aliObjectsRef->{$bseqRef->{'segment_intObjectId'}}->{'sequence'}, ($bseqRef->{'segment_start'}-1), $bseqRef->{'segment_end'}-$bseqRef->{'segment_start'}+1);
    my $aliString = cigar2align($bseqRef->{'cigar'},$seq);
    return $aliString;
}

sub cigar2align  {
    my $cigar = shift;
    $cigar =~ s/\"//g;
    my $seq = shift;
    my $tmp = $cigar;
    my $start = 0;
    my $len = length($seq);
    $tmp =~ s/(\d+)D/'-'x$1/eg;
    $tmp =~ s/D/\-/g;
    $tmp =~ s/(\d+)I/'.'x$1/eg;
    $tmp =~ s/I/\./g;
    $tmp =~ s/(\d{0,5})M/if($1){$start+=$1,($start<=$len)?substr($seq,$start-$1,$1):'~'x$1}else{$start+=1,($start<=$len)?substr($seq,$start-1,1):'~'}/eg;
    return $tmp;

}

sub getConsensus {
    my $con = shift;
    my ($source, $featuresRef) = each %$con;
    return $featuresRef->[0]->{'feature_label'};

}




1;
