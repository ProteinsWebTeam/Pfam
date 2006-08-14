
# AlignmentGenerator.pm
# jt6 20060601 WTSI
#
# ?
#
# $Id: AlignmentGenerator.pm,v 1.4 2006-08-14 10:43:45 jt6 Exp $

package PfamWeb::Controller::Family::AlignmentGenerator;

use strict;
use warnings;

use Data::Dumper;
use Bio::Pfam::ColourAlign;

use base "PfamWeb::Controller::Family";

# pick up a URL like http://localhost:3000/family/alignmentgenerator?acc=PF00067

sub getData : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # somewhere to dump the paging info
  $c->stash->{alignments} = {};

  my @dsnList = ( "http://pfam1b.internal.sanger.ac.uk:9000/das/pfamAlign" );

  # retrieve the DasLite client from the base model class and hand it
  # the list of DSNs
  my $dl = $c->model("PfamDB")->getDasLite;
  $dl->dsn( \@dsnList );

  # get the limits from the parameters
  my $rows;
  ( $rows ) = $c->req->param( "range" ) =~ m/^(\d+\-\d+)$/
	if defined $c->req->param( "range" );

  unless( defined $rows ) {
	$c->log->debug( "AlignmentGenerator::getData: rows undefined; calculating from start and end" );

	my( $start, $end );
	( $start ) = $c->req->param( "start" ) =~ m/^(\d+)$/
	  if defined $c->req->param( "start" );
	( $end   ) = $c->req->param( "end"   ) =~ m/^(\d+)$/
	  if defined $c->req->param( "end" );

	$rows = $start . "-" . $end if( defined $start and defined $end );
  }

  $rows = $this->{defaultRows} unless( defined $rows && $rows =~ /^\d+\-\d+$/ );
  $c->log->debug( "AlignmentGenerator::getData: rows finally set to: |$rows|" );

  # store the start and end of the range
  $rows =~ m/^(\d+)\-(\d+)$/;
  $c->stash->{alignments}->{start} = $1;
  $c->stash->{alignments}->{end}   = $2;

  # store the scroll position
  if( defined $c->req->param( "scrollValue" ) ) {
	$c->req->param( "scrollValue" ) =~ /^(\d+)$/;
	$c->stash->{scroll} = $1;
	$c->log->debug( "AlignmentGenerator::getData: set scroll value to |" . $c->stash->{scroll} . "|" );
  }

  my $rawAlignment = $dl->alignment( { query => $c->stash->{pfam}->pfamA_acc . ".full",
									   rows  => $rows
									 } );
  my $features     = $dl->features( $c->stash->{pfam}->pfamA_acc . ".full" );
  my $consensus    = Bio::Pfam::ColourAlign::parseConsensus( getConsensus( $features ) );

  my( $alignments, $alignmentLengths ) = reconstructAli( $rawAlignment );
  my @markedUpAlignments;
   foreach my $alignment ( @$alignments ) {
 	push @markedUpAlignments, Bio::Pfam::ColourAlign::markupAlignSeparate( $alignment, $consensus );
   }

  $c->stash->{alignments}->{alignments} = \@markedUpAlignments;
  $c->stash->{alignments}->{lengths}    = $alignmentLengths;

}

#-------------------------------------------------------------------------------
# override the end method from the Family class, so that we now hand
# off to a template that doesn't require the wrapper

sub end : Private {
  my( $this, $c ) = @_;

  # not sure we need this check here, since it's being done in the
  # default method already
  #return unless defined $c->stash->{pfam};

  $c->stash->{template} = "components/blocks/family/alignmentFragment.tt";

  # forward to the class that's got the WRAPPER set to null
  $c->forward( "PfamWeb::View::TTBlock" );

}

#-------------------------------------------------------------------------------

sub reconstructAli {
  my $ali = shift;
  my ($source, $aliData) = each %$ali;

  my( @alignments, @alignmentLengths );
  #print "$source, $aliData\n";
  for (my $i = 0; $i < scalar(@$aliData); $i++) {
	my %aliObjects = map{$_->{'alignobject_intObjectId'} => $_} @{$aliData->[$i]->{'alignobject'}};
	push @alignmentLengths, $aliData->[$i]->{alignment_max};
	foreach my $block (sort{$a->{'block_blockOrder'} <=> $b->{'block_blockOrder'}} @{$aliData->[$i]->{'block'}}) {
	  my %ali;
	  foreach my $bseqRef (@{$block->{'segment'}}) {
		$ali{$bseqRef->{'segment_intObjectId'}."/".$bseqRef->{'segment_start'}."-".$bseqRef->{'segment_end'}} = &getAliString($bseqRef, \%aliObjects); 
		#printf("%-20s %s\n", $bseqRef->{'segment_intObjectId'}."/".$bseqRef->{'segment_start'}."-".$bseqRef->{'segment_end'}, $aliString);
	  }
	  push(@alignments, \%ali);
	}
  }
  return \@alignments, \@alignmentLengths;
}

# an attempt to re-write the previous method to be a little more
# efficient. It's so efficient it doesn't work. Remove once the class
# has been checked in once...
sub reconstructAlignment {
  my( $alignmentData, $consensus ) = @_;

  # get the data for the single alignment out of the junk that we're passed...
  my $alignment = ( values %$alignmentData )[0]->[0];
  print STDERR "alignment: " . Dumper( $alignment ) . "\n";

  # convert an array of alignment rows into a hash, which the uniprot
  # ID as the key and the alignment row hash as the value
  my %aliObjects = map { $_->{alignobject_intObjectId} => $_ } @{ $alignment->{alignobject} };

  # walk over the alignment and apply the cigar strings
  my( @alignments, $key );
  foreach my $block ( sort { $a->{block_blockOrder} <=> $b->{block_blockOrder} }
					  @{$alignment->{block}} ) {
	my %ali;
	foreach my $bseqRef (@{ $block->{segment} } ) {
	  $key = $bseqRef->{segment_intObjectId} . "/" .
		     $bseqRef->{segment_start} . "-" .
		     $bseqRef->{segment_end};
	  $ali{$key} = getAliString( $bseqRef, \%aliObjects );
	}
	print STDERR "parsed alignment: " . Dumper( \%ali ) . "\n";
	push @alignments, Bio::Pfam::ColourAlign::markupAlign( \%ali, $consensus );
  }
  return \@alignments;
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
