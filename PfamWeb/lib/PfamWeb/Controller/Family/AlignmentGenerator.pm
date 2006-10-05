
# AlignmentGenerator.pm
# jt6 20060601 WTSI
#
# $Id: AlignmentGenerator.pm,v 1.7 2006-10-05 12:49:55 rdf Exp $

=head1 NAME

PfamWeb::Controller::Family::AlignmentGenerator - controller to
display sequence alignments in various formats

=cut

package PfamWeb::Controller::Family::AlignmentGenerator;

=head1 DESCRIPTION

A sprawling class that handles the generation and display of sequence
alignments. There's a "viewer" which allows interactive generation of
the alignment from DAS sources, plus a few different display options.

Generates a B<full page>.

$Id: AlignmentGenerator.pm,v 1.7 2006-10-05 12:49:55 rdf Exp $

=cut

use strict;
use warnings;

use Data::Dumper;
use Bio::Pfam::ColourAlign;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 showAlignment : Path

Hands straight off to a template that generates a "tool" page
containing the necessary hooks to load the interactive alignment
viewer.

Picks up a URL like http://localhost:3000/family/showalignments?acc=PF00067

=cut

sub showAlignment : Path( "/family/showalignment" ) {
  my( $this, $c ) = @_;

  $c->stash->{template} = "components/blocks/family/alignmentTool.tt";
}

#-------------------------------------------------------------------------------

sub downloadAlignment : Path( "/family/downloadalignment" ) {
  my( $this, $c ) = @_;
  my $acc = $c->stash->{pfam}->pfamA_acc;
  #Need to find alignment type and family accession.

  #first get the alignment filehandle.......
  my $fh = $c->forward("getAlignFile", $style );

  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $fh );
  };
  if($@){
    $pfamaln->throw("Error reading stockholm file:[$@]");
  };


  #gaps param can be default, dashes, dot or none 
  if($c->req->params("gaps") eq "none"){
    $pfamaln->map_chars('-', '');
    $pfamaln->map_chars('\.', '');
  }elsif($c->req->params("gaps") eq "dots"){
    $pfamaln->map_chars('-', '.');
  }elsif($c->req->params("gaps") eq "dashes"){
    $pfamaln->map_chars('\.', '-');
  }

  #case param can be u or l
  if($c->req->params("case") eq "u"){
    $pfamaln->uppercase;
  }

  #order param can be tree or alphabetical
  if($c->req->params("order") eq "alpha"){
    $pfamaln->sort_alphabetically;
  }

  #Format param can be one of pfam, stockholm, fasta or MSF
  if($c->req->params(format) eq "stockholm"){
    $pfamaln->write_stockholm(\*OUT);
  }elsif($c->req->params(format) eq "pfam"){
    $pfamaln->write_Pfam(\*OUT);
  }elsif($c->req->params(format) eq "fasta"){
    $pfamaln->write_fasta(\*OUT);
  }elsif($c->req->params(format) eq "msf"){
    $pfamaln->write_MSF(\*OUT);
  }


  foreach my $param ( sort keys %{ $c->req->params } ) {
	$c->log->debug( "|$param| => |" . $c->req->param( $param ) . "|" );
  }

  $c->stash->{template} = "components/blocks/family/alignmentTool.tt";
}

#-------------------------------------------------------------------------------

=head2 getData : Private

Generates a segment of a sequence alignment. Retrieves the alignment
from DAS sources and determines start/end row numbers from parameters.

Picks up a URL like http://localhost:3000/family/alignmentgenerator?acc=PF00067

=cut

sub default : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # somewhere to dump the paging info
  $c->stash->{alignments} = {};

  # retrieve the DasLite client from the base model class and hand it
  # the list of DSNs
  my $dl = $c->model("PfamDB")->getDasLite;
  $dl->dsn( [ qw|http://pfam1b.internal.sanger.ac.uk:9000/das/pfamAlign| ] );

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
  if ( defined $c->req->param( "scrollValue" ) ) {
	$c->req->param( "scrollValue" ) =~ /^(\d+)$/;
	$c->stash->{scroll} = $1;
	$c->log->debug( "AlignmentGenerator::getData: set scroll value to |"
					. $c->stash->{scroll} . "|" );
  }

  my $rawAlignment = $dl->alignment( { query => $c->stash->{pfam}->pfamA_acc . ".full",
									   rows  => $rows
									 } );
  my $features     = $dl->features( $c->stash->{pfam}->pfamA_acc . ".full" );
  my $consensus    = Bio::Pfam::ColourAlign::parseConsensus( getConsensus( $features ) );

  my( $alignments, $alignmentLengths ) = reconstructAli( $rawAlignment );
  my @markedUpAlignments;
  foreach my $alignment ( @$alignments ) {
 	push @markedUpAlignments,
	  Bio::Pfam::ColourAlign::markupAlignSeparate( $alignment, $consensus );
  }

  $c->stash->{alignments}->{alignments} = \@markedUpAlignments;
  $c->stash->{alignments}->{lengths}    = $alignmentLengths;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = "components/blocks/family/alignmentFragment.tt";

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

# reconstructs a blocked alignment from a raw alignment

sub reconstructAli {
  my $ali = shift;
  my ($source, $aliData) = each %$ali;

  my( @alignments, @alignmentLengths );

  for (my $i = 0; $i < scalar(@$aliData); $i++) {
	my %aliObjects = 
	  map{ $_->{alignobject_intObjectId} => $_ } @{ $aliData->[$i]->{alignobject} };

	push @alignmentLengths, $aliData->[$i]->{alignment_max};

	foreach my $block ( sort { $a->{block_blockOrder} <=> $b->{block_blockOrder} }
						@{$aliData->[$i]->{block} } ) {
	  my %ali;
	  foreach my $bseqRef (@{ $block->{segment} } ) {

		my $key = $bseqRef->{segment_intObjectId}
		  . "/" . $bseqRef->{segment_start}
		  . "-" . $bseqRef->{segment_end};

		$ali{$key} = &getAliString($bseqRef, \%aliObjects);

	  }

	  push @alignments, \%ali;
	}
  }

  return \@alignments, \@alignmentLengths;
}

#-------------------------------------------------------------------------------
# get the alignment string from the alignment

sub getAliString{
  my( $bseqRef, $aliObjectsRef ) = @_;

  my $seqStr = $aliObjectsRef->{ $bseqRef->{segment_intObjectId} }->{sequence};

  my $seq = substr( $seqStr,
					$bseqRef->{segment_start} - 1,
					$bseqRef->{segment_end} - $bseqRef->{segment_start} + 1 );

  my $aliString = cigar2align( $bseqRef->{cigar}, $seq );

  return $aliString;
}

#-------------------------------------------------------------------------------
# convert a cigar string into an alignment row

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

#-------------------------------------------------------------------------------
# calculate the consensus sequence

sub getConsensus {
  my $con = shift;

  my( $source, $featuresRef ) = each %$con;
  return $featuresRef->[0]->{feature_label};
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
