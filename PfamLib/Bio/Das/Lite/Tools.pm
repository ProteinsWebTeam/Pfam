package Bio::Das::Lite::Tools;
use strict;
use warnings;
use base qw( Exporter );
our @EXPORT_OK = qw(
	convertSegmentsToFeatures convertSegmentToFeature extractSegmentsFromAlignment
	getMappingsForAlignment convertCigarAlignment convertCigarBlocks convertCigarBlock
	cigar_to_gapped_sequence gapped_sequence_to_cigar convertFeatures convertFeature
);

=head1 METHODS

=head2 convertSegmentsToFeatures

Copies many alignment segments, and converts the data structures to represent features.

See: L<convertsegmenttofeature>

Requires either:
=over 8
=item A list of segment data structures
=item A reference to an array of segment data structures
=back

Returns either (depending on array or scalar context):
=over 8
=item A reference to an array of feature data structures
=item A list of feature data structures
=back
=cut

# $Author: aj5 $

sub convertSegmentsToFeatures
{
	my $segments;
	if (!ref $_[0]) {
		die "Unsupported argument to convertSegmentsToFeatures";
	} elsif (ref $_[0] eq 'ARRAY') {
		$segments = $_[0];
	} elsif (ref $_[0] eq 'HASH') {
		$segments = \@_;
	}
	
	my @features = ();
	foreach my $segment (@$segments) {
		my $feature = &convertSegmentToFeature($segment);
		push (@features, $feature);
	}
	return wantarray ? @features : \@features;
}

=head2 convertSegmentToFeature

Copies a single alignment segment, and converts the data structure to represent a feature.

Requires:
=over 8
=item A reference to a segment data structure
=back

Returns:
=over 8
=item A reference to a feature data structure
=back
=cut
sub convertSegmentToFeature
{
	my ($segment) = @_;
	my %feature = %{ $segment };
	$feature{start} = $feature{segment_start};
	$feature{end} = $feature{segment_end};
	$feature{segment_id} = $feature{segment_intObjectId};
	my $label = "$feature{segment_id}\[$feature{start}-$feature{end}\]";
	$feature{feature_id} = $label;
	$feature{feature_label} = $label;
	delete $feature{segment_start};
	delete $feature{segment_end};
	delete $feature{segment_intObjectId};
	return \%feature;
}

=head2 extractSegmentsFromAlignment

Iterates over the blocks within an alignment and collates the segments.
If an object ID is specified, only objects with that internal ID are collated.

Requires either:
=over 8
=item A reference to an alignment data structure
=item A reference to an array of block data structures
=back

Optional:
=over 8
=item An internal object ID to use as a filter. Only segments with the same ID will be collated.
=back

Returns either (depending on array or scalar context):
=over 8
=item A reference to an array of segment data structures
=item A list of segment data structures
=back
=cut
sub extractSegmentsFromAlignment
{
	my ($data, $intObIdFilter) = @_;
	
	my $blocks;
	my $refType = ref $data;
	if ($refType eq 'ARRAY') {
		$blocks = $data;
	} elsif ($refType eq 'HASH') {
		$blocks = $data->{block};
	} else {
		die "Unsupported argument type".($refType?" '$refType' ":" ")."to extractSegmentsFromAlignment";
	}
	
	my @collatedSegments = ();
    foreach my $block (@$blocks) {
        foreach my $segment (@{$block->{segment}}) {
            my $id = $segment->{segment_intObjectId};
			next if (defined $intObIdFilter && $intObIdFilter ne $id);
            push(@collatedSegments, $segment);
        }
    }
	
	return wantarray ? @collatedSegments : \@collatedSegments;
}

=head2 getMappingsForAlignment

Maps all equivalent positions between the alignment objects.
Iterates over the blocks within an alignment and uses the segments to determine matching regions.
Note that the 'block' property of the alignment will be altered in order to rationalise any 'cigar'
format segments within the alignment. See L<convertcigaralignment>.

Requires:
=over 8
=item A reference to an alignment data structure
=back

Returns:
=over 8
=item A reference to a nested hash data structure, accessed thus: C<< $pos2 = $hash->{$obId1}->{$obId2}->{$pos1} >>
=back
=cut
sub getMappingsForAlignment
{
    my ($alignment) = @_;
    
    my $map;
    
	&convertCigarAlignment($alignment);

	# Each block is a block of paired equal length
    foreach my $block (@{$alignment->{block}})
    {
        foreach my $segment (@{$block->{segment}})
        {
            my $obId = $segment->{segment_intObjectId};
			#  Segment positions, relative its parent sequence.
			my $segStart = $segment->{segment_start};
			my $segEnd = $segment->{segment_end};
			my $segLen = $segEnd - $segStart + 1;
			
    	    foreach my $otherSegment (@{$block->{segment}})
	        {
    	        my $othId = $otherSegment->{segment_intObjectId};
    	        next if ($othId eq $obId); # Don't map a segment to itself.
				
				my $othStart = $otherSegment->{segment_start};
				my $othEnd = $otherSegment->{segment_end};
				my $othLen = $othEnd - $othStart + 1;
				die "Segments $obId and $othId are in the same alignment block but different lengths." if ($segLen != $othLen);
				
				# Map between the segments for every position.
				for (my $i=0; $i<$segLen; $i++) {
					$map->{$obId}{$othId}{$i+$segStart} = ($i+$othStart);
				}
	        }
        }
    }
    
    return $map;
}

=head2 convertCigarAlignment

Rationalises any 'cigar' format segments within an alignment.
The existing blocks within the alignment will be replaced with blocks of equal length,
each containing positionally-equivalent segments for the alignment objects.
Segments of objects that are not equivalent to segments in any other objects are
represented by blocks containing only one segment.

See L<convertcigarblocks>.

Requires:
=over 8
=item A reference to an alignment data structure
=back

Returns:
=over 8
=item Nothing.
=back
=cut
sub convertCigarAlignment
{
	my ($alignment) = @_;
	my $newBlocks = &convertCigarBlocks($alignment->{block});
	$alignment->{block} = $newBlocks;
}

=head2 convertCigarBlocks

Converts a set of blocks with cigar format segments into a new set of blocks.
Each new block will represent a section of sequence containing matching segments of equal length.

See L<convertcigarblock>.

Requires either:
=over 8
=item A reference to an array of block data structures
=item An array of block data structures
=back

Returns either (depending on array or scalar context):
=over 8
=item A reference to an array of block data structures
=item A list of block data structures
=back
=cut
sub convertCigarBlocks
{
	my @args = @_;
	return undef if (!@args);
	
	my $oldBlocks;
	if (ref $args[0] eq 'HASH') {
		$oldBlocks = \@args;
	}
	elsif (ref $args[0] eq 'ARRAY') {
		$oldBlocks = $args[0];
	}
	else {
		die "Unsupported argument type to convertCigarBlocks";
	}
	
	my @newBlocks = ();
	foreach my $oldBlock (@$oldBlocks) {
		my $splitBlocks = &convertCigarBlock( $oldBlock, scalar (@newBlocks) );
		push ( @newBlocks, @{ $splitBlocks } );
	}
	
	return wantarray ? @newBlocks : \@newBlocks;
}

=head2 convertCigarBlock

Converts a block with cigar format segments into a new set of blocks.
Each new block will represent a section, containing one or more matching segments of equal length.

See: L<_expand_cigar>

Requires:
=over 8
=item A reference to a block data structure
=back

Optional:
=over 8
=item An offset to apply to the block order of all new blocks
=back

Returns either (depending on array or scalar context):
=over 8
=item A reference to an array of block data structures
=item A list of block data structures
=back
=cut
sub convertCigarBlock
{
	my ($block, $blockOrderOffset) = @_;
	my ($seqLen, %seqs, %positions);
	
    foreach my $segment (@{$block->{segment}})
    {
    	my $obId = $segment->{segment_intObjectId};
		if (!defined $obId) {
			warn "Segment does not have an object ID; skipping cigar block conversion";
		    return wantarray ? ( ) : [ ];
		}
		my $segStart = $segment->{segment_start};
		$segStart = 1 if (!defined $segStart); # Assume beginning of sequence if not specified
		
    	my ($seq, $thisLen);
    	if (defined $segment->{cigar}) {
    		$seq = &_expand_cigar($segment->{cigar});
	    	$thisLen = length($seq);
    	}
    	else {
			if (!$segment->{segment_end}) {
				warn "Segment does not have an end position; skipping cigar block conversion";
			    return wantarray ? ( ) : [ ];
			}
    		$thisLen = $segment->{segment_end}-$segment->{segment_start}+1;
    		$seq = 'X'x$thisLen;
    	}
    	
    	if (defined $seqLen && $seqLen != $thisLen) {
    		warn "Segments in alignment block not of equal length for $obId";
			return wantarray ? ( ) : [ ];
    	}
    	if (exists $positions{$obId}) {
    		warn "Duplicate segment for $obId in alignment block";
			next;
    	}
		
    	$positions{$obId} = $segStart;
    	$seqLen = $thisLen;
    	my @chars = split //, $seq;
    	$seqs{$obId} = \@chars;
    }
    
    my @newBlocks = ( );
    my $currentSegments = [ ];
    my $blockLen = 0;
    
    my @ids = keys %seqs;
    for (my $i=0; $i<=$seqLen; $i++) # Go one past the end, to make sure we stop.
    {
    	my (%stopping, %starting, @matching);
    	foreach my $obId (@ids)
    	{
    		my $thisC = $seqs{$obId}->[$i];
    		my $prevC = $seqs{$obId}->[$i-1];
    		$thisC = '-' if ($i == $seqLen); # Position 'end+1'
    		$prevC = '-' if ($i == 0);		 # Position 'start-1' (have to be careful as arrays are circular)
    		
    		# If the state is a match, extend the match position.
    		if ($thisC ne '-') {
    			push(@matching, $obId);
    		}
    		
    		# If the state has changed from gap to match, begin a new block
    		if ($prevC eq '-' && $thisC ne '-') {
    			$starting{$obId} = 1;
    		}
    		# If the state has changed from match to gap, end the block
    		elsif ($prevC ne '-' && $thisC eq '-') {
    			$stopping{$obId} = 1;
    		}
    	}
    	
    	# If the block contents are changing (i.e. anything is stopping or starting), end the block if present.
    	if (%stopping || %starting) {
	    	if ($blockLen) {
    			foreach (@$currentSegments) {
    				$_->{segment_end} = $_->{segment_start} + $blockLen - 1;
    				$positions{$_->{segment_intObjectId}} += $blockLen;
    			}
				my %newBlock = %{ $block };
				$newBlock{segment} = $currentSegments;
				$newBlock{block_blockOrder} = scalar(@newBlocks) + $blockOrderOffset + 1;
    			push (@newBlocks, \%newBlock);
    			# If there are still some matching, we will need to start a new block for them.
    			foreach (@matching) {
    				$starting{$_} = 1;
    			}
	    		$blockLen = 0;
    		}
    	}
    	
    	# If we have matches, extend or begin a block
    	if (@matching) {
    		$blockLen++;
    		if (%starting) {
    		$currentSegments = [ ];
    			foreach (@matching) {
    				push (@$currentSegments, {segment_start=>$positions{$_}, segment_intObjectId=>$_} );
    			}
	    	}
    	}
    }
    
    return wantarray ? @newBlocks : \@newBlocks;
}

=head2 _expand_cigar

Rationalises a cigar string into a series of 'X' or '-' characters.

Example:
=over 8
=item 10M5IM -> XXXXXXXXXX-----X
=back
=cut
sub _expand_cigar  {
    my ($cigar) = @_;

    $cigar =~ s/\"//g; # Get rid of quotes

    my $tmp = $cigar;
    $tmp =~ s/(\d+)M/'X'x$1/eg;
    $tmp =~ s/M/X/g;
    $tmp =~ s/(\d+)I/'-'x$1/eg;
    $tmp =~ s/I/\-/g;
    $tmp =~ s/(\d+)D/'-'x$1/eg;
    $tmp =~ s/D/\-/g;

    return $tmp;
}

# convert a cigar string into an alignment row
# Based on PfamWeb::Controller::Family::AlignmentGenerator
sub cigar_to_gapped_sequence  {
    my ($cigar, $seq) = @_;

    $cigar =~ s/\"//g; # Get rid of quotes

    my $tmp = $cigar;
    my $start = 0;
    my $len = length($seq);

    $tmp =~ s/(\d+)D/'-'x$1/eg;
    $tmp =~ s/D/\-/g;
#    $tmp =~ s/(\d+)I/'.'x$1/eg;
    $tmp =~ s/(\d+)I/'-'x$1/eg;
#    $tmp =~ s/I/\./g;
    $tmp =~ s/I/\-/g;

    $tmp =~ s/(\d{0,5})M/if($1){$start+=$1,($start<=$len) ? substr($seq, $start-$1,$1) : '~'x$1}else{$start+=1,($start<=$len)?substr($seq,$start-1,1):'~'}/eg;

  return $tmp;
}

sub gapped_sequence_to_cigar
{
    my ($str) = @_;
    chomp($str);
    my @chars = split //, $str;

    my $count_for_cigar_string = 0;
    my $state_for_cigar_string = 'M';
    my $cigar_string = '';
    foreach my $char (@chars){
        
        my $new_state;
        if ($char eq '-') {
            $new_state = 'I'; # gap
        } else {
            $new_state = 'M'; # match
        }

        if ($new_state ne $state_for_cigar_string){
			# If changing state, append the previous section (if pending) to the cigar.
            if ($count_for_cigar_string) {
        		$cigar_string .= $count_for_cigar_string unless $count_for_cigar_string == 1;
				$cigar_string .= $state_for_cigar_string;
            }
            $count_for_cigar_string = 0;
			$state_for_cigar_string = $new_state;
        }
        $count_for_cigar_string++;
    }

	# At the end, append the previous section (if pending) to the cigar.
    if ($count_for_cigar_string){
        $cigar_string .= $count_for_cigar_string unless $count_for_cigar_string == 1;
		$cigar_string .= $state_for_cigar_string;
    }

    return $cigar_string;
}

# Copies features, converting co-ordinates and filtering any that don't map.
sub convertFeatures
{
	my ($features, $mapFromTo) = @_;
	my @newFeatures = ( );
	
	die "No mappings available.\n" unless defined $mapFromTo;
	my ($keptHash, $returnMe);
	if (ref $features eq 'ARRAY') {
		$returnMe = [ ];
		$features = { 'dummy' => $features };
		$keptHash = { 'dummy' => $returnMe };
	}
	elsif (ref $features eq 'HASH')
	{
		$returnMe = $keptHash = { };
	}
	else {
		die "Can only convert hash or array references";
	}
	
	foreach my $key (keys %$features)
	{
		my $arr = $features->{$key};
		next unless (ref $arr eq 'ARRAY');
		my $keptArr = $keptHash->{$key};
		if (!defined $keptArr)
		{
			$keptArr = [ ];
			$keptHash->{$key} = $keptArr;
		}
		
	    # Transpose each feature onto the target sequence.
    	foreach my $feature (@$arr)
	    {
			my $newFeature = &convertFeature($feature, $mapFromTo);
			# Store an altered copy of the mutation.
			push (@$keptArr, $newFeature) if defined $newFeature;
		}
    }
	
	return $returnMe;
}

# Copies a feature and converts its co-ordinates.
# Returns undef if the feature doesn't map.
sub convertFeature
{
	my ($feature, $mapFromTo) = @_;
	
    my $featureId = $feature->{feature_id};
    my $segmentId = $feature->{segment_id};
    my ($start, $stop) = ($feature->{start}, $feature->{end});
	
	# Convert to original query frame of reference.
    my ($newStart, $newStop) = ($mapFromTo->{$start}, $mapFromTo->{$stop});

    # Skip features that are completely outside the target sequence.
    if (!defined $newStart && !defined $newStop)
    {
        return undef;
    }
    # If the end maps but not the start, walk towards the end until it does.
    elsif (!defined $newStart)
    {
		for ($start=$start+1; $start<=$stop; $start++)
		{
			$newStart = $mapFromTo->{$start};
			last if (defined $newStart);
		}
    }
    # If the start maps but not the end, walk towards the start until it does.
    elsif (!defined $newStop)
    {
		for ($stop=$stop-1; $stop>=$start; $stop--)
		{
			$newStop = $mapFromTo->{$stop};
			last if (defined $newStop);
		}
    }

	if ($newStop < $newStart || !$newStart || !$newStop)
	{
		warn "BAD FEATURE for $segmentId: $featureId (start=$newStart,end=$newStop)";
		return undef;
	}
	
	my %newFeature = %{ $feature };
	$newFeature{start} = $newStart;
	$newFeature{end} = $newStop;
	return \%newFeature;
}

# Rebuilds an alignment from its objects, blocks and segments, and calculates mappings between the objects.
# Attempts to rebuild using cigar strings.
sub getMappingsForAlignmentOld
{
    my ($alignment) = @_;

    my %segments;
	
	# Each block is a block of paired equal length
    foreach my $block (@{$alignment->{block}})
    {
        # Collate all segments for each object.
        foreach my $segment (@{$block->{segment}})
        {
            my $id = $segment->{segment_intObjectId};
            $segments{$id} = ( ) unless (defined $segments{$id});
            push(@{ $segments{$id} }, $segment);
        }
    }
	
	my ($aln_to_ref, $ref_to_aln, $ob_to_ob);

    # Build mappings from each sequence object to the 'consensus'.
    OBJECT: foreach my $object (@{ $alignment->{alignobject} })
    {
        my $id = $object->{alignobject_intObjectId};
        my $refSeq = $object->{sequence};

        unless (defined $segments{$id})
        {
            warn "Skipping alignment object $id as no segments refer to it.\n";
            next OBJECT;
        }

        # Set temporary co-ordinates for this object to and from the 'consensus' (by processing each segment).
        SEGMENT: foreach my $segment (@{ $segments{$id} })
        {
            my ($segStart, $segStop) = ($segment->{segment_start}, $segment->{segment_end});
			unless (($segStart && $segStop) && ($segStart <= $segStop))
			{
				warn "Bad segment for $id:$segStart,$segStop\n";
				next SEGMENT;
			}
            # The region of the sequence the cigar string applies to.
            my $segSeq = substr ( $refSeq, $segStart-1, $segStop-$segStart+1 );
			
            # Walk through the reference (object) sequence, and find the corresponding indices in the 'consensus'.
			# Note that regions of the alignment that aren't covered by a segment will be 'skipped', and this will
			# be reflected in the intermediate indices; best thought of as gapped regions that are chopped out of
			# the consensus.
            my @segChars = split(//, $segSeq);
			my $refSeqLen = length($refSeq);
            CHAR: for (my ($refIndex, $segIndex) = (1, $segStart); $refIndex<= $refSeqLen; $refIndex++,$segIndex++)
            {
                while ($segChars[$segIndex-1] eq '-')
                {
                	$segIndex++;
                }
				
				if (!defined $segChars[$segIndex-1])
				{
					# Shouldn't get to the end in gap mode, because reference sequence should end first.
					warn "Bad alignment encountered at position $refIndex for $id.\n";
					delete $ref_to_aln->{$id};
					delete $aln_to_ref->{$id};
					next OBJECT;
				}
				my $alnIndex = $segIndex + $segStart - 1;
				my $currAln = $ref_to_aln->{$id}{$refIndex};
				my $currRef = $aln_to_ref->{$id}{$alnIndex};
				if ((defined $currAln && $currAln != $alnIndex) or (defined $currRef && $currRef != $refIndex))
				{
					warn "Alignment contains conflicting segments at position $refIndex for $id.\n";
				}

                $ref_to_aln->{$id}{$refIndex} = $alnIndex;
                $aln_to_ref->{$id}{$alnIndex} = $refIndex;
            }
        }
	}
	
    foreach my $object (@{ $alignment->{alignobject} })
	{
        my $id = $object->{alignobject_intObjectId};
        next unless (defined $ref_to_aln->{$id});
		
        # Set co-ordinates for this object to each other object in the alignment.
        foreach my $otherObject (@{ $alignment->{alignobject} })
        {
            my $otherId = $otherObject->{alignobject_intObjectId};
            next if ($otherId eq $id); # Don't map to the same object as that's silly.
            next unless (defined $aln_to_ref->{$otherId});

            # Set co-ordinates for each position.
            foreach my $refIndex (keys %{ $ref_to_aln->{$id} })
            {
                my $alignIndex = $ref_to_aln->{$id}{$refIndex};
                my $otherIndex = $aln_to_ref->{$otherId}{$alignIndex};

                $ob_to_ob->{$id}{$otherId}{$refIndex} = $otherIndex;
                
                warn "$id -> $otherId : $refIndex - $otherIndex\n";
            }
        }
    }
	
	return $ob_to_ob;
}

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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


1;

