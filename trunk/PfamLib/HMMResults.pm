
#
# Perl Module for HMMResults
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package HMMResults;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use warnings;
use HMMUnit;
use HMMSequence;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' );


my %fields = (
    #Insert field names here as field => undef,
	      domain => undef, # will be an array of HMMUnit
	      seq => undef,    # will be a hash on name of HMMSequence
	      hmmname => undef
);


sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	'_permitted' => \%fields,
	%fields, };

    $self->{'domain'} = []; # array of HMMUnits
    $self->{'seq'} = {};
    bless $self, $class;
    return $self;
}


sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) || carp "$self is not an object - can't therefore find a member!";
    my $name = $AUTOLOAD;
    $name =~ /::DESTROY/ && return;
    $name =~ s/.*://;
    unless (exists $self->{'_permitted'}->{$name} ) {
	carp "In type $type, can't access $name - probably passed a wrong variable into HMMResults";
    }
    if (@_) {
	return $self->{$name} = shift;
    } else {
	return $self->{$name};
    }
}

sub number {
    my $self = shift;
    my $val;
    
    $val = @{$self->{'domain'}};
    return $val;
}

sub addHMMUnit {
    my $self = shift;
    my $unit = shift;
    my $name;

    $name = $unit->seqname();

    if( ! exists $self->{'seq'}->{$name} ) {
	carp "Adding a domain of $name but with no HMMSequence. Will be kept in domain array but not added to a HMMSequence";
    } else {
	$self->{'seq'}->{$name}->addHMMUnit($unit);
    }
    push(@{$self->{'domain'}},$unit);
}

sub deleteHMMUnit {

  my $self = shift;
  my $unit = shift;
  my $name;

  my $count = 0;
  my $unit_count;

  foreach my $stored_unit (@{$self->{'domain'}}) {
    if ($unit eq $stored_unit) {
      $unit_count = $count;
    }
    $count++;
  }
   splice(@{$self->{'domain'}}, $unit_count, 1);
}


sub eachHMMUnit {
    my $self = shift;
    my (@arr,$u);

    foreach $u ( @{$self->{'domain'}} ) {
	push(@arr,$u);
    }

    return @arr;
}

sub addHMMSequence {
    my $self = shift;
    my $seq  = shift;
    my $name;

    $name = $seq->name();
    if( exists $self->{'seq'}->{$name} ) {
	carp "You already have $name in HMMResults. Replacing by a new entry!";
    }

    $self->{'seq'}->{$name} = $seq;
}

sub eachHMMSequence {
    my $self = shift;
    my (@array,$name);

    foreach $name ( keys %{$self->{'seq'}} ) {
	push(@array,$self->{'seq'}->{$name});
    }

    return @array;
}

sub getHMMSequence {
    my $self = shift;
    my $name = shift;

    return $self->{'seq'}->{$name};
}

sub parse_hmmpfam {
    my $self = shift;
    my $file = shift;
    my $mode = shift;
    my ($id,$sqfrom,$sqto,$hmmf,$hmmt,$sc,$ev,$unit,$nd,$seq,$name,$seqname,$from,$to,%hash,%acc,$acc);
    my $count = 0;

    while(<$file>) {
	#print STDERR "$_";
	if( /^Query sequence:\s+(\S+)/ ) {
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

	    $seq = new HMMSequence;
	    $seq->name($seqname);
	    $self->addHMMSequence($seq);
	    %hash = ();
	    
	    while(<$file>){
		/^Parsed for domains/ && last;
		if( (($id,$acc, $sc, $ev, $nd) = /^\s*(\S+)\s+([A-Za-z]+\d+).+?\s(\S+)\s+(\S+)\s+(\d+)\s*$/)) {
		    $hash{$id} = $sc; # we need this for the sequence score of hte domains below!
		    $acc{$id} = $acc;
		} elsif ( (($id,$sc, $ev, $nd) = /^\s*(\S+).+?\s(\S+)\s+(\S+)\s+(\d+)\s*$/) ) {
		    $hash{$id} = $sc; # we need this for the sequence score of hte domains below!
		}
	    }

	    while(<$file>) {
		/^Align/ && last;
		
		# this is meant to match

		#Sequence Domain  seq-f seq-t    hmm-f hmm-t      score  E-value
		#-------- ------- ----- -----    ----- -----      -----  -------
		#PF00621    1/1     198   372 ..     1   207 []   281.6    1e-80

		if( (($id, $sqfrom, $sqto, $hmmf,$hmmt,$sc, $ev) = /(\S+)\s+\S+\s+(\d+)\s+(\d+).+?(\d+)\s+(\d+)\s+\S+\s+(\S+)\s+(\S+)\s*$/)) {
		    
		    
		    $unit = new HMMUnit;
		    $unit->seqname($seqname);
		    $unit->hmmname($id);
		    $unit->start_seq($sqfrom);
		    $unit->end_seq($sqto);
		    $unit->start_hmm($hmmf);
		    $unit->end_hmm($hmmt);
		    $unit->bits($sc);
		    $unit->evalue($ev);
		    $unit->mode($mode);
		    if( !exists($hash{$id}) ) {
			carp("HMMResults parsing error in hmmpfam for $id - can't find sequecne score");
		    }

		    $unit->seqbits($hash{$id});
	    
		    if( exists $acc{$id} ) {
			$unit->hmmacc($acc{$id});
		    }
		    
		    # this should find it's own sequence!
		    $self->addHMMUnit($unit);
		}
	    }
	    $_ = <$file>;


	    # parses alignment lines. Icky as we have to break on the same line
	    # that we need to read to place the alignment lines with the unit.

	    while(1) {
		/^\/\// && last;

		# matches:
		# PF00621: domain 1 of 1, from 198 to 372
		if( /^\s*(\S+):.*from\s+(\d+)\s+to\s+(\d+)/ ) {
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

		    $from = $2;
		    $to   = $3;

		    # find the HMMUnit which this alignment is from

		    $unit = $self->get_unit_nse($seqname,$name,$from,$to);
		    if( !defined $unit ) {
			carp("Could not find $name $from $to unit even though I am reading it in. ugh!");
			next;
		    }
		    while(<$file>) {
			/^\/\// && last;
			/^\s*\S+:.*from\s+\d+\s+to\s+\d+/ && last;
			$unit->add_alignment_line($_);
		    }
		} else {
		    $_ = <$file>;
		}
		    
	    }

	    # back to main 'Query:' loop
	}
    }
}


sub get_unit_nse {
    my $self = shift;
    my $seqname = shift;
    my $domname = shift;
    my $start = shift;
    my $end = shift;
    my($seq,$unit);
    
    $seq = $self->getHMMSequence($seqname);
    if( !defined $seq ) {
	carp("Could not get sequence name $seqname - so can't get its unit");
	return undef;
    }
    foreach $unit ( $seq->eachHMMUnit() ) {
	if( $unit->hmmname() eq $domname && $unit->start_seq() == $start &&  $unit->end_seq() == $end ) {
	    return $unit;
	}
    }

    return undef;
}

sub parse_decypher {
    my $self = shift;
    my $file = shift;
    my ($id,$sqfrom,$sqto,$sc,$ev,$unit,$nd,$seq,$hmmf,$hmmt,%seqh);
    my $count = 0;

    while(<$file>) {
        /^RANK       SCORES/ && last;
    }
    

    # Parse out sequence scores
    while(<$file>) {
	/^\[BEGIN ALIGNMENTS TIME\]/ && last;

	if( (($sc, $id, $ev) = /\d+\s+(-?\d+\.\d+)\s+1\s+(\S+)\s+\S+\s+\S+\s+(\S+)/)) {
	    $seq = new HMMSequence;
	    $seq->name($id);
	    $seq->bits($sc);
	    $seqh{$id} = $sc;
	    $seq->evalue($ev);
	    $self->addHMMSequence($seq);
	}
    }

    # Parse out domain scores
    while(<$file>) {

	/^\[END ALIGNMENTS TIME\]/ && last;
	if(/^ T = (\S+)/) {
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


	    # Multiple domains
	} elsif(/^Domain.*Score = (\S+)\s+E_Value =\s+(\S+)\s*$/) {
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

	    $ev=$2;
	} elsif(/^RANK.*Score =\s+(\S+)\s+E_Value =\s+(\S+)\s$/) { # These will be overwritten by multiple domain
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

	    $ev=$2;
	} elsif( (($hmmf, $hmmt,$sqfrom,$sqto) = /QS =\s+(\d+)\s+QE =\s+(\d+)\s+TS =\s+(\d+)\s+TE =\s+(\d+)\s+$/)) {
	    $unit = new HMMUnit;
	    $unit->seqname($id);
	    $unit->seqbits($seqh{$id});
	    $unit->start_seq($sqfrom);
	    $unit->bits($sc);
	    $unit->evalue($ev);
	    $unit->end_seq($sqto);
	    $unit->start_hmm($hmmf);
	    $unit->end_hmm($hmmt);
	    $self->addHMMUnit($unit);
	    $count++;
	}
    }
    
    return $count;
}



sub parse_HMMer2 {
    my $self = shift;
    my $file = shift;
    my %seqh;
    my $count = 0;

    while(<$file>) {
	/^Scores for complete sequences/ && last;
    }
    
    while(<$file>) {
	/^Parsed for domains/ && last;
	if( my( $id, $de, $sc, $ev) = /^(\S+)\s+(.*?)\s+(\S+)\s+(\S+)\s+\d+\s*$/ ) {
	    my $seq = new HMMSequence;
	    $seq->name($id);
	    $seq->desc($de);
	    $seq->bits($sc);
	    $seqh{$id} = $sc;
	    $seq->evalue($ev);
	    $self->addHMMSequence($seq);
	}
    }

    while(<$file>) {
	/^Histogram of all scores/ && last;
	if( my($id, $sqfrom, $sqto, $hmmf, $hmmt, $sc, $ev) = /^(\S+)\s+\S+\s+(\d+)\s+(\d+).+?(\d+)\s+(\d+)\s+\S+\s+(\S+)\s+(\S+)\s*$/ ) {
	    my $unit = new HMMUnit;

	    $unit->seqname($id);
	    $unit->start_seq($sqfrom);
	    $unit->end_seq($sqto);
	    $unit->bits($sc);
	    $unit->start_hmm($hmmf);
	    $unit->end_hmm($hmmt);
	    $unit->evalue($ev);
	    $unit->seqbits($seqh{$id});
	    $unit->hmmname("");
	    $self->addHMMUnit($unit);
	    $count++;
	}
    }
    
    return $count;
}

sub parse_HMMer1 {
    my $self = shift;
    my $file = shift;
    my %seqh;
    my $count = 0;

    while(<$file>) {
	if( my( $bits, $s, $e, $id ) = 
	    /^(-?\d+\.?\d*)\s+\(bits\)\s+f:\s+(\d+)\s+t:\s+(\d+)\s+Target:\s+(\S+)/ ) {
	    if( $id =~ /(\S+)\/(\d+)-(\d+)/ ) {
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

	    }

	    if( my $seq = $self->getHMMSequence( $id ) ) {
		if( $bits > $seq->bits ) {
		    $seqh{$id} = $bits;
		    $seq->bits( $bits );
		}
	    }
	    else {
		my $seq = new HMMSequence;
		$seq->name($id);
		$seq->bits($bits);
		$seqh{$id} = $bits;
		$self->addHMMSequence($seq);
	    }

	    my $unit = new HMMUnit;
	    $unit->seqname($id);
	    $unit->start_seq($s);
	    $unit->end_seq($e);
	    $unit->bits($bits);
#	    $unit->start_hmm($hmmf);
#	    $unit->end_hmm($hmmt);
#	    $unit->evalue($ev);
	    $unit->seqbits($seqh{$id});
	    $unit->hmmname("");
	    $self->addHMMUnit($unit);
	    $count++;
	}
    }
    
    return $count;
}


sub parse_pfam {
    my $self = shift;
    my $file = shift;
    my $count = 0;

    while(<$file>) {
	/^# Domain scores/ && last;
	if( my( $name, $desc, $bits, $eval ) = /^(\S+)\s+(.*?)\s+(\S+)\s+(\S+)\s+(\d+)\s*$/ ) {
	    my $seq = new HMMSequence;
	    $seq->name($name);
	    $seq->desc($desc);
	    $seq->bits($bits);
	    $seq->evalue($eval);
	    $self->addHMMSequence($seq);
	}
    }
    
    while(<$file>) {
	if( my( $name, $st, $en, $mst, $men, $bits, $eval ) = /^(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+(\S+)\s*$/ ) {
	    my $dom = new HMMUnit;
	    $dom->seqname($name);
	    $dom->start_seq($st);
	    $dom->end_seq($en);
	    $dom->start_hmm($mst);
	    $dom->end_hmm($men);
	    $dom->bits($bits);
	    $dom->evalue($eval);
	    $dom->seqbits( $self->getHMMSequence($name)->bits );
	    $self->addHMMUnit($dom);
	    $count ++;
	}
    }
    return $count;
}


sub domain_bits_cutoff_from_evalue {
    my $self = shift;
    my $eval = shift;
    my ($dom,$prev,@doms,$cutoff,$sep,$seen);

    @doms = $self->eachHMMUnit;

    @doms = sort { $b->bits <=> $a->bits } @doms;
    $seen = 0;
    foreach $_ ( @doms ) {
	if( $_->evalue > $eval ) {
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

	    $dom = $_;
	    last;
	} 
	$prev = $_;
    }
    
    if( ! defined $prev || $seen == 0) {
	carp("Evalue is either above or below the list...");
	return undef;
    }

    $sep = $prev->bits - $dom->bits ;
    
    if( $sep < 1 ) {
	return $prev->bits();
    }
    if( $dom->bits < 25 && $prev->bits > 25 ) {
	return 25;
    }

    return $dom->bits + sprintf("%.1f",$sep/2);
    
}


sub dictate_hmm_acc {
    my $self = shift;
    my $acc = shift;
    my ($unit);

	
    foreach $unit ( $self->eachHMMUnit() ) {
	$unit->hmmacc($acc);
    }	   
}


sub write_FT_output {
    my $self = shift;
    my $file = shift;
    my $idt  = shift;
    my ($seq,$unit);

    if( !defined $idt ) {
	$idt = "DOMAIN";
    }


    foreach $seq ( $self->eachHMMSequence() ) {
	print $file sprintf("ID   %s\n",$seq->name());
	foreach $unit ( $seq->eachHMMUnit() ) {
	    print $file sprintf("FT   %s   %d %d %s\n",$idt,$unit->start_seq,$unit->end_seq,$unit->hmmname);
	}
	print $file "//\n";
    }
}

sub filter_on_cutoff {
    my $self = shift;
    my $seqthr = shift;
    my $domthr = shift;
    my ($new,$seq,$unit,@array,@narray);

    if( !defined $domthr ) {
	carp("hmmresults filter on cutoff needs two arguments");
    }

    $new = new HMMResults;

    foreach $seq ( $self->eachHMMSequence()) {
	
	if( $seq->bits() < $seqthr ) {
	    next;
	}
	
#	my $newseq = HMMSequence->new();
#	$newseq->name($seq->name);
#	$newseq->bits($seq->bits);
#	$newseq->evalue($seq->evalue);

	$new->addHMMSequence($seq);
	
	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( $unit->bits() < $domthr ) {
		next;
	    }
	    $new->addHMMUnit($unit);
	}
    }
    return $new;
}



# writes as seq sstart send modelacc hstart hend modelname

sub write_ascii_out {
    my $self = shift;
    my $fh = shift;
    my ($unit,$seq);

    if( !defined $fh) {
	$fh = \*STDOUT;
    }


    foreach $seq ( $self->eachHMMSequence()) {
	foreach $unit ( $seq->eachHMMUnit()) {
	    print $fh sprintf("%s %4d %4d %s %4d %4d %7s %8s %s\n",$unit->seqname(),$unit->start_seq(),$unit->end_seq(),$unit->hmmacc,$unit->start_hmm,$unit->end_hmm,$unit->bits,$unit->evalue,$unit->hmmname);
	}
    }
	    
}

    
sub write_GDF_bits {
    my $self = shift;
    my $seqt = shift;
    my $domt = shift;
    my $file = shift;
    my $seq;
    my $unit;
    my (@array,@narray);

    if( !defined $file ) {
	carp("Attempting to use write_GDF_bits without passing in correct arguments!");
	return;
    }

    foreach $seq ( $self->eachHMMSequence()) {

	if( $seq->bits() < $seqt ) {
	    next;
	}

	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( $unit->bits() < $domt ) {
		next;
	    }
	    push(@array,$unit);
	}

    }

    @narray = sort { my ($aa,$bb,$st_a,$st_b); 
		     $aa = $a->seqname(); 
		     $bb = $b->seqname(); 
		     if ( $aa eq $bb) {
			 $st_a = $a->start_seq();
			 $st_b = $b->start_seq();
			 return $st_a <=> $st_b;
			 } 
		     else {
			 return $aa cmp $bb; 
		     } } @array;

    foreach $unit ( @narray ) {
	print $file sprintf("%-24s\t%6d\t%6d\t%15s\t%.1f\t%g\n",$unit->get_nse(),$unit->start_seq(),$unit->end_seq(),$unit->seqname(),$unit->bits(),$unit->evalue);
    }

}

sub write_scores_bits {
    my $self = shift;
    my $seqt = shift;
    my $domt = shift;
    my $file = shift;
    my $seq;
    my $unit;
    my (@array,@narray);

    if( !defined $file ) {
	carp("Attempting to use write_scores_bits without passing in correct arguments!");
	return;
    }

    foreach $seq ( $self->eachHMMSequence()) {

	if( $seq->bits() < $seqt ) {
	    next;
	}

	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( $unit->bits() < $domt ) {
		next;
	    }
	    push(@array,$unit);
	}

    }

    @narray = sort { my ($aa,$bb,$st_a,$st_b); 
		     $aa = $a->bits(); 
		     $bb = $b->bits(); 
		     return $aa <=> $bb; 
		     } @array;

    foreach $unit ( @narray ) {
	print $file sprintf("%4.2f %s\n",$unit->bits(),$unit->get_nse());
    }

}

sub write_GDF {
    my $self = shift;
    my $file = shift;
    my $unit;

    if( !defined $file ) {
	$file = \*STDOUT;
    }


    foreach $unit ( $self->eachHMMUnit() ) {
	print $file sprintf("%-24s\t%6d\t%6d\t%15s\t%.1f\t%g\n",$unit->get_nse(),$unit->start_seq(),$unit->end_seq(),$unit->seqname(),$unit->bits(),$unit->evalue);
    }
    
}

sub highest_noise {
    my $self = shift;
    my $seqt = shift;
    my $domt = shift;
    my ($seq,$unit,$hseq,$hdom,$noiseseq,$noisedom);

    $hseq = $hdom = -100000;
    
    foreach $seq ( $self->eachHMMSequence()) {
	if( $seq->bits() < $seqt && $seq->bits() > $hseq  ) {
	    $hseq = $seq->bits();
	    $noiseseq = $seq;
	}
	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( (($seq->bits() < $seqt) || ($seq->bits() > $seqt && $unit->bits < $domt)) && $unit->bits() > $hdom ) {
		$hdom  = $unit->bits();
		$noisedom = $unit;
	    }
	}
    }


    return ($noiseseq,$noisedom);
   
}


sub lowest_true {
    my $self = shift;
    my $seqt = shift;
    my $domt = shift;
    my ($seq,$unit,$lowseq,$lowdom,$trueseq,$truedom);

    if( ! defined $domt ) {
	carp "lowest true needs at least a domain threshold cut-off";
	return (0,0);
    }

    $lowseq = $lowdom = 100000;

    foreach $seq ( $self->eachHMMSequence()) {
	
	if( $seq->bits() >= $seqt && $seq->bits() < $lowseq  ) {
	    $lowseq = $seq->bits();
	    $trueseq = $seq;
	}
	if( $seq->bits() < $seqt ) {
	    next;
	}

	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( $unit->bits() >= $domt && $unit->bits() < $lowdom ) {
		$lowdom  = $unit->bits();
		$truedom = $unit;
	    }
	}
    }


    return ($trueseq,$truedom);
    
}


sub merge_results {
    # merge two HMMResults objects, preferring those from self over res in case of overlap
    my $self = shift;
    my $res  = shift;

    foreach my $seq ( $res->eachHMMSequence ) {
	if( not $self->getHMMSequence($seq->name) ) {
	    # $self doesn't contain a sequence with this name
	    my $newseq = new HMMSequence;
	    $newseq->name($seq->name);
	    $newseq->bits($seq->bits);
	    $newseq->desc($seq->desc);
	    $newseq->evalue($seq->evalue);
	    $self->addHMMSequence($newseq);
	}
      UNIT: foreach my $unit ( $seq->eachHMMUnit() ) {
	  foreach my $oldunit ( $self->getHMMSequence($seq->name)->eachHMMUnit() ) {
	      if( ( $unit->hmmname() eq $oldunit->hmmname() ) and
		  ( ( $unit->start_seq() >= $oldunit->start_seq() and $unit->start_seq() <= $oldunit->end_seq() ) or
		    ( $unit->end_seq()   >= $oldunit->start_seq() and $unit->end_seq()   <= $oldunit->end_seq() ) or
		    ( $unit->start_seq() <= $oldunit->start_seq() and $unit->end_seq()   >= $oldunit->end_seq() ) ) ) {
		  next UNIT;
	      }
	  }
	  $self->addHMMUnit($unit);
      }
    }
    return $self;
}


sub write_pfam_output {
    my $self = shift;
    my $fh   = shift;

    print $fh <<EOF;
# ===========
# Pfam output
# ===========
#
# Sequence scores
# ---------------
#
# name      description                                   bits      evalue   n
#
EOF

    foreach my $seq ( sort { $b->bits <=> $a->bits } $self->eachHMMSequence ) {
	$_ = $seq->desc;
	my( $desc ) = /^(.{1,42})/;
	$desc = uc( $desc );
	printf $fh ( "%-10s  %-42s %8.1f  %9s %3d\n", 
		     $seq->name,
		     $desc,
		     $seq->bits,
		     $seq->evalue,
		     scalar( $seq->eachHMMUnit() ) );
    }

    print $fh <<EOF;
#
# Domain scores
# -------------
#
# name       start     end  hmm-st  hmm-en   bits      evalue
#
EOF
    
    foreach my $dom ( sort { $b->bits <=> $a->bits } $self->eachHMMUnit ) {
	printf $fh ( "%-10s  %6d  %6d  %6d  %6d  %6.1f  %9s\n", 
		     $dom->seqname,
		     $dom->start_seq,
		     $dom->end_seq,
		     $dom->start_hmm,
		     $dom->end_hmm,
		     $dom->bits,
		     $dom->evalue );
    }
	
}
    

sub calculate_evalues {
    my $res = shift;
    if( @_ < 3 ) {
	die( "HMMResults: cannot calculate evalues without mu, lambda and dbsize" );
    }
    my $mu     = shift;
    my $lambda = shift;
    my $dbsize = shift;
    my %cache;

    foreach my $seq ( $res -> eachHMMSequence ) {
	if( exists $cache{$seq->bits} ) {
	    $seq -> evalue( $cache{$seq->bits} );
	}
	else {
	    $seq -> evalue( bits2evalue( $seq->bits, $mu, $lambda, $dbsize ) );
	}
	foreach my $unit ( $seq -> eachHMMUnit ) {
	    if( exists $cache{$unit->bits} ) {
		$unit -> evalue( $cache{$unit->bits} );
	    }
	    else {
		$unit -> evalue( bits2evalue( $unit->bits, $mu, $lambda, $dbsize ) );
	    }
	}
    }
    return $res;
}


sub bits2evalue {
    my( $bits, $mu, $lambda, $n ) = @_;
    return sprintf( "%.2g", $n * _p_value_from_bits( $bits, $mu, $lambda, $n ) );
}

sub _p_value_from_bits {
    my( $bits, $mu, $lambda, $n ) = @_;
    my $pvalue = 1 / ( 1 + ( 2 ** $bits ) );
    my $pval2  = _extreme_value_p( $bits, $mu, $lambda );
    $pvalue = $pval2 if( $pval2 < $pvalue );
    return $pvalue;
}

sub _extreme_value_p {
    my( $bits, $mu, $lambda ) = @_;
    my $y = exp( -1 * $lambda * ( $bits - $mu ) );
    if( $y < 1e-7 ) {
        return $y;
    }
    else {
	return ( 1.0 - exp( -1 * $y ) );
    }
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

__END__

=head1 NAME

HMMResults

=head1 SYNOPSIS

A Perl Module for handling HMMer2 results files.

=head1 DESCRIPTION

HMMResults is the top level module for handling HMMer2 results. 
It relies on HMMSequence as being a representation of the per-sequence
scores and HMMUnit as being a representation of the per-domain scores.

The object model is quite simple. Each HMMSequence has a name, bit-score and
evalue attributes, along with a list of HMMUnits. Each HMMUnit has a sequence
and a HMM name, a Range in sequence and a Range in HMM and bits and evalue scores.

HMMResults holds a list of HMMSequences and a list of HMMUnits. The HMMSequences
are correctly parsed so they contain the correct HMMUnits (thoug of course, depending
on the --domT parameter you used for the output, not all the domains that make up the
score will actually be held).

These modules are aimed quite specifically at parsing HMMer2 and nothing else. If
one wants a generic 'domain' model then it is probably better to explicitly write
one from scratch than mess around sub/super classing this (though that would be another
root). I don't - sadly - have good kosher generic domain modules yet. 


=head1 AUTHOR

B<Ewan Birney> Email birney@sanger.ac.uk

=head1 ATTRIBUTES

HMMResults have no basic attributes. They do however have two internal lists,
with add/each pairs to provide access to them

=over

=item addHMMUnit

Adds an HMMUnit to the HMMResults set. It will try to add it to the correct
HMMSequence (so you don't need to worry about it). This does mean that the
appropiate HMMSequence must be loaded first. It warns if it can't find the HMMSequence

=item eachHMMUnit

Gives a list of all HMMUnits held by HMMResults

=item addHMMSequence

Adds an HMMSequence to the HMMResults set. These are stored in an internal
hash for ease of use.

=item eachHMMSequence

Gives a list of all HMMSequence held by HMMResults. Currently this list
is from the hash... ie *unordered*. Ooops!


=end

=head1 MEMBER FUNCTIONS

=item parse_HMMer2

actually parses in HMMer2 output. Use as

	$res = new HMMResults;
	$res->parse_HMMer2(\*STDIN);

	....

Actually written to parse the results of hmmsearch


=item write_GDF

writes all the HMMUnits out to a file.


=item write_GDF_bits

Using a two threshold system, writes HMMUnits which pass the 2 thresholds
out to a file.

=item lowest_true

Using a two threshold system, gives back the lowest HMMSequence and the lowest
HMMUnit to be included in the reals.

	$res = new HMMResults;
	$res->parse_HMMer2(\*STDIN);

	($seq,$dom) = $res->lowest_true(25,10);

	print sprintf("Lowest sequence is %.2f (%.2g). Lowest domain is %.2f (%.2g)\n",$seq->bits(),$seq->evalue(),$dom->bits(),$dom->evalue());
	 
	....


=item highest_noise

like the inverse lowest_true, gives back the highest sequence and unit which were not
included in the two-threshold cut-off

=item parse_hmmpfam

Parses the results of hmmpfam

=item write_seqdom_output

No current documentation

=item write_FT_output

No current documentation

=item filter_n_cutoff

filters the HMM results on the basis of a two level cutoff.

=item number

No current documentation

=item get_unit_nse

No current documentation

=item getHMMSequence

No current documentation

=item write_acc_out

No current documentation
