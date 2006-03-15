
#
# Perl Module for WWWBlastResults
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
#Copyright Genome Research Limited (1999). Please see information on licensing in LICENSE

package Bio::Pfam::Web::WWWBlastResults;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use strict;



#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

use Bio::Tools::BPlite;
use Bio::Pfam::PfamRegion;

use Bio::Pfam::Web::PfamWWWConfig;


@ISA = ( 'Exporter');


sub new {
    my ($ref, %params) = @_;
    my ($cutoff) = ( ($params{'-cutoff'}||$params{'cutoff'}));
    my $self = {'blast_hits' => [] };
    bless $self, $ref;

    $self->cutoff( $cutoff ); 
    
    return $self;
}


=head2 add_results_to_AnnSeq

 Title   : add_blast_hsps_to_AnnSeq
 Usage   : $res->add_results_to_Annseq( $annseq )
 Function: 
    Takes the given AnnotatedSequence and and fills it with regions,
    according to the blast hits recovered at the parsing stage
 Returns : 
 Args    : Bio::Pfam::AnnotatedSequence

=cut

sub add_results_to_AnnSeq {
    my $self = shift;
    my $annsq = shift;

    foreach my $hsp ($self->each_hit()) {
	my ($acc, $id) = $hsp->parent->desc =~ /^(\S+);(\S+);$/;
#	my $annot = Bio::Pfam::Annotation->new('-DESCRIPTION' => $id );
	my $annot = Bio::Annotation->new('-DESCRIPTION' => $id );
	$annsq->addAnnotatedRegion( 
	    Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
				       '-PFAM_ID' => $id,
				       '-FROM' => $hsp->start('query'),
				       '-TO' => $hsp->end('query'),
				       '-ANNOTATION' => $annot 
				       )
				    );
    }
}



=head2 add_hit

 Title   : add_hit
 Usage   : $res->add_hit( $hit )
 Function: 
    Adds a Blast hsp to the object
 Returns : 
 Args    : An HSP object obtained from Bio::Tools::Blast object

=cut

sub add_hit {
    my ($self, $hit) = @_; 
    
    push @{$self->{'blast_hits'}}, $hit;
}



=head2 each_hit

 Title   : each_hit
 Usage   : @list  = $res->each_hit()
 Function: 
    Adds a Blast hsp to the object
 Returns : A list od HSPs
 Args    : 

=cut

sub each_hit {
    my $self = shift;

    return @{$self->{'blast_hits'}};
}




=head2 cutoff

 Title   : cutoff
 Usage   : $res->cutoff( $number )
 Function: 
    Gets/sets the cutoff, used when parsing the blast results
 Returns : A list od HSPs
 Args    : A cutoff number (optional)

=cut

sub cutoff {
    my ($self, $value) = @_;

    if (defined $value) {
	$self->{'blast_cutoff'} = $value;
    }

    return $self->{'blast_cutoff'};
}



=head2 number

 Title   : number
 Usage   : if ($res->number()) { ...
 Function: 
    returns the number of hits stored by the object. Will be
    zero before the Blast results have been parsed.
 Returns : A number
 Args    : 

=cut

sub number {
    my $self = shift;

    return scalar(@{$self->{'blast_hits'}});
}




=head2 parse_blast_results

 Title   : parse_blast_results
    Usage   : $res->parse_blast_results( $blast_res );
 Function: 
    Makes sense of the muddle of results that could result, using
    an ad-hoc conflict resolution strategy
 Returns : 
 Args    : Bio::Tools::Blast

=cut

sub parse_blast_results {
    my $self = shift;
    my $results = shift;
    my (%log);

    while(my $sbjct = $results->nextSbjct) {
      my($acc, $id, $seqname);
      if ($sbjct->name =~ /(\S+)\s+(\S+);(\S+);/) {
	$acc = $2; $id = $3;
	
     } 
      while(my $hsp = $sbjct->nextHSP) {
	
	if ($hsp->P > $self->cutoff()) {	 
	  next;
	}
	my $start = $hsp->query->start;
	my $end = $hsp->query->end;
	my ($dontadd, %templog);
	foreach my $item (keys %log) {
	 
	  my @templist = ();
	  foreach my $oldhsp (@{$log{$item}}) {
	    my $st = $oldhsp->query->start;
	    my $en = $oldhsp->query->end;
	    
	   
	    if ($start > $en or $end < $st) {
	      push @templist, $oldhsp;
	    }
	    else {
	      # there is an overlap; replace element if it has a lower evalue
	     # print "hsp: " .$hsp->P . " \n OLD: " .$oldhsp->P . " \n"; sleep 1;
	      if ($hsp->P > $oldhsp->P) {
		# if the new hsp has a lower e-value, it will be added at the end
		push @templist, $oldhsp;
		$dontadd = 1;
	      }
	      
	    }
	  }
	  $log{ $item } = \@templist;
	}



	if (! $dontadd) {
	  if (defined $log{$acc}) {
	    push @{$log{$acc . "~" . $id}}, $hsp;
	  }
	  else {
       
	    $log{$acc. "~" . $id} = [ $hsp ];
	  }
	}

      }
    }

    # now build and return the final list

    my @retlist;

    foreach my $key (keys %log) {
      my @eep = @{$log{$key}};
      foreach (@eep) {
	$self->add_hit( $key . "~" . $_->query->start . "~" . $_->query->end . "~" . $_->P);

      }

    }

 #   foreach my $key ( keys %log ) {

 #   print "query: $key \n";
#	my $eep =  @{$log{$key}};
#	print "EEP: $eep ," .$eep->query->start . " \n";
#      my %example = $log{$key};
#      print "ex: %example \n";
##      	$self->add_hit( $key . "~" . $log{$key}->query->start . "~" . $log{$key}->query->end . "~" . $log{$key}->query->P  )
  #  }
#
  #  foreach my $key (sort { $a->query->start <=> $b->query->start } @retlist) {
 #    print "THE query: $key \n";
#	$self->add_hit( $key );
 #  }


#    my (%log);
#    foreach my $hit ($results->hits) {
#      print "HIT: $hit \n";
   

#	my ($acc, $id) = $hit->desc() =~ /^(\S+);(\S+);$/;
#	foreach my $hsp ($hit->hsps) {
#	    if ($hsp->expect > $self->cutoff()) {
#		next;
#	    }

#	    my $start = $hsp->start('query');
#	    my $end = $hsp->end('query');
	    
#	    my ($dontadd, %templog);
#	    foreach my $item (keys %log) {
#		my @templist = ();
#		foreach my $oldhsp (@{$log{$item}}) {
#		    my $st = $oldhsp->start('query');
#		    my $en = $oldhsp->end('query');
		    
#		    if ($start > $en or $end < $st) {
#			push @templist, $oldhsp;
#		    }
#		    else {
#			# there is an overlap; replace element if it has a lower evalue

#			if ($hsp->expect > $oldhsp->expect) {
#			    # if the new hsp has a lower e-value, it will be added at the end

#			    push @templist, $oldhsp;
#			    $dontadd = 1;
#			}
			
#		    }
#		}
#		$log{ $item } = \@templist;
#	    }
	    
#	    if (! $dontadd) {
#		if (defined $log{$acc}) {
#		    push @{$log{$acc}}, $hsp;
#		}
#		else {
#		    $log{$acc} = [ $hsp ];
#		}
#	    }		
#	}
#    }
    
#    # now build and return the final list
#    my @retlist;
#    foreach my $key ( keys %log ) {
#	push @retlist, @{$log{$key}};
#    }

#    foreach my $key (sort { $a->start('query') <=> $b->start('query') } @retlist) {
#	$self->add_hit( $key );
#    }
}



##############################
# This is a complete hack - prints out blast output to a file
#
#
################################

sub print_blast_to_file {
  my($self, $file_out) = @_;
  open(_FILE, ">$file_out") or print STDERR "Pfam: CANNA OPEN $file_out FILE AS $! <P>";
  foreach my $hsp ( $self->each_hit) {
    my ($acc, $name, $start, $end, $expect) = split(/~/, $hsp);
    print _FILE "$acc~$name~$start~$end~$expect\n";
    
  }
  
  
  close(_FILE);
  
}

=head2 write_table

 Title   : write_table
    Usage   : $res->write_table( *STDOUT, $href );
 Function: 
    Prints the enclosed hits in an html table
 Returns : 
 Args    : File handle

=cut

sub write_html_table {
    my ($self, $file, $href) = @_;
    
    my ($t,$name,$acc);

    if ($self->number()) {
	print $file sprintf("<table border=1 cellpadding=5 cellspacing=0 bgcolor=$PfamWWWConfig::pfamcolour><tr bgcolor=#000070 ><th class=whitenormalfont>Domain</th><th class=whitenormalfont>Start</th><th class=whitenormalfont>End</th><th class=whitenormalfont>Evalue</th><th class=whitenormalfont>Alignment</th></tr>\n");
	
	foreach my $hsp ( $self->each_hit ) {
	    ($acc, $name) = $hsp->parent->desc() =~ /^(\S+);(\S+);$/;
	    eval("\$t = \"$href\";");
	    
	    print $file sprintf("<tr><td>%s</td><td>%d</td><td>%d</td><td>%4.2g</td><td><a href=\"#%s\">Align</a></td></tr>\n",$t,$hsp->start('query'),$hsp->end('query'),$hsp->expect, "Pfam-B/".$hsp->start('query')."-".$hsp->end('query'));
	    
	} 
	print $file "</table><br><br>\n";
    }
   
}


=head2 write_html_align

 Title   : write_align
    Usage   : $res->write_align( *STDOUT, $href );
 Function: 
    Prints the enclosed hits as text alignment regions
 Returns : 
 Args    : File handle

=cut

sub write_html_align {
    my $self = shift;
    my $file = shift;
    my $href = shift;
    my $gseq  = shift; # sequence to make subsequence from
    
    my ($t,$temp2,$name,$acc,$start,$end,$subseq,$sname);

    if ($self->number()) {
	# print $file "<hr>";

	print $file "<form method=\"POST\" action=\"$PfamWWWConfig::align2seed\">";
	
	print $file <<EOF;
<span class=normaltext>Format for fetching alignments to Pfam-B families: </span>
<select name=format >
<option value=linkswisspfam> Hypertext linked to swisspfam
<option value=mul> Plain Pfam format
<option value=stock> Plain Stockholm format
<option value=fal> Fasta format
<option value=belvu> Belvu helper application
<option value=jalview> Jalview Java alignment viewer

</select>

    <p>
EOF
	my $count = 0;
	foreach my $hsp ( $self->each_hit ) {	    
	    if( defined $href ) {
		my ($acc, $name) = $hsp->parent->desc =~ /^(\S+);(\S+);$/;
		$start = $hsp->start('query');
		$end = $hsp->end('query');
		$subseq = $gseq->seq($start, $end);
		$sname  = $gseq->id();
		eval("\$t = \"$href\";");
		
		$temp2 = "<center><input type=hidden name=name$count value=$name>\n";
		$temp2 .= "<input type=hidden name=acc$count value=\"$acc\">\n";
		$temp2 .= "<input type=hidden name=start$count value=\"$start\">\n";
		$temp2 .= "<input type=hidden name=end$count value=\"$end\">\n";
		$temp2 .= "<input type=hidden name=sname$count value=\"$sname\">\n";
		$temp2 .= "<input type=hidden name=subseq$count value=\"$subseq\">\n";
		$temp2 .= "<input type=submit name=submit$count value=\"Align to family\"></center>";

		$count++;
	    } else {
		$t = $name;
	    }
	    
	    print $file sprintf("<a name=\"%s\"><img src=$PfamWWWConfig::WWW_root/gifs/arrow.gif> <span class=normaltext>Query</a> %s/%d-%d matching $t</span><pre>\n", "Pfam-B/".$hsp->start('query')."-".$hsp->end('query'), $sname, $start, $end);
	    $self->write_hsp_alignment( $hsp, $gseq, $file );

	    print "</pre>\n$temp2\n<p>\n";
	    
	}
	
	
	# lastly, the paramters that will be common to all alignments
	
	print $file "<input type=hidden name=lastindex value=$count>\n";
	print $file "<input type=hidden name=pfamB value=1>\n";
	print $file "</form>";
    }
}




sub write_hsp_alignment {
    my $self = shift;
    my $hsp = shift;
    my $qseq = shift;
    my $file = shift;

    my $blocksize = 50;

    my $qid = $qseq->id();
    my ($tid, $tstart, $tend) = $hsp->parent->name =~ /^(\S+)\/(\d+)\-(\d+)$/;
    my $namelength = 5;
    if (length($qid) > length($tid)) {
        $namelength += length($qid);
    }
    else {
        $namelength += length($tid);
    }

    my $qstart = $hsp->start('query');
    my $qend = $hsp->end('query');
    $tstart += $hsp->start('sbjct') - 1; 
    $tend += $hsp->end('sbjct') - 1;
    my $startlength = (length($qend) > length($tend))?length($qend):length($tend);
    
    my $sbjct = $hsp->seq_str('sbjct');
    my $match = $hsp->seq_str('match');
    my $query = $hsp->seq_str('query');

    my $offset = 0;
    my $qcount = $qstart;
    my $tcount = $tstart;
    while ($offset <= length($query)) {
        my $formatstring1 = "%$namelength"."s"." "x$startlength."  %s\n";
        my $formatstring2 = "%$namelength"."s %".$startlength."d %s %s\n";

        my $temp;
        ($temp = substr($query, $offset, $blocksize)) =~ s/[.-]//g;
        my $querychars = length($temp);
        ($temp = substr($sbjct, $offset, $blocksize)) =~ s/[.-]//g;
        my $sbjctchars = length($temp);

        print $file sprintf("$formatstring2", $tid, $tcount, substr($sbjct, $offset, $blocksize), $tcount + $sbjctchars - 1); 
        print $file sprintf("$formatstring1", "", substr($match, $offset, $blocksize)); 
        print $file sprintf("$formatstring2", $qid, $qcount, substr($query, $offset, $blocksize), $qcount + $querychars - 1);

        $offset += $blocksize;
        $qcount += $querychars;
        $tcount += $sbjctchars;

        print "\n";
    }
}


sub write_ascii_out {
    my $self = shift;
    my $fh = shift;

    if (not defined $fh) {
	$fh = \*STDOUT;
    }

    if ($self->number()) {
	foreach my $hsp ( $self->each_hit ) {
	    my ($acc, $name) = $hsp->parent->desc() =~ /^(\S+);(\S+);$/;

	    print $fh sprintf("%s %4d %4d %s %4.2f %4.2g %s\n",$hsp->parent->parent->name, $hsp->start('query'),$hsp->end('query'), $acc, $hsp->bits, $hsp->expect, $name);
	    
	} 
    }



}



1;  # says use was ok

