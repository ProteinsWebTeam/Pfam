package Bio::Easel::MSA;

use strict;
use warnings;
use File::Spec;
use Carp;

=head1 NAME

Bio::Easel - The great new Bio::Easel!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

my $src_file      = undef;
my $typemaps      = undef;
my $easel_src_dir = undef;

BEGIN {
    $src_file = __FILE__;
    $src_file =~ s/\.pm/\.c/;

    my $file = __FILE__;
    ($easel_src_dir) = $file =~ /^(.*)\/blib/;
    $easel_src_dir = File::Spec->catfile($easel_src_dir, 'src/easel');

    $typemaps = __FILE__;
    $typemaps =~ s/\.pm/\.typemap/;
}

use Inline
  C        => "$src_file",
  VERSION  => '0.01',
  ENABLE   => 'AUTOWRAP',
  INC      => "-I$easel_src_dir",
  LIBS     => "-L$easel_src_dir -leasel",
  TYPEMAPS => $typemaps,
  NAME     => 'Bio::Easel::MSA';


=head1 SYNOPSIS

Multiple sequence alignment handling through inline C with Easel.

Perhaps a little code snippet.

    use Bio::Easel::MSA;

    my $foo = Bio::Easel::MSA->new({"fileLocation" => $alnfile});
    ...

=head1 EXPORT

No functions currently exported.

=head1 SUBROUTINES/METHODS

=cut

=head2 new 

  Title    : new
  Incept   : EPN, Thu Jan 24 09:28:54 2013
  Usage    : Bio::EslMSA->new
  Function : Generates a new Bio::EslMSA object
  Args     : <fileLocation>: file location of alignment
           : <nseq>: number of seqs
  Returns  : Bio::EslMSA object
  
=cut

sub new {
    my( $caller, $args) = @_;
    my $class = ref($caller) || $caller;
    my $self = {};

    bless( $self, $caller );

    # First check that the file exists. If it exists, read it with 
    # Easel and populate the object from the ESL_MSA object
    if(-e $args->{fileLocation}){
	eval{
	    $self->{path} = $args->{fileLocation};
	    $self->read_msa();
	}; # end of eval
	
	if($@) {
	    confess("Error creating ESL_MSA from @{[$args->{fileLocation}]}, $@\n");
	}
    } 
    else {
	confess("Expected to receive a valid file location path (@{[$args->{fileLocation}]} doesn\'t exist)");
    }
    if(defined $args->{aliType}){
	$self->{aliType} = $args->{aliType};
    }

    return $self;
}


=head2 msa

  Title    : msa
  Incept   : EPN, Tue Jan 29 09:06:30 2013
  Usage    : Bio::Easel::MSA->msa()
  Function : Accessor for msa: sets (if nec) and returns MSA.
  Args     : none
  Returns  : msa   

=cut

sub msa {
    my($self) = @_;

    if(! defined ($self->{esl_msa})) { 
	$self->read_msa();
    }

    return $self->{esl_msa};
}

=head2 path

  Title    : path
  Incept   : EPN, Tue Jan 30 15:42:30 2013
  Usage    : Bio::Easel::MSA->path()
  Function : Accessor for path, read only.
  Args     : none
  Returns  : string containing path to the SEED or undef.   

=cut
sub path {
  my ($self) = @_;
  return defined($self->{path}) ? $self->{path} : undef;
}

=head2 read_msa

  Title    : read_msa
  Incept   : EPN, Mon Jan 28 09:26:24 2013
  Usage    : Bio::Easel::MSA->read_msa($fileLocation)
  Function : Opens $fileLocation, reads first MSA, returns it
  Args     : <fileLocation>: file location of alignment
  Returns  : ESL_MSA *C* object
  
=cut

=head2 _c_read_msa

  Title    : _c_read_msa
  Incept   : EPN, Thu Jan 31 07:30:50 2013
  Usage    : _c_read_msa(<fileLocation>, <abc>)
  Function : In C, opens $fileLocation and reads first MSA
  Args     : <fileLocation>: file location of alignment
           : <abc>:          place holder for ESL_ALPHABET C object
  Returns  : ESL_MSA C object, fills <abc> with ESL_ALPHABET C object
  
=cut

sub read_msa { 
    my ($self, $fileLocation) = @_;

    if($fileLocation) { 
	$self->{path} = $fileLocation;
    }
    if(! defined $self->{path}) { 
	die "ERROR: path not set in Bio::Easel::MSA object"; 
    }

    $self->{esl_msa} = _c_read_msa($self->{path}, $self->{esl_abc});
    return;
}

=head2 nseq

  Title    : nseq
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : Bio::Easel::MSA->nseq()
  Function : Sets (if nec) and returns number of sequences in MSA.
  Args     : none
  Returns  : number of sequences (esl_msa->nseq)
  
=cut

=head2 _c_nseq

  Title    : _c_nseq
  Incept   : EPN, Thu Jan 31 07:35:23 2013
  Usage    : _c_nseq(<msa>)
  Function : Returns number of seqs in msa (msa->nseq)
  Args     : ESL_MSA C object 
  Returns  : number of sequences in msa (msa->nseq)
  
=cut

sub nseq { 
    my ($self) = @_;
    
    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if (! defined $self->{nseq}) { 
	$self->{nseq} = _c_nseq($self->{esl_msa});
    }
    return $self->{nseq};
}

=head2 alen

  Title    : alen
  Incept   : EPN, Tue Jan 29 07:41:08 2013
  Usage    : Bio::Easel::MSA->alen()
  Function : Sets (if nec) and returns alignment length.
  Args     : none
  Returns  : alignment length, number of columns (esl_msa->alen)
  
=cut

=head2 _c_alen

  Title    : _c_alen
  Incept   : EPN, Thu Jan 31 07:37:21 2013
  Usage    : _c_alen(msa)
  Function : Returns alignment length (msa->alen)
  Args     : ESL_MSA C object
  Returns  : alignment length, number of columns (msa->alen)
  
=cut

sub alen { 
    my ($self) = @_;
    if (! defined $self->{alen}) { 
	$self->{alen} = _c_alen($self->{esl_msa});
    }
    return $self->{alen};
}

=head2 get_sqname

  Title    : get_sqname
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : $msaObject->get_sqname($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence $idx (esl_msa->sqname[$idx])
  
=cut

=head2 _c_get_sqname

  Title    : _c_get_sqname
  Incept   : EPN, Thu Jan 31 07:38:13 2013
  Usage    : _c_get_sqname(msa, idx)
  Function : Returns name of sequence <idx> in <msa>.
  Args     : <msa>: ESL_MSA C object
           : <idx>: sequence index in <msa> 
           : NOTE: <idx> should be 0..nseq-1 (to return first sqname, pass 0)
  Returns  : name of sequence <idx> (msa->sqname[<idx>])
     
=cut

sub get_sqname { 
    my ($self, $idx) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    return _c_get_sqname($self->{esl_msa}, $idx);
}

=head2 set_sqname

  Title    : set_sqname
  Incept   : EPN, Mon Jan 28 09:48:42 2013
  Usage    : $msaObject->set_sqname($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence index $idx 
  
=cut

=head2 _c_set_sqname

  Title    : _c_set_sqname
  Incept   : EPN, Thu Jan 31 07:39:18 2013
  Usage    : _c_set_sqname(<msa>, <idx>, <newname>)
  Function : Sets sqname of seq <idx> in <msa> to <newname>
  Args     : <msa>: ESL_MSA C object
           : <idx>: index of sequence to set name of
           : <newname>: name for sequence <idx>
  Returns  : void
  
=cut

sub set_sqname { 
    my ($self, $idx, $newname) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    _c_set_sqname($self->{esl_msa}, $idx, $newname);
    return;
}

=head2 write_msa

  Title    : write_msa
  Incept   : EPN, Mon Jan 28 09:58:19 2013
  Usage    : $msaObject->write_msa($fileLocation)
  Function : Write MSA to a file
  Args     : name of output file 
  Returns  : void
  
=cut

=head2 _c_write_msa

  Title    : _c_write_msa
  Incept   : EPN, Thu Jan 31 07:40:58 2013
  Usage    : _c_write_msa(<msa>, <outfile>)
  Function : Write MSA to a file
  Args     : <msa>: ESL_MSA C object
           : <outfile> name of output file 
  Returns  : void
  
=cut

sub write_msa { 
    my ($self, $outfile) = @_;
    _c_write_msa($self->{esl_msa}, $outfile);
    return;
}

=head2 any_allgap_columns

  Title    : any_allgap_columns
  Incept   : EPN, Mon Jan 28 10:44:12 2013
  Usage    : Bio::Easel::MSA->any_allgap_columns()
  Function : Return TRUE if any all gap columns exist in MSA
  Args     : none
  Returns  : TRUE if any all gap columns, FALSE if not
  
=cut

=head2 _c_any_allgap_columns

  Title    : _c_any_allgap_columns
  Incept   : EPN, Thu Jan 31 07:41:38 2013
  Usage    : _c_any_allgap_columns(<msa>)
  Function : Return TRUE if any all gap columns exist in <msa>
  Args     : ESL_MSA C object
  Returns  : TRUE if any all gap columns exist in <msa>, FALSE if not
           : 'gap' defined as '.', '-', '_', or '~';
  
=cut

sub any_allgap_columns {
    my ($self) = @_;

    # EPN should I do error checking in c_any_allgap_columns()? 
    return _c_any_allgap_columns($self->{esl_msa});
}

=head2 nse_create

  Title    : nse_create
  Incept   : EPN, Wed Jan 30 10:37:54 2013
  Usage    : $msaObject->nse_create
  Function : Creates hash of two dim arrays (self->nseHAA) from MSA names that match N/S-E
           : N: sq accession
           : S: hit start (> end if on opposite strand)
           : E: hit end   
           : hash (self->nseHAA) key is N, value is array of arrays, 
           : with 2nd array dim including 5 fields: $s, $e, $a, $b, $strand,
           : $s=S and $e=E, in original order ("S" may be > "E")
           : if ($a, $b, $strand) == ($s, $e, 1) else ($a, $b, $strand) = ($e, $s, -1)
  Args     : none
  Returns  : number of sequences with names that match format N/S-E
  
=cut

sub nse_create {
    my ($self) = @_;

    my $idx;     # counter over names in msa
    my $sqname;  # sequence name from msa
    my $n;       # sqacc
    my $s;       # start, from seq name (can be > $end)
    my $e;       # end,   from seq name (can be < $start)
    my $a;       # minimum of $s, $e
    my $b;       # maximum of $s, $e
    my $strand;  # strand
    my $ctr = 0; # number of n/s-e names processed (added to hashes)
    my $max_nseq = 10000; # maximum numer of seqs we allow this subroutine to be called on

    if($self->nseq >= $max_nseq) { 
	die "ERROR trying to process name/start-end names of MSA with max num seqs ($self->nseq > $max_nseq seqs!)"
    }


    for($idx = 0; $idx < $self->nseq; $idx++) { 
	$sqname = $self->get_sqname($idx);
	if($sqname =~ m/^(\S+)\/(\d+)\-(\d+)\s*/) {
	    ($n, $s, $e) = ($1, $2, $3);
	    if($s <= $e) { $a = $s; $b = $e; $strand =  1; }
	    else         { $a = $e; $b = $s; $strand = -1; }
	    printf("pushing $s $e $a $b $strand to $n\n");
	    push(@{$self->{nseHAA}{$n}}, [$s, $e, $a, $b, $strand]);
	    $ctr++;
	}
    }	    
    return $ctr;
}

=head2 nse_overlap

  Title    : nse_overlap
  Incept   : EPN, Wed Jan 30 09:37:31 2013
  Usage    : $msaObject->nse_overlap_nres($nse)
  Function : Checks if $nse of format "name/start-end" overlaps with
           : any sequences stored in $self->{startHA}, $self->{endHA}, 
           : $self->{strandHA}
  Args     : <sqname>: seqname of format "name/start-end"
  Returns  : 2 values:
           : name of sequence in $self of maximum fractional overlap, "" if none
           : fractional overlap of max fractional overlap
=cut

sub nse_overlap {
    my ($self, $sqname) = @_;

    my $n;       # sqacc
    my ($s, $s2);  # start, from seq name (can be > $end)
    my ($e, $e2);  # end,   from seq name (can be < $start)
    my ($a, $a2);  # minimum of $s, $e
    my ($b, $b2);  # maximum of $s, $e
    my $strand;  # strand, 1 if $s < $e, else -1
    my $strand2; # strand, 1 if $s2 < $e2, else -1
    my $max_fract  = 0.;     # maximum fraction of overlap
    my $max_sqname = "";     # name of seq in sqinfoHHA 
    my $overlap_exists = 0;
    my $is_nse;  # TRUE if $sqname adheres to format n/s-e
    my $i;
    my $fract_overlap; # fractional overlap

    ($is_nse, $n, $a, $b, $strand) = $self->nse_breakdown($sqname);
    if($is_nse) { 
	if(exists $self->{nseHAA}->{$n}) { 

	    for($i = 0; $i < scalar(@{$self->{nseHAA}->{$n}}); $i++) { 
		($s2, $e2, $a2, $b2, $strand2) = @{$self->{nseHAA}->{$n}}[$i];
		if($strand eq $strand2) { 
		    $fract_overlap = _overlap_fraction($a, $b, $a2, $b2);
		    if($fract_overlap > $max_fract) { 
			$max_fract  = $fract_overlap;
			$max_sqname = $self->nse_create($a2, $b2, $strand2);
			$overlap_exists = 1;
		    }
		}
	    }
	}
    }
    if($overlap_exists) { return ($max_sqname, $max_fract); }
    else                { return ("", 0.); }
}

=head2 nse_breakdown

  Title    : nse_breakdown
  Incept   : EPN, Wed Jan 30 09:50:07 2013
  Usage    : $msaObject->nse_breakdown($nse)
  Function : Checks if $nse is of format "name/start-end" and if so
           : breaks it down into $n, $a, $b, $strand (see 'Returns' section)
  Args     : <sqname>: seqname, possibly of format "name/start-end"
  Returns  : 5 values:
           :   '1' if seqname was of "name/start-end" format, else '0'
           :   $n: name ("" if seqname doesn't match "name/start-end")
	   :   $a: minimum of start, end (0 if seqname doesn't match "name/start-end")
	   :   $b: maximum of start, end (0 if seqname doesn't match "name/start-end")
	   :   $strand: 1 if start <= $end, -1 if not (0 if seqname doesn't match "name/start-end")
=cut
sub nse_breakdown {
    my ($self, $sqname) = @_;

    my $n;       # sqacc
    my $s;       # start, from seq name (can be > $end)
    my $e;       # end,   from seq name (can be < $start)
    my $a;       # minimum of $s, $e
    my $b;       # maximum of $s, $e
    my $strand;  # strand

    if($sqname =~ m/^(\S+)\/(\d+)\-(\d+)\s*/) {
	($n, $s, $e) = ($1,$2,$3);
	if($s <= $e) { $strand =  1; $a = $s; $b = $e; }
	else         { $strand = -1; $a = $e; $b = $s; }
	return (1, $n, $a, $b, $strand);
    }
    return (0, "", 0, 0, 0);
}

=head2 free_msa

  Title    : free_msa
  Incept   : EPN, Mon Jan 28 11:06:18 2013
  Usage    : $msaObject->free_msa()
  Function : Frees an MSA->{$esl_msa} object
  Args     : name of output file 
  Returns  : void
  
=cut

sub free_msa { 
    my ($self) = @_;
    _c_free_msa($self->{esl_msa});
    return;
}

=head2 DESTROY

  Title    : DESTROY
  Incept   : EPN, Mon Jan 28 10:09:55 2013
  Usage    : $msaObject->destroy()
  Function : Frees an MSA object
  Args     : name of output file 
  Returns  : void
  
=cut

sub DESTROY { 
    my ($self) = @_;
    _c_destroy($self->{esl_msa}, $self->{esl_abc});
    return;
}

#############################
# Internal helper subroutines
#############################

=head2 _max

  Title    : _max
  Incept   : EPN, Thu Jan 31 08:55:18 2013
  Usage    : _max($a, $b)
  Function : Returns maximum of $a and $b.
  Args     : $a: scalar, usually a number
           : $b: scalar, usually a number
  Returns  : Maximum of $a and $b.
=cut

sub _max {
  return $_[0] if @_ == 1;
  $_[0] > $_[1] ? $_[0] : $_[1]
}

=head2 _min

  Title    : _min
  Incept   : EPN, Thu Jan 31 08:56:19 2013
  Usage    : _min($a, $b)
  Function : Returns minimum of $a and $b.
  Args     : $a: scalar, usually a number
           : $b: scalar, usually a number
  Returns  : Minimum of $a and $b.
=cut

sub _min {
  return $_[0] if @_ == 1;
  $_[0] < $_[1] ? $_[0] : $_[1]
}

=head2 _overlap_fraction

  Title    : _overlap_fraction
  Incept   : EPN, Thu Jan 31 08:50:55 2013
  Usage    : _overlap_fraction($from1, $to1, $from2, $to2)
  Function : Returns fractional overlap of two regions.
  Args     : $from1: start point of first region (must be <= $to1)
           : $to1:   end   point of first region
           : $from2: start point of second region (must be <= $to2)
           : $to2:   end   point of second region
  Returns  : Fractional overlap, defined as nres_overlap / minL
             where minL is minimum length of two regions
=cut

sub _overlap_fraction {
    my($from1, $to1, $from2, $to2) = @_;
    
    if($from1 > $to1) { die "ERROR Bio-Easel's _overlap_fraction, expect from1 <= y1 but $from1 > $to1"; }
    if($from2 > $to2) { die "ERROR Bio-Easel's _overlap_fraction, expect x2 <= y2 but $from2 > $to2"; }

    my $L1 =$to1 - $from1 + 1;
    my $L2 =$to2 - $from2 + 1;
    my $minL = _min($L1, $L2);
    my $D    = _overlap_nres($from1, $to1, $from2, $to2);
    return $D / $minL;
}

=head2 _overlap_nres

  Title    : _overlap_nres
  Incept   : EPN, Thu Jan 31 08:50:55 2013
  Usage    : _overlap_fraction($from1, $to1, $from2, $to2)
  Function : Returns number of overlapping residues of two regions.
  Args     : $from1: start point of first region (must be <= $to1)
           : $to1:   end   point of first region
           : $from2: start point of second region (must be <= $to2)
           : $to2:   end   point of second region
  Returns  : Number of residues that overlap between the two regions.
=cut

sub _overlap_nres {
    my ($from1, $to1, $from2, $to2) = @_;
    
    if($from1 > $to1) { die "ERROR, GetOverlap(), from1 > to1\n"; }
    if($from2 > $to2) { die "ERROR, GetOverlap(), from2 > to2\n"; }

    my $minlen = $to1 - $from1 + 1;
    if($minlen > ($to2 - $from2 + 1)) { $minlen = ($to2 - $from2 + 1); }

    # Given: $from1 <= $to1 and $from2 <= $to2.

    # Swap if nec so that $from1 <= $from2.
    if($from1 > $from2) { 
	my $tmp;
	$tmp   = $from1; $from1 = $from2; $from2 = $tmp;
	$tmp   =   $to1;   $to1 =   $to2;   $to2 = $tmp;
    }

    # 3 possible cases:
    # Case 1. $from1 <=   $to1 <  $from2 <=   $to2  Overlap is 0
    # Case 2. $from1 <= $from2 <=   $to1 <    $to2  
    # Case 3. $from1 <= $from2 <=   $to2 <=   $to1
    if($to1 < $from2) { return 0.; }                             # case 1
    if($to1 <   $to2) { return ($to1 - $from2 + 1) / $minlen; }  # case 2
    if($to2 <=  $to1) { return ($to2 - $from2 + 1) / $minlen; }  # case 3
    die "ERROR, unforeseen case in GetOverlap $from1..$to1 and $from2..$to2";
}


=head2 dl_load_flags

  Title    : dl_load_flags
  Incept   : ?
  Usage    : ?
  Function : ?
  Args     : ?
  Returns  : ?
  
=cut


=head1 AUTHORS

Eric Nawrocki, C<< <nawrockie at janelia.hhmi.org> >>
Jody Clements, C<< <clementsj at janelia.hhmi.org> >>
Rob Finn, C<< <finnr at janelia.hhmi.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bio-easel at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Bio-HMM-Logo>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bio::Easel::MSA


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Bio-HMM-Logo>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Bio-HMM-Logo>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Bio-HMM-Logo>

=item * Search CPAN

L<http://search.cpan.org/dist/Bio-HMM-Logo/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Eric Nawrocki.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of Bio::HMM::Logo

1;
