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

=head2 acc

  Title    : acc
  Incept   : EPN, Fri Feb  1 11:43:08 2013
  Usage    : $msaObject->get_accession()
  Function : Gets accession for MSA.
  Args     : none
  Returns  : the accession, a string
  
=cut

=head2 _c_acc

  Title    : _c_acc
  Incept   : EPN, Fri Feb  1 11:43:31 2013
  Usage    : _g_set_accession(<msa>)
  Function : Returns msa->acc.
  Args     : <msa>:    ESL_MSA C object
  Returns  : msa->acc
  
=cut

sub acc { 
    my ($self) = @_;

    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if (! defined $self->{acc}) { 
	$self->{acc} = _c_acc($self->{esl_msa});
    }
    return $self->{acc};
}

=head2 set_accession

  Title    : set_accession
  Incept   : EPN, Fri Feb  1 11:11:05 2013
  Usage    : $msaObject->set_accession($acc)
  Function : Sets accession for MSA.
  Args     : accession string to set
  Returns  : void
  
=cut

=head2 _c_set_accession

  Title    : _c_set_accession
  Incept   : EPN, Fri Feb  1 11:12:15 2013
  Usage    : _c_set_accession(<msa>, <newacc>)
  Function : Sets acc of msa.
  Args     : <msa>:    ESL_MSA C object
           : <newacc>: string of new accession
  Returns  : void
  
=cut

sub set_accession { 
    my ($self, $newacc) = @_;

    my $success = _c_set_accession($self->{esl_msa}, $newacc);
    if(! $success) { croak "unable to set accession (failure in C code)"; }

    # set accession in perl object
    $self->{acc} = _c_acc($self->{esl_msa});

    return;
}

=head2 write_msa

  Title    : write_msa
  Incept   : EPN, Mon Jan 28 09:58:19 2013
  Usage    : $msaObject->write_msa($fileLocation)
  Function : Write MSA to a file
  Args     : name of output file 
           : format ('stockholm', 'pfam' or 'afa')
  Returns  : void
  
=cut

=head2 _c_write_msa

  Title    : _c_write_msa
  Incept   : EPN, Thu Jan 31 07:40:58 2013
  Usage    : _c_write_msa(<msa>, <outfile>, <format>)
  Function : Write MSA to a file
  Args     : <msa>: ESL_MSA C object
           : <outfile>: name of output file 
           : <format>: string with file format, if none 'stockholm' is used
  Returns  : void
  
=cut

sub write_msa { 
    my ($self, $outfile, $format) = @_;

    if(! defined $format) { 
	$format = "stockholm";
    }
    if($format ne "stockholm" && 
       $format ne "pfam" && 
       $format ne "afa") { 
	croak "format must be \"stockholm\" or \"pfam\" or \"afa\"";
    }
    _c_write_msa($self->{esl_msa}, $outfile, $format);
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

=head2 nse_createHAA

  Title    : nse_createHAA
  Incept   : EPN, Wed Jan 30 10:37:54 2013
  Usage    : $msaObject->nse_createHAA
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

sub nse_createHAA {
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
		($s2, $e2, $a2, $b2, $strand2) = @{$self->{nseHAA}->{$n}[$i]};
		if($strand eq $strand2) { 
		    $fract_overlap = _overlap_fraction($a, $b, $a2, $b2);
		    if($fract_overlap > $max_fract) { 
			$max_fract  = $fract_overlap;
			$max_sqname = $self->nse_string($n, $a2, $b2, $strand2);
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

=head2 nse_string

  Title    : nse_string
  Incept   : EPN, Thu Jan 31 09:37:55 2013
  Usage    : $msaObject->nse_string($name, $a, $b, $strand)
  Function : Creates a "name/start-end" string from $name, $a, $b, $strand.
             If $strand==1, returns "$name/$a-$b", else
             returns "$name/$b-a".
  Args     : $name:   name, usually a sqacc.version
           : $a:      start coord, must be <= $b
           : $b:      end coord
           : $strand: 1 for positive, -1 for negative
  Returns  : "$name/$a-$b" if ($strand==1) else "$name/$b-$a";
=cut
sub nse_string {
    my ($self, $name, $a, $b, $strand) = @_;

    if($strand    ==  1) { return $name . "/" . $a . "-" . $b; }
    elsif($strand == -1) { return $name . "/" . $b . "-" . $a; }
    die "ERROR nse_string, invalid strand valid $strand (should be 1 or -1)\n";
}

=head2 nse_len

  Title    : nse_len
  Incept   : EPN, Thu Jan 31 10:08:24 2013
  Usage    : $msaObject->nse_len($name);
  Function : Returns length of sequence given $nse,
           : where $nse is of format:
           : <sqacc>/<start>-<end>
           : and <start> may be > <end>.
  Args     : $nse: sequence name in <sqacc>/<start>-<end> format
  Returns  : Length in residues represented by $nse
=cut
sub nse_len {
    my ($self, $nse) = @_;

    if($nse =~ m/^\S+\/(\d+)\-(\d+)\s*/) {
	my ($start, $end) = ($1, $2);
	if($start <= $end) { return ($end - $start + 1); }
	else               { return ($start - $end + 1); }
    }
    die "ERROR nse_len, invalid name, doesn't match <name>/<start>-<end>\n";
}

=head2 average_pid

  Title    : average_pid
  Incept   : EPN, Fri Feb  1 06:59:50 2013
  Usage    : $msaObject->average_pid($max_nseq)
  Function : Calculate and return average percent identity of 
           : all pairs of sequences in msa. If more than $max_nseq
           : sequences exist in the seed, an average is computed
           : over a stochastic sample (the sample and thus the 
           : result with vary over multiple runs).
  Args     : max number of sequences for brute force calculation
  Returns  : average percent id of all seq pairs or a sample
  
=cut

=head2 _c_average_pid

  Title    : _c_average_pid
  Incept   : EPN, Fri Feb  1 07:29:53 2013
  Usage    : _c_average_pid(msa, max_nseq)
  Function : C function for average_pid, acutally calculates
           : and return average percent identity of 
           : all pairs of sequences in msa, or a sample of
           : <max_nseq * max_nseq> if more than <max_nseq>
           : sequences exist.
  Args     : msa:      ESL_MSA C object
           : max_nseq: max number of sequences for brute force calculation
  Returns  : average percent id of all seq pairs or a sample
  
=cut

sub average_pid { 
    my ($self, $max_nseq) = @_;

    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if (! defined $self->{average_pid}) { 
	$self->{average_pid} = _c_average_pid($self->{esl_msa}, $max_nseq);
    }
    return $self->{average_pid};
}


=head2 get_sqlen

  Title    : get_sqlen
  Incept   : EPN, Fri Feb  1 16:56:24 2013
  Usage    : $msaObject->get_sqlen()
  Function : Return unaligned sequence length of 
           : sequence <idx>.
  Args     : index of sequence you want length of
  Returns  : unaligned sequence length of sequence idx
  
=cut

=head2 _c_get_sqlen

  Title    : _c_get_sqlen
  Incept   : EPN, Fri Feb  1 07:29:53 2013
  Usage    : _c_get_sqlen(msa, idx)
  Function : Return unaligned sequence length of sequence 
           : index <idx>.
  Args     : msa: ESL_MSA C object
           : idx: sequence index to get length of (0..nseq-1)
  Returns  : unaligned length of seq <idx>
  
=cut

sub get_sqlen { 
    my ($self, $idx) = @_;

    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if($idx < 0 || $idx > $self->nseq) { croak "$idx out of range" }

    return _c_get_sqlen($self->{esl_msa}, $idx);
}

=head2 average_sqlen

  Title    : average_sqlen
  Incept   : EPN, Fri Feb  1 06:59:50 2013
  Usage    : $msaObject->average_sqlen()
  Function : Calculate and return average unaligned sequence length
           : in the MSA.
  Args     : none
  Returns  : average unaligned sequence length
  
=cut

=head2 _c_average_sqlen

  Title    : _c_average_sqlen
  Incept   : EPN, Fri Feb  1 16:54:58 2013
  Usage    : $msaObject->_c_average_sqlen()
  Function : Calculate and return average unaligned sequence length
           : in the MSA.
  Args     : ESL_MSA C object
  Returns  : average unaligned sequence length
  
=cut

sub average_sqlen { 
    my ($self) = @_;

    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if (! defined $self->{average_sqlen}) { 
	$self->{average_sqlen} = _c_average_sqlen($self->{esl_msa});
    }
    return $self->{average_sqlen};
}

=head2 calc_and_write_bp_stats

  Title    : calc_and_write_bp_stats
  Incept   : EPN, Fri Feb  1 10:29:11 2013
  Usage    : $msaObject->calc_and_write_bp_stats($fileLocation)
  Function : Calculate per-basepair stats for an RNA alignment
           : with SS_cons information and output it.
  Args     : name of requested output file 
  Returns  : void
  
=cut

=head2 _c_calc_and_write_bp_stats

  Title    : _c_calc_and_write_bp_stats
  Incept   : EPN, Fri Feb  1 10:30:29 2013
  Usage    : _c_calc_and_write_bp_stats(msa, outfile)
  Function : C function for calc_and_write_bp_stats; does 
           : all the actual calculations and output.
  Args     : msa:     ESL_MSA C object
           : outfile: name of file to print to
  Returns  : '1' if file was successfully written
             '0' if file was not written due to an error
  
=cut

sub calc_and_write_bp_stats {
    my ($self, $fileLocation) = @_;

    my $errbuf = "";
    # TODO: get this owrking with errbuf, I couldn't get this to work though:
    #my $status = _c_calc_and_write_bp_stats($self->{esl_msa}, $fileLocation, $errbuf);
    my $success = _c_calc_and_write_bp_stats($self->{esl_msa}, $fileLocation);
    if(! $success) { croak "ERROR: unable to calculate and write bp stats"; }

    return;
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
    # printf STDERR "D: $D minL: $minL\n";
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
    if($to1 < $from2) { return 0; }                    # case 1
    if($to1 <   $to2) { return ($to1 - $from2 + 1); }  # case 2
    if($to2 <=  $to1) { return ($to2 - $from2 + 1); }  # case 3
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
