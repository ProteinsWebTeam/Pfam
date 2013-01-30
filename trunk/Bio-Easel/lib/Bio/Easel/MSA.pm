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

=head2 read_msa

  Title    : read_msa
  Incept   : EPN, Mon Jan 28 09:26:24 2013
  Usage    : Bio::Easel::MSA->read_msa($fileLocation)
  Function : Opens $fileLocation and updates 
  Args     : <fileLocation>: file location of alignment
  Returns  : ESL_MSA *C* object
  
=cut

sub read_msa { 
    my ($self, $fileLocation) = @_;

    if($fileLocation) { 
	$self->{path} = $fileLocation;
    }
    if(! defined $self->{path}) { 
	die "ERROR: path not set in Bio::Easel::MSA object"; 
    }

    $self->{esl_msa} = c_read_msa($self->{path}, $self->{esl_abc});
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

sub nseq { 
    my ($self) = @_;
    
    if (! defined $self->{esl_msa}) { 
	$self->read_msa();
    }
    if (! defined $self->{nseq}) { 
	$self->{nseq} = c_nseq($self->{esl_msa});
    }
    return $self->{nseq};
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

=head2 alen

  Title    : alen
  Incept   : EPN, Tue Jan 29 07:41:08 2013
  Usage    : Bio::Easel::MSA->alen()
  Function : Sets (if nec) and returns alignment length.
  Args     : none
  Returns  : alignment length, number of columns (esl_msa->alen)
  
=cut

sub alen { 
    my ($self) = @_;
    if (! defined $self->{alen}) { 
	$self->{alen} = c_alen($self->{esl_msa});
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

sub get_sqname { 
    my ($self, $idx) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    return c_get_sqname($self->{esl_msa}, $idx);
}

=head2 set_sqname

  Title    : set_sqname
  Incept   : EPN, Mon Jan 28 09:48:42 2013
  Usage    : $msaObject->set_sqname($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence index $idx 
  
=cut

sub set_sqname { 
    my ($self, $idx, $newname) = @_;
    if($idx < 0 || $idx >= $self->nseq) { die "ERROR: how should we handle this?"; }
    c_set_sqname($self->{esl_msa}, $idx, $newname);
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

sub write_msa { 
    my ($self, $outfile) = @_;
    c_write_msa($self->{esl_msa}, $outfile);
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

sub any_allgap_columns {
    my ($self) = @_;

    # EPN should I do error checking in c_any_allgap_columns()? 
    return c_any_allgap_columns($self->{esl_msa});
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
    c_free_msa($self->{esl_msa});
    return;
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
    my $max_nseq = 1; # maximum numer of seqs we allow this subroutine to be called on
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
		($s2, $e2, $a2, $b2, $strand2) = @{$self->{nseHAA}->{$n}}[$i];
		if($strand eq $strand2) { 
		    $fract_overlap = Bio::Rfam::TempRfam::overlapExtent($a, $b, $a2, $b2);
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
    c_destroy($self->{esl_msa}, $self->{esl_abc});
    return;
}
1;
