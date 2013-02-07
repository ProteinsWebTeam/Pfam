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
# Easel status codes, these must be consistent with #define's in Bio-Easel/src/easel/easel.h
our $ESLOK =              '0';    # no error/success            
our $ESLFAIL =            '1';    # failure                     
our $ESLEOL =             '2';    # end-of-line (often normal)  
our $ESLEOF =             '3';    # end-of-file (often normal)  
our $ESLEOD =             '4';    # end-of-data (often normal)  
our $ESLEMEM =            '5';    # malloc or realloc failed    
our $ESLENOTFOUND =       '6';    # file or key not found       
our $ESLEFORMAT =         '7';    # file format not correct     
our $ESLEAMBIGUOUS =      '8';    # an ambiguity of some sort   
our $ESLEDIVZERO =        '9';    # attempted div by zero       
our $ESLEINCOMPAT =      '10';    # incompatible parameters     
our $ESLEINVAL =         '11';    # invalid argument/parameter  
our $ESLESYS =           '12';    # generic system call failure 
our $ESLECORRUPT =       '13';    # unexpected data corruption  
our $ESLEINCONCEIVABLE = '14';    # "can't happen" error        
our $ESLESYNTAX =        '15';    # invalid user input syntax   
our $ESLERANGE =         '16';    # value out of allowed range  
our $ESLEDUP =           '17';    # saw a duplicate of something
our $ESLENOHALT =        '18';    # a failure to converge       
our $ESLENORESULT =      '19';    # no result was obtained      
our $ESLENODATA =        '20';    # no data provided, file empty
our $ESLETYPE =          '21';    # invalid type of argument   
our $ESLEOVERWRITE =     '22';    # attempted to overwrite data
our $ESLENOSPACE =       '23';    # ran out of some resource   
our $ESLEUNIMPLEMENTED = '24';    # feature is unimplemented   
our $ESLENOFORMAT =      '25';    # couldn't guess file format 
our $ESLENOALPHABET =    '26';    # couldn't guess seq alphabet
our $ESLEWRITE =         '27';    # write failed (fprintf, etc)

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
  Usage    : Bio::Easel::MSA->new
  Function : Generates a new Bio::Easel::MSA object
  Args     : <fileLocation>: file location of alignment
  Returns  : Bio::Easel::MSA object

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
  Function : Opens $fileLocation, reads first MSA, sets it.
  Args     : <fileLocation>: file location of alignment
  Returns  : void

=cut

sub read_msa { 
    my ($self, $fileLocation) = @_;

    if($fileLocation) { 
	$self->{path} = $fileLocation;
    }
    if(! defined $self->{path}) { 
	croak "trying to read msa but path is not set";
    }
    $self->{esl_msa} = _c_read_msa($self->{path}, $self->{esl_abc});
    return;
}

=head2 nseq

  Title    : nseq
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : $msaObject->nseq()
  Function : Gets number of seqs in MSA
  Args     : none
  Returns  : number of sequences (esl_msa->nseq)

=cut

sub nseq { 
    my ($self) = @_;

    $self->_check_msa(); 
    return _c_nseq($self->{esl_msa});
}

=head2 alen

  Title    : alen
  Incept   : EPN, Tue Jan 29 07:41:08 2013
  Usage    : $msaObject->alen()
  Function : Get alignment length.
  Args     : none
  Returns  : alignment length, number of columns (esl_msa->alen)

=cut

sub alen { 
    my ($self) = @_;

    $self->_check_msa(); 
    return _c_alen($self->{esl_msa});
}

=head2 get_sqname

  Title    : get_sqname
  Incept   : EPN, Mon Jan 28 09:35:21 2013
  Usage    : $msaObject->get_sqname($idx)
  Function : Returns name of sequence $idx in MSA.
  Args     : index of sequence 
  Returns  : name of sequence $idx (esl_msa->sqname[$idx])
             ($idx runs 0..nseq-1)

=cut

sub get_sqname { 
    my ($self, $idx) = @_;

    $self->_check_msa(); 
    $self->_check_sqidx($idx);
    return _c_get_sqname($self->{esl_msa}, $idx);
}

=head2 set_sqname

  Title    : set_sqname
  Incept   : EPN, Mon Jan 28 09:48:42 2013
  Usage    : $msaObject->set_sqname($idx, $newName)
  Function : Returns nothing
  Args     : index of sequence, new sequence name. 
  Returns  : void

=cut

sub set_sqname { 
    my ($self, $idx, $newname) = @_;

    $self->_check_msa(); 
    $self->_check_sqidx($idx);
    _c_set_sqname($self->{esl_msa}, $idx, $newname);
    return;
}

=head2 get_accession

  Title    : get_accession
  Incept   : EPN, Fri Feb  1 11:43:08 2013
  Usage    : $msaObject->get_accession()
  Function : Gets accession for MSA.
  Args     : none
  Returns  : the accession, a string

=cut

sub get_accession { 
    my ($self) = @_;

    $self->_check_msa(); 
    return _c_get_accession($self->{esl_msa});
}

=head2 set_accession

  Title    : set_accession
  Incept   : EPN, Fri Feb  1 11:11:05 2013
  Usage    : $msaObject->set_accession($acc)
  Function : Sets accession for MSA in <esl_msa>
  Args     : accession string to set
  Returns  : void

=cut

sub set_accession { 
    my ($self, $newacc) = @_;

    $self->_check_msa(); 
    my $status = _c_set_accession($self->{esl_msa}, $newacc);
    if($status != $ESLOK) { 
	croak "unable to set accession (failure in C code)"; 
    }
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

sub write_msa { 
    my ($self, $outfile, $format) = @_;

    $self->_check_msa(); 
    if(! defined $format) { 
	$format = "stockholm";
    }
    if($format ne "stockholm" && 
       $format ne "pfam" && 
       $format ne "afa") { 
	croak "format must be \"stockholm\" or \"pfam\" or \"afa\"";
    }
    my $status = _c_write_msa($self->{esl_msa}, $outfile, $format);
    if($status != $ESLOK) { 
	if   ($status == $ESLEINVAL) { croak "problem writing out msa, invalid format $format"; }
	elsif($status == $ESLFAIL)   { croak "problem writing out msa, unable to open output file $outfile for writing"; }
    }
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

    $self->_check_msa(); 
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

    $self->_check_msa(); 
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

    my $n;         # sqacc
    my ($s, $s2);  # start, from seq name (can be > $end)
    my ($e, $e2);  # end,   from seq name (can be < $start)
    my ($a, $a2);  # minimum of $s, $e
    my ($b, $b2);  # maximum of $s, $e
    my $strand;    # strand, 1 if $s < $e, else -1
    my $strand2;   # strand, 1 if $s2 < $e2, else -1
    my $i;         # counter over sequences
    my $is_nse;               # TRUE if $sqname adheres to format n/s-e
    my $overlap_exists = 0;   # have we seen an overlap?
    my $fract_overlap;        # fractional overlap
    my $max_fract      = 0.;  # maximum fraction of overlap
    my $max_sqname     = "";  # name of seq in sqinfoHHA 

    $self->_check_msa(); 
    if(! defined $self->{nseHAA}) { 
	$self->nse_createHAA;
    }

    # check for overlaps
    ($is_nse, $n, $a, $b, $strand) = $self->nse_breakdown($sqname);
    if($is_nse) { # TRUE if name matches name/start-end format
	if(exists $self->{nseHAA}->{$n}) { # TRUE if name is in nseHAA from MSA
	    for($i = 0; $i < scalar(@{$self->{nseHAA}->{$n}}); $i++) { 
		($s2, $e2, $a2, $b2, $strand2) = @{$self->{nseHAA}->{$n}[$i]};
		if($strand eq $strand2) { 
		    $fract_overlap = _overlap_fraction_strict($a, $b, $a2, $b2);
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

    my $nse;
    if($strand    ==  1) { $nse = $name . "/" . $a . "-" . $b; }
    elsif($strand == -1) { $nse = $name . "/" . $b . "-" . $a; }
    else { croak "invalid strand $strand (should be 1 or -1)\n"; }
    return $nse;
}

=head2 nse_sqlen

  Title    : nse_sqlen
  Incept   : EPN, Thu Jan 31 10:08:24 2013
  Usage    : $msaObject->nse_len($name);
  Function : Returns length of sequence given $nse,
           : where $nse is of format:
           : <sqacc>/<start>-<end>
           : and <start> may be > <end>.
  Args     : $nse: sequence name in <sqacc>/<start>-<end> format
  Returns  : Length in residues represented by $nse

=cut
sub nse_sqlen {
    my ($self, $nse) = @_;

    my $sqlen;
    if($nse =~ m/^\S+\/(\d+)\-(\d+)\s*/) {
	my ($start, $end) = ($1, $2);
	if($start <= $end) { $sqlen = $end - $start + 1; }
	else               { $sqlen = $start - $end + 1; }
    }
    else { 
	croak "invalid name $nse doesn't match name/start-end format\n";
    }
    return $sqlen;
}

=head2 average_id

  Title    : average_id
  Incept   : EPN, Fri Feb  1 06:59:50 2013
  Usage    : $msaObject->average_id($max_nseq)
  Function : Calculate and return average fractional identity of 
           : all pairs of sequences in msa. If more than $max_nseq
           : sequences exist in the seed, an average is computed
           : over a stochastic sample (the sample and thus the 
           : result with vary over multiple runs).
  Args     : max number of sequences for brute force calculation
  Returns  : average percent id of all seq pairs or a sample
  
=cut

sub average_id { 
    my ($self, $max_nseq) = @_;

    $self->_check_msa(); 
    if(! defined $max_nseq) { 
	$max_nseq = 100;
    }
    # average percent id is expensive to calculate, so we set it once calc'ed
    if (! defined $self->{average_id}) { 
	$self->{average_id} = _c_average_id($self->{esl_msa}, $max_nseq);
    }
    return $self->{average_id};
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

sub get_sqlen { 
    my ($self, $idx) = @_;

    $self->_check_msa(); 
    $self->_check_sqidx($idx);
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

sub average_sqlen { 
    my ($self) = @_;

    $self->_check_msa(); 
    # this could be expensive to calculate if nseq is very high, so we store it
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

sub calc_and_write_bp_stats {
    my ($self, $fileLocation) = @_;

    # TODO: get this working with errbuf, I couldn't get this to work though:
    # my $errbuf = "";
    #my $status = _c_calc_and_write_bp_stats($self->{esl_msa}, $fileLocation, $errbuf);
    $self->_check_msa(); 
    my $status = _c_calc_and_write_bp_stats($self->{esl_msa}, $fileLocation);
    if($status != $ESLOK) { croak "ERROR: unable to calculate and write bp stats"; }

    return;
}

=head2 addGF

  Title    : addGF
  Incept   : EPN, Fri Feb  1 17:43:38 2013
  Usage    : $msaObject->addGF($tag, $value)
  Function : Add GF tag/value to a C ESL_MSA object.
  Args     : $tag:   two letter tag 
           : $value: text for the line
  Returns  : void

=cut

sub addGF {
    my ($self, $tag, $value) = @_;

    $self->_check_msa(); 
    my $status = _c_addGF($self->{esl_msa}, $tag, $value);
    if($status != $ESLOK) { croak "ERROR: unable to add GF annotation"; }
    return;
}

=head2 addGS

  Title    : addGS
  Incept   : EPN, Fri Feb  1 17:43:38 2013
  Usage    : $msaObject->addGF($tag, $value)
  Function : Add GS tag/value for a specific sequence 
           : to a C ESL_MSA object.
  Args     : $tag:   two letter tag 
           : $value: text for the line
           : $sqidx: seq index to add GS for
  Returns  : void

=cut

sub addGS {
    my ($self, $tag, $value, $sqidx) = @_;

    $self->_check_msa(); 
    my $status = _c_addGS($self->{esl_msa}, $sqidx, $tag, $value);
    if($status != $ESLOK) { croak "ERROR: unable to add GS annotation"; }
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

    # don't call _check_msa, if we don't have it, that's okay
    _c_free_msa($self->{esl_msa});
    return;
}

=head2 revert_to_original

  Title    : revert_to_original
  Incept   : EPN, Sat Feb  2 14:53:27 2013
  Usage    : $msaObject->revert_to_original()
  Function : Frees current $msaObject->{esl_msa} object
             and rereads it from $msaObject->{path}
  Args     : none
  Returns  : void

=cut

sub revert_to_original {
    my ($self) = @_;

    if(! defined $self->{path}) { 
	croak "trying to revert_to_original but path not set";
    }
    $self->free_msa;
    $self->read_msa;
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


=head2 overlap_fraction

  Title    : overlap_fraction
  Incept   : EPN, Thu Jan 31 08:50:55 2013
  Usage    : overlap_fraction($from1, $to1, $from2, $to2)
  Function : Returns fractional overlap of two regions.
           : If $from1 is <= $to1 we assume first  region is 
           : on + strand, else it's on -1.
           : If $from2 is <= $to2 we assume second region is 
           : on + strand, else it's on -1.
           : If regions are on opposite strand, return 0.
  Args     : $from1: start point of first region (maybe < or > than $to1)
           : $to1:   end   point of first region
           : $from2: start point of second region (maybe < or > than $to2)
           : $to2:   end   point of second region
  Returns  : Fractional overlap, defined as nres_overlap / minL
             where minL is minimum length of two regions
=cut

sub overlap_fraction {
    my($from1, $to1, $from2, $to2) = @_;
    
    my $strand1;
    my $strand2; 
    $strand1 = ($from1 <= $to1) ? 1 : -1;
    $strand2 = ($from2 <= $to2) ? 1 : -1;

    if($strand1 != $strand2) { 
	return 0.; 
    }
    if($strand1 == 1) { 
	return _overlap_fraction_strict($to1, $from1, $to2, $from2); 
    }
    else { 
	return _overlap_fraction_strict($from1, $to1, $from2, $to2); 
    }
}

#############################
# Internal helper subroutines
#############################

=head2 _check_msa

  Title    : _check_msa
  Incept   : EPN, Sat Feb  2 13:42:27 2013
  Usage    : Bio::Easel::MSA->_check_msa()
  Function : Reads msa only if it is currently undefined
  Args     : none
  Returns  : void

=cut

sub _check_msa { 
    my ($self) = @_;

    if (! defined $self->{esl_msa}) { 
	$self->read_msa(); 
    }
    return;
}

=head2 _check_sqidx

  Title    : _check_seqidx
  Incept   : EPN, Sat Feb  2 13:46:08 2013
  Usage    : $msaObject->_check_sqidx($idx)
  Function : Check if $idx is in range 0..nseq-1,
             if not, croak.
  Args     : none
  Returns  : void

=cut

sub _check_sqidx { 
    my ($self, $idx) = @_;

    $self->_check_msa(); 
    my $nseq = $self->nseq;
    if($idx < 0 || $idx >= $nseq) { 
	croak "invalid sequence index %d (must be [0..%d])", $idx, $nseq;
    }
    return;
}

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


=head2 _overlap_fraction_strict

  Title    : _overlap_fraction_strict
  Incept   : EPN, Thu Jan 31 08:50:55 2013
  Usage    : _overlap_fraction_strict ($from1, $to1, $from2, $to2)
  Function : Returns fractional overlap of two regions.
           : Called 'strict' because $from1 must be <= $to1
           : and $from2 must be <= $to2. See non-strict version
           : overlap_fraction() for alternative.
  Args     : $from1: start point of first region (must be <= $to1)
           : $to1:   end   point of first region
           : $from2: start point of second region (must be <= $to2)
           : $to2:   end   point of second region
  Returns  : Fractional overlap, defined as nres_overlap / minL
             where minL is minimum length of two regions

=cut

sub _overlap_fraction_strict {
    my($from1, $to1, $from2, $to2) = @_;
    
    if($from1 > $to1) { croak "ERROR Bio-Easel's _overlap_fraction_strict, expect from1 <= to1 but $from1 > $to1"; }
    if($from2 > $to2) { croak "ERROR Bio-Easel's _overlap_fraction_strict, expect from2 <= to2 but $from2 > $to2"; }

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


=head2 _c_read_msa
=head2 _c_write_msa
=head2 _c_nseq
=head2 _c_alen
=head2 _c_get_accession
=head2 _c_set_accession
=head2 _c_get_sqname
=head2 _c_set_sqname
=head2 _c_any_allgap_columns
=head2 _c_average_id
=head2 _c_get_sqlen
=head2 _c_average_sqlen
=head2 _c_addGF
=head2 _c_addGS
=head2 _c_count_msa
=head2 _c_bp_is_canonical
=head2 _c_calc_and_write_bp_stats
=head2 dl_load_flags

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


=head1 ACKNOWLEDGEMENTS

Sean R. Eddy is the author of the Easel C library of functions for 
biological sequence analysis, upon which this module is based.

=head1 LICENSE AND COPYRIGHT

Copyright 2013 Eric Nawrocki.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of Bio::HMM::Logo

1;
