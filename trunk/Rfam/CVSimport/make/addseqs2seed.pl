#! /software/bin/perl -w

# A program to take sequences from either a fasta file or a list of ids (n/s-e) and build a SEED.new file using cmalign

use strict;
use Getopt::Long;
use SeqFetch;
use Rfam::RfamAlign;
use Bio::SimpleAlign;
use Rfam::SS;

my ($seqfile, $idfile, $iter, $help);

&GetOptions("f|s|seqfile=s"         => \$seqfile,
            "id|idfile=s"           => \$idfile,
	    "i|iter|iterate"        => \$iter,
	    "h|help"                => \$help
    );

if( $help || (defined($idfile) && defined($seqfile)) || (!defined($idfile) && !defined($seqfile) && !defined($iter)) || !(-e "SEED") ) {
    &help();
    exit(1);
}

if (defined($idfile)){
    
    open( ID, $idfile ) or die "$idfile exists but can't be opened\n[$!]";
    
    my (%forward, %reverse);
    while( my $line = <ID> ) {
	my ($valid, $name, $start, $end);
	if ($line =~ /(\S+)\/(\d+)\-(\d+)/){
	    $name = $1;
	    $start = $2;
	    $end = $3;
	    $valid=1;
	}
	elsif  ($line =~ /(\S+)\t(\d+)\t(\d+)/){
	    $name = $1;
	    $start = $2;
	    $end = $3;
	    $valid=1;
	}
	
	if (defined($valid) && $start < $end ){
	    print "fwd: $name $start $end\n";
	    push( @{ $forward{$name} }, { 'start'  => $start,
					  'end'    => $end} );
	}
	elsif (defined($valid) && $start > $end ){
	    print "rev: $name $start $end\n";
	    push( @{ $reverse{$name} }, { 'start'  => $end,
					  'end'    => $start} );
	}
	
    }
    
    $seqfile = $idfile . "\.fa";
    open( FA, ">$seqfile" ) or die;
    SeqFetch::fetchSeqs(\%forward, $Rfam::rfamseq, 0, \*FA);
    SeqFetch::fetchSeqs(\%reverse, $Rfam::rfamseq, 1, \*FA);
    close(FA) || die "Could not close fasta file:[$!]\n";
}

if (-e "SEED\.new"){
    print "WARNING: SEED.new exists, moving it sideways to SEED.new_moved_sideways\n";
    system("mv SEED.new SEED.new_moved_sideways");
}


system("/software/rfam/extras/infernal-0.81/src/cmbuild -F CM.81 SEED") and die("FATAL: Error in: [/software/rfam/extras/infernal-0.81/src/cmbuild -F CM.81 SEED].\n");

if (defined($iter)){
    $seqfile = "seed.fa";
    system("sreformat fasta SEED > $seqfile");
    open( SD, "SEED" ) or die ("FATAL: Couldn't open SEED [$!]\n $!\n");
    my $seed = new Rfam::RfamAlign;
    $seed -> read_stockholm( \*SD );
    close(SD);
    my @list = $seed->each_seq();
    my $self  = new Rfam::RfamAlign;

    foreach my $seqobj ( @list ) {
	
	my $seq = new Bio::LocatableSeq( '-seq'   => $seqobj->seq,
				     '-id'    => "DELME" . $$ . "." . $seqobj->id,
				     '-start' => $seqobj->start,
				     '-end'   => $seqobj->end, 
				     '-type'  => 'aligned'
	    );
        $self -> add_seq($seq);
    }
    my $ss_cons = $seed->ss_cons->getInfernalString();
    my $ss = new Rfam::SS;
    $ss -> parseInfernalString( $ss_cons );
    
    $self -> ss_cons( $ss );
    my $len = length($list[0]->seq); 
    my $tmpseed = "/tmp/$$.SEED";
    open(SDOUT, ">$tmpseed" ) or die ("FATAL: Couldn't open $tmpseed\n[$!]");
    Rfam::RfamAlign::write_stockholm($self, \*SDOUT, $len);
    close(SDOUT);
    system("/software/rfam/extras/infernal-0.81/src/cmalign --withpknots --withali $tmpseed -o /tmp/$$.SEED.new CM.81 $seqfile") and die( "FATAL: Error in [/software/rfam/extras/infernal-0.81/src/cmalign --withpknots --withali $tmpseed -o /tmp/$$.SEED.new CM.81 $seqfile].\n[$!]");
    system("sreformat --pfam stockholm /tmp/$$.SEED.new | grep -v ^DELME$$\. > /tmp/$$.SEED.new2" ) and die( "FATAL: Error in [sreformat --pfam stockholm /tmp/$$.SEED.new | grep -v ^DELME$$\. > /tmp/$$.SEED.new2]\n[$!]");
    system("sreformat --pfam stockholm /tmp/$$.SEED.new2 > SEED.new") and die( "FATAL: Error in [sreformat --pfam stockholm /tmp/$$.SEED.new2 > SEED.new]\n[$!]");
    printf "but don't look at it, this one is much more interesting: SEED.new\n";
#    system("/software/rfam/extras/infernal-0.81/src/cmalign -o SEED.new CM.81 $seqfile") and die( "FATAL: Error in [/software/rfam/extras/infernal-0.81/src/cmalign -o SEED.new CM.81 $seqfile].\n");
}
else {
    system("/software/rfam/extras/infernal-0.81/src/cmalign --withpknots --withali SEED -o SEED.new CM.81 $seqfile") and die( "FATAL: Error in [/software/rfam/extras/infernal-0.81/src/cmalign --withpknots --withali SEED -o SEED.new CM.81 $seqfile].\n[$!]");
}

exit();

######################################################################
sub help {
    
    print STDERR <<EOF;

addseqs2seed.pl - reads in either a fasta sequence file or a file of sequence ids and coordinates 
                - this can be either tab-delimited or in n/s-e format, one entry per line. The 
		sequences are aligned to the SEED file in the current directory using cmalign.

Usage:   addseqs2seed.pl -s  seqfile.fa
         addseqs2seed.pl -id  idfile
Options:       
-h or -help                    show this help
  -f|-s|-seqfile  <seqfile>    add sequences in seqfile to SEED
  -id|-idfile     <idfile>     add sequences corresponding to n/s-e\47s to SEED
  -i|-iter|-iterate            iteratively realign SEED sequences to the CM. NEEDS A METRIC! 
EOF
}







