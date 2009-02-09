#!/usr/local/bin/perl

#
# Check flat - checks a flat file integrity. 
#

use strict;
use Getopt::Long;

my $versions_on;
my $pfamB_on;

GetOptions(
		   "v" => \$versions_on,
		   "b" => \$pfamB_on,
		   );

$| =1;
my ($pfam_id,$count,$size,$noseqs,$accessions);
my (%seqs, @nested_seqs, %pfams, @nested_pfams, $pfamB);
LINE: while(<>) {
    chop;

    if (!/^\# STOCKHOLM/) {
	print "Line '$_' is not Stockholm format (should be '# STOCKHOLM')";
    }

    $_ = <>; chop;
    my %hash = ();
	my $id;
    if (/^\#=GF\s+ID\s{3}([0-9a-zA-Z\-_]{1,15})/) {
	$pfam_id=$1;
	$hash{'ID'} = $pfam_id;
	$noseqs = 0;
    }
    else {
	print "Expecting an ID line in the file, found [$_]";
    }
 
    while(<>) {
	chop;
# Compulsory lines
	## Ordered as expected in file (as a nicity)

	## Pfam acc are in the fromat PF00001
	if (!$versions_on){
		/^\#=GF\s+AC\s{3}(PF\d{5})$/ && do { $hash{'AC'}++; $id = $1; $pfams{$id}++; next; };
	}else{
		/^\#=GF\s+AC\s{3}(PF\d{5})\.\d{1,3}$/ && do { $hash{'AC'}++; $id = $1; $pfams{$id}++; next; };
	}
	## Basically this is free text
	/^\#=GF\s+DE\s{3}.*/ && do { $hash{'DE'}++; next; };
	
	## SURNAME INITALS or (SURNAME INITALS,)* SURNAME INITALS
	/^\#=GF\s+AU\s{3}((\S+\s{1}\S{1,3}|SMART|LOAD)(\,\s{1})?)*$/ && do {$hash{'AU'}++; next; };
	
	# Alignment Methods.  This needs changing
	/^\#=GF\s+AL\s{3}/ && do {$hash{'AL'}++; next; };

	# Seed Source, basically free text
	/^\#=GF\s+SE\s{3}.*/ && do {$hash{'SE'}++; next; };

	## Thresholds
	/^\#=GF\s+GA\s{3}-?\d+\.{1}\d{2}\s{1}-?\d+\.{1}\d{2}\;$/ && do {$hash{'GA'}++; next; };
	/^\#=GF\s+TC\s{3}-?\d+\.{1}\d{2}\s{1}-?\d+\.{1}\d{2}\;$/ && do {$hash{'TC'}++; next; };
	/^\#=GF\s+NC\s{3}(-?\d+\.{1}\d{2}\s{1}-?\d+\.{1}\d{2}|undefined undefined)\;$/ && do {$hash{'NC'}++; next; };

	## Can be one of four types
	/^\#=GF\s+TP\s{3}(Domain|Family|Repeat|Motif)$/ && do {$hash{'TP'}++; next; };
	
	## Can be one of three options
	/^\#=GF\s+AM\s{3}(byscore|globalfirst|localfirst)$/ && do {$hash{'AM'}++; next; };
	
	## Needs to be four lines.
	if (/^\#=GF\s+BM\s{3}/) {
		$hash{'BM'}++;
		next;
	}
	
	if (/^\#=GF\s+SM\s{3}/) {
		$hash{'SM'}++;  
	  next;
	}
	
	## A number
	/^\#=GF\s+SQ\s{3}(\d+)$/ && do { $size = $1; $count = 0; $accessions = 0; last; };
	/^\/\// && do { $noseqs = 1; last;};

# Non compulsory fields
	##Previous ID - some names were changed as they were too long
	/^\#=GF\s+PI\s{3}(\S{1,25}\;\s?)*$/ && next;
	
	/^\#=GF\s+DC\s{3}/ && next;
	
	if (/^\#=GF\s+DR\s{3}(\S+);/){
		my $db = $1;
		if ($db eq "PDB"){
			next if (/\#=GF\s+DR\s{3}PDB;\s{1}\S{4}\s{1}\S{0,1};\s{1}-?\d{1,5};\s{1}-?\d{1,5};$/);
		}elsif($db eq "SMART"){
			next if (/\#=GF\s+DR\s{3}SMART;\s{1}\S+;$/);
		}elsif($db eq "SCOP"){
			next if (/\#=GF\s+DR\s{3}SCOP;\s{1}\S{4};\s\S{2};$/);
		}elsif($db eq "TC"){
			next if (/\#=GF\s+DR\s{3}TC;\s{1}\d+\.[A-Za-z]\.\d+;$/);
		}elsif($db eq "PFAMB"){
			$pfamB++;
			next if (/\#=GF\s+DR\s{3}PFAMB;\s{1}PB\d{6};$/);
		}elsif($db eq "PROSITE"){
			next if (/\#=GF\s+DR\s{3}PROSITE;\s{1}PDOC\d{5};$/);
		}elsif($db eq "PROSITE_PROFILE"){
			next if (/\#=GF\s+DR\s{3}PROSITE_PROFILE;\s{1}PS\d{5};$/);
		}elsif($db eq "PRINTS"){
			next if (/\#=GF\s+DR\s{3}PRINTS;\s{1}PR\d{5};$/);
		}elsif($db eq "HOMSTRAD"){
			next if (/\#=GF\s+DR\s{3}HOMSTRAD;\s{1}\S+;$/);
		}elsif($db eq "CAZY"){
			next if (/\#=GF\s+DR\s{3}CAZY;\s{1}\S{2,3}_\d+;$/);
		}elsif($db eq "MIM"){
			next if (/\#=GF\s+DR\s{3}MIM;\s{1}\d{6};$/);
		}elsif($db eq "MEROPS"){
			next if (/\#=GF\s+DR\s{3}MEROPS;\s{1}[A-Z]\S+;$/);
		}elsif($db eq "URL"){
			next if (/\#=GF\s+DR\s{3}URL;\s{1}http:\/\/\S+;$/);
		}elsif($db eq "LOAD"){
			next if (/\#=GF\s+DR\s{3}LOAD;\s{1}\S+;$/);
		}elsif($db eq "INTERPRO"){
			next if (/\#=GF\s+DR\s{3}INTERPRO;\s{1}IPR\d{6};$/);
		}

		


	}	
	## Reference Section
	/^\#=GF\s+RN\s{3}/ && next;
	/^\#=GF\s+RC\s{3}/ && next;
	/^\#=GF\s+RM\s{3}/ && next;
	/^\#=GF\s+RT\s{3}/ && next;
	/^\#=GF\s+RA\s{3}/ && next;
	/^\#=GF\s+RL\s{3}/ && next;
	
	
	# Comments, free text
	/^\#=GF\s+CC\s{3}/ && next;
	
	## Nested domains - Should be a nest location for every nested domain
	## Pfam accession
	/^\#=GF\s+NE\s{3}(PF\d{5});$/ && do{$hash{'NE'}++; push(@nested_pfams, "$id~$1"); next;};
	## Sequence and start/end points
	/^\#=GF\s+NL\s{3}(\S+)\/\d+-\d+/ && do{$hash{'NL'}++; push(@nested_seqs, "$id~$1");next;};
	

	print "1) In $pfam_id, got a bad  line [$_]\n";	
    }

    if( $hash{'AC'} != 1 ) {
	print ("Warning: $hash{'AC'} AC lines for $pfam_id\n");
    }
    if( $hash{'DE'} != 1 ) {
	print ("Warning: $hash{'DE'} DE lines for $pfam_id\n");
    }
    if( $hash{'AU'} != 1 ) {
	print ("Warning: $hash{'AU'} AU lines for $pfam_id\n");
    }
    if( $hash{'TC'} != 1 ) {
	print ("Printing: $hash{'TC'} TC lines for $pfam_id\n");
    }
    if( $hash{'NC'} != 1 ) {
	print ("Printing: $hash{'NC'} NC lines for $pfam_id\n");
    }
    if( $hash{'GA'} != 1 ) {
	print ("Printing: $hash{'GA'} GA lines for $pfam_id\n");
    }
    if( $hash{'SE'} != 1 ) {
	print ("Printing: $hash{'SE'} SE lines for $pfam_id\n");
    }
    if( $hash{'TP'} != 1 ) {
	print ("Printing: $hash{'TP'} TP lines for $pfam_id\n");
    }
    if( $hash{'BM'} != 1 ) {
	print ("Printing: $hash{'BM'} BM lines for $pfam_id\n");
	}
   if( $hash{'SM'} != 1 ) {
	print ("Printing: $hash{'SM'} SM lines for $pfam_id\n");
	}
	if($hash{'NE'} || $hash{'NL'}){
		if( $hash{'NE'} != $hash{'NL'} ){
			print "Miss-Match betweeen number of NE and NL lines for $pfam_id\n";
		}
	}


    if ($noseqs) {
	print ("Printing: There were no sequences in $pfam_id\n");
	next LINE;
    }


    my ($name,$start,$end,$count,$aln);
    my $alen = 0;
    while(<>) {
	chop;
	/^\#=GS\s+\S+\s+AC\s+(\S+)/ && do {
	    $accessions++;
		$seqs{$1}++;
	    next;
	};
	
	##=GS ADA_ECOLI/10-75 DR PDB; 1adn ; 10; 75;
	/^\#=GS\s{1}\S+\/\d+-\d+\s+DR PDB; \S{4}\s{1}\S{0,1}; -?\d+--?\d+;$/ && next;
	
	
	##=GC seq_cons
	/^\#=GC\s{1}seq_cons\s+\S+$/ && do {
	    $hash{'CONSENSUS'}++; next;
	};
	
	/^\#=GC\s+SS_cons\s+[CHGIEBTSX.-]*$/ && do {
		$hash{'SS_CONS'}++; next;
	};
	/^\#=GC\s+SA_cons\s+[0-9X.-]*$/ && do {
		$hash{'SA_CONS'}++; next;
	};
	##=GR SERA_ECOLI/10-105 SS 
	/^\#=GR\s+\S+\/\d+-\d+\s+SS\s+([CHGIEBTSX.-]*)$/ && do {
		$hash{'SS'}++; 
		next if $1 =~ /[CHGIEBTS]/;
	};
	
	##=GR SERA_ECOLI/10-105 AS
	/^\#=GR\s+\S+\/\d+-\d+\s+AS\s+[*.-]*$/ && next;

        ##=GR SERA_ECOLI/10-105 pAS
        /^\#=GR\s+\S+\/\d+-\d+\s+pAS\s+[*.-]*$/ && next;

        ##=GR SERA_ECOLI/10-105 sAS
        /^\#=GR\s+\S+\/\d+-\d+\s+sAS\s+[*.-]*$/ && next;

	#=GC RF	
	/^\#=GC\s+RF\s+([x.-]*)$/ && do {
		$hash{'RF'}++; 
		next;
	};

	/^\/\// && last;
	

	if(/^([A-Z0-9_]+)\/(\d+)\-(\d+)\s+([A-Za-z\.\-]+)/ ) {
          $name  = $1;
          $start = $2;
          $end   = $3;
          $aln   = $4;
          my $sequence = $aln;
	  
          $sequence =~ s/\.//g;
          $sequence =~ s/\-//g;
	  
          my $seqlen=length($sequence);
          my $expected_seqlen=$end-$start+1;
          if ($seqlen != $expected_seqlen){
            print "In $pfam_id, $name/$start-$end, start-ends are out of sync with sequence length ($expected_seqlen = expected sequence length, real = $seqlen)\n";
          }
	 	$count++;
		if( $alen != 0 ) {
	    	if( length $aln != $alen ) {
			print("In $pfam_id, line [$_] is a different length from other alignments!\n");
	    	}
		} else {
	    $alen = length $aln;
		}
		next;
	
	}
	print "2) In $pfam_id, got a bad line [$_]\n";	
    }
    
	
	if ($hash{'CONSENSUS'} != 1){
		print "No Consensus sequence for $pfam_id\n"; 
	}

	if ($count != $size) {
	print ("Counted $count seqs for $pfam_id but SQ was $size");
    }
    if ($count != $accessions) {
	print ("Counted $count seqs for $pfam_id but $accessions accession numbers");
    }
	if ($hash{'RF'}){
		if((!$hash{'NE'}) || (!$hash{'NL'})){
			print "Found #=RF line, but not NL/NE lines\n";
		}
	}
}
#Check that all NE's are valid	
#foreach my $pfam_nest (@nested_pfams){
#	my($pfam_acc, $nest_acc) = split(/~/, $pfam_nest);
#	if (!$pfams{$nest_acc}){
#		print "$pfam_acc has a nested domains that was not found in pfam\n";
#	}
#}

#Check that all NL's are valid
#foreach my $nested_seq (@nested_seqs){
#	my($pfam_acc, $seq_acc) = split(/~/, $nested_seq);
#	if (!$seqs{$seq_acc}){
#		print "$pfam_acc has a nested domain tied to a sequence no longer found in pfam\n";
#	}
#}

if($pfamB_on){
	if (!$pfamB){
		print "PfamB links are supposed to be in, but I have not found any\n";
	}
}
	


    
