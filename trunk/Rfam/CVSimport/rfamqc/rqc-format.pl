#!/software/bin/perl -w

BEGIN {
    $rfam_mod_dir = "/nfs/team71/pfam/jd7/scripts/Modules/";
    #$rfam_mod_dir = "//software/rfam/scripts/Modules/";
}
use lib $rfam_mod_dir;
use strict;
use Rfam;

my $family = shift;
#Hash containing valid TP lines:
my %TP_hash = (	
		'Gene' => {
		    'tRNA' => 1,
		    'rRNA' => 1,
		    'miRNA' =>	1,
		    'ribozyme' => 1,
		    'antisense' => 1,
		    'CRISPR'  => 1,
		    'sRNA' => 1,
		    'snRNA' => {
			'splicing' => 1,
			'snoRNA' => {
				'CD-box' => 1,
				'HACA-box' => 1,
				'scaRNA' => 1,				    
				}
			}
		},
		'Intron' => 1,
		'Cis-reg' => {
		    'IRES' => 1,
		    'riboswitch' => 1,
		    'thermoregulator' => 1,
		    'frameshift_element' => 1,
		    'leader' => 1
		    }
		);

my $SOsuggestions="Common SO mappings:
         our standard for cis reg: 
           DR   SO:0005836 SO:regulatory_region
         our standard for gene:
           DR   SO:0001263 SO:ncRNA_gene
         riboswitches
           DR   SO:0000035 SO:riboswitch
         CRISPR:
           DR   SO:0001459 SO:CRISPR
         5' and 3' UTR elements
           DR   SO:0000204 SO:five_prime_UTR
           DR   SO:0000205 SO:three_prime_UTR
         microRNA
           DR   SO:0001244 SO:pre_miRNA
         snoRNAs
         CD and haca:
           DR   SO:0000594 SO:H_ACA_box_snoRNA
           DR   SO:0000593 SO:C_D_box_snoRNA
         Cajal body:
           DR   SO:0000275 SO:snoRNA
         or look up:
            http://www.sequenceontology.org/cgi-bin/miso.cgi
        ";

my $GOsuggestions="Common GO mappings:
         microRNA:
           DR   GO:0035195 GO:miRNA-mediated gene silencing
           DR   GO:0035068 GO:micro-ribonucleoprotein complex
         snoRNAs
         CD and haca:
           DR   GO:0006396 GO:RNA processing
           DR   GO:0005730 GO:nucleolus
         Cajal body:
           DR   GO:0015030 GO:Cajal body
         or look up: 
           http://www.geneontology.org/
        ";


my $error;
if( !&check_timestamps( $family ) ) {
    $error = 1;
}
if( !&desc_is_OK( $family ) ) {
    $error = 1;
}
if( !&seed_align_is_OK( $family ) ) {
    $error = 1;
}

open( LOG, ">$family/format" ) or die;   
if( $error ) {
    print STDERR "$family: Your family contains errors\n\n";
    print LOG "$family: Your family contains errors\n\n";
    exit(1);
}
print STDERR "$family: No errors found\n";




sub seed_align_is_OK{

    my $family = shift @_;
    my $error = 0;
    #read in the seed and align files using sreformat to check for formatting errors-only catputure STDERR
    my $output = `sreformat fasta $family/ALIGN 2>&1 1>/dev/null && sreformat fasta $family/SEED 2>&1 1>/dev/null`;
    
    if( $error ) {
        return 0;               # failure
    }
    else {
        return 1;               # success
    }

}


sub check_timestamps {
    my $family = shift;
    my $error;

    foreach my $file ( @Rfam::rcs_file_set ) {
	
        if( !(-s "$family/$file") ) {
            warn "$family: $file does not exist\n";
            $error = 1;
        }
    }
    
    if( -M "$family/SEED" < -M "$family/CM" ) {
        warn "$family: Your SEED [$family/SEED] is younger than your CM file [$family/CM].\n";
        $error = 1;
    }
    if( -M "$family/CM" < -M "$family/OUTPUT" ) {
        warn "$family: Your CM [$family/CM] is younger than your OUTPUT file [$family/OUTPUT].\n";
        $error = 1;
    }
    if( -M "$family/CM" < -M "$family/TABFILE" ) {
        warn "$family: Your CM [$family/CM] is younger than your TABFILE file [$family/TABFILE].\n";
        $error = 1;
   } 
    if ( -M "$family/OUTPUT" < -M "$family/ALIGN" ) {
        warn "$family: Your OUTPUT [$family/OUTPUT] is younger than your full alignment [$family/ALIGN].\n";
        $error = 1;
    }
    if ( -M "$family/TABFILE" <  -M "$family/ALIGN" ) {
	warn "$family: Your TABFILE [$family/TABFILE] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }	
    if( -M "$family/OUTPUT" < -M "$family/scores" ) {
        warn "$family: Your OUTPUT [$family/OUTPUT] is younger than your scores [$family/scores].\n";
        $error = 1;
    }
    if( -M "$family/TABFILE" <  -M "$family/scores" ) {
	warn "$family: Your TABFILE [$family/TABFILE] is younger than your scores [$family/scores].\n";
	$error = 1;
    }	
    
    if($error) {
        return 0;         # failure
    }
    else {
        return 1;         # success
    }

}


sub desc_is_OK {
    my $family = shift @_;
    my $error = 0;
    my $ref;
    my %fields;
    my $GOstring;
    my $SOstring;

    open(DESC, "$family/DESC") or die "Can't open $family/DESC file\n";
    while( <DESC> ) {
        chop;
        if( length $_ > 80 ) {
	    if (! $_ =~/^DR/ && $_ =~/^WK/ ){ 
	   	warn "$family: Line greater than 80 chars [$_]\n";
		$error = 1;
	    }
	}
        if( /\r/ ) {
            warn "$family: DESC contains DOS newline characters\n";
            $error = 1;
            last;
        }

        SWITCH : {
          # Compulsory fields: These must be present
            /^AC/ && do { 
                $fields{$&}++;
                if (! /^AC   RF(\d\d\d\d\d)$/ ) {
                    warn "$family: Bad accession line [$_]";
                    $error = 1;
                }
                last;
            };
            /^ID/ && do { 
                $fields{$&}++;
		if ( /^ID\s+(.*)$/) {
		    my $st=$1;
		    if ($1=~/[^a-zA-Z0-9\-\_]/){
			warn "$family: Bad formatting. Illegal character in ID line [$_]";
		    }
		}else{
		    warn "$family: Problem with ID line [$_]";
		}
                last;
            };
            /^DE/ && do { 
                $fields{$&}++; 
                if( !/^DE   .{1,80}/ ) {
                    warn "$family: DE lines should have 1 to 80 characters\n";
                    $error = 1;
                } elsif (/^DE.*s$/){
                    warn "$family: DE lines should not be plural!, please check and remove if plural\n";
                } elsif (/^DE.*\.$/){
		    warn "$family: DE lines should not end with a fullstop\n";
                } elsif (/^DE.*\;$/){
		    warn "$family: DE lines should not end with a semicolon\n";
                } elsif(/\t/){
		    warn "$family: DE lines contains a tab character\n";
		}
                last; };
            /^AU/ && do { $fields{$&}++; last; };
            /^TC/ && do {
                $fields{$&}++; 
                if ( !/TC   (\d+\.\d+)$/){
                    warn "$family: TC lines should look like:\nTC   23.00\n";
                    $error = 1;
                }
                last; 
            };
            /^NC/ && do {
                $fields{$&}++; 
                if ( (!/NC   \d+\.\d+$/) && ( !/NC   undefined$/) )  {
		    warn "$family: NC lines should look like:\nNC   11.00\nnot\n'$_'\n";
                    $error = 1;
                }
                last; 
            };
            /^SE/ && do { $fields{$&}++; last; };
            /^SS/ && do { 
		$fields{$&}++;
		#my $SS=$_;
		if ( (!/SS   (Published|Predicted)\;/) && ( !/SS   Pseudobase$/) ){
		    warn "$family: SS line format incorrect- check for terminal semicolons [$_]\n";
                    $error = 1;
		}
		if ( /Published/ && !/PMID\:(\d+)(\; \w+)?\b/){
		   warn "$family: SS line format incorrect- check PMID [$_]\n"; 
		   $error = 1; 
		}
		last;
	    };
            /^GA/ && do {
                $fields{$&}++; 
                if (! /^GA   \d+\.\d+$/){
                    warn "$family: GA lines should look like:\nGA   20.00;\nNot\n'$_'\n";
                    $error = 1;
                }
		last;
            };
	    /^TP/ && do {
		$fields{$&}++;
		my $TP;
		my $i =0;
		if (/^TP   (\w+.*)/){
		    $TP = "$1 ";
		}
		my @TPline = split /; /, $TP;
		foreach my $element (@TPline){
		    $i++;
		}
		if ($i == 1){
		    unless (exists $TP_hash{$TPline[0]}){
			print "$family: Invalid TP line: $TP\n";
			$error = 1;
		    }
		}
		
		elsif ($i == 2){
		    unless (exists $TP_hash{$TPline[0]}->{$TPline[1]} ){
			print "$family: Invalid TP line: $TP\n";
			$error = 1;
		    }
		}
		elsif ($i == 3){
		    unless (exists $TP_hash{$TPline[0]}->{$TPline[1]}->{$TPline[2]}){
			print "$family: Invalid TP line: $TP\n";
			$error = 1;
		    }
		}
		elsif ($i == 4){
		    unless (exists $TP_hash{$TPline[0]}->{$TPline[1]}->{$TPline[2]}->{$TPline[3]}){
			print "$family: Invalid TP line: $TP\n";
			$error = 1;
		    }
		}

                last;
	    };
            /^BM/ && do {
                $fields{$&}++;
		if( not /^BM   cmbuild\s+(\-\w\s)?CM SEED\;\s+cmcalibrate\s(\-\-mpi )?\-s\s\d\sCM$/ 
		    and not  ( /^BM\s+cmsearch  (\-\w\s\d+)?\s(\-\w \d+)?\s+\-\-toponly\s+(\-g\s+)?(\-\-fil\-no\-hmm\s+)?CM SEQDB$/
			     or    /^BM   cmsearch\s+(\-\-local\s+)?\-\-toponly  CM SEQDB$/ 
			       )) {
                    warn "$family: Your BM line doesn't look right [$_]\n";
		    $error = 1;
		}
                last;
            };
	    #check for WK and SO and GO here
	    /^WK/ && do {
                $fields{$&}++; 
                if (! /^WK   http:\/\/en.wikipedia.org\/wiki\/\S+$/){
                    warn "$family: WK lines should look like:\nWK   http:\/\/en.wikipedia.org\/wiki\/CRISPR;\nNot $_\nProblem most likely a terminal semicolon- remove it!";
                    $error = 1;
                }
		last;
            };
            /^SQ/ && do {
                $error = 1;
                warn "$family: DESC files should not contain SQ lines, please check and remove\n";
                last;
            };


            # Non-Compulsory fields: These may be present
	    /^PI/ && do {
		$fields{$&}++;	
		if (/^PI\s+$/ || /.*\;$/){
		   $error = 1;
                    warn "$family: DESC files should not contain blank PI lines or terminal semi colons on PI lines:  please check and remove\n"; 
		   last;
	       }
		my $PIline=$_;
		$PIline =~s/^PI   //g;
		my @ids=split("\; ", $PIline);
		foreach my $i (@ids){
		    if ($i=~/\;/){
			$error = 1;
			warn "$family: DESC file PI lines wrongly formatted, should be semicolon space separated list 'id1; ids2; id3' \n"; 	
			last;
		    }
		}
		
		last;
	    };

            /^\*\*/ && do { last; };  # These are ** lines which are confidential comments.
            /^CC/ && do {
                if (/^CC\s+$/){
                    $error = 1;
                    warn "$family: DESC files should not contain blank CC lines, please check and remove\n";
                    last;
		} elsif (/^CC.*-!-/){
                    $error = 1;
                    warn "$family: DESC files should not contain -!- in CC lines, please check and remove\n";
                }
                last; 
            };
            /^RT/ && do { last; };
            /^RL/ && do { 
		if( !/^RL   .*\d{4};\d+:(\w*\d+)(?:-(\w?\d+))?\.$/ ) {
                    warn "$family: Bad reference line [$_]\nFormat is:    Journal abbreviation year;volume:page-page.\n";
                   # $error = 1;
                } else {
                    my $start = $1;
		    my $end = $2;
		    if ($start=~/^[A-Z]/){
			$start=~s/^.//; 
		    }
                    if (($end) && ($end=~/^[A-Z]/ )) { 
			$end=~s/^.//;
		    }

		    if( $end and $start > $end ) {
                        warn "$family: Your reference line has a start ($start) bigger than end ($end)";
                        #$error = 1;
                    }
                }
                last; 
            };
            /^RN/ && do {
                $fields{$&}++;
                if( !/^RN   \[\d+\]/ ) {
                    warn "$family: Bad Ref No line [$_]\n";
                    $error = 1;
                }
                last;
            };
            /^RA/ && do { last; };
            /^RM/ && do  {
                $fields{$&}++;
                $ref=1;
                if( !/^RM   \d{6,8}$/) {
                    warn "$family: Bad Medline reference [$_]\nShould be a six to eight digit number\n";
                    $error = 1;
                }
                last;
            };
            /^DR/ && do  {
		$ref=1;
                DBREF : { 
                    /^DR   EXPERT;\s+/ && do {
                        if( !/^DR   EXPERT;\s+\S+@\S+$/ ) {
                            warn "$family: Bad expert reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
                    /^DR   MIR;\s+/ && do {
                        if( !/^DR   MIR;\s+MI\d+$/ ) {
                            warn "$family: Bad MIR reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
                    /^DR   MIPF;\s+/ && do {
                        if( !/^DR   MIPF;\sMIPF\d+$/ ) {
                            warn "$family: Bad MIPF reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
		     /^DR   snoRNABase;\s+/ && do {
                        if( !/^DR   snoRNABase;\s[0-9,A-Z,a-z,-]+$/ ) {
                            warn "$family: Bad snoRNABase reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };  
		    /^DR   snornadb;\s+/ && do {
			#URL; http://people.biochem.umass.edu/sfournier/fournierlab/snornadb/snrs/snR11_ta.php;]
                        if( !/^DR   snornadb;\ssnR\d+$/ ) {
                            warn "$family: Bad snornadb reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
		    /^DR   PKBASE;\s+/ && do {
			#URL; http://www.ekevanbatenburg.nl/PKBASE/PKB00218.HTML;
			if( !/^DR   PKBASE;\sPKB\d+$/ ) {
                            warn "$family: Bad PKBASE reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
		     /^DR   snorna\;\s/ && do {
			#DR   URL; http://www-snorna.biotoul.fr/plus.php?id=U54;
                        if( !/^DR   snorna;\s(\S+)$/ ) {
                            warn "$family: Bad snornadb reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
		    /^DR   snoopy\;\s/ && do {
			# http://snoopy.med.miyazaki-u.ac.jp/snorna_db.cgi?mode=sno_info&id=Loxodonta_africana300005
                        if( !/^DR   snoopy\;\s\S+$/ ) {
                            warn "$family: Bad snoopy reference [$_]\n";
                            $error = 1;
                            last SWITCH;
			}
                        last SWITCH;
                    };
		    /^DR   SO\:/ && do {
			$SOstring.=$_;
                        if( !/^DR   SO:\d+\sSO\:\S+/ ) {
                            warn "$family: Bad SO reference [$_]\n";
                            $error = 1;
                            last SWITCH;
			    #http://www.sequenceontology.org/miso/current_cvs/term/SO:0000655
                        }
                        last SWITCH;
                    };		 
		    /^DR   GO\:/ && do {
			$GOstring.=$_;
                        if( !/^DR   GO\:\d+\sGO\:\S+/ ) {
                            warn "$family: Bad GO reference [$_]\n";
                            #$error = 1;
                            last SWITCH;
			    #http://www.sequenceontology.org/miso/current_cvs/term/SO:0000655
                        }
                        last SWITCH;
                    };
   
		    /^DR   URL;\s+(\S+)$/ && do {
                        warn "$family: DR line is URL-maybe add this to our known db list? $1\n";
                        last SWITCH;
                    };
                    warn "$family: Bad reference line: unknown database [$_]\n";
                    $error = 1;
                    last SWITCH;
                };
            };
           
            warn "$family: Unrecognised DESC file line [$_]\n";
            $error = 1;
        }
    }

    # Check compulsory feilds
    # there shouldn't be a AC line before the family is created!

#    if ($fields{AC} eq "0"){
#        warn "$family: There is no accession line. SERIOUS ERROR.\n";
#        $error = 1;
#    }
    

    if (exists $fields{AC} and $fields{AC} > 1){
        warn "$family: There are [$fields{AC}] accession lines. SERIOUS ERROR.\n";
        $error = 1;
    }
    if ($fields{TC} != 1){
        warn "$family: There are [$fields{TC}] TC lines. SERIOUS ERROR.\n";
        $error = 1;
    }
    if ($fields{NC} != 1){
        warn "$family: There are [$fields{NC}] NC lines. SERIOUS ERROR.\n";
        $error = 1;
    }
    if ($fields{TP} != 1){
        warn "$family: There are [$fields{TP}] TP lines. SERIOUS ERROR.\n";
        $error = 1;
    }
    if ($fields{BM} != 2){
        warn "$family: There are [$fields{BM}] BM lines. SERIOUS ERROR.\n";
        $error = 1;
    }
    if ($fields{DE} > 1){
        warn "$family: There are [$fields{DE}] description lines. The description must be on a single line.\n";
        $error = 1;
    }
    if ($fields{AU} eq "0"){
        warn "$family: There are no author lines\n";
        $error = 1;
    }
    if ($fields{AU} ne "1"){
        warn "$family: There are [$fields{AU}] author lines, there should only be one author line\n";
        $error = 1;
    }
    if ($fields{SE} ne "1"){
        warn "$family: There are [$fields{SE}] SE lines\n";
        $error = 1;
    }
    if ($fields{SS} ne "1"){
        warn "$family: There are [$fields{SS}] SS lines\n";
        $error = 1;
    }
    if ($fields{GA} ne "1"){
        warn "$family: There are [$fields{GA}] GA lines\n";
       $error = 1;
    }
    if ($fields{WK} ne "1"){
        warn "$family: There are [$fields{WK}] WK lines. SERIOUS ERROR\n";
       $error = 1;
    }
 
    if (! defined $SOstring ){
	warn "$family: There are no SO mappings for this family. SERIOUS ERROR\n$SOsuggestions\n\n";
	$error=1;
    }
    if (! defined $GOstring ){
	warn "$family: There are no GO mappings for this family.\n$GOsuggestions\n\n";
    }
    

    $fields{RN} = 0 if !exists $fields{RN};
    $fields{RM} = 0 if !exists $fields{RM};
    if ($fields{RN} ne $fields{RM}){
        warn "$family: There is a discrepancy between the number of RN ($fields{RN})and RM ($fields{RM})lines\n";
        $error = 1;
    }

    if (($fields{PI}) && ($fields{PI} > 1)){
        warn "$family: There are [$fields{PI}] PI lines, please fix this \n";
       $error = 1;
    }

    close(DESC);

    if( !$ref ) {
        warn "$family: DESC has no references\n";
    }

    if( $error ) {
        return 0;               # failure
    }
    else {
        return 1;               # success
    }
}



