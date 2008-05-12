#!/software/bin/perl -w

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

my $error;
if( !&check_timestamps( $family ) ) {
    $error = 1;
}
if( !&desc_is_OK( $family ) ) {
    $error = 1;
}

open( LOG, ">$family/format" ) or die;   
if( $error ) {
    print STDERR "$family: Your family contains errors\n";
    print LOG "$family: Your family contains errors\n";
    exit(1);
}
print STDERR "$family: No errors found\n";


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
    if ( -M "$family/OUTPUT" < -M "$family/ALIGN" ) {
        warn "$family: Your OUTPUT [$family/OUTPUT] is younger than your full alignment [$family/ALIGN].\n";
        $error = 1;
    }
    if( -M "$family/OUTPUT" < -M "$family/scores" ) {
        warn "$family: Your OUTPUT [$family/OUTPUT] is younger than your scores [$family/scores].\n";
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

    open(DESC, "$family/DESC") or die "Can't open $family/DESC file\n";
    while( <DESC> ) {
        chop;
        if( length $_ > 80 ) {
            warn "$family: Line greater than 80 chars [$_]\n";
            $error = 1;
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
		if ( /^ID   (\S+);$/ ) {
		    warn "$family: Bad formatting. Illegal semi colon at end of ID line [$_]";
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
                }
                last; };
            /^AU/ && do { $fields{$&}++; last; };
            /^TC/ && do {
                $fields{$&}++; 
                if ( !/TC   \S+\s*$/){
                    warn "$family: TC lines should look like:\nTC   23.00\n";
                    $error = 1;
                }
                last; 
            };
            /^NC/ && do {
                $fields{$&}++; 
                if ( !/NC   \S+\s*$/){
                    warn "$family: NC lines should look like:\nNC   11.00\n";
                    $error = 1;
                }
                last; 
            };
            /^SE/ && do { $fields{$&}++; last; };
            /^SS/ && do { 
		$fields{$&}++;
		if ( !/SS   (Published|Predicted)\;/){
                    warn "$family: SS line format incorrect [$_]\n";
                    $error = 1;
		}
		last;
	    };
            /^GA/ && do {
                $fields{$&}++; 
                if (! /^GA   \S+\s*$/){
                    warn "$family: GA lines should look like:\nGA   20.00;\nNot $_\n";
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
		if( not /^BM   cmbuild (-\S+ )?CM SEED$/ and 
		    not  ( /^BM   cmsearch (-\S+ )?-W \d+ (-\S+ )?CM SEQDB$/ or  /^BM   cmsearch\s+ --toponly  CM SEQDB$/  )) {
                    warn "$family: Your BM line doesn't look right [$_]\n";
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
		if (/^PI\s+$/){
		   $error = 1;
                    warn "$family: DESC files should not contain blank PI lines, please check and remove\n"; 
		   last;
	       }elsif ((! /^PI\s{3}(\S+;\s){1,10}/ ) || (! /;$/))   {
		   $error = 1;
                    warn "$family: DESC file PI lines wrongly formatted, please check format\n (missing space after internal semi-colon, or missing terminal semicolon?)\n"; 
		   last;
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
                    $error = 1;
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
                        $error = 1;
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
                        if( !/^DR   EXPERT;\s+\S+@\S+;$/ ) {
                            warn "$family: Bad expert reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
                    /^DR   MIR;\s+/ && do {
                        if( !/^DR   MIR;\s+MI\d+;$/ ) {
                            warn "$family: Bad MIR reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
                    /^DR   MIPF;\s+/ && do {
                        if( !/^DR   MIPF;\sMIPF\d+;$/ ) {
                            warn "$family: Bad MIPF reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
		     /^DR   snoRNABase;\s+/ && do {
                        if( !/^DR   snoRNABase;\s[0-9,A-Z,a-z,-]+\;$/ ) {
                            warn "$family: Bad snoRNABase reference [$_]\n";
                            $error = 1;
                            last SWITCH;
                        }
                        last SWITCH;
                    };
		    
		    /^DR   URL;\s+(\S+);$/ && do {
                       # warn "$family: Please check the URL $1\n";
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

