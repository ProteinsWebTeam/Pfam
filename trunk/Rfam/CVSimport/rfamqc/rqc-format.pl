#!/usr/local/bin/perl -w

use strict;
use Rfam;

my $family = shift;

my $error;
if( !&check_timestamps( $family ) ) {
    $error = 1;
}
if( !&desc_is_OK( $family ) ) {
    $error = 1;
}
   
if( $error ) {
    print STDERR "$family: Your family contains errors\n";
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
		unless( /^TP   Gene; tRNA;\s*$/ or /^TP   Gene; rRNA;\s*$/
		or /^TP   Gene; snRNA; splicing;\s*$/ or /^TP   Gene; snRNA; guide;\s*$/
		or /^TP   Gene; miRNA;\s*$/ or /^TP   Gene; ribozyme;\s*$/
		or /^TP   Gene; antisense;\s*$/ or /^TP   Gene; other;\s*$/
		or /^TP   Gene; RUF;\s*$/ or /^TP   Intron;\s*$/
		or /^TP   Cis-reg;\s*$/ or /^TP   Cis-reg; riboswitch\s*$/
		or /^TP   Cis-reg; IRES\s*$/
		) {
                    warn "$family: invalid TP line \"$_\"\n";
		    warn "Valid TP lines are as follows:\n";
		    warn "Gene; tRNA;\n";
		    warn "Gene; rRNA;\n";
		    warn "Gene; snRNA; splicing;\n";
		    warn "Gene; snRNA; guide;\n";
		    warn "Gene; miRNA;\n";
		    warn "Gene; ribozyme;\n";
		    warn "Gene; antisense;\n";
		    warn "Gene; other;\n";
		    warn "Gene; RUF;\n";
		    warn "Intron;\n";
		    warn "Cis-reg;\n";
		    warn "Cis-reg; riboswitch\n";
		    warn "Cis-reg; IRES\n";
                    $error = 1;
 		}
                last;
	    };
            /^BM/ && do {
		if( not /^BM   cmbuild / and not /^BM   cmsearch / ) {
                    warn "$family: BM lines should start with cmbuild or cmsearch\n";
		    $error = 1;
		}
                $fields{$&}++;
                last;
            };
            /^SQ/ && do {
                $error = 1;
                warn "$family: DESC files should not contain SQ lines, please check and remove\n";
                last;
            };
            # Non-Compulsory fields: These may be present
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
                if( !/^RL   .*\d{4};\d+:(\w*\d+)(?:-(\d+))?\.$/ ) {

                    warn "$family: Bad reference line [$_]\nFormat is:    Journal abbreviation year;volume:page-page.\n
";
                    $error = 1;
                } else {
                    my $start = $1;
                    my $end = $2;
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
                if( !/^RM   \d{7,8}$/) {
                    warn "$family: Bad Medline reference [$_]\nShould be a seven or eight digit number\n";
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
                    /^DR   URL;\s+(\S+);$/ && do {
                        warn "$family: Please check the URL $1\n";
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
        warn "$family: There are $fields{AC} accession lines. SERIOUS ERROR.\n";
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

