
#
# PfamQC - an attempt to bring all the pfam quality control
# measures into one place
#
# sgj 
#

=head1 NAME

PfamQC

=head1 SYNOPSIS

Contains quality check routines typically to check local family prior to rcs check-in.

=cut

package Bio::Pfam::PfamQC;

use strict;
use Bio::Pfam;
use Bio::Pfam::DB;
use Bio::Pfam::AlignPfam;
use Bio::Index::Fasta;
use Bio::Index::Swissprot;

my @Pfam_Annotation_File_Set = @Bio::Pfam::annotation_file_set;
my @Pfam_Family_File_Set     = @Bio::Pfam::family_file_set;
my @Pfam_View_File_Set       = @Bio::Pfam::view_file_set;
my @Pfam_optional_file       = @Bio::Pfam::optional_file_set;
my @Pfam_output_files        = @Bio::Pfam::output_file_set;


=head2 all_format_checks_ok

 Title    : all_format_checks_ok
 Usage    : &PfamQC::all_format_checks_ok("family_id", 1)
 Function : Runs all format check routines from this module,
            and warns about any errors
 Returns  : 1 if all is OK, 0 if not
 Args     : family id, true flag if family is new

=cut

sub all_format_checks_ok {
# all format checks from routines below
    my $family = shift;
    my $family_is_new = shift;
    
    # check directory exists
    if( !-d "$family" ) {
	die "$family: directory does not exist locally\n";
    }

    my $error = 0;

    # Check length of $id
    if( ! &name_length_is_OK($family) ) {
	$error = 1;
	warn "$family: your family identifier must be less than 16 characters!\n";
    }

    # Check format of $id
    if( ! &name_format_is_OK($family) ) {
	$error = 1;
	warn "$family: your family identifier contains disallowed characters\n";
    }

    if( $family_is_new ) {
	# Check name is unused
		my $clash = &name_clashes($family);
		if( $clash ) {
	    	$error = 1;
	    	warn "$family: your family identifier clashes with existing id $clash\n";
		}
    }else{
		#Check that the accession and name match with stored copy
		if(&check_acc_vs_name($family)){
			$error = 1;
		}
	}

    # quick test to start with
    if( ! &load_ann_middleware($family) ) {
	$error = 1;
	warn "$family: failed to load annotation via middleware layer\n";
    }

    # check family files are intact
    if( ! &check_current_family_files($family) ) {
	$error = 1;
	warn "$family: $family family files are not intact\n";
    }

    # check desc file format
    if( ! &desc_is_OK($family) ) {
	$error = 1;
	warn "$family: $family annotation does not pass format checks\n";
    }

    # check seed alignment
    if( ! &mul_format_is_OK("SEED",$family) ) {
	warn "$family: Bad format for SEED!\n";
	$error = 1;
    }
    if( ! &alignment_is_flush("SEED",$family) ) {
	warn "$family: SEED is not flush!\n";
	$error = 1;
    }

    # check align
    if( ! &mul_format_is_OK("ALIGN",$family) ) {
	warn "$family: Bad format for ALIGN!\n";
	$error = 1;
    }
    if( ! &alignment_is_flush("ALIGN",$family) ) {
	warn "$family: ALIGN is not flush!\n";
	$error = 1;
    }

    # check outputs and hmms have been built with correct hmmer version

    if( !&correct_hmmer_version("HMM_ls", $family) ) {
	warn "$family: HMM_ls appears to be built with the wrong version of hmmer\n";
	$error = 1;
    }
    if( !&correct_hmmer_version("HMM_fs", $family) ) {
	warn "$family: HMM_fs appears to be built with the wrong version of hmmer\n";
	$error = 1;
    }

    # check HMM
    if( ! &HMM_is_calibrated("HMM_ls",$family) ) {
	warn "$family: Your HMM_ls has not been calibrated!\n";
	$error = 1;
    }
    if( ! &HMM_is_calibrated("HMM_fs",$family) ) {
	warn "$family: Your HMM_fs has not been calibrated!\n";
	$error = 1;
    }

    if( ! &scores_format_is_OK($family) ) {
	warn "$family: Your scores file is corrupt!\n";
	$error = 1;
    }

    if (! &more_in_align_than_seed($family)){
      warn "$family: You have more sequences in SEED than ALIGN\n";
      $error = 1;
    }

    if (! &compare_align_to_scores($family)){
      warn "$family: You have a different number of matches in scores and ALIGN files\n";
      $error = 1;
    }

    if ( &seed_has_all_gapped_columns($family)){
	warn "$family: your SEED contains all gapped columns\n";
	$error = 1;
    }

    if( $error ) {
	warn "$family: Family $family contains errors\n";
	return 0;  # failure
    } else {
	warn "$family: Family $family passes checks \n";
	return 1;  # success
    }
}


=head2 check_output_files

 Title    : check_output_files
 Usage    : &PfamQC::check_current_family_files("family_id")
 Function : Checks that the requisite files are present and correct in the
            local family directory
 Returns  : 1 is all is OK, 0 if there are problems
 Args     : family id

=cut

sub check_output_files {
    my $family = shift;
    my $error = 0;

    foreach my $file (@Pfam_output_files) {
	if( !(-e "$family/$file") ) {
	    warn "$family: $file does not exist\n";
	    $error = 1;
	}
    }
    if( -M "$family/HMM_ls" < -M "$family/PFAMOUT_ls" ) {
	warn "$family: Your HMM [$family/HMM_ls] is younger than your OUTPUT file [$family/PFAMOUT_ls].\n";
	$error = 1;
    }
    if( -M "$family/HMM_fs" < -M "$family/PFAMOUT_fs" ) {
	warn "$family: Your HMM [$family/HMM_fs] is younger than your OUTPUT file [$family/PFAMOUT_fs].\n";
	$error = 1;
    }
    if ( -M "$family/PFAMOUT_ls" < -M "$family/ALIGN" ) {
	warn "$family: Your OUTPUT [$family/PFAMOUT_ls] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }
    if ( -M "$family/PFAMOUT_fs" < -M "$family/ALIGN" ) {
	warn "$family: Your OUTPUT [$family/PFAMOUT_fs] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }
    if( -M "$family/PFAMOUT_ls" < -M "$family/scores" ) {
	warn "$family: Your OUTPUT [$family/PFAMOUT_ls] is younger than your scores [$family/scores].\n";
	$error = 1;
    }
    if( -M "$family/PFAMOUT_fs" < -M "$family/scores" ) {
	warn "$family: Your OUTPUT [$family/PFAMOUT_fs] is younger than your scores [$family/scores].\n";
	$error = 1;
    }

    if($error) {
	return 0;         # failure
    }
    else {
	return 1;         # success
    }
}



=head2 check_current_family_files

 Title    : check_current_family_files
 Usage    : &PfamQC::check_current_family_files("family_id")
 Function : Checks that the requisite files are present and correct in the
            local family directory
 Returns  : 1 is all is OK, 0 if there are problems
 Args     : family id

=cut

sub check_current_family_files {
    # no longer checks output files as these are not in rcs system
    my $family = shift @_;
    my $error = 0;

    foreach my $file (@Pfam_Annotation_File_Set, @Pfam_Family_File_Set) {
	if( !(-e "$family/$file") ) {
	    warn "$family: $file does not exist\n";
	    $error = 1;
	}
    }
    if( -M "$family/SEED" < -M "$family/HMM_ls" ) {
	warn "$family: Your SEED [$family/SEED] is younger than your HMM [$family/HMM_ls].\n";
	$error = 1;
    }
    if( -M "$family/SEED" < -M "$family/HMM_fs" ) {
	warn "$family: Your SEED [$family/SEED] is younger than your HMM [$family/HMM_fs].\n";
	$error = 1;
    }
    if ( -M "$family/HMM_ls" < -M "$family/ALIGN" ) {
	warn "$family: Your HMM [$family/HMM_ls] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }
    if ( -M "$family/HMM_fs" < -M "$family/ALIGN" ) {
	warn "$family: Your HMM [$family/HMM_fs] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }
    if ( -M "$family/HMM_ls" < -M "$family/scores" ) {
	warn "$family: Your HMM [$family/HMM_ls] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }
    if ( -M "$family/HMM_fs" < -M "$family/scores" ) {
	warn "$family: Your HMM [$family/HMM_fs] is younger than your full alignment [$family/ALIGN].\n";
	$error = 1;
    }

    if($error) {
	return 0;         # failure
    }
    else {
	return 1;         # success
    }
}


=head2 cutoffs_low

 Title    : cutoffs_low
 Usage    : &PfamQC::cutoffs_low("family_id")
 Function : Checks the DESC file cutoffs are sensible
 Returns  : 0 if all is OK, 1 if there are problems
 Args     : family id

=cut





=head2 desc_is_OK

 Title    : desc_is_OK
 Usage    : &PfamQC::desc_is_OK("family_id")
 Function : Checks the format of the DESC file
 Returns  : 1 if all is OK, 0 if there are problems
 Args     : family id

=cut

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
		if (! /^AC   PF(\d\d\d\d\d)$/ ) {
		    warn "$family: Bad accession line [$_]";
		    $error = 1;
		}
		last;
	    };
	    /^ID/ && do { 
		$error = 1;
		warn "$family: DESC files should not contain ID lines, please check and remove\n";
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
			$error = 1 ;
		}
		last; };
	    /^AU/ && do { 
			$fields{$&}++; 
			if (!/^AU\s{3}((\S+\s{1}\S{1,3}|SMART|LOAD)(\,\s{1})?)*$/){
				warn "$family:Your AU lines do not match the format \"Bloggs J\".  Make sure there is no whitespace at the end of the line! ";
				$error = 1; 
			}
		last; };
	    /^TC/ && do {
			$fields{$&}++; 
			if ( !/TC   \S+ \S+; \S+ \S+;\s*$/){
		    	warn "$family: TC lines should look like:\nTC   23.00 6.10; 10.00 0.40;\n";
		    	$error = 1;
			}elsif( /undefined undefined;/i){
		    	warn "$family: You have undefined trusted cutoffs!!  This is very naughty and is no longer allowed.\n";
		    	$error = 1;
			}
			last; 
	    };
	    /^NC/ && do {
		$fields{$&}++; 
		if ( !/NC   \S+ \S+; \S+ \S+;\s*$/){
		    warn "$family: NC lines should look like:\nNC   11.00 5.10; 10.00 0.40;\n";
		    $error = 1;
		}
		last; 
	    };
            /^AL/ && do { 		                     # restricted vocab
                $fields{$&}++; 
				print STDERR "AL lines are no longer required.  These will be removed shortly from all DESC files.";
				print STDERR "AL lines will never be seen in the flatfiles!!!\n";

                if(/^AL   Clustalw$/ ){                      # Made with Clustalw program
		} elsif (/^AL   Clustalw_manual$/){          # Made with Clustalw program with manual adjustment
		} elsif (/^AL   Clustalw_mask_\S{4,4}$/){    # Made with Clustalw program and structural mask
		} elsif (/^AL   Clustalv$/){                 # Made with Clustalv program
		} elsif (/^AL   MAFFT$/){                    
		} elsif (/^AL   HMM_built_from_alignment$/){ 
		} elsif (/^AL   Alignment kindly provided by SMART$/){ 
		} elsif (/^AL   HMM_simulated_annealing$/){ 
		} elsif (/^AL   Manual$/){ 
		} elsif (/^AL   Prodom$/){                   # Taken directly from Prodom 
		} elsif (/^AL   Prosite_pattern$/){          # Some alignments don't make complete sense!
		} elsif (/^AL   Structure_superposition$/){  # Alignment is based on structural superposition
		} elsif (/^AL   Domainer$/){                 # Domainer alignment
		} elsif (/^AL   pftools$/){                  # Alignment is based on pftools prosite profile
		} elsif (/^AL   converted_from_SMART$/){     # Alignment is converted from SMART
		} elsif (/^AL   T_Coffee$/){                 # Made with T-Coffee program
		} elsif (/^AL   Unknown$/){}                 # Alignment method is unknown

		else {
                    warn "$family: Your AL line [$_] is not part of restricted vocabulary. Bad news!\n";
                    $error = 1;
		}
		last; 
	    };
            /^TP/ && do { 		                     # restricted vocab
                $fields{$&}++; 
                if(/^TP   Family$/ ){
		} elsif (/^TP   Domain$/){
		} elsif (/^TP   Motif$/){
		} elsif (/^TP   Repeat$/){}
		else {
                    warn "$family: Your TP line [$_] is not part of restricted vocabulary. Bad news!\n";
                    $error = 1;
		}
		last; 
	    };
	    /^AM/ && do { 
		$fields{$&}++; 
                if(/^AM   globalfirst$/ ){
		} elsif (/^AM   localfirst$/){
		} elsif (/^AM   byscore$/){}
		else {
                    warn "$family: Your AM line [$_] is not part of restricted vocabulary. Bad news!\n";
                    $error = 1;
		}
		last; 
	    };
	    /^SE/ && do { $fields{$&}++; last; };
	    /^GA/ && do {
		$fields{$&}++; 
		if (! /^GA   \S+ \S+; \S+ \S+;$/){
		    warn "$family: GA lines should look like:\nGA   20 0; -10 -10;\nNot $_\n";
		    $error = 1;
		}
		last;
	    };
	    /^BM/ && do {
		$error = 1;
		warn "$family: DESC files should not contain BM lines, please check and remove\n";
		$fields{$&}++;
		last;
	    };
	    /^SQ/ && do {
		$error = 1;
		warn "$family: DESC files should not contain SQ lines, please check and remove\n";
		last;
	    };
	    # Non-Compulsory fields: These may be present
	    /^NE/ && do {
		if (/^NE\s+$/)
			{
		    	$error = 1;
		    	warn "$family: DESC files should not contain blank NE lines, please check and remove\n";
		    	last;
		  	}
		elsif(/^NE\s{3}(PF\d+)\;/)
			{
			$fields{$&}++;
			my @overlaps_allowed = $1;
			foreach my $acc (@overlaps_allowed)
				{
				#verify the PF numbers are valid
				my $pf_name;
				my $db = &Bio::Pfam::default_db();
    				$pf_name = $db->acc2id($acc);
				if (!defined $pf_name)
					{
					$error = 1;
		    			warn "$family: NE line: Invalid pfam accession number $acc;\n";
		    			last;
					}
				else
					{
					warn "$family: is allowed to overlap with $pf_name\n";
					next;
					}
				}
			} 
		else
			{
			$error = 1;
		    	warn "$family: NE lines incorrect format, should be PF00001;\n";
		  	last;
		  	}
		last;
		};
	/^NL/ && do {
		if (/^NL\s+$/)
			{
		    	$error = 1;
		    	warn "$family: DESC files should not contain blank NL lines, please check and remove\n";
		    	last;
		  	}
		elsif(/^NL\s{3}(\S+)\/(\d+)-(\d+)\;/)
			{
			$fields{$&}++;
			}
		else
			{
			$error = 1;
		    	warn "$family: NL lines incorrect format, should be NL  SEQ_ID/1-2;\n";
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
		} elsif (/(\w+):(\w+)/){
		    my $db=$1;
		    my $acc=$2;
		    if ($db eq 'Swiss'){

			unless ($acc =~ /^\S{6}$/){
			    $error = 1;
			    warn "$family: DESC file format for link to Swiss-Prot is wrong $acc is not a valid accession\n";
			}
		    }

		    if ($db eq 'Pfam'){
			unless ($acc =~ /^PF\d{5}/){
			    $error = 1;
			    warn "$family: DESC file format for link to Pfam is wrong $acc is not a valid accession\n";
			}
		    }
		
		    if (/(\w+):(\S+)/){
		    	$db=$1;
			$acc=$2;
		    	if ($db eq 'EC'){
			unless ($acc =~ /^\(?(\d+)\.(\d+|-)\.(\d+|-)\.(\d+|-)(\.|\,|\)){0,2}$/){
			    $error = 1;
			    warn "$family: DESC file format for link to EC is wrong $acc is not a valid accession\n";
			}
		    }}
		}


		last; 
	    };
	    /^PI/ && do { last; };
	    /^RC/ && do { last; };
	    /^DC/ && do { last; };
	    /^RT/ && do { last; };
	    /^RL/ && do { 
		if( !/^RL   .*\d{4};\d+:(\w*\d+)(?:-(\d+))?\.$/ and
		    !/^RL   .*\d{4};\d+:(\d+)\.$/  and
		    ! /^RL   .*\d+;\d+:RESEARCH.*$/ and
		    ! /^RL   .*\d+;\s+\[Epub ahead of print\].*$/
		  ) {
		    warn "$family: Bad reference line [$_]\nFormat is:    Journal abbreviation year;volume:page-page.\n";
		    $error = 1;
		} else {
		    my $start = $1;
		    my $end = $2;
		    $end = $start unless defined $end ;
		    $start = $end  unless $start ;
		    if( $start > $end ) {
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
	    /^RA/ && do {
	    if( !/^RA\s\s\s\S+/ ){
	    warn "$family: Bad RA line! [$_]\n";
	    }
	    last; };
	     
	    /^RM/ && do  {
		$fields{$&}++;
		$ref=1;
		if( !/^RM   \d{6,8}$/) {
		    warn "$family: Bad Medline reference [$_]\nShould be a six to eight digit number\n";
		    $error = 1;
		}
		last;
	    };
	    /^ED/ && do {
		$fields{$&}++;
		if( !/^ED   \S+\/\d+-\d+; \S+\/\d+-\d+;\s*$/ and !/^ED   \S+\/\d+-\d+;\s*/ ) {
		    warn "$family: Bad ED line [$_]\nShould be in form [ED  Q94354/1-68; Q94354/35-68;] or [ED  Q94354/1-68;]\n";
		    $error = 1;
		}
		last;
	    };
	    /^DR/ && do  {
		$ref=1;
		DBREF : { 
		    /^DR   PRINTS;\s/ && do {
			if( !/^DR   PRINTS;\s+(PR\d{5});$/ ) {
			    warn "$family: Bad prints reference [$_]\n";
			    $error = 1;
			    last SWITCH;
			}
			last SWITCH;
		    };
			# Expert Lines Removed
		    #/^DR   EXPERT;\s+/ && do {
			#if( !/^DR   EXPERT;\s+\S+@\S+;$/ ) {
			#    warn "$family: Bad expert reference [$_]\n";
			#    $error = 1;
			#    last SWITCH;
			#}
			#last SWITCH;
		    #};
		    /^DR   PROSITE;\s/ && do {
			if( !/^DR   PROSITE;\s+(PDOC\d{5});$/ ) {
			    warn "$family: Bad prosite reference [$_]\n";
			    $error = 1;
			    last SWITCH;
			}
			last SWITCH;
		    };
		    /^DR   PROSITE_PROFILE/ && do {
			if( !/^DR   PROSITE_PROFILE;\sPS\d{5};$/ ) {
			    warn "$family: Bad prosite reference [$_]\n";
			    $error = 1;
			    last SWITCH;
			}
			last SWITCH;
		    };
		    /^DR   HOMSTRAD/ && do {
			if( !/^DR   HOMSTRAD;\s\S+;$/ ) {
			    warn "$family: Bad homstrad reference [$_]\n";
			    $error = 1;
			    last SWITCH;
			}
			last SWITCH;
		    };
		    /^DR   TC/ && do {
			if( !/^DR   TC;\s\d+\.\w+\.\d+;$/ ) {
			    warn "$family: Bad TC reference [$_]\n";
			    $error = 1;
			    last SWITCH;
			}
			last SWITCH;
		    };
		    /^DR   SCOP;\s/ && do {
			if( !/^DR   SCOP;\s+\S{4};\s+(\w+);$/ ) {
			    warn "$family: Bad SCOP reference [$_]\n";
			    $error = 1;
			    last SWITCH;
			}
			my $tag = $1;
			if( $tag eq "sf" || $tag eq "fa" || $tag eq "pr") {
			    last SWITCH;
			} else {
			    warn "$family: Bad scop reference (must have sf or fa tag) [$_]\n";
			    last SWITCH;
			}
		    };
		    /^DR   URL;\s+(\S+);$/ && do {
			warn "$family: Please check the URL $1\n";
			last SWITCH;
		    };
		    /^DR   MIM; \d{6};$/ && do {
			last SWITCH;
		    };
		    /^DR   MEROPS; \S\d+;$/ && do {
			last SWITCH;
		    };
		    /^DR   LOAD; \S+;$/ && do {
			last SWITCH;
		    };
		    /^DR   CAZY; GH_\d+;$/ && do {
		        last SWITCH;
		    };  
		    /^DR   CAZY; GT_\d+;$/ && do {
			last SWITCH;
		    }; 
		    /^DR   CAZY; CBM_\d+;$/ && do {
			last SWITCH;
		    };
		    /^DR   CAZY; PL_\d+;$/ && do {
			last SWITCH;
		    };
		     /^DR   CAZY; CE_\d+;$/ && do {
			last SWITCH;
		    };
		    /^DR   SMART; \w+;$/ && do {
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
    if ($fields{AC} eq "0"){
	warn "$family: There is no accession line. This is fine for new families.\n";
    }
    if ($fields{AC} > 1){
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
    if ($fields{GA} ne "1"){
	warn "$family: There are [$fields{GA}] GA lines\n";
	$error = 1;
    }
    if ($fields{TP} ne "1"){
	warn "$family: There are [$fields{TP}] TP lines\n";
	$error = 1;
    }
    if ($fields{AM} ne "1"){
	warn "$family: There are [$fields{AM}] AM lines\n";
	$error = 1;
    }
    $fields{RN} = 0 if !exists $fields{RN};
    $fields{RM} = 0 if !exists $fields{RM};
    if ($fields{RN} ne $fields{RM}){
	warn "$family: There is a discrepancy between the number of RN ($fields{RN})and RM ($fields{RM})lines\n";
	$error = 1;
    }
    $fields{'NE'} = 0 if !exists $fields{'NE'};   # bizarre: {NE} gives warnings
    $fields{NL} = 0 if !exists $fields{NL};
    if ($fields{NL} ne $fields{'NE'}){
	warn "$family: There is a discrepancy between the number of NE and NL lines\n";
	$error = 1;
    }
	if ($fields{BM}){
		if ($fields{BM} != 4 ){
			warn "Incorrect number of BM lines: $fields{BM}.  There should be 4 BM lines.\n";
			$error = 1;
		}
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





=head2 family_overlaps_with_db

 Title    : family_overlaps_with_db
 Usage    : &PfamQC::family_overlaps_with_db("family_id", \@ignore, 1)
 Function : Checks that the alignment contains no overlaps to data in 
            the RDB, and prints any overlaps to STDERR
 Returns  : number of overlaps
 Args     : family_id, reference to an array of families to ignore, 
            optional flag to calculate end points

=cut

sub family_overlaps_with_db {
    my $family        = shift @_;
    my $ignore_ref    = shift @_;
    my $endpoints_opt = shift @_;
	
    my (%ignore, @overlaps);

    my $rdb = Bio::Pfam->live_rdb();
    my $defaultdb = Bio::Pfam->default_db();
   
    if( !-d "$family" ) {
	die "$family: can't find local directory\n";
    }

    my %nested = $rdb->get_nested_domain($family);  
 	foreach my $nest (keys %nested)
		{
		if ($nest eq $family)
			{
			push(@{$ignore_ref}, @{$nested{$nest}});
			}
		else
			{
			foreach my $nested_dom (@{$nested{$nest}})
				{
				if ($nested_dom eq $family)
					{	
					push(@{$ignore_ref}, $nest);
					}	
				}
			}
		}
    my @overlaps_allowed = _get_DESC_overlaps($family);
    foreach (@overlaps_allowed)
		{
		push(@{$ignore_ref}, $_);
		}

    # Test ignores are real families   
    foreach my $ignore ( @{$ignore_ref} ) {
	warn "Ignoring family $ignore\n";
	eval {
	    $defaultdb->id2acc($ignore);
	};
	$@ and die "The family $ignore does not seem to exist. Have you mistyped the name?\n";
	$ignore{$ignore} = 1;
    }

    # read alignments into objects
    my ($test_full, $test_seed);
    eval {
	$test_full = Bio::Pfam::AlignPfam->new();  $test_full->read_Pfam_file( "$family/ALIGN" );
	$test_seed = Bio::Pfam::AlignPfam->new();  $test_seed->read_Pfam_file( "$family/SEED" );
    };
    $@ and die "Could not read alignments for your family $family [$@]";

    # test for overlaps

    my %regions;
    foreach my $seq ($test_full->each_seq, $test_seed->each_seq) {
	push @{$regions{ $seq->id }}, Bio::Pfam::PfamRegion->new('-SEQ_ID'  => $seq->id(), 
                                                                     # seq->id() could be id or acc
								 '-FROM'    => $seq->start,
								 '-TO'      => $seq->end,
								 '-PFAM_ID' => $family);
    }

    # now %regions is a hash of anonymous arrays keyed on seq id 

    my @namelist = keys %regions;
    my $leftextent = my $rightextent = 10000;
    my $leftname   = my $rightname   = undef;

    foreach my $annseq ($rdb->get_AnnotSeqs(\@namelist, ['seed', 'full'])) {
	my @yourregs = sort { $a->from <=> $b->from } ($annseq->eachAnnotatedRegion);
	my @arr      = sort { $a->from <=> $b->from } @{$regions{ $annseq->id }};

	foreach my $myreg (@arr) {
	    foreach my $yourreg (@yourregs) {
		if (! $ignore{$yourreg->id}) {
		    if ($yourreg->from > $myreg->to) {
			# no overlap, but we need to update right extent
			if ($yourreg->from - $myreg->to - 1 < $rightextent) {
			    $rightextent = $yourreg->from - $myreg->to - 1;
			    $rightname = $annseq->id;
			}
			# because regions are sorted, we now know there are no 
			# overlaps, and the extensions will not be affected
			# by scanning the rest of the list. So...
			last;
		    }
		    elsif ($myreg->from > $yourreg->to) {
			# no overlap, but we need to update left extent
			if ($myreg->from - $yourreg->to - 1 < $leftextent) {
			    $leftextent = $myreg->from - $yourreg->to - 1;
			    $leftname = $annseq->id;
			}
		    }
		    else {
			# we have an overlap. Record it.
			push @overlaps, sprintf("%s:%s/%d-%d:%s/%d-%d", 
						$annseq->id,
						$myreg->id, $myreg->from, $myreg->to,
						$yourreg->id, $yourreg->from, $yourreg->to);
		    }		   		   
		}
	    }
	}   
    }

    open(LOG, ">$family/overlap") or die "Can't open $family/overlap file\n";
    foreach my $overlap ( @overlaps ) {
	# lines look like seq_name:domain-start-end:domain-start-end
	my ($nm,$dom1,$dom2) = split(/:/,$overlap);
	my ($n1, $s1, $e1) = ($dom1 =~ /([\w-]+)\/(\d+)-(\d+)/);
	my ($n2, $s2, $e2) = ($dom2 =~ /([\w-]+)\/(\d+)-(\d+)/);
	warn "Sequence [$nm] overlap $n1/$s1-$e1 with $n2/$s2-$e2\n";
	print LOG "Sequence [$nm] overlap $n1/$s1-$e1 with $n2/$s2-$e2\n";
    }
    close LOG;

    if( $endpoints_opt ) {
	print STDOUT sprintf("Max left extension %d in %s\n", $leftextent, $leftname);
	print STDOUT sprintf("Max right extension %d in %s\n",$rightextent, $rightname);
    }
    
    # Finally, test for internal overlaps...

    my @list = $test_seed->each_seq();
    for( my $seq = shift(@list); defined $seq; $seq = shift(@list) ) {
	foreach my $other ( @list ) {
	    if( $seq->id ne $other->id ) {
		next;
	    }
	    if( ($other->start() >= $seq->start() && $other->end() <= $seq->end()) || 
		($other->start() <= $seq->end() && $other->end() >= $seq->end()) ||  
		($other->start() <= $seq->start && $other->end() >= $seq->start()) ) {
	    
		printf STDERR ("Internal overlap of %s/%d-%d to %s/%d-%d\n",
				  $seq->id,
				  $seq->start,
				  $seq->end,
				  $other->id,
				  $other->start,
				  $other->end);
	    }
	}
    }

#    warn "Done. Found ", scalar(@overlaps), " external overlaps.\n";

    if( scalar(@overlaps) ) {
	return scalar(@overlaps);
    }
    else {
	return 0;
    }
}



=head2 local_dirs_overlap

 Title    : local_dirs_overlap
 Usage    : &PfamQC::local_dirs_overlap( \@dirs )
 Function : Check for overlaps in the supplied list of directories
 Returns  : 0 if all is OK, number of overlaps
 Args     : reference to list of directories (full paths safest)

=cut

sub local_dirs_overlap {

    my $dir = shift;
    my $fams = shift;
    my( %scores, %hash, %overlaps );
	open (OVERLAPS, ">$dir/overlaps") || die "Could not open $dir/overlap file: $!";
    foreach my $fam ( @$fams ) {
	# check is a directory
	if( ! -e "$dir/$fam/scores") {
	    warn "Cannot find directory $dir/$fam\n";
	    next;
	}
	
	open(AL,"$dir/$fam/scores") || die "Could not open $fam/scores $!";
	# read names into hash, concating as $family/start/end:
	# Also store score for each region
	while(<AL>) {
	    /^(\S+)\s+([\w0-9_]+\/\d+\-\d+)/ || die "Bad line in scores for $fam [$_]";
	    my ($name,$start,$end) = split(/ |\-|\//,$2);
	    # print "$_ Name is $name, $start $end\n";
	    my $fse = join('/',$start,$end,$fam);
	    $scores{"$name/$start/$end"}=$1;
	    $hash{$name} .= ':'.$fse;
	}

	open(SEED,"$dir/$fam/SEED") || die "Could not open $fam/SEED $!";
	# read names into hash, concating as $fam/start/end:
	$fam = "SEED!$fam";
	my $aln = new Bio::Pfam::AlignPfam;
	$aln -> read_selex( \*SEED );
	foreach my $seq ( $aln -> each_seq() ) {
	    my $fse = join('/',$seq->start,$seq->end,$fam);
	    $hash{$seq->id} .= ':'.$fse;
		}
	close(AL);
    }

    my $error = 0;

    # Loop over each protein to find overlaps

    foreach my $name ( keys %hash ) {
	my @domains = split(/:/,$hash{$name});

	for (my $testdomain=shift(@domains); defined $testdomain;$testdomain = shift(@domains)) {
	    $testdomain =~ /^\d/ || next;
	    my ($start,$end,$dom) = split(/\//,$testdomain);

	    # loop through the rest, check it does not overlap

	    foreach my $domain ( @domains ) {
		my ($nstart,$nend,$ndom) = split(/\//,$domain);
		#   print STDERR "Comparing $nstart $nend $ndom to $start $end $dom\n";
		my $score1=$scores{"$name/$start/$end"}; 
		my $score2=$scores{"$name/$nstart/$nend"}; 
		my $allowed = 0;
	  	#check that the domain $dom and $ndom do not occur in the overlap hash. 
		#	
		my ($dom1, $dom2); 
		if ($dom =~ /SEED!(\S+)/) {
		    $dom1 = $1;
		}
		else {
		    $dom1 = $dom;
		}
		if ($ndom =~ /SEED!(\S+)/) {
		    $dom2 = $1;
		}
		else {
		    $dom2 = $ndom;
		}
				
		if ( not exists $overlaps{$dom1} ) {
		    $overlaps{$dom1} = [ _get_DESC_overlaps($dir."/".$dom1) ];
		}
		if( $overlaps{$dom1} ) {
		    foreach (@{$overlaps{$dom1}}) {
			if ($_ eq $dom2) {
			    $allowed = 1;
			}
		    }
		}

		if ( not exists $overlaps{$dom2} ) {
		    $overlaps{$dom2} = [ _get_DESC_overlaps($dir."/".$dom2) ];
		}
		if( $overlaps{$dom2} ) {
		    foreach (@{$overlaps{$dom2}}) {
			if ($_ eq $dom1) {
			    $allowed = 1;
			}
		    }
		}

		if( $nstart <= $start && $nend >= $start && !$allowed ) {
		    if( (!($ndom =~ /SEED/) && !($dom =~ /SEED/)) || ($ndom =~ /SEED/ && $dom =~ /SEED/)) {
			print "(1) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			print OVERLAPS "(1) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			$error++;
		    }
		    	elsif ($dom1 ne $dom2){
		    	print "(1) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
		    	print OVERLAPS "(1) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			$error++;
			}  
  
		}
		elsif( $nstart <= $end && $nend >= $end && !$allowed ) {
		    if( (!($ndom =~ /SEED/) && !($dom =~ /SEED/)) || ($ndom =~ /SEED/ && $dom =~ /SEED/)) {
			print "(2) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			print OVERLAPS "(2) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			$error++;
		    }
		    	elsif ($dom1 ne $dom2){
			print "(2) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			print OVERLAPS "(2) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			$error++;
			}

		} 
		elsif( $nstart >= $start && $nend <= $end && !$allowed ) {
		    if( (!($ndom =~ /SEED/) && !($dom =~ /SEED/)) || ($ndom =~ /SEED/ && $dom =~ /SEED/)) {
			print "(3) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			print OVERLAPS "(3) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			$error++;
		    }
		    	elsif ($dom1 ne $dom2){
			print "(3) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			print OVERLAPS "(3) In $name: $dom/$start-$end ($score1 bits) overlaps with $ndom/$nstart-$nend ($score2 bits)\n";
			$error++;
			}
		}
	    }
	}
    }
	close(OVERLAPS) || die "Could not close $dir/overlap file: $!\n";
    return $error;
}


=head2 HMM_is_calibrated

 Title    : HMM_is_calibrated
 Usage    : &PfamQC::HMM_is_calibrated("HMM_file","family_id")
 Function : Checks that the file HMM_file in directory family_id has been 
            calibrated
 Returns  : 1 if all is OK, 0 if there are problems
 Args     : family id

=cut

sub HMM_is_calibrated {
    my $file = shift;
    my $family = shift;
    open(HMM, "$family/$file") or die "Can't open $family/$file - bad!\n";
    while(<HMM>) {
	if((/^COM\s+hmmcalibrate\s+\-\-seed\s+0/) or (/^COM\s+hmmcalibrate\s+\-\-cpu\s+1\s+\-\-seed\s+0/))  {
	    return 1;
	}
    }
    return 0;
}



=head2 load_ann_middleware

 Title    : load_ann_middleware
 Usage    : &PfamQC::load_ann_middleware("family_id")
 Function : Attempts to load the DESC files from family_id/ into the
            middleware layer
 Returns  : 1 is all is OK, 0 if there are problems
 Args     : family id

=cut

sub load_ann_middleware {
    my $family = shift @_;
    my $en = Bio::Pfam::EntryA_RCS->new();
    $en->id($family);
    $en->_directory("$family");
    $en->_desc_filename("DESC");
    eval {
	$en->ann; 
    }; 
    if ($@) {
	print STDERR "$@\n";
	return 0;          # failed to load annotation via middleware layer
    }
    else {
	return 1;          # success
    }
}




=head2 mul_format_is_OK

 Title    : mul_format_is_OK
 Usage    : &PfamQC::mul_format_is_OK("file","family_id")
 Function : Checks a mul format alignment for errors
 Returns  : 1 if all is OK, 0 if there are problems
 Args     : mul file, family id

=cut

sub mul_format_is_OK {
# can we replace this routine with loading into a simplealign object?
    my $file = shift @_;
    my $family = shift @_;
    my $error = 0;
    my $seen = 0;
    my $length = 0;
	my $rf_length = 0;
	my $s_length =0;
    my ($start,$end,$nse,$expected_length,$sequence,$name);

    open(MUL, "$family/$file") or die "Can't open $family/$file\n";
    while(<MUL>) {
	chop;
	$seen = 1;

	/^([A-Z0-9_]+)\/(\d+)\-(\d+)\s+([ABCDEFGHIKLMNPQRSTVWXYZabcdefghiklmnpqrstvwxyz\.\-]+)\s*$/ || do {
	    if ($_ !~ /\#=RF/)
		{
		warn "$family: [$_] looks like a bad mul format line\n";
	    	$error = 1;
	    	next;
		}
	   else
		{
		if (/#=RF\s+(\S+)/){
			$rf_length = length($1);
    		}

		next;
		}
	};
	$name = $1;
	$start = $2;
	$end = $3;
	$sequence = $4;
	$s_length = $4;
	if( $start >= $end or $start < 1 or $end < 1) {
	    warn "$family: [$_] has bad start/end points";
	    $error = 1;
	}

	# Check that length of sequence is end-start+1.
	$expected_length=$end-$start+1;
	$sequence =~ s/\.//g;
	$sequence =~ s/\-//g;
	if ($expected_length != length($sequence)) {
	    warn "$family: Your start-ends $name/$start-$end is out of sync with your sequence length\n ";
	    $error = 1;
	}

	# Checking that all the mul alignment data is lined up correctly 
	/^([A-Z0-9_]+\/\d+\-\d+\s+)/ or die "Mmmm - you shouldn't ever get this error!\n";
	$nse = $1;

	if( !$length ) {
	    $length = length($nse);
	} 
	else {
	    if( length($nse) != $length ) {
		warn "$family: looks like mul format is out of sync on this file\n";
		$error = 1;
	    }
	}
	}	
	if ($rf_length){
		if ($rf_length != length($s_length)){
		warn "$family: RF line length does not match alignment length $rf_length,".length($s_length)."\n";
		$error = 1;
		}
    }


    if( !$seen ) {
	warn "$family: empty alignment\n";
	$error = 1;
    }

    if( $error ) {
	return 0;
    }
    else {
	return 1;
    }
}



=head2 alignment_is_flush

 Title    : alignment_is_flush
 Usage    : &PfamQC::alignment_is_flush("SEED","family_id")
 Function : Checks that alignment is flush
 Returns  : 1 is its all OK, 0 if not
 Args     : alignment file name, family id

=cut

sub alignment_is_flush {
    my $file   = shift;
    my $family = shift;
    my $aln = new Bio::Pfam::AlignPfam;
    open( ALN, "$family/$file" ) or die "can't open Pfam alignment $family/$file";
    $aln -> read_Pfam( \*ALN );
    if( $aln -> is_flush() ) {
	return 1;
    }
    else {
	return 0;
    }
}


=head2 name_clashes

 Title    : name_clashes
 Usage    : &PfamQC::name_clashes("family_id","ignore")
 Function : Checks to see if the family name has been used before 
            in case independant fashion
 Returns  : id of any clashing family name or 0 if no clashes
 Args     : family id, optional 2nd id to ignore (when we want to use
	    pfmove to change the case of an id for example)

=cut

sub name_clashes {
# returns 0 if all is OK, id of clashing family if all is not OK
    my $family = shift;
    my $ignore = shift;
    my $lc_family = $family;
    $lc_family =~ tr/A-Z/a-z/;

    my $db = Bio::Pfam -> default_db();
    my @fam_list = $db->get_allacc();

    foreach my $acc (@fam_list) {
	my $id = $db -> acc2id("$acc");
	if( $ignore ) {
	    next if( $id eq $ignore );
	}
	my $lc_id = $id;
	$lc_id =~ tr/A-Z/a-z/;
	if( $lc_family eq $lc_id ) {
	    return $id;
	}
    }
    return 0;
} 



=head2 name_format_is_OK

 Title    : name_format_is_OK
 Usage    : &PfamQC::name_format_is_OK("family_id")
 Function : Checks the format of the family name supplied
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub name_format_is_OK {
    my $family = shift;
    if( $family =~ /^([A-Za-z0-9_\-]+)$/ ) {
	return 1;
    } 
    else {
	return 0;
    }
}



=head2 name_length_is_OK

 Title    : name_length_is_OK
 Usage    : &PfamQC::name_length_is_OK("family_id")
 Function : Checks the length of the family name
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub name_length_is_OK {
    my $family = shift;
    my $id_len = length($family);
    if( $id_len > 15 ) {
	return 0;
    }
    else {
	return 1;
    }
}


=head2 scores_format_is_OK

 Title    : scores_format_is_OK
 Usage    : &PfamQC::scores_format_is_OK("family_id")
 Function : Checks that the format of the scores file is correct
          : will also check that there is at least one line in the file
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub scores_format_is_OK {
    my $family = shift;

    my $count;  # See how many lines in scores file
    open(DESC, "$family/scores") or die "Can't open $family/scores file\n";
    while( <DESC> ) {
      if (/^-?\d+\.\d+ \S+\/\d+-\d+ [lf]s$/){  # Very strict pattern match
	$count++;
      } else {
	print STDERR "Incorrect format line in $family/scores file\n$_";
	return 0;
      }
    }
    close (DESC);

    if ($count>0){
      return 1;
    } else {
      print STDERR "$family: scores file appears to be empty!\n";
      return 0;
    }
}


=head2 more_in_align_than_seed

 Title    : more_in_align_than_seed
 Usage    : &PfamQC::more_in_align_than_seed("family_id")
 Function : Checks that the ALIGN file has more sequence
          : than the SEED alignment
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub more_in_align_than_seed {
    my $family = shift;

    my ($num_seed,$num_full);
    open (FH, "$family/SEED") or die "Can't open $family/SEED";
    while(<FH>){
      if (/^\S+\/\d+-\d+\s+\S+/){
	$num_seed++;
      }
    }
    close FH;


    open (FH, "$family/ALIGN") or die "Can't open $family/ALIGN";
    while(<FH>){
      if (/^\S+\/\d+-\d+\s+\S+/){
	$num_full++;
      }
    }
    close FH;

    if ($num_seed>$num_full){
      return 0;
    } else {
      return 1;
    }
}

=head2 compare_align_to_scores

 Title    : compare_align_to_scores
 Usage    : &PfamQC::compare_align_to_scores("family_id")
 Function : Checks that the number in ALIGN is the same as in scores
 Returns  : 1 if all is OK, 0 if not
 Args     : family id

=cut

sub compare_align_to_scores {
    my $family = shift;

    my ($num_align,$num_scores);
    open (FH, "$family/scores") or die "Can't open $family/scores";
    while(<FH>){
      if (/^-?\d+\.\d+ \S+\/\d+-\d+ [lf]s$/){
	$num_scores++;
      }
    }
    close FH;


    open (FH, "$family/ALIGN") or die "Can't open $family/ALIGN";
    while(<FH>){
      if (/^\S+\/\d+-\d+\s+\S+/){
	$num_align++;
      }
    }
    close FH;

    if ($num_align != $num_scores){
      return 0;
    } else {
      return 1;
    }
}


=head2 seed_has_all_gapped_columns 

 Title    : seed_has_all_gapped_columns
 Usage    : &PfamQC::seed_has_all_gapped_columns("family_id")
 Function : checks seed for all gapped columns
 Returns  : 0 if all is OK, 1 if we find any
 Args     : family id

=cut

sub seed_has_all_gapped_columns {
    my $family = shift;
    my $aln = new Bio::Pfam::AlignPfam;
    open( SEED, "$family/SEED" ) or die;
    $aln -> read_Pfam( \*SEED );
    my $new_aln = $aln->allgaps_columns_removed();
    if( $aln->length() != $new_aln->length() ) {
	return 1;
    }
    return 0;
}

=head2 correct_hmmer_version

 Title    : correct_hmmer_version
 Usage    : &PfamQC::correct_hmmer_version( file, family_id )
 Function : checks that HMM and OUTPUT files have been built with correct version of hmmer
 Returns  : 1 if all is OK, 0 if not
 Args     : file, family id

=cut

sub correct_hmmer_version {
    my $file = shift;
    my $family = shift;
    open( F, "$family/$file" ) or die "can't open $family/$file";
    while(<F>) {
	if(/HMMER2\.0\s+\[2\.2g\]/ or /HMMER 2\.3/ or /HMMER2\.0\s+\[2\.3\]/ or /HMMER2\.0\s+\[2\.3\.1\]/ or /HMMER2\.0\s+\[2\.3\.2\]/) {
	    return 1;
	}
	if( /Scores for complete sequences/ or /^EVD/ ) {
	    return 0;
	}
    }
    close F;
    return 0;
}


=head2 _get_DESC_overlaps

 Title    : _get_allowed_overlaps
 Usage    : $self->_get_DESC_overlaps("family_id")
 Function : Gets a list of families that are allowed to overlap with this family
 Returns  : @_
 Args     : family id

=cut

sub _get_DESC_overlaps 
	{
	my $family = shift;
	my @overlaps;
	if (-e "$family/DESC")
		{
		open (DESC, "$family/DESC") || die "can not open DESC file\n";
		while(<DESC>)
			{
			if ($_ =~ /^NE\s+(.*)$/)
				{
				my @overlaps_allowed = split /;/, $1;
                        	my $db = Bio::Pfam->default_db;
                        	foreach (@overlaps_allowed)
                           		{
                           		my $id = $db->acc2id($_); 
					push (@overlaps, $id);
					}
				}
			}
		close(DESC);
		}
	#

	return @overlaps;
	}


=head2 _get_flat_overlaps_all
 Title    : _get_flat_overlaps_all
 Usage    : $self->_get_flat_overlpas("$current_dir")
 Function : Gets a list of all families that are allowed to overlap
 Returns  : @_
 Args     : family id
=cut 

sub _get_flat_overlaps_all {
	my $fams = shift;
	my %nested;
	foreach my $path ( @$fams ) 
		{
		my( $dir, $fam );
		if( $path =~ /\//) 
			{
	    		( $dir, $fam ) = $path =~ ( /^(\S+)\/(\S+)$/ ); # get the last name
			}
		else 
			{
			$fam = $path;
			}
	# check is a directory
		if( ! -e "$dir/$fam/DESC") 
			{
	    		warn "Cannot find directory $dir/$fam\n";
	    		next;
			}
		my ($outer, $inner, @ids);
		open(DESC, "$dir/$fam/DESC") || die "Can not open $fam/DESC file"; 
		while(<DESC>)
			{
			if ($_ =~ /^AC   (PF\d+)$/)
				{
				$outer = $1;
				my $db = Bio::Pfam::default_db();
				$outer = $db->acc2id($outer);
				}
			elsif ($_ =~ /^NE   (.*)$/)
				{
				$inner = $1;
				my @accs = split /;/, $inner;
				my $db = Bio::Pfam::default_db();
				foreach (@accs)
					{
					my $id = $db->acc2id($_);
					push(@ids, $id);
					}
				}
			}
		close (DESC)|| die "Can not close $fam/DESC file"; 
		if (@ids)
			{
			$nested{$outer} = [@ids];
			$outer = undef;
			@ids = undef;
			}
		}
	return %nested
	}

=head2 ragged_seed

 Title    : ragged_seed
 Usage    : PfamQC::ragged_seed($family)
 Function : Checks the SEED to make sure that it is not too ragged at the ends
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : family id

=cut

sub ragged_seed {
	my  $family = shift;
	my $error = 0;
	open(ALIGN, "$family/SEED");
	
	my $aln = Bio::Pfam::AlignPfam->new();
	$aln->read_Pfam(\*ALIGN);

	my $no_seq = 0;
	my $no_lhd = 0;
	my $no_rhd = 0;
	foreach my $seq ( $aln->each_seq()){
		my $sequence = $seq->seq();
		if($sequence =~ /^(\.*)\S+/){
			$no_lhd += length($1);
		}
		if($sequence =~ /(\.*)$/){
			$no_rhd += length($1);
		}
		$no_seq++;
	}

	die "No sequences found in the seed. This is very bad\n"if (!$no_seq);
	my $bad_n=$no_lhd/$no_seq;
	my $bad_c=$no_rhd/$no_seq;

	if (($bad_n > 0.5) || ($bad_c > 0.5)){
		$error = 1;	
		printf STDERR "\n%-20s\t%7s\t%7s\n", "FAMILY", "N-term", "C-term";
		printf STDERR "%-20s\t%5.2f\t%5.2f\n", $family, $bad_n, $bad_c;
	}
	return $error;
}

=head2 sequence_checker

 Title    : sequence_checker
 Usage    : PfamQC::sequence_checher($family)
 Function : Checks the sequences in the SEED and ALIGN are valid.
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : family id or path 

=cut

sub sequence_checker {
	my $family = shift;
	my $inx = Bio::Index::Fasta -> new( '-filename' => $Bio::Pfam::pfamseqnew_inx_file );
	my $error = 0;
	my @aligns = qw( SEED ALIGN );
	foreach my $alnfile ( @aligns ) {
    	my $aln = new Bio::Pfam::AlignPfam;
    	open( ALN, "$family/$alnfile" ) or die "can't open $family/$alnfile";
    	$aln -> read_selex( \*ALN );
    	foreach my $seq ( $aln -> each_seq ) {
			my $pfamseq = $inx -> fetch( $seq->id );
			if( not $pfamseq) {
	    		print "$family/$alnfile: cannot find ".$seq->id." in pfamseq database\n";
	    		$error = 1;
	    		next;
			}	    
			my $str_ali = $seq -> seq();
			$str_ali =~ s/[.-]//g;
			$str_ali = uc( $str_ali );
			if( $seq->end > $pfamseq->length ) {
	    		print "$family/$alnfile: The sequence of ".$seq->id." does not match the pfamseq database\n";
	    		$error = 1;
	    		next;
			}	    
			my $str_pfamseq = $pfamseq -> trunc( $seq->start, $seq->end ) -> seq();
			if( $str_ali ne $str_pfamseq ) {
	    		print "$family/$alnfile: The sequence of ".$seq->id." does not match the pfamseq database\n";
	    		$error = 1;
	    		next;
			}
    	}
	}
	return $error
}

=head2 doggy_sequence_in_seed
 
 Title    : doggy_sequence_in_seed
 Usage    : PfamQC::doggy_sequence_in_seed($family)
 Function : Does exactly what you expect.  It identifies if there are any fragments in the SEED
            file.
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : family id

=cut

sub doggy_sequence_in_seed {
	my $family = shift;
	my %bad_seq;	
	open(ALIGN, "$family/SEED");
	my $aln = Bio::Pfam::AlignPfam->new();
	$aln->read_Pfam(\*ALIGN);

	foreach my $seq ( $aln->each_seq()){
		my $sequence = $seq->seq();
		if($sequence =~ /^(\.*)\S+/){
			if( length($1) > 15){
				$bad_seq{$seq->id}++;
			}
		}
		if($sequence =~ /(\.*)$/){
			if( length($1) > 15){
				$bad_seq{$seq->id}++;
			}
		}
	}
	
	my $error = 0;
	if ( keys %bad_seq){
		printf "%-20s\t%10s\n", "FAMILY", "SEQ";
	}
	foreach my $seq (keys %bad_seq){
		$error = 1;
		print STDERR "$seq look like a fragment in the seed\n";
	}
	return $error;
}




=head2  check_acc_vs_name
 
 Title    : check_acc_vs_name 
 Usage    : check_acc_vs_name($family)
 Function : Checks that the accesion in the DESC file and family matches that stored in RCS 
 Returns  : 1 if there are errors, 0 if the SEED is fine
 Args     : family id

=cut

sub  check_acc_vs_name {
  my $family = shift;
  my $error = 0;
  my $db = Bio::Pfam->default_db();
  open(DESC, "$family/DESC") || die "Could not open DESC file:[$!]\n";
  my $desc_acc;
  while(<DESC>){
    if(/^AC\s+(PF\d+)/){
      $desc_acc = $1;
      last;
    }
  }
  close(DESC);
  if($desc_acc){
    my $rcs_acc = $db->id2acc($family);
    my $rcs_family = $db->acc2id($desc_acc);
    if($rcs_acc ne $desc_acc){
      warn "\n$family acc:$desc_acc does not match rcs acc:$rcs_acc\n\n";
      $error = 1;
    }elsif ($rcs_family ne $family){
      warn "\n$family does not match rcs family name [expected $rcs_family]\n\n";
      $error = 1;
    }
  }
  else{
    warn "Assuming this family is new and does not have an accession\n";
  }
  return $error;
}



=head2 ed_valid

 Title    : ed_valid
 Usage    : &PfamQC::ed_valid( $dir, \@families )
 Function : Check if ED lines in DESC file are valid (ie that they actually overlap with something they should not)
 Returns  : nothing
 Args     : reference to list of directories (full paths safest)

=cut

sub ed_valid {

    my $dir = shift;
    my $fams = shift;
    my( %hash, %overlaps, %edits );

    foreach my $fam ( @$fams ) {
	# check is a directory
	if( ! -e "$dir/$fam/scores") {
	    warn "$dir/$fam is not a directory or does not contain a scores file\n";
	    next;
	}

	open(DESC, "$dir/$fam/DESC") || die "Could not open $fam/DESC $!";
        #read names into hash concating as start/end/fam:
        my ($name, $start, $end);
        while(<DESC>) {
	    if(/^ED\s+([\w0-9_]+)\/(\d+)\-(\d+)\;/) {    
	      $name = $1;
              $start = $2;
              $end = $3;
              my $fse = join('/', $start, $end, $fam);
              $edits{$name} .= $fse.':';
	  }
        }
        close(DESC);
	open(AL,"$dir/$fam/scores") || die "Could not open $fam/scores $!";
	# read names into hash, concating as $family/start/end:
	while(<AL>) {
	    /^(\S+)\s+([\w0-9_]+\/\d+\-\d+)/ || die "Bad line in scores for $fam [$_]";
	    my ($name2,$start2,$end2) = split(/ |\-|\//,$2);
	    # print "$_ Name is $name, $start $end\n";
	    my $fse = join('/',$start2,$end2,$fam);
	    $hash{$name2} .= $fse.':';
	}
	close(AL);
    }
    open (EDITS, ">$dir/removed_edits") || die "Could not open $dir/removed_edits file: $!";
    my $not_valid=0;
    foreach my $acc (keys %edits) {
	my @all_dom = split(/:/, $hash{$acc});
        my @ed_line = split (/:/, $edits{$acc});
        
        for(my $i=0; $i<=$#ed_line; $i++) {
	    my $edit_seq = $ed_line[$i];
            my ($start, $end, $dom) = split(/\//, $edit_seq);    
            my $valid=0;
            foreach my $scores_se (@all_dom) {
		my ($nstart, $nend, $ndom) = split(/\//, $scores_se);             
		my $allowed = 0;
	  	#check that the domain $dom and $ndom do not occur in the overlap hash. 
	                     
		if ( not exists $overlaps{$dom} ) {
		    $overlaps{$dom} = [ _get_DESC_overlaps($dir."/".$dom) ];
		}
		if( $overlaps{$dom} ) {

		    foreach (@{$overlaps{$dom}}) {
			if ($_ eq $ndom) {
			    $allowed = 1;
			}
		    }
		}

		if ( not exists $overlaps{$ndom} ) {
		    $overlaps{$ndom} = [ _get_DESC_overlaps($dir."/".$ndom) ];
		}
		if( $overlaps{$ndom} ) {
		    foreach (@{$overlaps{$ndom}}) {
			if ($_ eq $dom) {
			    $allowed = 1;
			}
		    }
		} 
		#print STDERR "($dom\/$acc) start: $start  end: $end\n($ndom\/$acc) nstart: $nstart nend: $nend\n";
		if (($nstart <= $start && $nend >= $start && !$allowed ) or ($nstart <= $end && $nend >= $end && !$allowed) or 
		    ($nstart >= $start && $nend <= $end && !$allowed)) {
		       $valid = 1;      #Edit line overlaps with something so it is valid
                }
                #print STDERR "valid: $valid\n\n";  
             } 
                
             # Edit lines which do not overlap with a domain are no longer required and need removing    
	     if($valid == 0) {
		  print STDERR "Edit line: '$acc/$start-$end' in family $dom is no longer required\n";
                  print EDITS "Edit line: '$acc/$start-$end' in family '$dom' is no longer required\n";
             }
	    
	}
    }                            
}

1;


