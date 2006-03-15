#
# Quality control for competing families within
#
# rdf

=head1 NAME

ClanQC

=head1 SYNOPSIS

Contains quality check routines typically to check competeing families 
prior tocheck-in.

=cut

package Bio::Pfam::Clan::ClanQC;
use strict;
use Bio::Pfam;
use Bio::Pfam::DB;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::PfamQC;
use Bio::AnnotationCollectionI;



my @clan_annotation = ('CLANDESC', '.list');

=head2 all_clan_format_checks_ok
	
 Title    : all_format_checks_ok
 Usage    : &CfamQC::all_format_checks_ok()
 Function : Runs all format check routines from this module,
            and warns about any errors. Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : family id, true flag if family is new

=cut
		
sub all_clan_format_checks_ok {

  my $clan_acc = shift;
  my $error = 0;
  #cross reference .list filei
  my ($match, @clans) = &cross_ref_list_and_dir;
  
  if (!$match){
    $error = 1;
    warn "The list of families that have undergone competion does not correspond to the directory structure\n";
  }
  

  ## Check the format of the Clandesc file
  open(CLANDESC, "CLANDESC") || die "Can not open the CLANDESC file for $clan_acc:$!\n";

  if(!&check_clandesc_file($clan_acc, \*CLANDESC)){
    $error = 1;
    warn "There are errors in your clandesc file format\n";
  }

  close(CLANDESC);
  ## foreach family do a normal check.
  foreach my $clan (@clans){
    if(!&Bio::Pfam::PfamQC::all_format_checks_ok($clan)){
      warn "$clan does not pass normal Pfam format checks!\n";
    }
    
    if(!&Bio::Pfam::PfamQC::check_output_files($clan)){
      $error = 1;
      warn "$clan does not pass normal output file checks!\n";	
    }
    
    if(!&clan_annotation){
      $error = 1;
      warn "These competing families have errors in the annotation\n";
    }
  } 
  #And finally 
  if ($error){
    return 0;         # failure
  }
  else {
    return 1;         # success
  }
}

=head2 all_clan_overlap_checks
	
 Title    : all_clan_overlap_checks
 Usage    : &CfamQC::all_clan_overlap_checks(@ignore)
 Function : Runs overlap checks internally within the clan and externally. 
			Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : Can supply a array of pfamA entries to ignore during the overlap checks (optional).
		    Note, this is only used for external overlap checks.

=cut

sub all_clan_overlap_checks{

  my @ignore = @_;
  my $error = 0;
  #cross reference .list file
  my ($match, @clans) = &cross_ref_list_and_dir;
  
  if (!$match){
    $error = 1;
    warn "The list of families that have undergone competion does not correspond to the directory structure\n";
  }
  
  
  # No overlap with non-clan families
  
  my ($result, $no_overlaps) = &check_no_clan_overlap(\@ignore, @clans);
  if(!$result){
    $error = 1;
    warn "Found $no_overlaps overlaps with Pfam entries outside the Clan set\n";
  }
  
  # No overlap between competing families family alignment
  my ($result_int, $no_int_overlap) = &check_overlap_within_clan(@clans);
  
  if(!$result_int){
    $error = 1;
    warn "Found $no_int_overlap internal overlaps within competing families\n";	
  }
  
  if ($error){
    return 0;         # failure
  }
  else {
    return 1;         # success
  }
}

=head2 all_clan_output_files
	
 Title    : all_clan_output_files
 Usage    : &CfamQC::all_clan_output_files
 Function : Checks that the the requisite files are present and correct for each clan member.
			Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : none

=cut


sub all_clan_output_files {
  my $error = 0;
  #cross reference .list file
  my ($match, @clans) = &cross_ref_list_and_dir;
  if (!$match){
    $error = 1;
    warn "The list of families that have undergone competion does not correspond to the directory structure\n";
  }
  
  foreach my $clan_member (@clans){
    if (!&Bio::Pfam::PfamQC::check_output_files( $clan_member )){
      $error = 1;
    }
  }
  
  if ($error){
    return 0;         # failure
  }
  else {
    return 1;         # success
  }
  
}

=head2 cross_ref_list_and_dir
	
 Title    : cross_ref_list_and_dir
 Usage    : &CfamQC::cross_ref_list_and_dir
 Function : Checks that the clan members that were competed are in present and that there
			are no other pfam families in the directory.
			Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : none

=cut

sub cross_ref_list_and_dir {
  #print STDERR "Checking Cfam list and directory structure\n";
  opendir(CLANDIR, ".") || die "can not open dir handle on current directory:$!\n";
  my @contents = grep{ $_ ne "." and $_ ne ".."}readdir(CLANDIR);
  my @clan_dirs;
  foreach (@contents){
    push(@clan_dirs, $_) if (-d "$_");
  }
  open(CLANLIST, ".list") || die "can not open list file:$!\n";
  my %clans;
  while(<CLANLIST>){
    chomp;
    $clans{$_}++;
  }
  
  my $mismatch = 0;
  foreach my $fam (@clan_dirs){
    if (!$clans{$fam}){
      warn "$fam is in the current working directory but is not part of the clan\n";
      $mismatch++;
    }
  }
  
  my @keys = keys %clans;
  ($mismatch++ && warn "number of clans dirs is less than that it was built with") if ($#keys != $#clan_dirs);
  
  if ($mismatch){
    return (0, @clan_dirs);         # failure
  }
  else {
    return (1, @clan_dirs);         # success
  }
  
}

=head2 pfam_status
	
 Title    : pfam_status
 Usage    : &CfamQC::pfam_status(@clan_members);
 Function : Checks that there is not more than 1 new Pfam family in the directory. 
 Returns  : 1 if all is OK, 0 if not
 Args     : Array of pfam ids that constitue the clan

=cut

sub pfam_status {
  my @clans = @_;
  my $db = Bio::Pfam::default_db();
  my $no_new =0;
  foreach my $clan (@clans){
    eval{
      $db->id2acc($clan);
    };
    $no_new++ if($@);	
  }
  if ($no_new > 1){
    return 0; #failure
  }
  else{
    return 1; #success
  }
}

=head2 clan_annotation
	
 Title    : clan_annotation
 Usage    : &CfamQC::clan_annotation();
 Function : Checks the clan_desc format. Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : None

=cut

sub clan_annotation{

  my $error = 0;
  my %fields;
  my $ref = 0;
  my $db = Bio::Pfam::default_db();
  my $clan = "newclan";
  foreach my $clan_file (@clan_annotation){
    $error = 1 if(!-s $clan_file);
  }
  
  open(CLANDESC, "CLANDESC") or die "Can't open CLANDESC file\n";
  while( <CLANDESC> ) {



    chop;
    if( length $_ > 80 ) {
      warn "CLANDESC line greater than 80 chars [$_]\n";
      $error = 1;
    }
    if( /\r/ ) {
      warn "CLANDESC contains DOS newline characters\n";
      $error = 1;
      last;
    }
    CLAN_DESC_LINE : {
      # Compulsory fields: These must be present
      /^AC/ && do { 
	$fields{$&}++;
	if (/^AC   (CL\d{4})(\.\d+)?$/ ) {
	  $clan = $1;
	}
	else {
	  warn "$clan: Bad accession line [$_]";
	  $error = 1;
	}
	last;
      };
      /^ID/ && do { 
	$fields{$&}++;
	if(/^ID   (\S+)$/){
	    ;
	}else{
	    warn "$clan: Bad ID line [$_]";
	    $error = 1;
	}
	last; 
      };
      /^DE/ && do { 
	$fields{$&}++; 
	if( !/^DE   .{1,80}/ ) {
	  warn "$clan: DE lines should have 1 to 80 characters\n";
	  $error = 1;
	} 
	elsif (/^DE.*s$/){
	  warn "$clan: DE lines should not be plural!, please check and remove if plural\n";
	} 
	elsif (/^DE.*\.$/){
	  warn "$clan: DE lines should not end with a fullstop\n";
	}
	last; 
      };
      
      /^AU/ && do { 
	$fields{$&}++; 
	last; 
      };
      
      #CC are compulsory for Clans
      /^CC/ && do {
	$fields{$&}++;
	if (/^CC\s+$/){
	  $error = 1;
	  warn "$clan: DESC files should not contain blank CC lines, please check and remove\n";
	  last;
	} 
	elsif (/^CC.*-!-/){
	  $error = 1;
	  warn "$clan: DESC files should not contain -!- in CC lines, please check and remove\n";
	} elsif (/(\w+):(\w+)/){
	  my $db=$1;
	  my $acc=$2;
	  if ($db eq 'Swiss'){
	    unless ($acc =~ /^\S{6}$/){
	      $error = 1;
	      warn "$clan: DESC file format for link to Swiss-Prot is wrong $acc is not a valid accession\n";
	    }
	  }
	  
	  if ($db eq 'Pfam'){
	    unless ($acc =~ /^PF\d{5}/){
	      $error = 1;
	      warn "$clan: DESC file format for link to Pfam is wrong $acc is not a valid accession\n";
	    }
	  }
	  
	  if (/(\w+):(\S+)/){
	    $db=$1;
	    $acc=$2;
	    if ($db eq 'EC'){
	      unless ($acc =~ /^\(?(\d+)\.(\d+|-)\.(\d+|-)\.(\d+|-)(\.|\,|\)){0,2}$/){
		$error = 1;
		warn "$clan: DESC file format for link to EC is wrong $acc is not a valid accession\n";
	      }
	    }	
	  }
	}
	last; 
      };
      #Non Compulsory fields
      /^RC/ && do { last; };
      /^RT/ && do { last; };
      /^RL/ && do { 
	if( !/^RL   .*\d+;\d+:(\d+)-(\d+)\.$/ and 
	    !/^RL   .*\d+;\d+:RESEARCH.*$/ and 
	    !/^RL   .*\d+;\s+\[Epub ahead of print\].*$/ and 
	    !/^RL   .*\d+;\d+:(\S+)\.$/) {
	  warn "$clan: Bad reference line [$_]\nFormat is:    Journal abbreviation year;volume:page-page.\n";
	  $error = 1;
	} 
	else {
	  my $start = $1;
	  my $end = $2;
	  if( $start > $end ) {
	    warn "$clan: Your reference line has a start ($start) bigger than end ($end)";
	    $error = 1;
	  }
	}
	last; 
      };
      /^RN/ && do {
	$fields{$&}++;
	if( !/^RN   \[\d+\]/ ) {
	  warn "$clan: Bad Ref No line [$_]\n";
	  $error = 1;
	}
	last;
      };
      /^RA/ && do { last; };
      /^RM/ && do  {
	$fields{$&}++;
	$ref=1;
	if( !/^RM   \d{6,8}$/) {
	  warn "$clan: Bad Medline reference [$_]\nShould be a six to eight digit number\n";
	  $error = 1;
	}
	last;
      };
      /^DR/ && do  {
	$ref = 1;
	/^DR   SCOP;\s/ && do {
	  if( !/^DR   SCOP;\s+\d{5,6};$/ ) {
	    warn "$clan: Bad SCOP reference [$_]. For Clan we want the sunids as this gives a unique idtag for the node in the branch\n";
	    $error = 1;
	    last CLAN_DESC_LINE;
	  }
	  last;
	};
	/^DR   CATH;\s/ && do {
	  if( !/^DR   CATH;\s+(\d+|\.)*;$/ ) {
	    warn "$clan: Bad CATH reference [$_].\n"; 
	    $error = 1;
	    last CLAN_DESC_LINE;
	  }
	  last;
	};
	/^DR   MEROPS;\s/ && do {
	  if( !/^DR   MEROPS;\s+(\S{1,3});$/ ) {
	    warn "$clan: Bad MEROPS reference [$_].\n"; 
	    $error = 1;
	    last CLAN_DESC_LINE;
	  }
	  last;
	};
	
	warn "$clan: Bad reference line: unknown database [$_]\n";
	$error = 1;
	last CLAN_DESC_LINE;
      };
      /^MB   (PF\d{5})/ && do  {
	#Make sure that the MB line AC is vaild for Pfam entry
	my $acc = $1;
	eval{
	  $db->acc2id($acc);
	};
	if($@){
	  warn "$acc is not a vaild accession number\n";
	  $error = 1;
	  last CLAN_DESC_LINE;
	}	
	#Need to make sure that these are vaild for the family
	
	last;
      };
      warn "$clan: Unrecognised DESC file line [$_]\n";
      $error = 1;
    }
  }# end while
  close(CLANDESC);
  # Check compulsory feilds are present and in the expected number
  if (!$fields{AC}){
    warn "$clan: There is no accession line. This is fine for new families.\n";
  }
  if ($fields{AC} > 1){
    warn "$clan: There are $fields{AC} accession lines. SERIOUS ERROR.\n";
    $error = 1;
  }
  if ($fields{ID} > 1){
    warn "$clan: There are [$fields{ID}] ID lines. The identifing lines must be on a single line.\n";
    $error = 1;
  }
  if(!$fields{ID}){
    warn "There are no ID lines\n";
    $error =1;
  }
  if ($fields{DE} > 1){
    warn "$clan: There are [$fields{DE}] description lines. The description must be on a single line.\n";
    $error = 1;
  }
  if(!$fields{DE}){
    warn "There are no DE lines\n";
    $error =1;
  }
  if (!$fields{AU}){
      warn "$clan: There are no author lines\n";
      $error = 1;
  }
  if ($fields{AU} ne "1"){
    warn "$clan: There are [$fields{AU}] author lines, there should only be one author line\n";
    $error = 1;
  }
  if (!$fields{CC} ){
    warn "$clan:  There are no CC lines.  There should be at least some lines describing the family!\n";
    $error = 1 ;
  }
  $fields{RN} = 0 if !exists $fields{RN};
  $fields{RM} = 0 if !exists $fields{RM};
  if ($fields{RN} ne $fields{RM}){
    warn "$clan: There is a discrepancy between the number of RN ($fields{RN})and RM ($fields{RM})lines\n";
    $error = 1;
  }
  
  if( !$ref ) {
    warn "$clan: CLANDESC has no references\n";
  }
  
  if( $error ) {
    return 0;               # failure
  }
  else {
    return 1;               # success
  }
}

=head2 check_overlap_within_clan
	
 Title    : check_overlap_within_clan
 Usage    : &CfamQC::check_overlap_within_clan()
 Function : Runs internal overlap checks. 
			Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : None 

=cut

sub check_overlap_within_clan {
  my @clans = @_;
  print STDERR "Checking for Internal Overlaps between @clans\n";
  my $overlap = &Bio::Pfam::PfamQC::local_dirs_overlap(".", \@clans);
  if ($overlap){
    return (0, $overlap);         # failure
  }
  else {
    return 1;         # success
  }
}

=head2 check_no_clan_overlap
	
 Title    : check_no_clan_overlap
 Usage    : &CfamQC::check_no_clan_overlap(\@ignore)
 Function : Looks for external overlaps. 
			Expects to be in the clan directory.
 Returns  : 1 if all is OK, 0 if not
 Args     : Can supply a array ref of pfamA entries to ignore during the overlap checks (optional).

=cut

sub check_no_clan_overlap {
  my ($ignore_ref, @clan_members) = @_;
  my $db = Bio::Pfam::default_db();
  my @ignore = @$ignore_ref;
  foreach my $clan (@clan_members){
    eval{
      $db->id2acc($clan);
    };
    push(@ignore, $clan) if(!$@);	
  }
  my $overlap_tot = 0;
  foreach my $clan (@clan_members){
    print STDERR "Checking External overlap for $clan\n";
    my $overlap = &Bio::Pfam::PfamQC::family_overlaps_with_db($clan, \@ignore);
    $overlap_tot += $overlap;
  }
  if ($overlap_tot){
    return (0, $overlap_tot);         # failure
  }
  else {
    return 1;         # success
  }
}

=head2 clan_fidelity
	
 Title    : clan_fidelity
 Usage    : &CfamQC::clan_fidelity($clan_acc, @families)
 Function : Makes sure that a pfamA family only belongs to the clan specified. 
 Returns  : 1 if all is OK, 0 if not
 Args     : clan accesion (CL0001) and an array of pfam entries

=cut

sub clan_fidelity{
  my ($clan_acc, @families) = @_;
  my $rdb = Bio::Pfam::live_rdb;
  my $error = 0;
  foreach my $family (@families){
    eval{ 
      $rdb->id2acc($family)
    };
    next if ($@); # Probably a new family
    my @clans = $rdb->query("select distinct clan_acc from clans join clan_membership, pfamA where clans.auto_clan=clan_membership.auto_clan and pfamA.auto_pfamA=clan_membership.auto_pfamA and pfamA_id=\"$family\" and clan_acc!=\"$clan_acc\"");
    if (@clans){
      warn "$family is found in this clan:$clan_acc and clan:".${$clans[0]}[0]."\n";
      $error++;
    }
  }
 return $error;
}

=head2 clan_QC_checks_performed_ok
	
 Title    : clan_QC_checks_performed_ok
 Usage    : &Bio::Pfam::PfamQC::clan_QC_checks_performed_ok($clan_acc);
 Function : Makes sure that all format checks have been complete and that they were performed
			after the clan was made.
 Returns  : 1 if all is OK, 0 if not
 Args     : Can supply a array of pfamA entries to ignore during the overlap checks (optional).
		    Note, this is only used for external overlap checks.

=cut


sub clan_QC_checks_performed_ok{
  my ($clan_acc) = @_;
  # Go through an check all QCs 
  if (!-e "$clan_acc/.list"){
    die "$clan_acc has not been made correctly !\n";
  }
  if (!-e "$clan_acc/.spell"){
    die "$clan_acc spell checks have not been performed!\n";
  }
  if (!-e "$clan_acc/.format"){
    die "$clan_acc format checks have not been performed!\n";
  }
  if (!-e "$clan_acc/.nooverlaps"){
    die "$clan_acc overlap checks have not been performed!\n";
  }
  if (!-e "$clan_acc/.pqc_all_members"){
    die "$clan_acc members have not been checked !\n";
  }
  my $error = 0;
  if(-M "$clan_acc/.list" < -M "$clan_acc/.spell"){
    warn "$clan_acc: Your clan has been made after the spell check\n";
    $error = 1;
  }
  if(-M "$clan_acc/.list" <  -M "$clan_acc/.format"){
    warn "$clan_acc: Your clan has been made after the format check\n";
    $error = 1;
  }
  if(-M "$clan_acc/.list" <  -M "$clan_acc/.nooverlaps"){
    warn "$clan_acc: Your clan has been made after the overlap check\n";
    $error = 1;
  }
  #if(-M "$clan_acc/.nooverlaps" > -M  "$clan_acc/.pqc_all_member"){
  #	warn "$clan_acc: Your clan has been made after the spell check\n";
  #	$error = 1;
  #}
  if (!$error){		
    return 1; # success, everything is okay
  }
  else {
    return 0; #Something is wrong with the test
  }
}

=head2 pfam_in_clan
	
 Title    : pfam_in_clan
 Usage    : &CfamQC::pfam_in_clan($pfam_id);
 Function : Finds out if a pfam family is present in a clan or not. 
 Returns  : 1 if found in a clan, 0 if not
 Args     : scalar containing the pfam id.

=cut


sub pfam_in_clan {
  my $pfam = shift;
  my $rdb = Bio::Pfam->live_rdb;
  
  my $acc = $rdb->id2acc($pfam);
  warn "$pfam is not a valid id\n" if ($acc !~ /PF\d+/);
  my @clans = $rdb->query("select distinct clan_acc from clans join clan_membership, pfamA where clans.auto_clan=clan_membership.auto_clan and pfamA.auto_pfamA=clan_membership.auto_pfamA and pfamA_acc=\"$acc\"");
  if (scalar(@clans)){
    return 1;
  }
  else{
    return 0;
  }
}
=head2 perform_RCS_checks_on_pfams

 Title    : perform_RCS_checks_on_pfams
 Usage    : &ClanDB::perform_RCS_checks_on_pfams(@family)
 Function : Runs all RCS checks in preparation for checking
 Returns  : Nothing
 Args     : family id

=cut
  
sub perform_RCS_checks_on_pfams {
  my ($name, @oldpfams) = @_;
  foreach my $family (@oldpfams){
    my ($haslocked,$peoplelocked) = &Bio::Pfam::PfamRCS::user_has_locked_family($family,$name);
    if( $haslocked == 0 ) {
      die "The family [$family] was not locked by you[$name], it was locked by [$peoplelocked]. $0 failed\n";
    }
    
    if( ! open(DESC,"./$family/DESC") ) {
      die "A bad error - cannot open the desc file [./$family/DESC]. Yikes! [$!]";
    }
    
    my $has_ac;
    
    while(<DESC>) {
      if(/^AC/) {
	$has_ac=1;
      }
    }    
    
    close(DESC);
    
    if(!$has_ac) {
      die "Your DESC file has no AC line. This is bad!\n";
    }
    
    # Okay if we have got this far we should be able to chek the family in.
    
    if( ! &Bio::Pfam::PfamRCS::check_family_directory_exists($family) ) {
      die "Family [$family] does not have a directory in RCS_MASTER.\nIf this is a new family, check in a new family with pfnew\n";
    }
    
    if( ! &Bio::Pfam::PfamRCS::check_family_exists($family) ) {
      die "Bad error Family [$family] has RCS_MASTER directory, but not the correct files internally.\nPlease contact pfam\@sanger.ac.uk to resolve the error\n";
    }
    
    if( -s "$family/missing" ) {
      print("pfci: your family seems to be missing members compared to the RCS copy\n(see $family/missing). Please inspect loss of members.\n");
      print("Do you want to continue regardless? [y/n]  ");
      my $reply = <STDIN>;
      chop $reply;
      if ($reply ne "y") {
	exit(0);
      }
    } elsif (! -e "$family/missing"){
      die "pfci: you have not checked if you have lost any members of the family.\nPlease run pqc-check\n";
    }
    elsif(! -e "$family/spell" ) {
      die "pfci: you have not spell checked family.\nPlease run pqc-spell\n";
    }
  }
}

=head2 check_clandesc_file

 Title    : check_clandesc_file
 Usage    : &ClanQC::check_clandesc_file(\*CLANDESC)
 Function : 
 Returns  : 
 Args     : 

=cut

sub check_clandesc_file {
  my ($clan_acc, $clandesc_fh) = @_;
  my $error = 0;
  my ($acc, $author, $desc, $id);
  my $ann = Bio::Annotation::Collection->new();
  my @lines = (<$clandesc_fh>);
  my $i; # line counter
  for($i=0;$i <= $#lines;$i++){
    $_ = $lines[$i];
    ### Check that this is not a daft line
    unless (/^AC\s{3}|^ID\s{3}|^DE\s{3}|^AU\s{3}|^RN\s{3}|^RM\s{3}|^RT\s{3}|^RA\s{3}|^RL\s{3}|^DR\s{3}|^CC\s{3}|^MB\s{3}/) {
	chomp;
	die "'$_' is not a recognised CLANDESC line\n"; 
      }

    /^AC/ && do{
	if(/^AC\s+(CL\d+)(\.\d+)?$/){
	    #happy days....
	}else{
	    chomp;
	    warn "Bad AC line [$_]\n";
	    $error = 1;
	}
	next;
    };
    /^ID/ && do{
	if(/^ID\s+(\S{1,16})$/){
	    #happy days....
	}else{
	    chomp;
	    warn "Bad ID line [$_], should be between 1 and 16 characters long\n";
	    $error = 1;
	}
	next;
    };
    
    /^DE/ && do{
	if(/^DE\s{3}(.*)$/){
	    #happy days....
	}else{
	    chomp;
	    warn "Bad DE line\n";
	    $error = 1;
	}
	next;
    };
 
    /^AU/ && do{
	if(/^AU\s{3}((\w+\s{1}\w{1,3})(\,\s{1})?)*$/){
	    #happy days....
	}else{
	    chomp;
	    warn "Bad AU line [$_], should be between in the format of: Bloggs J, Smith J\n";
	    $error = 1;
	}
	next;
    };
    last;	
  }
  my ($refcomment, $ref);
  OPTIONAL_LINE :
    for(;$i <= $#lines;) { 
      #print STDERR "Looking at $lines[$i]";
      if( $lines[$i] =~ /^CC/ ) {
	last;
      }
      if( $lines[$i] =~ /^R/ ) {
	# read references
	my $refcomment;
	#print STDERR "Reference $lines[$i]";
	for(;$i <= $#lines;$i++) { 
	  #print STDERR "Looking at $lines[$i] in refcomment\n";
	  if( $lines[$i] =~ /^RC\s+(.*)/ ) {
	    $refcomment = $1;
	    for($i++;$i <= $#lines;$i++) {
	      $lines[$i] =~ /RC\s+(.*)/ || do { last; };
	      $refcomment .= $1;
	    }
	  };
	  
	  #print STDERR "Left with $lines[$i] after comments\n";
	  
	  # eaten all the RC lines at the top of this 
	  # reference. Now to eat the rest
	  
	  # if not an /R line, end of references
	  
	  $lines[$i] =~ /^R/ || do {
	    if( defined $refcomment ) {
	      $error =1;
	      warn "Got reference comment with no reference! [$clan_acc]";
	    }
	    last;
	  };
	  # make a new reference line, add in comment if there
	  $lines[$i] =~ /^RN\s+\[(\d+)\]/ || die ("[$lines[$i]] is not a reference number (expected) [$clan_acc]");
	  my $ref = new Bio::Annotation::Reference; 
	  if (defined $refcomment){
	    $ref->comment($refcomment);
	    undef $refcomment;
	  }
	  # first a medline line
	  $i++;
	  $ann->add_Annotation('reference', $ref);
	  $lines[$i] =~ /^RM   (\d+)/ || die ("[$lines[$i]] is not a reference medline line (expected) [$clan_acc]");
	  my $temp = $1; 
	  $ref->medline($temp);
	  # RT
	  
	  $i++;
	  
	  $lines[$i] =~ /^RT   (.*)/ || die ("[$lines[$i]] is not a reference title line (expected) [$clan_acc]");
	  $temp = $1 . " ";
	  for($i++;$i <= $#lines;$i++) {
	    if( $lines[$i] =~ /^RT   (.*)/ ) {
	      $temp .= $1 . " ";
	    } else {
	      last;
	    }
	  }
	  $temp =~ s/  / /g;
	  $ref->title($temp);
	  
	  # don't need to add one, as we ended on a non RT line
	  
	  $lines[$i] =~ /^RA   (.*)/ || die ("[$lines[$i]] is not a reference author line (expected) [$clan_acc]");
	  $temp = $1 . " ";
	  for($i++;$i <= $#lines;$i++) {
	    if( $lines[$i] =~ /^RA   (.*)/ ) {
	      $temp .= $1 . " ";
	    } else {
	      last;
	    }
	  }
	  $temp =~ s/  / /g;
	  $ref->authors($temp);
	  
	  $lines[$i] =~ /^RL   (.*)/ || die ("[$lines[$i]] is not a reference line (expected) [$clan_acc]");
	  $temp = $1;
	  $ref->location($temp);
	}
	next OPTIONAL_LINE;
      }
      if( $lines[$i] =~ /^D/ ) {
	my $com;
	#print STDERR "Link $lines[$i]";
	for(;$i <= $#lines;$i++) { 
	  #print STDERR "Looking at $lines[$i] as a link\n";
	  my $link = Bio::Annotation::DBLink->new();
	  if( $lines[$i] =~ /^DC\s+(.*)/ ) {
	    $com = $1;
	    for($i++;$i <= $#lines;$i++) {
	      $lines[$i] =~ /DC\s+(.*)/ || do { last; };
	      $com .= $1;
	    }
	    $link->comment($com);
	    undef $com;  
	  }
	  # eaten all the DC lines at the top of this 
	  # link. Now to eat the rest
	  # if not an /DR line, end of references
	  my ($db_name, $db_ref, $rest);
	  if ( $lines[$i] =~ /^DR\s+/) {
	    if ($lines[$i] =~ /^DR\s+(\S+);\s+(\S+\s\S?);\s*(.*)/) {
	      ($db_name, $db_ref, $rest) = ($1, $2, $3);
	    }
	    elsif ($lines[$i] =~ /^DR\s+(\S+);\s+(\S+);\s*(.*)/) {
	      ($db_name, $db_ref, $rest) = ($1, $2, $3);
	    }
	    else {
	      $error = 1;
	      warn ("Bad DR line - $lines[$i]");
	    }
	  }
	  else {
	    if( defined $com ) {
	      $error = 1;
	      warn "Got link comment with no links - at $lines[$i]! [$clan_acc]";
	    }
	    last;
	  } 
	  $ann->add_Annotation('dblink', $link);
	  $link->database($db_name);
	  $link->primary_id($db_ref);
	  $link->optional_id($rest);
	}
	next OPTIONAL_LINE;
	
      } # end of if ^D
      
    }
  #Comment lines
  for(;$i <= $#lines;$i++) { 
    my $com = new Bio::Annotation::Comment;
    if( $lines[$i] =~ /^CC\s+(.*)/ ){
      $com->text($1);
      $ann->add_Annotation('comment', $com);
    } 
    elsif ( $lines[$i] =~ /^CC\s*$/ ) {
      $com->text("");
      $ann->add_Annotation('comment', $com);
    } 
    elsif ( $lines[$i] =~ /^MB\s*(PF\d+)/ ) {
      ;
    } 
    else {
      $error = 1;
      warn "Cannot read [$lines[$i]] as comment";
    }
  }
  
  for(;$i <= $#lines;$i++) {
    $error =1 ;
    warn "Unexpected line at end of the CLANDESC [$lines[$i]]";
  }
  
  if (!$error){
    return 1; # There are no errors
  }
  else{
    return 0; # There are errors
  }
   
}

1;
