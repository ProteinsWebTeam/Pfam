
#
# Perl Module for PfamSearch
#
# Cared for by Mhairi Marshall <mm1@sanger.ac.uk>
#
# Originally written by Kevin Howe & Ewam Birney
#
#Copyright Genome Research Limited (1999). Please see information on licensing in LICENSE

package Bio::Pfam::Web::PfamSearch;

$ENV{'COILSDIR'} = '/nfs/disk100/pubseq/src/coils/';

use vars qw($AUTOLOAD @ISA @EXPORT_OK);

use Exporter;
use strict;
use Bio::Pfam::Web::CRC64;
use HMMThres;

use Bio::Pfam::Web::WWWHMMResults;
use Bio::Pfam::Web::WWWBlastResults;
use Bio::Pfam::Web::AnnSeq2WWW;
use Bio::Pfam::Web::PfamWWWConfig;

use Bio::Pfam::PfamAnnSeqFactory;
use Bio::Pfam::HMMOtherRegion;
use Bio::Pfam::OtherRegion;
use Bio::Pfam::PfamRegion;


@EXPORT_OK = qw();
@ISA = ('Exporter');

$| = 1;
$ENV{'BLASTMAT'} = '/nfs/disk100/pubseq/blastdb/';
$ENV{'BLASTFILTER'} = '/usr/local/pubseq/bin/';


#############################
#
#  GRAPHICAL PROTEIN SEARCH
#
#############################

sub standard_search {

  my ($query, $prot, $type, $ethr, $blastcutoff, $out_format, $tempname, $non_pfam_hmms, $domain_search, $blast_file_name, %new_select_order) = @_;

#  print "\n<P>WOW $query, $prot, $type, $ethr, $blastcutoff, $out_format, $tempname, $non_pfam_hmms, $domain_search, $blast_file_name, %new_select_order \n<P>";

 # print "BLAST: $blast_file_name <P>";
  #  $blast_file_name = "$$" . "_";
 # print "BLAST: $blast_file_name <P>";
  my(@non_pfam_hmms) = split(/~/, $non_pfam_hmms);
 
  my $got__ = 1;
 # print "PROT: QUERY: $query \n";
  if ($prot =~ /BLAST/) {
     $prot = undef;
    open(_READ, "$query");
    my (@lines);
    while(<_READ>) {
      if ($_ =~ /\>/) {
	
	if ($got__) {
	  push @lines, $_;
	} 
	$got__ = 0;
      } else {
	push @lines, $_;
      }

    }
   # print "QUERY: $query \n\nLINES: @lines \n";
    $prot = &Bio::Pfam::Web::PfamWWWConfig::seq_from_lines(\@lines);
   # $type = "normal";
   # $blastcutoff = 0.00001;
   # $ethr = "1.0"
  }


#  print "\n THE OUT: $prot  \n";
######## NEW CODE FOR ALREADY GOT SEQ ##########
  my $seq = $prot->seq();
 # print "EEP SEQ: $seq <BR>";
  my $crc_key = &Bio::Pfam::Web::CRC64::crc64($seq);

  my(%all_pfam_ids) = &Bio::Pfam::Web::PfamWWWConfig::get_protein_by_crc64($crc_key);
  my(%all_pfamB_ids) = &Bio::Pfam::Web::PfamWWWConfig::get_protein_by_crc64($crc_key, "B");

  my $existing_search_done = 0;
 
  if ( (%all_pfam_ids) || (%all_pfamB_ids)  ){
    $existing_search_done = 1;
  } 
 
  ### Get the 2 modes - ls always before fs as will mess up following code, sigh
  my @modes;
  push @modes , 'ls'  if ( ($type =~ /normal/i) || ($type =~ /both/i) );
  push @modes , 'fs'  if ( ($type =~ /frag/i)  || ($type =~ /both/i) );

 
  ### For each of the modes 
  my @file_names;
  
  my ($pfamb_fasta, %pfamb_subs, @store);

  ## If it is an existing protein then get the ls/fs hmms into seperate files
  if ($existing_search_done) {
    foreach my $mode (@modes) {
      my $hmm_sub = $blast_file_name . "HMMSUB" . "_" . $mode;
      push @file_names, $hmm_sub;
      open(_HMMSUBFILE, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub") or print "Canna open $hmm_sub as $! <BR>";
      
      foreach (sort keys %all_pfam_ids) {
	
	my $fh = FileHandle->new();
	my $pfam_id = $all_pfam_ids{$_};
	
	if ( ($type =~ /frag/i) || ($mode =~ /fs/i) ) {
	  $fh->open( " /usr/local/pubseq/bin/hmmfetch $Bio::Pfam::Web::PfamWWWConfig::data_root/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname $pfam_id |") or print STDERR "Canna do hmmfetch fs for $pfam_id $! <BR>";
	  
	} elsif ( ($type =~ /normal/i) || ($mode =~ /ls/i) )  {
	  $fh->open( " /usr/local/pubseq/bin/hmmfetch $Bio::Pfam::Web::PfamWWWConfig::data_root/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname $pfam_id |") or print STDERR "Canna do hmmfetch ls for $pfam_id $! <BR>";
	}
	
	
	while(<$fh>) {
	  print _HMMSUBFILE;
	}
	
      }
      
      close (_HMMSUBFILE) or print STDERR "Canna *CLOSE* $hmm_sub as $! <BR>";
      
    }				#/ end for each mode - ls/fs


    ## parse the pfamB params for an existing protein
     
    foreach (sort keys %all_pfamB_ids) {
      my($seq_start, $seq_end) = split(/~/, $_);
      
      my $substr = substr($prot->seq(), $seq_start - 1, $seq_end - $seq_start + 1 );
    
      my %temp = ('key' => $all_pfamB_ids{$_},
		  'start' => $seq_start,
		  'string' => $substr
		 );
      push @store, \%temp;   
    }
    
    @store = sort { $a->{'start'} <=> $b->{'start'} } @store;

  } #/ end if it is an existing protein 
  
  
  
######## END NEW CODE
 
  
  my ($res, $res2, $res_mode2, $www_blast_res, $db, $db2, $ga, $nc, $fs_ga, $fs_nc,$leftover,$res_print_align, $res_mode1, $all_parsed_res, $pfamb_fastadb);
#  print "type: $type <P>";
  if ($existing_search_done) {  ### either fs or ls 
    foreach my $hmm_sub (@file_names) {
      if (!$db) {
	$db = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub" ; ## get ls if ls/fs or fs if fs only
      } else {
	$db2 = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub" ; ## get fs if ls/fs as $db is ls
      }
    }

     $pfamb_fastadb  = 0;
  } elsif ($type =~ /frag/i) {
    $db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname";
    $pfamb_fastadb  = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname";
  } else { 
    $db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname";
    $db2 = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname";
    $pfamb_fastadb  = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname";
  }

 # print "db: $db , db2: $db2 <P>";

  my $pfama_file_name = $blast_file_name . "pfamA";
  my $file_exists = 1;
#  print "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name <BR>";
  open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name") or $file_exists = 0;
  close(_CHECKFILE);

  ####### log file ###
  my $other_hmm_names = undef;
  foreach my $hmm_db (@non_pfam_hmms) {
    $other_hmm_names = $other_hmm_names . " $hmm_db";
  }

  $other_hmm_names = "only" if  (!defined($other_hmm_names));
#print "WOMAMA <P>";

  if ($file_exists) {
     &Bio::Pfam::Web::PfamWWWConfig::logs("Protein Graphical Search: Existing Pfam $other_hmm_names File:$query");
    open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name ");
    
     print "<P>EXISTS $file_exists  <P>";
  } else {

    ######## NEW CODE - DOES BLAST FIRST  #########################################################

#    my $tempname = "$$";
#    my $file = $tempname . "_fastsearch";
#    my $pfamalib = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamalib_fname";
#    my $temp_res = &pfamb_blast_search( $query, $pfamalib, $blastcutoff, undef, undef, $file);
#    my %allids;
    
#    foreach my $hsp ( $temp_res->each_hit) {
#      #  print "HSP: $hsp <BR>";
#      my ($acc, $name, $start, $end, $expect) = split(/~/, $hsp);
#      # print "$acc~$name~$start~$end~$expect <BR>";
#      $allids{"$name"} = 1;
      
#    }
#    $db = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$$-hmm.small";
#    if (keys %allids) {
#      open (_HMM, ">$db") or do {
#	print "An error occurred while determining the Pfam domain structure of the query - no jits found";
#	return 0;
#      };
      
#      foreach my $fam (keys %allids) {
#	my $fh = &Bio::Pfam::Web::PfamWWWConfig::hmm_filehandle_by_name( $fam );
#	while(<$fh>) {
#	  print _HMM;
#	}
#      }
#      close(_HMM) or return;

#    }

    ######### /END NEW CODE ##################################################

    &Bio::Pfam::Web::PfamWWWConfig::logs("Protein Graphical Search: New Pfam $other_hmm_names File:$query");
   # print "$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr -Z 5193 $db $query | <P>";
   # exit(0);
    open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr -Z 5193 $db $query |") or
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");

    open(_PFAMA, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name");
  
   while(<PROG>) {
      print _PFAMA $_;
      
    }
    close(_PFAMA);

    close(PROG);
 # exit(0);
   open(PROG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name ");
  }

  
				  
  
  #####
  $res = new Bio::Pfam::Web::WWWHMMResults;
  $res_mode2 = new Bio::Pfam::Web::WWWHMMResults;
  $res_print_align = new Bio::Pfam::Web::WWWHMMResults;
  
  $res->parse_hmmpfam(\*PROG, $modes[0]); ### Parse hmm output


  close(PROG) or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?");

  
  if (defined($modes[1])) {
    
    my $pfama_file_name_fs = $blast_file_name . "pfamA_fs";
    $file_exists = 1;

    open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs") or $file_exists = 0;
    close(_CHECKFILE);

    if ($file_exists) {
      &Bio::Pfam::Web::PfamWWWConfig::logs("Protein Graphical Search: Existing Pfam $other_hmm_names File:$query");
      open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs ");
      
    } else {
      
      
      #print "$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr  -Z 5193 $db2 $query | <P>";
      #exit(0);
      open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr -Z 5193 $db2 $query |") or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");
      
      open(_PFAMA, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs");
      
      while(<PROG>) {
	print _PFAMA $_;
      }
      close(_PFAMA);
      
      close(PROG);
      open(PROG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs ");
     
     
   
    }
     $res_mode2->parse_hmmpfam(\*PROG, $modes[1]);
  } 
  
 # exit(0);
  
  ##/ end test 
 


  ($ga, $nc, $fs_ga, $fs_nc) = &read_thresholds("pfam");
 
  if (not defined $ga or not defined $nc) {
    &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not get the thresholds $! $?");
  } 

 if ($pfamb_fastadb) {
   $www_blast_res = &pfamb_blast_search( $query, 
				   "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname", 
				   $blastcutoff,
				   undef,
				   undef,
				   $blast_file_name


				  );

     ### NOW PARSE THE PFAMB OUTPUT INTO A USABLE FORMAT!
   foreach my $hsp ( $www_blast_res->each_hit) {
     # print "HSP: $hsp <BR>";
      my ($acc, $id, $seq_start, $seq_end, $score) = split(/~/, $hsp);
     # print "PFAMB: $acc, $id, $seq_start, $seq_end, $score <BR>";
      #open(_FILE, "$www_blast_res");
      #while(<_FILE>) {
     #my($acc, $id, $seq_start, $seq_end, $score) = split(/~/, $_);
     my $substr = substr($prot->seq(), $seq_start - 1, $seq_end - $seq_start + 1 );
     my %temp = ('key' => "temp" . "~" . $seq_start. "~" . $seq_end . "~" . $acc . "~" . $id,
		 'start' => $seq_start,
		 'string' => $substr,
		 'end' => $seq_end,
		 'score' => $score
		);
     push @store, \%temp;   
   }
   
   @store = sort { $a->{'start'} <=> $b->{'start'} } @store;

 }

  my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();

  my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
  $fac->db($the_db);
  

  my $annseq = $fac->createAnnotatedSequence();
  $annseq->sequence( $prot );

  ### Add pfamB stuff to the relevent packages
 # $www_blast_res->add_results_to_AnnSeq( $annseq ) if ($pfamb_fastadb);  ## Add pfamB to Annseq - taken out 
  &b_predict_add_results_to_AnnSeq($annseq, @store) if (@store); ## add pre-predicted pfamB region to Annseq



  ######## get the NON_PFAM HMM'S #############
 

  my %hmm_destination;


  print qq(<form method=POST action=$Bio::Pfam::Web::PfamWWWConfig::search>) if ($non_pfam_hmms =~ /[A-Z]/i);


  my %pfam_www_blast;
  ## Foreach non pfam region !

  foreach my $hmm_db (@non_pfam_hmms) {
    if ($hmm_db =~ /[A-Z]/i)  {
      my $non_pfam_res;
      my $db =  "$Bio::Pfam::Web::PfamWWWConfig::hmmfetch_libs_path/$hmm_db";
      
      
      my $hmm_file_name = $blast_file_name . "$hmm_db";
      
      my $file_exists = 1;
      open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name") or $file_exists = 0;
      close(_CHECKFILE);
      
      if ($file_exists) {
	
	open(HMM_PROG,"$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name ");
	
	
      } else {
	
	open(HMM_PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr $db $query |") or
	  &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");
	
	open(_HMM, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name");
	
	while(<HMM_PROG>) {
	  print _HMM $_;
	}
	close(_HMM);
	
	
	open(HMM_PROG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name ");
      }
      
      
      
      $non_pfam_res = new WWWHMMResults;
      $non_pfam_res->parse_hmmpfam(\*HMM_PROG);
      close(HMM_PROG) or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?");
      $pfam_www_blast{$hmm_db} = $non_pfam_res;
      
      ### Need to get the exact SMART name 
      if ($hmm_db =~ /smart/i) {
     	foreach my $hmmunit ($non_pfam_res->eachHMMUnit) {
	  
	  my $new_name = &convert_smart_names($hmmunit->hmmname);
	  $hmmunit->hmmname($new_name);
	  
	}
      }
      
      print qq(
	       
	       <input type=hidden name=evalue value=$ethr>
	       <input type=hidden name=type value=$type>
	       <input type=hidden name=smart value=$hmm_db>
	       <input type=hidden name=out_format value=$out_format>
	       
	      );
      print  "<input type=hidden name=protseq value=" . $prot->seq() . ">\n" ;
      print "<input type=hidden name=blast_file_name value=$blast_file_name>\n";
      
      
      
      
      
      ### ADD the non-pram hmm regions 
      $annseq = add_non_pfam_regions($annseq , $non_pfam_res, $hmm_db, $prot) if($non_pfam_hmms =~ /[A-Z]/i);

    }

 
  }
  
  my $gifdrawer = AnnSeq2WWW->new();
  my $drawn_gif = 0;

 	
  $non_pfam_hmms = "YES" if($domain_search =~ /[A-Z]/);


  ########################################################################
  #### NEW CODE FOR NON_HMM SEARCHES - sgp, tmhhm, coils & low complexity
  ########################################################################

  my @all_nonhmm_values;

  my @domain_search = split(/~/, $domain_search);
    
  
  foreach my $domain (@domain_search) {
    my @values;

    if ($domain =~ /tmhmm/) {  ##### TMHMM #######
      @values = find_tmhmm("$query");
    } elsif ($domain =~ /sigp/) {  #### SIGP ####
      @values = find_sigp("$query");
    } elsif ($domain =~ /ncoils/) {  ##### COILS ####
      @values = find_coiled_regions("$query");
    } elsif ($domain =~ /seg/) {  #### SEG #####
      @values = find_seg("$query");      
    }
    $annseq = add_other_domains($annseq , @values) if($values[0] =~ /[A-Z]/i);
    foreach (@values) {
      if ($_  =~ /[A-Z]/i) {
	push @all_nonhmm_values, $_ ;
      } 
      
    }
    @values = ();
    
  } ### /end for each non-hmm search!
    
 
  
#  print "<center class=normaltext><font color=#FF0000>*</FONT> Due to system problems your protein will only be searched against pfamA families, pfamB searches are NOT available <font color=#FF0000>*</FONT></center><P>";

  if  ( ($res->number() == 0)  && ($res_mode2->number() ==0) )  {
    
    #####################################################################
    ##### NO PFAMA MATCHES                            ###################
    #####################################################################
    
    print sprintf("<CENTER><span class=normaltext>There were no matches to Pfam-A (including borderline matches) for %s\n </span></CENTER><P>",$prot->id());
    
    
 #   ### Print out pfamB hits table
#    if ( $pfamb_fastadb && $www_blast_res->number()) {
#      &write_table(\*STDOUT,
#		   "Matches to Pfam-B",
#		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=\$acc>\$name</a>",
#		   $www_blast_res);
      
#    } elsif (@store) {
    if (@store) {
      _predict_b_table(@store);
    } else {
     print sprintf("<CENTER><span class=normaltext>There were no matches to Pfam-B for %s\n </span></CENTER><P>",$prot->id());	  
    }				#/ end of pfamB
    
    
    my $non_pfam_result = 0;
    
    ### prints out other hmms - smart/tigr etc
    foreach my $hmm_db (@non_pfam_hmms) {
      if ($pfam_www_blast{$hmm_db}->number() == 0) {
	print sprintf("<CENTER><span class=normaltext>There were no matches to $hmm_db for %s\n</span></center><P>",$prot->id());
	
      } else {
	
	&write_table(\*STDOUT,
		     "Align $hmm_db ",
		     undef,
		     $pfam_www_blast{$hmm_db});
	$non_pfam_result = 1;
      }
      
    }				#/ end of other hmm
    
    if (!$pfamb_fastadb) {
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1 , \%new_select_order, $non_pfam_hmms);
    } elsif ( ($www_blast_res->number()) || ($non_pfam_result) )  {
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1 , \%new_select_order, $non_pfam_hmms); 
    }
    
    
    
  } elsif ( $type eq 'frag' ) {
    # Right. Accession numbers will not be present, so we have to hack it for
    # now (until the new HMMer comes along) and add the accessions in by hand
    
     
    #####################################################################
    ##### FRAGMENT MATCHES                            ###################
    #####################################################################

    #####################################################################
    #### FRAGMENT : Filter the result using Gathering Threshold
    #####################################################################

    $res2 = $fs_ga->filter_results_acc($res);
    #$res2->number() = 0;
    if( $res2->number() == 0 ) {

      ####################################
      ##### FRAGMENT: NO MATCHES ABOVE GA
      ####################################

      print "<CENTER><span class=normaltext>Fragment Search: There are no Pfam-A domains higher than gathering threshold</SPAN></CENTER><p>\n";
      
      
      ### Print out pfamB hits table
#      if ($pfamb_fastadb && $www_blast_res->number()) {
#	&write_table(\*STDOUT,
#		     "Matches to Pfam-B",
#		     "<a href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=\$acc>\$name</a>",
#		     $www_blast_res);
	
#      } elsif (@store) {
      if (@store) {
	
	_predict_b_table(@store);
      }				#/ end of pfamB
      
      
      ### prints out other hmms - smart/tigr etc
      foreach my $hmm_db (@non_pfam_hmms) {
	if ($pfam_www_blast{$hmm_db}->number() > 0) {     
	  &write_table(\*STDOUT,
		       "Align $hmm_db ",
		       undef,
		       $pfam_www_blast{$hmm_db});
	} else {
	  print sprintf("<CENTER><span class=normaltext>There were no matches to <B>$hmm_db</B> for %s\n<P></SPAN></CENTER>",$prot->id());
	}
	
      }				#/ end of other hmm
      
      
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms ) if (!$drawn_gif);
      $drawn_gif = 1;




    } else {
      #print "NUM: " .$res2->number() . "<P>";
      
      ####################################
      ##### FRAGMENT: TRUSTED MATCHES
      ####################################
      
      
      foreach my $hmmunit ($res2->eachHMMUnit) {
	$hmmunit->hmmacc( $the_db->id2acc( $hmmunit->hmmname ) );
      }
      
      &write_table(\*STDOUT,
		   "Fragment search: Trusted matches - domains scoring higher than the gathering threshold",
		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		   $res2,
		   "pfam");
      
      
      #    ### Print out pfamB hits table
      #    if ( $pfamb_fastadb && $www_blast_res->number()) {
      #      &write_table(\*STDOUT,
      #		   "Matches to Pfam-B",
      #		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=\$acc>\$name</a>",
      #		   $www_blast_res);
      #    } elsif (@store) {
      if (@store) {
	_predict_b_table(@store);
      } else {
	print sprintf("<CENTER><span class=normaltext>There were no matches to Pfam-B for %s\n </span></CENTER><P>",$prot->id());
      }				#/ end of pfamB
      
      
      ### prints out other hmms - smart/tigr etc
      foreach my $hmm_db (@non_pfam_hmms) {
	if ($pfam_www_blast{$hmm_db}->number() > 0) {     
	  &write_table(\*STDOUT,
		       "Align $hmm_db ",
		       undef,
		       $pfam_www_blast{$hmm_db});
	} else {
	  print sprintf("<CENTER><span class=normaltext>There were no matches to <B>$hmm_db</B> for %s\n<P></span></CENTER>",$prot->id());
	}
	
      }				#/ end of other hmm
      
      $fac->addHMMResultsToAnnSeq( $annseq, $res2 );
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms) if (!$drawn_gif);
      $drawn_gif = 1;
      $res_print_align = $res;	## when printing out alignment!
    }

    $all_parsed_res = new  WWWHMMResults;
    $all_parsed_res = $res2;
    
    $leftover = $fs_ga->filter_negative_acc($res);
    
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE THE CUTOFF ###################
    #####################################################################
    
    $res2 = $fs_nc->filter_negative_acc($leftover);
    
    #if ($res_mode2) {
    #  if ($res_mode2->number() != 0) {
	#my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	#$res2->merge_results($temp_leftover);
      #}
    #}
    
    my $cutoff_merge = new WWWHMMResults;
    
    $cutoff_merge = $res2;
    
    $cutoff_merge->non_overlapping_results($all_parsed_res);
    
    
    if( $cutoff_merge->number() > 0 ) {
      print "<hr>\n";
      
      &write_table(\*STDOUT, 
		   "Fragment: Potential matches - Domains with Evalues above the cutoff",
		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		   $cutoff_merge,
		   "pfam");
      
      
      $res_print_align->merge_results($cutoff_merge); ## when printing out alignment!
      
      $all_parsed_res->merge_results($cutoff_merge); ## keeps store of domains printed in tables
      
      $fac->addHMMResultsToAnnSeq( $annseq, $cutoff_merge);
      
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms )if (!$drawn_gif);
      $drawn_gif = 1;
    }
    
           
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE RECORDED NOISE  ###################
    #####################################################################
    
            
    $res2 = $fs_nc->filter_results_acc($leftover);
    
    #if ($res_mode2) {
    #  if ($res_mode2->number() != 0) {
	
#	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
#	my $temp_res = $fs_nc->filter_results_acc($temp_leftover);
	
#	$res2->merge_results($temp_res);
	
 #     }
 #   }
    
    my $noise_merge = new WWWHMMResults;
    
    $noise_merge = $res2;
    
    $noise_merge->non_overlapping_results($all_parsed_res);
    
    
    if( $noise_merge->number() > 0 ) {
      print "<hr>\n";
      
      &write_table(\*STDOUT, 
		   "Potential matches - Domains scoring higher than the highest recorded noise",
		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		   $noise_merge,
		   "pfam");

      $res_print_align->merge_results($noise_merge); ## when printing out alignment!
      
      $fac->addHMMResultsToAnnSeq( $annseq, $noise_merge );
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms)if (!$drawn_gif);
      $drawn_gif = 1;


      ########################################################################
      ## END OF FRAGMENT MATCHES ##
      #########################################################################

    }


  }   else {
    
    #####################################################################
    ##### NO PFAMA MATCHES ABOVE GA                   ###################
    #####################################################################
    
    $res2 = $ga->filter_results_acc($res);
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	my $temp_res = $fs_ga->filter_results_acc($res_mode2);
	$res2->merge_results($temp_res);
      }
    }
    
    
    if( $res2->number() == 0 ) {
      print "<CENTER><span class=normaltext>There are no Pfam-A domains higher than gathering threshold</SPAN></CENTER><p>\n";
      
      
      ### Print out pfamB hits table
#      if ($pfamb_fastadb && $www_blast_res->number()) {
#	&write_table(\*STDOUT,
#		     "Matches to Pfam-B",
#		     "<a href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=\$acc>\$name</a>",
#		     $www_blast_res);
	
#      } elsif (@store) {
      if (@store) {
	
	_predict_b_table(@store);
      }				#/ end of pfamB
      
      
      ### prints out other hmms - smart/tigr etc
      foreach my $hmm_db (@non_pfam_hmms) {
	if ($pfam_www_blast{$hmm_db}->number() > 0) {     
	  &write_table(\*STDOUT,
		       "Align $hmm_db ",
		       undef,
		       $pfam_www_blast{$hmm_db});
	} else {
	  print sprintf("<CENTER><span class=normaltext>There were no matches to <B>$hmm_db</B> for %s\n<P></SPAN></CENTER>",$prot->id());
	}
	
      }				#/ end of other hmm
      
      
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms ) if (!$drawn_gif);
      $drawn_gif = 1;
      
      
    }  else {
      
      #####################################################################
      ##### TRUSTED MATCHES                             ###################
      #####################################################################
      
      &write_table(\*STDOUT,
		   "Trusted matches - domains scoring higher than the gathering threshold",
		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		   $res2,
		   "pfam");
      
      ### Print out pfamB hits table
#      if (  ($pfamb_fastadb)  && ($www_blast_res->number() ) )  {
#	&write_table(\*STDOUT,
#		     "Matches to Pfam-B",
#		     "<a href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=\$acc>\$name</a>",
#		     $www_blast_res );
#      } elsif (@store) {
      if (@store) {
	
	_predict_b_table(@store);
     }				#/ end of pfamB
      
#      ### prints out other hmms - smart/tigr etc
#      foreach my $hmm_db (@non_pfam_hmms) {
#	if ($pfam_www_blast{$hmm_db}->number() > 0) {     
#	  &write_table(\*STDOUT,
#		       "Align $hmm_db ",
#		       undef,
#		       $pfam_www_blast{$hmm_db});
#	} else {
#	  print sprintf("<CENTER><span class=normaltext>There were no matches to <B>$hmm_db</B> for %s\n</span></CENTER><P>",$prot->id());
#	}
	
#      }				#/ end of other hmm
      
      
      $fac->addHMMResultsToAnnSeq( $annseq, $res2 );



      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms)if (!$drawn_gif);
      $drawn_gif = 1;
      
      $res_print_align = $res2; ## when printing out alignment!
      
    }	 # end of trusted matches
    
    
    $all_parsed_res = new  WWWHMMResults;
    $all_parsed_res = $res2;
    
    $leftover = $ga->filter_negative_acc($res);
    
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE THE CUTOFF ###################
    #####################################################################
    
    $res2 = $nc->filter_negative_acc($leftover);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	$res2->merge_results($temp_leftover);
      }
    }
    
    my $cutoff_merge = new WWWHMMResults;
    
    $cutoff_merge = $res2;
    
    $cutoff_merge->non_overlapping_results($all_parsed_res);
    
    
    if( $cutoff_merge->number() > 0 ) {
      print "<hr>\n";
      
      &write_table(\*STDOUT, 
		   "Potential matches - Domains with Evalues above the cutoff",
		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		   $cutoff_merge,
		   "pfam");
      
      
      $res_print_align->merge_results($cutoff_merge); ## when printing out alignment!
      
      $all_parsed_res->merge_results($cutoff_merge); ## keeps store of domains printed in tables
      
      $fac->addHMMResultsToAnnSeq( $annseq, $cutoff_merge);
      
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms )if (!$drawn_gif);
      $drawn_gif = 1;
    }
    
           
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE RECORDED NOISE  ###################
    #####################################################################
    
            
    $res2 = $nc->filter_results_acc($leftover);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	
	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	my $temp_res = $fs_nc->filter_results_acc($temp_leftover);
	
	$res2->merge_results($temp_res);
	
      }
    }
    
    my $noise_merge = new WWWHMMResults;
    
    $noise_merge = $res2;
    
    $noise_merge->non_overlapping_results($all_parsed_res);
    
    
    if( $noise_merge->number() > 0 ) {
      print "<hr>\n";
      
      &write_table(\*STDOUT, 
		   "Potential matches - Domains scoring higher than the highest recorded noise",
		   "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		   $noise_merge,
		   "pfam");

      $res_print_align->merge_results($noise_merge); ## when printing out alignment!
      
      $fac->addHMMResultsToAnnSeq( $annseq, $noise_merge );
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms)if (!$drawn_gif);
      $drawn_gif = 1;
      
    } # end of above recorded noise
    
    print "</FORM><p>";
    
  } #/ end what type of pfamA hmm output it is

  sort_print_nonhmm_regs(@all_nonhmm_values);


  #####################################################################
  ##### NOW PRINT OUT THE ALIGNMENTS #######################
  #####################################################################

  
  ## PRINT OUT PFAM-A ALIGNMENT

  if ($res->number() > 0) {
    print "<hr>\n";
    &write_alignment(\*STDOUT, 
		     "Alignments of Pfam-A domains to HMMs",
		     "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?\$acc>\$name</a>",
		     $prot,
		     $res_print_align,
		     $type,
		     "pfamA");
  }
  
  ## PRINT OUT PFAM-B ALIGNMENT
#  if (($pfamb_fastadb) && ($www_blast_res->number() > 0)) {
#    print "<hr>\n";
#    &write_alignment(\*STDOUT, 
#		     "Alignments of Pfam-B domains to best-matching Pfam-B sequence",
#		     "<a href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=\$acc>\$name</a>",
#		     $prot,
#		     $www_blast_res);
#  }  elsif (@store) {
  if (@store) {
    
    &b_predict_write_html_align($prot->id, @store);
  }

  

  ### PRINT OUT OTHER HMMs ALIGNMENT - SMART/TIGR etc
  foreach my $hmm_db (@non_pfam_hmms) {
    if ($pfam_www_blast{$hmm_db}->number() > 0) {
      
      print "<hr>\n";
      &write_alignment(\*STDOUT, 
		       "Alignments of $hmm_db to HMMs",
		       undef,
		       $prot,
		       $pfam_www_blast{$hmm_db},
			 $type);
      
    }
  }


    
} #/ end SUB GRAPHICAL SEARCH



#################################
#
# TEMP FIX FOR GENEDB PEOPLE 
#
#################################
sub temp_fast_search {



#  my ($query, $len, $ethr, $blastcutoff) = @_;
#  my ( $prot, $type,  $out_format, $tempname, $non_pfam_hmms, $domain_search, $blast_file_name, %new_select_order );

#  return 0;

#}

#sub temp_fast_search_2 {



  my ($query, $len, $ethr, $blastcutoff) = @_;
  my ( $prot, $type,  $out_format, $tempname, $non_pfam_hmms, $domain_search, $blast_file_name, %new_select_order   );

#  return 0;
# print "BOO HERE <P>";
   ##################
  # TEMP FIX
  if(!$prot) {
    open(_FILE, "$query");
    my $tmp_seq;
    while(<_FILE>) {
      $tmp_seq .= $_;

    }
    my @lines = split(/\n/, $tmp_seq);
    $prot = &Bio::Pfam::Web::PfamWWWConfig::seq_from_lines(\@lines);

    $type = "ls";
    
  #  $ethr = "1.0" if (!$ethr);
    $blastcutoff = "1.e-05" if (!$blastcutoff);
    $out_format = "html";
    my $dol = "$$";
    $tempname = $dol;
    $blast_file_name = $dol;
  }


  ##################



  my(@non_pfam_hmms) = split(/~/, $non_pfam_hmms);





######## NEW CODE FOR ALREADY GOT SEQ ##########
  my $seq = $prot->seq();
  my $crc_key = &CRC64::crc64($seq);

  my(%all_pfam_ids) = &Bio::Pfam::Web::PfamWWWConfig::get_protein_by_crc64($crc_key);
  my(%all_pfamB_ids) = &Bio::Pfam::Web::PfamWWWConfig::get_protein_by_crc64($crc_key, "B");

  my $existing_search_done = 0;
 
  if ( (%all_pfam_ids) || (%all_pfamB_ids)  ){
    $existing_search_done = 1;
  } 
 
  ### Get the 2 modes - ls always before fs as will mess up following code, sigh
  my @modes;
  push @modes , 'ls';
 # push @modes , 'ls'  if ( ($type =~ /normal/i) || ($type =~ /both/i) );
 # push @modes , 'fs'  if ( ($type =~ /frag/i)  || ($type =~ /both/i) );

 
  ### For each of the modes 
  my @file_names;
  
  my ($pfamb_fasta, %pfamb_subs, @store);

  ## If it is an existing protein then get the ls/fs hmms into seperate files
  if ($existing_search_done) {
    foreach my $mode (@modes) {
      my $hmm_sub = $blast_file_name . "HMMSUB" . "_" . $mode;
      push @file_names, $hmm_sub;
      open(_HMMSUBFILE, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub") or print "Canna open $hmm_sub as $! <BR>";
      
      foreach (sort keys %all_pfam_ids) {
	
	my $fh = FileHandle->new();
	my $pfam_id = $all_pfam_ids{$_};
	
	if ( ($type =~ /frag/i) || ($mode =~ /fs/i) ) {
	  $fh->open( " /usr/local/pubseq/bin/hmmfetch $Bio::Pfam::Web::PfamWWWConfig::data_root/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname $pfam_id |") or print STDERR "Canna do hmmfetch fs for $pfam_id $! <BR>";
	  
	} elsif ( ($type =~ /normal/i) || ($mode =~ /ls/i) )  {
	  $fh->open( " /usr/local/pubseq/bin/hmmfetch $Bio::Pfam::Web::PfamWWWConfig::data_root/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname $pfam_id |") or print STDERR "Canna do hmmfetch ls for $pfam_id $! <BR>";
	}
	
	
	while(<$fh>) {
	  print _HMMSUBFILE;
	}
	
      }
      
      close (_HMMSUBFILE) or print STDERR "Canna *CLOSE* $hmm_sub as $! <BR>";
      
    }				#/ end for each mode - ls/fs


    ## parse the pfamB params for an existing protein
     
    foreach (sort keys %all_pfamB_ids) {
      my($seq_start, $seq_end) = split(/~/, $_);
      
      my $substr = substr($prot->seq(), $seq_start - 1, $seq_end - $seq_start + 1 );
    
      my %temp = ('key' => $all_pfamB_ids{$_},
		  'start' => $seq_start,
		  'string' => $substr
		 );
      push @store, \%temp;   
    }
    
    @store = sort { $a->{'start'} <=> $b->{'start'} } @store;

  } #/ end if it is an existing protein 
  
  
  
######## END NEW CODE
  
   
  my ($res, $res2, $res_mode2, $www_blast_res, $db, $db2, $ga, $nc, $fs_ga, $fs_nc,$leftover,$res_print_align, $res_mode1,  $all_parsed_res, $pfamb_fastadb);
  if ($existing_search_done) {  ### either fs or ls 
    foreach my $hmm_sub (@file_names) {
      
      if (!$db) {
	$db = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub" ; ## get ls if ls/fs or fs if fs only
      } else {
	$db2 = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub" ; ## get fs if ls/fs as $db is ls
      }
    }

     $pfamb_fastadb  = 0;
  } elsif ($type =~ /frag/i) {
    $db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname";
    $pfamb_fastadb  = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname";
  } else { 
    $db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname";
    $pfamb_fastadb  = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname";
  }

 

  my $pfama_file_name = $blast_file_name . "pfamA";
  my $file_exists = 1;
  open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name") or $file_exists = 0;
  close(_CHECKFILE);

  ####### log file ###
  my $other_hmm_names = undef;
  foreach my $hmm_db (@non_pfam_hmms) {
    $other_hmm_names = $other_hmm_names . " $hmm_db";
  }

  $other_hmm_names = "only" if  (!defined($other_hmm_names));


  if ($file_exists) {
  #   &Bio::Pfam::Web::PfamWWWConfig::logs("Protein Graphical Search: Existing Pfam $other_hmm_names File:$query");
    open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name ");
    

  } else {
    open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog  -Z 5193 $db $query |") or print "Could not do: $Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr -Z 5193 $db $query <P>";
#	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");

    open(_PFAMA, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name");
  
    while(<PROG>) {
      print _PFAMA $_;
      
    }
    close(_PFAMA) or print "Cant close prpg as $! <BR>";

    close(PROG);
    open(PROG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name ");
  }

  
				  
  
  #####
  $res = new WWWHMMResults;
  $res_mode2 = new WWWHMMResults;
  $res_print_align = new WWWHMMResults;
  
  $res->parse_hmmpfam(\*PROG, $modes[0]); ### Parse hmm output

 

  close(PROG);
  #or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?");

  
#  if (defined($modes[1])) {
    
#    my $pfama_file_name_fs = $blast_file_name . "pfamA_fs";
#    $file_exists = 1;

#    open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs") or $file_exists = 0;
#    close(_CHECKFILE);

#    if ($file_exists) {
#   #   &Bio::Pfam::Web::PfamWWWConfig::logs("Protein Graphical Search: Existing Pfam $other_hmm_names File:$query");
#      open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs ");
      
#    } else {
      
      
#      ## WEBBLAST
#      open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog_gene -E $ethr $db2 $query |") or print "Could not do: $Bio::Pfam::Web::PfamWWWConfig::prog_gene -E $ethr $db2 $query <P>";
#      #or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");
      
#      open(_PFAMA, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs");
      
#      while(<PROG>) {
#	print _PFAMA $_;
#      }
#      close(_PFAMA);
      
#      close(PROG);
#      open(PROG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfama_file_name_fs ");
     
     
   
#    }
#     $res_mode2->parse_hmmpfam(\*PROG, $modes[1]);
#  } 
  
  
  
  ##/ end test 
 



  ($ga, $nc, $fs_ga, $fs_nc) = &read_thresholds("pfam");
 
  if (not defined $ga or not defined $nc) {
  #  &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not get the thresholds $! $?");
  } 

$pfamb_fastadb = 0;
#print "EEP: $query :: $pfamb_fastadb :: $Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname <P>";
  
 if ($pfamb_fastadb) {
   $www_blast_res = &blast_search( $query, 
				   "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname", 
				   $blastcutoff,
				   undef,
				   undef,
				   $blast_file_name
				  );

 }


  my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();

  my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
  $fac->db($the_db);
  

  my $annseq = $fac->createAnnotatedSequence();
  $annseq->sequence( $prot );

  ### Add pfamB stuff to the relevent packages
 # $www_blast_res->add_results_to_AnnSeq( $annseq ) if ($pfamb_fastadb);  ## Add pfamB to Annseq - taken out 
  &b_predict_add_results_to_AnnSeq($annseq, @store) if (@store); ## add pre-predicted pfamB region to Annseq



  ######## get the NON_PFAM HMM'S #############
 

  my %hmm_destination;


  print qq(<form method=POST action=$Bio::Pfam::Web::PfamWWWConfig::search>) if ($non_pfam_hmms =~ /[A-Z]/i);


  my %pfam_www_blast;
  ## Foreach non pfam region !

#  foreach my $hmm_db (@non_pfam_hmms) {
#    if ($hmm_db =~ /[A-Z]/i)  {
#      my $non_pfam_res;
#      my $db =  "$Bio::Pfam::Web::PfamWWWConfig::hmmfetch_libs_path/$hmm_db";
      
      
#      my $hmm_file_name = $blast_file_name . "$hmm_db";
      
#      my $file_exists = 1;
#      open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name") or $file_exists = 0;
#      close(_CHECKFILE);
      
#      if ($file_exists) {
	
#	open(HMM_PROG,"$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name ");
	
	
#      } else {
#	## WEBBLAST
#	open(HMM_PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog_gene  -E $ethr $db $query |") or print "Could not do: $Bio::Pfam::Web::PfamWWWConfig::prog_gene  -E $ethr $db $query <BR>"; #or
#	 # &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");
	
#	open(_HMM, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name");
	
#	while(<HMM_PROG>) {
#	  print _HMM $_;
#	}
#	close(_HMM);
	
	
#	open(HMM_PROG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_file_name ");
#      }
      
      
      
#      $non_pfam_res = new WWWHMMResults;
#      $non_pfam_res->parse_hmmpfam(\*HMM_PROG);
#      close(HMM_PROG); # or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?");
#      $pfam_www_blast{$hmm_db} = $non_pfam_res;
      
#      ### Need to get the exact SMART name 
#      if ($hmm_db =~ /smart/i) {
#     	foreach my $hmmunit ($non_pfam_res->eachHMMUnit) {
	  
#	  my $new_name = &convert_smart_names($hmmunit->hmmname);
#	  $hmmunit->hmmname($new_name);
	  
#	}
#      }
      

      
      
      
      
      
#      ### ADD the non-pram hmm regions 
#      $annseq = add_non_pfam_regions($annseq , $non_pfam_res, $hmm_db, $prot) if($non_pfam_hmms =~ /[A-Z]/i);

#    }

 
#  }
  
  my $gifdrawer = AnnSeq2WWW->new();
  my $drawn_gif = 0;

 	
  $non_pfam_hmms = "YES" if($domain_search =~ /[A-Z]/);



  if  ( ($res->number() == 0)  && ($res_mode2->number() ==0) )  {
    
    #####################################################################
    ##### NO PFAMA MATCHES                            ###################
    #####################################################################
    
    print sprintf("<CENTER>There were no matches to Pfam-A (including borderline matches) for %s\n </CENTER><P>",$prot->id());
    
      
    my $non_pfam_result = 0;   
    
    if (!$pfamb_fastadb) {
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1 , \%new_select_order, $non_pfam_hmms, 1);  
    } elsif ( ($www_blast_res->number()) || ($non_pfam_result) )  {
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1 , \%new_select_order, $non_pfam_hmms, 1);  
    }
    
    
    
  }   else {
    
    #####################################################################
    ##### NO PFAMA MATCHES ABOVE GA                   ###################
    #####################################################################
    
    $res2 = $ga->filter_results_acc($res);
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	my $temp_res = $fs_ga->filter_results_acc($res_mode2);
	$res2->merge_results($temp_res);
      }
    }
    
    
    if( $res2->number() == 0 ) {
      print "<CENTER><There are no Pfam-A domains higher than gathering threshold</CENTER><p>\n";
      
      
      
      
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms, 1 ) if (!$drawn_gif);
      $drawn_gif = 1;
      
      
    }
    else {
      
      #####################################################################
      ##### TRUSTED MATCHES                             ###################
      #####################################################################
      

      
      
      $fac->addHMMResultsToAnnSeq( $annseq, $res2 );



      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms, 1)if (!$drawn_gif);
      $drawn_gif = 1;
      
      $res_print_align = $res2; ## when printing out alignment!
      
    }				# end of trusted matches
    
    
    $all_parsed_res = new  WWWHMMResults;
    $all_parsed_res = $res2;
    
    $leftover = $ga->filter_negative_acc($res);
    
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE THE CUTOFF ###################
    #####################################################################
    
    $res2 = $nc->filter_negative_acc($leftover);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	$res2->merge_results($temp_leftover);
      }
    }
    
    my $cutoff_merge = new WWWHMMResults;
    
    $cutoff_merge = $res2;
    
    $cutoff_merge->non_overlapping_results($all_parsed_res);
    
    
    if( $cutoff_merge->number() > 0 ) {

      
      $res_print_align->merge_results($cutoff_merge); ## when printing out alignment!
      
      $all_parsed_res->merge_results($cutoff_merge); ## keeps store of domains printed in tables
      
      $fac->addHMMResultsToAnnSeq( $annseq, $cutoff_merge);
      
      $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms, 1 )if (!$drawn_gif);
      $drawn_gif = 1;
    }
    
           
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE RECORDED NOISE  ###################
    #####################################################################
    
            
    $res2 = $nc->filter_results_acc($leftover);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	
	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	my $temp_res = $fs_nc->filter_results_acc($temp_leftover);
	
	$res2->merge_results($temp_res);
	
      }
    }
    
    my $noise_merge = new WWWHMMResults;
    
    $noise_merge = $res2;
    
    $noise_merge->non_overlapping_results($all_parsed_res);
    
    
    if( $noise_merge->number() > 0 ) {

	    $res_print_align->merge_results($noise_merge); ## when printing out alignment!

	    $fac->addHMMResultsToAnnSeq( $annseq, $noise_merge );
	    $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms, 1)if (!$drawn_gif);
	    $drawn_gif = 1;
	    
    } # end of above recorded noise
    
    print "</FORM><p>";
    
  } #/ end what type of pfamA hmm output it is









} #/ end temp_fast_search








#############################
#
#  TEXT ONLY PROTEIN SEARCH
#
#############################


sub standard_search_text_out {

  
  my ($query, $prot, $type, $ethr, $blastcutoff, $out_format, $non_pfam_hmms, %new_select_order) = @_;
  

  my $got__ = 1;
  my $got_blast;
 # print "PROT: QUERY: $query \n";
  if ($prot =~ /BLAST/) {
     $prot = undef;
    open(_READ, "$query");
    my (@lines);
    while(<_READ>) {
      if ($_ =~ /\>/) {
	
	if ($got__) {
	  push @lines, $_;
	} 
	$got__ = 0;
      } else {
	push @lines, $_;
      }

    }
   # print "QUERY: $query \n\nLINES: @lines \n";
    $prot = &Bio::Pfam::Web::PfamWWWConfig::seq_from_lines(\@lines);
   # $type = "normal";
   # $blastcutoff = 0.00001;
   # $ethr = "1.0"
     print "<PRE>";
     $got_blast = 1;
  }


  my(@non_pfam_hmms) = split(/~/, $non_pfam_hmms);
 
  #    my ($res, $res2, $www_blast_res, $db, $ga, $nc, $leftover);

  my ($res, $res2, $res_mode2, $www_blast_res, $db, $db2, $ga, $nc, $fs_ga, $fs_nc,$leftover,$res_print_align, $res_mode1,  $all_parsed_res);
  
  ######## NEW CODE FOR ALREADY GOT SEQ ##########
  my $seq = $prot->seq();
  my $crc_key = &CRC64::crc64($seq);
  
  
  my(%all_pfam_ids) = &Bio::Pfam::Web::PfamWWWConfig::get_protein_by_crc64($crc_key);
  my(%all_pfamB_ids) = &Bio::Pfam::Web::PfamWWWConfig::get_protein_by_crc64($crc_key, "B");
  
  my $existing_search_done = 0;
  
  if ( (%all_pfam_ids) || (%all_pfamB_ids)  ) {
    $existing_search_done = 1;
  } 
  
  ### Get the 2 modes - ls always before fs as will mess up following code, sigh
  my @modes;
  push @modes , 'ls'  if ( ($type =~ /normal/i) || ($type =~ /both/i) );
 # push @modes , 'fs'  if ( ($type =~ /frag/i)  || ($type =~ /both/i) );
  
  
  ### For each of the modes 
  my @file_names;
  
  my ($pfamb_fasta, %pfamb_subs, @store);

  ## If it is an existing protein then do ls first if /ls/fs search or fs if frag search only
  if ($existing_search_done) {
    foreach my $mode (@modes) {
      my $hmm_sub =  "$$.HMMSUB" . "_" . $mode;
      push @file_names, $hmm_sub;
      open(_HMMSUBFILE, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub") or print "Canna open $hmm_sub as $! <BR>";
      
      foreach (sort keys %all_pfam_ids) {
	
	my $fh = FileHandle->new();
	my $pfam_id = $all_pfam_ids{$_};
	
	if ( ($type =~ /frag/i) || ($mode =~ /fs/i) ) {
	  $fh->open( " /usr/local/pubseq/bin/hmmfetch $Bio::Pfam::Web::PfamWWWConfig::data_root/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname $pfam_id |") or print STDERR "Canna do hmmfetch fs for $pfam_id $! <BR>";
	  
	} elsif ( ($type =~ /normal/i) || ($mode =~ /ls/i) )  {
	  $fh->open( " /usr/local/pubseq/bin/hmmfetch $Bio::Pfam::Web::PfamWWWConfig::data_root/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname $pfam_id |") or print STDERR "Canna do hmmfetch ls for $pfam_id $! <BR>";
	}
	
	
	while(<$fh>) {
	  print _HMMSUBFILE;
	}
	
      }
      
      close (_HMMSUBFILE) or print STDERR "Canna *CLOSE* $hmm_sub as $! <BR>";
      
    }				#/ end for each mode - ls/fs

    ## parse the pfamB params for an existing protein
   
    foreach (sort keys %all_pfamB_ids) {
      my($seq_start, $seq_end) = split(/~/, $_);
      
      my $substr = substr($prot->seq(), $seq_start - 1, $seq_end - $seq_start + 1 );
      
      my %temp = ('key' => $all_pfamB_ids{$_},
		  'start' => $seq_start,
		  'string' => $substr
		 );
      push @store, \%temp;   
    }
    
    @store = sort { $a->{'start'} <=> $b->{'start'} } @store;
    

  }

  
 
  my $pfamb_fastadb;

  if ($existing_search_done) {  ### either fs or ls 
    foreach my  $hmm_sub (@file_names) {
      
      if (!$db) {
	$db = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub" ; ## get ls if ls/fs or fs if fs only
      } else {
	$db2 = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$hmm_sub" ; ## get fs if ls/fs as $db is ls
      }
    }

    $pfamb_fastadb  = 0;
  } elsif ($type =~ /frag/i) {
    $db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname";
    $pfamb_fastadb  = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname";
  } else { 
    $db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname";
    $db2 = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname";
    $pfamb_fastadb  = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname";
  }
 

    

  #  #  ####### log file ###



  my $other_hmm_names = undef;
  foreach my $hmm_db (@non_pfam_hmms) {
    $other_hmm_names = $other_hmm_names . " $hmm_db";
  }
  
  $other_hmm_names = "only" if  (!defined($other_hmm_names));
  

  &Bio::Pfam::Web::PfamWWWConfig::logs("Protein Text Search: New Pfam $other_hmm_names File:$query");
  
  # /end logs
  
  
  open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr -Z 5193 $db $query |") or
    &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere", 1);
  
  
      
  $res = new WWWHMMResults;
  $res->parse_hmmpfam(\*PROG, $modes[0]);
  

  close(PROG) or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?", 1);
  $res_mode2 = new WWWHMMResults;
  
  ### GET THE FS SEARCH IF THERE IS ONE TO DO OFF COURSE 
  
  if (defined($modes[1])) {	
    
    open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr -Z 5193 $db2 $query |") or
      &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");
    
    $res_mode2->parse_hmmpfam(\*PROG, $modes[1]);
  } 
  
  ($ga, $nc, $fs_ga, $fs_nc) = &read_thresholds("pfam");
  

  if (not defined $ga or not defined $nc) {
    &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not get the thresholds $! $?", 1);
  }
  
  
  
  
  my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
  
  ### GET THE OTHER HMM STUFF #############
  

  my %pfam_www_blast;
  ## Foreach non pfam region !
  
  foreach my $hmm_db (@non_pfam_hmms) {
    if ($hmm_db =~ /[A-Z]/i)  {
      my $non_pfam_res;
      my $db =  "$Bio::Pfam::Web::PfamWWWConfig::hmmfetch_libs_path/$hmm_db";
      
      
      open(HMM_PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr $db $query |") or
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - mis
configuration somehere");
      
      $non_pfam_res = new WWWHMMResults;
      $non_pfam_res->parse_hmmpfam(\*HMM_PROG);
      close(HMM_PROG) or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?");
      $pfam_www_blast{$hmm_db} = $non_pfam_res;


    }
  }
  

#  #### GET PFAMB SEARCHES 
#  if ($pfamb_fastadb) {
#    $www_blast_res = &blast_search( $query, 
#				    "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamblib_fname", 
#				    $blastcutoff);
    
#  }
  

 

  if ( ($res->number() == 0) && ($res_mode2->number() == 0) ) {
    
    #####################################################################
    ##### NO PFAMA MATCHES                            ###################
    #####################################################################
    
    print sprintf("There were no matches to Pfam-A (including borderline matches) for %s\n\n",$prot->id());
    
    ## pfamB output 
#    if ( $pfamb_fastadb && $www_blast_res->number()) {
#      print "Pfam-B matches:\n\n";
#      $www_blast_res->write_ascii_out();
      
#    } elsif (@store) {
#      print "Pfam-B matches:\n\n";
#      b_predict_write_ascii_out(@store);
#    } else {
#      print sprintf("\n\nThere were no matches to Pfam-B matches for %s\n\n",$prot->id());
#    }
    
#    if ($www_blast_res->number()) {
#      print "Pfam-B matches:\n\n";
#      $www_blast_res->write_ascii_out();
#    }
    


    ## print out other hmm's like smart/tigr	
    foreach my $hmm_db (@non_pfam_hmms) {
      
      if ($pfam_www_blast{$hmm_db}->number() == 0) {
	print sprintf("\n\nThere were no matches to $hmm_db for %s\n\n",$prot->id());
	
      } else {
	
	print "\n\nAlign to $hmm_db HMM:\n\n";
	
	$pfam_www_blast{$hmm_db}->write_ascii_out();
      }
      
    }				#/ end for each other hmm
    
    
  }     elsif ( $type eq 'frag' ) {
    
    # Right. Accession numbers will not be present, so we have to hack it for
    # now (until the new HMMer comes along) and add the accessions in by hand
    
    #####################################################################
    ##### FRAGMENT MATCHES                            ###################
    #####################################################################
    
    
    foreach my $hmmunit ($res->eachHMMUnit) {
      $hmmunit->hmmacc( $the_db->id2acc( $hmmunit->hmmname ) );
    }
    
    print "Caution: Fragment search, manual thresholds were not used.\n\n";
    $res->write_ascii_out();

#    ## pfamB output 
#    if ( $pfamb_fastadb && $www_blast_res->number()) {
#      print "\n\nPfam-B matches:\n\n";
#      $www_blast_res->write_ascii_out();
      
#    } elsif (@store) {
#      print "\n\nPfam-B matches:\n\n";
#      b_predict_write_ascii_out(@store);
#    } else {
#      print sprintf("\n\nThere were no matches to Pfam-B matches for %s\n\n",$prot->id());
#    }

 
    
    ## print out other hmm's like smart/tigr
    foreach my $hmm_db (@non_pfam_hmms) {
      
      if ($pfam_www_blast{$hmm_db}->number() == 0) {
	print sprintf("\\n\nThere were no matches to $hmm_db for %s\n\n",$prot->id());
	
      } else {
	
	print "\n\nAlign to $hmm_db HMM:\n\n";
	
	$pfam_www_blast{$hmm_db}->write_ascii_out();
      }
      
    }				#/ end for each other hmm
    
  }  else {
    
    #####################################################################
    ##### NO PFAMA MATCHES ABOVE GA                   ###################
    #####################################################################
    
    $res2 = $ga->filter_results_acc($res);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	my $temp_res = $fs_ga->filter_results_acc($res_mode2);
	$res2->merge_results($temp_res);
      }
    }
    
   if( $res2->number() == 0 ) {
      print "There are no Pfam-A domains higher than gathering threshold\n\n";
      
      


    ## pfamB output 
#    if ( $pfamb_fastadb && $www_blast_res->number()) {
#      print "\n\nPfam-B matches:\n\n";
#      $www_blast_res->write_ascii_out();
      
#    } elsif (@store) {
#      print "\n\nPfam-B matches:\n\n";
#      b_predict_write_ascii_out(@store);
#    } else {
#      print sprintf("\n\nThere were no matches to Pfam-B matches for %s\n\n",$prot->id());
#    }


      
      
      ## print out other hmm's like smart/tigr
      foreach my $hmm_db (@non_pfam_hmms) {
	
	if ($pfam_www_blast{$hmm_db}->number() == 0) {
	  print sprintf("<P>There were no matches to $hmm_db for %s\n\n",$prot->id());
	  
	} else {
	  
	  print "\n\nAlign to $hmm_db HMM:\n\n";
	  
	  $pfam_www_blast{$hmm_db}->write_ascii_out();
	}
	
      }				#/ end foreach other hmm
      
      
    } else {
      
      #####################################################################
      ##### TRUSTED MATCHES                             ###################
      #####################################################################
      
      # Trusted domains
      print "Trusted Pfam-A matches scoring higher than the gathering threshold:\n\n";
      $res2->write_ascii_out();
      

#      ## pfamB output 
#      if ( $pfamb_fastadb && $www_blast_res->number()) {
#	print "\n\nPfam-B matches:\n\n";
#	$www_blast_res->write_ascii_out();
	
#      } elsif (@store) {
#	print "\n\nPfam-B matches:\n\n";
#	b_predict_write_ascii_out(@store);
#      } else {
#	print sprintf("\n\nThere were no matches to Pfam-B matches for %s\n\n",$prot->id());
#      }


      
      ## print out other hmm's like smart/tigr
      foreach my $hmm_db (@non_pfam_hmms) {
	
	if ($pfam_www_blast{$hmm_db}->number() == 0) {
	  print sprintf("\n\nThere were no matches to $hmm_db for %s\n\n",$prot->id());
	  
	} else {
	  
	  print "\n\nAlign to $hmm_db HMM:\n\n";
	  
	  $pfam_www_blast{$hmm_db}->write_ascii_out();
	}
	
      }				#/ end for each other hmm
      
    }				# end of trusted matches
    
    $all_parsed_res = new  WWWHMMResults;
    $all_parsed_res = $res2;
    
    $leftover = $ga->filter_negative_acc($res);
    
    
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE THE CUTOFF ###################
    #####################################################################
    
    $res2 = $nc->filter_negative_acc($leftover);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	$res2->merge_results($temp_leftover);
      }
    }
    
    my $cutoff_merge = new WWWHMMResults;
    
    $cutoff_merge = $res2;
    
    $cutoff_merge->non_overlapping_results($all_parsed_res);
    
    if( $cutoff_merge->number() > 0 ) {
      # Domains scoring higher than the e-value cut-off
      print "\n\nPotential Pfam-A matches scoring higher than the e-value cutoff:\n\n";
      $cutoff_merge->write_ascii_out();
      
      $all_parsed_res->merge_results($cutoff_merge); ## keeps store of domains printed in tables
      
      
    }
    
    
    #####################################################################
    ##### GET POTENTIAL MATCHES ABOVE RECORDED NOISE  ###################
    #####################################################################
    
    $res2 = $nc->filter_results_acc($leftover);
    
    if ($res_mode2) {
      if ($res_mode2->number() != 0) {
	
	my $temp_leftover = $fs_ga->filter_negative_acc($res_mode2);
	my $temp_res = $fs_nc->filter_results_acc($temp_leftover);
	
	$res2->merge_results($temp_res);
	
      }
    }
    
    my $noise_merge = new WWWHMMResults;
    
    $noise_merge = $res2;
    
    $noise_merge->non_overlapping_results($all_parsed_res);
    
    if( $noise_merge->number() > 0 ) {
      print "\n\nPotential Pfam-A matches scoring higher than the highest recorded noise:\n\n";
      # domains scoring higher than the highest recorded noise
      $noise_merge->write_ascii_out();
	  
    }			# end of above recorded noise
	
	
	
  } #/ end of what type of pfamA match has happened
    
  print "</PRE>" if ($got_blast);

}#/ end SUB TEXT SEARCH


#############################
#
#  FAST SEARCH
#
#############################


sub fast_search {
    my $query = shift;
    my $length = shift;
    my $ethr = shift;
    my $blastcutoff = shift;


#    open (MAIL, "| Mail -s 'TEST ' mm1\@sanger.ac.uk ");
#  #     open (MAIL, "| Mail -s 'ANNOTATION SUBMISSION' agb\@sanger.ac.uk , sgj\@sanger.ac.uk");
#    print MAIL "WOO HOO $query\n";
#    open(_FILE,  "$query") or print MAIL "BOOO AS $! \n";
#    while(<_FILE>) {
#      print MAIL "$_";
#    }
#    close(_FILE);
#    close MAIL;
  
#  print &Bio::Pfam::Web::PfamWWWConfig::header("Thankyou", $id, $acc);
  
 # print <<"EOF";


   # print "FAST SEARCH $query";
    #$query = "/nfs/WWWdev/SANGER_docs/htdocs/tmp/blast/r2a/65/d7/r2a65d75AvIl05uifI.tmp";
    my $output_dir = $1 if ($query =~ /(\S+\/)/);
   # print "THE DIR: $output_dir <P>";
   # exit(0);
	$blastcutoff = 0.00001;
	$ethr = 1.0;
	
    my ($res, $www_blast_res, $hmmlib, $ga, $nc, %allids);

    my $gifdrawer = AnnSeq2WWW->new();
    my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
    my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();

   

    $fac->db($the_db);
    my $annseq = $fac->createAnnotatedSequence();
    $annseq->length($length);

    ($ga, $nc) = &read_thresholds();
    if (not defined $ga or not defined $nc) {
	print "An error occurred while determining the Pfam domain structure of the query - no thresholds";
	return 0;
    }

    my $pfamalib = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamalib_fname";
  #  print "LIB: $pfamalib <P>";
    my $tempname = "$$";
    my $file = $tempname . "_fastsearch";
  #  print "$query, $pfamalib, $blastcutoff, undef, undef, $file, 1 <P>";
    $www_blast_res = &pfamb_blast_search( $query, $pfamalib, $blastcutoff, undef, undef, $file, $output_dir);
    #print "WWW: $www_blast_res <P>";
    if (not defined $www_blast_res) {
	print "An error occurred while determining the Pfam domain structure of the query - blast search failed ($www_blast_res)";
	return 0;
    }
 #   open(_BLEE, ">>/nfs/WWW/htdocs/Software/Pfam/temp/BLEE");
    #print "THIS IS A TEST <P>";
    #print STDOUT "THIS IS A TEST <P>";
    #open(_FILE, "$www_blast_res");
    #while(<_FILE>) {
    #  my ($acc, $id, @junk)  = split(/~/, $_);
   #   print "$acc, $id <BR>";
    #  $id =~ s/\;//g;
    #  $allids{"$id"} = 1;  
   #   print _BLEE "$id \n";
   
    #}
    #close(_FILE);
  #  close(_BLEE);

    foreach my $hsp ( $www_blast_res->each_hit) {
    #  print "HSP: $hsp <BR>";
      my ($acc, $name, $start, $end, $expect) = split(/~/, $hsp);
   # print "$acc~$name~$start~$end~$expect <BR>";
    	$allids{"$name"} = 1;
    
  }

#    foreach my $hsp ($www_blast_res->each_hit) {
#	my ($acc, $id) = $hsp->parent->desc =~ /^(\S+);(\S+);$/;
#	print "ACC: $acc <BR>";
#	$allids{"$id"} = 1;
#    }
    
    $hmmlib = "$output_dir/$$-hmm.small";

    $res = new WWWHMMResults;    


    if (keys %allids) {
	open (_HMM, ">$hmmlib") or do {
	    print "An error occurred while determining the Pfam domain structure of the query - no jits found";
	    return 0;
	};

	foreach my $fam (keys %allids) {
	    my $fh = &Bio::Pfam::Web::PfamWWWConfig::hmm_filehandle_by_name( $fam );
	    while(<$fh>) {
		print _HMM;
	    }
	}
	close(_HMM) or return;
      
#	print "$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr $hmmlib $query <P>";
	open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr $hmmlib $query |") or do {
	    print "An error occurred while determining the Pfam domain structure of the query - blast prog not found";
	    return 0;
	};	
#	print "HERE <P>";
#	my $temp_output = "$output_dir/$$-hmm_output";
#	open(_OUT, ">$temp_output");
#	while(<PROG>) {
#	  print _OUT $_;
#	}
#	close(_OUT);
#	close(_PROG);
#	open(PROG, "$output_dir/$$-hmm_output");
	$res->parse_hmmpfam(\*PROG);
	close(PROG) or print STDERR "canna close Pfam quick blast as $! <P>"; 
#	print "RES: $res <P>";
	fast_search_draw($res, $length);
    }
    else {
	print "There were no significant matches to Pfam domains";
#	print "Pfam file: $www_blast_res \n";
	return 1;
    }


#    foreach my $hmmunit ($res->eachHMMUnit) {
#	$hmmunit->hmmacc( $the_db->id2acc( $hmmunit->hmmname ) );
#    }

#    my $res2 = $ga->filter_results_acc($res);

#    if ($res2->number() == 0) {
#	print "There were no significant matches to Pfam";
#    }
#    else {
#	$fac->addHMMResultsToAnnSeq( $annseq, $res2 );
#	$gifdrawer->display_domains_gif( $annseq,  1/(2*int(($length/1000)+1)), 0, 1);
#    }

#    unlink $hmmlib;
   
    return 1;
}


################################################################################
######################## NEW STUFF for fast_search from omniblast on WWW   #####
################################################################################


sub fast_search_calculate {
  my $query = shift;
  my $length = shift;
  my $ethr = shift;
  my $blastcutoff = shift;
  
  my $error;
  $blastcutoff = 0.00001;
  $ethr = 1.0;
  
  my ($res, $www_blast_res, $hmmlib, $ga, $nc, %allids);
  
  my $gifdrawer = AnnSeq2WWW->new();
  my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
  my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
  
#  open(_FILE, "$query");
#  while(<_FILE>) {
#    print "$_ ";

#  }

#  close(_FILE);
  
  
  $fac->db($the_db);
  my $annseq = $fac->createAnnotatedSequence();
  $annseq->length($length);
  
  ($ga, $nc) = &read_thresholds();
  if (not defined $ga or not defined $nc) {
    $error =  "<center>An error occurred while determining the Pfam domain structure of the query<P></center>";
    print STDERR "Pfam: problem with ga/nc for fast_search_calculate PfamSearch";
    return 0, $error;
  }
  
  my $pfamalib = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamalib_fname";
  
  
  my $tempname = "$$";
  my $file = $tempname . "_fastsearch";
  
  $www_blast_res = &fast_blast_search( $query, $pfamalib, $blastcutoff, undef, undef, $file);
  
  if (not defined $www_blast_res) {
    $error =	 "<center>An error occurred while determining the Pfam domain structure of the query<P></center>";
    return 0, $error;
  }
  
  
  foreach my $hsp ($www_blast_res->each_hit) {
    my ($acc, $id) = $hsp->parent->desc =~ /^(\S+);(\S+);$/;
    $allids{"$id"} = 1;
  }
  
  $hmmlib = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$$-hmm.small";
  
  $res = new WWWHMMResults;    
  
  
  if (keys %allids) {
    open (_HMM, ">$hmmlib") or do {
      $error =  "<center>An error occurred while determining the Pfam domain structure of the query<P></center>";
      return 0, $error;
    };
    
    foreach my $fam (keys %allids) {
      my $fh = &Bio::Pfam::Web::PfamWWWConfig::hmm_filehandle_by_name( $fam );
      while(<$fh>) {
	print _HMM;
      }
    }
    close(_HMM) or return;
    
    
  #  print "\n\n Prog: $Bio::Pfam::Web::PfamWWWConfig::prog E: -E $ethr LIB: $hmmlib QUERY: $query \n\n";
  #  exit(0);

    my $prog = "/usr/local/pubseq/bin/hmmpfamt "; 

    open(PROG," $prog -E $ethr $hmmlib $query |") or do {
      $error = "<center>An error occurred while determining the Pfam domain structure of the query<P></center>";
      return 0, $error;
    };	
    
    $res->parse_hmmpfam(\*PROG);
    close(PROG) or print STDERR "canna close Pfam quick blast as $! <P>"; 
  }
  else {
    $error =  "<center>There were no significant matches to Pfam<P></center>";
    return 0, $error;
  }
  
  unlink $hmmlib;
  
  return $res;

}




sub fast_search_draw {
  my $res = shift;
  my $length = shift;
  
  my $gifdrawer = AnnSeq2WWW->new();
  my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
  my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
 # open(_BLEE, ">>/nfs/WWW/htdocs/Software/Pfam/temp/BLEE");

  foreach my $hmmunit ($res->eachHMMUnit) {
  # print "HMM: $hmmunit \n";
    $hmmunit->hmmacc( $the_db->id2acc( $hmmunit->hmmname ) );
  }
  
  my($ga, $nc) = &read_thresholds();
  if (not defined $ga or not defined $nc) {
    print "An error occurred while determining the Pfam domain structure of the query";
    return 0;
  }
  
  
  $fac->db($the_db);
 # print " a ";
  my $annseq = $fac->createAnnotatedSequence();
 # print " b ";
  $annseq->length($length);
  
  my $res2 = $ga->filter_results_acc($res);
  if ($res2->number() == 0) {
    print "There were no significant matches to Pfam";
  }
  else {
    
    $fac->addHMMResultsToAnnSeq( $annseq, $res2 );
   # print _BLEE "HERE: HERE !!!!  $annseq, $res2\n";
   # foreach my $region ($annseq->eachAnnotatedRegion()) {
   #   print _BLEE "REGION: $region , \n";

   # }
   # print _BLEE "DONE\n";
    # $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms, 1
    my %new_select_order;
    my $non_pfam_hmms;
    $gifdrawer->display_domains_gif( $annseq, 0.5, 0, 1, \%new_select_order, $non_pfam_hmms);
  #   close(_BLEE);
    
  }
  
  
  
  return 1;
  
  
  
}



sub fast_blast_search {
    my $query = shift;
    my $fastadb = shift;
    my $blastcutoff = shift;
    my $search_type = shift;
    my $options = shift;
    my $blast_file_in = shift;

    my ($www_blast_res, $blastres, $error, $command);

  
    $command = ($search_type)?"$search_type":"$Bio::Pfam::Web::PfamWWWConfig::blastprog";

    $command = "/usr/local/pubseq/bin/wublastp ";

  #  print "COMMAND: $command \n";
   # exit(0);

    open (SAVEIN, ">&STDIN");   

    ## FILE NAME PASSED and update to pfamB version
    my $pfamb_file_name = $blast_file_in . "pfamB";
  #  print "PFAMB: $pfamb_file_name <P>";
   
  my $file_exists = 1;
  open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name") or $file_exists = 0;
  close(_CHECKFILE);

 #   print "HERE <P>";
    ## TEMP 
 #   $file_exists = 1;
 #   $pfamb_file_name = "229047_fastsearchpfamB";
    ### /TEMP

    if($file_exists) {
    #  print "EXISTS <P>";
      open (STDIN, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name ");
      
    } else {

   #     print "QUERY: $command  $query <P>";
     # exit(0);

     # print "<BR>DOSENT EXIST <BR>";
#	$query = "/nfs/WWWdev/SANGER_docs/htdocs/tmp/blast/blast.r2a72r80AVFMGg8939c.tmp";
#	print "\n\n $command $fastadb $query $options <P>\n\n";
	#exit(0);
      open (STDIN, "$command $fastadb $query $options |");
    
      
    #  $query = "/nfs/WWW/htdocs/Software/Pfam/temp/1230963.pep";
    #  print "QUERY: $query <P>";
      
   #   print "comm: $command db: $fastadb query: $query options: $options | <BR>"; exit(0);

      my $tempdir = "/nfs/WWW/SANGER_docs/htdocs/tmp/blast";

      open (_BLASTFILE, ">$tempdir/$pfamb_file_name") or die "canna open blast results: $Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name as $!  \n";

      #open (_BLASTFILE, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name") or die "canna open blast results: $Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name as $!  \n";
      
      while(<STDIN>) {
#	print "$_ \n";
	print _BLASTFILE $_;
      }
      
      close(_BLASTFILE);

    #  open(STDIN, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name");
      open(STDIN, "$tempdir/$pfamb_file_name");
    }


    # if blank do the blast job & write to the file
    # else get input from the file :-)


    eval {
            
     	$blastres = Bio::Tools::Blast->new('-parse' => 1, 
					   '-signif' => $blastcutoff,
					   '-wait' => 300);
	$www_blast_res = WWWBlastResults->new('-cutoff' => $blastcutoff);
	$www_blast_res->parse_blast_results( $blastres );


    };
   # print "WWW BLAST: $www_blast_res <P>";
    if ($@) {
	$error = "true";
	print "ERROR AS $@ <P>";
    }


    open (STDIN, ">&SAVEIN");

    if (defined $error) {
	return undef;
    }
    else {
	return $www_blast_res;
    }
}




############################################################
##### / end of new fast_search code ########################
############################################################



#############################
#
#  DNA SEARCH
#
#############################

sub dna_search {
    my $query = shift;
    my $length = shift;
    my $blastcutoff = shift;

    my ($res, $www_blast_res, $hmmlib, $ga, $nc, %allids);

    my $gifdrawer = AnnSeq2WWW->new();
    my $the_db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
    my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
    $fac->db($the_db);
    my $annseq = $fac->createAnnotatedSequence();
    $annseq->length($length);


    my $pfamalib = "$Bio::Pfam::Web::PfamWWWConfig::fastalibs_path/$Bio::Pfam::Web::PfamWWWConfig::pfamalib_fname";
    
    $www_blast_res = &blast_search( $query, $pfamalib, $blastcutoff, "bsub -I -q fastblastq blastx", "-filter seg+xnu");
    if (not defined $www_blast_res) {
	print "An error occurred while determining the Pfam domain structure of the query";
	return 0;
    }

    if (not $www_blast_res->each_hit) {
	print "There were no significant matches to Pfam";
    }
    else {
	$www_blast_res->add_results_to_AnnSeq( $annseq );
	$gifdrawer->display_domains_gif( $annseq, 1/(2*int(($length/1000)+1)), 0, 1);
    }

    return 1;
}

#############################
#
#  BLAST SEARCH
#
#############################

sub blast_search {
    my $query = shift;
    my $fastadb = shift;
    my $blastcutoff = shift;
    my $search_type = shift;
    my $options = shift;
    my $blast_file_in = shift;

    my ($www_blast_res, $blastres, $error, $command);
  #  print "HERE <P>";
  
    $command = ($search_type)?"$search_type":"$Bio::Pfam::Web::PfamWWWConfig::blastprog";

    open (SAVEIN, ">&STDIN");   

    ## FILE NAME PASSED and update to pfamB version
    my $pfamb_file_name = $blast_file_in . "pfamB";
  my $file_exists = 1;
  open(_CHECKFILE, "<$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name") or $file_exists = 0;
  close(_CHECKFILE);


    ## TEMP 
 #   $file_exists = 1;
 #   $pfamb_file_name = "229047_fastsearchpfamB";
    ### /TEMP

    if($file_exists) {
    #  print "EXISTS <P>";
      open (STDIN, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name ");
      
    } else {

     # print "<BR>DOSENT EXIST <BR>";
      open (STDIN, "$command $fastadb $query $options |");
     
  #   print "$command $fastadb $query $options | <BR>";
  #    print "BLAST: $Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name <P>";
      open (_BLASTFILE, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name") or die "canna open blast results \n";
      
      while(<STDIN>) {
	print _BLASTFILE $_;
      }
      
      close(_BLASTFILE);

      open(STDIN, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name");
    }


    # if blank do the blast job & write to the file
    # else get input from the file :-)

  # print "BEFORE BEFORE <P>";
    eval {
            
  #    print "CREATE BLAST OBJECT <P>";
     # my $blee = Bio::Tools::Blast->new() || or print "CANNA OPEN AS $! <P>";
     	$blastres = Bio::Tools::Blast->new('-parse' => 1, 
					   '-signif' => $blastcutoff,
					   '-wait' => 300);
	#print "BLAST: $blastres <P>";
	$www_blast_res = WWWBlastResults->new('-cutoff' => $blastcutoff);
	$www_blast_res->parse_blast_results( $blastres );
#	print "www: $www_blast_res <P>";

    };
    if ($@) {
      print "Problem as $@ <P>";
    }
 #   print "AFTER: WWW BLAST: $www_blast_res <P>";
    if ($@) {
	$error = "true";
    }


    open (STDIN, ">&SAVEIN");

    if (defined $error) {
	return undef;
    }
    else {
   #   print "RETURN $www_blast_res <P>";
	return $www_blast_res;
    }
}


sub pfamb_blast_search {
    my $query = shift;
    my $fastadb = shift;
    my $blastcutoff = shift;
    my $search_type = shift;
    my $options = shift;
    my $blast_file_in = shift;
    my $output_dir = shift;

    my ($www_blast_res, $blastres, $error, $command);
   # print "HERE <P>";
  
    $command = ($search_type)?"$search_type":"$Bio::Pfam::Web::PfamWWWConfig::blastprog";

    open (SAVEIN, ">&STDIN");   

    ## FILE NAME PASSED and update to pfamB version
    my $pfamb_file_name = $blast_file_in . "pfamB";
    my $file_exists = 1;

    my $checkfile_dir;
    if ($output_dir) {
      $checkfile_dir = $output_dir;
    } else {
      $checkfile_dir = "$Bio::Pfam::Web::PfamWWWConfig::tempdir";
    }
    
#    print "$checkfile_dir/$pfamb_file_name <BR>";
  open(_CHECKFILE, "<$checkfile_dir/$pfamb_file_name") or $file_exists = 0;
  close(_CHECKFILE);

 #   print "EXISTS: $file_exists <BR>";

    ## TEMP 
 #   $file_exists = 1;
 #   $pfamb_file_name = "229047_fastsearchpfamB";
    ### /TEMP

    if($file_exists) {

   #   open (STDIN, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name ");
      
    } else {
     ## $fastadb = "/nfs/WWW/htdocs/Software/Pfam/data/BlastTemp/Pfam-B.fasta";
     # print "THE COMMAND $command $fastadb $query $options  <P>";
      open (STDIN, "$command $fastadb $query $options |");

  #    print "$command $fastadb $query $options <P>";
      my $blast_res;
      my $www_blast_res;
      
      eval {
	#   print "WOW \n";
	$blastres = new Bio::Tools::BPlite(-fh=>\*STDIN);
	#   print "RES: $blastres\n";
	$www_blast_res = WWWBlastResults->new('-cutoff' => $blastcutoff);
	$www_blast_res->parse_blast_results( $blastres );
      };
    #  if ($@) {
#	print "BLAST FAILED AS $@ <P>";
#      }
#      print "BLAST: $www_blast_res <P>";

#      foreach my $hsp ( $www_blast_res->each_hit) {
#    my ($acc, $name, $start, $end, $expect) = split(/~/, $hsp);
#    print "$acc~$name~$start~$end~$expect <BR>";
    
#  }
 #     print "WWW BLAST RES: $www_blast_res <BR>";
      return $www_blast_res;
      exit(0);


#      ## NEW CODE 

#      my $blast_res;
#      my $www_blast_res;

#      eval {
#	#   print "WOW \n";
#	$blastres = new Bio::Tools::BPlite(-fh=>\*STDIN);
#	#   print "RES: $blastres\n";
#	$www_blast_res = WWWBlastResults->new('-cutoff' => $blastcutoff);
#	$www_blast_res->parse_blast_results( $blastres );
#      };
#      open (STDIN, ">&SAVEIN");


#      print "BLAST: $www_blast_res <P>";
#      exit(0);


      #print "$command $fastadb $query $options <P>";
      #open (_BLASTFILE, ">/nfs/WWW/htdocs/tmp/blast/$pfamb_file_name") or die "canna open blast results \n";
      #print "BLSATFILE: $Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name <P>";
      open (_BLASTFILE, ">$checkfile_dir/$pfamb_file_name") or die "canna open blast results \n";
      while(<STDIN>) {
	print _BLASTFILE $_;
#	print "$_ <BR>";
      }
      
      close(_BLASTFILE);

   #   open(STDIN, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name");
    }

    my $dol = "$$";
    my $file_out = "$checkfile_dir/$dol" . "_blast_temp";
    #print "/nfs/WWWdev/SANGER_docs/cgi-bin/Pfam/blast_test.pl $Bio::Pfam::Web::PfamWWWConfig::tempdir/$pfamb_file_name $file_out $blastcutoff <P>";
    open(_BLEE, "/nfs/WWW/SANGER_docs/cgi-bin/Pfam/blast_test.pl $checkfile_dir/$pfamb_file_name $file_out $blastcutoff |");
    while(<_BLEE>) {
     # print "boo : $_ <P>";
    }
    close(_BLEE);

#    if ($temp_var) {
#      print _BLEE "ERROR: $error $! and $error2 \n";
      
#    }
  #  system("/nfs/WWW/cgi-bin/Pfam/blast_test.pl /nfs/WWW/htdocs/tmp/blast/$pfamb_file_name $file_out $blastcutoff");

    # if blank do the blast job & write to the file
    # else get input from the file :-)

#    print "BEFORE BEFORE <P>";

    
#    eval {
            
#  #    print "CREATE BLAST OBJECT <P>";
#     # my $blee = Bio::Tools::Blast->new() || or print "CANNA OPEN AS $! <P>";
#     	$blastres = Bio::Tools::Blast->new('-parse' => 1, 
#					   '-signif' => $blastcutoff,
#					   '-wait' => 300);
#	#print "BLAST: $blastres <P>";
#	$www_blast_res = WWWBlastResults->new('-cutoff' => $blastcutoff);
#	$www_blast_res->parse_blast_results( $blastres );
##	print "www: $www_blast_res <P>";

#    };
#    if ($@) {
#      print "Problem as $@ <P>";
#    }
#  #  print "AFTER: WWW BLAST: $www_blast_res <P>";
#    if ($@) {
#	$error = "true";
#    }





    open (STDIN, ">&SAVEIN");
    $file_out = "$checkfile_dir/$dol" . "_blast_temp";
    #print "FILE OUT: $file_out <P>";
#    print _BLEE "EEP OUT: $file_out\n" if ($temp_var);
#    close(_BLEE);
    if (defined $error) {
      $file_out;
	#return undef;
    }
    else {
      $file_out;
#	return $www_blast_res;
    }

}

#############################
#
#  HMM SEARCH
#
#############################

sub hmm_search {
    my $query = shift;
    my $type = shift;
    my $ethr = shift;

    my ($res, $db);

    if ($type =~ /frag/i) {
	$db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::frag_hmmlib_fname";
    }
    else {
	$db = "$Bio::Pfam::Web::PfamWWWConfig::hmmlibs_path/$Bio::Pfam::Web::PfamWWWConfig::std_hmmlib_fname";
    }
    

    open(PROG,"$Bio::Pfam::Web::PfamWWWConfig::prog -E $ethr $db $query |") or
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open pipe to $Bio::Pfam::Web::PfamWWWConfig::prog - misconfiguration somehere");

    $res = new WWWHMMResults;
    $res->parse_hmmpfam(\*PROG);
    close(PROG) or &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not close $Bio::Pfam::Web::PfamWWWConfig::prog pipe $! $?");

    return $res;
}



#############################
#
#  READ THRESHOLDS
#
#############################

sub read_thresholds {
    my ($ls_ga, $ls_nc, $fs_ga, $fs_nc);
    my $db = shift;
      open(THR,"$Bio::Pfam::Web::PfamWWWConfig::thres_fname") or
	return undef;  
    ($ls_ga,$ls_nc, $fs_ga, $fs_nc) = &HMMThres::read_ga_nc(\*THR);
    close(THR);
    return ($ls_ga, $ls_nc, $fs_ga, $fs_nc);
}

#############################
#
#  WRITE TABLE
#
#############################

sub write_table {
    my $file = shift;
    my $title = shift;
    my $link = shift;
    my $re = shift;
 
    print "<center>\n";
    print $file "<span class=normalmediumtext>$title</span><P>\n";
    $re->write_html_table($file, $link, undef, $title);
    print $file "</center>\n";
}

#############################
#
#  WRITE ALIGNMENT
#
#############################

sub write_alignment {
    my $file = shift;
    my $title = shift;
    my $link = shift;
    my $pro = shift;
    my $re = shift;
    my $db = shift;
    my $pfam_alignment = shift;
    

    print $file "<center><span class=normallargetext>$title</SPAN></center><p>\n";
    $re->write_html_align($file, $link, $pro, $db, $pfam_alignment, $title);
}

#############################
#
#  ADD NON PFAM REGIONS
#
#############################

sub add_non_pfam_regions {

  my($annseq, $results, $hmm_db, $prot) = @_;

 my ($unit,$seq,$line,$t,$temp2,$name,$acc,$start,$end,$subseq,$sname);

foreach $seq ( $results->eachHMMSequence() ) {
        foreach $unit ( $seq->eachHMMUnit() ) {
	  $name = $unit->hmmname;
	  $acc  = $unit->hmmacc;
	  $start = $unit->start_seq();
	  $end   = $unit->end_seq();
	  $subseq = $prot->seq($start, $end);
	  
	  $annseq->addAnnotatedRegion( Bio::Pfam::HMMOtherRegion->new('-DOMAIN' => "$name",
								       '-FROM' => "$start",
								       '-TO' => "$end",
								       '-HMM_DB' => "$hmm_db"
									    ));

	}
      }

return $annseq;


}


#############################
#
#  CONVERT SMART NAMES
#
#############################

sub convert_smart_names {

  my $old_name = shift;

 open (_SMART,  "$Bio::Pfam::Web::PfamWWWConfig::data_root/smart_names") or
   &Bio::Pfam::Web::PfamWWWConfig::exit_html("Could not open the smart_names file: $Bio::Pfam::Web::PfamWWWConfig::hmmfetch_libs_path/smart_names ");


  my $new_name;
  while(<_SMART>) {
    chop($_);
    my ($temp_new_name, $temp_old_name) = split(/ /, $_);
    if ($temp_old_name eq $old_name) {
      $new_name = $temp_new_name;
      last;
    }


  }

  close(_SMART);

  return $new_name;

}

####################################################################################################################
#   EXISTING PROTEINS PFAM-B ROUTINES -- NEEDS TO BE UPDATED AND MOVED SOMEWHERE ELSE !
####################################################################################################################
 
#############################
#
#  EXISTING B WRITE HTML ALIGN
#
#############################


sub b_predict_write_html_align {
  
  
  my ($seq_id, @store) = @_;
  
  my ($t,$temp2,$name,$acc,$start,$end,$subseq,$sname);
  
  if (@store) {
    
    print "<P><HR><P><CENTER><span class=normalmediumtext>Alignments of Pfam-B domains to best-matching to Pfam-B sequence</span></CENTER><P>\n";
    print  "<form method=\"POST\" action=\"$Bio::Pfam::Web::PfamWWWConfig::align2seed\">";
    
    print qq(
	     <span class=normaltext>Format for fetching alignments to Pfam-B families: </span>
	     <select name=format >
	     <option value=linkswisspfam> Hypertext linked to swisspfam
	     <option value=mul> Plain Pfam format
	     <option value=stock> Plain Stockholm format
	     <option value=fal> Fasta format
	     <option value=belvu> Belvu helper application
	     <option value=jalview> Jalview Java alignment viewer
	     
	     </select>
	     
	     <p>
	    );
    
    my $count = 0;
    
    my($outer_loop) = 0;
    
    while ($outer_loop <= $#store ) {
      my ($sname, $start,$end,  $acc, $name) = split(/~/, $store[$outer_loop]->{'key'});
      my $subseq = $store[$outer_loop]->{'string'};
      
      $outer_loop++;
      
      
      my $temp2;
      
      $temp2 = "<center><input type=hidden name=name$count value=$name>\n";
      $temp2 .= "<input type=hidden name=acc$count value=\"$acc\">\n";
      $temp2 .= "<input type=hidden name=start$count value=\"$start\">\n";
      $temp2 .= "<input type=hidden name=end$count value=\"$end\">\n";
      $temp2 .= "<input type=hidden name=sname$count value=\"$sname\">\n";
      $temp2 .= "<input type=hidden name=subseq$count value=\"$subseq\">\n";
      $temp2 .= "<input type=submit name=submit$count value=\"Align to family\"></center>";
      
      $count++;
      
      my $t = "<A href=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$acc>$name</A>";
      print  sprintf("<a name=\"%s\"><img src=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/gifs/arrow.gif> <span class=normaltext>Query</a> %s/%d-%d matching $t</span><pre>\n", "Pfam-B/".$start."-".$end, $seq_id, $start, $end, $t);
      b_predict_write_hsp_alignment( $seq_id, $sname, $start, $end, $subseq );
      
      print "</pre>\n$temp2\n<p>\n";
      
    }
    
    
    #	# lastly, the paramters that will be common to all alignments
	
	print  "<input type=hidden name=lastindex value=$count>\n";
	print  "<input type=hidden name=pfamB value=1>\n";
	print  "</form>";
    }
}



#############################
#
#  EXISTING B - TABLE
#
#############################

sub _predict_b_table {
  
  my (@store) = @_;
  print "<CENTER><span class=normalmediumtext>Matches to Pfam-B</SPAN><P>";
  print  sprintf("<table border=1 cellpadding=5 cellspacing=0 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour><tr bgcolor=#000070 ><th class=whitenormalfont><font color=FFFFFF>Domain</font></th><th class=whitenormalfont><font color=FFFFFF>Start</font></th><th class=whitenormalfont><font color=FFFFFF>End</font></th><th class=whitenormalfont><font color=FFFFFF>Alignment</font></th></tr>\n");
  my($outer_loop) = 0;
  
  while ($outer_loop <= $#store ) {
    my ($protein, $seq_start,$seq_end,  $pfamb_acc, $pfamb_id) = split(/~/, $store[$outer_loop]->{'key'});
    my $align = "Pfam-B/" . $seq_start . "-" . $seq_end;
    print "<TR><TD><A HREF=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$pfamb_acc>$pfamb_id</A></TD><TD>$seq_start</TD><TD>$seq_end</TD><TD><A href=#$align>Align</A></TD></TR>";
    $outer_loop++;
    
  }
  print  "</table><br><br>\n";		
  print "</CENTER>";
}

#############################
#
#  EXISTING B - ASCII OUT
#
#############################

sub b_predict_write_ascii_out {
  
  my (@store) = @_;

 #   if (not defined $fh) {
 #       $fh = \*STDOUT;
 #   }

  my($outer_loop) = 0;
  
  while ($outer_loop <= $#store ) {
    my ($protein, $seq_start,$seq_end,  $pfamb_acc, $pfamb_id) = split(/~/, $store[$outer_loop]->{'key'});

    print  sprintf("%s %4d %4d %s  %s\n",$protein, $seq_start,$seq_end, $pfamb_acc,  $pfamb_id);

  #  my $align = "Pfam-B/" . $seq_start . "-" . $seq_end;
 #   print "<TR><TD><A HREF=$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$pfamb_acc>$pfamb_id</A></TD><TD>$seq_start</TD><TD>$seq_end</TD><TD><A href=#$align>Align</A></TD></TR>";
 
    $outer_loop++;
    
  }

   # if ($self->number()) {
#        foreach my $hsp ( $self->each_hit ) {
#            my ($acc, $name) = $hsp->parent->desc() =~ /^(\S+);(\S+);$/;

#            print $fh sprintf("%s %4d %4d %s %4.2f %4.2g %s\n",$hsp->parent->parent->name, $hsp->start('query'),$hsp->e
#nd('query'), $acc, $hsp->bits, $hsp->expect, $name);
            
#        } 
#    }

}


#############################
#
#  EXISTING B - HSP ALIGNMENT
#
#############################

sub b_predict_write_hsp_alignment {
  my ($qid, $tid, $qstart, $qend, $query) = @_;
  
  my $tstart = $qstart;
  my $tend = $qend;
  my $blocksize = 50;
  
  my $namelength = 5;
  if (length($qid) > length($tid)) {
    $namelength += length($qid);
  }
  else {
    $namelength += length($tid);
  }
  
  my $startlength = (length($qend) > length($tend))?length($qend):length($tend);
  
  my $sbjct = $query;
  my $match = $query;
  
  
  my $offset = 0;
  my $qcount = $qstart;
  my $tcount = $tstart;
  while ($offset <= length($query)) {
    my $formatstring1 = "%$namelength"."s"." "x$startlength."  %s\n";
    my $formatstring2 = "%$namelength"."s %".$startlength."d %s %s\n";
    
    my $temp;
    ($temp = substr($query, $offset, $blocksize)) =~ s/[.-]//g;
    my $querychars = length($temp);
    ($temp = substr($sbjct, $offset, $blocksize)) =~ s/[.-]//g;
    my $sbjctchars = length($temp);
    
    print  sprintf("$formatstring2", $tid, $tcount, substr($sbjct, $offset, $blocksize), $tcount + $sbjctchars - 1); 
    print  sprintf("$formatstring1", "", substr($match, $offset, $blocksize)); 
    print  sprintf("$formatstring2", $qid, $qcount, substr($query, $offset, $blocksize), $qcount + $querychars - 1);
    
    $offset += $blocksize;
    $qcount += $querychars;
    $tcount += $sbjctchars;
    
    print "\n";
  }
}


#############################
#
#  EXISTING B - ADD RESULTS TO ANNSEQ OBJECT
#
#############################

sub b_predict_add_results_to_AnnSeq {
  my ($annsq, @store) = @_;
  
  my($outer_loop) = 0;
  
  while ($outer_loop <= $#store ) {
    my ($sname, $start,$end,  $acc, $id) = split(/~/, $store[$outer_loop]->{'key'});
    $outer_loop++;
    
 #   my $annot = Bio::Annotation->new('-DESCRIPTION' => $id );
    $annsq->addAnnotatedRegion(  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
							  '-PFAM_ID' => $id,
							  '-FROM' => $start,
							  '-TO' => $end,
							  '-ANNOTATION' => $id 
							 )
			      );
    }

 
}




######################################
#
# OTHER REGIONS 
#
#
########################################


sub add_other_domains {

  my ($annseq, @values) = @_;
 # $annseq = add_other_domains($annseq , @values) if($values[0] =~ /[A-Z]/i);
  
  foreach my $domain_val (@values) {

    my ($domain_type, $program, $start, $end, $score) = split(/~/, $domain_val);

    $annseq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => "",
							     '-FROM' => $start,
							     '-TO' => $end,
							     '-TYPE' => $domain_type,
							     '-SOURCE' => $program,
							     '-SCORE' => $score

							     ));


  }
 


  return $annseq;

}




################ FIND TM REGIONS ##########################
#
#
#


sub find_tmhmm {

  my $prot_seq = shift;



  my @values;
  my $dol = "$$";
  

  my $tm_file = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$dol" . "_trans_data.dat";

  my $exit = system("cat $prot_seq | $Bio::Pfam::Web::PfamWWWConfig::file_root/bin/TMHMM2.0/bin/decodeanhmm -f $Bio::Pfam::Web::PfamWWWConfig::file_root/bin/TMHMM2.0/lib/TMHMM2.0.options -modelfile /nfs/WWW/SANGER_docs/htdocs/Software/Pfam/bin/TMHMM2.0/lib/TMHMM2.0.model > $tm_file ");
 
#print "exit: $exit <P>";

 open(_TMHMM, "$tm_file");


  my($id, %transmem );
  my $last_membrane = 0;
  while (<_TMHMM>) {
        if ($_ =~ /%pred/){
      
      my($junk, $values) = split(/:/, $_);
      my(@trans) = split(/,/, $values);
      
      foreach (@trans) {
	
	$_ = substr($_, 1);
	
	my($orien, $start, $end) = split(/ /, $_);
	
	$orien = uc($orien);
	if($orien =~ /M/i) {
	  
	  $transmem{$orien} = $start . " " . $end;
	} else {
	  
	  $transmem{$orien} = $start;
	}
		
	if ($orien =~ /M/i) {
	  $last_membrane = 1;
	} elsif ($last_membrane) {
	  my($start, $end) = split(/ /, $transmem{'M'});
#	  print  "\\N\t$id\t$start\t$end\ttransmembrane\ttmhmm\t\t";
	  if($transmem{'I'} < $transmem{'O'}) {
#	    print   " I-O\n";
	  } else {
#	    print  " O-I\n";
	  }
	  push @values, "transmembrane~tmhmm~$start~$end~";
	  $last_membrane =0;
	}
	
      }
      
      
    }
    
    
    
  }

#print "TRANS VALUES: @values <P>";
  close(_TMHMM);
  system("rm -f $tm_file ");

  return @values;

}



#################### FIND SIGNAL PEPTIDE REGIONS ####################
#
#


sub find_sigp {

  my $prot_seq = shift;

 my $queue = 'bsub -I -q fastblastq';

my $tempname_2 = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/sigp.dat";

  # Put the first 60 aa's into a seperate file
  open(_PFAMSEQ, $prot_seq) or die "Fatal: Could not open file";
  while(<_PFAMSEQ>) {

    if ($_ =~ /^\>/) {

    } else {

      my($sub_str);
      
      ## Get only what we need !!
      $sub_str  = substr $_, 0, 50;

      open (TMP, ">$tempname_2");
      print TMP $sub_str . "\n";
      close TMP;

      last;
      
    }
  }

  close(_PFAMSEQ);
 

  ## RUN sigp on the file
  print "/nfs/WWW/SANGER_docs/htdocs/Software/Pfam/bin/signalp-1.2/./signalp  -t euk $tempname_2  <P>";

  open(_SIGP, "/nfs/WWW/SANGER_docs/htdocs/Software/Pfam/bin/signalp-1.2/./signalp  -t euk $tempname_2 | ") or die "Could not open the file \n";
 
  my($name);
  
  my (%line_store, $line_counter);
  $line_counter = 1;
  
  my $sigp_data;
  while(<_SIGP>) {
    print "$_ <BR>";
    $name = $1 if( $_ =~ /^>(\S+)/);
    
    
    $line_store{$line_counter} = $_ if ($_ =~ /max./i);
    if ($_ =~ /Most likely/i) {
      
      
      my($numbers) = m/(\d+)/;
      
      
      if ( ( $line_store{1} =~ /YES/i  ) || ($line_store{2} =~ /YES/i ) || ($line_store{3} =~ /YES/i ) ) {
	$sigp_data = "sig_p~signalp~1~$numbers";
	
      }
      
      
      
    } #/ end if Actual output
    
    if ($line_counter =~ /3/) {
      $line_counter = 1;
    } else {
      $line_counter++;
  }

  } #/ end WHILE
  close(_SIGP);

  return $sigp_data;



}


#################### FIND COILED REGIONS #########################
#
#

sub find_coiled_regions {

  my $prot_seq = shift;
my $queue = 'bsub -I -q fastblastq';
  my @store_values;
 
	my( %x_position);

	my $one_sequence;
	
	my($first) = 1;
	my($num) = 1;
	my($prev);

#  my $exit = system("/usr/local/pubseq/bin/ncoils -f < $prot_seq >! $Bio::Pfam::Web::PfamWWWConfig::tempdir/coiled_regions.dat");
#  print "exit: $exit <P>";
open (_NCOILS, "/usr/local/pubseq/bin/ncoils -f < $prot_seq | " ) or print STDERR "Pfam: Fatal: could not open + NCOILS";
 
 
  while(<_NCOILS>) {
 #   print "$_ <P>";
	if  ($_ =~ /^\>/)  {
		$one_sequence = "";


	} else {
		chop($_);
		$one_sequence .= $_;

	}

  }

	while ($one_sequence =~ m/x/g) {
	
		my($x_pos);

		$x_pos =  pos $one_sequence;

#		print "$x_pos, ";

		## FIRST ONE

		if($first) {
	
			$first = 0;
			$x_position{$num} = $x_pos;
			$prev = $x_pos;
	#		print "FIRST x  : $x_pos , ALL-$x_position{$num}, prev = $prev  \n";
			
#
		} else {

			### AMEND TO PREVIOUS
			if ( ($prev + 1) eq ($x_pos)) {
				$prev = $x_pos;
				if ($num eq 8) {
		#		print "AMMEND -> $prev \n";
				}
				
			} else {

			### WHOLE NEW REGION

				$x_position{$num} .= "~" . $prev;
#				print "NEW :num : $num,  $x_position{$num}\n"; 
				$num++;
				$x_position{$num} = $x_pos;
				$prev = $x_pos;
#				print "NEW -> $x_pos, $num , total =>  $x_position{$num} \n";

			} #/ end IF

		} #/ end IF


	

	} #/ end WHILE	


	## ADD THE LAST ONE
	$x_position{$num} .= "~" . $prev;


	foreach my $x_arr (sort keys %x_position) {

		if ($x_position{$x_arr} =~ /~/) {

#			print "coiled_coil : $x_arr -> ";

			my($from, $to) = split(/~/, $x_position{$x_arr});

			push @store_values, "coiled_coil~ncoils~$from~$to" if ($from =~ /[0-9]/);
 
		} #/ end IF


	
	} #/ end FOR
	

#	$total += $rdb->update_other_reg( \@list, "coils_test");


  return @store_values;

}


sub sort_print_nonhmm_regs {

  my(@array) = @_;
  
  if($array[0]) {
    
    print "<P><hr><P>";
    my ( @doms);
    foreach (@array) {
      my %temp;
      my($region, $package, $from, $to) = split(/~/, $_);
      %temp = ( 'reg' => $region,
		'package' =>$package,
		'from' => $from,
		'to' => $to
	      );
      push @doms, \%temp;
      
    }
    
    
    @doms = sort { $a->{'from'} <=> $b->{'from'} } @doms;

    print qq(<center><span class=normalmediumtext>Other regions</SPAN><P><table border=1 cellpadding=5 cellspacing=0 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour><tr bgcolor=#000070 ><td class=whitenormalfont>Type</TD><TD class=whitenormalfont>Source</TD><TD class=whitenormalfont>Start</TD><TD class=whitenormalfont>End</TD></TR>);

    foreach my $reg (@doms) {

      my %non_hmm = %{$reg};
      print "<TR><TD class=normaltext>" .$non_hmm{reg} . "</TD><TD class=normaltext>" .$non_hmm{"package"} . "</TD><TD class=normaltext>" . $non_hmm{from}. "</TD><TD class=normaltext>" .$non_hmm{to} . "</TD></TR>";

    }


    print qq(</TABLE></CENTER><P><P>);

  }

}



############ FIND LOW COMPLEXITY REGIONS : SEG #################
#
#
#


sub find_seg {

  my $prot_seq = shift;
my $queue = 'bsub -I -q offlineblastq';
# my $queue = ' ';
 my @all_values;

 
#print "HERE  : $prot_seq <P>";
#print "$queue /usr/local/pubseq/bin/seg $prot_seq -l <P>";
#exit(0);

#  print " bsub -q offlineblastq ' /usr/local/pubseq/bin/seg $prot_seq -l > $Bio::Pfam::Web::PfamWWWConfig::tempdir/seg_results.dat'  <P>";
#exit(0);
#my $exit =   system(" /usr/local/pubseq/bin/seg $prot_seq -l > $Bio::Pfam::Web::PfamWWWConfig::tempdir/seg_results.dat ");
#print "exit: $exit <P>";
#exit(0);

  #  open (_SEG, "$Bio::Pfam::Web::PfamWWWConfig::tempdir/seg_results.dat ") or die "Could not open the seg command - $!";
open (_SEG, "/usr/local/pubseq/bin/seg $prot_seq -l  |  ") or print STDERR "Pfam: Could not open the seg command - $!";
  while(<_SEG>) {
 #   print "$_ <P>";

	my ($seqid, $from, $to, $score);
	
	/^>(\S+)\((\d+)\-(\d+)\)\s+complexity=(\S+)/ && do {
	    $seqid = $1;
	    $from = $2;
	    $to = $3;
	    $score = $4;
	    push @all_values, "low_complexity~seg~$from~$to~$score";
	    
	};
    }




 # print "ALL: @all_values <P>";
#  exit(0);
  return @all_values;

}



1;  # says use was ok
