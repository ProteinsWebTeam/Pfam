#!/usr/local/bin/perl


use Getopt::Long;
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';
use lib '/nfs/team71/pfam/mm1/rfam_cvs/scripts/Modules';
use Rfam;
use RfamWWWConfig;
use vars qw(%all_rfamseq);
#my $rdb = Rfam->live_rdb();
my($input_dir, $output_dir, $file_type, $ss_cons_only, $family, $web_file);

&GetOptions(  'input_dir=s' => \$input_dir,
	      'output_dir=s' => \$output_dir,
	      'file_type=s' => \$file_type,
	      'ss_cons_only' => \$ss_cons_only,
	      'family=s' => \$family,
	      'seed=s'  => \$web_file);

#die "need input_dir\n" if(!$input_dir);
#die "need output_dir\n" if(!$output_dir);
#die "need file_type , either seed or full \n" if (!$file_type);

$file_type = "seed";
#my(@results);
#if ($family) {
#  (@results) = $rdb->query("select rfamseq_acc, species from rfamseq, rfam, rfam_reg_" . $file_type. " where rfam_acc = '$family'  and rfam.auto_rfam  = rfam_reg_" . $file_type. ".auto_rfam and rfam_reg_" .$file_type . ".auto_rfamseq  = rfamseq.auto_rfamseq ");
#} else {
#  (@results) = $rdb->query("select rfamseq_acc, species from rfamseq");
#}


#foreach (@results) {
#  my($rfamseq_acc, $spec) = @{$_};
#  my $spec_short;
#  if ($spec =~ /(\S\S\S)\S*?\s+(\S\S\S)\S*?.*/) {
#    $spec_short = $1 . "." . $2 . ".";
#  } elsif ($spec =~ /(\S\S\S)\S*?\s+(\S)\s+/) {
#    $spec_short = $1 . "." . $2 . ".&nbsp;&nbsp;";
#  } elsif($spec =~ /(\S\S\S)/) { 
    
#    $spec_short = $1 . ".&nbsp;&nbsp;&nbsp;&nbsp;";
#  }
#  $all_rfamseq{$rfamseq_acc} = $spec_short;
#}


my @colours = ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z");

my %colours_count;
my $count = 0;
foreach (@colours) {
  $colours_count{$_} = $count;
  $count++;
}


my(@seq, @sec_struc, $conserved, %names);
my $count = 0;
my $total_count = 0;
my $maxname_len = 0;
my (@arrays, @new_seq, @seqs_match);
my $length;
my @blocks;
my $conserved;
#my $total_seq_count;

$input_dir = $input_dir . "/$file_type";
if ($web_file) {
  
  _do_one_family($family, $input_dir, $output_dir, $file_type, $ss_cons_only, $web_file);
  exit(0);
} elsif ($family) {
  $family .= ".full.gz";
  _do_one_family($family, $input_dir, $output_dir, $file_type, $ss_cons_only);
  exit(0);
}


opendir(_WHOLE, "$input_dir") || die("Could not open $input_dir $!");

foreach my $file ( readdir(_WHOLE) ) {
  $file =~ /^\.+$/ && next;
  #next if ( ($file =~ /RF00177/) && ($file_type =~ /full/)  );
  _do_one_family($file, $input_dir, $output_dir, $file_type, $ss_cons_only  );
} 


sub _do_one_family {

  my($file, $input_dir, $output_dir, $file_type, $ss_cons_only, $web_file) = @_;
 # print "FILE: $file, $input_dir, $output_dir, $file_type, $ss_cons_only, $web_file \n";
  @blocks = @arrays = @new_seq = @seq = @sec_struc = ();
  %names = {};
  $count = $total_count = $maxname_len =  $length = $conserved = $maxdisplayname_length = undef;

  
  my $file_sub;
  $file_sub = $1 if ($file =~ /(RF\d+\.full).gz/);

  my $complete_file = $input_dir . "/" . $file;

 # $complete_file = "/nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/full/Rfam.full.tmp";
#  print "FILE: $complete_file \n";
 # my $file_sub;
 # $file_sub = $1 if ($file =~ /(RF\d+\.full.gz)/);

  my $complete_out = $output_dir . "/" . $file_type. "/" . $file_sub;
#print "OUT: $complete_out \n";
  #$complete_out = "/nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/markup_align/full/Rfam.full.tmp";


  if ($web_file) {
    open(_FILE, "$web_file");
    
  } else {

    open(_FILE, "gunzip -c $complete_file |");

  }
 #  open(_FILE, " $complete_file |") or print "CANNA AS $! \n";
  
  while(<_FILE>) {
   #print "$_ ";
    next if ($_ =~ /\# STOCKHOLM/);
    chop($_);
    #print "$_";
    
    if ($_ !~ /^\#/) {
      if ($_ =~ /(\S+)\s+(\S+)/ ) {
	$seq[$count] .= $2 ;
	#    my $seq = $2;
	#   print "seq: $seq \n";
	#     $length = length $seq if(!$length);
	my $name = $1;
	#  print "NAME: $name :: len : " . length($name) ." \n"; sleep 1;
	#    chop($1);
	$names[$count] = $1;
#	print "NAME: $1 \n";
#	print "COUNT: $count \n" if ($1 =~ /U41756/);
	$maxdisplayname_length = length $1 if (length $1 > $maxdisplayname_length);
	if ( ($file_type =~ /seed/i) || ($ss_cons_only)  ) {
	  
	  $count++;
	  
	}



      }
      #  print "COUNT: $count :: seq: $1 \n";
      
    } elsif ($_ =~ /\#=GR\s+\S+\s+SS\s+(\S+)/) { 
      $sec_struc[$count] .= $1;
      $count++;
      
      
      
    } elsif ( $_ =~  /\#=GC\s+SS_cons\s+(\S+)/) {
 #     print "$_ \n";
      $conserved .= $1;
      $count = 0;
      
      
    }
    
    
    
  }
  
 # $seq_count = $count;
 # print "COUNT: $total_seq_count \n";

  $length = length $seq[1];
  #print "LEN: $length \n";
  #exit(0);
  
  $maxdisplayname_length = $maxdisplayname_length + 1;
  
  close(_FILE) or die "CANNA CLOSE FILE $web_file  AS $! \n";
  
  ;
  
  my $start = 0;
  #print "LENGTH: $length \n";
  

  while($start < $length) {
    push @blocks, $start;
    
    $start = $start + 79;
  }
  
  #print "LENGTH: $length :: BLOCKS: @blocks \n";
  my $count = 0;
  
  
  my %all_arrays;
  
  
  
  my $file_count = 1;
  my $count_track = 0;
  
  my ($junk, $temp_arr, $temp_col, %cons_colour) = _add_markup("" , $conserved );

  my @col_blocks = @{$temp_arr};
  my @col_colours_blocks = @{$temp_col};
  

#  print "HERE @col_blocks \n";
#  exit(0);
#  foreach my $key(sort keys %cons_colour) {
#    print "key: $key, val: " . $cons_colour{$key}. " \n";
#  }


 

  my $key = 0;

  my %fix_colours;
  my $colour_count = 0;

  my @colour_map;
  my(%lowest, %highest);

  my $last = undef;

 # print "OLD: @col_colours_blocks\n";

  foreach (@col_colours_blocks) {
    
    if(defined($fix_colours{$_})) {
      
    } else {
      
      $fix_colours{$_} = $colours[$colour_count];
      
      $colour_count++;
      
    }

    push @colour_map, $fix_colours{$_};

  }

 # foreach (sort keys %fix_colours) {
 #   print "key: $_ :: val: " .$fix_colours{$_} . "\n";
 # }
#print "NEW: @colour_map \n";

  while($key <= $length) {
  #  print "key: $key \n";sleep 1;
#    if(defined($fix_colours{$cons_colour{$key}})) {
      
#    } elsif ($cons_colour{$key} =~ /[a-z]/) {
      
#      $fix_colours{$cons_colour{$key}} = $colours[$colour_count];
      
#      $colour_count++;
      
#    }
    
    
    if (defined($cons_colour{$key})) {
      
     # print "KEY: $key :: val: " . $cons_colour{$key}. " \n";

      $cons_colour{$key} = $fix_colours{$cons_colour{$key}};
     # print "letter: " .$cons_colour{$key} . " count: " .$key . " \n";

      
#      ### find lowest
#      if (defined($lowest{$cons_colour{$key}})) {
#	$lowest{$cons_colour{$key}} = $key if ($key < 	$lowest{$cons_colour{$key}});
#      } else  {
#	$lowest{$cons_colour{$key}} = $key;	
#      }

#      ### find highest
#      if (defined($highest{$cons_colour{$key}})) {
#	$highest{$cons_colour{$key}} = $key if ($key > 	$highest{$cons_colour{$key}});
#      } else  {
#	$highest{$cons_colour{$key}} = $key;	
#      }
     

      
 #     if ($cons_colour{$key} ne $last) {
#	push @colour_map, $cons_colour{$key};
#	$last = $cons_colour{$key};
#      }

      
    }

    $key++;
  
  }
  
#exit(0);
#  my %colour_blocks;

#  foreach my $letter (sort keys %lowest) {
#    $colour_blocks{$letter} = $lowest{$letter} . "~" . $highest{$letter};
#   # print "letter: $letter , lowest: " . $lowest{$letter}. " , highest: " .$highest{$letter} . " \n";
    
#  }


#  print "COL: @colour_map \n";

#  exit(0);
  $cons_colour{SS} = 1;

#  my %cons_last_colour = %cons_colour;
#  $cons_last_colour{last} = 1;
my $tmp_seq_count = 0;
  
my $print = 0;
  foreach (@seq) {
    my $the_seq = $_;
    push @new_seq, $the_seq;
   # print "NAME: " . $names[
    my($new_seq, %colour, $temp);
 #   $print = 1 if ($count eq 12);
    if ( ($file_type =~ /full/i) && (!$ss_cons_only)  ){ 
      ($new_seq, $temp, $temp_cols, %colour) = _add_markup($the_seq, $sec_struc[$count] , $print);
      
   #   foreach (sort keys %colour) {
#	print "key: $_ :: val: " .$colour{$_} . " \n";
#      }

      my @temp_blo_col = @{$temp};
      my @temp_col_col_block = @{$temp_cols};

      my $last;
      my @col_map;


      %fix_colours = undef;

      my $colour_count = 0;

      foreach (@temp_col_col_block) {
    
#	print "BLEE: $_ \n";
	if(defined($fix_colours{$_})) {
      
	} else {
      
	  $fix_colours{$_} = $colours[$colour_count];
      
	  $colour_count++;
      
	}

	push @col_map, $fix_colours{$_};

      }

 #     print "TEMP : @temp_col_col_block\n";
    #  foreach (@temp_col_col_block) {
#		print "BLEE: $_ \n";
#	push @col_map,$fix_colours{$_}; 
	
#      }



      my $key = 0;
      my $first  = 1;
      while($key <= $length) {

	if (defined($colour{$key})) {

#	  print "OLD: " .$colour{$key} . "  ::  ";
	  $colour{$key} = $fix_colours{$colour{$key}};
#	  print  "NEW : " . $colour{$key}. " \n";
#	  if ($colour{$key} ne $last) {
#	    push @col_map, $colour{$key};
#	    $last = $colour{$key};
#	  }

	  if ($first) {
	  #  print "COL: $colour{$key} \n";
	    $first = 0;
	  }
	}

	$key++;

      }

     
      if (@col_map eq @colour_map) {
	#print "EQUALS \n";
      } else {

	%colour = undef;
	my $key = 0;

	my $cons_arrows = $conserved ;
	
	my $new_seq = $the_seq;
	my $new_arrows = $sec_struc[$count];

	my %temp_new_colours;

	while($key <= $length) {
	  
	  my $sub_cons_arrow = substr($cons_arrows, $key, 1);
	  my $sub_new_seq = substr($new_seq, $key, 1);
	  my $sub_new_arrows = substr($new_arrows, $key, 1);

	#  print "SUB ARROW: $sub_cons_arrow  :: NEW ARROW: $sub_new_arrows -> letter $sub_new_seq \n";

	  if ( $sub_cons_arrow ne $sub_new_arrows) {

	  # print "SUB ARROW: $sub_cons_arrow  :: NEW ARROW: $sub_new_arrows -> letter $sub_new_seq \n";
	  } else {
	    $temp_new_colours{$key} = $cons_colour{$key};
	  }

	  $key++;
	}
	%colour = undef;
	%colour = %temp_new_colours;



      }

    } else {

      ### Got a seed alignment

     # ($new_seq, $temp, $temp_cols, %colour)
      ($new_seq, $temp, $temp_cols,%colour) = _add_markup($the_seq, $conserved);

      
      
      my $key = 0;
      my %new_fix_colours;
      my $colour_count = 0;
      
      
      
      
      while($key <= $length) {
	
	if(defined($new_fix_colours{$colour{$key}})) {
	  
	} elsif ($colour{$key} =~ /[a-z]/) {

	  
	  $new_fix_colours{$colour{$key}} = $colours[$colour_count];
	  
	  $colour_count++;
	  
	}
	
	
	if (defined($colour{$key})) {
	#  print "key: $key \n";sleep 1;
	#  print "TMP key: $key MATCH: " .$seqs_match[$tmp_seq_count]{$key} . " \n";
	  if ($seqs_match[$tmp_seq_count]{$key}) {
	    $colour{$key} = $new_fix_colours{$colour{$key}};# if ($seqs_match[$tmp_seq_count]{$key});
	  } else {
	    $colour{$key} = undef;
	  }
	  
	  
	  
	}
	$key++;
	
      }
      
      
      
      


    }
      
  #   print "\n\n$new_seq \n" if ($count eq 11);
    #exit(0);
#    my $colour_array = %colour;
    
    push @arrays, \%colour;
    
    $count++;
  #  exit if ($count > 12);
    if (not($count % 500)) {
      ##my $file_out = $complete_out . "." . $file_count;
      my $file_out;
      if ($web_file) {
	$file_out = $web_file . "." . $file_count;
      } else {
	$file_out = $complete_out . "." . $file_count;
      }
      push @arrays, \%cons_colour;
      push @new_seq, $conserved;

      _print_to_file( $count_track, $file_out,$maxdisplayname_length );
      @new_seq = ();
      @arrays = ();
      $file_count++;
      $count_track = $count_track + 500;
      
    }
    $tmp_seq_count++;
  }
  #print "SEQ: @new_seq \n";

  push @arrays, \%cons_colour;
  push @new_seq, $conserved;
  my $file_out;
  if ($web_file) {
    if ($web_file !~ /htdocs/) {
      $file_out = $file . ".html";
    } else {
      $file_out = $web_file . "." . $file_count;
    }

  } else {
   $file_out = $complete_out . "." . $file_count;
  }
  _print_to_file( $count_track, $file_out,$maxdisplayname_length ) if(defined($new_seq[0]));
  

}



#print "BLOCKS: @blocks SEQ: @seq\n";





#########################################
#
#   print the marked up sequences to file :-)
#
##########################################

sub _print_to_file {

  my ( $seq_name_count, $file_out,$maxdisplayname_length )  = @_;
#  print "LENGTH: $maxdisplayname_length \n";
  open(_OUT, ">$file_out") || print "CANNA OPEN $file_out as $! \n";
#  print _OUT "<html><head><link REL=\"stylesheet\" HREF=\"rfam_align.css\"></head><body>";
  
  my $block_count = 0;
 # print "BLOCKS: @blocks \n";
  if ($file_out =~ /html/) {
    print _OUT "<HTML><HEAD><LINK REL=\"stylesheet\" HREF=\"rfam_align.css\"></HEAD><BODY>\n";
  }
  print _OUT "<pre>";
  foreach my $block (@blocks) {
#    print "bloock: $block \n";sleep 1;
    my $internal_seq_name_count = $seq_name_count;
    my $seq_count  = 0;
    foreach my $seq (@new_seq) {
      my $parsed_sub;
      #  print "SEQ: $seq \n"; sleep 1;
      my $sub = substr($seq, $block, 79);
   #   print "SUB: $sub \n";
      my $sub_count = 0;
      
      #  my $tmp_block = $block;
      
      while($sub_count < 79) {
	#print "$sub_count \n";
	my $tmp_sub = substr($sub, $sub_count, 1);
	my $prefix = $sub_count + $block;
	#print "$seq_count, $prefix \n";
	if(defined($arrays[$seq_count]{$prefix})) {
	  
	  $tmp_sub = "<b ID=\"" .$arrays[$seq_count]{$prefix} . "\">$tmp_sub</b>";
	 # 	print "SUB: $tmp_sub \n";
	} else {
	  $tmp_sub = "<b>$tmp_sub</b>";
	}
	
	
	$parsed_sub = $parsed_sub . $tmp_sub;
	$sub_count++;
      }
      #  print "PARSED: $parsed_sub \n";
      #  print "MAX: $maxdisplayname_length : LEN: " . . " 

      my $name; 
      my $spaces;
      my $species_short = "&nbsp;" x 8;
      if(defined($arrays[$seq_count]{SS}) ) {
#	print "SS_cons " .$names[$internal_seq_name_count] . "  : COUNT: $internal_seq_name_count \n";
#	print _OUT "\n";
	$spaces = "&nbsp;" x ($maxdisplayname_length - length("SS_cons"));
	$name = "<a name=\"" . $seq_count . "~" . $blocks[$block_count] . "\">SS_cons</A>";
      } else {
	my $link = $RfamWWWConfig::srsserver;
#	print "LINK: $link \n";
#	print "WOW: " .$names[$internal_seq_name_count] . " \n";
	my $acc = $1 if ($names[$internal_seq_name_count] =~ /^(\S+)\.\d+\// );
	#chop($acc); # remove the dot
#	print "\nACC: $acc \n";
	$species_short = &RfamWWWConfig::species_for_rfamseq($acc);
#	print "SHORT: $species_short \n";
	#$species_short = $all_rfamseq{$acc};
	$species_short = "<font color=#FF0000>" . $species_short . "</FONT>";
	$link =~ s/THEACC/$acc/;
#	print "FINAL : $link \n";
#exit(0);
	$name = "<a name=\"" . $seq_count . "~" . $blocks[$block_count] . "\"><A href=$link>" . $names[$internal_seq_name_count] . "</A></A>";
	$spaces = "&nbsp;" x ($maxdisplayname_length - length($names[$internal_seq_name_count])); 
      }

    #  my $spaces = "&nbsp;" x ($maxdisplayname_length - length($names[$internal_seq_name_count])); 
      # print "SUB: $sub \n seq: $seq \n\n";

  

      # print "Name: $name \n"; sleep 1;
      my $next = "<A HREF=#" . $seq_count . "~" . $blocks[$block_count + 1]. ">Next</A>" if (defined($blocks[$block_count + 1] ) );
  
      my $prev = "<A HREF=#" . $seq_count . "~" . $blocks[$block_count - 1]. ">Prev</A>" if ( $block_count > 0 );

 #     print "BLOCK: $block_count :: PREV: $prev \n";

      print  _OUT  sprintf("%s%s%s  %s  %s %s\n", $name, $spaces, $species_short, $parsed_sub, $next, $prev);
      
      # print _OUT "$names[$seq_count]$spaces$parsed_sub <br>";
      
      # my $spaces = "        ";
      # print _OUT  sprintf("%-22s      %s\n",$names[$seq_count],$parsed_sub);
      # print _OUT $names[$seq_count] . "  $parsed_sub<br>";
      $seq_count++;
      $internal_seq_name_count++;
    }
  
    print _OUT "\n\n";
    $block_count++;
  }

  print _OUT "</pre>";
  close(_OUT);
  system("gzip -f $file_out") if ($file_out !~ /html/);
 # exit(0);


}



#############################
#
#  SUB _add_markup
#
#############################

sub _add_markup {
  
  my($seq, $sec_struc, $print) = @_;
 #  print "\n$seq\n$sec_struc \n";
  my $new_seq, $new_sec_struc, %seqs_ma;
  
  
 
  
  my @font_colours;
  open(_COL, "/nfs/WWW/htdocs/Pfam/data/hex_colours");
  while(<_COL>) {
    my($num, $color, @junk) = split(/~/, $_);
    push @font_colours, $color;
    
  }
  
  close(_COL);
  
  
  my $old = $sec_struc;
  
  ## work out loops and covarence
  my $length = length $sec_struc ;
  #	  print "LEN: $length <P>";
  my $start = 0;
  my $count = 0;
  my $for_prev = 0;
  my $back_prev = 0;
  my %new_block_track;
  my (%for_storage, %back_storage, %store_sub_seq);
  my $new_block = 0;
  
  
  my %seq_colour;
  
  my %text_colour;
  
  my $font_color = 0;
  
  my @arr;
  my @txt_arr;
  my $first = 1;
  

  ## temp var
 # my $blee_for = 0;
 # my $blee_back = 0;

  my %arrows;
#print "SEQ: $seq\n";
  while($start <= $length) {
    my $sub = substr($sec_struc, $start, 1);
   
    my $txt_sub =  substr($seq, $start, 1);
 #   print "$txt_sub :$sub :: " if ( ($print) && ($sub =~ /[\>|\<]/)  );
    if ( ($sub eq "<") || ($sub eq "{") ||  ($sub eq "(") ||  ($sub eq "[") ){
  #    $blee_for++;
      $arrows{$start} = "for";
      $store_sub_seq{$start} = $txt_sub;
      $count++ if(!$back_prev);
      $for_storage{$count} = $start;
      #	print "$sub: $start :: count: $count :: stor: " .$for_storage{$count} .  " <BR>";
      
      if ( ($back_prev) || ($first) ) {
	$new_block_track{$count} = $count;
	#	  print "TRACK: " .$new_block_track{$count}  ." <BR>";
	$first = 0;
      }
      
      if ($back_prev) {
	$new_block++;
	$font_color = 0;
	# $new_block{$count} = $count;
	
      }
      $for_prev = 1;
      $back_prev = 0;
      
    }	elsif ( ($sub eq ">") || ($sub eq "}") ||  ($sub eq ")") ||  ($sub eq "]") ){
   #   $blee_back++;
      $count-- if (!$for_prev);
      $back_storage{$count} = $start;
       $arrows{$start} = "back";
      
      if ($for_prev) {
	#  $new_block = 1;
      }
      #	print "SUB: $sub \n";
      
      #		$arr[$for_storage{$count} ] =  "<font color=#" . $colours[$new_block] . "><B>></B></font>";
      $arr[$for_storage{$count}] =  "<b ID=\"" . $colours[$new_block] . "\">" .$arr[$for_storage{$count} ] . "</b>";

   #   print " arr: " .  $arr[$for_storage{$count}]. " IS: " . $txt_arr[$for_storage{$count}] . "  " if ($print);

      $seq_colour{$for_storage{$count}} = $colours[$new_block];
    #  print "SEQ COUNT > : " .$for_storage{$count} . " , < $start  eq : " .$seq_colour{$for_storage{$count}} . " \n";
   #   print "FOR SEQ: " .$store_sub_seq{$for_storage{$count}} . " -> $txt_sub \n";
      my $nuc_mat = check_nucs_match($store_sub_seq{$for_storage{$count}} . "~". $txt_sub);

      $seqs_ma{$start} = $nuc_mat;
      $seqs_ma{$for_storage{$count}} = $nuc_mat;
      
      $seq_colour{$start} = $colours[$new_block];
      
      #		$sub = "<font color=#" . $colours[$new_block] . "><b><</b></font>";
      $sub = "<b ID=\"" . $colours[$new_block] . "\"><</b>";
   #   print " sub: $sub IS : " . $colours[$new_block]. " \n" if ($print);
#      print "$sub :: COLOURS: " .$colours[$new_block]  . " :: NEW : $new_block \n";
      
      
      
      #	$txt_sub = "<font color=#" . $colours[$new_block] . "><b>$txt_sub</b></font>";
      $txt_sub = "<b ID=\"" . $colours[$new_block] . "\">$txt_sub</b>";
      
      #		$txt_arr[$for_storage{$count}] =  "<font color=#" . $colours[$new_block] . "><B>" . 	$txt_arr[$for_storage{$count}]. "</B></font>";
      $txt_arr[$for_storage{$count}] =  "<b ID=\"" . $colours[$new_block] . "\">" . 	$txt_arr[$for_storage{$count}]. "</b>";
      
      
      $font_color++;
      
      #	print "NEW: $sub <BR>";
      if (defined($new_block_track{$count})) {
	$new_block_track{$count} = undef;
	$new_block++;
	$font_color = 0;
      }
      $for_prev = 0;
      $back_prev = 1;
    } else {
      ## default
      $sub = "<b>$sub</b>";
      
    }
    
    
    push @arr, $sub;
    push @txt_arr, $txt_sub;
    
    $start++;
    
  }
  
  push @seqs_match, \%seqs_ma;
  
  my $tmp;
  foreach (@arr) {
    $tmp = $tmp . "$_";
    
  }
  $sec_struc = $tmp;
  
  my $tmp;
  foreach (@txt_arr) {
    if ($_ !~ /ID/) {
      #	       print "$_ :: ";
      $_ = "<b>$_</b>";
    }
    
    $tmp = $tmp . "$_";
  }
#  print "TMP: $tmp \n";
  $new_seq = $tmp;
  
#  print "FOR:  $blee_for  BACK: $blee_back \n\n";

#  my @blee = ("1", "2", "3");
  my @blocks;
  my @col_blocks;
  my $start = 0;
  my ($last_arrow, $last_col, $num_start, $num_end , $last_num);
  while ($start <= $length) {
  #foreach my $key (sort keys %arrows) {
    if (defined($arrows{$start}  ) ) {
      if ( ( $last_arrow ne $arrows{$start}) || ($last_col ne   $seq_colour{$start}  ) ) {

    #  if ($last_col ne   $seq_colour{$start}) {  
	
	push @blocks, $num_start . "~" . $last_num if ($num_start);
	push @col_blocks, $seq_colour{$start};
 
	$num_start = $start;

	$last_arrow = $arrows{$start};
	$last_col =  $seq_colour{$start};

      } else {
	$last_num = $start;
      }

#      print "key: $start :: arrow: " . $arrows{$start}. " :: colour: "  . $seq_colour{$start}. " \n";
      

    }
    $start++;
  }

  push @blocks, $num_start . "~" . $last_num if ($num_start);

#  foreach (@col_blocks) {
#    print "$_ \n";
#  }
#  foreach (sort keys %seq_colour) {
#    print "$_ -> " . $seq_colour{$_}. " \n";
#  }
  #print "num_start: $num_start :: last : $last_num \n\n";
  #print "BLOCKS: \n@blocks \n\n";
 # print "COL: \n@col_blocks\n";

# exit(0);
#print "NEW: $new_seq \n";
#print "blocks : @col_blocks \n";

  return $new_seq, \@blocks, \@col_blocks,  %seq_colour;
}


sub check_nucs_match {
  my($both_nuc) = @_;
 # print "BOTH: $both_nuc \n";
  my(%match);
  $match{'A~U'} = "match";
  $match{'U~A'} = "match";
  $match{'C~G'} = "match";
  $match{'G~C'} = "match";
  $match{'U~G'} = "match";
  $match{'G~U'} = "match";
  if (defined($match{$both_nuc})) {
    return "match";
  }

}
