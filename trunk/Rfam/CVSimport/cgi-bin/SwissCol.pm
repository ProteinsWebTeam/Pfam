#! /usr/local/bin/perl

use strict;

######
#
# Generates the coloured mark-up from jalview
#
##

sub coloured_rfam {
  my ($acc, $temp_rfamseq_ids) = @_;
  
  my %rfamseq_ids = %{$temp_rfamseq_ids};
  
  my $dol = "$$";
  
  my $align = "$RfamWWWConfig::tempdir/$acc" . "_species_" . $dol;
  open(_ALIGN, ">$align");
  my ($file);
#print "Content-type: text/html\n\n"; print "HERE <P>";
  ### If rfam-B or rfam-A

  $file = "$RfamWWWConfig::data_root/full/$acc" . ".full.gz";
  open(_FILE, "gunzip -c $file |") or print  "BOO CANNA OPEN $file AS $! <P>";
  
  
  while(<_FILE>) {
    #print "$_ <BR>";
    if ($_ =~ /^\#=GC\s+SS_cons/) {
      
     # if (defined($rfamseq_ids{$1})) {
	print _ALIGN  "$_\n";
     # }
    } else {
      if ($_ =~ /^(\S+)\.\d+\/\d+\-\d+/) {
	if (defined($rfamseq_ids{$1})) {
	  print _ALIGN  "$_";
	}
      }
      
    }
    
    
  }
  
  close(_FILE);
  close(_ALIGN);
	

  generate_jtml_file_swiss($acc, $align, $acc. "_species_" . $dol);
  return $align . ".1" . ".gz";

}


sub generate_jtml_file_swiss {

  my ($acc, $align, $file) = @_;
  # print "ACCL $acc :: ALIGN: $align<P>";
 my  $outfile = "$RfamWWWConfig::tempdir/$acc" . "_species.tmp";
  
#  if (! -e $write) {
    
    
    
    
    my $dol = "$$";
 
   my $command = "/nfs/WWW/cgi-bin/Rfam/bin/new_parse_rfam.pl --input_dir /nfs/WWW/SANGER_docs/htdocs/Software/Rfam/temp/ --output_dir  /nfs/WWW/SANGER_docs/htdocs/Software/Rfam/temp --file_type full --ss_cons_only --family $acc --web_file $align ";
# print "COMMAND : $command <P>";
    #my $command = "$RfamWWWConfig::file_root/bin/jalview/rfam_jtml $align 80 > $outfile";
    my $result;
    eval {
      $result =  system($command);
    };
    if ($@) {
      print "EEP <P>";
    }
    my $gunzip = "gunzip $align" . ".*";
    system($command);
   # system($gunzip);
 # system("chmod 777 $align");
  #  print "COMMAND :$command , $gunzip, RES: $result<P>";
  #  open(_COM, "$command |") or print "WOW <P>";
  #  while(<_COM>) {

 #     print "EEP BPP <P>";
  #  }
   # close(_COM) or print "BAA  BOP $command as $! <P>";
#    $write = "$RfamWWWConfig::tempdir/$acc" . "_species_" . $dol . ".shtml" if (!$write);
#    $www_write = "$RfamWWWConfig::tempwww/$acc" . "_species_" . $dol . ".shtml" if (!$www_write);
    
#    open(_READ, "$outfile");
#    open(_JTML, ">$write");
    
#    while(<_READ>) {
#      my $line = $_;
      
#      if ($_ =~ /(HREF=\"Jtml\.css\")/) {
#	#   my $jtml_tag;
#	my $tag = $1;
#	my $jtml_tag = "HREF=\"$RfamWWWConfig::WWW_root/Jtml.css\" ";
#	$_ =~ s/$tag/$jtml_tag/;
	
#      }
      
      
#      my ($id, $junk) = split(/&/, $_);
#      if ($id =~ /<b>(.*)/) {
#	my $all = $1;
	
#	my $swiss;
#	my $residues;
	
#	if ($all =~ /\//) {
#	  ($swiss, $residues) = split(/\//, $all);
#	} else {
#	  $swiss = $all;
#	}
	
#	my $new_string = "<NOBR><A HREF=/cgi-bin/Rfam/swissrfamget.pl?name=$swiss>$all</A></NOBR>";
#	$line =~ s/$all/$new_string/;
#	print _JTML $line;
	
#      } else {
#	print _JTML $_;
#      }
      
#    }
    
#    close(_JTML);
#    close(_READ);


#  }
  my $www_write = "http://www.sanger.ac.uk/Software/Rfam/temp/" . $file . ".1";

#print CGI->redirect("$www_write");
#exit(0);






}

1;
