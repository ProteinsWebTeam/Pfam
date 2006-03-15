package Bio::Pfam::Web::SwissCol;

use strict;
use Bio::Index::Stockholm;
use Bio::Pfam::Web::PfamWWWConfig;

######
#
# Generates the coloured mark-up from jalview
#
##

sub coloured_swisspfam {
  my ($acc, $temp_pfamseq_ids) = @_;
  
  my %pfamseq_ids = %{$temp_pfamseq_ids};  
  my $align = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$acc" . "_species_" . $$;
  open(_ALIGN, ">$align") || die "Could not open align: $align";
  my ($file);

  ### If pfam-B or pfam-A
  if ($acc =~ /^PB/) {
    
    $file = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$acc" . "_index";
    if (-e "$file") {
    } else {
      ## Retrieve the pfamb data
      get_pfamb_index($acc, $file);
    }

    open(_FILE, "$file ") or print STDERR "EEP CANNA OPEN $file AS $! <P>";
  } else {
    $file = "$Bio::Pfam::Web::PfamWWWConfig::data_root/full/$acc" . ".full.gz";
    open(_FILE, "gunzip -c $file |") or print STDERR  "Cant open pfamA file: $file as $! <P>";
  }
  
  while(<_FILE>) {
    if ($_ =~ /^\#=GS\s+(\S+)\/\d+\-\d+/) {
      
      if (defined($pfamseq_ids{$1})) {
	print _ALIGN  "$_";
      }
    } else {
      if ($_ =~ /^(\S+)\/\d+\-\d+/) {
	if (defined($pfamseq_ids{$1})) {
	  print _ALIGN  "$_";
	}
      }
      
  }
    
    
  }
  
  close(_FILE);
  close(_ALIGN);
	

  &generate_jtml_file_swiss($acc, $align);

}


sub generate_jtml_file_swiss {
    my ($acc, $align) = @_;
    #my  $outfile = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$acc" . "_species.tmp";
    my ($write, $www_write);
    #if (! -e $outfile) {
    
	#my $command = "$Bio::Pfam::Web::PfamWWWConfig::file_root/bin/jalview/pfam_jtml $align 80 > $outfile";
	#system($command);
	
	$write = "$Bio::Pfam::Web::PfamWWWConfig::tempdir/$acc" . "_species_" . $$ . ".shtml" if (!$write);
	$www_write = "$Bio::Pfam::Web::PfamWWWConfig::tempwww/$acc" . "_species_" . $$ . ".shtml" if (!$www_write);
	
	open(_READ, "$Bio::Pfam::Web::PfamWWWConfig::file_root/bin/pfam_jtml $align 80 |");
	open(_JTML, ">$write");
	
	while(<_READ>) {
	    my $line = $_;
	    
	    if ($_ =~ /(HREF=\"Jtml\.css\")/) {
		#   my $jtml_tag;
		my $tag = $1;
		my $jtml_tag = "HREF=\"$Bio::Pfam::Web::PfamWWWConfig::WWW_root/Jtml.css\" ";
		$_ =~ s/$tag/$jtml_tag/;
		
	    }
	    
	    
	    my ($id, $junk) = split(/&/, $_);
	    if ($id =~ /<b>(.*)/) {
		my $all = $1;
		
		my $swiss;
		my $residues;
		
		if ($all =~ /\//) {
		    ($swiss, $residues) = split(/\//, $all);
		} else {
		    $swiss = $all;
		}
		
		my $new_string = "<NOBR><A HREF=/cgi-bin/Pfam/swisspfamget.pl?name=$swiss>$all</A></NOBR>";
		$line =~ s/$all/$new_string/;
		print _JTML $line;
		
	    } else {
		print _JTML $_;
	    }
	    
	}
	
	close(_JTML);
	close(_READ);
	
	
    #}
    print STDERR "FILE: $www_write\n";
    print CGI->redirect("$www_write");
    exit(0);
}

sub get_pfamb_index {

  my($pfamB_acc, $file) = @_;
 
  my $Index_File_Name = "$Bio::Pfam::Web::PfamWWWConfig::data_root/Pfam-B.index";
  my $inx = Bio::Index::Stockholm->new($Index_File_Name);

  
  my $bio_index = $inx->fetch($pfamB_acc);
  
  open(_BIO, ">$file");
  
  while(<$bio_index>) {
    /^\/\// && last;
    print _BIO $_;
  }
  close(_BIO);
  
  
  
  
}



1;
