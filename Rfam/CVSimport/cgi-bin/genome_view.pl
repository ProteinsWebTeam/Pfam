#! /usr/local/bin/perl -T

use strict;

use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';

use CGI;
use RfamWWWConfig;
use Tree;
use GetzTax;
#use GIFAlign;
#use Bio::Rfam::SeqRfam;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

my ( $q, $zoom, $family, $name, $print_name, $tree, @list, @ids, $galign );

$q = new CGI;
#$zoom = 0.5;
my $temp_count = $q->param("temp_count");
my $count = $1 if ($temp_count =~  /^([-\@~:\w.]+)$/);

my $temp = $q->param('auto_genome');
my $auto_genome= $1 if ($temp =~  /^([-\@~\w.\s]+)$/);
my $species;


my ($species) = &RfamWWWConfig::genome_species_name($auto_genome);
#$species =~ s/~/ /g;
#my($first_species) = split(/\s+/, $species);

print $q->header;
my (@buttons) = qw (all selected);
my $selected_button;
foreach (@buttons) {
  my $temp = $q->param($_ . ".x");
  my $blee = $1 if ($temp =~  /^([-\@~:\w.]+)$/);
  if($blee) {
    $selected_button = $_;
  }

}

my (%regs, @all_regions, %species_results);
if ($selected_button =~ /selected/) {
  my $temp_count = $q->param("max_count");

  my $max_count = $1 if ($temp_count =~  /^([-\@~:\w.]+)$/);
  my $count  = 0;
  while ($count <= $max_count)  {  
    my $temp = $q->param($count);
    my $blee = $1 if ($temp =~  /^([-\@~:\w.]+)$/);
    #  $ids{$blee} = $blee if ($blee);
    if ($blee) {
      $regs{$blee} = $blee;
    }
    $count++;
  }

  (@all_regions) = &RfamWWWConfig::genome_regions($auto_genome, %regs);

} elsif ($selected_button =~ /all/) {

  (@all_regions) = &RfamWWWConfig::genome_regions($auto_genome);
} else {
  #(%species_results) =   &RfamWWWConfig::genome_category($first_species);
  (%species_results) =   &RfamWWWConfig::genome_category($auto_genome, 1);

}


print &RfamWWWConfig::header("Genome distribution: Rfam families for species: $species");


#print "first: $first_species <P>";
#my(%species_results) =   &RfamWWWConfig::genome_category($first_species);


#print "BUTTON: $selected_button <P>";
if ($selected_button) {
 
 print qq ( <table><tr><td><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL accession</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Rfam family</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >Start</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >End</TD></TR>
	  );

  foreach (@all_regions) {
    my($rfamseq_acc, $rfam_id, $rfam_acc , $seq_start, $seq_end) = split(/~/, $_);
    
    my $link = $RfamWWWConfig::srsserver;
    $link =~ s/THEACC/$rfamseq_acc/;

    print "<TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left><A HREF=$link>$rfamseq_acc</A></TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left><A href=$RfamWWWConfig::getacc?$rfam_acc>$rfam_id</A></TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$seq_start</TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$seq_end</TD></TR>";
    
  }

 print "</TABLE><P>";

} else {
  
  
  print qq(<form method=post enctype=\"multipart/form-data\"  action=/cgi-bin/Rfam/genome_view.pl>
	   
	   <input type=hidden name=auto_genome value=$auto_genome>
	   <table><tr><td><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=	#000070 CLASS=whitetableheader>Rfam family</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Number of regions</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >View Regions</TD></TR>
	   
	   
	   
	   
	   
	  );
  
  foreach (sort keys %species_results) {
    #print "key: $_ , val: " . $species_results{$_}. " <BR>"

    my (@res) = @{$species_results{$_}};
    
    my (@blee);
    foreach (@res) {
      my ($rfam_id, $rfam_acc, $sum) = split(/~/, $_);
      my %tmp = ( 'acc' => $rfam_acc,
		  'id' => $rfam_id,
		  'sum' => $sum
		);
      push @blee, \%tmp;
    }
    @blee = sort { $a->{'sum'} <=> $b->{'sum'} } @blee;

    my @res = reverse @blee;

    #my (%results) = %{$species_results{$_}};
    my $count = 1;
    foreach (@res) {
      my (%results) = %{$_};
      my $id = $results{id};
      my $acc = $results{acc};
      print "<TR><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left><A href=getacc?$acc>$id</A></TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>" . $results{sum} . "</TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  ALIGN=CENTER   NOWRAP valign=center align=left><input type=checkbox name=$count value=$id></TD></TR>";
      $count++;
    }
    print "<input type=hidden name=max_count value=$count><input type=hidden name=auto_genome value=$auto_genome>\n";
  }
  
  
  
  print "</TABLE>";
  
  print "</TD></TR><TR><TR><TR><TR><TD align=right><input type=image SRC=$RfamWWWConfig::image_link/selected_regions.gif name=\"selected\" value=selcted border=0><input type=image SRC=$RfamWWWConfig::image_link/all_regions.gif name=\"all\" value=all border=0></TABLE>";

print "</form><P>";

}

print &RfamWWWConfig::footer();
&RfamWWWConfig::logs("GENOMEVIEW:$species");
#$galign -> view_all_figures( $zoom, 1, $acc);

#&RfamWWWConfig::logs("Species domain organisation in $name for family $family");

