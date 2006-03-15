
#
# Perl Module for GIFAlign
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package Bio::Pfam::Web::GIFAlign;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

use Bio::Pfam::Web::PfamWWWConfig;
#use Bio::Pfam::Web::Paging;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::Pfam::Drawing::Image::Image; 
use Sanger::Graphics::ColourMap;
#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' , 'Bio::Pfam::AlignPfam' );


sub view_all_figures {
  
  my ($self, $zoom_factor, $verbose, $acc,  $dom_name, $all_params, $temp_new_select_order, $no_key) = @_;
  my ($disname,$seq,$name, $annseq);
  my %new_select_order;
  %new_select_order = %{$temp_new_select_order} if ($temp_new_select_order);
  my $entry;
  my $number;    
#  print "ALL: $all_params <P>";
  my ($p,$h,$f,$famname);
  
  # make a new paging module
  
  my $max_key_count = 5;
  
  $famname = $self->id();
#  $p = new Bio::Pfam::Web::Paging;
#  $p->dir($Bio::Pfam::Web::PfamWWWConfig::tempdir);
  $h = &Bio::Pfam::Web::PfamWWWConfig::header("$famname proteins");
  $f = &Bio::Pfam::Web::PfamWWWConfig::footer();
  
#  #$p->header("$h <p>All proteins for $famname (cont...)");
#  $p->header("<p>All proteins for $famname (cont...)");
#  $p->footer("<hr>Next page <a href=\\\"$Bio::Pfam::Web::PfamWWWConfig::image_temp/\$next_file\\\">here</a><p>\Q$f\E");
  
  $self->set_displayname_count(); # Count to avoid multiple displays


  #print STDERR localtime() . " debug: GIFAlign.pm: Starting to get stuff for drawing\n";

  #start the drawing code
  my %seqs;
  foreach $seq ( $self->each_seq() ) {
      $seqs{$seq->id()}++;
  }
  my @pfamseq_names=keys(%seqs);
  my $db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
  my @entries = $db->get_Seq_pfamseq(\@pfamseq_names);
  my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  $layout->layout_sequences(@entries);
  my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
  $imageset->create_images($layout->layout_to_XMLDOM);
  foreach my $im ($imageset->each_image){
      $im->print_image;
  }
  my $domain_order = $layout->region_order;
  
  #print STDERR localtime() . " debug: GIFAlign.pm: Finished drawing\n";
  
  my $swiss_link = &Bio::Pfam::Web::PfamWWWConfig::link_mapper()->{'SWISSPROT'}->[0]->{'link'};
    
   
    
  $layout->printDomainKey();
  #Then the bit about overlaps......But this does not work
  #print "<span class=normaltext>This family may contain <B>overlapping domains</b>, to change the graphical view click <A href=#over_align>here</A></span><p>"; 
	
  
  #print STDERR localtime() . " debug: GIFAlign.pm: printed Domain Key\n";

  print "<table>";
  
  my $swiss_link = &Bio::Pfam::Web::PfamWWWConfig::link_mapper()->{'SWISSPROT'}->[0]->{'link'};
  foreach my $image ($imageset->each_image){
      my @acc = split(/\./, $image->image_name);
      my $acc = $acc[0];
      my $link;
      eval("\$link = \"$swiss_link\";");
      print "<tr><td><a href=$link>$acc</font></a><span class=normaltext>".$image->image_info."</span><p class=gfxspace>";
      print "<img src=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/".$image->file_location." usemap=\#".$image->image_name." border=0><NOBR><span class=normaltext>[".$image->length/$image->scale_x." residues]</span>";
      print "<map name =".$image->image_name.">";
      print $image->image_map;
      print "</map></td></tr>";
      #print "<tr><td>".$image->image_name."</td></tr>";
  }
  print "</table>";
  #print STDERR localtime() . " debug: GIFAlign.pm: printed table \n";
}



1;  # says use was ok_

=head1 NAME

GIFAlign

=head1 DESCRIPTION

Description for B<GIFAlign>

GIFAlign is a very simple subclass of Align which allows the integration
of swiss2gif code (this is for the family view in Pfam web site). 

The philosophy of this subclassing is to allow Align to be still loosely
coupled to the web site whereas GIFAlign is tightly coupled to swiss2gif
(and swiss2gif is tightly coupled to PfamWWWConfig.pm ... )


=head1 AUTHOR

B<Ewan Birney> Email birney@sanger.ac.uk

=over
