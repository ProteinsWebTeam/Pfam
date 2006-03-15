package Bio::Pfam::Web::GetzTax;

use vars qw ( @ISA @EXPORT_OK );
use strict;
use Exporter;
@ISA = qw ( Exporter );
use Bio::Pfam::Web::PfamWWWConfig;

sub get {
    my ( $fh, $family, $list ) = @_;
    
    my ( $taxonomy, $ID, $OS, $OC );
    
    while ( <$fh> ) {                   	
	
	if( !( /^SQ\s/ ) ) {
	    if ( /^ID\s+(\w+)\s+/ ) {  
		$ID = $1;
	    } elsif ( /^OS\s+(.*)/ ) {
		$OS .= $1;
		$OS =~ s/[\.\,\;]$//;
	    } elsif ( /^OC\s+(.*)/ ) {
		$OC .= $1;
		chop $OC;
		$OC .=";";
	    }
	} else {
	  $list = _parse_output($OC, $OS, $ID, $family, $list);
	   $OC = $OS = $ID = "";
	  
	} 
    }
	close $fh;
    return $list;
}


sub _rdb_domain_species {
 my (  $family, $list , $ncbi_code) = @_;
 my (%ncbi_species, %ncbi_seqs);
 my @domain_all = &Bio::Pfam::Web::PfamWWWConfig::domain_species($family, $ncbi_code);
 foreach (@domain_all) {

   ##############################################
   #
   # Example:
   # 
   #############################################

   ### returns pfamseq_id , species, taxonomy:
   # T2BA_BACAM | Bacillus amyloliquefaciens. | Bacteria; Firmicutes; Bacillus/Clostridium group;Bacillus/Staphylococcus group; Bacillus. |
   # | Q44657     | Bacillus amyloliquefaciens. | Bacteria; Firmicutes; Bacillus/Clostridium group;Bacillus/Staphylococcus group; Bacillus. |
   # | Q9RM91     | Bacillus sp.                | Bacteria; Firmicutes; Bacillus/Clostridium group;Bacillus/Staphylococcus group; Bacillus. |
   
   my ($ID, $OS, $OC, $NCBI) = split(/~/, $_);
   $ncbi_species{$NCBI} = $OS;
   $ncbi_seqs{$NCBI} .= "~$ID";
   $list = _parse_output($OC, $NCBI, $ID, $family, $list);
 }
 return \%ncbi_species, \%ncbi_seqs,   $list;
}


sub _parse_output {

  my($OC, $OS, $ID, $family, $list, $NCBI) = @_;
  my $taxonomy;
 # print "OS: $OS <BR>";
  if ( !( $OC =~ /^VIRUSES/ ) ) {
    #print "MOM <P>";
    $OS =~ s/\S+\s+(\w+)\W?.*$/\1/;
    chop $OC;
    #print "<B>OS: $OS </B><BR>";
    $OC .= " $OS;";
    $OS = "";
  } else {
  #  print "EEP : $OS <P>";
    $OS .= ";";
  }
 # print "WOW ID: $ID, OC: $OC , OS: $OS  <BR>";
  
  $taxonomy = lc ";$OC$OS";
  $taxonomy =~ s/\(.*\)//g;
  $taxonomy =~ s/;\s+/;/g;
  $taxonomy =~ s/\s+;/;/g;
  $taxonomy =~ s/,/-/g;
  $taxonomy =~ s/- /-/g;
  if ( !( $taxonomy  =~ /^;viruses/ ) ) { 
 #   print "BOO <P>";
    while ( $taxonomy =~ /\;([a-z])/ ) {
      my $upper_case = uc $1;
      $taxonomy =~ s/\;([a-z])/\;$upper_case/;
    }
  } else {
    $taxonomy  =~ s/^;viruses/;Viruses/;
  }
  $taxonomy = $family.$taxonomy.$ID;
  push ( @$list, $taxonomy );
  $OC = $OS = $ID = "";
  
#print "TAX: $taxonomy <BR>";
  return $list;

}

