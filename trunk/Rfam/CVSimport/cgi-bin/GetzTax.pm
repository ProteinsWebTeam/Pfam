package GetzTax;

use vars qw ( @ISA @EXPORT_OK );
use strict;
use Exporter;

#use Bio::Index::Stockholm;

@ISA = qw ( Exporter );


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




sub genome_species {
 my (  $acc, $list , $species) = @_;


# my (@results) = @{$list};
 my(@results) = &RfamWWWConfig::genome_build($species);

 foreach (@results) {
   
   #print "$_ <BR>";
   my($ID, $OS, $OC) = split(/~/, $_);
   ##print "id: $ID, OS: $OS , OC: $OC <BR>";
   $list = _parse_output($OC, $OS, $ID, $acc, $list);
 }

 close(_SPECIES);
# print "LIST: $list :: VAL: %values <P>";
 return $list;

}



sub _file_domain_species {
 my (  $acc, $list ) = @_;

# print "HERE <P>";
# close(_TEMP);

# my $at = "getz -f org -f taxon \@/nfs/intweb/doctree/htdocs/Software/Rfam/temp/temp.dat |";
# print "AT: $at <BR>";
# open(_GETZ, "$at");

# while(<_GETZ>) {


#   print "$_ <BR>";

# }

#close(_GETZ);

# foreach my $key (sort keys %values) {

#   print "KEY: $key, val: " .$values{$key} . " <BR>";
# }

# print "WOW <P>";
 open(_SPECIES, "/nfs/WWW/htdocs/Software/Rfam/data/species/$acc.species") or print "CANNA OPEN: /nfs/WWW/htdocs/Software/Rfam/data/species/$acc.species AS $! <BR>";

 while(<_SPECIES>) {
   chop($_);
   
  # print "$_ <BR>";
   my($ID, $OS, $OC) = split(/~/, $_);
   $list = _parse_output($OC, $OS, $ID, $acc, $list);
 }

 close(_SPECIES);
# print "LIST: $list :: VAL: %values <P>";
 return $list;

}

sub _rdb_domain_species {
 my (  $family, $list ) = @_;

 my @domain_all = &RfamWWWConfig::domain_species($family);
 foreach (@domain_all) {
   my ($ID, $OS, $OC) = split(/~/, $_);
   
   $list = _parse_output($OC, $OS, $ID, $family, $list);
 }
 return $list;
}


sub _parse_output {

  my($OC, $OS, $ID, $family, $list) = @_;
  my $taxonomy;
  if ( !( $OC =~ /^VIRUSES/ ) ) {
    $OS =~ s/\S+\s+(\w+)\W?.*$/\1/;
    chop $OC;
    $OC .= " $OS;";
    $OS = "";
  } else {
    $OS .= ";";
  }
  
  $taxonomy = lc ";$OC$OS";
  $taxonomy =~ s/\(.*\)//g;
  $taxonomy =~ s/;\s+/;/g;
  $taxonomy =~ s/\s+;/;/g;
  $taxonomy =~ s/,/-/g;
  $taxonomy =~ s/- /-/g;
  if ( !( $taxonomy  =~ /^;viruses/ ) ) { 
    while ( $taxonomy =~ /\;([a-z])/ ) {
      my $upper_case = uc $1;
      $taxonomy =~ s/\;([a-z])/\;$upper_case/;
    }
  } else {
    $taxonomy  =~ s/^;viruses/;Viruses/;
  }
  $taxonomy = $family.$taxonomy.$ID;
  #print "TAX: $taxonomy <BR>";
  push ( @$list, $taxonomy );
  $OC = $OS = $ID = "";
  
  
  return $list;

}

