#!/usr/local/bin/perl 


use strict;

#use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use CGI;
use RfamWWWConfig;
#use Paging;

my (%db, $id, $acc, $des);

#$ENV{'PATH'} = '/bin:/usr/bin';
#my $path = $ENV{'PATH'};

my $pfamdb = $RfamWWWConfig::srs_pfam;

# mainly constants


my $q = new CGI;

print $q->header();

my $temp_terms = $q->param('terms');
my $terms = $1 if ($temp_terms =~  /^([-\@~\w\&\s.]+)$/);
#my @db = $q->param('db');

#foreach my $db (@db) {
#	if ($db =~  /^([-\@\w.]+)$/) {
#		$db = $1;
#	}	
#}

##pop the databases into a hash (everything called db
##here - a bit odd!

#if (not @db) {
#    &RfamWWWConfig::user_error("You must select at least one database to search!");
#}

#foreach my $db (@db) {
#    $db{$db} = 1;
#}

my $page_size = $q->param('size');
if ($page_size =~  /^([-\@\w.]+)$/) {
  $page_size = $1;
}

my $location  = $q->param('loc'); # for parasite sites.
if ($location =~  /^([-\@\w.]+)$/) {
  $location = $1;
}
#my $getacc    = $q->param('getacc');
#if ($getacc =~  /^([-\@\w.]+)$/) {
#  $getacc = $1;
#}

my $swissget    = "$RfamWWWConfig::swisspfam?name";
my $prositeget  = RfamWWWConfig::link_mapper()->{'PROSITE'}->[0]->{'link'};

if( !defined $location ) {
    $location = $RfamWWWConfig::cgibin; # for local site.
}

#if( !defined $getacc ) {
#    $getacc = "$location/getacc?";
#}


$terms !~ /\S/ && &RfamWWWConfig::user_error("You have not typed anything into the query box");
$terms =~ s/(\w)\s+(\w)/$1&$2/g;


#build a new paging module

#my $p = new Paging;
#$p->dir($RfamWWWConfig::tempdir);
#$p->chunk($page_size);
#$p->maxkbytes(200); #2 Mbytes in the cache system

#my $pagehead = &RfamWWWConfig::header("Results for Rfam text Search (cont..)");

print &RfamWWWConfig::header("Query Rfam text");
print ("<center><SPAN class=normallargetext>Results for query '<SPAN class=normalbluetext>$terms</SPAN>'</SPAN></center><p>");
#start by processing all of the query linked to Rfam

my $srsq = "";
my $touched = 0;
#foreach my $db ( keys %db ) {
#    if( $touched != 0 ) {
#	$srsq .= "|";
#    } else {
#	$touched = 1;
#    }

#    if( $db eq 'prosite') {
#	$srsq .= "([prositedoc-des:$terms]>$pfamdb)";
#    } elsif ( $db eq 'pfam' )  {
#      if ($pfamdb =~  /^([-\@\w.]+)$/) {
#	$pfamdb = $1;
#      }
      
#      if ($terms =~  /^([-\@\w.]+)$/) {
#	$terms = $1;
#      }
#	$srsq .= "([$pfamdb-alltext:$terms])";
#    } elsif ( $db eq 'swiss' )  {
#	$srsq .= "([swissprot-alltext:$terms]>$pfamdb)";
#    } else {
#	$srsq .= "([$db-alltext:$terms])";
#    }
#}

my @results = ();


### TEMP RESULTS GET FROM RDB AT THE MOMENT !!!! 
#print "HERE : $terms <P>";
my @db_results = &RfamWWWConfig::search_rfam($terms);
#print "BOO <P>";
my @results;
foreach (@db_results) {
  my ($acc, $id, $desc) = split(/~/, $_);
  push @results, {'name' => $id};
  $results[$#results]->{'acc'} = $acc;
  $results[$#results]->{'des'} .= $desc;

}
#print "DB: @db_results <P>";

#open(GETZ,"/usr/local/pubseq/bin/getz -f acc -f id -f des '$srsq' |") || die "Could not open Rfam SRS stream";

#while(<GETZ>) {
#    /^ID\s+(\S+)/ && do {
#	$id = $1;
#	push @results, {'name' => $id};
#	next;
#    };
#    /^AC\s+(\w+\d+)/ and do {
#	$acc  = $1;
#	$results[$#results]->{'acc'} = $acc;
#	next;
#    };
#    /^DE\s+(.*?)\s*$/ and do {
#	$des  = $1;
#	$results[$#results]->{'des'} .= $des;
#	next;
#    };
    
#}
 print("<center><SPAN CLASS=normaltext>Matches to documentation in the selected databases with links back to Rfam</SPAN></center><p>\n");
if (@results) {
   # $p->header("$pagehead<p><center><SPAN CLASS=normallargetext>NN Results for query '<SPAN class=normalbluetext>$terms</SPAN>'</SPAN></center><p><center><SPAN class=normaltext>Matches to documentation in the selected databases with links back to Rfam (cont...)</span></center><p><table border=1 cellpadding=5 cellspacing=0 align=center bgcolor=$RfamWWWConfig::rfamcolour><tr bgcolor=#000070><th class=whitetableheader>Family</th><th  class=whitetableheader>Description</th></tr>\n");
    #$p->footer("</table><P>[<a href=\\\"$RfamWWWConfig::image_temp/\$next_file\\\">Next page >></a>]");


    print("<table border=1 cellpadding=5 cellspacing=0 align=center bgcolor=$RfamWWWConfig::rfamcolour><tr bgcolor=#000070  ><th class=whitetableheader>Family</th><th  class=whitetableheader>Description</th></tr>\n");
    foreach my $res (@results) {
	if (not $res->{'des'}) {
	    $res->{'des'} = $res->{'acc'};
	}
	print("<tr><td > <a href=\"$RfamWWWConfig::getacc?".$res->{'acc'}."\">".$res->{'name'}."</a></td><td class=normaltext>".$res->{'des'}."</td></tr>");
	#$p->break;
    }
    print("</table>\n");
}
else {
    print("<center><span class=normallargetext>There were no Rfam families hit</center>");
}
#$p->print("<BR><hr noshade  color=#000070 size=2>\n<P>");
print("<P><P>");
print &RfamWWWConfig::footer() ;
&RfamWWWConfig::logs("SEARCH:$terms");
#$p->break;



##ok - if there is pfam - process it separately.

#if( exists $db{'pfam'} ) {
#    @results = ();
#    open(GETZ,"/usr/local/pubseq/bin/getz -f  acc -f id -f des '[$pfamdb-alltext:$terms]' |") || die "Could not open Rfam all text stream";
#    while(<GETZ>) {
#	/^ID\s+(\S+)/ && do {
#	    $id = $1;
#	    push @results, {'name' => $id};
#	    next;
#	};
#	/^AC\s+(\w+\d+)/ and do {
#	    $acc  = $1;
#	    $results[$#results]->{'acc'} = $acc;
#	    next;
#	};
#	/^DE\s+(.*?)\s*$/ and do {
#	    $des  = $1;
#	    $results[$#results]->{'des'} .= $des;
#	    next;
#	};
#    }
#    $p->print("<center><SPAN class=normallargetext>Matches to Rfam documentation</SPAN></center><p>");
#    if (@results) {
#	$p->header("$pagehead<p><center><SPAN class=normallargetext>Results for query '<SPAN class=normalbluetext>$terms</SPAN>'</SPAN></center><p><center><SPAN class=normaltext>Matches to Rfam documentation (cont...)</SPAN></center><p><table border=1 cellpadding=5 cellspacing=0  align=center $RfamWWWConfig::pfamcolour><tr bgcolor=#000070 CLASS=whitetableheader><th CLASS=whitetableheader>Family</th><th CLASS=whitetableheader>Description</th></tr>\n");
#	$p->print("<table border=1 cellpadding=5 cellspacing=0  align=center bgcolor=$RfamWWWConfig::pfamcolour><tr bgcolor=#000070><th CLASS=whitetableheader>Name</th><th CLASS=whitetableheader>Description</th></tr>\n");
	
#	foreach my $res (@results) {
#	    $p->print("<tr><td> <a href=\"$getacc".$res->{'acc'}."\">".$res->{'name'}."</a></td><td CLASS=normaltext>".$res->{'des'}."</td></tr>");
#	    $p->break;
#	}
#	$p->print("</table>");
#    }
#    else {
#	$p->print("<center><span class=normaltext>There were no hits to Rfam documentation</span></center>");
#    }
#    $p->print("<BR><hr noshade color=#000070  size=2>\n<P>");
#    $p->break;
#}


## Now for Prosite

#if( exists $db{'prosite'} ) {
#    my (%prositehash);
#    @results = ();

#    open(GETZ,"/usr/local/pubseq/bin/getz -e '[prositedoc-des:$terms]' |") || die "Could not open Rfam all text stream";
#    while(<GETZ>) {
#	/^{(PDOC\d+)}/ && do {
#	    $id = $1;
	    
#	    #make sure we haven't already processed this

#	    if( $prositehash{$id} == 1 ) {
#		next;
#	    } else {
#		$prositehash{$id} = 1;
#	    }

#	    while(<GETZ>) {
#		/^\*\s+(.*)\s+\*/ && do {
#		    $des = $1;
#		    last;
#		};
#	    }

#	    my $temp;
#	    eval("\$temp = \"$prositeget\";");

#	    push @results, { 'name' => $id,
#			     'des' => $des,
#			     'link' => $temp };

#	};
#    }

#    $p->print("<center><span class=normallargetext>Matches Prosite documentation</span></center><p>");
#    if (@results) {
#	$p->header("$pagehead<p><center><h3>Results for query <SPAN class=normalbluetext>'$terms</SPAN>'</h3></center><p><center><SPAN class=normaltext>Matches to Prosite documentation (cont...)</SPAN></center><p><table border=1 cellpadding=5 cellspacing=0  align=center bgcolor=$RfamWWWConfig::pfamcolour><tr bgcolor=#000070><th CLASS=whitetableheader>Prosite</th><th CLASS=whitetableheader >Description</th></tr>\n");
#	$p->print("<table border=1 cellpadding=5 cellspacing=0  align=center bgcolor=$RfamWWWConfig::pfamcolour><tr bgcolor=#000070><th class=whitetableheader>Prosite</th><th class=whitetableheader>Description</th></tr>\n");
#	foreach my $res (@results) {
#	    $p->print("<tr><td> <a href=\"".$res->{'link'}."\">".$res->{'name'}."</a></td><td CLASS=normaltext>".$res->{'des'}."</td></tr>");
#	    $p->break;
#	}
#	$p->print("</table>");
#    }
#    else {
#	$p->print("<center><span class=normaltext>There were no matches to Prosite documentation</span></center>");
#    }
#    $p->print("<BR><hr color=#000070  noshade size=2>\n<P>");

#    $p->break;
#}


## And finally, swissprot

#if( exists $db{'swiss'} ) {
#    @results = ();
#    open(GETZ,"/usr/local/pubseq/bin/getz -f  acc -f id -f des '[pfamseq-alltext:$terms]' |") || die "Could not open Rfam all text stream";
#    while(<GETZ>) {
#	/^ID\s+(\S+)/ and do {
#	    $id = $1;
#	    push @results, {'name' => $id};
#	    next;
#	};
#	/^AC\s+(\w+\d+)/ and do {
#	    $acc = $1;
#	    $results[$#results]->{'acc'} = $acc;
#	    next;
#	};
#	/^DE\s+(.*?)\s*$/ and do {
#	    $des = $1;
#	    $results[$#results]->{'des'} .= $des;
#	};
#    }


#    $p->print("<center><span class=normallargetext>Matches to SWISS-PROT/TrEMBL documentation</span></center><p>");
#    if (@results) { 
#	$p->header("$pagehead<p><center><h3>Results for query '<SPAN class=normalbluetext>$terms</SPAN>'</h3></center><p><center><SPAN class=normaltext>Matches to SWISS-PROT/TrEMBL documentation (cont...)</SPAN></center><p><table border=1 cellpadding=5 cellspacing=0  align=center bgcolor=$RfamWWWConfig::pfamcolour><tr bgcolor=#000070 ><th CLASS=whitetableheader>Protein</th><th CLASS=whitetableheader>Description</th></tr>\n");
#	$p->print("<table border=1 cellpadding=5 cellspacing=0 align=center bgcolor=$RfamWWWConfig::pfamcolour><tr bgcolor=#000070 ><th CLASS=whitetableheader>Protein</th><th  CLASS=whitetableheader >Description</th></tr>\n");    
#	foreach my $res (@results) {
#	    $p->print("<tr><td> <a href=\"$swissget=".$res->{'name'}."\">".$res->{'name'}."</a></td><td class=normaltext>".$res->{'des'}."</td></tr>");
#	    $p->break;
#	}
#	$p->print("</table>");
#    }
#    else {
#	$p->print("<center><span class=normaltext>There were no matches to SWISS-PROT entries</span></center>");
#    }
#  #  $p->print("<hr noshade size=2 color=#000070  ><p>");
#    $p->break;
#}

#$p->print("<P><span class=normaltext>End of query results</span><p>");




	
	
	

