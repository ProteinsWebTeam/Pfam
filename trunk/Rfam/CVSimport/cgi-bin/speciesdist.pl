#! /usr/local/bin/perl -T


use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use CGI;
use RfamWWWConfig;
use Tree;
use GetzTax;
$| = 1;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

my ( @list, $family, $depth, $tag, $notag, $prog_depth, $tree, $output );

$query = new CGI;

print $query->header;

my $temp  = $query->param('id');
$family =  $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp = $query->param('depth');
$depth = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp = $query->param('tag');
$tag = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp  = $query->param('acc');
my $acc =  $1 if ($temp =~  /^([-\@~\w.]+)$/);

$notag = 1 unless ( $tag );

#$swisslink = $PfamWWWConfig::swisslink;

if (not $depth or $depth =~ /all/ ) {
    $prog_depth = 100;
}
else {
    $prog_depth = $depth;
}

print &RfamWWWConfig::header("Species distribution for family $family ($depth levels)", $family, $acc);

#print "<HR NOSHADE SIZE=2><P>";
print <<END;
<HR NOSHADE SIZE=2>
<span class=normaltext><B>Member Sequences</B> - Click on the links to see the domain organisation of the proteins containing the $family domain.<BR>

<BR><B>Alignment</B> - Select the checkboxes then click 'View selected species alignment' . This wil
l generate a Coloured Alignment of proteins for ONLY the selected species  containing the $family do
main. Plain Pfam format and Stockholm format will be available soon.<P>
Values in brackets represent the number of proteins containing the domain in the respective families
.<P>
END

print "<form name=\"species_align\" enctype=\"multipart/form-data\" method=\"post\" action=\"species_alignment.pl\">";


GetzTax::_file_domain_species($acc, \@list);
##print "$PfamWWWConfig::getz -t \"[$PfamWWWConfig::srs_pfam-id:$family]>$PfamWWWConfig::srs_db\" | <P>";
#GetzTax::_rdb_domain_species($family, \@list);

#if (!@list) {
#  open (FH, "$RfamWWWConfig::getz -t \"[$RfamWWWConfig::srs_pfam-id:$family]>$RfamWWWConfig::srs_db\" |" );
#  GetzTax::get( \*FH, $family, \@list );
#}


$tree = Tree -> new();
$tree -> grow_tree ( \@list, ';' );
$tree = $tree -> clear_root();
$output = $tree -> to_text( $prog_depth, $tag );
my ($count);
($output, $count) = &replace_text_with_links ($output, $acc);
$output = &replace_ids_with_links ($output, $acc); 
print "<center><input type=submit value=\"View selected species alignment\">  <input type=Reset></center>";
print "<input type=hidden name=acc value=$acc>\n";
print "<input type=hidden name=family value=$family>\n";
print "<input type=hidden name=count value=$count>\n";

print "<br><pre>\n";
print "$output\n";
print "</pre>\n";
print "<center><input type=submit value=\"View selected species alignment\">  <input type=Reset></center>";

print &RfamWWWConfig::footer;
&RfamWWWConfig::logs("SPECIESDIST:$acc");

my $acc = $query->param('acc');
#&RfamWWWConfig::logs("Species distribution: $acc $family ($depth)");


sub replace_text_with_links {
    my $line = shift;
    my $acc = shift;
    my $count = 1;
	while ($line =~ /\#(\w.*)?\(/) {
	
		$cginame = $name = $1;
		$cginame =~ s/\s/%20/g;
		my $species_count = "species_" . $count;
		$line =~ s/\#$name/<a href=\"$RfamWWWConfig::speciesview?family=$family&acc=$acc&name=$cginame\">$name<\/a> <input type=checkbox name=$species_count value=$cginame>/;
		$count++;
	}

    return ($line, $count);
}

sub replace_ids_with_links {
    my $line = shift;
    my $acc = shift;

    while ($line =~ /\*(\S+)/) {
		
		$name = $1;
		eval("\$temp = \"$RfamWWWConfig::swisslink\"");
		$line =~ s/\*$name/<a href=\"$RfamWWWConfig::swisspfam?name=$name&acc=$acc\">$name<\/a>/;
    
	}

    return $line;
}






