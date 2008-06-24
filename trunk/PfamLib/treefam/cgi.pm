package treefam::cgi;

use strict;
use warnings;

use Exporter;
use CGI qw/:standard/;
use treefam::db;
use treefam::generic qw(last_time gopen detaint);
use treefam::nhx_plot;
use treefam::align_plot;
use treefam::ensembl;

use SangerWeb;
use vars qw(@ISA @EXPORT $dbhost $dbport $dbuser $dbpass $dbname);

@ISA    = qw(Exporter);
@EXPORT = qw();

$dbhost = "localhost"; $dbport = 3306; $dbuser = 'anonymous'; $dbpass = ''; $dbname = 'treefam';
#$dbhost = "vegasrv"; $dbport = 3308; $dbuser = 'anonymous'; $dbpass = ''; $dbname = 'treefam_2';

sub new {
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = { '-tmp'=>'../data/tmp', '-exe'=>'../bin-offline', @_ };
	$self->{'_host'} = "http://" . ($ENV{'HTTP_X_FORWARDED_HOST'} || $ENV{'HTTP_HOST'} || $ENV{'SERVER_NAME'} || "");
	$_ = detaint($ENV{DOCUMENT_ROOT}, '/.*');
	s/\/[^\/]+(\/?)$//;
	$self->{'_root'} = $_;
	$self->{'_sw'} = SangerWeb->new();
	$self->{'_db'} = treefam::db->new(-host=>$dbhost, -name=>$dbname, -port=>$dbport, -user=>$dbuser,
			-passwd=>$dbpass, -tmp=>$self->{-tmp});
	$ENV{'PATH'} = "$self->{_root}/bin-offline:/bin:/usr/bin";
	bless($self, $class);
	$self->cgi_parse;
	return $self;
}
sub root { my $self = shift; return $self->{_root}; }
sub host { my $self = shift; return $self->{_host}; }
sub sw { my $self = shift; return $self->{_sw}; }
sub db { my $self = shift; return $self->{_db}; }
sub dbh { my $self = shift; return $self->db->dbh; }
sub cgi_parse {
	my ($self) = @_;
	my $q = $self->sw->cgi;
	for my $t ($q->param) {
		$self->{$t} = $q->param($t);
	}
	for my $t ($q->keywords) {
		$self->{$t} = "";
	}
	# Well, a small but useful trick :-)
	$self->{ac} = $self->db->get_1ac($self->{ac}) if ($self->{ac});
	# Well, another even more useful trick :-)
	if (!$self->{ac} && $self->{extac} && $self->{spec}) {
		my $ens = treefam::ensembl->new;
		my @rst;
		$ens->fetch_ext($self->{extac}, uc($self->{spec}), \@rst);
		if (@rst && @{$rst[0]{gene}}) {
			$self->{ac} = $self->db->get_1ac($rst[0]{gene}[0]);
		}
	}
}
sub locate_cgi_dir
{
	my $self = shift;
	$_ = detaint($ENV{SCRIPT_NAME}, '/.*');
	s/\/[^\/]+$//;
	return $_;
}
#############
# print title
#
sub print_tf_header
{
	my ($self, $disp) = @_;
	my $cgi_bin = $self->locate_cgi_dir;
	print header('text/html');
	print <<EOF;
<html><head><title>$disp</title>
<link rel="stylesheet" type="text/css" href="/treefam.css" />
</head><body>
EOF
print <<EOF;
<table border=0 width="100%" cellspacing=0 cellpadding=6>
<font face="verdana, arial, sans-serif">
<tr bgcolor='#FFFFFF' style='color:002000'>
	<td width="20%">
		<a href="$cgi_bin/misc_page.pl"><img src="/images/logo1.png" height="60" border="0" alt="TreeFam"></a>
		<br><b><font size="3">$disp</font></b><br>
	</td>
	<td width="60%" align='center'>
			<font size="5"><b><font color="green">Tree fam</font>ilies database</b></font></td>
	<td width="15%">
	<a href="http://www.sanger.ac.uk/">
		<img src="/images/sanger_logo.gif" border="0" width="137" height="40" alt="The Sanger Institute">
	</a></td>
	<td width="5%">
	<a href="http://www.genomics.org.cn/bgi_new/english/platforms/bioinfo.htm">
		<img src="/images/bgi_logo.gif" border="0" height="40" alt="Beijing Genomics Institute">
	</a></td>
</tr>
</table>
<table border=0 cellspacing=0 cellpadding=3>
<tr bgcolor='darkgreen' align=left>
	<td><a href="$cgi_bin/misc_page.pl?home"><font color='#FFFFFF' size=1><b>[Home]</b></font></a>
	<td><a href="$cgi_bin/search2.pl"><font color='#FFFFFF' size=1><b>[Search]</b></font></a>
	<td><a href="$cgi_bin/TFinfo.pl?all"><font color='#FFFFFF' size=1><b>[Browse]</b></font></a>
	<td><a href="$cgi_bin/misc_page.pl?download"><font color='#FFFFFF' size=1><b>[Download]</b></font></a>
	<td><a href="$cgi_bin/misc_page.pl?faq"><font color='#FFFFFF' size=1><b>[FAQ]</b></font></a>
	<td  width='100%'></td>
	<td><a href="http://platform.humgen.au.dk:8080/"><font color='#FFFFFF' size=1><b>[TreeFam-1]</b></font></a>
</tr>
</font>
</table>
EOF
}
##############
# print footer
#
sub print_tf_footer {
	my $self = shift;
	my $lastmod = '';
	if (defined $ENV{"LAST_MODIFIED"}) {
		$lastmod = qq(Last Modified $ENV{"LAST_MODIFIED"});
	} elsif (defined $ENV{"SCRIPT_FILENAME"}) {
		$ENV{"SCRIPT_FILENAME"} =~ /^(.*)$/;
		my $filename    = $1;
		my ($mtime)     = (stat($filename))[9];
		my $scriptstamp = localtime($mtime);
		$lastmod        = qq(Last Modified $scriptstamp) if(defined $scriptstamp);
	}
	print qq(<table border=0 cellspacing=0 cellpadding=3 width="100%">
		<tr bgcolor='darkgreen'>
			<td width="50%" align="left"><font color='white' size=1>$lastmod</font></td>
			<td width="50%" align="right">
				<a href="mailto:treefam\@sanger.ac.uk"><font color='white' size=1>treefam\@sanger.ac.uk</font></a></td>
		</tr>\n);
	print qq(</body></html>\n);
}

sub CgiErr {
	my ( $self, $msg ) = @_;
	$self->print_tf_header("ERROR");
	$msg = "TreeFam ERROR: $msg";
	print "<h3>$msg</h3>\n";
	$self->print_tf_footer();
}
sub CgiDebug {
	my ( $self, $msg ) = @_;
	$self->print_tf_header("DEBUG");
	print "<h1>$msg</h1>\n";
	$self->print_tf_footer();
}

sub print_png_js {
	my ($self) = @_;
	print <<EOF;
<script language="JavaScript">
function openPNG(file, count)
{
	height = 14 * count + 30 + 20 + 60;
	win = open(file,'_blank', "width=670,height=" + height + ",status=no,toolbar=no,scrollbars=yes,menubar=no,resizable=yes");
}
</script>
EOF
}
sub print_open_js
{
	my $self = shift;
	print <<EOF;
<script language="JavaScript">
function openWin(url, title)
{
	window.open(url, title, "width=650,height=485,status=no,toolbar=no,scrollbars=yes,menubar=no,resizable=yes");
}
</script>
EOF
}
#########################################
# print a ATV-related JavaScript function
#
sub print_atv {
	my ($self, $use_japplet) = @_;
	print <<EOF;
<script language="JavaScript">
// This function is derived from http://www.ebi.ac.uk/goldman-srv/pandit/pandit.cgi
function openATV(u) {
	atv_window = open("", "atv_window", "width=280,height=135,status=no,toolbar=no,menubar=no,resizable=no");
	// open document for further output
	atv_window.document.open();
	// create document
	atv_window.document.write( "<HTML><HEAD><TITLE>ATV</TITLE></HEAD>" );
	atv_window.document.write( "<BODY TEXT=\\"#FFFFFF\\" BGCOLOR=\\"#000000\\">" );
	atv_window.document.write( "<FONT FACE=\\"HELVETICA,ARIAL\\">" );
	atv_window.document.write( "<CENTER><B>" );
	atv_window.document.write( "Please do not close this window<BR>as long as you want to use ATV." );
EOF
	if ( !$use_japplet ) {
		print <<EOF;
	atv_window.document.write( "<APPLET ARCHIVE=\\"eATVapplet.jar\\" " );
	atv_window.document.write( " CODE=\\"forester.atv_awt.ATVapplet.class\\" " );
EOF
	}
	else {
		print <<EOF;
	atv_window.document.write( "<APPLET ARCHIVE=\\"eATVjapplet.jar\\" " );
	atv_window.document.write( " CODE=\\"forester.atv.ATVjapplet.class\\" " );
EOF
	}
	print <<EOF;
	atv_window.document.write( " CODEBASE=\\"$self->{_host}/\\" " );
	atv_window.document.write( " WIDTH=200 HEIGHT=50> " );
	atv_window.document.write( "<PARAM NAME=url_of_tree_to_load VALUE=\\"$self->{_host}" + u + "\\">");
	atv_window.document.write( "</APPLET>" );
	atv_window.document.write( "</BODY></HTML>" );
	// close the document - (not the window!)
	atv_window.document.close();
}
</script>
EOF
}
####################
# print descriptions
#
sub print_desc {    # update
	my ($self, $des) = @_;
	my ( $x, %t, @a );
	my $ref = [
		{ 'AC'          => 'Accession' },
		{ 'SYMBOL'      => 'Symbol' },
		{ 'NAME'        => 'Family Name' },
		{ 'SYNONYM'		=> 'Synonyms' },
		{ 'COUNT'		=> 'Family Size'},
		{ 'NEW_AC'		=> 'New Accessions'},
		{ 'OLD_AC'		=> 'Old Accessions'},
		{ 'CLUSTER'     => 'Clustered' },
		{ 'AUTHOR'      => 'Curator' },
		{ 'DESC'        => 'Descriptions' },
		{ 'PHIGS'       => 'PhIGs Cluster ID' },
		{ 'MODIFIED'    => 'Last Modified' },
		{ 'METHOD'      => 'Methods' },
		{ 'COMMENT'     => 'Comment' },
	];

	my %desc;
	my $FAMPAGE = qq(TFinfo.pl);
	foreach my $x (keys %$des) {
		$desc{$x} = $des->{$x};
	}
	$desc{'DESC'} = '' unless(defined($desc{'DESC'}));
	print qq(<table border="1" style="text-align:left;" cellspacing="0" cellpadding="0" width="750" class="main">\n);

	# pre-process some data
	my $ac = $desc{AC};
	$desc{'COUNT'} = ($ac =~ /^TF1/)? "seed: $desc{N_SEED} / full: $desc{N_FULL}" : "full: $desc{N_FULL}";
	$desc{"COMMENT"} ||= "";
	$desc{"COMMENT"} =~ s/\s+/ /smg;
	$desc{"NEW_AC"} = join("; ", map{qq(<a href="$FAMPAGE?ac=$_" title="This family was curated into $_.">$_</a>);} split(";",($desc{"NEW_AC"}||"")))
	  		if (defined($desc{"NEW_AC"}));
	$desc{"OLD_AC"} = join("; ", map{qq(<a href="$FAMPAGE?ac=$_" title="This family was curated from $_.">$_</a>);} split(";", ($desc{"OLD_AC"}||"" )))
	  		if (defined($desc{"OLD_AC"}));
	$desc{"CLUSTER"} = join("; ", map{qq(<a href="$FAMPAGE?ac=$_" title="related families">$_</a>);} split(";",($desc{"CLUSTER"}||"")))
	  		if (defined($desc{"CLUSTER"}));
	$desc{'PHIGS'} = qq(<a href="http://phigs.jgi-psf.org/cgi-bin/psdCluster2.pl?database=PhIGs2&host=durant&clusterId=$desc{PHIGS}" title="open PhIGs cluster viewer in a new window" target="_blank">$desc{PHIGS}</a>) if (defined($desc{PHIGS}));
	if ($ac =~ /^TF1/) {
		$desc{'AC'} = qq(<a href="javascript:void(0);" title="TreeFam-A (curated seed tree, automatically generated full tree)">$desc{AC}</a>);
	} else {
		$desc{'AC'} = qq(<a href="javascript:void(0);" title="TreeFam-B (automatically generated seed and full trees)">$desc{AC}</a>);
	}
	$desc{'DESC'} =~ s/PMID:\s+(\d+)/PMID: <a href="http:\/\/www.ncbi.nlm.nih.gov\/entrez\/query.fcgi?cmd=Retrieve&db=pubmed&dopt=Abstract&list_uids=$1" title="open PubMed in a new window" target="_blank">$1<\/a>/g;

	for my $pairref ( @{$ref} ) {
		my ( $key, $value ) = (%$pairref);
		next unless ( $desc{$key} );
		print qq(<tr><th width="150">$value</th><td>$desc{$key}</td></tr>\n);
	}
	print "</table>\n";
	#print qq(<p><font size="1">(Move mouse over the <font color="blue"><u>links</u></font> to see further information.)</font></p>\n);
	print qq(<p><font size="1">(Please mail to <a href="mailto:treefam\@sanger.ac.uk">treefam\@sanger.ac.uk</a> if you think this tree is wrong or have any other comments.)</font></p>\n);

	if (defined($desc{NEW_AC}) && $desc{NEW_AC}) {
		print qq(<h3>This family has been curated. Follow the "New Accessions" to see the curated families.</h3><hr>\n);
		return 1;
	}
	return 0;
}
#####################
# used in search page
#
sub print_fam_line
{
	my ($self, $sth, $is_showed, $sth_aux) = @_;
	unless ($sth) {
		print qq(<table border="1" cellspacing="0" cellpadding="0" width="100%" class="main">
			<tr><th>AC<th>SYMBOL<th>NAME<th>NUM<th>Seed Tree<th>Full Tree</tr>\n);
		return 1;
	}
	my @t;
	return 0 unless ((@t = $sth->fetchrow_array()));
	return 2 if ($is_showed && $is_showed->{$t[0]});
	$is_showed->{$t[0]} = 1 if ($is_showed);
	if ($t[1] eq $t[0]) {
		if ($sth_aux && $t[0] =~ /^TF3/) {
			$sth_aux->execute($t[0]);
			($t[1]) = $sth_aux->fetchrow_array;
		} else { $t[1] = 'N/A'; }
	}
	$t[2] = 'N/A' if ($t[2] eq $t[0]);
	$t[3] = 'raw' if ($t[3] eq $t[0]);
	# Due to update, $t[3] (STATUS) is not printed out.
	print qq(<tr><td width="0%"><a href="TFinfo.pl?ac=$t[0]">$t[0]</a>
		<td>$t[1]<td>$t[2]<td width="1%">$t[4]/$t[5]
		<td width="1%"><input type="button" value="view tree" onClick="openPNG('treeview.pl?ac=$t[0]&stype=seed', $t[4])"></td>
		<td width="1%"><input type="button" value="view tree" onClick="openPNG('treeview.pl?ac=$t[0]&stype=full', $t[5])"></td></tr>\n);
	return 1;
}
#############################
# return the link of sequence
#
sub get_link # new
{
	my ($self, $gene, $swcode) = @_;
	my %hash = (
		'HUMAN'=>'Homo_sapiens',
		'MOUSE'=>'Mus_musculus',
		'RAT'  =>'Rattus_norvegicus',
		'BRARE'=>'Danio_rerio',
		'DROME'=>'Drosophila_melanogaster',
		'FUGRU'=>'Fugu_rubripes',
		'CHICK'=>'Gallus_gallus',
		'APIME'=>'Apis_mellifera',
		'CIOIN'=>'Ciona_intestinalis',
		'PANTR'=>'Pan_troglodytes',
		'CANFA'=>'Canis_familiaris',
		'XENTR'=>'Xenopus_tropicalis',
		'TETNG'=>'Tetraodon_nigroviridis',
		'ANOGA'=>'Anopheles_gambiae'
	);
	my %link = (
		'CAEBR'=>'www.wormbase.org/db/gene/gene?name=',
		'CAEEL'=>'www.wormbase.org/db/gene/gene?name=',
		'YEAST'=>'db.yeastgenome.org/cgi-bin/locus.pl?locus=',
		'ARATH'=>'www.arabidopsis.org/servlets/Search?type=general&action=detail&method=4&sub_type=gene&name=',
		'SCHPO'=>'www.genedb.org/genedb/Search?submit=Search+for&organism=pombe&desc=yes&wildcard=yes&name='
	);
	if ($swcode && defined($hash{$swcode})) {
		if ($gene =~ /^ENS[A-Z]*T\d+$/ || $gene =~ /^CG\d+-R[A-Z]$/) {
			return "http://www.ensembl.org/$hash{$swcode}/transview?transcript=$gene&db=core";
		} else {
			return "http://www.ensembl.org/$hash{$swcode}/geneview?gene=$gene";
		}
	} elsif ($swcode && defined($link{$swcode})) {
		return "http://$link{$swcode}$gene";
	} else {
		return "http://www.expasy.ch/cgi-bin/sprot-search-ac?$gene";
	}
}

#####################################
#
# FUNCTIONS FOR PRINTING IMAGE MAPS
#
#####################################

sub print_div_js
{
	my $self = shift;
	print <<EOF;
<style type="text/css">
<!--
	div { text-align: justify; }
	.divT { position:absolute; top: 0px; left: 0px; z-index: 2; width: 250px; visibility:hidden; background-color: #D0FFD0; padding: 0px; border: 0px solid black; }
//-->
</style>

<script language="javascript1.2" src="/mouseover.js"></script>
<script language="javascript" type="text/javascript">
/* this function is copied from PhIGs website */
function ShowDT(gene)
{
	var tooltipOBJ = (document.getElementById) ? document.getElementById(gene) : eval("document.all[gene]");
	if (tooltipOBJ != null) {
		var tooltipLft = (document.body.offsetWidth?document.body.offsetWidth:document.body.style.pixelWidth)
			- (tooltipOBJ.offsetWidth?tooltipOBJ.offsetWidth:(tooltipOBJ.style.pixelWidth?tooltipOBJ.style.pixelWidth:380)) - 30;
		var tooltipTop = 10;
		if (navigator.appName == 'Netscape') {
			tooltipTop = (document.body.scrollTop>=0?document.body.scrollTop+10:event.clientY+10);
			tooltipOBJ.style.top = tooltipTop+"px";
			tooltipOBJ.style.left = tooltipLft+"px";
		} else {
			tooltipTop = (document.body.scrollTop>=0?document.body.scrollTop+10:event.clientY+10);
			if ((event.clientX > tooltipLft)
				&& (event.clientY < (tooltipOBJ.scrollHeight?tooltipOBJ.scrollHeight:tooltipOBJ.style.pixelHeight) + 10))
			{
				tooltipTop = (document.body.scrollTop?document.body.scrollTop:document.body.offsetTop) + event.clientY + 20;
			}
			tooltipOBJ.style.left = tooltipLft;
			tooltipOBJ.style.top = tooltipTop;
		}
		tooltipOBJ.style.visibility = "visible";
	}
}

function HideDT(gene)
{
	var tooltipOBJ = (document.getElementById) ? document.getElementById(gene) : eval("document.all[gene]");
	if (tooltipOBJ != null)
		tooltipOBJ.style.visibility = "hidden";
	hidetip();
}
</script>
EOF
}
sub print_div
{
	my ($self, $list) = @_;
	my $sql = qq{SELECT g.GID,g.DESC,s.TAX_ID,s.TAXNAME FROM genes g, species s WHERE g.ID=? AND g.TAX_ID=s.TAX_ID GROUP BY GID};
	my $sth = $self->db->dbh->prepare($sql);
	foreach my $p (@$list) {
		$sth->execute($p);
		my ($gid, $desc, $tax_id, $taxname) = $sth->fetchrow_array;
		if ($gid) {
			$desc = ($desc)? "$desc<br>" : '';
			print qq(<div class="divT" id="$gid"><table width="100%"><tr><th><font size="2">$gid</font></tr>);
			print qq(<tr><td><font size="1">$desc Taxon: <font color="blue">$taxname [$tax_id]</font></font></tr>);
			print qq(</table></div>\n);
		}
	}
	$sth->finish;
}
##########################
# print map for tree image
#
sub print_map
{
	my $self = shift;
	my $tree = shift;
	my $is_detail = (@_)? shift : 0;
	my $nhx_config = (@_)? shift : ();
	my $count = 0;
	my $nhx = treefam::nhx_plot->new(%$nhx_config);
	$nhx->parse($tree);
	$nhx->plot(0);
	my $id = int(0x7fffffff * rand);
	my ($sth, $sth2, $sth3);
	$sth2 = $self->db->dbh->prepare(qq{SELECT h.AC, h.SCORE, h.EVALUE FROM hmmer h WHERE h.ID=? ORDER BY h.SCORE DESC});
	$sth3 = $self->db->dbh->prepare(qq{SELECT h.AC, h.SCORE, h.EVALUE, g.GID FROM hmmer h, genes g WHERE h.ID=g.ID AND g.GID=? GROUP BY h.AC ORDER BY h.SCORE DESC});
	print "<map name='map$id'>\n";
	foreach my $p ($nhx->node_array) {
		if (defined($p->{node_area})) {
			my $area = join(",", @{$p->{node_area}});
			my $str = '';
			if ($p->{D}) {
				$str .= (uc($p->{D}) eq 'Y')? "duplication<br>" : "speciation<br>";
			}
			$str .= "species: $p->{S}<br>" if ($p->{S});
			$str .= "length: $p->{dist}<br>" if (defined($p->{dist}) && $p->{dist} >= 0.0);
			$str .= "bootstrap: $p->{B}<br>" if (defined($p->{B}) && $p->{B} >= 0);
			if ($p->{E} && $p->{E} =~ /^\$-(\S+)$/) {
				$_ = $1;
				s/\-/,/g;
				$str .= "loss: $_<br>";
			}
			$str =~ s/^(.*)....$/$1/; # chop the trailing <br>
			print qq(<area coords="$area" onmouseover="showtip(this,event,'$str','#D0FFD0')" onmouseout="hidetip()">\n);
			
		}
		if (!$p->{C} && $p->{G}) { # external nodes
			my $str = join(",", @{$p->{area}});
			$sth ||= $self->db->dbh->prepare(qq{SELECT g.GID,g.DESC,s.TAX_ID,s.TAXNAME FROM genes g, species s WHERE g.GID=? AND g.TAX_ID=s.TAX_ID GROUP BY GID});
			$sth->execute($p->{G});
			my ($gid, $desc, $tax_id, $taxname) = $sth->fetchrow_array;
			print qq(<area coords="$str" );
			if ($is_detail == 1) {
				my $gid = $p->{G};
				print qq{onmouseover="ShowDT('$gid')" onmouseout="HideDT('$gid')" };
			} elsif ($is_detail == 2) {
				if ($gid) {
					$desc = ($desc)? "$desc<br>" : '';
					$desc =~ s/['"]//g;
					print qq{onmouseover="showtip(this,event,'$desc Taxon: <font color=%22blue%22>$taxname [$tax_id]</font>','#D0FFD0', 300)" onmouseout="hidetip()" };
				}
			}
			my $link = $self->get_link($p->{G}, $p->{S});
			$link = "%22$link%22";
			my $taxa_link = ($tax_id)? qq(http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&lvl=3&lin=f&keep=1&srchmode=1&unlock&id=$tax_id) : '';
			$taxa_link = "%22$taxa_link%22";
			my $link_id = ($p->{O})? $p->{O} : $p->{G};
			my $sth_true = ($p->{O})? $sth2 : $sth3;
			my $hmmer_str = '<hr>';
			$sth_true->execute($link_id);
			my ($ac, $score, $evalue);
			$hmmer_str .= qq{<font size=%221%22>};
			while ((($ac, $score, $evalue) = $sth_true->fetchrow_array)) {
				$hmmer_str .= qq{<a href=%22TFinfo.pl?ac=$ac%22>$ac</a> $score<br>};
			}
			$hmmer_str .= qq{</font>};
			chop($hmmer_str) for (0 .. 3);
			print qq{href="javascript:showtip(0,0,'<a href=$link title=%22go to source database%22 target=%22_blank%22>view gene</a><br><a href=$taxa_link title=%22go to NCBI taxa browser%22 target=%22_blank%22>view taxon</a><br><a href=%22TFseq.pl?id=$link_id%22 title=%22go to TreeFam gene page%22 target=%22_blank%22>view TFseq</a>$hmmer_str','#D0FFD0')">\n};
		}
	}
	$sth->finish if ($sth);
	$sth2->finish if ($sth2);
	$sth3->finish if ($sth3);
	print "</map>\n";
	return ("map$id", $nhx);
}
#########################
# print map for alignment
#
sub print_aln_map
{
	my ($self, $ac) = @_;
	my $id = int(rand(0x7fffffff));
	my @aln;
	$self->db->get_aln_pos($ac, \@aln);
	my $ap = treefam::align_plot->new;
	$ap->init(\@aln);
	$ap->align_plot;
	print qq{<map name="aln$id">\n};
	foreach my $p (sort keys %{$ap->{_area}}) {
		my @t = split(",", $ap->{_area}{$p});
		print qq(<area shape=rect coords="$p" title="$t[0], $t[1]" target="_blank" href="http://www.sanger.ac.uk/cgi-bin/Pfam/getacc?$t[0]">\n);
	}
	print "</map>\n";
	return "aln$id";
}
sub print_hmmer_match
{
	my ($self, $ac, $leaf_list) = @_;
	my (@t, %has_appear, %gene_hash);
	my $sth = $self->dbh->prepare(qq{SELECT g.GID FROM genes g WHERE g.ID=?});
	if (!$leaf_list) {
		$self->db->get_leaves($ac, \@t);
		$leaf_list = \@t;
	}
	foreach my $p (@$leaf_list) {
		$sth->execute($p);
		($_) = $sth->fetchrow_array;
		$gene_hash{$_} = 1;
	}
	$sth->finish;
	$sth = $self->dbh->prepare(qq{SELECT g.ID, h.SCORE, s.SWCODE, g.SYMBOL, g.GID FROM hmmer h, genes g, species s WHERE h.AC=? AND h.ID=g.ID AND g.TAX_ID=s.TAX_ID ORDER BY h.SCORE DESC, g.ID});
	$sth->execute($ac);
	print qq{<h3>HMMer Matches<font size="1"><br><a href="javascript:void(0);"
onMouseOver="showtip(this, event, 'Speices in <font color=%22red%22>red</font> color are outgroups. <font color=%22red%22>Red</font> scores indicate genes that belong to the current family.', '#d0ffd0', 300)"
onMouseOut="hidetip()">Instructions</a></font></h3>\n};
	print qq{<table cellspacing="0" cellpadding="0" width="100%" class="tiny">\n};
	my %out = (ARATH=>1, YEAST=>1, SCHPO=>1);
	while ((@t = $sth->fetchrow_array)) {
		next if ($has_appear{$t[4]});
		$has_appear{$t[4]} = 1;
		print qq{<tr><td class="tiny"><a href="TFseq.pl?id=$t[0]" target="_blank">$t[3]</a></td><td class=};
		print ((!$out{$t[2]})? qq{"tiny"} : qq{"redtiny"});
		print qq{>$t[2]</td><td align="right" class=};
		print ((!$gene_hash{$t[4]})? qq{"tiny"} : qq{"redtiny"});
		print qq{>$t[1]</td>\n};
	}
	print qq{</table><hr>\n};
	$sth->finish;
}
sub print_quick_search
{
	my $self = shift;
	my $cgi_bin = $self->locate_cgi_dir;
	print qq{<h3>Quick Search<font size="1"><br>
<a href="javascript:window.document.fam.f.value='A cyclin';window.document.gene.q.value='cyclin human mouse';void(0);">example</a>
</font></h3>
<form name="fam" method="get" action="$cgi_bin/search2.pl">
<table>
	<tr><td size=100%><input name="f"></tr>
	<tr><td><input type="submit" value="Go for families"></tr>
</table></form>
<form name="gene" method="get" action="$cgi_bin/search2.pl">
<table>
	<tr><td size=100%><input name="q"></tr>
	<tr><td><input type="submit" value="Go for genes"></tr>
</table></form>
<hr>\n};
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
