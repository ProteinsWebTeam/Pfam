#! /software/bin/perl -w

# A program to pull features from Mole/EMBL for the given EMBL ID (and coordinates)

use strict;
use Getopt::Long;
use DBI;
use Rfam;

#Graphic Modules
use Bio::Pfam::Drawing::Layout::PfamLayoutManager;
use Bio::Pfam::Drawing::Image::ImageSet;
use Bio::Pfam::Drawing::Image::Image;
use XML::LibXML;
use Digest::MD5 qw(md5_hex);

# MySQL connection details.
#my $database = $Rfam::embl;
#my $database = "embl_test";
my $database = "embl_93";
my $host     = "cbi3";
my $user     = "genero";
my $dist     = 10000;
my ($start,$end,$name,$plusstrand,$minusstrand,$strand,$help);
my $printname="";

&GetOptions("a|s|start|begin=s"  => \$start,
            "e|b|end=s"          => \$end,
            "i|n|id|embl|name=s" => \$name,
	    "strand|str=s"       => \$strand,
	    "plusstrand|p"       => \$plusstrand,
	    "minusstrand|m"      => \$minusstrand,
	    "d|dist|distance=s"  => \$dist,
	    "h|help"             => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

if ($name =~ /(\S+)\/(\d+)\-(\d+)\:(\S+)/){
    $name = $1;
    $start = $2;
    $end = $3;
    $strand = $4;
}
elsif ($name =~ /(\S+)\/(\d+)\-(\d+)/){
    $name  = $1;
    $start = $2;
    $end   = $3;
}

if (!defined($strand) && (defined($plusstrand) || defined($minusstrand)) ){
    if (defined($plusstrand) && defined($minusstrand)){
	die("Cant be both + and - minus stranded you muppet!");
    }
    
    if (defined($plusstrand)){
	$strand = 1;
	$printname = "$name\/$start\-$end";
    }
    elsif (defined($minusstrand)){
	$strand = -1;
	$printname = "$name\/$end\-$start";
    }
}
elsif ( (!defined($strand) && !defined($plusstrand) && !defined($minusstrand)) && defined($start) && defined($end) ) {
    if ($start<$end){
	$strand = 1;
	print "Fetching: $name\/$start\-$end\tstrand=$strand\n";
	$printname = "$name\/$start\-$end";
    }
    else {
	$strand = -1;
	print "Fetching: $name\/$start\-$end\tstrand=$strand\n";
	$printname = "$name\/$start\-$end";
	my $tmp = $start;
	$start = $end;
	$end = $tmp;
    }
}

if( !defined($name) && !defined($start) && !defined($end) && !defined($strand) ) {
    print "FATAL: one of name [$name] or start [$start] or end [$end] or strand [$strand] is not defined\n";
    &help();
    exit(1);
}
elsif (length($printname)==0 && $strand>0) {
    $printname = "$name\/$start\-$end";
}
elsif (length($printname)==0 && $strand<0) {
    $printname = "$name\/$end\-$start";
}

my $query0 = qq(
           select entry.accession_version, dbxref.database_id, dbxref.primary_id, dbxref.secondary_id, dbxref.tertiary_id, entry.sequence_length
           from entry, dbxref
           where entry.accession_version=?
           and entry.entry_id=dbxref.entry_id;
   );

# Create a connection to the database.
my $dbh = DBI->connect(
    "dbi:mysql:$database;$host", $user, "", {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
}    );

my $features0;
my $sequencelength=0;
###########
# Prepare the query for execution.
my $sth0 = $dbh->prepare($query0);
 $sth0->execute($name);
my $res0 = $sth0->fetchall_arrayref;
foreach my $row (@$res0){
    $features0 .= join("\t", (@{$row}, "\n") );
    $sequencelength = pop(@{$row});
}

#######
my $unfound=0;
if( !defined($features0) ) {
    $features0 = "no features available";
    $unfound = 1;
}

my $totallength  = 2*$dist + ($end-$start+1);
my $rfamend      = $dist + ($end-$start+1);
my $xscale       = 1000/$totallength;

my $delta5 = $dist-$start;
my $delta3 = ($delta5+$sequencelength) - $totallength + 1;

my ($xmlrfamstart,$xmlrfamend)=seq2xmlblock($start, $end, $delta5, $delta3, $strand, $sequencelength);

my $xmlhead = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<image xmlns=\"http://pfam.sanger.ac.uk/static/documents/pfamDomainGraphics.xsd\"
       xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
       xsi:schemaLocation=\"http://pfam.sanger.ac.uk/static/documents/pfamDomainGraphics.xsd
                           http://pfam.sanger.ac.uk/static/documents/pfamDomainGraphics.xsd\"
       format=\"png\" 
       layout=\"continuous\" 
       scale_x=\"$xscale\" 
       scale_y=\"1.0\">
  <sequence length=\"$totallength\" display_data=\"test all drawing features\" name=\"TestSeq\">
    <region start=\"$xmlrfamstart\" end=\"$xmlrfamend\" label=\"Rfam\">
      <colour1>
        <colour><hex hexcode=\"c00f0f\"/></colour>
      </colour1>
      <colour2>
        <colour><hex hexcode=\"e83737\"/></colour>
      </colour2>
      <bigShape leftStyle=\"curved\" rightStyle=\"curved\"/>
    </region>
";

my ($xmlseqstart,$xmlseqend)=seq2xmlblock(1, $sequencelength, $delta5, $delta3, $strand, $sequencelength);
#print "xmlseqstart=$xmlseqstart, xmlseqend=$xmlseqend\n\n";

#Mark beginning
if ($xmlseqstart>0){
$xmlhead .= "    <markup start=\"$xmlseqstart\" v_align=\"top\" label=\"start\">
      <line style=\"bold\">
        <colour>
          <hex hexcode=\"666666\"/>
        </colour>
      </line>
      <head style=\"diamond\">
        <colour>
          <hex hexcode=\"FF3366\"/>
        </colour>
      </head>
    </markup>
";
}

#Mark end
if ( $xmlseqend<$totallength){

     $xmlhead .= "    <markup start=\"$xmlseqend\" v_align=\"top\" label=\"end\">
      <line style=\"bold\">
        <colour>
          <hex hexcode=\"666666\"/>
        </colour>
      </line>
      <head style=\"circle\">
        <colour>
          <hex hexcode=\"FF3366\"/>
        </colour>
      </head>
    </markup>
";
}

my $xmltail = "  </sequence>
</image>
";

if (!(-e "domain_gfx")){
     mkdir "domain_gfx" or die "Can't create directory: domain_gfx\n[$!]";
}

my $xmlfeature = "";
my ($cdscolour1, $cdscolour2) = ("hex hexcode=\"339999\"","RGB R=\"51\" G=\"204\" B=\"204\"");
my ($rnacolour1, $rnacolour2) = ("hex hexcode=\"666666\"","hex hexcode=\"9966cc\"");

my $featurecount = 0;
my @features0 = split(/\n/, $features0);
for (my $i = 0; $i < scalar(@features0); $i++) { 
    
    if ($unfound || scalar(@features0) == 0){
	print "$i: $features0[$i]\n";
	last;
    }
    
    my $sstrand=1;
    my ($sstart, $send); 
    if ($features0[$i] =~ m/\s+(\d+)\.\.(\d+)/){
	$sstart = $1;
	$send = $2;
    }
    elsif ($features0[$i] =~ m/\s+complement\((\d+)\.\.(\d+)\)/){
	$sstart = $1;
	$send = $2;	
	$sstrand = -1;
    }
    
    my ($colour1, $colour2) = ("hex hexcode=\"9999ff\"","hex hexcode=\"99ccff\"");
    my $featurename = "";
    if ($features0[$i] =~ m/^\S+\s+(\S+)/){
        $featurename = $1;
        $featurename =~ s/EMBL\-//g;
        if ($featurename =~ m/CDS/){
                ($colour1, $colour2) = ($cdscolour1, $cdscolour2);
        }
        elsif ($featurename =~ m/RNA/){
                ($colour1, $colour2) = ($rnacolour1, $rnacolour2);
        }
    }
    
    if (defined($start) && defined($end) && defined($sstart) && defined($send)) {
        
        my ($xmlfeaturestart,$xmlfeatureend)=seq2xmlblock($sstart, $send, $delta5, $delta3, $strand, $sequencelength);
        
	if ( (0<$xmlfeaturestart && $xmlfeaturestart<$totallength) || (0<$xmlfeatureend && $xmlfeatureend<$totallength)  ) {
	    $featurecount++;
            
            my $ol = overlap($start, $end, $sstart, $send);
            my $sdist = 0;
            
            if ($ol){
                  $sdist = minabs($sstart-$start, $send-$end);
            }
            else {
                  $sdist = minabs($send-$start, $sstart-$end);
            }
            
            print "$i: $features0[$i]\t$sdist\n";

#print "sequencelength=$sequencelength, totallength=$totallength, delta5=$delta5, delta3=$delta3, dist=$dist\n";
#print "sstart=$sstart, send=$send\n";
#print "xmlfeaturestart=$xmlfeaturestart, xmlfeatureend=$xmlfeatureend\n\n";

            $xmlfeature .= "    <region start=\"$xmlfeaturestart\" end=\"$xmlfeatureend\" label=\"$featurename\">
      <colour1>
        <colour><$colour1/></colour>
      </colour1>
      <colour2>
        <colour><$colour2/></colour>
      </colour2>
      <bigShape leftStyle=\"curved\" rightStyle=\"curved\"/>
    </region>
";
        $xmlfeatureend=$xmlfeatureend-1;

        if ($sstrand*$strand<0 && (0<$xmlfeatureend && $xmlfeatureend<$totallength) ){
            $xmlfeature .= "    <markup start=\"$xmlfeatureend\" v_align=\"bottom\" label=\"negative strand\">
      <line style=\"bold\">
        <colour>
          <hex hexcode=\"666666\"/>
        </colour>
      </line>
      <head style=\"circle\">
        <colour>
          <hex hexcode=\"333399\"/>
        </colour>
      </head>
    </markup>
";
           }        
#	
	}
    }
    elsif (!defined($start) && !defined($end)) {
	print "$i: $features0[$i]\t$name\n";
    }
    
}

$dbh->disconnect;

if ($featurecount>0){
    $printname =~ s/\//\_/;
#    my $pngfilename = "domain_gfx/$name\_$start-$end.png";
#    my $xmlfilename = "domain_gfx/$name\_$start-$end.xml";
    my $pngfilename = "domain_gfx/$printname\.png";
    my $xmlfilename = "domain_gfx/$printname\.xml";
    my $xmlString = $xmlhead . $xmlfeature . $xmltail;
    open(OUTFILE, ">$xmlfilename") or warn "Cannot print $xmlfilename: [$!]\n";
    printf OUTFILE $xmlString . "\n";
    close(OUTFILE);
    my $parser = XML::LibXML->new;
    my $dom = $parser->parse_string( $xmlString );

    #The drawing bit
    my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
    $imageset->create_images($dom);

    foreach my $im ($imageset->each_image){
      print_image_ppg($im,$pngfilename);
    }
    
    make_html();
}


exit();


######################################################################
sub maxabs {
  return $_[0] if @_ == 1;
  abs($_[0]) > abs($_[1]) ? $_[0] : $_[1]
}

sub minabs {
  return $_[0] if @_ == 1;
  abs($_[0]) < abs($_[1]) ? $_[0] : $_[1]
}

sub max {
  return $_[0] if @_ == 1;
  $_[0] > $_[1] ? $_[0] : $_[1]
}

sub min {
  return $_[0] if @_ == 1;
  $_[0] < $_[1] ? $_[0] : $_[1]
}

######################################################################
#Paul's nasty hack of Rob's "print_image" function:
sub print_image_ppg {
  my $self = shift;
  my $filename = shift;
  
  open(OUTFILE, ">$filename") or warn "Cannot print $filename: [$!]\n";
  
  binmode OUTFILE;
  # Convert the image to PNG and print it on standard output
  print OUTFILE $self->image->png;
  close(OUTFILE) or warn "Cannot close $filename :[$!]";
  
  warn "try to convert from png to different format, this is not implemented!\n"
        if( $self->format && $self->format ne "png" );
  
}

######################################################################
#Collate all the png images into a single html page:
sub make_html {
    
    my $htmlhead = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html>
<head>
    
<title>Rfam: synteny map</title>
<body>

<h1>Rfam: synteny map</h1>\n\n\n";
    
    my $htmltail = "
<!-- ====================================================================== -->

    <!-- end of content -->

    
    <!-- start of footer -->
    <div id=\"footer\"></div>

    <div class=\"cleaner\"><!-- wrap --></div>
    <!-- end of footer -->

    </div>

<!-- footer start -->

<div id=\"siteFooter\">
  <p>Comments or questions on the site? Send a mail to <a href=\"mailto:rfam\@sanger.ac.uk\">rfam\@sanger.ac.uk</a></p>
  <p class=\"spaced\"><a href=\"http://www.wellcome.ac.uk/\">The Wellcome Trust</a></p>
</div>

<!-- footer end -->

  </body>

</html>\n";

    my @pngfile = glob("domain_gfx/*png");
    my $htmlbody = "";

    foreach my $pf (@pngfile){
	
	$pf =~ m/^domain_gfx\/(\S+)\.(\d+)\_(\d+)\-(\d+)\.png/;
	my ($name, $version, $start, $end) = ($1, $2, $3, $4);
	my @shortpf = split(/\//, $pf);
	my $shortpf = $shortpf[1];
	
	$htmlbody .= "<small><b>$name\.$version\/$start\-$end</b></small><br />\n<a href=\"http://srs.ebi.ac.uk/srsbin/cgi-bin/wgetz?-noSession+-e+[EMBLRELEASE-ACC:$name]\"><img src=\"$shortpf\"\n     usemap=\"#$name\.$version\/$start\-$end\"\n     alt=\"\" /></a><br />\n\n\n";
	
    }
    
    open(OUTFILE, ">domain_gfx/index_auto.html") or warn "Cannot print to domain_gfx/index_auto.html: [$!]\n";
    print OUTFILE $htmlhead . $htmlbody . $htmltail;    
    close(OUTFILE);
}
######################################################################
sub overlap {
    my($x1, $y1, $x2, $y2) = @_;
    
    if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
        return 1;
    }
    else {
        return 0;
    }
}

######################################################################
sub seq2xmlcoord {
    my($coord, $delta5, $delta3, $strand, $sequencelength) = @_;
    my $xmlcoord=0;
    if ($strand>0){
	$xmlcoord = $coord+$delta5;
    }
    elsif ($strand<0) {
	my $coord3pto5p = $sequencelength - $coord;
	$xmlcoord = $coord3pto5p-$delta3;
    }
    
    return $xmlcoord;
}

######################################################################
sub seq2xmlblock {
    my($coord1, $coord2, $delta5, $delta3, $strand, $sequencelength) = @_;
    my ($xmlcoord1, $xmlcoord2)=(0,0);
    if ($strand>0){
	$xmlcoord1 = seq2xmlcoord($coord1, $delta5, $delta3, $strand, $sequencelength);
	$xmlcoord2 = seq2xmlcoord($coord2, $delta5, $delta3, $strand, $sequencelength);
    }
    elsif ($strand<0) {
	$xmlcoord1 = seq2xmlcoord($coord2, $delta5, $delta3, $strand, $sequencelength);
	$xmlcoord2 = seq2xmlcoord($coord1, $delta5, $delta3, $strand, $sequencelength);
    }
    
    return ($xmlcoord1, $xmlcoord2);
}

######################################################################
sub help {
    
    print STDERR <<EOF;

find_flanking_features.pl - Connects to the mole database, fetches regions from the EMBL file within 
dist nucleotides either side of the input region. Returns the results in a tabular format to STDOUT 
and graphical format to domain_gfx/index_auto.html 
                
Usage:   find_flanking_features.pl <options>

Options:       
  -h or -help                Show this help.
  -i|-n|-id|-embl|-name <str>  EMBL ID or Xfam n/s-e format.
  -a|-s|-start|-begin   <num>  Start coordinate (optional)
  -e|-b|-end            <num>  End coordinate   (optional)
  -strand|-str          <1|-1> Strand           (optional)
  -plusstrand|-p               Positive strand  (optional)
  -minusstrand|-m              Minus strand     (optional)
  -d|-dist|-distance    <num>  Distance between coordinates and 
                               features for printing (default=$dist)

Popular run modes:
On SEED:
sreformat --pfam stockholm SEED | grep \"/\" | grep -v \"//\" | awk \'{print \"find_flanking_features.pl -d 5000 -n \"\$1}\' | sh

On ALIGN2SEED:
grep \">\" ALIGN2SEED | tr -d \">\" | awk \'{print \"find_flanking_features.pl -d 5000 -n \"\$1}\' | sh

EOF
}



