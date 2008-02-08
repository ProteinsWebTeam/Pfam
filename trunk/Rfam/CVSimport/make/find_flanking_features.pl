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
my (@name,@start,@end,@strand,@printname,$plusstrand,$minusstrand,$outlist,$makehtml,$help);
my $printthreshold=0;
&GetOptions("a|s|start|begin=i@"  => \@start,
            "e|b|end=i@"          => \@end,
            "i|n|id|embl|name=s@" => \@name,
	    "strand|str=s@"       => \@strand,
	    "plusstrand|p"        => \$plusstrand,
	    "minusstrand|m"       => \$minusstrand,
	    "d|dist|distance=i"  => \$dist,
	    "o|outlist"           => \$outlist,
	    "t|thresh|threshold=s"  => \$printthreshold,
	    "makehtml"                => \$makehtml,
	    "h|help"              => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

if ($makehtml){
    make_html();
    exit();
}

#Initialise @name, @start, @end, @strand
my (@type,@score,$threshold, @desc);
my $rfamdesc = "Rfam";
if (defined($outlist)){
    if (-e "out.list"){
	open(OUTLIST, "out.list") or die "Could not open out.list\n[$!]";
    }
    else {
	print "FATAL: could not find the file \42out.list\42\n\n";
	&help();
	exit(1);
    }
    
    open( DESC, "DESC" ) or warn "Can't open DESC to determine global/local requirement\n";
    while( <DESC> ) {
	/^ID/ and do {
	    substr($_,0,3) = "";
	    $rfamdesc = $_;
	};
    }
    close DESC;
    
    (@name,@start,@end,@strand,@printname) = ((),(),(),(),(),());
    while (my $line = <OUTLIST>){
	if ($line =~ m/^(\S+)\t(\S+)\t(\S+)\t(\d+)\t(\d+)\s+(.+)/){
	    if ($printthreshold<$1){
		push(@score, $1);
		push(@type, $2);
		my ($oname, $ostart, $oend, $odesc) = ($3, $4, $5, $6);
		push(@name, $oname);
		push(@desc,$odesc);
		push(@printname, "$oname\_$ostart\-$oend");
		if ($ostart<$oend){
		    push(@strand, 1);
		    push(@start, $ostart);
		    push(@end, $oend);
		}
		else {
		    push(@strand, -1);
		    push(@start, $oend);
		    push(@end, $ostart);
		}
	    }
	}
	elsif ($line =~ m/CURRENT THRESHOLD: (\S+) bits/) {
	    $threshold = $1;
	}
	
    }
    close(OUTLIST);
}
elsif (@name) {
    $score[0]=0;
    $type[0]="ALIGN";
    $threshold = $printthreshold;
    if ($name[0] =~ /(\S+)\/(\d+)\-(\d+)\:(\S+)/){
	$name[0] = $1;
	$start[0] = $2;
	$end[0] = $3;
	$strand[0] = $4;
	$desc[0] = "";
    }
    elsif ($name[0] =~ /(\S+)\/(\d+)\-(\d+)/){
	$name[0] = $1;
	$start[0] = $2;
	$end[0] = $3;
	$desc[0] = "";
    }
    
    if (!@strand && (defined($plusstrand) || defined($minusstrand)) ){
	if (defined($plusstrand) && defined($minusstrand)){
	    print "Cant be both + and - minus stranded you muppet!";
	    &help();
	    exit(1);
	}
	
	if (defined($plusstrand)){
	    $strand[0] = 1;
	    $printname[0] = "$name[0]\_$start[0]\-$end[0]";
	}
	elsif (defined($minusstrand)){
	    $strand[0] = -1;
	    $printname[0] = "$name[0]\_$end[0]\-$start[0]";
	}
    }
    elsif ( (!@strand && !defined($plusstrand) && !defined($minusstrand)) && @start && @end ) {
	
	$printname[0] = "$name[0]\_$start[0]\-$end[0]";
	if ($start[0]<$end[0]){
	    $strand[0] = 1;
	}
	else {
	    $strand[0] = -1;
	    my $tmp = $start[0];
	    $start[0] = $end[0];
	    $end[0] = $tmp;
	}
    }
}

if( !@name || !@start || !@end || !@strand ) {
    print "FATAL: one of name [$name[0]] or start [$start[0]] or end [$end[0]] or strand [$strand[0]] is not defined\n";
    &help();
    exit(1);
}
elsif ( scalar(@name)!=scalar(@start) || scalar(@name)!=scalar(@end) || scalar(@name)!=scalar(@strand) || scalar(@name)!=scalar(@printname) ) {
    print "FATAL: one of name [@name] or start [@start] or end [@end] or strand [@strand] is unmatched\n";
    &help();
    exit(1);
}

my $query0 = qq(
           select entry.accession_version, dbxref.database_id, dbxref.primary_id, dbxref.secondary_id, dbxref.tertiary_id, dbxref.quaternary_id, entry.sequence_length
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

my $htmlbody="";
my @xmlString;
my $firstbelowthresh=1;
###############BIG LOOP BEGINS HERE##############
for (my $ii=0; $ii<scalar(@name); $ii++){
    my $name   = $name[$ii];
    my $start  = $start[$ii];
    my $end    = $end[$ii];
    my $strand = $strand[$ii];
    my $desc   = $desc[$ii];
    
    my $type   = $type[$ii];
    my $score  = $score[$ii];
    my $printname = $printname[$ii];
    my $features0;
    my $sequencelength=0;
    
    print "FEATURE: $score\t$type\t$name\t$start\t$end\tstrand=$strand\n";
    
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
    
    my $totallength  = 2*$dist + $end-$start+1;
    my $rfamend      =   $dist + $end-$start+1;
    my $xscale       = 1000/$totallength;
    
    my $delta5       = $dist-$start;
    my $delta3       = ($delta5+$sequencelength) - $totallength + 1;
    
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
    <region start=\"$xmlrfamstart\" end=\"$xmlrfamend\" label=\"$rfamdesc\">
      <colour1>
        <colour><hex hexcode=\"c00f0f\"/></colour>
      </colour1>
      <colour2>
        <colour><hex hexcode=\"e83737\"/></colour>
      </colour2>
      <bigShape leftStyle=\"straight\" rightStyle=\"arrow\"/>
    </region>
";

    my ($xmlseqstart,$xmlseqend)=seq2xmlblock(1, $sequencelength, $delta5, $delta3, $strand, $sequencelength);

#Mark beginning
    if (0<$xmlseqstart && $xmlseqstart<$totallength ){
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
    if ( 0<$xmlseqend && $xmlseqend<$totallength ){

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
        my ($featurename,$featuredesc) = ("","");
        if ($features0[$i] =~ m/^\S+\t(\S+)\t\S+\t\S+\t\S+\t(\S+)/){
            #print "features0[$i] $features0[$i]\n";
            $featurename = $1;
            $featuredesc = $2;
            $featurename =~ s/EMBL\-//g;
            if ($featurename =~ m/CDS/){
                    ($colour1, $colour2) = ($cdscolour1, $cdscolour2);
            }
            elsif ($featurename =~ m/RNA/i || $featurename =~ m/RFAM/i){
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
               
               my ($leftStyle,$rightStyle) = ("straight","arrow");
               if ($sstrand*$strand<0 && (0<$xmlfeatureend && $xmlfeatureend<$totallength) ){
                              ($leftStyle,$rightStyle) = ("arrow","straight");
               }

                $xmlfeature .= "    <region start=\"$xmlfeaturestart\" end=\"$xmlfeatureend\" label=\"$featurename $featuredesc\">
      <colour1>
        <colour><$colour1/></colour>
      </colour1>
      <colour2>
        <colour><$colour2/></colour>
      </colour2>
      <bigShape leftStyle=\"$leftStyle\" rightStyle=\"$rightStyle\"/>
    </region>
";
              	
	 }
    }
#    elsif (!defined($sstart) && !defined($send)) {
#	print "$i: $features0[$i]\t$name\n";
#    }
    
   }

    my $xmlString = $xmlhead . $xmlfeature . $xmltail;
    push(@xmlString, $xmlString);
    
    my $pngfilename = "domain_gfx/$printname\.png";
#    my $xmlfilename = "domain_gfx/$printname\.xml";

#    open(OUTFILE, ">$xmlfilename") or warn "Cannot print $xmlfilename: [$!]\n";
#    printf OUTFILE $xmlString . "\n";
#    close(OUTFILE);
    my $parser = XML::LibXML->new;
    my $dom = $parser->parse_string( $xmlString );

    #The drawing bit
    my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
    $imageset->create_images($dom);

    foreach my $im ($imageset->each_image){
      print_image_ppg($im,$pngfilename);
    }
    
    my ($markupstart,$markupend) = ("", "");
    if ($type =~ /ALIGN/ && $score>$threshold){
	$markupstart = "<font color=\"\#0000A0\">";
	$markupend = "</font>";
    }
    elsif ($type =~ /ALIGN/ && $score<$threshold ){
	$markupstart = "<font color=\"\#006400\">";
	$markupend = "</font>";
    }
    elsif ($type =~ /SEED/ && $score<$threshold ){
	$markupstart = "<font color=\"\#FF0000\">";
	$markupend = "</font>";
    }
    
    if ($score<$threshold && $firstbelowthresh){
	$htmlbody .= "<br /><br />" . "&#42;" x 11 . "CURRENT THRESHOLD: $threshold bits" . "&#42;" x 11 . "<br/><br />\n";
	$firstbelowthresh=0;
    }
    
    $name =~ m/^(\S+)\.\d+/;
    my $shortname = $1;
    $pngfilename =~ s/domain_gfx\///;
    $htmlbody .= "$markupstart<small><b>$score &#x0009; $type &#x0009; $name\/$start\-$end strand=$strand</b> $desc</small>$markupend<br />\n<a href=\"http://srs.ebi.ac.uk/srsbin/cgi-bin/wgetz?-noSession+-e+[EMBLRELEASE-ACC:$shortname]\"><img src=\"$pngfilename\"\n     usemap=\"#$name\/$start\-$end\"\n     alt=\"\" /></a><br />\n";
    
#    $htmlbody .=  "<img src=" . $pngfilename . " usemap=\#" . $imageset->Bio::Pfam::Drawing::Image::Image::image_name . " border=0>";
#    $htmlbody .=  "<map name =".$imageset->Bio::Pfam::Drawing::Image::Image::image_name.">";
#    $htmlbody .=  $imageset->Bio::Pfam::Drawing::Image::Image::image_map;
#    $htmlbody .=  "</map>\n\n\n";



#print "<img src=".$image->file_location." usemap=\#".$image->image_name." border=0>"
#print "<map name =".$image->image_name.">";
#print $image->image_map;
#print "</map>    



}
$dbh->disconnect;

make_html_ordered($htmlbody);

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
sub make_html_ordered {
    my $htmlbody = shift;
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

    open(OUTFILE, ">domain_gfx/index_auto.html") or warn "Cannot print to domain_gfx/index_auto.html: [$!]\n";
    print OUTFILE $htmlhead . $htmlbody . $htmltail;    
    close(OUTFILE);

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
    my (%markupstarts,%markupends);
    if (-e "domain_gfx/markup"){
	open(MARKUP, "<domain_gfx/markup") or warn "Cannot open domain_gfx/markup: [$!]\n";
	while (my $l = <MARKUP>){
	    if ($l =~ m/(\S+)\.(\d+)\_(\d+)\-(\d+)\.png/){
		push(@{ $markupstarts{$1} }, $3);
		push(@{ $markupends{$1} },   $4);
	    }
	    elsif ($l =~ m/(\S+)\.(\d+)\/(\d+)\-(\d+)/){
		push(@{ $markupstarts{$1} }, $3);
		push(@{ $markupends{$1} },   $4);
	    }
	}
	close(MARKUP);
    }
    
    foreach my $pf (@pngfile){
	
	$pf =~ m/^domain_gfx\/(\S+)\.(\d+)\_(\d+)\-(\d+)\.png/;
	my ($name, $version, $start, $end) = ($1, $2, $3, $4);
	my ($markupstart,$markupend) = ("", "");
	my @shortpf = split(/\//, $pf);
	my $shortpf = $shortpf[1];
	
	if (defined($markupstarts{$name}) && defined($markupends{$name}) ){
	    for (my $i=0; $i<scalar(@{$markupstarts{$name}}); $i++){
		if ( ($markupstarts{$name}[$i]==$start && $markupends{$name}[$i]==$end) || ($markupends{$name}[$i]==$start && $markupstarts{$name}[$i]==$end) ){
		    $markupstart = "<font color=\"\#FF0000\">";
		    $markupend = "</font>";
		}
	    }
	}
	
	$htmlbody .= "$markupstart<small><b>$name\.$version\/$start\-$end</b></small>$markupend<br />\n<a href=\"http://srs.ebi.ac.uk/srsbin/cgi-bin/wgetz?-noSession+-e+[EMBLRELEASE-ACC:$name]\"><img src=\"$shortpf\"\n     usemap=\"#$name\.$version\/$start\-$end\"\n     alt=\"\" /></a><br />\n\n\n";
	
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
\"dist\" nucleotides either side of the input region. Returns the results in a tabular format to STDOUT 
and graphical format to domain_gfx/index_auto.html 
                
One can markup fonts in index_auto.html adding either png filenames or name/start-end strings to the file 
\"domain_gfx/markup\", NB. one entry per line. 

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
  -makehtml                    Generates unordered html for all previously generated graphics.
  -o|-outlist                  Read in sequences and coords from out.list
  -t|-thresh|-threshold <num>  Only used in conjunction with the -o option, only produces graphics for 
                               regions with score greater than <num>.

EXAMPLES: 
On out.list (from rfmake.pl -l):
find_flanking_features.pl -o 

On ALIGN2SEED:
grep \">\" ALIGN2SEED | tr -d \">\" | awk \'{print \"find_flanking_features.pl -d 5000 -n \"\$1}\' | sh
grep \">\" ALIGN2SEED | tr -d \">\" | awk \'{print \$1}\' > domain_gfx/markup
find_flanking_features.pl -makehtml

On SEED:
sreformat --pfam stockholm SEED | grep \"/\" | grep -v \"//\" | awk \'{print \"find_flanking_features.pl -d 5000 -n \"\$1}\' | sh
\#Should be followed by running:
find_flanking_features.pl -makehtml

TO ADD:

Make  graphics prettier, arrows instead of lollipops to indicate strand, show sequence start and ends on the backbone (addOffSet).

Use \"image_map\" so that nice pop-up windows can bee added. 

Could switch to one of biograph, Zmap, gff2ps, gbrowse, ...

EOF
}



