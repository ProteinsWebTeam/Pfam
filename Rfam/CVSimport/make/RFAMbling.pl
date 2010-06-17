#!/software/bin/perl

use strict;
use warnings;
use List::Util qw(max min);
use Getopt::Long;
use DBI;
use JSON;
use Data::Dump qw( dump );

use Rfam;
use Rfam::RfamAlign;

#Program outline:
#Implemented:
#1. read in alignment
#2. make a most-informative/consensus sequence
#3. read in ss-stats-perbasepair
#4. use Andreas Grubers' SVG functions to make RNA 2' structure diagrams marking up fracion-of-canonical-pairs & covariation metrics

#To add:
#5. Generate Ben's HMM-logos
#6. Generate BubblePlots
#   http://biowiki.org/BubblePlots
#7. Score distributions (out.list.pdf)
#8. TexShade?

my ($alnfile, $statsfile, $help);

&GetOptions(
    "f|a|alnfile=s"      => \$alnfile,
    "s|statsfile=s"      => \$statsfile,
    "h|help"             => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

if (!defined($alnfile)){
    $alnfile = "SEED";
}

if (!defined($statsfile)){
    $statsfile = "ss-stats-perbasepair";
}

if ( !(-e $alnfile) || !(-e $statsfile) ){
    print "FATAL: missing either $alnfile or $statsfile\n";
    &help();
    exit(1);
}


#system("sreformat --mingap --pfam stockholm $alnfile");

#if (!defined($dontrunbollocks)){

#open( SD, "$alnfile" ) or die ("FATAL: Couldn't open $alnfile [$!]\n $!\n");
open( SD, "sreformat --pfam stockholm $alnfile | " ) or die ("FATAL: Couldn't open $alnfile [$!]\n $!\n");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SD );
close(SD);
my @list = $seed->each_seq();
my $self  = new Rfam::RfamAlign;

my $ss_cons = $seed->ss_cons->getInfernalString(); #This is damned confusing!
my $mis = $seed->mis;

my $length = length($mis);
$ss_cons =~ tr/[\<\>\[\]\{\}]/[\(\)\(\)\(\)]/;

#PS file globals:
my $macro_seen= 0;
my %mfe = ();        # hash of mfe pairs
my @ss_ps = ('',''); # head and tail of the ss.ps file

my $zzz = "-1:" x $length;
my @fcbp = split(':', $zzz);
my @covariation  = @fcbp;

#remove gaps from $mis update \@fcbp, \@covariation, @entropy, @conservation
my @mis = split(//,$mis);
my @ss_cons = split(//,$ss_cons);

readstats($statsfile, \@fcbp, \@covariation);   # read stats

my $entropy = $seed->Rfam::RfamAlign::compute_pos_entropy();
my @entropy = @$entropy;
my $conservation = $seed->Rfam::RfamAlign::compute_seq_conservation();
my @conservation = @$conservation;
my ($maxcmseq, $maxcmstruct, $minbitscore, $maxbitscore, $bitscores) = compute_bitscores();
my @bitscores = @$bitscores;



my $ii=0;
my ($maxent,$minent)=(0,0);
while ($ii<@mis){
    
    if ($mis[$ii] =~ m/\-/ && $ss_cons[$ii] =~ m/[\,\:\.\_\-]/ && $fcbp[$ii] eq "-1"){
	splice(@mis, $ii, 1);
	splice(@ss_cons, $ii, 1);
	splice(@fcbp, $ii, 1);
	splice(@covariation, $ii, 1);
	splice(@entropy, $ii, 1);
	splice(@conservation, $ii, 1);
	$ii--;
    }
    
    if ($maxent<$entropy[$ii]){
	$maxent=$entropy[$ii]
    }

    if ($minent>$entropy[$ii]){
	$minent=$entropy[$ii]
    }
    
    $ii++;
}

for( my $i=0; $i<scalar(@entropy); $i++){
    $entropy[$i] = ($entropy[$i]-$minent)/($maxent-$minent);
}

$maxent=sprintf("%0.3f",$maxent);
$minent=sprintf("%0.3f",$minent);

$mis = join('',@mis);
$ss_cons = join('',@ss_cons);

open RNAPLOT, " | RNAplot  --pre \'\'" 
    or die "FATAL: problem running RNAplot\n[$!]";
print RNAPLOT "$mis\n$ss_cons\n";
close RNAPLOT or die "FATAL: problem closing RNAplot pipe\n[$!]";

################################
#For updating rfam table:
open  VIENNA, "> SEED.vienna"; 
print VIENNA "$mis\n$ss_cons\n";
close VIENNA;

my %structHash = (
    reference_sequence  => $mis,
    reference_structure => $ss_cons,
    fcbp                => \@fcbp,
    cov                 => \@covariation,
    ent                 => \@entropy,
    cons                => \@conservation
    );

#my $json = new JSON;
#my $jsonStructHash   = $json->pretty->encode(\%structHash);
my $jsonStructHash = JSON->new->pretty(1)->encode(\%structHash);
#my $jsonStructHash   = to_json(\%structHash);
#$json->pretty(1);
#my $jsonStructHash = $json->encode(\%structHash);

open  JASON, "> SEED.json"; 
print JASON $jsonStructHash;
close JASON;

###############################

my $n = swallow_ss_ps( );    # read ss plot
makepsfile("rna_fcbp.ps",\@fcbp, 0,1,"Frac. canonical BPs");
makepsfile("rna_cov.ps",\@covariation, -2,2, "Covariation");
makepsfile("rna_ent.ps",\@entropy, $minent,$maxent,"Relative entropy");
makepsfile("rna_cons.ps",\@conservation, 0,1,"Sequence conservation");
#makesvgfile(\@fcbp, \@covariation);
#makesvgfile();

#Reset PS file globals:
$macro_seen= 0;
%mfe = ();        # hash of mfe pairs
@ss_ps = ('',''); # head and tail of the ss.ps file

$minbitscore=0.00;
for (my $jj=0; $jj<@bitscores; $jj++){
    $bitscores[$jj]=($bitscores[$jj]-$minbitscore)/($maxbitscore-$minbitscore);
}

$minbitscore=sprintf("%0.3f",$minbitscore);
$maxbitscore=sprintf("%0.3f",$maxbitscore);

open RNAPLOT, " | RNAplot  --pre \'\'" 
    or die "FATAL: problem running RNAplot\n[$!]";
print RNAPLOT "$maxcmseq\n$maxcmstruct\n";
close RNAPLOT or die "FATAL: problem closing RNAplot pipe\n[$!]";

$n = swallow_ss_ps( );    # read ss plot
makepsfile("rna_maxcm.ps",\@bitscores, $minbitscore, $maxbitscore,"Max. CM parse");


#ls -1 *ps | perl -lane '$pre = "system(\42convert -density 600 -geometry 75\%  $F[0]\47\[0\]\47 "; $pos = " -resample 200 -trim +repage -bordercolor white -border 3  "; $b = $F[0]; $b =~ s/\.ps/\.png/; print "$pre $pos $b\42);";'

#convert -density 600 -geometry 400  rna_cons.ps'[0]' rna_cons.png
system("convert -density 600 -geometry 400  rna_cons.ps'[0]' rna_cons.png") and warn "WARNING: failed to convert rna_cons.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_cov.ps'[0]'    rna_cov.png") and warn "WARNING: failed to convert rna_cov.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_ent.ps'[0]'    rna_ent.png") and warn "WARNING: failed to convert rna_ent.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_fcbp.ps'[0]'    rna_fcbp.png") and warn "WARNING: failed to convert rna_fcbp.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_maxcm.ps'[0]'  rna_maxcm.png") and warn "WARNING: failed to convert rna_maxcm.ps to a stupid non-vector format";





exit();

######################################################################

sub makesvgfile {
    
#SVG work:
    
##############################################################################
# VARIBALES THAT STORE DATA
##############################################################################

    my @IDS = ( );
    my @X = ( );
    my @Y = ( );
    my @SEQUENCE = ( );
    my @PARTNER = ( );
    
##############################################################################
# SOME CONSTANT SVG AND JAVASCRIPT
##############################################################################
    
    my $menue = '<g transform="translate(10 550)" font-family="Arial,Helvetica">
     <text font-size="12px" x="5" y="0" fill="dimgray" font-weight="bold">Other display options</text>

     <circle cx="10" cy="15" r="5" stroke-width="1" stroke="rgb(100,100,100)" fill="white" onclick="click(\'COV\')" />
     <circle cx="10" cy="15" r="3" id="rbCOV"  style="fill:black;visibility:visible;" onclick="click(\'COV\')" />
     <text x="20" y="20" font-size="12px" fill="dimgray" onclick="click(\'COV\')"> Covariation </text> 

     <circle cx="10" cy="35" r="5" stroke-width="1" stroke="rgb(100,100,100)" fill="white" onclick="click(\'FCBP\')" />
     <circle cx="10" cy="35" r="3" id="rbFCBP"  style="fill:black;visibility:visible;" onclick="click(\'FCBP\')" />
     <text x="20" y="40" font-size="12px" fill="dimgray" onclick="click(\'FCBP\')"> Frac. canonical BPs </text> 

     <circle cx="10" cy="55" r="5" stroke-width="1" stroke="rgb(100,100,100)" fill="white" onclick="click(\'ENTROPY\')" />
     <circle cx="10" cy="55" r="3" id="rbENTROPY"  style="fill:black;visibility:visible;" onclick="click(\'ENTROPY\')" />
     <text x="20" y="60" font-size="12px" fill="dimgray" onclick="click(\'ENTROPY\')"> Relative entropy </text> 

     <circle cx="10" cy="75" r="5" stroke-width="1" stroke="rgb(100,100,100)" fill="white" onclick="click(\'CONSERVATION\')" />
     <circle cx="10" cy="75" r="3" id="rbCONSERVATION"  style="fill:black;visibility:visible;" onclick="click(\'CONSERVATION\')" />
     <text x="20" y="80" font-size="12px" fill="dimgray" onclick="click(\'CONSERVATION\')"> Sequence conservation </text> 

     <circle cx="10" cy="95" r="5" stroke-width="1" stroke="rgb(100,100,100)" fill="white" onclick="click(\'NONE\')" />
     <circle cx="10" cy="95" r="3" id="rbNONE"  style="fill:black;visibility:hidden;" onclick="click(\'NONE\')" />
     <text x="20" y="100" font-size="12px" fill="dimgray" onclick="click(\'NONE\')"> Nothing </text>
</g>';

    
    my $click =  'function click(choice) {
   var radiobutton1 = (SVGDocument.getElementById("rbCOV"))  ? SVGDocument.getElementById("rbCOV")  : SVGDocument.getElementById("dummycircle");
   var radiobutton2 = (SVGDocument.getElementById("rbFCBP")) ? SVGDocument.getElementById("rbFCBP") : SVGDocument.getElementById("dummycircle");
   var radiobutton3 = (SVGDocument.getElementById("rbENTROPY")) ? SVGDocument.getElementById("rbENTROPY") : SVGDocument.getElementById("dummycircle");
   var radiobutton4 = (SVGDocument.getElementById("rbCONSERVATION")) ? SVGDocument.getElementById("rbCONSERVATION") : SVGDocument.getElementById("dummycircle");
   var radiobutton5 = (SVGDocument.getElementById("rbNONE")) ? SVGDocument.getElementById("rbNONE") : SVGDocument.getElementById("dummycircle");


   var legend1 = (SVGDocument.getElementById("legend_cov")) ? SVGDocument.getElementById("legend_cov") : SVGDocument.getElementById("dummycircle");
   var legend2 = (SVGDocument.getElementById("legend_fcbp")) ? SVGDocument.getElementById("legend_fcbp") : SVGDocument.getElementById("dummycircle");
   var legend3 = (SVGDocument.getElementById("legend_entropy")) ? SVGDocument.getElementById("legend_entropy") : SVGDocument.getElementById("dummycircle");
   var legend4 = (SVGDocument.getElementById("legend_conservation")) ? SVGDocument.getElementById("legend_conservation") : SVGDocument.getElementById("dummycircle");

   if (choice == "COV") {
      radiobutton1.setAttribute("style", "visibility: visible");
      radiobutton2.setAttribute("style", "visibility: hidden");
      radiobutton3.setAttribute("style", "visibility: hidden");
      radiobutton4.setAttribute("style", "visibility: hidden");
      radiobutton5.setAttribute("style", "visibility: hidden");
      legend1.setAttribute("style", "visibility: visible");
      legend2.setAttribute("style", "visibility: hidden");
      legend3.setAttribute("style", "visibility: hidden");
      legend4.setAttribute("style", "visibility: hidden");

      for (i = 0; i < IDS.length; i++){
          var tmp = SVGDocument.getElementById("c"+IDS[i]);
          tmp.setAttribute("style", "fill:"+COV[i]);
          
      }
      var seq = SVGDocument.getElementById("seq");
      seq.setAttribute("style", "visibility: hidden");
      seq.setAttribute("style", "visibility: visible");
   }

   if (choice == "FCBP") {
      radiobutton1.setAttribute("style", "visibility: hidden");
      radiobutton2.setAttribute("style", "visibility: visible");
      radiobutton3.setAttribute("style", "visibility: hidden");
      radiobutton4.setAttribute("style", "visibility: hidden");
      radiobutton5.setAttribute("style", "visibility: hidden");
      legend1.setAttribute("style", "visibility: hidden");
      legend2.setAttribute("style", "visibility: visible");
      legend3.setAttribute("style", "visibility: hidden");
      legend4.setAttribute("style", "visibility: hidden");

      for (i = 0; i < IDS.length; i++){
          var tmp = SVGDocument.getElementById("c"+IDS[i]);
          tmp.setAttribute("style", "fill:"+FCBP[i]);
          
      }
      var seq = SVGDocument.getElementById("seq");
      seq.setAttribute("style", "visibility: hidden");
      seq.setAttribute("style", "visibility: visible");
   }

   if (choice == "ENTROPY") {
      radiobutton1.setAttribute("style", "visibility: hidden");
      radiobutton2.setAttribute("style", "visibility: hidden");
      radiobutton3.setAttribute("style", "visibility: visible");
      radiobutton4.setAttribute("style", "visibility: hidden");
      radiobutton5.setAttribute("style", "visibility: hidden");
      legend1.setAttribute("style", "visibility: hidden");
      legend2.setAttribute("style", "visibility: hidden");
      legend3.setAttribute("style", "visibility: visible");
      legend4.setAttribute("style", "visibility: hidden");

      for (i = 0; i < IDS.length; i++){
          var tmp = SVGDocument.getElementById("c"+IDS[i]);
          tmp.setAttribute("style", "fill:"+ENTROPY[i]);
          
      }
      var seq = SVGDocument.getElementById("seq");
      seq.setAttribute("style", "visibility: hidden");
      seq.setAttribute("style", "visibility: visible");
   }

   if (choice == "CONSERVATION") {
      radiobutton1.setAttribute("style", "visibility: hidden");
      radiobutton2.setAttribute("style", "visibility: hidden");
      radiobutton3.setAttribute("style", "visibility: hidden");
      radiobutton4.setAttribute("style", "visibility: visible");
      radiobutton5.setAttribute("style", "visibility: hidden");
      legend1.setAttribute("style", "visibility: hidden");
      legend2.setAttribute("style", "visibility: hidden");
      legend3.setAttribute("style", "visibility: hidden");
      legend4.setAttribute("style", "visibility: visible");

      for (i = 0; i < IDS.length; i++){
          var tmp = SVGDocument.getElementById("c"+IDS[i]);
          tmp.setAttribute("style", "fill:"+CONSERVATION[i]);
          
      }
      var seq = SVGDocument.getElementById("seq");
      seq.setAttribute("style", "visibility: hidden");
      seq.setAttribute("style", "visibility: visible");
   }

   if (choice == "NONE") {
      radiobutton1.setAttribute("style", "visibility: hidden");
      radiobutton2.setAttribute("style", "visibility: hidden");
      radiobutton3.setAttribute("style", "visibility: hidden");
      radiobutton4.setAttribute("style", "visibility: hidden");
      radiobutton5.setAttribute("style", "visibility: visible");
      legend1.setAttribute("style", "visibility: hidden");
      legend2.setAttribute("style", "visibility: hidden");
      legend3.setAttribute("style", "visibility: hidden");
      legend4.setAttribute("style", "visibility: hidden");
  
      for (i = 0; i < IDS.length; i++){
          var tmp = SVGDocument.getElementById("c"+IDS[i]);
          tmp.setAttribute("style", "fill:#ffffff");
      }
      var seq = SVGDocument.getElementById("seq");
      seq.setAttribute("style", "visibility: hidden");
      seq.setAttribute("style", "visibility: visible");
   }
}';
    
##############################################################################
# READ RNA STRUCTURE FILE
##############################################################################
    
    open(IN,'<rna.ps') || die "Can not open file rna.ps: $!";
    my ($i,$j) = (0,0);
    my $sequence = '';
    my @COV = ( );
    my @FCBP = ( );
    my @ENTROPY = ( );
    my @CONSERVATION = ( );
    
    while(<IN>)
    {
    #chomp;
	$sequence .= $1 if (m/(.*)(\\$)/);
	
	if (m/(\[)(-?\d+\.\d+)\s(-?\d+\.\d+)(\])/)
	{
	    $IDS[$i] = $i;
	    $X[$i] = $2;
	    $Y[$i] = $3;
	    $i++;
	}
	elsif(m/(\[)(\d+)\s(\d+)(\])/)
	{
	    $PARTNER[$2-1] = $3;
	}
	elsif (m/\s\s(\S+)\n/){
	    
	    $COV[$j++]         =$1;
	    $FCBP[$j++]        =$1;
	    $ENTROPY[$j++]     =$1;
	    $CONSERVATION[$j++]=$1;
	    
	}
	
    }
    close(IN);
    $sequence =~ s/(\/sequence\s\()(.*)/$2/;	
    @SEQUENCE = split (//, $sequence);	

    for my $i (0 .. $#IDS)
    {
	$PARTNER[$i] = -1 if (!$PARTNER[$i]);
    }

##############################################################################
# HERE IMPLEMENT THE COLOR CODINGS YOU WANT TO HAVE
##############################################################################
    my @colours = ("#0050dc", "#0064dc", "#0078dc", "#008cdc", "#00a0dc", "#00b4dc", "#00c8dc", "#00dcdc", "#00dcc8", "#00dcb4", "#00dca0", "#00dc8c", "#00dc78", "#00dc64", "#00dc50", "#00dc3c", "#00dc28", "#00dc14", "#00dc00", "#14dc00", "#28dc00", "#3cdc00", "#50dc00", "#64dc00", "#78dc00", "#8cdc00", "#a0dc00", "#b4dc00", "#c8dc00", "#dcdc00", "#dcc800", "#dcb400", "#dca000", "#dc8c00", "#dc7800", "#dc6400", "#dc5000", "#dc3c00", "#dc2800", "#dc1400");

    my $legendbod = "";
    my $legendcov = "
<g id=\"legend_cov\" transform=\"translate(230 550)\" font-family=\"Arial,Helvetica\">
<text x=\"0\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">-2</text>\n";
    my $legendfcbp = "
<g id=\"legend_fcbp\" transform=\"translate(230 550)\" font-family=\"Arial,Helvetica\">
<text x=\"0\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">0</text>\n";
    
    my $legendentropy = "
<g id=\"legend_entropy\" transform=\"translate(230 550)\" font-family=\"Arial,Helvetica\">
<text x=\"0\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">$minent</text>\n";
    
    my $legendconservation = "
<g id=\"legend_conservation\" transform=\"translate(230 550)\" font-family=\"Arial,Helvetica\">
<text x=\"0\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">0</text>\n";
    

    my $ii=0;
    foreach my $col (@colours){
	$legendbod .= "<rect style=\"stroke: $col; stroke-width:0;fill: $col\" height=\"10\" x=\"$ii\" y=\"30\" width=\"2\" />\n";
	$ii += 2;
    }
    $legendcov .= $legendbod;
    $legendfcbp .= $legendbod;
    $legendentropy .= $legendbod;
    $legendconservation .= $legendbod;
    
    $legendcov .= "<text x=\"73\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">2</text>
</g>\n";
    $legendfcbp .= "<text x=\"73\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">1</text>
</g>\n";
    $legendentropy .= "<text x=\"73\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">$maxent</text>
</g>\n";
    $legendconservation .= "<text x=\"73\" y=\"28\" font-size=\"12px\" fill=\"dimgray\">1</text>
</g>\n";
    
# assign everywhere the same color
    for my $i (0 .. $#IDS)
    {
	if ($covariation[$i]<0){
	    $COV[$i] = "#ffffff";
	}
	else {
	    my $j = int($covariation[$i] * (scalar(@colours)-1));
	    $COV[$i] = $colours[$j];
	}
	
	if ($fcbp[$i]<0){
	    $FCBP[$i] = "#ffffff";
	}
	else {
	    $j = int($fcbp[$i] * (scalar(@colours)-1));
	    $FCBP[$i] = $colours[$j];
	}

	if ($entropy[$i]<0){
	    $ENTROPY[$i] = "#ffffff";
	}
	else {
	    $j = int($entropy[$i] * (scalar(@colours)-1));
	    $ENTROPY[$i] = $colours[$j];
	}

	if ($conservation[$i]<0){
	    $CONSERVATION[$i] = "#ffffff";
	}
	else {
	    $j = int($conservation[$i] * (scalar(@colours)-1));
	    $CONSERVATION[$i] = $colours[$j];
	}

    }
    
    
##############################################################################
# PRINT SVG
##############################################################################
#open( SD, "$alnfile" ) or die ("FATAL: Couldn't open $alnfile [$!]\n $!\n");
    
    open(RNASVG, ">rna.svg") or die ("FATAL: Couldn't open rna.svg\n [$!]");
    
# HEADER
    my $header = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<svg xmlns="http://www.w3.org/2000/svg" height="650" width="452" onload=\'Init(evt)\'>';
    print RNASVG $header;
    
# JAVASCRIPT
    print RNASVG '<script type="text/ecmascript">',"\n",'<![CDATA[',"\n";
    print RNASVG "IDS = new Array('",join("','",@IDS),"');\n";
    print RNASVG "COV = new Array('",join("','",@COV),"');\n";
    print RNASVG "FCBP = new Array('",join("','",@FCBP),"');\n";
    print RNASVG "ENTROPY = new Array('",join("','",@ENTROPY),"');\n";
    print RNASVG "CONSERVATION = new Array('",join("','",@CONSERVATION),"');\n";
    print RNASVG "var SVGDocument = null;\nvar SVGRoot = null;\n";
    print RNASVG 'function Init(evt) { SVGDocument = evt.target.ownerDocument;',"\n";
    print RNASVG 'SVGRoot = SVGDocument.documentElement;click("NONE"); }',"\n";
    
    print RNASVG $click;
    
    print RNASVG ']]>',"\n",'</script>',"\n";
    print RNASVG '<circle id="dummycircle" cx="1" cy="1" r="1" stroke-width="0" fill="white" />',"\n";
    print RNASVG $menue;
    print RNASVG $legendcov;
    print RNASVG $legendfcbp;
    print RNASVG $legendentropy;
    print RNASVG $legendconservation;


# SCALE AND TRANSFORM (center the structure)
    my $minx = (min @X);
    my $miny = (min @Y);
    my $maxx = (max @X);
    my $maxy = (max @Y);
    my $xspan = $maxx - $minx;
    my $yspan = $maxy - $miny;

    foreach my $i (0 .. $#IDS)
    {
	$Y[$i] = ($Y[$i]-$miny)*-1+$yspan+8;
	$X[$i] = $X[$i]-$minx +8;
    }
    my $divisor = max $xspan, $yspan;
    $divisor += 16;
    
    my $scale = 450/$divisor;
    print RNASVG '<g transform="scale(',$scale,',',$scale,') translate(',0,',',((452-$yspan*$scale)/2)/$scale,')">',"\n" if ($xspan > $yspan);
    print RNASVG '<g transform="scale(',$scale,',',$scale,') translate(',((452-$xspan*$scale)/2)/$scale,',',0,')">',"\n" if ($xspan <= $yspan);
    
    
# DRAW THE CIRCLES (INITIALIZE TO WHITE)
    foreach my $i (0 .. $#IDS)
    {
	print RNASVG '<circle id="c'.$IDS[$i].'" cx="'.$X[$i].'" cy="'.$Y[$i].'" r="8" style="fill:#ffffff" stroke="black" stroke-width="0"/>'."\n";
    }
    
# PRINT BACKBONE
    print RNASVG '<polyline style="stroke: black; fill: none; stroke-width: 1.5" id="outline" points="';
    foreach my $i (0 .. $#IDS)
    {
	print RNASVG $X[$i],",",$Y[$i],"\n";
    }
    print RNASVG '" />',"\n";
    
# PRINT PAIRS
    print RNASVG '<g style="stroke: black; stroke-width: 1" id="pairs">';
    foreach my $i (0 .. $#IDS)
    {
	if ($PARTNER[$i] > 0)
	{
	    my $j = $PARTNER[$i]-1;
	    print RNASVG '<line id="',$i,',',$j,'" x1="',$X[$i],
	    '" y1="',$Y[$i],'" x2="',$X[$j],'" y2="',$Y[$j],'" />';
	}
    }
    print RNASVG "</g>\n";
    
# PRINT SEQUENCE
    print RNASVG '<g style="font-family: SansSerif" transform="translate(-6, 6)" id="seq">';
    foreach my $i (0 .. $#IDS)
    {
	print RNASVG '<text id="t',$IDS[$i],'" x="',$X[$i],'" y="',$Y[$i],'" fill="black">',$SEQUENCE[$i],'</text>',"\n";
    }
    print RNASVG "</g>\n";
    
    print RNASVG "</g>\n";
    print RNASVG "</svg>\n";

    close(RNASVG);

#    compute_bitscores();

    
}

######################################################################

sub readstats {
    
    my $statsfile = shift;
    #my $cov  =  shift;
    #my $fcbp = shift;
    #my @cov  = @$cov;
    #my @fcbp = @$fcbp;
    
    #print "$statsfile\n";
    open STATS, "<$statsfile" or die "FATAL: could not open $statsfile\n[$!]";
    
    #@ss_cons
    my %isComplementary = (
	'(' => ')',
	'<' => '>',
	'[' => ']',
	'{' => '}',
	')' => '(',
	'>' => '<',
	']' => '[',
	'}' => '{',
	);
    
    while (my $l = <STATS>){
	next if /^FAMILY/;
	
	if ($l =~ /\S+\s+(\d+)\:(\d+)\s+(\S+)\s+(\S+)/){
	    print "RFAMbling ERROR: coords out of bounds in [$l]\n" if not defined $fcbp[$2-1];
	    print "RFAMbling ERROR: ss-stats-perbasepair and SS_cons basepairs are inconsistent!
ss-stats-perbasepair: $1:$2
             SS_cons: " .  $ss_cons[$1-1] . ":" . $ss_cons[$2-1] . "\n"
		if not defined $isComplementary{$ss_cons[$1-1]} 
	        or not defined $isComplementary{$ss_cons[$2-1]}  
	        or $ss_cons[$1-1] ne $isComplementary{$ss_cons[$2-1]};
	    $fcbp[$1-1]=$3;
	    $fcbp[$2-1]=$3;
	    $covariation[$1-1] =($4+2)/4;
	    $covariation[$2-1] =($4+2)/4;
	    #print "$1\t$2\t$3\n";
	}
    }
    close(STATS);
    return 1;
}
######################################################################

sub makepsfile {

my ($psfile, $refstats, $min, $max, $title) = @_;    
#makepsfile("rna_fcbp.ps",\@fcbp, 0,1);
my @stats = @$refstats;

open PSFILE, ">$psfile" or die "FATAL: could not open $psfile\n[$!]";
print PSFILE $ss_ps[0];     # print head
if (!$macro_seen) {
  print PSFILE <<_E_O_F_
/range 0.8 def
/drawreliability {
 /Smax 1 def
 0
 coor {
   aload pop
   S 3 index get
   dup 0 lt {
       pop pop pop
   }
   {
       Smax div range mul
       invert {range exch sub} if
       1 1 sethsbcolor
       newpath
       fsize 2 div 0 360 arc
       fill
   } ifelse
   1 add
 } forall
} bind def
/colorbar { % xloc yloc colorbar -> []
  /STR 8 string def
  gsave
    xmin xmax add size sub 2 div
    ymin ymax add size sub 2 div translate
    size dup scale
    translate
    0.015 dup scale
    /tics 64 def
    gsave
      10 tics div 1 scale
      0 1 tics
      {
          dup 0 moveto 0.5 add
          tics div range mul
          invert {range exch sub} if
          1 1 sethsbcolor
          1 0 rlineto 0 1 rlineto -1 0 rlineto closepath fill
      } for
    grestore
    0 setgray
    -0.1 1.20 moveto ($min) gsave 0.1 dup scale show grestore
    -0.1 -1.20 moveto ($title) gsave 0.1 dup scale show grestore
    10 1.20 moveto ($max) STR cvs
    gsave 0.1 dup scale dup stringwidth pop -2 div 0 rmoveto show grestore
  grestore
} bind def
_E_O_F_
}
print PSFILE "/S [\n";
foreach my $stat (@stats) {
    $stat = 0 if not defined $stat;
    printf PSFILE "  %7.5f\n", $stat;
}
print PSFILE "] def\n\n";
print PSFILE "/invert ", 'true', " def\n";
print PSFILE "drawreliability\n";
print PSFILE "0.1 -0.075 colorbar\n";
print PSFILE $ss_ps[1];  # print tail
close(PSFILE);
    
    return 1;
}

######################################################################

sub swallow_ss_ps {

    open SS, "<rna.ps"
    or die "FATAL: problem opening rna.ps\n[$!]";

#Change from:
#%%BoundingBox: 66 210 518 662
#To:
#%%BoundingBox: 66 170 518 682

#This elsewhere:
#drawreliability
#0.1 -0.075 colorbar

#
#% Start Annotations
#90 130 moveto (5') show
#117.989 125 moveto (3\42) show
#% End Annotations


  # read the secondary structure plot
    my $length=0;
    my $tail=0;
    my $seenCoor=0;
    my (@coord5p, @coord3p);
    my $prevLine = '';
    while (<SS>) {
	$macro_seen=1 if /drawreliability /;
	$length ++ if /^\/coor/ .. /^\] def/;
	if (/^\/pairs/ .. /^\] def/) {
	    $mfe{$1,$2}=1 if /(\d+)\s+(\d+)/;
	}
	$tail=1 if /^drawoutline/;
	$_ = "%%BoundingBox: 66 170 518 690\n" if /BoundingBox/;
	$ss_ps[$tail] .= $_;
	$seenCoor = 1 if /^\/coor \[/;
	$seenCoor = 0 if /^\] def/ && $seenCoor;
	if($seenCoor && /\[(\S+) (\S+)\]/){
	    @coord5p = ($1, $2) if not @coord5p;
	    @coord3p = ($1, $2);
	}
	
	if ($prevLine =~ /Start Annotations/){
	    
	    my $y5p = $coord5p[1]+10;
	    my $y3p = $coord3p[1]+10;
	    
	    #print "[" . $coord5p[0] . "] [" . $y5p . "] moveto (5\') show\n";
	    #print "[" . $coord3p[0] . "] [" . $y3p . "] moveto (3\') show\n";
	    $ss_ps[$tail] .= $coord5p[0] . " " . $y5p . " moveto (5\') gsave 0.5 dup scale show grestore\n";
	    $ss_ps[$tail] .= $coord3p[0] . " " . $y3p . " moveto (3\') gsave 0.5 dup scale show grestore\n";
	}
	
	last if eof;
	$prevLine = $_;
    }
    return $length-2;
}

######################################################################
sub compute_bitscores {
    my $stkfile;
    if (@ARGV){
	$stkfile = shift;
    }
    else {
	$stkfile="SEED";
    }
    
    system("$Rfam::infernal_path/cmbuild --emap CM.10.emap -F CM.10 $alnfile") and die("FATAL: Error in: [$Rfam::infernal_path/cmbuild --emap CM.10.emap -F CM.10 $alnfile].\n");
    
    #SLURP emap file
    my (%emap, %lemap, %remap); #mapping from CM node number to alignment/string
		 #coordinates
    my $CMlength;
    open(CMemap,'<CM.10.emap') || die "Can not open file CM.10.emap: $!";
    while (my $l = <CMemap>){
	
	if ($l =~ /\s+(\d+)\s+(\S+)\s+(\d+)\s+(\d+)\s+(\d+)/){
	    my ($Node, $Node_type, $lpos, $rpos, $epos) = ($1,$2,$3,$4,$5);
	    
	    if (defined($Node) && defined($Node_type)  && defined($lpos)  && defined($rpos)  && defined($epos) ){
		
		if ($Node_type eq "MATL"){
		    $emap{$Node} = $lpos;
		}
		elsif ($Node_type eq "MATR"){
		    $emap{$Node} = $rpos;
		}
		elsif ($Node_type eq "MATP"){
		    $emap{$Node} = ($lpos,$rpos);
		    ($lemap{$Node}, $remap{$Node}) = ($lpos,$rpos);
		}
		elsif ($Node_type eq "ROOT"){
		    $CMlength = $rpos-1;
		}
	    }
	}
	
    }
    
    close(CMemap);
    
    #SLURP CM file
    open(CM,'<CM.10') || die "Can not open file CM.10: $!";
    
    my %CMstates = (
	MATL => "ML",
	MATR => "MR",
	MATP => "MP"
	);
    my %CMstatenum = (
	MATL => 4,
	MATR => 4,
	MATP => 16
	);
    my %CMalpha = (
	MATL => [qw(A C G U)],
	MATR => [qw(A C G U)],
	MATP => [qw(AA AC AG AU CA CC CG CU GA GC GG GU UA UC UG UU)]
	);
    my ($statenum, @maxcmparsestr, @maxcmparsestruct, @maxcmparsescore); 
    my $state = "";
    
    
    while (my $l = <CM>){
	
	if (defined($l) && $l =~ /\s+\[\s+(\S+)\s+(\d+)\s+\]/){

	    ($state, $statenum)=($1,$2);
	}
	elsif (defined($CMstates{$state}) && $l =~ /^\s+$CMstates{$state}/){
	    
	    my @score = split(/\s+/, $l);
	    my ($maxscore, $maxstr) = (0,"");
	    for (my $i=0; $i<$CMstatenum{$state}; $i++){
		
		my $score = pop(@score); 
		if ($score>$maxscore){
		    $maxscore=$score;
		    $maxstr  =${ $CMalpha{$state} }[$CMstatenum{$state}-$i-1];
		    
		}
		
	    }
	    
	    if ($state eq "MATP" && defined($emap{$statenum})){
		
		    my @chars = split(//,$maxstr);
		    my $lposn = $lemap{$statenum};
		    my $rposn = $remap{$statenum};
		    #print "$statenum\t$maxstr\t$maxscore\t$lposn\t$rposn\n";
		    $maxcmparsestr[$lposn]=$chars[0];
		    $maxcmparsestr[$rposn]=$chars[1];
		    $maxcmparsestruct[$lposn]="(";
		    $maxcmparsestruct[$rposn]=")";
		    $maxcmparsescore[$lposn]=$maxscore;
		    $maxcmparsescore[$rposn]=$maxscore;
		
	    }
	    else {
		if (defined($emap{$statenum}) && defined($maxstr) && defined($maxscore)){
		    my $posn = $emap{$statenum};
		    $maxcmparsestr[$posn]=$maxstr;
		    $maxcmparsestruct[$posn]=".";
		    $maxcmparsescore[$posn]=$maxscore;
		    #print "$statenum\t$maxstr\t$maxscore\t$emap{$statenum}\n";
		}
	    }
	    
	}
    }
    close(CM);
    
    my ($minbitscore, $maxbitscore, $ii) = (999,0,0);
    
    while ($ii<@maxcmparsestr){
    
	if ( !defined($maxcmparsestr[$ii]) || !defined($maxcmparsestruct[$ii]) || !defined($maxcmparsescore[$ii]) ){
	    splice(@maxcmparsestr, $ii, 1);
	    splice(@maxcmparsestruct, $ii, 1);
	    splice(@maxcmparsescore, $ii, 1);
	    $ii--;
	}
	elsif ($minbitscore>$maxcmparsescore[$ii]) {
	    $minbitscore=$maxcmparsescore[$ii];
	}
	elsif ($maxbitscore<$maxcmparsescore[$ii]) {
	    $maxbitscore=$maxcmparsescore[$ii];
	}
	
	$ii++;
    }
    
    my $maxcmparsestr = join('',@maxcmparsestr);
    my $maxcmparsestruct = join('',@maxcmparsestruct);
    #print "$maxcmparsestr\n$maxcmparsestruct\n";
    
    return ($maxcmparsestr, $maxcmparsestruct, $minbitscore, $maxbitscore, \@maxcmparsescore);    
#$maxcmseq, $maxcmstruct, $bitscores    
    
}

######################################################################

sub help {
    print STDERR <<EOF;

RFAMbling.pl: produce pretty graphics for an Rfam family
This code is largely borrowed from Ivo Hofacker & Andreas Gruber\47s SVG script.

Usage:   RFAMbling.pl <options>
Options:       -h|-help               show this help
               -f|-a|-alnfile <str>   specify an alignment file (default: SEED)  
	       -s|-statsfile <str>    specify a statistics file, produced by rqc-ss-cons.pl 
	                              (default: ss-stats-perbasepair)
TO ADD:

5\47 and 3\47 labels to ss plots. 
-should be in a smaller font...	
-add checks that stats are current
-Make a more general function that takes a tab-delimited file as input with headers, makes appropriate files on demand...

--Try adding 5\' and 3\' annotations and shift legends on the structure diagrams. 

EOF
}
