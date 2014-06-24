#!/usr/bin/env perl 
#
# create-ss-graphics.pl: create secondary structure graphics given a SEED and CM
# 
use strict;
use Getopt::Long;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use JSON;
use List::Util qw(max min); # used only by Paul's code in makesvgfile() which is currently not called

######################################
# NOTES TO JOHN AND/OR ROB:
# 
# I've replaced all the code up to line 133 of the original RFAMbling.pln
# (https://xfamsvn.ebi.ac.uk/svn/code/branches/jfrc-rfam_20121001/Rfam/CVSimport/make/RFAMbling.pl)
# And slightly modified the rest of the script, by modifying variable
# names and reordering some code blocks to make it easier to follow in
# my opinion.
#
# The script now relies on the new modules and not on the old ones.
#
# Organization of code:
# Main script:
# 1. Collect info/stats required for the SS graphics (rewritten by me).
# 2. Paul's POSTSCRIPT block: creates postscript diagrams and converts
#    them to PNG.
# 3. Paul's VIENNA block: creates 'SEED.vienna' file (for Rfam table?)
# 4. Paul's JSON block: creates 'SEED.json' not sure where/if this is used later
#
# SUBROUTINES:
# 1. cm_consensus_sequence_stats(): written by me, to get info for 'max CM parse' graphic
# 2. help(): outputs usage info.
# 3. Paul's makepsfile():    creates postscript files, called by main script, 
# 4. Paul's swallow_ss_ps(): helps create postscript files, called by main script
# 5. Paul's makesvgfile():   NOT called from main script, not sure why
# 
# I did not change subroutines 3-5 from the original RFAMbling.pl.
#
# - Eric 
############################################


###################################################################
# Preliminaries:
# - set default values that command line options may change
# - process command line options

my $in_alifile = "SEED"; # name of seed alignment
my $in_cmfile  = "CM";   # name of CM file

my $do_help = 0;
my $exec_description = "create secondary structure graphics for a family.";

my $options_okay = 
    &GetOptions( "ali=s"        => \$in_alifile,
                 "cm=s"         => \$in_cmfile, 
                 "h|help"       => \$do_help );

if(! $options_okay) { 
  &help($exec_description); 
  die "ERROR, unrecognized option;"; 
}

if(! -e $in_alifile) { $do_help = 1; }
if(! -e $in_cmfile)  { $do_help = 1; }

if ( $do_help ) {
  &help($exec_description);
  exit(1);
}

########################
# Setup and validation #
########################
my $io     = Bio::Rfam::FamilyIO->new;
my $config = Bio::Rfam::Config->new;

# open and validate file 
my $msa = Bio::Easel::MSA->new({
  fileLocation => $in_alifile 
});
my $cm = $io->parseCM($in_cmfile);

# verify SEED and CM have matching CKSUMs
if($cm->cmHeader->{cksum} != $msa->checksum) { die "ERROR, $in_cmfile was not built from $in_alifile (checksum mismatch)" };

#######################################
# Collect info and create SS graphics #
#######################################
# remove RF gap positions from SEED
$msa->remove_rf_gap_columns();

# collect info/stats we need, (all will be consensus length of the CM b/c we removed RF gaps):
my $ss_cons       = $msa->get_ss_cons_dot_parantheses;       # consensus secondary structure string
my $mis           = $msa->most_informative_sequence(0.0001); # most-informative-sequence (Freyhult, Moulton, and Gardner, 2005)
my @fcbpA         = $msa->pos_fcbp();                        # fraction canonical basepairs per position (see Bio-Easel::MSA function for details)
my @covariationA  = $msa->pos_covariation();                 # covariation statistic per position (see Bio-Easel::MSA function for details)
my @entropyA      = $msa->pos_entropy();                     # entropy per position (see Bio-Easel::MSA function for details)
my @conservationA = $msa->pos_conservation();                # conservation per position (see Bio-Easel::MSA function for details)

# one last thing: get maximum emission score per position and maximum scoring sequence to CM
my $clen = $cm->cmHeader->{clen}; # consensus length of the CM
my $cpos;                         # counter over consensus positions
my @bitscoresA  = ();             # emission score per position (impt: pair scores are evenly split between left and right position)
my @cseqA = ();                   # consensus sequence, the maximally scoring sequence to the CM
get_consensus_sequence_score($config, $in_cmfile, $clen, \@bitscoresA, \@cseqA);
# create the consensus sequence by concatenating the elements of the cseqA array
my $cseq = ""; 
foreach my $cres (@cseqA) { $cseq .= $cres; }

# We want our values in the range 0.0 to 1.0 for the coloring in the 
# graphics. So we divide each value by the full range (max-min).

# fcbpA:         range will be 0..1, and it already is so we do nothing

# conservationA: range will be 0..1, and it already is so we do nothing

# entropyA:      range will be min..max: 
my $maxent = Bio::Rfam::Utils::maxArray(\@entropyA, $clen);
my $minent = Bio::Rfam::Utils::minArray(\@entropyA, $clen);
for($cpos = 0; $cpos < $clen; $cpos++) { 
  $entropyA[$cpos] = ($entropyA[$cpos] - $minent) / ($maxent - $minent);
}

# covariationA: range will be -2 to 2:
for($cpos = 0; $cpos < $clen; $cpos++) { 
  $covariationA[$cpos] = ($covariationA[$cpos] - 2.) / 4.;
}

# bitscoresA: range will be 0..max: 
my $maxbitscore = Bio::Rfam::Utils::maxArray(\@bitscoresA, $clen);
for($cpos = 0; $cpos < $clen; $cpos++) { 
  if($bitscoresA[$cpos] < -0.000001) { die "ERROR negative bit score emission in consensus sequence"; }
  $bitscoresA[$cpos] /= $maxbitscore;
}

########################################
# BEGIN PAUL'S CODE, FROM RFAMbling.pl #
########################################

###############################
# Paul's POSTSCRIPT block
# Two stages: 
# 1. Create the fcbp, covariation, entropy and sequence conservation plots
#    using the most-informative-sequence ($mis) as the reference sequence
#    to be printed as the sequence on the SS graphic. 
# 2. Create the max CM parse plot using the CM's consensus sequence as
#    the reference sequence.

# Begin stage 1.
# Set PS file globals for stage 1:
my $macro_seen= 0;
my %mfe = ();        # hash of mfe pairs
my @ss_ps = ('',''); # head and tail of the ss.ps file

# print the mis and ss_cons to a file for RNAplot processing
open RNAPLOT, " | RNAplot  --pre \'\'" 
    or die "FATAL: problem running RNAplot\n[$!]";
print RNAPLOT "$mis\n$ss_cons\n";
close RNAPLOT or die "FATAL: problem closing RNAplot pipe\n[$!]";

# postscript files can't handle too much precision, so truncate these ranges:
$minent = sprintf("%0.3f", $minent);
$maxent = sprintf("%0.3f", $maxent);

my $n = swallow_ss_ps( );    # read ss plot

makepsfile("rna_fcbp.ps", \@fcbpA,         0,       1,       "Frac. canonical BPs");
makepsfile("rna_cov.ps",  \@covariationA, -2,       2,       "Covariation");
makepsfile("rna_ent.ps",  \@entropyA,      $minent, $maxent, "Relative entropy");
makepsfile("rna_cons.ps", \@conservationA, 0,       1,       "Sequence conservation");
#makesvgfile(\@fcbp, \@covariation);
#makesvgfile();

# Begin stage 2.

#Reset PS file globals for stage 2
$macro_seen= 0;
%mfe = ();        # hash of mfe pairs
@ss_ps = ('',''); # head and tail of the ss.ps file


# print the mis and ss_cons to a file for RNAplot processing
open RNAPLOT, " | RNAplot  --pre \'\'" 
    or die "FATAL: problem running RNAplot\n[$!]";
print RNAPLOT "$cseq\n$ss_cons\n";
close RNAPLOT or die "FATAL: problem closing RNAplot pipe\n[$!]";

# postscript files can't handle too much precision, so truncate these ranges:
$maxbitscore = sprintf("%0.3f", $maxbitscore);

$n = swallow_ss_ps( );    # read ss plot

makepsfile("rna_maxcm.ps",\@bitscoresA, "0.000", $maxbitscore,"Max. CM parse");

#ls -1 *ps | perl -lane '$pre = "system(\42convert -density 600 -geometry 75\%  $F[0]\47\[0\]\47 "; $pos = " -resample 200 -trim +repage -bordercolor white -border 3  "; $b = $F[0]; $b =~ s/\.ps/\.png/; print "$pre $pos $b\42);";'

# Convert all postscript files to png files
#convert -density 600 -geometry 400  rna_cons.ps'[0]' rna_cons.png
system("convert -density 600 -geometry 400  rna_cons.ps'[0]'  rna_cons.png")  and warn "WARNING: failed to convert rna_cons.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_cov.ps'[0]'   rna_cov.png")   and warn "WARNING: failed to convert rna_cov.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_ent.ps'[0]'   rna_ent.png")   and warn "WARNING: failed to convert rna_ent.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_fcbp.ps'[0]'  rna_fcbp.png")  and warn "WARNING: failed to convert rna_fcbp.ps to a stupid non-vector format";
system("convert -density 600 -geometry 400  rna_maxcm.ps'[0]' rna_maxcm.png") and warn "WARNING: failed to convert rna_maxcm.ps to a stupid non-vector format";

# END OF POSTSCRIPT BLOCK
##########################

################################
# VIENNA block
#For updating rfam table:
open  VIENNA, "> SEED.vienna"; 
print VIENNA "$mis\n$ss_cons\n";
close VIENNA;
################################

################################
# JSON block 
my %structHash = (
    reference_sequence  => $mis,
    reference_structure => $ss_cons,
    fcbp                => \@fcbpA,
    cov                 => \@covariationA,
    ent                 => \@entropyA,
    cons                => \@conservationA
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
################################

exit 0;

##################################
# END OF PAUL'S MAIN SCRIPT CODE #
##################################

###############
# SUBROUTINES #
###############
=head2 cm_consensus_sequence_stats

    Title    : cm_consensus_sequence_stats
    Incept   : EPN, Wed May 21 08:52:13 2014
    Usage    : Bio::Rfam::Infernal::cm_consensus_sequence_stats($config, $cmfile, $clen)
    Function : Given a CM file, use cmemit and cmalign to get stats on the 
             : models consensus sequence, and the highest emission scores 
             : at each consensus position of the model. This subroutine was
             : originally written to get data for a SS diagram that is hosted
             : on the Rfam website.
    Args     : $config:  Rfam config, with infernalPath
             : $cmfile:  path to a CM file
             : $clen:    consensus length of a CM (this will be the size of the return arrays)
             : $escAR:   ref to array, filled here [0..cpos..$clen-1]: max emission score for consensus position $cpos
             : $cseqAR:  ref to array, filled here [0..cpos..$clen-1]: max scoring residue for consensus position $cpos
    Returns  : void
    Dies     : if cmemit/cmalign command fails, or there is a problem parsing the trace file

=cut


sub get_consensus_sequence_score { 
  my ($config, $cmfile, $clen, $escAR, $cseqAR) = @_;

  my $cmemitPath = $config->infernalPath . "cmemit";
  my $cmalignPath = $config->infernalPath . "cmalign";
  my $out_tfile = "$$.tfile";

  # create the trace file we'll parse below
  Bio::Rfam::Utils::run_local_command("$cmemitPath -c $cmfile | cmalign --notrunc -g --tfile $out_tfile $cmfile - > /dev/null");

  # parse trace file
  # example lines:
  ## ..skip..
  ## idx   emitl  emitr   state  mode  nxtl  nxtr  prv   tsc   esc 
  ## ----- ------ ------ ------- ----- ----- ----- ----- ----- -----
  ##   0     1    101      0S  Joint     1    -1    -1 -0.11  0.00
  ##   1     1G   101      3ML Joint     2    -1     0 -0.05  1.43
  ## ..skip..
  ##   7     2G    96C    21MP Joint     8    -1     6 -0.02  2.83
  ## ..skip..
  ##  10     5C    93G    39MP Joint    11    -1     9 -0.02  2.63
  open(IN, $out_tfile) || die "ERROR unable to open $out_tfile for reading";

  @{$escAR}  = ();
  @{$cseqAR} = ();
  my $cpos; # counter over consensus positions
  # initialize arrays
  for($cpos = 0; $cpos < $clen; $cpos++) { 
    $escAR->[$cpos]  = "";
    $cseqAR->[$cpos] = "";
  }
  while(my $line = <IN>) { 
    if($line =~ m/^\s+\d+\s+\d+/) { 
      # $line is a trace line of this form:
      #   10     5C    93G    39MP Joint    11    -1     9 -0.02  2.63
      $line =~ s/^\s+//; # remove leading whitespace
      my @elA = split(/\s+/, $line);
      my ($emitl, $emitr, $state, $esc) = ($elA[1], $elA[2], $elA[3], $elA[9]);
      my ($lchar, $rchar, $lpos, $rpos) = ("", "", -1, -1);
      if($emitl =~ m/^(\d+)([A-Z])/) { $lpos = $1; $lchar = $2; }
      if($emitr =~ m/^(\d+)([A-Z])/) { $rpos = $1; $rchar = $2; }
      if($lchar ne "") { 
        if($state !~ m/ML/ && $state !~ m/MP/) { die "ERROR, unexpected left  emission in trace file: $line"; } 
        $escAR->[$lpos-1]  = ($state =~ m/MP/) ? $esc/2. : $esc; # split MP scores over both positions by dividing by 2
        $cseqAR->[$lpos-1] = $lchar;
      }
      if($rchar ne "") { 
        if($state !~ m/MR/ && $state !~ m/MP/) { die "ERROR, unexpected right emission in trace file: $line"; }
        $escAR->[$rpos-1]  = ($state =~ m/MP/) ? $esc/2. : $esc; # split MP scores over both positions by dividing by 2
        $cseqAR->[$rpos-1] = $rchar;
      } 
    }
  }
  # we're done; check to make sure we filled all consensus positions (we should have)
  for($cpos = 0; $cpos < $clen; $cpos++) { 
    if($escAR->[$cpos]  eq "") { die "ERROR, did not fill esc for consensus position $cpos"; }
    if($cseqAR->[$cpos] eq "") { die "ERROR, did not fill cseq for consensus position $cpos"; }
  }

  # clean up and return
  unlink $out_tfile;

  return;
}
    
sub help {
  my ($exec_description) = (@_);
  print STDERR <<EOF;
    
create-ss-graphics.pl - $exec_description

Usage:      create-ss-graphics.pl [options]

Options:    -ali <f> : seed alignment is in file <f> [default: 'SEED']
            -cm  <f> : CM is in file <f> [default: 'CM']
  	    -h|-help : print this help, then exit
EOF
}

##############################################
# BEGIN PAUL'S SUBROUTINES FROM RFAMbling.pl #
##############################################
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
	if ($covariationA[$i]<0){
	    $COV[$i] = "#ffffff";
	}
	else {
	    my $j = int($covariationA[$i] * (scalar(@colours)-1));
	    $COV[$i] = $colours[$j];
	}
	
	if ($fcbpA[$i]<0){
	    $FCBP[$i] = "#ffffff";
	}
	else {
	    $j = int($fcbpA[$i] * (scalar(@colours)-1));
	    $FCBP[$i] = $colours[$j];
	}

	if ($entropyA[$i]<0){
	    $ENTROPY[$i] = "#ffffff";
	}
	else {
	    $j = int($entropyA[$i] * (scalar(@colours)-1));
	    $ENTROPY[$i] = $colours[$j];
	}

	if ($conservationA[$i]<0){
	    $CONSERVATION[$i] = "#ffffff";
	}
	else {
	    $j = int($conservationA[$i] * (scalar(@colours)-1));
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

