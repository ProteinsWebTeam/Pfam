#!/usr/local/bin/perl

#Mark up alignment according to posterior probabilities.

use strict;
use warnings;
use Getopt::Std;

use Bio::Pfam::Config;
use Bio::Pfam::AlignPfam;

our $VERSION = 0.1;


my $SS = << 'EOF_ss';
      .t0  { background-color: #FF0000 } /* RED */
      .t1  { background-color: #EA1900 } 
      .t2  { background-color: #D63200 } 
      .t3  { background-color: #C14C00 } 
      .t4  { background-color: #AD6600 } 
      .t5  { background-color: #997F00 } 
      .t6  { background-color: #849900 } 
      .t7  { background-color: #70B200 }
      .t8  { background-color: #5BCC00 }
      .t9  { background-color: #47E500 }
      .tm  { background-color: #33FF00 } /* GREEN */
      .w  { background-color: #FFFFFF } /* white */
       body {font-family:monospace}
      
      /* scale */
      div.scale {
        font-family:verdana,arial,helvetica,sans-serif;
        text-align: center;
      }
      /* outer wrapper */
      div.align {}
      
      /* alignment block */
      div.aliblock {
        margin: 1em 0;
      }
      
      /* alignment row */
      div.alirow {}
      
      
      /* row header */
      span.nse {
        width: 16em;
        float: left;
      }
EOF_ss

#-------------------------------------------------------------------------------

my $config = Bio::Pfam::Config->new;

my %option = ();
getopts( 'a:b:vnws', \%option );

# we need *some* arguments
usage() unless scalar keys %option;

# see if we should just print out the stylesheet and die
if ( $option{s} ) {
  print $SS;
  exit;
}

# turn on debugging ?
my $DEBUG = defined $option{v} ? 1 : 0;

# check the options are populated
usage( 'alignment file not given' ) unless defined $option{a};

if ( $DEBUG ) {
  print STDERR "reading alignment from file: |$option{a}|\n";
}

# set block length
my $block_length;
if ( $option{n} ) {
  # set the block length to a big number, so that we end up with just one line
  $block_length = 99999999;
} else {
  # set the block length to the user specified option, defaulting to 80 if the
  # command-line option isn't given
  $block_length = $option{b} || 80;
}

#-------------------------------------------------------------------------------
#

system("sreformat fasta ALIGN.ann > fa.ann") and die "Failed to run sreformat:[$!]\n";
system($config->hmmer3bin."/hmmalign HMM fa.ann > ALIGN.pp.wsto") and die "Failed to run hmmalign:[$!]\n";

my ($aliHash, $aliOrder) = read_alignment(); 
my ($aliHM);

my $maxBlock = -1;

  
foreach my $nse (@{$aliOrder}){
  
  my @pp = split('', $aliHash->{$nse}->{'PP'});
  my @aa = split('', $aliHash->{$nse}->{'SQ'});
  my $block_num = 0;
  $maxBlock = $block_num if $block_num > $maxBlock;
  my $currentClass   = '';
  my $currentSSClass = '';
  my $pos            = 0;
  my $counter        = 0;
  my $length         = scalar(@aa);
  
  for (my $i = 0; $i <= $#aa; $i++){
    my $class;   
    if($pp[$i] eq "."){
      $class = "w";
    }elsif( $pp[$i] eq "*"){
      $class = "tm"; 
    }else{
      $class = "t".$pp[$i]; 
    }
    
    # wrap the character up in HTML tags
     my $taggedAA;
     if ( $class eq $currentClass ) {
        $taggedAA = $aa[$i];
     }elsif ( $pos == 0 ) {
        $taggedAA = qq(\n<div class="alirow">\n<span class="nse">$nse</span><span class="$class">$aa[$i]);
     } else {
        $taggedAA = qq(</span><span class="$class">$aa[$i]); 
     }
      $currentClass = $class;
    
      # close off the the markup row. Is the row still shorter than the allowed 
      # block length ?
      if ( $counter < $block_length ) {
        # yes; just add the marked-up character
        $aliHM->{$nse}->[$block_num] .= $taggedAA;
 
      }else {
        # no; start a new block and add the character to it

        # close off the old block...
        $aliHM->{$nse}->[$block_num] .= qq(</span>\n</div>);
        $block_num++;
        $maxBlock = $block_num if $block_num > $maxBlock;
    
       # and start the new one
        $aliHM->{$nse}->[$block_num] = qq(\n<div class="alirow">\n<span class="nse">$nse</span><span class="$class">$aa[$i]);
        $counter = 0;
      }
    
    $counter++;
    $pos++;
    if ( $pos == $length ) {
      $aliHM->{$nse}->[$block_num] .= qq(</span>\n</div>);
    }  
      
  }
 
}






#----------------------------------------
# print out the blocks



print_page_head() if $option{w};

print q(<div class="scale">0);
foreach my $c ( qw(t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 tm) ){
  print qq(<span class="$c">&nbsp;&nbsp;</span>); 
}
print qq(100% Posterior Probability</div>\n);

print q(<div class="align">);
for ( my $i = 0; $i <= $maxBlock; $i++ ) {
  print q(<div class="aliblock">);
  foreach my $nse ( @{ $aliOrder} ) {
    print $aliHM->{$nse}->[$i] . "\n";
  } 
  print qq(</div>\n);
}
print qq(</div>\n);

print_page_foot() if $option{w};

exit;

sub usage {
  my $msg = shift;
  print STDERR "error: $msg\n" if defined $msg;
  print STDERR << "EOF_usage";
usage: $0 -a <alignment_file> [-w] [-b <block_length>|-n] [-v]

  required switches:
    a <alignment_file> - file containing Stockholm format alignment
  
  optional switches:
    s                - print the CSS and exit                          (default off)
    w                - wrap alignment in HTML to make full page        (default off)
    b <block_length> - make blocks <block_length> characters long      (default 80)
    n                - no blocks; outputs alignment rows as long lines (default off)
    v                - be verbose; prints debug messages to STDERR     (default off)
  
EOF_usage
  exit;
}

#-------------------------------------------------------------------------------

sub print_page_head {

  print << "EOF_html";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
  <head>
    <title>Alignment</title>
    <style type="text/css">
$SS
    </style>
  </head>
  <body>
EOF_html

}

#-------------------------------------------------------------------------------

sub print_page_foot {

  print << 'EOF_foot';
  </body>
</html>
EOF_foot

}

sub read_alignment {
  
#  MED15_HUMAN/9-73           KDMESHVFLKAKTRDEYLSLVARLIIHFRDIHNKK........
#  #=GR MED15_HUMAN/9-73   PP ********************************985........
  
  my $reformat;
  my @order; 
  my $maxlength = 0;
  open(ALISTO, "ALIGN.pp.wsto") || die "Could not open ALIGN.sto\n";
 
  while(<ALISTO>){
    if(/^#=GR (\S+)\s+PP\s(\S+)$/){
      $reformat->{$1}->{'PP'} .= $2;
    }elsif(/^(\S+)\s+(\S+)$/){
      $maxlength = length($1) if ($maxlength < length($1));
      if(!$reformat->{$1}){
        push(@order, $1);  
      }
      $reformat->{$1}->{'SQ'} .= $2;
    }elsif(/^#/){
      next;
    }elsif(/\/\//){
      next;
    }elsif(/^$/){
      next; 
    }else{ 
      warn "Did not parse $_ from the stockholm format\n";
    }
  }
  close(ALISTO) || die "Error closing ALIGN.sto\n";
  return($reformat, \@order); 
}
