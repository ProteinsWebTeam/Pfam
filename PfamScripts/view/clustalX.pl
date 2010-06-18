#!/usr/local/bin/perl

# clustalX.pl
# rdf/jt6 20080418 WTSI
#
# a script to convert Stockholm-format sequence alignments into HTML, according
# to supplied consensus strings
#
# $Id$

use strict;
use warnings;

use Bio::Pfam::AlignPfam;

our $VERSION = 0.1;

use Getopt::Std;

#-------------------------------------------------------------------------------
#- preamble --------------------------------------------------------------------
#-------------------------------------------------------------------------------

# set up the mapping between residue symbol and code references

my %METHODS = ( A => \&ACFILMVW, 
                C => \&ACFILMVW, 
                F => \&ACFILMVW, 
                I => \&ACFILMVW,
                L => \&ACFILMVW, 
                M => \&ACFILMVW, 
                V => \&ACFILMVW, 
                W => \&ACFILMVW, 

                H => \&HY, 
                Y => \&HY, 

                S => \&ST, 
                T => \&ST, 

                '.' => \&gap,
                '-' => \&gap,

                B => \&B, 
                D => \&D, 
                E => \&E, 
                G => \&G, 
                J => \&J, 
                K => \&K, 
                N => \&N, 
                O => \&O, 
                P => \&P, 
                Q => \&Q, 
                R => \&R,
                U => \&U, 
                X => \&X, 
                Z => \&Z, 
              );

my $SS = << 'EOF_ss';
      .o  { background-color: #ff7f11 } /* orange */
      .y  { background-color: #ffff11 } /* yellow */
      .b  { background-color: #9999FF } /* lilac */
      .g  { background-color: #99FF99 } /* light green */
      .r  { background-color: #FF9999 } /* pink */
      .c  { background-color: #99FFFF } /* cyan */
      .w  { background-color: #FFFFFF } /* white */
      .as { background-color: #000; color: #FFF }
      .pas{ background-color: #666; color: #FFF }
      .sas{ background-color: #999; color: #FFF }
      .H  { background-color: #F00 !important; color: #FFF }
      .E  { background-color: yellow !important; color: #000 }
      body {font-family:monospace}

      /* outer wrapper */
      div.align {}
      
      /* alignment block */
      div.aliblock {
        margin: 1em 0;
      }
      
      /* alignment row */
      div.alirow {}
      
      /* secondary structure row */
      div.ssrow span {
        background-color: #EEEEEE;
      }
      div.ssrow span.nse {
        background-color: #FFF;
      }
      
      /* row header */
      span.nse {
        width: 16em;
        float: left;
      }
EOF_ss

#-------------------------------------------------------------------------------

my %option = ();
getopts( 'a:c:b:vnws', \%option );

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
usage( 'consensus file not given' ) unless defined $option{c};

if ( $DEBUG ) {
  print STDERR "reading alignment from file: |$option{a}|\n";
  print STDERR "reading consensus from file: |$option{c}|\n";
}

# set block length
my $block_length;
if ( $option{n} ) {
  # set the block length to a big number, so that we end up with just one line
  $block_length = 99999999;
}
else {
  # set the block length to the user specified option, defaulting to 80 if the
  # command-line option isn't given
  $block_length = $option{b} || 80;
}

#-------------------------------------------------------------------------------
#- main ------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# read in the alignment

open ( ALIGN, $option{a} )
  or die "error: couldn't open alignment file ($option{a}): $!";

# read the alignment into an object
my $ali = Bio::Pfam::AlignPfam->new();
$ali->read_stockholm( \*ALIGN );

close ALIGN;


#----------------------------------------
# read in the consensus

open ( CON, $option{c} )
  or die "error: couldn't open consensus file ($option{c}): $!";

my %consensuses;
while ( <CON> ) {
  next unless /consensus\/(\d+)\%\s+(\S+)/;
  @{ $consensuses{$1} } = split '', $2;
}

close CON;

#----------------------------------------
# mark up the alignment

my %markedUpAli;
my %markedUpSS;
my $maxBlock = -1;
my @nse;

foreach my $seq ($ali->each_seq ) {

  if ( $DEBUG ) {
    print STDERR "-------------------------------------------------------------------------------\n";
    print STDERR "SQ: " . $seq->seq . "\n";
  }

  my $ss = 0;
  my @ss = ();
  if ( $seq->sec_struct ) {
    print STDERR "\nSS: ", $seq->sec_struct->display, "\n" if $DEBUG;
    $ss = 1;
    @ss = split '', $seq->sec_struct->display;
  }

  my $as = '';
  my @as = ();
  if ( $seq->active_site ) {
    print STDERR "\nAS: ", $seq->active_site->display, "\n" if $DEBUG;
    print STDERR "AS: ", $seq->active_site->type, "\n\n" if $DEBUG;

    $as = $seq->active_site->type;
    @as = split '', $seq->active_site->display;

    # type is one of
    #   active_site - experimentally determined as (black)
    #   pfam_pred_active_site - pfam predicted AS (dark grey)
    #   sprot_pred_active_site - SwissProt pred. AS (light grey)
  }elsif ( $seq->sprot_pred_active_site ){
    print STDERR "\nAS: ", $seq->sprot_pred_active_site->display, "\n" if $DEBUG;
    print STDERR "AS: ", $seq->sprot_pred_active_site->type, "\n\n" if $DEBUG;

    $as = $seq->sprot_pred_active_site->type;
    @as = split '', $seq->sprot_pred_active_site->display;
  }elsif( $seq->pfam_pred_active_site ){
    print STDERR "\nAS: ", $seq->pfam_pred_active_site->display, "\n" if $DEBUG;
    print STDERR "AS: ", $seq->pfam_pred_active_site->type, "\n\n" if $DEBUG;

    $as = $seq->pfam_pred_active_site->type;
    @as = split '', $seq->pfam_pred_active_site->display;
  }

  my $block_num = 0;
  $maxBlock = $block_num if $block_num > $maxBlock;

  my $nse = $seq->id . '/' . $seq->start . '-' . $seq->end;
  push @nse, $nse;

  my $currentClass   = '';
  my $currentSSClass = '';
  my $pos            = 0;
  my $counter        = 0;
  my $length         = length( $seq->seq );

  foreach my $aa ( split '', $seq->seq ) {

    # choose the class for this character
    my $class;

    # is it an active site residue ?
    if ( $as and $as[$pos] ne '.' ) {

      # yes; class it according to the type of AS
      $class = ( $as eq 'active_site' )            ?  'as'
             : ( $as eq 'pfam_pred_active_site' )  ? 'pas'
             : ( $as eq 'sprot_pred_active_site' ) ? 'sas'
             : '';
    }
    else {
      # no; class it according to the colouring scheme
      my $codeRef = $METHODS{ uc($aa) };
      $class = &$codeRef( $pos, \%consensuses );
    }

    # wrap the character up in HTML tags
    my $taggedAA;
    if ( $class eq $currentClass ) {
      $taggedAA = $aa;
    }
    elsif ( $pos == 0 ) {
      $taggedAA = qq(\n<div class="alirow">\n<span class="nse">$nse</span><span class="$class">$aa);
    }
    else {
      $taggedAA = qq(</span><span class="$class">$aa); 
    }
    $currentClass = $class;

    # and now do the same for the SS mark up, if there any exists for this row
    my $taggedSS;
    if ( $ss and $ss[$pos] ) {
      if ( $ss[$pos] eq $currentSSClass ) {
        $taggedSS = $ss[$pos];
      }
      elsif ( $pos == 0 ) {
        $taggedSS = qq(\n<div class="ssrow">\n<span class="nse">$nse (SS)</span><span class="$ss[$pos]">$ss[$pos]);
      }
      else {
        $taggedSS = qq(</span><span class="$ss[$pos]">$ss[$pos]); 
      }
      $currentSSClass = $ss[$pos];
    }
    
    # close off the the markup row. Is the row still shorter than the allowed 
    # block length ?
    if ( $counter < $block_length ) {
      # yes; just add the marked-up character

      $markedUpAli{$nse}->[$block_num] .= $taggedAA;
      $markedUpSS{$nse}->[$block_num]  .= $taggedSS
        if ( $ss and $ss[$pos] );
    }
    else {
      # no; start a new block and add the character to it

      # close off the old block...
      $markedUpAli{$nse}->[$block_num] .= qq(</span>\n</div>);
      $markedUpSS{$nse}->[$block_num]  .= qq(</span>\n</div>)
        if ( $ss and $ss[$pos] );

      $block_num++;
      $maxBlock = $block_num if $block_num > $maxBlock;

      # and start the new one
      $markedUpAli{$nse}->[$block_num] = qq(\n<div class="alirow">\n<span class="nse">$nse</span><span class="$class">$aa);
      $markedUpSS{$nse}->[$block_num]  = qq(\n<div class="ssrow">\n<span class="nse">$nse (SS)</span><span class="$ss[$pos]">$ss[$pos])
        if ( $ss and $ss[$pos] );

      $counter = 0;
    }
 
    $counter++;
    $pos++;
    if ( $pos == $length ) {
      $markedUpAli{$nse}->[$block_num] .= qq(</span>\n</div>);
      $markedUpSS{$nse}->[$block_num]  .= qq(</span>\n</div>)
        if ( defined $markedUpSS{$nse}->[$block_num] );
    }
  }
}

#----------------------------------------
# print out the blocks

print_page_head() if $option{w};

print q(<div class="align">);
for ( my $i = 0; $i <= $maxBlock; $i++ ) {
  print q(<div class="aliblock">);
  foreach my $nse ( @nse ) {
    print $markedUpAli{$nse}->[$i] . "\n";
    print $markedUpSS{$nse}->[$i]  . "\n" if defined $markedUpSS{$nse}->[$i];
  } 
  print qq(</div>\n);
}
print qq(</div>\n);

print_page_foot() if $option{w};

exit;

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

sub usage {
  my $msg = shift;
  print STDERR "error: $msg\n" if defined $msg;
  print STDERR << "EOF_usage";
usage: $0 -a <alignment_file> -c <consensus_file> [-w] [-b <block_length>|-n] [-v]

  required switches:
    a <alignment_file> - file containing Stockholm format alignment
    c <consensus_file> - file containing consensus strings

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

#-------------------------------------------------------------------------------
#- code references -------------------------------------------------------------
#-------------------------------------------------------------------------------

sub ACFILMVW {
  my ( $pos, $consensuses ) = @_;
  foreach my $aa ( qw( A C F H I L M V W Y P Q h ) ) {
    return 'b' if $consensuses->{60}->[$pos] eq $aa;
  }
  return 'w';
}

#-------------------------------------------------------------------------------

sub B {
  my ( $pos, $consensuses ) = @_;

  return 'b' if $consensuses->{60}->[$pos] eq 'h';

  return 'b' if $consensuses->{80}->[$pos] eq 'h';

  foreach my $aa ( qw( A C F H I L M V W Y P Q ) ) {
    return 'b' if $consensuses->{85}->[$pos] eq $aa;
  }
  return 'w'; 
}

#-------------------------------------------------------------------------------

sub D {
  my ( $pos, $consensuses ) = @_;
  return 'r' if ( $consensuses->{60}->[$pos] eq '+' or 
                  $consensuses->{60}->[$pos] eq 'R' or 
                  $consensuses->{60}->[$pos] eq 'K' );

  foreach my $aa ( qw( D E N ) ) {
    return 'r' if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'r' if ( $consensuses->{50}->[$pos] eq '-' or 
                  $consensuses->{60}->[$pos] eq 'E' or 
                  $consensuses->{60}->[$pos] eq 'D' );

  return 'w';
}

#-------------------------------------------------------------------------------

sub E {
  my ( $pos, $consensuses )  = @_;

  return 'r' if ( $consensuses->{60}->[$pos] eq '+' or 
                  $consensuses->{60}->[$pos] eq 'R' or 
                  $consensuses->{60}->[$pos] eq 'K' );

  foreach my $aa ( qw( D E ) ) {
    return 'r' if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'r' if ( $consensuses->{50}->[$pos] eq 'b' or 
                  $consensuses->{50}->[$pos] eq 'E' or 
                  $consensuses->{50}->[$pos] eq 'Q' );

  return 'w'; 
}

#-------------------------------------------------------------------------------

sub G {
  return 'o'; 
}

#-------------------------------------------------------------------------------

sub HY {
  my ( $pos, $consensuses ) = @_;
  
  return 'c' if $consensuses->{60}->[$pos] eq 'h';

  return 'c' if $consensuses->{80}->[$pos] eq 'h';

  foreach my $aa (qw( A C F H I L M V W Y P Q h ) ){
    return 'c' if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'w'; 
}

#-------------------------------------------------------------------------------

sub K {
  my ( $pos, $consensuses ) = @_;
  
  return 'r' if ( $consensuses->{60}->[$pos] eq '+' or 
                  $consensuses->{60}->[$pos] eq 'R' or 
                  $consensuses->{60}->[$pos] eq 'K' );

  foreach my $aa ( qw( K R Q ) ) {
    return 'r' if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'w'; 
}

#-------------------------------------------------------------------------------

sub N {
  my ( $pos, $consensuses ) = @_;

  return 'g' if $consensuses->{50}->[$pos] eq 'N';
  return 'g' if $consensuses->{85}->[$pos] eq 'D';
  return 'w';
}

#-------------------------------------------------------------------------------

sub P {
  return 'Y'; 
}


#-------------------------------------------------------------------------------
sub Q {
  my ( $pos, $consensuses ) = @_;

  return 'g' if ( $consensuses->{50}->[$pos] eq 'b' or 
                  $consensuses->{50}->[$pos] eq 'E' or 
                  $consensuses->{50}->[$pos] eq 'Q' );

  foreach my $aa ( qw( Q T K R ) ) {
    return 'g' if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'g' if ( $consensuses->{60}->[$pos] eq '+' or 
                  $consensuses->{60}->[$pos] eq 'K' or 
                  $consensuses->{50}->[$pos] eq 'R' );

  return 'w'; 
}

#-------------------------------------------------------------------------------

sub R {
  my ( $pos, $consensuses ) = @_;

  foreach my $aa ( qw( Q K R ) ) {
     return 'r' if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'r' if ( $consensuses->{60}->[$pos] eq '+' or 
                  $consensuses->{60}->[$pos] eq 'R' or 
                  $consensuses->{60}->[$pos] eq 'K' );

  return 'w';
}

#-------------------------------------------------------------------------------

sub ST {
  my ( $pos, $consensuses ) = @_;

  return 'g' if ( $consensuses->{50}->[$pos] eq 'a' or 
                  $consensuses->{50}->[$pos] eq 'S' or 
                  $consensuses->{50}->[$pos] eq 'T' );

  foreach my $aa (qw(A C F H I L M V W Y P Q)){
    return 'g'  if $consensuses->{85}->[$pos] eq $aa;
  }

  return 'w';
}

#-------------------------------------------------------------------------------

sub gap {
  return 'w'; 
}

#-------------------------------------------------------------------------------

sub X {
  return 'w'; 
}

#-------------------------------------------------------------------------------
sub Z {
  return 'w';
}
