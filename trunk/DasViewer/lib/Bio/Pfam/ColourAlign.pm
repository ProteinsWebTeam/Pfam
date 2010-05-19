
# $Author: pg6 $

package Bio::Pfam::ColourAlign;

use strict;
use warnings;
#use CGI;
our ($groups, $class, $colours);

#my $cgi = new CGI;

#&printCSS;
#my $alignRef = &parseAlign;
#my $conRef = &parseConsensus;
#&markupAlign($alignRef, $conRef);





sub printCSS {
    open(CSS, ">alignment.css") || die "Could not open ccs:[$!]\n";
    foreach my $className (sort{$a <=>$b} keys %$colours){
	     print CSS "span.S".$className."{background-color:".$colours->{$className}.";color:#FFFFFF}\n";
	     print CSS "span.T".$className."{color:".$colours->{$className}."}\n";
    }
    close(CSS);
}


sub parseAlign {
    my $FH = shift;
    my %align;
    #open(ALIGN, "Daxx/ALIGN") || die "Could not open align\n";
    while(<$FH>){
	if(/(\S+\/\d+\-\d+)\s+(.*)/){
	    $align{$1}=$2;
	}else{
	    warn "$_ is an unrecognised alignment line\n";
	}
    }
    #close(ALIGN);
    return \%align;
}

sub parseConsensus{
    my $consensus = shift;
    my @c = split(//,$consensus);
    return(\@c);
}

sub markupAlign {
  my( $alignRef, $conRef ) = @_;

  my( $r, $R );

  my $op = "<div class=\"align\">\n";
  my $rowNum = 0;
  foreach my $nse ( keys %$alignRef ){
	my $nseLabel;
	( $nseLabel = sprintf "%-20s", $nse ) =~ s/\s/\&nbsp;/g;
	my $i = 0;
	$op .= "<span class=\"alirow " . ( ( $rowNum++ % 2 ) ? "odd" : "even" ) . "\"><span class=\"nse\">$nseLabel</span><span class=\"alidata\">";
	while( $alignRef->{$nse}) {
	  $r = substr( $alignRef->{$nse}, 0, 1, "" );
	  $R = uc $r;

	  if( $R eq "." or $R eq "-" ) {
		$op .= $r;
	  } elsif( $R eq $conRef->[$i] ) {
		$op .= "<span class=\"S" . $class->{$R} . "\">$r</span>";
	  } elsif( $groups->{$conRef->[$i]}->{$R} ) {
		$op .= "<span class=\"T" . $class->{$R} . "\">$r</span>";
	  } else {
		$op .= $r;
	  }
	  $i++;
	  $op .= "\n" unless $i % 20;
	}
	$op .= "</span></span>\n";
  }
  $op .= "</div>";

  return $op;
}

sub markupAlignSeparate {
  my( $alignRef, $conRef ) = @_;

  my( $r, $R );

  my $key = "<div id=\"alignmentKey\">";
  my $ali = "<div id=\"alignmentData\">";

  my $rowNum = 0;
  foreach my $nse ( keys %$alignRef ){
	my $i = 0;

	$key .= "<span class=\"" . ( $rowNum % 2 ? "odd" : "even" ) . "\">$nse</span>\n";
	$ali .= "<span class=\"" . ( $rowNum++ % 2 ? "odd" : "even" ) . "\">";

	while( $alignRef->{$nse}) {
	  $r = substr( $alignRef->{$nse}, 0, 1, "" );
	  $R = uc $r;

	  if( $R eq "." or $R eq "-" ) {
		$ali .= $r;
	  } elsif( $R eq $conRef->[$i] ) {
		$ali .= "<span class=\"S" . $class->{$R} . "\">$r</span>";
	  } elsif( $groups->{$conRef->[$i]}->{$R} ) {
		$ali .= "<span class=\"T" . $class->{$R} . "\">$r</span>";
	  } else {
		$ali .= $r;
	  }
	  $i++;

	}

	$ali .= "<br /></span>\n";
  }

  $key .= "</div>\n";
  $ali .= "</div>\n";

  # composite the key and alignment into a single, large div
  return "<div id=\"alignmentBlock\">\n$key\n$ali</div>\n";
}

## subroutine added by pg6 for specific need( das alignment viewer );
#sub markupAlignSingle {
#  my( $acc, $seq, $conRef, $rowNum ) = @_;
#  
#  my ( $key, $value,$r, $R );
#  
#  my ($id, $start, $end );
#  if( $acc =~ /([A-Za-z0-9\.]+)\/(\d+)\-(\d+)/ ){
#    ( $id, $start, $end ) = $acc =~ /([A-Za-z0-9\.]+)\/(\d+)\-(\d+)/;  
#  }else{
#    $start = 1;
#    $id = $acc;
#  }
#  
#  
#  my $i = 0;
#  
#  my $col_count = 1;
#  my $res_count = $start;
#  
#  $key   = "<span id=\"$id\" class=\"" . ( $rowNum % 2 ? "odd" : "even" ) . "\">$acc</span>\n";
#	$value = "<span id=\"$id".'seq"'." class=\"" . ( $rowNum++ % 2 ? "odd" : "even" ) . "\">";
#
#	while( $seq ) {
#	  
#	  $r = substr( $seq, 0, 1, "" );
#	  $R = uc $r;
#    #print STDERR "the r is $r|$R|\n";
#    
#	  if( $R eq "." or $R eq "-" ) {
##		  $value .= $r;
#		  $value .= '<span class="C' . $col_count . qq(">$r</span>);
#		  
#	  } elsif( $R eq $conRef->[$i] ) {
##		  $value .= "<span class=\"S" . $class->{$R} . "\" class=\"C" . $i . "\""." class=\"R" . $res_count .>$r</span>";
#		  $value .= '<span class="S' . $class->{$R} . ' C'. $col_count .' R'. $res_count.'">'.$r.'</span';
#		  $res_count++;
#	  } elsif( $groups->{$conRef->[$i]}->{$R} ) {
#		  $value .= '<span class="T' . $class->{$R} . ' C'. $col_count .' R'. $res_count.'">'.$r.'</span';
#		  $res_count++;
##		  $value .= "<span class=\"T" . $class->{$R} . "\">$r</span>";
#		  #print "$value|$R|$conRef->[$i]|\n";
#	  } else {
#		  $value .= "<span class=\"C" . $col_count . " R" . $res_count . "\">$r</span>";
#		  $res_count++;
##		  $value .= $r;
#		  #print "$value|$R|$conRef->[$i]|\n";
#	  }
#	  $i++;
#    $col_count++;
#	}
#
#	$value .= "<br /></span>\n";
#
#  return $key,$value ;
#}

 #working version of this subroutine, I am making changes above;
sub markupAlignSingle {
  my( $acc, $seq, $conRef, $rowNum ) = @_;
  
  my ( $key, $value,$r, $R );
  
  my $i = 0;
  
#  # these changes are done by me;
#  my ($id, $start, $end );
#  if( $acc =~ /([A-Za-z0-9\.]+)\/(\d+)\-(\d+)/ ){
#    ( $id, $start, $end ) = $acc =~ /([A-Za-z0-9\.]+)\/(\d+)\-(\d+)/;  
#  }else{
#    $start = 1;
#    $id = $acc;
#  }
  # replaced id to acc;
  $key   = "<span id=\"$acc\" class=\"" . ( $rowNum % 2 ? "odd" : "even" ) . "\">$acc</span>\n";
	$value = "<span id=\"$acc".'seq"'." class=\"" . ( $rowNum++ % 2 ? "odd" : "even" ) . "\">";
  
  # if the consensus string is defined, then do the following else add the string to the span element
  if( scalar( @{ $conRef } ) > 0 ){
  
    while( $seq ) {
	  
  	  $r = substr( $seq, 0, 1, "" );
  	  $R = uc $r;
      
      
  	  if( $R eq "." or $R eq "-" ) {
  		  $value .= $r;
  		  #print "$value|$R|$conRef->[$i]|\n";
  	  } elsif( $R eq $conRef->[$i] ) {
  		  $value .= "<span class=\"S" . $class->{$R} . "\">$r</span>";
  		  #print "$value|$R|$conRef->[$i]|\n";
  	  } elsif( $groups->{$conRef->[$i]}->{$R} ) {
  		  $value .= "<span class=\"T" . $class->{$R} . "\">$r</span>";
  		  #print "$value|$R|$conRef->[$i]|\n";
  	  } else {
  		  $value .= $r;
  		  #print "$value|$R|$conRef->[$i]|\n";
  	  }
  	  $i++;
  
  	} 
  	 
  } # end of scalar conref 
  else{
    $value .= $seq;
  }
	

	$value .= "<br /></span>\n";

  return $key,$value ;
}

#These are build at compile time as it makes it faster running in modperl and
#as they should not be altered.  I have put them down here to make the code a 
#little cleaner.

BEGIN{
$groups =  {'a' => { F => 1, Y => 1, W => 1, H => 1 },
	       'l' => { I => 1, V => 1, L => 1},
	       'h' => { I => 1, V => 1, L => 1, F => 1, Y => 1, W => 1, H => 1,  A => 1, G => 1, M => 1 , C => 1, K => 1, R => 1, T => 1},
	       '+' => { H => 1, K => 1, R => 1 },
	       '-' => { D => 1, E => 1},
	       'c' => { H => 1, K => 1, R => 1, D => 1, E => 1},
	       'p' => { H => 1, K => 1, R => 1, D => 1, E => 1, Q => 1, N => 1, S=> 1, T => 1, C => 1},
	       'o' => { S => 1, T => 1},
	       'u' => { G => 1, A => 1, S => 1},
	       's' => { G => 1, A => 1, S => 1, V => 1, T => 1, D => 1, N => 1, P => 1, C => 1 },
	       't' => { G => 1, A => 1, S => 1, H => 1, K => 1, R => 1, D => 1, E => 1, Q => 1, N => 1, T => 1, C => 1}
	   };


$class = {
    'a' => 1,
    'l' => 2,
    'h' => 3,
    '+' => 4,
    '-' => 5,
    'c' => 6,
    'p' => 7,
    'o' => 8,
    'u' => 9,
    's' => 10,
    't' => 11,
    'A' => 12,
    'B' => 13,
    'C' => 14,
    'D' => 15,
    'E' => 16,
    'F' => 17,
    'G' => 18,
    'H' => 19,
    'I' => 20,
    'K' => 21,
    'L' => 22,
    'M' => 23,
    'N' => 24,
    'P' => 25,
    'Q' => 26,
    'R' => 27,
    'S' => 28,
    'T' => 29,
    'V' => 30,
    'W' => 31,
    'X' => 32,
    'Y' => 33,
    'Z' => 34     
    };

$colours = {
    '1'  => "#009900", 	     #aromatic
    '2'  => "#33cc00", 	     #aliphatic
    '3'  => "#33cc00", 	     #hydrophobic
    '4'  => "#cc0000",	     #positive charge
    '5'  => "#0033ff",	     #negative charge
    '6'  => "#6600cc", 	     #charged
    '7'  => "#0099ff", 	     #polar
    '8'  => "#0099ff", 	     #alcohol
    '9'  => "#33cc00", 	     #tiny
    '10' => "#33cc00", 	     #small
    '11' => "#33cc00", 	     #turnlike
    '12' => "#197fe5",    #hydrophobic
    '13' => "#ff11ff",    #D or N
    '14' => "#ffff11",    #cysteine
    '15' => "#ff11ff",    #clustal-pink, negative charge
    '16' => "#ff11ff",   #clustal-pink         #negative charge
    '17' => "#197fe5",  #clustal-dull-blue    #large hydrophobic
    '18' => "#ff7f11", # clustal-orange       #glycine
    '19' => "#197fe5", #clustal-dull-blue    #large hydrophobic
    '20' => "#197fe5", #clustal-dull-blue    #hydrophobic
    '21' => "#ff1111", #clustal-red          #positive charge
    '22' => "#197fe5", #clustal-dull-blue    #hydrophobic
    '23' => "#197fe5", #clustal-dull-blue    #hydrophobic
    '24' => "#11dd11", #clustal-green        #polar
    '25' => "#ffff11", #clustal-yellow       #proline
    '26' => "#11dd11", #clustal-green        #polar
    '27' => "#ff1111", #clustal-red          #positive charge
    '28' => "#11dd11", #clustal-green        #small alcohol
    '29' => "#11dd11", #clustal-green        #small alcohol
    '30' => "#197fe5", #clustal-dull-blue    #hydrophobic
    '31' => "#197fe5", #clustal-dull-blue    #large hydrophobic
    '32' => "#666666", #clustal-dark-gray    #any
    '33' => "#11ffff", #clustal-cyan         #large hydrophobic
    '34' => "#ff11ff" #clustal-pink         #E or Q
    };
}
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;

