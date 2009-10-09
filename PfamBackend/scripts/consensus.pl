#!/usr/local/bin/perl

use strict;
use warnings;
use Getopt::Long;
use Data::Dumper;
use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();
 
# The main body of the program starts here 
my (@THRESHOLD, $file, $method, $help, $print_sets);
GetOptions( "thr=s"    => \@THRESHOLD,
            "file=s"   => \$file,
            "method=s" => \$method,
            "help"     => \$help,
            "sets"     => \$print_sets );
if($print_sets){
  print_sets();
  exit 0;
}
my %METHODS;
%METHODS = ( A => \&A, B => \&B, C => \&C, D => \&D, E => \&E, F => \&F, G => \&G, H => \&H, I => \&I,
             J => \&J, K => \&K, L => \&L, M => \&M, N => \&N, O => \&O, P => \&P, Q => \&Q, R => \&R,
             S => \&S, T => \&T, U => \&U, V => \&V, W => \&W, X => \&X, Y => \&Y, Z => \&Z);
if(!$file){
  $logger->logdie("You need to specifiy an alignment file (e.g. SEED, ALIGN)"); 
}

if(!$method){
  $logger->debug("Setting method to clustal");
  $method = "clustal"; 
}

#Die if the method is unknown.
unless($method eq "clustal" or $method eq "pfam" or $method eq "belvu"){
   $logger->logdie("Unknown method $method");
}

#Set the default usage thresholds if not set
if(!scalar(@THRESHOLD)){
  if($method eq "clustal"){
    @THRESHOLD = qw(85 80 60 50); 
  }elsif($method eq "pfam"){
    @THRESHOLD = qw(60)
  }elsif($method eq "belvu"){
    @THRESHOLD = qw(0) 
  }
}

my %matrix;
if($method eq "belvu"){
  _readBlosum(\%matrix); 
}

# Set up of global variables

my %SET; 
my @ID;
my %ALIGNMENT;
my %SCORE;
my $LENGTH;
my %CONCENSUS;



read_alignment($file);

printf "%-20s %s\n",          $ID[0], join("", @{$ALIGNMENT{$ID[0]}}); 
foreach my $threshold (reverse sort @THRESHOLD) {
  compute_consensus($threshold, $method);
  my $format = sprintf "%d%%", $threshold;
  printf "%-9s/%-9s  %s\n", 'consensus', $format, $CONCENSUS{$threshold};
}




###############
# Subroutines #
###############

sub read_alignment {
  my $file = shift;
  open(TMP, "$file") or die "can't open file '$file'\n";
  while (<TMP>){
    next if ($_ =~ /^CLUSTAL/);
    if ($_ =~ /^(\S+\/\d+-\d+)\s+([-a-zA-Z*.]+) *$/) {
      my $nse = $1;
      my $ali = $2;
      $ali =~ s/\s|\t|\n//g;
      push @{$ALIGNMENT{$nse}}, split("", $ali);
    }else{
      $logger->warn("Failed to parse $_\n");
    }
  }
  close TMP;
  @ID = keys %ALIGNMENT; 
  $LENGTH = check_lengths();
}

sub check_lengths {
    my ($id, $l);
    my $len = scalar(@{$ALIGNMENT{$ID[0]}}); 
    foreach $id (@ID) {
        if (($l = @{$ALIGNMENT{$id}}) != $len) {
            die "sequence length differs for '$id' ($l)\n";
        }
    }
    return $len;
}
  

sub compute_consensus {
  my ($threshold, $method) = @_;
  my ($column) = [];
  my $aa;
  my $c;
  for ($c = 0; $c < $LENGTH; $c++) {
	 # This is where my changes really kick in.......
	 my %column_aa;
	 my $aaCount = 0;
	 foreach my $id (@ID){
	   next if (${$ALIGNMENT{$id}}[$c] =~ /(\.|-)/);
     $aa = ${$ALIGNMENT{$id}}[$c];
	   $column_aa{uc($aa)}++;
     $aaCount++;
   }
	
   if($aaCount) {
		%SET = ();
		%SCORE = ();
		tally_column_new(\%column_aa, $method);
    arbitrate($threshold) unless $method eq "belvu";
   } else {
     # This appears to be used when given an alignment column of
	   # only gaps.  Weird huh!
     $CONCENSUS{$threshold} .= '.';
    }
  }
}
 
 
sub tally_column_new{
  my $column = shift;
  my $m = shift;;
  my ($class, $score);
  
  if($m eq "belvu"){
    my $n = scalar(@ID);
    foreach my $matAa (keys %matrix){
      my $aaScore;
      foreach my $aa (keys %$column){
        $aaScore += ($matrix{$matAa}->{$aa} * $column->{$aa})/$n;
      }
      $aaScore =  sprintf("%.2f", $aaScore);
      print "$matAa, $aaScore\n" if($aaScore > 0); 
    }
    print "___\n";
  }else{
  
  
  #This initalises the specific amino acid sets and sums the score for the amino acid type
  foreach my $aa (keys %$column){
     $aa= uc($aa);
     $SET{$aa} = ["$aa", [ "1" ]] unless ($SET{$aa});
     $SCORE{$aa} += $$column{$aa};
  } 
  
  foreach my $aa (keys %SCORE){
    $SET{"any"} = [".", [ "20" ]] unless($SET{"any"});
		$SCORE{"any"} += $SCORE{$aa};
		if($m eq "clustal"){
		  #KR#QE#TS#ED
		  if($aa eq "Q" or $aa eq "E" or $aa eq "B"){
		    clustalBs($SCORE{$aa});
		  }
		  
		  if($aa eq "T" or $aa eq "S"){
		    clustalAlcohol($SCORE{$aa});
		  }
		  
		  if($aa eq "E" or $aa eq "D"){
		    clustalNegative($SCORE{$aa});
		  }
		  
		  if($aa eq "K" or $aa eq "R" or $aa eq "O"){
		    clustalPositive($SCORE{$aa}); 
		  }
		  
		  foreach my $h (qw(W L V I M A F C Y H P)){
		    if($aa eq $h){
		      clustalHydro($SCORE{$aa});
		      last;
		     }
		  } 
		}else{
		  my $codeRef = $METHODS{$aa};
		  &$codeRef($aa);
		}
  } 
  } 
}

sub S {
  my $class = shift;
  &alcohol($SCORE{$class});
  &polar($SCORE{$class});
  &tiny($SCORE{$class}); 
}

sub T {
   my $class = shift; 
  	&alcohol($SCORE{$class});
	  &small($SCORE{$class});
		&turnlike($SCORE{$class});
		&polar($SCORE{$class});
		&hydrophobic($SCORE{$class});
}

sub I {
  my $class = shift;
  &aliphatic($SCORE{$class});
} 

sub L {
  my $class = shift;
  &aliphatic($SCORE{$class});
}

sub J {
  my $class = shift;
  &aliphatic($SCORE{$class});
} 

sub B {
  my $class = shift;
  &aliphatic($SCORE{$class});
}

sub V {
  my $class = shift;
  &aliphatic($SCORE{$class});
	&small($SCORE{$class});
}

sub A {
  my $class = shift;
  &hydrophobic($SCORE{$class});
	&tiny($SCORE{$class});
}

sub C {
  my $class = shift;
  &hydrophobic($SCORE{$class});
	&polar($SCORE{$class});
	&small($SCORE{$class});
	&turnlike($SCORE{$class});
}

sub D {
  my $class = shift;
	&negative($SCORE{$class});
	&small($SCORE{$class});
  &turnlike($SCORE{$class});
}

sub E	{
  my $class = shift;
	&negative($SCORE{$class});
	&turnlike($SCORE{$class});
}

sub F {
  my $class = shift;
	&aromatic($SCORE{$class});
}

sub G {
  my $class = shift;
	&tiny($SCORE{$class});
	&hydrophobic($SCORE{$class});
}			

sub H {
  my $class = shift;
  &aromatic($SCORE{$class});
	&positive($SCORE{$class});
	&turnlike($SCORE{$class});
}

sub K{
  my $class = shift;
	&positive($SCORE{$class});
	&turnlike($SCORE{$class});
	&hydrophobic($SCORE{$class});
}

sub M {
  my $class = shift;
	&hydrophobic($SCORE{$class});
}

sub N {
  my $class = shift;
  &polar($SCORE{$class});
	&small($SCORE{$class});
	&turnlike($SCORE{$class});
}

sub P {
  my $class = shift;
  &small($SCORE{$class});
}

sub Q {
  my $class = shift;
  &polar($SCORE{$class});
  &turnlike($SCORE{$class});
}

sub R {
  my $class = shift;
	&positive($SCORE{$class});
	&turnlike($SCORE{$class});
	&hydrophobic($SCORE{$class});
}

sub W {
  my $class = shift;
	&aromatic($SCORE{$class});
}

sub Y {
  my $class = shift;
	&aromatic($SCORE{$class});
}

#Selenocysteine
sub U {
  my $class = shift;
  &hydrophobic($SCORE{$class});
	&polar($SCORE{$class});
	&small($SCORE{$class});
	&turnlike($SCORE{$class});
}

sub Z {
  my $class = shift;
  &turnlike($SCORE{$class}); 
}

sub O {
  my $class = shift;
	&positive($SCORE{$class});
	&turnlike($SCORE{$class});
	&hydrophobic($SCORE{$class});
}
sub X {
  #No score
}


sub negative {
	my $score = shift;
	$SET{"negative"} = ["-", [ "2" ]] unless($SET{"negative"});
	$SCORE{"negative"} += $score;
	&charged($score);
}



sub positive {
	my $score = shift;
	if (!defined $SCORE{"positive"})
		{
		$SET{"positive"} = ["+", [ "3" ]];
		$SCORE{"positive"} = $score
		}
	else
		{
		$SCORE{"positive"} = $SCORE{positive} + $score;
		}
	&charged($score);
	}

sub charged
	{
	my $score = shift;
	if (!defined $SCORE{"charged"})
		{
		$SET{"charged"} = ["c", [ "5" ]];
		$SCORE{"charged"} = $score
		}
	else
		{
		$SCORE{"charged"} = $SCORE{charged} + $score;
		}
	&polar($score);
	}
	
sub hydrophobic{
	my $score = shift;
	if (!defined $SCORE{"hydrophobic"})
		{
		$SET{"hydrophobic"} = ["h", [ "14" ]];
		$SCORE{"hydrophobic"} = $score
		}
	else
		{
		$SCORE{"hydrophobic"} = $SCORE{hydrophobic} + $score;
		}
	}



sub aromatic
	{
	my $score = shift;
	if (!defined $SCORE{"aromatic"})
		{
		$SET{"aromatic"} = ["a", [ "4" ]];
		$SCORE{"aromatic"} = $score
		}
	else
		{
		$SCORE{"aromatic"} = $SCORE{aromatic} + $score;
		}
	&hydrophobic($score)
	}

sub aliphatic
	{
	my $score = shift;
	if (!defined $SCORE{"aliphatic"})
		{
		$SET{"aliphatic"} = ["l", [ "3" ]];
		$SCORE{"aliphatic"} = $score
		}
	else
		{
		$SCORE{"aliphatic"} = $SCORE{aliphatic} + $score;
		}
	&hydrophobic($score);
	}

sub polar
	{
	my $score = shift;
	if (!defined $SCORE{"polar"})
		{
		$SET{"polar"} = ["p", [ "10" ]];
		$SCORE{"polar"} = $score
		}
	else
		{
		$SCORE{"polar"} = $SCORE{polar} + $score;
		}
	}

sub alcohol{	
  my $score = shift;
	$SCORE{"alcohol"} += $score;
	$SET{"alcohol"} = ["o", [ "2" ]] unless($SET{"alcohol"});
}



sub tiny 
	{
	my $score = shift;
	if (!defined $SCORE{"tiny"})
		{
		$SET{"tiny"} = ["u", [ "3" ]];
		$SCORE{"tiny"} = $score
		}
	else
		{
		$SCORE{"tiny"} = $SCORE{tiny} + $score;
		}

	&small($score);
	&turnlike($score);
	}

sub small
	{
	my $score = shift;
	if (!defined $SCORE{"small"})
		{
		$SET{"small"} = ["s", [ "9" ]];
		$SCORE{"small"} = $score
		}
	else
		{
		$SCORE{"small"} = $SCORE{small} + $score;
		}

	}

sub turnlike
	{
	my $score = shift;
	if (!defined $SCORE{"turnlike"})
		{
		$SET{"turnlike"} = ["t", [ "12" ]];
		$SCORE{"turnlike"} = $score
		}
	else
		{
		$SCORE{"turnlike"} = $SCORE{turnlike} + $score;
		}

	}

sub clustalHydro {
  my $score = shift;
  #WLVIMAFCYHP
  $SET{"clustalHydro"} = ["h", [ "11" ]] unless $SET{"clustalHydro"};
  $SCORE{"clustalHydro"} += $score;
}

#KR
sub clustalPositive {
	my $score = shift;
	$SET{"clustalPositve"} = ["+", [ "2" ]] unless($SET{"clustalPositive"});
	$SCORE{"clustalPositve"} += $score;
}

#QE
sub clustalBs{	
  my $score = shift;
	$SCORE{"clustalB"} += $score;
	$SET{"clustalB"} = ["b", [ "2" ]] unless($SET{"clustalB"});
}

#TS
sub clustalAlcohol{	
  my $score = shift;
	$SCORE{"clustalAlcohol"} += $score;
	$SET{"clustalAlcohol"} = ["o", [ "2" ]] unless($SET{"clustalAlcohol"});
}

#ED
sub clustalNegative {
	my $score = shift;
	$SET{"clustalNegative"} = ["-", [ "2" ]] unless($SET{"clustalNegative"});
	$SCORE{"clustalNegative"} += $score;
}

sub arbitrate {
    my ($threshold) = @_;
    my ($bestclass, $bestscore) = ("any", 0);
    #choose smallest class exceeding threshold and
    #highest percent when same size
    foreach my $class (keys %SCORE) {
	   $SCORE{$class} = ((100.0 * $SCORE{$class}) /@ID);
		}
 	
	foreach my $class (keys %SCORE) 
		{
  
        if ($SCORE{$class} >= $threshold) {
		my $a = 	${$SET{$class}[1]}[0];
		my $b = ${$SET{$bestclass}[1]}[0];
            #this set is worth considering further
            if ($a< $b) {

                #new set is smaller: keep it
                $bestclass = $class;
                $bestscore = $SCORE{$class};

            } elsif ($a == $b) {

                #sets are same size: look at score instead
                if ($SCORE{$class} > $bestscore) {

                    #new set has better score
                    $bestclass = $class;
                    $bestscore = $SCORE{$class};
                }
                
            }
        }
    }
    if ($bestclass) {
        $CONCENSUS{$threshold} .= $SET{$bestclass}[0];
    } else {
        $CONCENSUS{$threshold} .= $SET{"any"}[0];       
    }
}

sub print_long {
    my ($c) = @_;
    my $class;
    print ${$ALIGNMENT{$ID[0]}}[$c];
    foreach $class (sort keys %SET) {
        printf " %s=%5.1f", class($SET{$class}), $SCORE{$class};
    }
    print "\n";
}

sub print_sets {
    my ($class, $res);
    my %full_set;

$full_set{G}           = ['G', [ "G" ]];
$full_set{A}           = ['A', [ "A" ]];
$full_set{I}           = ['I', [ "I" ]];
$full_set{V}           = ['V', [ "V" ]];
$full_set{L}           = ['L', [ "L" ]];
$full_set{M}           = ['M', [ "M" ]];
$full_set{F}           = ['F', [ "F" ]];
$full_set{Y}           = ['Y', [ "Y" ]];
$full_set{W}           = ['W', [ "W" ]];
$full_set{H}           = ['H', [ "H" ]];
$full_set{C}           = ['C', [ "C" ]];
$full_set{P}           = ['P', [ "P" ]];
$full_set{K}           = ['K', [ "K" ]];
$full_set{R}           = ['R', [ "R" ]];  
$full_set{D}           = ['D', [ "D" ]];
$full_set{E}           = ['E', [ "E" ]];
$full_set{Q}           = ['Q', [ "Q" ]];
$full_set{N}           = ['N', [ "N" ]];
$full_set{S}           = ['S', [ "S" ]];
$full_set{T}           = ['T', [ "T" ]];
                      
$full_set{"aromatic"}    = ['a', [ qw(F Y W H) ]];
$full_set{"aliphatic"}   = ['l', [ qw(I V L )]];
$full_set{"hydrophobic"} = ['h', [ qw(A C F G H I K L M R T V W Y)]];
                      
$full_set{"positive"}    = ['+', [ qw(H K R) ]];
$full_set{"negative"}    = ['-', [ qw(D E) ]];
$full_set{"charged"}     = ['c', [ qw(H K R D E) ]];
                      
$full_set{"polar"}       = ['p', [ qw(H K R D E Q N S T C)]];
$full_set{"alcohol"}     = ['o', [ qw(S T)]];
                      
$full_set{"tiny"}        = ['u', [ qw(G A S) ]];
$full_set{"small"}       = ['s', [ qw(G A S V T D N P C) ]];
$full_set{"turnlike"}    = ['t', [ qw(G A S H K R D E Q N T C)]];

$full_set{"any"}         = ['.', [ qw(G A V I L M F Y W H C P K R D E Q N S T) ]];
    printf STDERR "    %-15s %-3s  %s\n", 'class', 'key', 'residues';
    foreach $class (sort keys %full_set) {
        printf STDERR "    %-15s %-3s  ", $class, $full_set{$class}[0];
        print STDERR join(",", sort @{$full_set{$class}[1]}), "\n";
    }
}

sub _readBlosum{
  print "In Blosum\n";
  my $matrixRef = shift;
  my @aa = qw( A  R  N  D  C  Q  E  G  H  I  L  K  M  F  P  S  T  W  Y  V  B  Z  X * );
  while(<DATA>){
    my @row = split(/\s+/, $_);
    my $aa = shift @row;
    for(my $i = 0; $i <= $#row; $i++){
      $matrixRef->{$aa}->{$aa[$i]} = $row[$i]; 
    }
  }
}


1;
__DATA__
A  4 -1 -2 -2  0 -1 -1  0 -2 -1 -1 -1 -1 -2 -1  1  0 -3 -2  0 -2 -1  0 -4 
R -1  5  0 -2 -3  1  0 -2  0 -3 -2  2 -1 -3 -2 -1 -1 -3 -2 -3 -1  0 -1 -4 
N -2  0  6  1 -3  0  0  0  1 -3 -3  0 -2 -3 -2  1  0 -4 -2 -3  3  0 -1 -4 
D -2 -2  1  6 -3  0  2 -1 -1 -3 -4 -1 -3 -3 -1  0 -1 -4 -3 -3  4  1 -1 -4 
C  0 -3 -3 -3  9 -3 -4 -3 -3 -1 -1 -3 -1 -2 -3 -1 -1 -2 -2 -1 -3 -3 -2 -4 
Q -1  1  0  0 -3  5  2 -2  0 -3 -2  1  0 -3 -1  0 -1 -2 -1 -2  0  3 -1 -4 
E -1  0  0  2 -4  2  5 -2  0 -3 -3  1 -2 -3 -1  0 -1 -3 -2 -2  1  4 -1 -4 
G  0 -2  0 -1 -3 -2 -2  6 -2 -4 -4 -2 -3 -3 -2  0 -2 -2 -3 -3 -1 -2 -1 -4 
H -2  0  1 -1 -3  0  0 -2  8 -3 -3 -1 -2 -1 -2 -1 -2 -2  2 -3  0  0 -1 -4 
I -1 -3 -3 -3 -1 -3 -3 -4 -3  4  2 -3  1  0 -3 -2 -1 -3 -1  3 -3 -3 -1 -4 
L -1 -2 -3 -4 -1 -2 -3 -4 -3  2  4 -2  2  0 -3 -2 -1 -2 -1  1 -4 -3 -1 -4 
K -1  2  0 -1 -3  1  1 -2 -1 -3 -2  5 -1 -3 -1  0 -1 -3 -2 -2  0  1 -1 -4 
M -1 -1 -2 -3 -1  0 -2 -3 -2  1  2 -1  5  0 -2 -1 -1 -1 -1  1 -3 -1 -1 -4 
F -2 -3 -3 -3 -2 -3 -3 -3 -1  0  0 -3  0  6 -4 -2 -2  1  3 -1 -3 -3 -1 -4 
P -1 -2 -2 -1 -3 -1 -1 -2 -2 -3 -3 -1 -2 -4  7 -1 -1 -4 -3 -2 -2 -1 -2 -4 
S  1 -1  1  0 -1  0  0  0 -1 -2 -2  0 -1 -2 -1  4  1 -3 -2 -2  0  0  0 -4 
T  0 -1  0 -1 -1 -1 -1 -2 -2 -1 -1 -1 -1 -2 -1  1  5 -2 -2  0 -1 -1  0 -4 
W -3 -3 -4 -4 -2 -2 -3 -2 -2 -3 -2 -3 -1  1 -4 -3 -2 11  2 -3 -4 -3 -2 -4 
Y -2 -2 -2 -3 -2 -1 -2 -3  2 -1 -1 -2 -1  3 -3 -2 -2  2  7 -1 -3 -2 -1 -4 
V  0 -3 -3 -3 -1 -2 -2 -3 -3  3  1 -2  1 -1 -2 -2  0 -3 -1  4 -3 -2 -1 -4 
B -2 -1  3  4 -3  0  1 -1  0 -3 -4  0 -3 -3 -2  0 -1 -4 -3 -3  4  1 -1 -4 
Z -1  0  0  1 -3  3  4 -2  0 -3 -3  1 -1 -3 -1  0 -1 -3 -2 -2  1  4 -1 -4 
X  0 -1 -1 -1 -2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -2  0  0 -2 -1 -1 -1 -1 -1 -4 
* -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4 -4  1 

