#!/usr/bin/perl -w

use strict;

# The main body of the program starts here 


# Get the user defined environment variables
if (!@ARGV) {
    print STDERR "usage: $0 alignment_file [threshold%]...\n";
    print_sets();
    exit 0;
}
my $FILE       = shift @ARGV;
my @THRESHOLD;
if (@ARGV) {
    @THRESHOLD = @ARGV;
} else {
    @THRESHOLD = (90, 80, 70, 60, 50);
}

# Set up of global variables

my %SET; 
my @ID;
my %ALIGNMENT;
my %SCORE;
my $LENGTH;
my %CONCENSUS;



read_alignment($FILE);
printf "%-20s %s\n",          $ID[0], join("", @{$ALIGNMENT{$ID[0]}});
my $threshold; foreach $threshold (reverse sort @THRESHOLD) {
    compute_consensus($threshold);
    my $format = sprintf "%d%%", $threshold;
    printf "%-9s/%-9s  %s\n", 'consensus', $format, $CONCENSUS{$threshold};
}


sub read_alignment {
    my ($file) = @_;
    my ($id, %alignment);
    open(TMP, "$file") or die "can't open file '$file'\n";
    while (<TMP>) {

        if ($_ =~ /^(\S+)\s+([-a-zA-Z*.]+) *$/) {
            if (! $alignment{$1}) {
                #new sequence identifier
                push @ID, $1;
            }

            #strip spaces,tabs,newlines: extend alignment array
            $_ = $2;
            $_ =~ tr/ \t\n//d;
            push @{$ALIGNMENT{$1}}, split("", $_);
        }
	else
		{
		warn "Could not parse $_ !!!!\n";
		} 

    }
    close TMP;
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
    my ($threshold) = @_;
    my ($column) = [];
    my $aa;
    my $c;
   	for ($c = 0; $c < $LENGTH; $c++) {
	# This is where my changes really kick in.......
        
		my %column_aa;
	foreach my $id (@ID) 
		{
        	if (${$ALIGNMENT{$id}}[$c] !~ /(\.|-)/)
			{
			$aa = ${$ALIGNMENT{$id}}[$c];
			$column_aa{$aa}++;
			}
        	}
	
        if (%column_aa and  keys(%column_aa)) {
		%SET = ();
		%SCORE = ();
		tally_column(\%column_aa);
            	arbitrate($threshold);
        } else {
	    # This appears to be used when given an alignment column of
	    # only gaps.  Weird huh!
            $CONCENSUS{$threshold} .= '.';
        }
    }
}
 
sub tally_column {
    my ($column) = @_;  
    my ($class, $score);
	foreach my $aa (keys %{$column})
		{
		my $score = $$column{$aa};
		$aa = ucfirst($aa);
		if (!defined $SET{$aa})
			{
			$SET{$aa} = ["$aa", [ "1" ]];
			$SCORE{$aa} = $score;
			}	
		else 
			{
			$SCORE{$aa} = $SCORE{$aa} + $score ;
			}
			}
	
	my @keys = keys %SCORE ;
	foreach $class (@keys)
		{
		if (!defined $SCORE{"any"})
			{
			$SET{"any"} = [".", [ "20" ]];
			$SCORE{"any"} = $SCORE{$class};
			}
		else 
			{
			$SCORE{"any"} = $SCORE{"any"} + $SCORE{$class};
			}

		if($class eq  "S")
			{
			&alcohol($SCORE{$class});
			&polar($SCORE{$class});
			&tiny($SCORE{$class});
			}
		elsif ($class eq "T")
			{
			&alcohol($SCORE{$class});
			&small($SCORE{$class});
			&turnlike($SCORE{$class});
			&polar($SCORE{$class});
			&hydrophobic($SCORE{$class});
			}
		elsif ($class eq "I")
			{
			&aliphatic($SCORE{$class});
			}
		elsif ($class eq "L")
			{
			&aliphatic($SCORE{$class});
			}
		elsif ($class eq "V")
			{
			&aliphatic($SCORE{$class});
			&small($SCORE{$class});
			}
		elsif ($class eq "A")
			{
			&hydrophobic($SCORE{$class});
			&tiny($SCORE{$class});
			}
		elsif ($class eq "C")
			{
			&hydrophobic($SCORE{$class});
			&polar($SCORE{$class});
			&small($SCORE{$class});
			&turnlike($SCORE{$class});
			}
		elsif ($class eq "D")
			{
			&negative($SCORE{$class});
			&small($SCORE{$class});
			&turnlike($SCORE{$class});
			}
		elsif ($class eq "E")
			{
			&negative($SCORE{$class});
			&turnlike($SCORE{$class});
			}
		elsif ($class eq "F")
			{
			&aromatic($SCORE{$class});
			}
		elsif ($class eq "G")
			{
			&tiny($SCORE{$class});
			&hydrophobic($SCORE{$class});
			}
		elsif ($class eq "H")
			{
			&aromatic($SCORE{$class});
			&positive($SCORE{$class});
			&turnlike($SCORE{$class});
			}
		elsif ($class eq "K")
			{
			&positive($SCORE{$class});
			&turnlike($SCORE{$class});
			&hydrophobic($SCORE{$class});
			}
		elsif ($class eq "M")
			{
			&hydrophobic($SCORE{$class});
			}
		elsif ($class eq "N")
			{
			&polar($SCORE{$class});
			&small($SCORE{$class});
			&turnlike($SCORE{$class});
			}
		elsif ($class eq "P")
			{
			&small($SCORE{$class});
			}
		elsif ($class eq "Q")
			{
			&polar($SCORE{$class});
			&turnlike($SCORE{$class});
			}
		elsif ($class eq "R")
			{
			&positive($SCORE{$class});
			&turnlike($SCORE{$class});
			&hydrophobic($SCORE{$class});
			}
		elsif ($class eq "W")
			{
			&aromatic($SCORE{$class});
			}
		elsif ($class eq "Y")
			{
			&aromatic($SCORE{$class});
			}
		elsif ($class ne "X")
			{
			warn "unrecognised aa $class\n";
			}
		}
}


sub negative
	{
	my $score = shift;
	if (!defined $SCORE{"negative"})
		{
		$SET{"negative"} = ["-", [ "2" ]];
		$SCORE{"negative"} = $score
		}
	else
		{
		$SCORE{"negative"} = $SCORE{negative} + $score;
		}
	&charged($score);
	}

sub positive
	{
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
	
sub hydrophobic
	{
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
sub alcohol
	{	
	my $score = shift;
	if (!defined $SCORE{"alcohol"})
		{
		$SET{"alcohol"} = ["o", [ "2" ]];
		$SCORE{"alcohol"} = $score
		}
	else
		{
		$SCORE{"alcohol"} = $SCORE{alcohol} + $score;
		}
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

