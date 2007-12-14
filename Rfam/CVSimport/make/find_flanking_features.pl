#! /software/bin/perl -w

# A program to pull features from Mole/EMBL for the given EMBL ID (and coordinates)

use strict;
use Getopt::Long;
use DBI;
use Rfam;

# MySQL connection details.
#my $database = $Rfam::embl;
my $database = "embl_test";
my $host     = "cbi3";
my $user     = "genero";
my $dist     = 10000;
my ($start,$end,$name,$plusstrand,$minusstrand,$strand,$help);

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
    }
    elsif (defined($minusstrand)){
	$strand = -1;
    }
}
elsif ( (!defined($strand) && !defined($plusstrand) && !defined($minusstrand)) && defined($start) && defined($end) ) {
    if ($start<$end){
	$strand = 1;
	print "Fetching: $name\/$start\-$end\tstrand=$strand\n";
    }
    else {
	$strand = -1;
	print "Fetching: $name\/$start\-$end\tstrand=$strand\n";
	my $tmp = $start;
	$start = $end;
	$end = $tmp;
    }
}

my $query0 = qq(
           select entry.accession_version, dbxref.database_id, dbxref.primary_id, dbxref.secondary_id, dbxref.tertiary_id
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

my ($features0, $features1, $features2, $features3, $features4);
###########
# Prepare the query for execution.
my $sth0 = $dbh->prepare($query0);
 $sth0->execute($name);
my $res0 = $sth0->fetchall_arrayref;
foreach my $row (@$res0){
    $features0 .= $row->[0] . "\t" . $row->[1] . "\t" . $row->[2] . "\t" . $row->[3] . "\t" . $row->[4] . "\n";
}

#######
my $unfound=0;
if( !defined($features0) ) {
    $features0 = "no features available";
    $unfound = 1;
}

#my (@res1, @res2, @res3) = (@$res1, @$res2, @$res3);
my @features0 = split(/\n/, $features0);
for (my $i = 0; $i < scalar(@features0); $i++) { 
        
    if ($unfound){
	print "$i: $features0[$i]\n";
	last;
    }
    
    my ($sstart, $send); 
    if ($features0[$i] =~ m/\s+(\d+)\.\.(\d+)/){
	$sstart = $1;
	$send = $2;
    }
    elsif ($features0[$i] =~ m/\s+complement\((\d+)\.\.(\d+)\)/){
	$sstart = $1;
	$send = $2;	
    }
    
    if (defined($start) && defined($end) && defined($sstart) && defined($send)) {
	if ( (abs($start-$send)<$dist) || (abs($sstart-$end)<$dist)  ) {
	    my $sdist = minabs($send-$start, $sstart-$end);
	    print "$i: $features0[$i]\t$sdist\n";
	}
    }
    elsif (!defined($start) && !defined($end)) {
	print "$i: $features0[$i]\t$name\n";
    }
    
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
sub help {
    
    print STDERR <<EOF;

find_flanking_features.pl - reads .
                
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

To Add: 

Popular run modes:
On SEED:
grep \"/\" SEED | grep -v \"//\" | awk \'{print \" ~/scripts/make/find_flanking_features.pl -d 5000 -n \"\$1}\' | sh

On ALIGN2SEED:
grep \">\" ALIGN2SEED | tr -d \">\" | awk \'{print \" ~/scripts/make/find_flanking_features.pl -d 5000 -n \"\$1}\' | sh

EOF
}



