#!/usr/bin/env perl 

# get_taxinfo.pl - Create a tax info file from a PFAMOUT file
# usage get_taxinfo.pl -T <> -t <> -l2print <>
# if -T is specified but -t is not then sequence and domain GA are the same
# if neither -T or -t are specified get the GAs from DESC file and report what they are in the output
# -l2print is optional and gives the number of levels of taxonomy to print. Default is 7 ####is this appropriate######

use strict;
use warnings;
use Getopt::Long qw(:config no_ignore_case); #this makes Getopt case sensitive
use Data::Dumper;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use DDP;

#set up and connect to db
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#set up db query
my $st1 = $dbh->prepare("select taxonomy from pfamseq where pfamseq_acc = ?") or die "Failed to prepare statement 1:".$dbh->errstr."\n";
my $st2 = $dbh->prepare("select species from pfamseq where pfamseq_acc = ?") or die "Failed to prepare statement 2:".$dbh->errstr."\n";

#get options from command line with defaults if not set
my $l2print = 7;
my ($tdom, $tseq, $help);

GetOptions (
    'T=f' => \$tseq,
    't=f' => \$tdom,
    'l2print=i' => \$l2print,
    'h' => \$help
    );

if($help){
    &help;
    exit;
}

#check SEED, PFAMOUT and DESC file are present - if not then barf

if (!-e 'SEED' || !-e 'PFAMOUT' || !-e 'PFAMOUT'){
    print "Error: DESC, SEED and PFAMOUT must be present\n";
    &help;
    exit;
}

#if -t specified but not -T specified make them the same

if (defined $tdom && !defined $tseq){
    $tseq = $tdom;
}

#if -T specified but -t not then they should be the same

if (defined $tseq && !defined $tdom){
    $tdom = $tseq;
}

#if -t and -T not specified get them from DESC

if (!defined $tseq && !defined $tdom){
#parse desc
    print "-T and -t not specified - obtaining GA from DESC file\n";
    open (DESC, "DESC") or die "Can't open DESC file \n";
    while (<DESC>){
	if ($_ =~ /^GA\s+(\d+\.\d+)\s+(\d+\.\d+);/){
	    $tseq = $1;
	    $tdom = $2;
	}
    }
    close DESC;
}

#if -t is bigger than -T return an error
if ($tdom > $tseq){
    print "Error: domain threshold cannot be greater than sequence threshold\n";
    &help;
    exit;
}


##make seed hash
my %seed;
open (SEED, "SEED") or die "Can't open SEED file\n";
while (<SEED>){
    if ($_ =~/^(\w{6,10})\.\d+\/\d+/){
	my $acc = $1;
	$seed{$acc}=1;
    }
}

#full and other hashes from PFAMOUT file
my %full;
my %other;

#hashes for uniprot accession and sequence score and uniprot accession and maximum domain score
my %sequence;
my %domain;

open (PFAMOUT, "PFAMOUT") or die "Can't open PFAMOUT file\n";
while (<PFAMOUT>){
    my $line = $_;
#in sequence section line is acc, id, de, score
    if ($line =~/^(\w{6,10})\.\d+\s+\w{2,10}_\w+\s+/){
	my $acc = $1;
	my $score = 0;
#parse out bit score and add to sequence hash - messy as PFAMOUT not tab delimited
	if ($line =~/(\d+\.\d+)\s+\S+\s+\d+\s+\S+\s+\S+$/){
	    $score = $1;
	}
	$sequence{$acc}=$score;
    } #end of sequence section

#in domain section line is acc, env_s, env_e....
    if ($line =~/^(\w{6,10})\.\d+\s+\d+\s+\d+\s+\d+\s+\d+\s+\d+\s+\d+\s+(\d+\.\d+)/){
	my $acc = $1;
	my $score = $2;
#add highest score to domain hash - the highest score will be the first one seen
	if (!exists $domain{$acc}){
	    $domain{$acc}=$score;
	}
    }#end of domain section


} #end of loop through PFAMOUT

#loop through the sequence hash and for every acc check to see if it is in seed. 
#If not check bit score:
#add to full if sequence >= T and domain >= t
#add to other if sequence or domain below threshold but sequence is still >= 18
#values of hashes are the maximum -t (domain bit score) for that uniprot
foreach my $uacc (keys %domain){
#is it in the seed?
    if (exists $seed{$uacc}){
#if so find domain bitscore and replace value in the seed hash
	my $dombit = $domain{$uacc};
	$seed{$uacc}=$dombit;
    } else {
#otherwise put in full or other
	my $seqbit = $sequence{$uacc};
	my $dombit = $domain{$uacc};
#should it be in full?
	if ($seqbit >= $tseq && $dombit >= $tdom){
	    $full{$uacc}=$dombit;
	} elsif ($seqbit >= 18){
#if not in full but seqbit >=18 put in other
	    $other{$uacc}=$dombit;
	}
    }#end of if not in seed
}#end of loop through domain hash

#numbers of seqs in seed, full, other
my $seed_no = keys %seed;
my $full_no = keys %full;
my $other_no = keys %other;

#query pfamlive to get a tax string for each and add to new hash

my %seedtax = %{make_tax_hash($st1, $l2print, %seed)};
my %fulltax = %{make_tax_hash($st1, $l2print, %full)};
my %othertax = %{make_tax_hash($st1, $l2print, %other)};

#create hashes for taxstring and number of times found (3 hashes - seed, full, other)
my %seed_tax_count = %{make_tax_count_hash(%seedtax)};
my %full_tax_count = %{make_tax_count_hash(%fulltax)};
my %other_tax_count = %{make_tax_count_hash(%othertax)};

#create hashes for taxstring and minimum t (3 hashes, seed, full, other)
my %seed_tax_max_t = %{make_max_score_hash(\%domain, \%seedtax)};
my %full_tax_max_t = %{make_max_score_hash(\%domain, \%fulltax)};
my %other_tax_max_t = %{make_max_score_hash(\%domain, \%othertax)};

#print pretty output to file
open (OUTFILE, ">taxinfo") or die "Cannot open taxinfo file to write\n";

#print heading
print OUTFILE "# taxinfo file\n# Sequence GA = $tseq\n# Domain GA = $tdom\n# SEED:\t\t$seed_no\n# FULL:\t\t$full_no\n# OTHER:\t$other_no\n#\n#\n";
#allow 20 characters per level printed
my $n = 20 * $l2print;

#table heading
printf OUTFILE "%-*s %16s %13s %13s \n", $n, ' ', 'SEED', 'FULL', 'OTHER'; 
my $header = "Taxonomy string prefix: $l2print levels";
printf OUTFILE "%-*s %-5s %6s %6s %6s %6s %6s %6s \n", $n, $header, 'mem', 'ct', 'max t', 'ct', 'max t', 'ct', 'max t';

for (my $i=0; $i<($n+48); $i++){
    print OUTFILE "=";
}
print OUTFILE "\n";

#print seed section
#sort by value
my @seedkeys = sort {$seed_tax_max_t{$b} <=> $seed_tax_max_t{$a}} keys %seed_tax_max_t;

foreach my $taxa (@seedkeys){
#populate a string for mem (SFO)
    my $mem = find_mem($taxa, \%seed_tax_count, \%full_tax_count, \%other_tax_count);
#array for counts and max
    my @cm = find_count_and_max ($taxa, \%seed_tax_count, \%seed_tax_max_t, \%full_tax_count, \%full_tax_max_t, \%other_tax_count, \%other_tax_max_t);
#print line
    printf OUTFILE "%-*s %-5s %6s %6s %6s %6s %6s %6s \n", $n, $taxa, $mem, $cm[0], $cm[1], $cm[2], $cm[3], $cm[4], $cm[5];
} #end of print seed section

print OUTFILE "#######\n";

#print full section
#sort by value
my @fullkeys = sort {$full_tax_max_t{$b} <=> $full_tax_max_t{$a}} keys %full_tax_max_t;

foreach my $taxa (@fullkeys){
#exlude any in seed
    if (!exists $seed_tax_max_t{$taxa}){
#populate a string for mem (SFO)
	my $mem = find_mem($taxa, \%seed_tax_count, \%full_tax_count, \%other_tax_count);
#array for counts and max
	my @cm = find_count_and_max ($taxa, \%seed_tax_count, \%seed_tax_max_t, \%full_tax_count, \%full_tax_max_t, \%other_tax_count, \%other_tax_max_t);
#print line
	printf OUTFILE "%-*s %-5s %5s %6s %6s %6s %6s %6s \n", $n, $taxa, $mem, $cm[0], $cm[1], $cm[2], $cm[3], $cm[4], $cm[5];
    }

} #end of print full section

print OUTFILE "#######\n";

#print other section
#sort by value
my @otherkeys = sort {$other_tax_max_t{$b} <=> $other_tax_max_t{$a}} keys %other_tax_max_t;

#loop through sorted keys
foreach my $taxa (@otherkeys){
#exclude any in seed or full
    if (!(exists  $seed_tax_max_t{$taxa} or exists $full_tax_max_t{$taxa})){
#populate a string for mem (SFO)
	my $mem = find_mem($taxa, \%seed_tax_count, \%full_tax_count, \%other_tax_count);
#array for counts and max
	my @cm = find_count_and_max ($taxa, \%seed_tax_count, \%seed_tax_max_t, \%full_tax_count, \%full_tax_max_t, \%other_tax_count, \%other_tax_max_t);
#print line
	printf OUTFILE "%-*s %-5s %5s %6s %6s %6s %6s %6s \n", $n, $taxa, $mem, $cm[0], $cm[1], $cm[2], $cm[3], $cm[4], $cm[5];
    } #end of if exists

} #end of print other section

#print end of outfile
for (my $i=0; $i<($n+48); $i++){
    print OUTFILE "=";
}
print OUTFILE "\n#\n# Explanation of table above:\n";
print OUTFILE "# Listed above are counts of hits in various taxonomic groups for the\n";
print OUTFILE "# three categories of hits (SEED, FULL, OTHER, defined below), for the selected GA threshold. \n";
print OUTFILE "# Definition of the three hit categories:\n";
print OUTFILE "#   [S]EED:  seed sequences\n";
print OUTFILE "#   [F]ULL:  sequences above current GA\n";
print OUTFILE "#   [O]THER: sequences below GA, with sequence bit score >= 18\n";
print OUTFILE "# Column abbreviations:\n";
print OUTFILE "#   'mem'  column:  three letter summary of which groups have at least 1 hit in this taxonomic group\n";
print OUTFILE "#   'ct'   columns: number of hits per hit category for this taxonomic group ('-' if none)\n";
print OUTFILE "#   'max t' columns: maximum domain bit score of all hits in this category and taxonomic group ('-' if none)\n";
close OUTFILE;

#print an output to a spcies file - acc, s/f/o, max t, spp, tax

open (SPECIES, ">species") or die "Cannot open species file to write\n";
print SPECIES "Acc         S/F/O   T     Species                                            Taxonomy\n";
for (my $i=0;$i<150; $i++){
    print SPECIES "=";
}
print SPECIES "\n";

my %species;
foreach my $uniprot (keys %sequence){
#want those with sequence score of >=18 ordered by score
    if ($sequence{$uniprot} >= 18){
	$st2->execute($uniprot) or die "Couldn't execute statement ".$st2->errstr."\n";
	my $array_ref = $st2->fetchall_arrayref();
	foreach my $row (@$array_ref) {
	    my $spp = $row->[0];
	$species{$uniprot}=$spp;
	} #end of loop through results array
    }
}
#sort by score
my @keyspp = sort {$sequence{$b} <=> $sequence{$a}} keys %sequence;
#print output
foreach my $accn (@keyspp){
    if ($sequence{$accn} >= 18){
#is it in seed, full or other?
#fine taxonomy
	my $sfo;
	my $tax;
	if (exists $seed{$accn}){
	    $sfo = 'SEED';
	    $tax = $seedtax{$accn};
	} elsif (exists $full{$accn}){
	    $sfo = 'FULL';
	    $tax = $fulltax{$accn};
	} elsif (exists $other{$accn}){
	    $sfo = 'OTHER';
	    $tax = $othertax{$accn};
	}

#some may not be in seed, full or other (eg in repeats where a sequence bit score of >18 can be obtained with no domain score being present) 
#so only print those in s, f or o
	if ($sfo){
#truncate species to print
	my $sppshort = substr($species{$accn}, 0, 47);

	printf SPECIES  "%-11s %-6s %-6.1f %-50s %-100s \n", $accn, $sfo, $sequence{$accn}, $sppshort, $tax;
	}
    }#end of if score > 18
}#end of loop through keys array


close SPECIES;
#disconnect from db
$dbh->disconnect;

###################
###subroutines#####
###################

#make_tax_hash = makes hash of acc and tax string
sub make_tax_hash {
    my %hash_out;
    my ($st1, $level, %hash_in) = @_;
#loop through input hash
    foreach my $acc (keys %hash_in){
#create taxstring and populate from database
	my $taxstring;
	$st1->execute($acc) or die "Couldn't execute statement ".$st1->errstr."\n";
	my $array_ref = $st1->fetchall_arrayref();
	foreach my $row (@$array_ref) {
	    $taxstring = $row->[0];
	} #end of loop through results array
#split the taxstring on semi colon and put into array
	my @taxarray = split (/;/, $taxstring);

#create new string from the taxarray, joining with semi colon. Use only the number in l2print.
	my @tax_parsed;
	for (my $i = 0; $i<$level; $i++){
	    if ($taxarray[$i]){
		push (@tax_parsed, $taxarray[$i]);
	    }
	}

#join the tax parsed array to make a string
	my $taxjoined = join (';', @tax_parsed);
#add to seedtax hash
	$hash_out{$acc}=$taxjoined;

    } #end of loop through accessions in seed

    return \%hash_out;
} #end of make_tax_hash subroutine

#make_tax_count_hash makes hash of taxstring and number of occurances
sub make_tax_count_hash {
    my %hash_out;
    my (%hash_in) = @_;
#loop throgh input hash
    foreach my $acc (keys %hash_in){
	my $taxstring = $hash_in{$acc};
	if (!exists $hash_out{$taxstring}){
	    $hash_out{$taxstring}=1;
	} else {
	    $hash_out{$taxstring}++;
	}
    }
    return \%hash_out;
} #end of make_tax_count_hash subroutine

#make_max_score_hash makes hash of tax string and maximum domain bit score
sub make_max_score_hash {
    my %hash_out;
    my %score_hash = %{$_[0]};
    my %string_hash = %{$_[1]};
#loop through each acc and get taxstring and score
    foreach my $acc (keys %string_hash){
	my $string = $string_hash{$acc};
	my $score = $score_hash{$acc};
    if ($score){
	        if (!exists $hash_out{$string}){
	         $hash_out{$string} = $score;
        	} elsif ($hash_out{$string} < $score) {
	         $hash_out{$string} = $score;
	        }
        }
    } 
    return \%hash_out;
} #end of make max score hash

#find_mem to create the mem string for the printed output
sub find_mem {
    my $memstr;
    my $tax = $_[0];
    my %seed = %{$_[1]};
    my %full = %{$_[2]};
    my %other = %{$_[3]};
    my $inseed = '-';
    my $infull = '-';
    my $inother = '-';
    if (exists $seed{$tax}){
	$inseed = 'S';
    }
    if (exists $full{$tax}){
	$infull = 'F';
    }
    if (exists $other{$tax}){
	$inother = 'O';
    }
    $memstr = $inseed . $infull . $inother;
    return $memstr;
}

#find_count_and_max finds the count and max t value of each tax string - 
#return an array - seed count, seed max, full count, full max, other count, other max
sub find_count_and_max {
    my @results;
    my $tax = $_[0];
    my %seed_c = %{$_[1]};
    my %seed_t = %{$_[2]};
    my %full_c = %{$_[3]};
    my %full_t = %{$_[4]};
    my %other_c = %{$_[5]};
    my %other_t = %{$_[6]};

#seed
    my $seed_count = '-';
    my $seed_max = '-';
    if (exists $seed_c{$tax}){
	$seed_count = $seed_c{$tax};
	    if (exists $seed_t{$tax}){
            $seed_max = $seed_t{$tax};
        }
    }
#full
    my $full_count = '-';
    my $full_max = '-';
    if (exists $full_c{$tax}){
	$full_count = $full_c{$tax};
	$full_max = $full_t{$tax};
    }
    my $other_count = '-';
    my $other_max = '-';
    if (exists $other_c{$tax}){
	$other_count = $other_c{$tax};
	$other_max = $other_t{$tax};
    }

    push (@results, ($seed_count, $seed_max, $full_count, $full_max, $other_count, $other_max));
    return @results;
}


sub help{
    print "****** get_taxinfo.pl - Create a tax info file from a PFAMOUT file ******\n";
    print "Usage:\n\tget_taxinfo.pl -T <> -t <> -l2print <>\n";
    print "Options:\n";
    print "\t-T\t\tSequence threshold (bits)\n";
    print "\t-t\t\tDomain threshold (its)\n";
    print "\t-l2print\tTaxonomic levels to print (default is 7). Must be an integer\n";
    print "If only -T or only -t are specified the sequence and domain thresholds are both set to the same value\n";
    print "If neither -T or -t are specified the current thresholds are taken from the DESC file\n";
    print "To be run in the family directory. Requires DESC, PFAMOUT and SEED files to exist\n";
}
