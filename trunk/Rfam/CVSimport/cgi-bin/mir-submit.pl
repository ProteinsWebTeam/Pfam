#!/usr/local/bin/perl -T

use strict;
use CGI;

$| = 1;

$ENV{'PATH'} = '/bin:/usr/bin';
my $mailto = "sgj\@sanger.ac.uk";

my( $hex, $tmpfile );

{
    $hex = sprintf "%lx", time();
    $tmpfile = "/nfs/disk100/pubseq/Pfam/temp/mir-submit/$hex.sub";
    if( -s $tmpfile ) {
	redo;
	sleep 2;
    }
}

open( TMP, ">$tmpfile" ) or die;

my $query = new CGI;
print $query -> header("text/html");
my @authors;
my $i = 1;
my $surname;
do {
    $surname = $query -> param( "surname$i" );
    my $initials = $query -> param( "initials$i" );
    $initials =~ s/\s+//g;
    push( @authors, "$initials $surname" ) if $surname;
    $i++;
} while( $surname );

my $correspond = $query -> param( "correspond" );
my $email      = $query -> param( "email" );
my $state      = $query -> param( "state" );
my $title      = $query -> param( "title" );

if( scalar(@authors) < 1 ) {
    print "<h4>You must specifiy at least one author.</h4>\n";
    die;
}
if( not $authors[$correspond] ) {
    print "<h4>You must specify a corresponding author.</h4>\n";
    die;
}
if( not $email ) {
    print "<h4>You must give a valid email address.</h4>\n";
    die;
}
if( $email !~ /\@/ ) {
    print "<h4>Your email address doesn't seem to be valid.</h4>\n";
    die;
}
if( not $title ) {
    print "<h4>You must specify a title for you manuscript.</h4>\n";
    die;
}
if( not $state ) {
    print "<h4>You must specify the state of your manuscript.</h4>\n";
    die;
}
if( $state < 3 ) {
    print "<h4>I'm sorry -- to avoid accidental duplication of names, the Registry will only assign names once a manuscript describing the miRNAs has been accepted for publication.  Please contact $mailto if you wish to discuss preliminary names. $state</h4>\n";
    die;
}

print TMP "Corresponding author: $authors[$correspond]\n";
print TMP "Email: $email\n\n";

if( my $title = $query -> param( "title" ) ) {
    print TMP "Reference: $title\n";
}
print TMP "Authors: ";

for( my $j=0; $j<scalar(@authors); $j++ ) {
    if( $j == scalar(@authors) - 1 and scalar(@authors) > 1 ) {
	print TMP "and $authors[$j]";
    }
    elsif( $j == scalar(@authors) - 2 or scalar(@authors) == 1 ) {
	print TMP "$authors[$j] ";
    }
    else {
	print TMP "$authors[$j]\, ";
    }
}

print TMP "\n";

my %states = ( "1" => "in preparation",
	       "2" => "in submission",
	       "3" => "accepted",
	       "4" => "in press",
	       "5" => "published" );

print TMP "State of manuscript: $states{$state}\n";

if( my $pubmed = $query -> param( "pubmed" ) ) {
    print TMP "Pubmed/Medline ID: $pubmed\n";
}

print TMP "\n";

my( $taxon, $ncbi );
print TMP "Number of sequences: ", $query -> param( "numseqs" ), "\n";
if( $taxon = $query -> param( "taxon" ) ) {
    print TMP "Organism: $taxon\n";
}
elsif( $ncbi = $query -> param( "taxonid" ) ) {
    print TMP "NCBI taxon ID: $ncbi\n";
}

if( not $taxon or $ncbi ) {
    print "<h4>You must specify either a species from the drop down box, or an NCBI taxon ID.</h4>\n";
    die;
}

if( my $other = $query -> param( 'other' ) ) {
    print TMP "\nOther information: $other\n";
} 

print TMP "\nFasta format sequence file follows:\n\n-------\n\n";
my $file = $query -> upload( 'fastafile' );
if( not $file ) {
    print "<h4>You must specify a file to upload</h4>\n";
    die;
}
my $count;
while( <$file> ) {
    $count ++ if( /^\>/ );
    print TMP "$_";
}
close TMP;

unless( $count ) {
    print "<h4>I couldn't understand your fasta file.<br>Please make sure it is in plain text format, and contains at least one sequence.<br>Email $mailto if you continue to have trouble.</h4>\n";
    die;
}

#if( $count != $query -> param( "numseqs" ) ) {
#    print "<h4>The number of sequences in your file did not seem to match the number you've specified.  Please check and try again.<br>Please email $mailto if you continue to have trouble.</h4>\n";
#    die;
#}

sleep 2;     # to eliminate race condition;

if( not -s "$tmpfile" ) {
    print "<h4>Sorry - something has gone wrong with your submission - please try again.\n";
    print "Email $mailto if you continue to have trouble.</h4>\n";
    die;
}

print "<h3>Submission number: $hex</h3>";
print "<b>The information you submitted is below.  A copy of this information has been emailed to $email.  Further contact regarding your submission and miRNA names will be via email.  <br><br>Please email <a href='mailto:$mailto'>$mailto</a> if you have any queries or further information about your submission, or if you don't hear from us within a few days.</b>";
print "<PRE>\n";
my $output;
open( T, $tmpfile ) or die;
while(<T>) {
    print;
	$output .= $_;
}
print "</pre>\n";

my $tmp = $1 if ($email =~  /^([-\_\@\w.]+)$/);
open( MAIL, "| Mail -r microrna\@sanger.ac.uk -s 'miRNA submission number $hex'  $tmp, $mailto") or die "Can't fork for sendmail: $!\n";
print MAIL <<EOT;
This is confirmation of your submission to the miRNA Registry.  Your submission number is $hex.  
Please contact $mailto if you have any questions about this service.\n\n-------\n\n
$output
-------\nEnd of message
EOT

close MAIL;


#Subject: miRNA submission number $hex
#This is confirmation of your submission to the miRNA Registry.  Your submission number is $hex.  Please contact $mailto if you have any questions about this service.\n\n-------\n\n
#$output
#-------\nEnd of message
#EOT
