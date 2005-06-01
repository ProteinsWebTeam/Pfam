#!/usr/local/bin/perl -w
use Getopt::Long;
my ($outfile, $robust);
&GetOptions( "outfile=s" => \$outfile,
	     "robust=s"  => \$robust,
	     "h"         => \$help );
sub help {
    print STDERR <<EOF;
 
pfold_wrapper.pl - Script to run pfold without having to copy things to /pfam/db manually
 
Usage:   pfold_wrapper.pl <FILE> -outfile <OUTPUTFILE> -robust <VALUE> (optional)
Options:       -h              show this help
               -outfile        name of output file to be written to
	       -robust         "robust" value for pfold - default is 0.05
EOF
exit;
}
if ($help){
&help;
}
my $input = shift;
if (!$input){
print STDERR "No input specified - Exiting!\n";
&help;
}
my $dir = `pwd`;
if (!$robust){
$robust = 0.05;
}
if (!$outfile){
print STDERR "No output specified - Exiting!\n";
&help;
}
chomp ($input, $dir, $outfile, $robust);
system "cp $input /pfam/db/Rfam/tmp/pfolds/in.$$";
chdir "/pfam/db/Rfam/tmp/pfolds/";
system "bsub -q pfam_fast -Rlinux -I \"pfold.pl -robust $robust in.$$ > $outfile.$$";
system "cp $outfile.$$ $dir/$outfile";
unlink "in.$$";
unlink "$outfile.$$";

