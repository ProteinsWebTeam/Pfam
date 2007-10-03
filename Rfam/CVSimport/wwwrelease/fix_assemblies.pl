#!/software/bin/perl -w

#hack of rfam2chr by jen to fix the genome_assemblies.rgp output.
#need entries for those with no GP in for the genome hits to be generated,
#need the genoem_assmeblies.rgp file and generates GP lines for those that are not
#made of contigs. Apparently the downstream code (rfam2chr) needs this.


use strict;

my $agp      = shift;
my $rfamfull = shift;


my @genomes;
{
#split the AGP file into entries
    local $/="//\n";
    open( IN, $agp ) or die;
    @genomes=<IN>;
    close (IN);
}


my %agp;
my $chr;

my $count=0;

open( HIT, "gunzip -c $rfamfull|" ) or die;
my @hits=<HIT>;
close (HIT);


open (OUT, ">genome_assemblies.rgp_fixed") || die "cant open the outfile $!";
foreach my $entry (@genomes){
    my @lines=split("\n", $entry);
    my @AC=grep{/^AC/} @lines;
    $chr=$AC[0];
    $chr=~ s/^AC\s+//g;
    my @GP=grep{/^GP/} @lines;

   
##DEAL WITH THOSE WITH GP LINES
    if (@GP > 0){
	print OUT $entry;
    } #end of gp>0

##DEAL WITH THOSE WITHOUT GP MAPPINGS
    if (@GP ==0){
	my @length=grep{/^LE/} @lines;
	my $l=$length[0];
	$l=~ s/^LE\s+//;
   	my @matches=grep{/$chr/} @hits;
        #if (@matches==0) { print  "no matches for $chr in rfam.full"};
	if (exists $matches[0]){
	    my $string=$matches[0];
	    if ($string=~ /^(\S+\.\d+)\/\d+\-\d+\s+/){
		my $version=$1;
		$lines[6]="GP   1\t$l\t$version\t1\t$l\t+";
		$lines[7]="//";
	    }
	}else {
	    print "WARNING: cant get the accesion from the rfam.full file for $chr\n";}
	   
	print OUT join("\n", @lines), "\n";;
	
	
    }# end of gp==0
       
}
close OUT;
