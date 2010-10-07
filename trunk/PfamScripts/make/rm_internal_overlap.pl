#! /usr/local/bin/perl -w

# A dumb script to remove internal overlaps!

my $align = shift @ARGV;

if (! $align){
    print STDERR <<"EOF";
Usage: $0 <align>

This program will take a stockholm/mul format alignment and remove any
overlapping segments and print the result to STDOUT. The programme
simply throws away the first one of each overlapping pair. This may
not be the optimal strategy!

EOF
exit 0;
}

system ("belvu -S a -o mul $align > $$.align");

open (FH, "$$.align")or die "cannot open sorted $$.align file";
my ($id, $from, $to, $old_from, $old_to);
my $old_id="";
while(<FH>){
    my $line=$_;

    next if (/\/\//);
    if (/^(\S+)\/(\d+)-(\d+)/){
	if ($id){
	    $old_id=$id;
	    $old_from=$from;
	    $old_to=$to;
	}

	$id=$1;
	$from=$2;
	$to=$3;

	if ($old_id eq $id){ # Domain in same sequence. Check for overlap
	    if ($old_to>=$from){
		print STDERR "Removing $id/$from-$to which overlaps with $old_id/$old_from-$old_to\n";

		# Don't print out there is an overlap
	    } else {
		print $line;
	    }
	} else { # Different IDs so cannot overlap. Also catches first line!
		print $line;
	}
    }
}
close FH;
