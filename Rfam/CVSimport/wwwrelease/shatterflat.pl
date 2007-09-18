#!/software/bin/perl -w

#
# Takes full, seed, dead flat files 
# and shatters them into ./desc/acc.desc
#                        ./seed/acc.seed
#                        ./full/acc.full
#

use Cwd;

$full = shift;
$seed =shift;
#$dead = shift;
$output_dir = shift;

if (!$output_dir) {
  $output_dir = cwd();
  $output_dir = $output_dir . "/data";

}

# Taken out as dont this its needed !
## $no_over = shift;



#if( !defined $dead ) {
#    print<<EOF;
#shatterflat <full> <seed> <dead> shatters flat files for web site
#EOF
#}

if( !-d "$output_dir/desc" ) {
    die "The directory $output_dir/desc/ must exist to shatter into";
}
if( !-d "$output_dir/seed" ) {
    die "The directory seed/ must exist to shatter into";
}
if( !-d "$output_dir/full" ) {
    die "The directory full/ must exist to shatter into";
}
#if (!$output_dir) {
#  die "Need output data dir to put files into ! \n";
#}

open(FULL,"$full") || die "Could not open $full $!";

open(SEED,"$seed") || die "Could not open $seed $!";

#open(DEAD,"$dead") || die "Could not open $dead $!";


print "$output_dir \n";
#exit(0);



#
# loop through full, building up id<->acc list
#
#

open(DESC,">$output_dir/desc/temp.desc");


while(<SEED>) {
    /^\# STOCKHOLM/ && next;
    /^\#=GF\s+SQ\s+(\S+)/ && do {
      $scount{$name} = $1;
#	if( $no_over == 1 && -e "$output_dir/seed/$acc.full.gz" ) {
#	    print STDERR "Skipping $name\n";
#	    while(<SEED>) {
#		/^\/\// && last;
#	    }
#	} else {
	   &make_alignment($name, "$output_dir/seed/$acc.full",\*SEED);	    
	   # $scount{$name} = $scount;
#	   system("gzip -f $output_dir/seed/$acc.full");
	    next;
#	}
    };
    /^\#=GF\s+AC\s+(\w+\d+)/ && do {
	$acc = $1;
    };
    /^\#=GF\s+ID\s+(\S+)/ && do {
	$name = $1;
	print STDERR "Doing seed $name\n";
    };
}
FAMILY:
while(<FULL>) {
    /^\# STOCKHOLM/ && next;

    # if we have a sequence, 
    # dump out the alignment into full, close the file handles
    # and rename the desc file to the acc number.

    # this will be at the end of each entry.
    
    # strip off the #=GF bit
    
    next if ($_ !~ /^\#/);

    s/^\#=GF\s+//;
    
    /^SQ\s+(\S+)/ && do {
      $fcount = $1;
#	if( $no_over == 1 && -e "$output_dir/full/$acc.full.gz" ) {
#	    print STDERR "Skipping $name\n";
#	    while(<FULL>) {
#		/^\/\// && last;
#	    }
#	} else {
	    ($avl,$avp)= &make_alignment($name, "$output_dir/full/$acc.full",\*FULL);	    
	   # system("gzip -f $output_dir/full/$acc.full");
	    print DESC "NF   $fcount\n";
	    print DESC "NS   $scount{$name}\n";
	    close(DESC);
	    rename("$output_dir/desc/temp.desc","$output_dir/desc/$acc.desc");
	    
	    # put away things for later into a hash
	    
	    $map{$name} = $acc;
	    $des{$name} = $des;
	    $scop{$name} = $scop;
	    $fcount{$name} = $fcount;
	    $avl{$name} = $avl;
	    $avp{$name} = $avp;
            $type{$name} = $type;
      
	    $acc = $des = $scop = $type = undef;
	    
	    open(DESC,">$output_dir/desc/temp.desc");
	    next;
#	}
    };

    # if there is a scop link, yank it out.

    /^DR\s+SCOP;\s+(\S+);/ && do { $scop = $1; };
    
    # otherwise - write it to desc file.

    print DESC $_ if ($_ !~ /^\#/);

    # catch the id, acc and des

    /^ID\s+(\S+)/ && do {
	$name = $1;
	print STDERR "Doing full $name\n";
    };
    /^AC\s+(\w+\d+)/ && do {
	$acc = $1;
    };
    /^DE\s+(\S.*)$/ && do { 
	$des= $1;
    };
    /^TP\s+(\S.*)$/ && do { 
	$type= $1;
    };
}

open(IND,">$output_dir/family.index");

@keys = keys %map;

@keys = sort { return $map{$a} cmp $map{$b}  } @keys;


foreach $key ( @keys ) {
    print IND "$map{$key}^$key^$type{$key}^$scount{$key}^$fcount{$key}^$avl{$key}^$avp{$key}^$scop{$key}^$des{$key}\n";
}
close(IND);


# finally. make the dead desc files

#while(<DEAD>) {
#    s/^\#=GF\s+//;

#    /^ID/ && do {
#	$idline = $_;
#	next;
#    };
#    /^AC\s+(PF\d+)/ && do {
#	$acc = $1;
#	open(DESC, ">$output_dir/desc/$acc.desc") or die "Could not open $output_dir/desc/$acc.desc for writing";
#	print DESC $idline;
#	print DESC $_;
#	next;
#    };
#    /^\/\// && do {
#	close(DESC) or print STDERR "Warning: Could not close $output_dir/desc/$acc\n";
#	next;
#    };

#    print DESC;
#}





sub make_alignment {
    my $fam_id = shift;
    my $dest = shift;
    my $fh = shift;
   # my $fcount = 0;
    my ($pname,$perc,$avl,$nse);

    open(OUT,">$dest");
    print OUT "# STOCKHOLM 1.0\n";
    while(<$fh>) {
	/^(\S+)\/\d+\-\d+\s+\S+/ && do {
#	    $fcount++;
	    print OUT;
	    next;
	};

	### for seed dont need GS
	/^\#=GS/ && do {
	    next;
	};


	/^\#=/ && do {
	    print OUT;
	    next;
	};
#	print "BLEE : $_ ";

	/^\s+/ && do {
	    next;
	};
	

	if ($_ =~ /^\/\//) {
		print OUT $_;
		last;

	}




	print STDERR "Weird line in given alignment $_";
    }
    # do an alistat
    close(OUT);


    if( $name ne 'GP120' ) {
	open(ALI,"alistat -f $dest |") || die "Could not open alistat";
	
	#yank the correct terms out of alistat
	
	while(<ALI>) {
	    /^Average length:\s+(\S+)/ && do { $avl = $1; };
	    /^Average identity:\s+(\d+)/ && do { $perc = $1; };
	}
	close(ALI);
    }
	
    return ($avl,$perc);
}
