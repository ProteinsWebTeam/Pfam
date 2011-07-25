#! /usr/local/bin/perl -w

# Script to find bad ED lines in Families directory
# Will read each DESC file and test whether ED line appears in scores file.
# This script is a bit dumb in that it rescans the scores file for every ED line
# but it still runs in a reasonable time.

my $dir=shift @ARGV;


opendir (DIR, "$dir")||die "Couldn't open directory\n";
my @list=readdir(DIR);
closedir(DIR);

# Remove . and .. directories 
shift  @list;
shift  @list;

foreach my $family (@list){
    #print "$family\n";
    if ($family =~ /PF\d{5}/){
	open (DESC, "$dir/$family/DESC") or warn "cannot open $dir/$family/DESC\n";
	while(<DESC>){
	    if (/^(ED   (\S+)\/(\d+)-(\d+); \S+\/(\d+)-(\d+);)/){
		my $ed_line=$1;
		my $name=$2;
		my $start1=$3;
		my $end1=$4;
		my $start2=$5;
		my $end2=$6;
		check_ed($name,$start1,$end1,$start2,$end2,$dir,$family,$ed_line);
	    }
	}
	close DESC;
    } else {
	warn "Unrecognised file or directory $family\n";
    }
}



sub check_ed {
    my ($name,$start1,$end1,$start2,$end2,$dir,$family,$ed_line)=@_;

    my $ok=0;

    # Test whether protein is still in UniProt - Uses pfetch which is imperfect. It 
    # still finds sequence when you use .version! SO have to srip that out
    # Risk that pfetch sequence database is out of sync with pfamseq!
    my $acc;
    if ($name=~/(\S+)\.\d+/){$acc=$1;}
    open (PFETCH, "pfetch $acc |") or warn "cannot run pfetch";
    while(<PFETCH>){
	if (/no match/){
	    print "pfetch output: $_";	
	    rm_ed_desc($dir,$family,$name,$start1,$end1);
	}
    }
    close PFETCH;

    if ($start1==$start2){
	open (SCORES, "$dir/$family/scores") or warn "cannot open $dir/$family/scores\n";
	while(<SCORES>){
	    if (/$name\/$start2-$end2/){
		#  print "GOOD ED $family: $name/$start-$end\n";
		$ok=1;
	    } elsif (/$name\/$start1-(\d+)/){
		my $new_ed_line="ED   $name/$start1-$1; $name/$start1-$end2;";
		print "**S1 ($family) $ed_line ---> $new_ed_line\n";
		fix_desc($dir,$family,$ed_line,$new_ed_line);
	    } elsif (/$name\/(\d+)-$end1/){
		my $new_ed_line="ED   $name/$1-$end1; $name/$1-$end2;";
		print "**S2 ($family) $ed_line ---> $new_ed_line\n";
		fix_desc($dir,$family,$ed_line,$new_ed_line);
	    }
	}
	close SCORES;

	if (! $ok){
	    # Hmm, so not in scores. Was it in PFAMOUT? If so then the sequence is probably
            # now below threshold
	    if (check_in_pfamout($dir,$family,$name)){
		# OK its in PFAMOUT, but not in scores. It should be below thresh. Lets remove it!
		print "Removing below threshold ED line:\n";
		rm_ed_desc($dir,$family,$name,$start1,$end1);
	    }
	}

    } elsif ($end1==$end2){
	open (SCORES, "$dir/$family/scores") or warn "cannot open $dir/$family/scores\n";
	while(<SCORES>){
	    if (/$name\/$start2-$end2/){
		#  print "GOOD ED $family: $name/$start-$end\n";
		$ok=1;
	    } elsif (/$name\/$start1-(\d+)/){
		my $new_ed_line="ED   $name/$start1-$1; $name/$start2-$1;";
		print "**E1 ($family) $ed_line ---> $new_ed_line\n";
		fix_desc($dir,$family,$ed_line,$new_ed_line);
	    } elsif (/$name\/(\d+)-$end1/){
		my $new_ed_line="ED   $name/$1-$end1; $name/$start2-$end2;";
		print "**E2 ($family) $ed_line ----> $new_ed_line\n";
		fix_desc($dir,$family,$ed_line,$new_ed_line);
	    }
	}
	close SCORES;

	if (! $ok){
	    # Hmm, so not in scores. Was it in PFAMOUT? If so then the sequence is probably
            # now below threshold
	    if (check_in_pfamout($dir,$family,$name)){
		# OK its in PFAMOUT, but not in scores. It should be below thresh. Lets remove it!
		print "Removing below threshold ED line:\n";
		rm_ed_desc($dir,$family,$name,$start1,$end1);
	    }

	}

    } else {
	print "Both start and end coordinates of ED line are different! Too difficult.\n";
    }

    if ($ok==0){
	print "*BAD ED $family: $name/$start2-$end2\n";
    }
    return $ok;
}

sub check_in_pfamout {
    my ($dir,$family,$name)=@_;

    open (PFAMOUT, "$dir/$family/PFAMOUT") or warn "cannot open $dir/$family/PFAMOUT\n";
    while(<PFAMOUT>){
	if (/$name/){return 1;}
    }
    close PFAMOUT;
    return 0;
}

# subroutine to move depracated sequence id from DESC ED line
sub rm_ed_desc {
    my ($dir,$family,$name,$start1,$end1)= @_;

    print "**($family) Removing ED line for $name/$start1-$end1\n";

    # Open DESC and edit it
    open (DESC2, "$dir/$family/DESC") or warn "cannot open $dir/$family/DESC\n";
    open (DESCNEW, "> $dir/$family/DESC.ededit") or warn "cannot write $dir/$family/DESC.ededit\n";
    while(<DESC2>){
	if (/ED.*$name\/$start1-$end1/){
	    # ED line is removed!
	} else {
	    print DESCNEW;
	}
    }
    close DESC2;
    close DESCNEW;

    # run pfmake
    print "cd $dir/$family/; cp DESC DESC.edbackup; cp DESC.ededit DESC;\n";
    system ("cd $dir/$family/; cp DESC DESC.edbackup; cp DESC.ededit DESC;");
}


sub fix_desc {
    my ($dir,$family,$ed_line,$new_ed_line)= @_;

    # Open DESC and edit it
    open (DESC2, "$dir/$family/DESC") or warn "cannot open $dir/$family/DESC\n";
    open (DESCNEW, "> $dir/$family/DESC.ededit") or warn "cannot write $dir/$family/DESC.ededit\n";
    while(<DESC2>){
	if (/$ed_line/){
	    print DESCNEW "$new_ed_line\n";
	} else {
	    print DESCNEW;
	}
    }
    close DESC2;
    close DESCNEW;

    # run pfmake
    print "cd $dir/$family/; cp DESC DESC.edbackup; cp DESC.ededit DESC; pfmake;\n";
    system ("cd $dir/$family/; cp DESC DESC.edbackup; cp DESC.ededit DESC; pfmake;");

}
