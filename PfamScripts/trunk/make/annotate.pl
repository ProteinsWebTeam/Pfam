#! /usr/local/bin/perl -w

# They said it couldn't be done! They might have been right.
# A script to automagically annotate Pfam families.  Generally its use is 
# intended for new DUFs ready for check in.
#
# Further ideas:
#
#  0) Pick up DUF number automatically from database
#  1) Subcellular localisation
#  2) Look for structure - Use UniProt PDB link which includes residue numbers!
#  3) Look for mutations in these domains - Use UniProt FT lines
#  4) Pull out potentially interesting refs
#  5) Fix problem with dealing with DE lines!

use strict;
use Text::Wrap;
use Getopt::Long;

my ($all,$duf,$noci,$viewseed,$not_associated,$ignoreoverlaps);
&GetOptions('all!' => \$all,
	    'duf!' => \$duf,
	    'noci!' => \$noci,
	    'viewseed!' => \$viewseed,
	    'ignoreoverlaps!' => \$ignoreoverlaps,
	    'na=s' => \$not_associated,

);

if (! @ARGV and ! $all){&help}

sub help {
    print STDERR <<"EOF";
They said it could not be done! They might have been right.
A script to automagically annotate Pfam families.  Generally its use is 
intended for new DUFs ready for check in, but it will work for any family.

USAGE: $0 <pfam dir name>

Arguments:
         -duf             - Add DUF information
         -noci            - Do not interactively ask if you want to pfnew family
         -all             - Option to look at every directory in current directory
         -viewseed        - Look at SEED alignment before making decision
         -ignoreoverlaps  - Does what it says on the tin
         -na <fraction>   - Will ignore any family that has more than the fraction of
                          - Pfam domains associated. fraction must be between 0 and 1.

EOF
exit 1;
}

if ($duf){
    # Get next available DUF number
    open (DUF, "nextDUF.pl |") or die "Cannot run nextDUF.pl";
    while(<DUF>){
	if (/DUF(\d+)/){
	    $duf=$1;
	}
    }
    close DUF;
    print STDERR "Setting duf number to $duf\n";
}

# Get list of all families in directory
my @dir_list;
if ($all){
  opendir (DIR, ".")||die "Couldn't open directory\n";
  @dir_list=readdir(DIR);
  closedir(DIR);

  # Remove . and .. directories 
  shift  @dir_list;
  shift  @dir_list;
} else {
    @dir_list=@ARGV;
}

# This is a list of common genome papers that we can ignore
my @boring_pmids;
open (PMID, "/nfs/team71/pfam/agb/boring_pmids") or die "cannot open file";
while(<PMID>){
    if (/^(\S+)\s+/){
	push (@boring_pmids,$1);
    }
}
close PMID;



# Check dir exists and try to write annotation automagically
FAMILY: foreach my $dir (sort @dir_list){
    $dir=~ s/\///; # Remove trailing / from directory names
    if ($duf){
	print STDERR "Looking at $dir - (DUF$duf)\n\n";
    } else {
	print STDERR "Looking at $dir\n\n";
    }

    if (! -d "$dir"){
	print STDERR "$dir is not a directory. Skipping ...!\n";
	next;
    }

    if (! -s "$dir/DESC"){
	print STDERR "$dir has no DESC probably not a family directory. Skipping ...!\n";
	next;
    }

    if (! -s "$dir/scores"){
	print STDERR "$dir has no scores probably not a family directory. Skipping ...!\n";
	next;
    }


    # Preliminary overlap check
    if ((-s "$dir/overlap") and not $ignoreoverlaps){
	print STDERR "$dir has overlaps. Skipping ...\n";
	next;
    }

    system ("pqc-overlap-rdb.pl $dir") and warn "Cannot run pqc-overlap-rdb.pl or $dir has overlaps [$!]";
    

    if ((-s "$dir/overlap") and not $ignoreoverlaps){
	print STDERR "$dir has overlaps. Skipping ...\n";
	next;
    }

    # Get info for sequence to write annotation
    if (! -e "$dir/seq_info"){
	&get_seq_info($dir);
    }
    
    # Make automatic annotation - get data from seq_info file
    my ($arc, $bac, $euk,$vir);
    my @lengths;
    my (%co_domains,%ipr_domains,%g3d_domains,%smart_domains,%pirsf_domains,%prosite_domains,%tigrfams_domains);
    my (%db_refs);
    my %keyword_hash;
    my $total_seq=0;
    my $id;
    my %interesting_id;
    my %interesting_ref;
    my %description;
    my $refnum;
    my %refhash;
    open (INFO, "$dir/seq_info") or die "cannot open $dir/seq_info";
    while(<INFO>){
	if (/^OC.*Bacteria/){$bac++;}
	if (/^OC.*Archaea/){$arc++;}
	if (/^OC.*Eukaryota/){$euk++;}
	if (/^OC.*Viruses/){$vir++;}

	if (/^KW\s+(\S.*)$/){
	    my $line=$1;
	    my @keywords=split(";",$line);
	    foreach my $keyword (@keywords){
		if ($keyword =~ /^\s(.*)/){ # Remove whitespace at front of keyword
		    $keyword=$1;
		}
		if ($keyword =~ /(.*)\.$/){ # Remove trailing full stops
		    $keyword=$1;
		}


		$keyword_hash{$keyword}++;
	    }
	}

	if (/^ID   (\S+)\s.*\s(\d+) AA.$/){
	    $id=$1;
	    push (@lengths,$2);
	    $total_seq++;
	    $interesting_id{$id}=1;
	    $refnum="";
	}

	if (/^DR\s+(\S+);\s(\S+);\s(\S+);/){
	    my $db=$1;
	    my $dr_acc=$2;
	    my $dr_id=$3;
	    my $id_acc=$dr_acc.";".$dr_id;

	    $db_refs{$db}{$id_acc}++;
	}

	if (/^DR   Pfam; (\S+); (\S+);/){
	    $co_domains{$1}++;
	}

	### FIND INTERESTING DE LINES ###

	# Not dealing correctly with multiline  nature of DE lines!
	# Could do this much more nicely I'm sure!!!
	if (/^DE   (.*)$/){
	    $description{$id}.=$1;
	    if (/UNCHARACTER/i){
		$interesting_id{$id}=0;
	    } elsif (/PUTATIVE/i){
		$interesting_id{$id}=0;
	    } elsif (/HYPOTHETICAL/i){
		$interesting_id{$id}=0;
	    } elsif (/PREDICTED/i){
		$interesting_id{$id}=0;
	    } elsif (/FRAGMENT/i){
		$interesting_id{$id}=0;
	    } elsif (/shotgun sequence/i){
		$interesting_id{$id}=0;
	    } elsif (/cDNA/i){
		$interesting_id{$id}=0;
	    } elsif (/FLJ\d+/i){
		$interesting_id{$id}=0;
	    } elsif (/UNKNOWN/i){
		$interesting_id{$id}=0;
	    } 
	}

	### FIND INTERESTING REFERENCES ###
	if (/^R/){
	    if (/^RN   \[(\d+)\]/){
		$refnum=$1;
		$refhash{$id}{$refnum}.=$_;
		$interesting_ref{$id}{$refnum}=1;
	    } elsif (/^RP   (.*)$/){
		$refhash{$id}{$refnum}.=$_;
		my $rp=$1;
		if ($rp =~ /LARGE SCALE/){
		    $interesting_ref{$id}{$refnum}=0;
		}
	    } elsif (/^RA   (.*)/){
		$refhash{$id}{$refnum}.=$_;
	    } elsif (/^RL   (.*)/){
		$refhash{$id}{$refnum}.=$_;
		my $rl=$1;
		if ($rl =~ /EMBL/){
		    $interesting_ref{$id}{$refnum}=0;
		}
	    } elsif (/^RT   (.*)/){
		$refhash{$id}{$refnum}.=$_;
		my $rt=$1;
		if ($rt =~ /Complete sequence|genome sequence|Whole genome|whole-genome/i){
		    $interesting_ref{$id}{$refnum}=0;
		}
	    } elsif (/^RX   (.*)/){
		$refhash{$id}{$refnum}.=$_;
		my $rx=$1;

		foreach my $pmid (@boring_pmids){
		    if ($rx=~ /$pmid/){
			$interesting_ref{$id}{$refnum}=0;
		    }
		}
	    }
	    # Ignoring RC and RG lines


	}


    }
    close INFO;

    
    ### LENGTH STUFF ###
    # Find range of lengths
    # Order by length and take 20th and 80th percentile values
    my @sorted_array=sort numerically @lengths;
    my $array_length=@sorted_array;

    my $lower_length=$sorted_array[int(0.2*$array_length)];
    my $upper_length=$sorted_array[int(0.8*$array_length)];

    my @full_lengths;
    # Get length lengths of seed alignment - I think this will be more reliable than full!
    open (SEED, "$dir/SEED") or die "cannot open $dir/SEED";
    while(<SEED>){
	if (/^\S+\/(\d+)-(\d+)\s/){
	    my $len=$2-$1+1;
	    #print STDERR "Len $len $1 $2\n";
	    push (@full_lengths,$len);
	}
    }
    close SEED;

    my @sorted_full_array=sort numerically @full_lengths;
    my $array_full_length=@sorted_full_array;

    my $lower_full_length=$sorted_full_array[int(0.2*$array_full_length)];
    my $upper_full_length=$sorted_full_array[int(0.8*$array_full_length)];

    my $length_info="Proteins in this family vary in length from $lower_length to $upper_length amino acids.\nThe domains in this family vary in length from $lower_full_length to $upper_full_length amino acids.\n";


    # Can use this info to decide if it is a domain in a longer protein or is the whole length. THen I 
    # can start to include info about protein or domain lengths in DESC!

    my ($type,$lc_type);
    if ($upper_full_length>($lower_length*0.7)){
	#print STDERR "Looks close enough to full length! $upper_full_length > $lower_length * 0.7\n";
	$type="Protein"; 
	$lc_type="protein";
    } else {
	print STDERR "Looks like a domain!\n";
	$type="Domain";
	$lc_type="domain";
    }


    ### INTRO COMMENTS ### Only include for DUFs
    my $comment;
    if ($duf){
	if ($type eq "Protein"){
	    $comment="This family of proteins is functionally uncharacterised.";
	} else {
	    $comment="This presumed domain is functionally uncharacterised.";
	}
    }
    
    ### SPECIES STUFF ###

    my @kingdom;
    my $sum=0;
    if ($bac){$sum++; push(@kingdom,"bacteria");}
    if ($arc){$sum++; push(@kingdom,"archaea");}
    if ($euk){$sum++; push(@kingdom,"eukaryotes");}
    if ($vir){$sum++; push(@kingdom,"viruses");}
    
    if ($type eq "Protein"){
	$comment.="This family of proteins is found in " ;
    } else {
	$comment.="This domain family is found in " ;
    }

    my $pad="";
    if ($duf){$pad=" ";}
        if ($sum==4){
	$comment .= $pad."bacteria, archaea, eukaryotes and viruses";
    } elsif ($sum==3){
	$comment .= $pad."$kingdom[0], $kingdom[1] and $kingdom[2]";
    } elsif ($sum==2){
	$comment .= $pad."$kingdom[0] and $kingdom[1]";
    } elsif ($sum==1){
	$comment .= $pad."$kingdom[0]";
    } elsif ($sum==0){
	die "Oh no. No species found. Should never get here!";
    }

    ### LENGTH COMMENTS ###
    my $approx=0;
    if ($type eq "Protein"){
	if (($upper_length-$lower_length)>10){
	    $comment .= ". Proteins in this family are typically between $lower_length and $upper_length amino acids in length.";
	} else {
	    $approx=10*(int(($upper_length+$lower_length+10)/20));
	    $comment .= ". Proteins in this family are approximately $approx amino acids in length.";
	}
    } else {
	if (($upper_full_length-$lower_full_length)>10){
	    $comment .= ", and is typically between $lower_full_length and $upper_full_length amino acids in length.";
	} else {
	    $approx=10*(int(($upper_full_length+$lower_full_length+10)/20));
	    $comment .= ", and is approximately $approx amino acids in length.";
	}
    }

    ### CO-OCCURING DOMAINS COMMENT ### # Should use the db_refs hash to do this instead!
    my @list;
    foreach my $domain (keys %co_domains){
	if ($not_associated and $co_domains{$domain}>($not_associated*$total_seq)){
	    print STDERR "Too many associated domains. Skipping family ...\n";
	    next FAMILY;
	}


	if ($co_domains{$domain}>(0.5*$total_seq)){
	    push (@list,"Pfam:$domain");
	}
    }
    my $list_len=@list;
    if ($list_len){
	my $join_list=join(", ",@list);
	$comment .= " The family is found in association with $join_list.";
    }



    ### SHORT MOTIF COMMENTS ###
    my @motifs;
    open (CONS, "consensus.pl -file $dir/SEED -thr 85 |") or die "Warn cannot open pipe with consensus.pl $dir/SEED";
    while(<CONS>){
	if (/consensus\/85\%\s+\S+$/){
	    # Find motifs  -look for no more than 1 motif up to 6 letters.
	    @motifs=m/[a-z\.]([A-Z]{3,})[a-z\.]/g;

	    my $n_motifs=@motifs;

	    # Find max length of motif
	    my $max_len=0;
	    foreach my $motif (@motifs){
		my $len=length($motif);
		if ($len>$max_len){
		    $max_len=$len;
		}
	    }

	    if ($n_motifs==1 and $max_len<8){
		$comment .= " There is a conserved $motifs[0] sequence motif.";
	    } elsif ($n_motifs==2 and $max_len<8){
		$comment .= " There are two conserved sequence motifs: $motifs[0] and $motifs[1].";
	    }

	}
    }
    close CONS;

    ### HIGHLY CONSERVED RESIDUES COMMENT ###
    @motifs="";
    open (CONS, "consensus.pl -file $dir/SEED -thr 99 |") or die "Warn cannot open pipe with consensus.pl $dir/SEED";
    while(<CONS>){
	if (/consensus\/99\%\s+\S+$/){
	    # Find motifs  -look for no more than 1 motif up to 6 letters.
	    @motifs=m/[a-z\.]([A-Z]{1,})[a-z\.]/g;

	    my $n_motifs=@motifs;

	    # Find max length of motif
	    my $max_len=0;
	    foreach my $motif (@motifs){
		my $len=length($motif);
		if ($len>$max_len){
		    $max_len=$len;
		}
	    }

	    if ($n_motifs==1 and $max_len<2){
		$comment .= " There is a single completely conserved residue $motifs[0] that may be functionally important.";
	    } elsif ($n_motifs==2 and $max_len<2){
		if ($motifs[0] ne $motifs[1]){
		    $comment .= " There are two completely conserved residues ($motifs[0] and $motifs[1]) that may be functionally important.";
		} else {
		    $comment .= " There are two completely conserved $motifs[0] residues that may be functionally important.";
		}
	    }

	}
    }
    close CONS;

    open (NEWDESC, "> $dir/DESC.new") or die "cannot write to new $dir/DESC.new file";
    open (DESC, "$dir/DESC") or die "cannot open $dir/DESC";
    open (ANN, "> $dir/autoann") or die "cannot write to $dir/autoann";

    if ($duf){
	print STDERR "Looks like duf number is set!\n";
	while(<DESC>){
	    if (/^ID/){
		print NEWDESC "ID   DUF"."$duf\n";
	    } elsif (/^DE/){
		print NEWDESC "DE   $type of unknown function (DUF$duf)\n";
	    } elsif (/^AU/){
		print NEWDESC "AU   Assefa S, Coggill P, Bateman A\n";
	    } elsif (/^CC/){
		#	    Strip out existing comments
	    } else {
		print NEWDESC;
	    }
	}
    } else {
	while(<DESC>){
	    if (/^CC/){
		#	    Strip out existing comments
	    } else {
		print NEWDESC;
	    }
	}
    }


    # Should add CC parts to wrap comments
    print STDERR "\nCOMMENT LINES\n\n";
    print ANN "\nCOMMENT LINES\n\n";
    $comment.="\n";
    print NEWDESC wrap("CC   ","CC   ",$comment);
    print STDERR wrap("CC   ","CC   ",$comment);
    print ANN wrap("CC   ","CC   ",$comment);


    ### LENGTH INFO ###
    print STDERR "\n$length_info";
    print ANN "\n$length_info";


    ### DATABASE REFERENCES ###
    print STDERR "\nDATABASE REFERENCES\n\n";
    print ANN "\nDATABASE REFERENCES\n\n";
    foreach my $link (keys %db_refs){
	if ($db_refs{$link}){
	    if ($link eq "EMBL"){next;}
	    foreach my $acc (keys %{$db_refs{$link}}){
		my $num=$db_refs{$link}{$acc};
		print STDERR "DR $link $acc $num/$total_seq ",int(100*$num/$total_seq),"%\n";
		print ANN "DR $link $acc $num/$total_seq ",int(100*$num/$total_seq),"%\n";
	    }
	}
    }

    ### INTERESTING REFS ###
    print STDERR "\nINTERESTING REFERENCES\n\n";
    print ANN "\nINTERESTING REFERENCES\n\n";
    foreach my $id (keys %refhash){
	if ($refhash{$id}){
	    foreach my $refnum (sort keys %{$refhash{$id}}){
		if ($interesting_ref{$id}{$refnum}){
		    my $ref=$refhash{$id}{$refnum};
		    print STDERR "$id\n$ref\n";
		    print ANN "$id\n$ref\n";
		}
	    }
	}
    }


    ### KEYWORDS INFO ### Would be good to sort keywords by most prevalent first!
    print STDERR "\nKEYWORDS\n\n";
    print ANN "\nKEYWORDS\n\n";
    foreach my $keyword (sort keys %keyword_hash){
	my $fraction=$keyword_hash{$keyword}/$total_seq;
	print STDERR "$keyword\t$keyword_hash{$keyword}/$total_seq\t",int($fraction*100),"%\n";
	print ANN "$keyword\t$keyword_hash{$keyword}/$total_seq\t",int($fraction*100),"%\n";
    }

    ### DE LINE INFO ###
    print STDERR "\nPROTEIN DESCRIPTIONS\n\n";
    print ANN "\nPROTEIN DESCRIPTIONS\n\n";
    foreach my $element (sort keys %interesting_id){
	if ($interesting_id{$element}){
	    print STDERR "$element\t$description{$element}\n";
	    print ANN "$element\t$description{$element}\n";
	}
    }


    close DESC;
    close NEWDESC;
    close ANN;
    
    # Move new DESC file sideways
    system ("cp $dir/DESC $dir/DESC.old") and die "Cannot copy $dir/DESC to $dir/DESC.old [$!]";
    system ("cp $dir/DESC.new $dir/DESC") and die "Cannot copy $dir/DESC.new to $dir/DESC [$!]";
    
    # Format checks
    system ("pqc-format.pl $dir");


    # Ask if you want to view seed
    if ($viewseed){
	    system ("belvu $dir/SEED") and die("Cannot belvu $dir/SEED [$!]");
    }

    # Ask if you want to pfnew the family
    if (! $noci){
	print STDERR "Are you happy to add this family to Pfam [y/n]\n";
	my $reply = <STDIN>;
	chop $reply;
	if ($reply eq "y") {
	    system ("pfnew.pl $dir") and die("Cannot pfnew.pl $dir [$!]");
	    system ("mv $dir ../CHECKED_IN/") and die("Cannot mv $dir ../CHECKED_IN/ [$!]");
	    if ($duf){$duf++;}
	}
    }
}


sub numerically{  $a <=> $b;  }

# A subroutine to do equivalent of get_ALIGN_info
sub get_seq_info {
    my $dir = shift @_;

    open (ALIGN, "$dir/ALIGN") || die "Cannot find the $dir/ALIGN file, are in you in the correct directory ?\n";

    my %ids;
    while (<ALIGN>){
        if ($_ =~ /(\S+)\//){
	    $ids{$1}++;
	}
    }

    close(ALIGN) || die "Cannot close the $dir/ALIGN file\n";

    my (@info, $all_ids);
    foreach my $id (keys %ids){     
        $all_ids .= " $id";
    }

    open(PFETCH, "pfetch -F $all_ids |");
    while (<PFETCH>){
        if ($_ =~ /^(  )\s{3}/){ # Remove sequence lines
	    next;
	}
        else{
	    push(@info, $_);
	}
    }
    close (PFETCH);
    open(OUTPUT, ">$dir/seq_info") || die "could not open seq_info: $!";
    
    foreach (@info){
        chomp ($_);
        print OUTPUT "$_\n";
    }
    close(OUTPUT);

}
