#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::PfamQC;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;
use File::Copy;


#Script to remove families from a clan
#The script will check for overlaps between the families being removed from the clan
#
#If there are overlaps, it will ask you whether you want to continue with the
#removal of the families from the clan. If you say yes, or if there are no overlaps
#it will:
# -remove the MB lines from the CLANDESC file for families being removed from the clan
# -remove the CL line from the DESC file of families being removed from the clan
# -remove the families being removed from the clan_membership table in the database
#There is also an option to just check the overlaps for the families being removed 
#from the clan (-check_overlaps)


#Get input options
my (@families_to_remove, $clan_acc, $help, $check_overlaps);
GetOptions('remove=s' => \@families_to_remove,
		   'clan=s'   => \$clan_acc,
           'check_overlaps' => \$check_overlaps,
           'help'     => \$help);


#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Check input options
@families_to_remove = split(/,/,join(',',@families_to_remove));
check_options($help, $clan_acc, \@families_to_remove, $pfamDB);

#Make the dir to work in
mkdir($clan_acc, 0755) or die "Couldn't mkdir $clan_acc, $!";
chdir($clan_acc) or die "Couldn't chdir into $clan_acc, $!";

#Check for overlaps in the families to be removed from the clan
my ($num_overlaps, $remove_group_overlap, $other_overlap) = check_overlaps($clan_acc, \@families_to_remove, $pfamDB);

#Report the number of overlaps
if($num_overlaps > 0) {
    print STDERR "\nThere are $num_overlaps overlaps in the families you wish to remove from the clan.\n";
    print STDERR "$remove_group_overlap are between the families you wish to remove from the clan.\n";
    print STDERR "\nPlease check the overlaps files: \n";
    print STDERR "$num_overlaps overlaps in $clan_acc/all_overlaps (all of the overlaps)\n";
    print STDERR "$remove_group_overlap overlaps in $clan_acc/remove_group_overlaps (overlaps betweeen the families you wish to remove from the clan)\n";
    print STDERR "$other_overlap overlaps in $clan_acc/other (overlaps that are not between the families you wish to remove from the clan)\n";
}
else {
    print STDERR "\nNo overlaps were found in the families you wish to remove from the clan\n";
}

#Exit now if only checking the overlaps
if($check_overlaps) {
    exit 0;
}

#If there are overlaps, check with user whether to proceed with removing families
if($num_overlaps > 0) {
    print STDERR "Do you wish to proceed with removing the families from the clan? [y/n]:";
    my $reply = <STDIN>;
    chomp $reply;
    unless($reply eq "y") {
        chdir("../");
        exit(1);
    }
}

print STDERR "\n\nGoing to remove @families_to_remove from $clan_acc\n";

my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };   # don't allow control C for a bit!

#Remove the families from the clan
#Families will:
# - be removed from the MB lines in the CLANDESC,
# - have the CL line removed from the DESC file
# - be removed from the clan_membership table in the db
remove_families_from_clan($clan_acc, \@families_to_remove, $dbh);
chdir("../");

print STDERR "Families have been removed from $clan_acc\n";

if($caught_cntrl_c) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** You should check that the script has done what you wanted (use pfinfo/clinfo to see if the CL/MB lines are correct, and check the clan_membership table in the database is correct" ;
}


sub check_options {

    my ($help, $clan_acc, $families_to_remove, $pfamDB) = @_;
    
    if($help) {
        help();
    }

    #Check we have at least one family to remove
    unless(@$families_to_remove) {
        help();
    }

    #Check clan_acc is in the db
    if($clan_acc) {
        my @clan = $pfamDB->getSchema->resultset("Clan")->search({ "clan_acc" => $clan_acc });    
        unless(@clan) {
            die "$clan_acc is not a valid clan accession\n";
        }
    }
    else {
        help();
    }

    #Check all the families to remove are in the clan
    my @clan_membership = $pfamDB->getSchema->resultset("ClanMembership")->search({ "clan_acc" => $clan_acc });
    my %clan_fam;
    my $clan_members=0;
    foreach my $fam (@clan_membership) {
        my $pfamA_acc = $fam->pfama_acc->pfama_acc;
        $clan_fam{$pfamA_acc}=1;
        $clan_members++;
    }
    my $error="";
    foreach my $pfamA_acc (@$families_to_remove) {
        if(exists($clan_fam{$pfamA_acc})) {
            $clan_members--;
        }
        else {
            $error .= "$pfamA_acc cannot be removed from $clan_acc as it is not in this clan\n";
        }
    } 
    
    if(-d $clan_acc) {
        $error .= "A directory called $clan_acc already exists in the current working directory, please remove this and then run the script again.\n";
    }


    if($error) {
        die "$error";
    }


    unless($clan_members >1) {
        print STDERR "If you remove [@$families_to_remove] from this clan, it will leave the clan with $clan_members member(s). Are you sure you want to continue? [y/n]:";
        my $reply = <STDIN>;
        chomp $reply;
        unless($reply eq "y") {
            exit(1);
        }
        print STDERR "Once this script is finished, you should either add more families to $clan_acc (clans should have at least two families in them), or if the clan is empty and no longer needed, you can kill it using clkill.pl\n";
    }

    return 0;
}

sub remove_families_from_clan {

    my ($clan_acc, $families_to_remove, $dbh) = @_;


    my %remove;
    foreach my $fam (@$families_to_remove) {
        $remove{$fam}=1;
    }
   
    my $caught_cntrl_c;
    $SIG{INT} = sub { $caught_cntrl_c = 1; };   # don't allow control C for a bit!

    #Checkout clan to be killed and remove the MB lines for the families that we are removing from the clan
    print STDERR "\nRemoving MB lines for families from $clan_acc CLANDESC\n\n";
    system("clco $clan_acc") and die "Couldn't run 'clco $clan_acc', $!";

    #Make a copy of the CLANDESC file
    move("$clan_acc/CLANDESC", "$clan_acc/CLANDESC.original") or die "Couldn't move $clan_acc/CLANDESC to $clan_acc/CLANDESC.original, $!";

    #Remove MB lines for families that will be removed
    open(CLANDESC, "$clan_acc/CLANDESC.original") or die "Couldn't open fh to $clan_acc/CLANDESC.original, $!";
    open(C, ">$clan_acc/CLANDESC") or die "Couldn't open fh to $clan_acc/CLANDESC, $!";
    while(<CLANDESC>) {
        if(/^MB\s+(PF\d{5})/) {  #MB   PF12947;
            my $acc = $1;
            if(exists($remove{$acc})) {
                next;        
            }
            print C $_;
        }
        else {
            print C $_;
        }
    }
    close CLANDESC;
    close C;

    #Commit the changes
    system("svn commit -m 'CLREMOVE:Removing @$families_to_remove from clan' $clan_acc/CLANDESC");


    #Remove CL line from the DESC files for the families
    print STDERR "\n\nRemoving CL lines in the DESC file of families\n";
    foreach my $pfamA_acc (sort keys %remove) {
       print STDERR "Updating CL line in $pfamA_acc/DESC\n";
       system("pfco $pfamA_acc") and die "Couldn't 'pfco $pfamA_acc', $!";
       system("sed -i -r '/^CL/d' $pfamA_acc/DESC") and die "Couldn't run 'sed -i -r '/^CL/d' $pfamA_acc/DESC' to remove CL line in $pfamA_acc/DESC, $!";
       #Commit the changes
       system("svn commit -m 'CLREMOVE:Removing CL line in DESC file (family is being removed from $clan_acc)' $pfamA_acc/DESC")
   }
    
    #Update database
    print STDERR "\nRemoving families from the clan_membership table in the database\n";
    my $st = $dbh->prepare("delete from clan_membership where pfamA_acc = ?");
    foreach my $pfamA_acc (keys %remove) {
        $st->execute($pfamA_acc) or die "Couldn't update clan_membership table in database, ". $st->errstr."\n";
    }


    if ($caught_cntrl_c) {
      print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** You should check that the script has done what you wanted (use pfinfo/clinfo to see if the CL/MB lines are correct, and check the clan_membership table in the database is correct" ;
    }

    print STDERR "\n@$families_to_remove have been removed from $clan_acc\n";
}

sub check_overlaps {

    my ($clan_acc, $families_to_remove, $pfamDB) = @_;
    
    my %remove;
    foreach my $fam (@$families_to_remove) {
        $remove{$fam}=1;
    }

    #Store all the regions for the families that are to be removed from the clan
    my %regions;
    my $num_regions=0;
    foreach my $pfamA_acc (keys %remove) {
        print STDERR "Adding regions for $pfamA_acc\n";
        my @full_regions = $pfamDB->getSchema->resultset("PfamARegFullSignificant")->search({ "pfama_acc" => $pfamA_acc });

        my $pfamA = $pfamDB->getSchema->resultset("PfamA")->find({ "pfama_acc" => $pfamA_acc });
        
        foreach my $reg (@full_regions) {
            $num_regions++;
            push(@{$regions{$reg->pfamseq_acc->pfamseq_acc}},
                  { from      => $reg->seq_start,
                    to        => $reg->seq_end,
                    ali_from  => $reg->ali_start, 
                    ali_to    => $reg->ali_end,
                    family    => $pfamA_acc,
                    ali       => 'ALIGN',
                    family_id => $pfamA->pfama_id,
                    score     => $reg->domain_bits_score});

        } 
        my @seed_regions = $pfamDB->getSchema->resultset("PfamARegSeed")->search({ "pfama_acc" => $pfamA_acc });
        foreach my $reg (@seed_regions) {
            push(@{$regions{$reg->pfamseq_acc}},
                  { from      => $reg->seq_start, 
                    to        => $reg->seq_end,
                    ali_from  => $reg->seq_start, #Seed regions don't have ali_from and ali_to, so 
                    ali_to    => $reg->seq_end,  #just replicate the start and ends
                    family    => $pfamA_acc,
                    ali       => 'SEED',
                    family_id => $pfamA->pfama_id });
        }
    }
    
    #Check whether these families overlap with anything
    print STDERR "Looking for overlaps\n";
    my (%overlaps, $ignore_ref);
    my ($num_overlaps, $overlap_array) = Bio::Pfam::PfamQC::findOverlapsDb(\%regions, $ignore_ref, $pfamDB, "", "", "", $num_regions);


    #Get nested domains
    my %nested_domains;
    foreach my $pfamA_acc (@$families_to_remove) {
            my @nested_results = $pfamDB->getSchema->resultset("NestedDomain")->search(
                [ { nests_pfama_acc => $pfamA_acc },
                  { pfama_acc       => $pfamA_acc }]);

        foreach my $n (@nested_results) {
            my $n1_pfamA_acc = $n->pfama_acc->pfama_acc; 
            my $n2_pfamA_acc = $n->nests_pfama_acc->pfama_acc;
            $nested_domains{"$n1_pfamA_acc:$n2_pfamA_acc"}=1;
            $nested_domains{"$n2_pfamA_acc:$n1_pfamA_acc"}=1;
        }
    }
    
    #Print all overlaps, but ignore any nested overlaps
    open(OVERLAPS, ">all_overlaps" ) or die "Can't open overlap file 'all_overlaps', $!\n";
    foreach my $overlap (@$overlap_array) {
        if($overlap =~ /(PF\d{5}).+(PF\d{5})/) {
            my ($fam1, $fam2) = ($1, $2);
            if(exists($nested_domains{"$fam1:$fam2"})) {  #Don't print nested domain overlaps
                $num_overlaps--; #Nested overlaps don't count
                next;
            }
            else { 
                print OVERLAPS $overlap;
            }
        }
    }
    close OVERLAPS;

    #Now separate the overlaps into 'between removing group', and 'not between removing group'  
    my ($remove_group_overlap, $other_overlap)=(0, 0);
    open(O, "all_overlaps") or die "Couldn't open fh to all_overlaps, $!";
    open(R, ">remove_group_overlaps") or die "Couldn't open fh to remove_group_overlaps, $!";
    open(A, ">other_overlaps") or die "Couldn't open fh to other_overlaps, $!";
    while(<O>) {
        if(/(PF\d{5}).+(PF\d{5})/) {
            my ($fam1, $fam2) = ($1, $2);
            if(exists($remove{$fam1}) and exists($remove{$fam2})) {
                $remove_group_overlap++;
                print R $_;
            }
            else {
                print A $_;
                $other_overlap++;
            }
        }
    }
    close O;
    close R;
    close A;
    return($num_overlaps, $remove_group_overlap, $other_overlap);

}


sub help {
print<<EOF;


    This script removes families from a clan. It will first
    check whether the families you wish to remove from the 
    clan have overlaps. Three overlap files will be written:

    *clan_acc/all_overlaps (all of the overlaps)
    *clan_acc/remove_group_overlaps (overlaps betweeen the families 
     you wish to remove from the clan)
    *clan_acc/other (overlaps that are not between the families 
     you wish to remove from the clan)

    Overlaps will not be filtered.
    
    If you just want to check whether there are overlaps in the families
    you wish to remove from the clan, you can use the -check_overlap option. 
    This will make the script exit once the overlaps have been reported 
    (ie the families won't be removed from the clan).

    If there are no overlaps, and if the check_overlap option is 
    not specified, the script will go ahead and remove the families 
    from the clan. If there are overlaps, the script will ask you 
    whether you want to proceed with removing the families from the clan.

    The script will do the following things for each family being removed
    from the clan:
     * remove the MB line from the CLANDESC
     * remove the CL line from the DESC file
     * remove from the clan membership table in the database

    Usage:

    You need to specify the clan accession, and the families 
    that you wish to remove from the clan as a comma separated 
    list:

    $0 -clan CLXXXX -remove PFXXXXX,PFXXXXX,PFXXXXX

    Eg. $0 -clan CL0001 -remove PF00001,PF00002

    Options:
    -check_overlap   :check for overlaps in the families being removed from 
                      the clan, but do not remove the families from the clan

EOF
    exit(1);
}
