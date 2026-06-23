#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use File::Copy;

#Script used to check seeds during seed surgery
#If the user is happy with the seed, it will get moved to a directory called Done in cwd
#Else there will be options to extend/wholeseq/create_alignment/pad_ends/go back to original seed

my ($list, $help, $noSEED);
GetOptions('list=s' => \$list,
            'noSEED' => \$noSEED,
            'help'  => \$help);

if($help or (!$list and !$noSEED)) {
    help();
}

#The 'Done' directory is referenced using a relative path so that the script
#does not break in future releases (i.e. it does not hard code the release number)
my $done_dir;

#The -noSEED option works on the Pfam families in the 'noSEED' directory only.
#It does not look at the SeedSurgery families at all.
if($noSEED) {
    process_noSEED();
    exit;
}

$done_dir = "Done";
mkdir($done_dir, 0755) unless(-d $done_dir);

my $count=0;
open(LIST, $list) or die "Couldn't open fh to $list, $!";
while(<LIST>) {
    if(/^(PF\d{5})/) {
        my $pfamA_acc=$1;
        $count++;
        unless(-d $pfamA_acc) {
            print STDERR "Skipping $pfamA_acc\n";
            next;
        }

        #Keep a copy of the original seed, just for insurance
        my $seed_copy = "$pfamA_acc/SEED.original";
        unless(-s $seed_copy) {
            copy("$pfamA_acc/SEED", "$seed_copy") or die "Couldn't copy $pfamA_acc/SEED to $seed_copy, $!";
        }

        #Get the pfamA id from the DESC file
        my $pfamA_id = pfamA_id("$pfamA_acc/DESC");

        #Find out what happened to the seed
        my ($num_deleted, $num_changed, $changed_seq) = parse_seed_surgery_log("$pfamA_acc/seed_surgery.log");
        
        print STDERR "\n$count: *** $pfamA_acc ($pfamA_id) ***\n\n";
        print STDERR "Sequences changed:\n$changed_seq\n";
        print STDERR "Summary: $num_changed sequences changed, $num_deleted sequences deleted\n\n";

        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED") and die "Couldn't open $pfamA_acc/SEED in belvu, $!";

        #Ask the user what to do
        user_response($pfamA_acc);
    }
}
close LIST;


sub pfamA_id {
    my ($desc_file) = @_;

    my $pfamA_id;
    open(DESC, $desc_file) or die "Couldn't open fh to $desc_file, $!";
    while(<DESC>) {
        if(/^ID\s+(\S+)/) {
            $pfamA_id = $1;
            last;
        }
    }
    close DESC;

    unless($pfamA_id) {
        die "Couldn't parse pfamA id from $desc_file";
    }
    return $pfamA_id;
}


sub parse_seed_surgery_log {
    my ($seed_surgery_log) = @_;

    my ($num_deleted, $num_changed, $changed_seq) = (0, 0, "");
    open(SSL, $seed_surgery_log) or die "Couldn't open fh to $seed_surgery_log, $!";
    while(<SSL>) {
        if(/deleted/) {
            $num_deleted++;
        }
        else {
            $num_changed++;
            $changed_seq.=$_;
        }

    }
    close SSL;

    return($num_deleted, $num_changed, $changed_seq);

}


sub user_response {
    my ($pfamA_acc) = @_;

    #Ask the user what to do
    print STDERR "\nIs the family done, or do you want to edit it further (enter y/n/q/a/b/c/e/f/o/p/w):\n\n";
    print STDERR "  y: yes family is done, move to the 'Done' directory\n";
    print STDERR "  n: no family is not done, leave it where it is and move on to next family\n";
    print STDERR "  q: quit the script\n\n";

    print STDERR "  a: add match states to SEED (use for families with nested domain)\n";
    print STDERR "  b: open SEED alignment again\n";
    print STDERR "  c: run create_alignment.pl on the family\n";
    print STDERR "  e: run extend.pl on the family\n";
    print STDERR "  f: fast pad_ends.pl (same as p, but won't ask your for max_pad value, and will move family to Done)\n";
    print STDERR "  o: go back to original seed (SEED.original)\n";
    print STDERR "  p: run pad_ends.pl on the family\n";
    print STDERR "  w: run wholeseq.pl on the family\n";

    my $reply = <STDIN>;
    chomp $reply;

    if($reply eq "y") {
        print STDERR "Moving $pfamA_acc to $done_dir\n";
        rename($pfamA_acc, "$done_dir/$pfamA_acc") or die "Couldn't move $pfamA_acc to $done_dir, $!";
    }
    elsif($reply eq "n") {
        print STDERR "Leaving $pfamA_acc in current working directory and moving on to next family\n";
    }
    elsif($reply eq "q") {
        print STDERR "Exiting the script\n";
        exit;
    }   
    elsif($reply eq "a") {
        print STDERR "Adding match states\n";
        add_match_states($pfamA_acc);
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
    elsif($reply eq "b") {
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
    elsif($reply eq "c") {
        create_alignment($pfamA_acc);
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
    elsif($reply eq "e") {
        extend($pfamA_acc);
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
    elsif($reply eq "f") {
        pad_ends($pfamA_acc, 10); 
        print STDERR "Moving $pfamA_acc to $done_dir\n";
        rename($pfamA_acc, "$done_dir/$pfamA_acc") or die "Couldn't move $pfamA_acc to $done_dir, $!";
    }
    elsif($reply eq "o") {
        print STDERR "Copying $pfamA_acc/SEED.original to $pfamA_acc/SEED\n";
        copy("$pfamA_acc/SEED.original", "$pfamA_acc/SEED") or die "Couldn't copy $pfamA_acc/SEED.original to $pfamA_acc/SEED, $!";
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
    elsif($reply eq "p") {
        pad_ends($pfamA_acc);
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);

    }
    elsif($reply eq "w") {
        wholeseq($pfamA_acc);
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
    else {
        print STDERR "Need to enter a valid option, try again!\n";
        user_response($pfamA_acc);
    }
}


sub extend {

    my $pfamA_acc = shift;

    my ($n, $c) = (-1, -1);
    until($n >= 0) {
        print STDERR "How many residues do you want to extend by at the N-terminus [default 0]\n";
        my $ans = <STDIN>;
        chomp $ans;
        $ans = 0 unless($ans);
        if($ans =~ /^(\d+)$/) {
            $n = $1;
        }
        else {
            print STDERR "Need to pass in a positive integer, try again!\n";
        }
    }
    until($c >= 0) {
        print STDERR "How many residues do you want to extend by at the C-terminus [default 0]\n";
        my $ans = <STDIN>;
        chomp $ans;
        $ans = 0 unless($ans);
        if($ans =~ /^(\d+)$/) {
            $c = $1;
        }
        else {
            print STDERR "Need to pass in a positive integer, try again!\n";
        }
    }

    my $original_seed = "SEED.b4_extend";
    print STDERR "\nMoving $pfamA_acc/SEED to $pfamA_acc/$original_seed\n";
    move("$pfamA_acc/SEED", "$pfamA_acc/$original_seed") or die "Couldn't move $pfamA_acc/SEED to $pfamA_acc/$original_seed, $!";

    my $command = "extend.pl -align $pfamA_acc/$original_seed -n $n -c $c -mu > $pfamA_acc/SEED";
    print STDERR "Running '$command'\n";
    system("$command") and die "Couldn't run '$command', $!";
}


sub create_alignment {

    my $pfamA_acc = shift;
    my $original_seed = "SEED.b4_create_alignment";

    print STDERR "Moving $pfamA_acc/SEED to $pfamA_acc/$original_seed\n";
    move("$pfamA_acc/SEED", "$pfamA_acc/$original_seed") or die "Couldn't move $pfamA_acc/SEED to $pfamA_acc/$original_seed, $!";

    my $command = "create_alignment.pl -fasta $pfamA_acc/$original_seed -mu > $pfamA_acc/SEED";
    print STDERR "Running '$command'\n";
    system("$command") and die "Couldn't run '$command', $!";
}


sub pad_ends {

    my ($pfamA_acc, $max_pad) = @_;
    my $original_seed = "SEED.b4_pad_ends";

    print STDERR "Moving $pfamA_acc/SEED to $pfamA_acc/$original_seed\n";
    move("$pfamA_acc/SEED", "$pfamA_acc/$original_seed") or die "Couldn't move $pfamA_acc/SEED to $pfamA_acc/$original_seed, $!";

    unless($max_pad) {
        $max_pad = -1;
        until($max_pad >= 0) {
            print STDERR "What do you want to set max_pad to [default 10]\n";
            my $ans = <STDIN>;
            chomp $ans;
            $ans = 10 unless($ans);
            if($ans =~ /^(\d+)$/) {
                $max_pad = $1;
            }
            else {
                print STDERR "Need to pass in a positive integer, try again!\n";
            }
        }
    }

    my $command = "pad_ends.pl -align $pfamA_acc/$original_seed -max_pad $max_pad > $pfamA_acc/SEED";
    print STDERR "Running '$command'\n";
    system("$command") and die "Couldn't run '$command', $!";
}


sub wholeseq {
    
    my $pfamA_acc = shift;
    my $original_seed = "SEED.b4_wholeseq";

    print STDERR "Moving $pfamA_acc/SEED to $pfamA_acc/$original_seed\n";
    move("$pfamA_acc/SEED", "$pfamA_acc/$original_seed") or die "Couldn't move $pfamA_acc/SEED to $pfamA_acc/$original_seed, $!";

    my $command = "wholeseq.pl -align $pfamA_acc/$original_seed -mu > $pfamA_acc/SEED";
    print STDERR "Running '$command'\n";
    system("$command") and die "Couldn't run '$command', $!";
}

sub add_match_states {

    my $pfamA_acc = shift;
    my $original_seed = "SEED.b4_add_match_states";    
        
    print STDERR "Copying $pfamA_acc/SEED to $pfamA_acc/$original_seed\n";
    copy("$pfamA_acc/SEED", "$pfamA_acc/$original_seed") or die "Couldn't copy $pfamA_acc/SEED to $pfamA_acc/$original_seed, $!";

    system("cd $pfamA_acc; add_match_states; cd ../") and die "Couldn't run add_match_states on $pfamA_acc, $!";

}


sub process_noSEED {

    #This subroutine deals with the families in the 'noSEED' directory only.
    #It is meant to be run from the SeedSurgery directory, so the noSEED directory
    #is a sub directory of the current working directory, and the Done directory is
    #a sibling of the SeedSurgery directory. Relative paths are used throughout so
    #that the script does not break in future releases.

    my $noSEED_dir = "noSEED";
    unless(-d $noSEED_dir) {
        die "Couldn't find the '$noSEED_dir' directory in the current working directory.\nPlease run this script from the SeedSurgery directory.\n";
    }

    #Move into the noSEED directory so that each family is a direct sub directory,
    #which allows the existing editing/presentation code to be reused unchanged
    chdir($noSEED_dir) or die "Couldn't chdir into $noSEED_dir, $!";

    #The Done directory sits inside the SeedSurgery directory, so from inside
    #the noSEED directory it is one level up (SeedSurgery/noSEED -> ../Done)
    $done_dir = "../Done";
    mkdir($done_dir, 0755) unless(-d $done_dir);

    #File to record families that could not be processed
    my $failures = "noSEED_failures.log";

    #Get a sorted list of the families in the noSEED directory
    opendir(my $dh, ".") or die "Couldn't open current directory, $!";
    my @families = sort grep { /^PF\d{5}$/ and -d $_ } readdir($dh);
    closedir($dh);

    unless(@families) {
        print STDERR "No families found in the '$noSEED_dir' directory\n";
        return;
    }

    my $total = scalar(@families);
    my $count = 0;
    foreach my $pfamA_acc (@families) {
        $count++;
        print STDERR "\n$count/$total: *** $pfamA_acc ***\n\n";

        #Build the RP filtered SEED for this family. If this fails (no usable
        #sequences) the family is logged and we move on to the next one.
        next unless(make_rp_seed($pfamA_acc, $failures));

        #Present the SEED to the curator, exactly as in the normal workflow
        print STDERR "Opening $pfamA_acc/SEED in belvu";
        system("belvu $pfamA_acc/SEED");
        user_response($pfamA_acc);
    }
}


sub make_rp_seed {

    #For a family in the noSEED directory this performs:
    #  cp ALIGN SEED
    #  seedRP.pl        (creates SEED.rp)
    #If SEED.rp has sequences in it, it is copied to SEED and we return 1.
    #If SEED.rp is empty (no RP sequences in the ALIGN), a warning is given,
    #the family is logged in the failures file, and we return 0.

    my ($pfamA_acc, $failures) = @_;

    unless(-s "$pfamA_acc/ALIGN") {
        warn "$pfamA_acc: no ALIGN file (or it is empty), skipping\n";
        log_failure($failures, $pfamA_acc, "no ALIGN file");
        return 0;
    }

    #cp ALIGN SEED
    copy("$pfamA_acc/ALIGN", "$pfamA_acc/SEED") or die "Couldn't copy $pfamA_acc/ALIGN to $pfamA_acc/SEED, $!";

    #seedRP.pl reads SEED and writes SEED.rp, both in the current directory,
    #so we have to run it from inside the family directory
    chdir($pfamA_acc) or die "Couldn't chdir into $pfamA_acc, $!";
    my $rv = system("seedRP.pl");
    chdir("..") or die "Couldn't chdir back out of $pfamA_acc, $!";

    if($rv) {
        warn "$pfamA_acc: seedRP.pl failed\n";
        log_failure($failures, $pfamA_acc, "seedRP.pl failed");
        return 0;
    }

    #If SEED.rp has no aligned sequences then there are no sequences in the ALIGN
    #that can be used to make the SEED alignment
    unless(seed_rp_has_sequences("$pfamA_acc/SEED.rp")) {
        warn "$pfamA_acc: no sequences in SEED.rp, none of the ALIGN sequences can be used to make the SEED\n";
        log_failure($failures, $pfamA_acc, "no sequences in SEED.rp");
        return 0;
    }

    #SEED.rp has sequences, so use it as the SEED
    copy("$pfamA_acc/SEED.rp", "$pfamA_acc/SEED") or die "Couldn't copy $pfamA_acc/SEED.rp to $pfamA_acc/SEED, $!";

    #Keep a copy of the SEED before any manual editing so the 'o' option works
    copy("$pfamA_acc/SEED", "$pfamA_acc/SEED.original") or die "Couldn't copy $pfamA_acc/SEED to $pfamA_acc/SEED.original, $!";

    return 1;
}


sub seed_rp_has_sequences {

    #Returns true if the file contains at least one aligned sequence line
    #(e.g. B5W3F0.1/12-152 ....), false otherwise. Note that SEED.rp can still
    #contain #=GF lines (for nested families) even when there are no sequences.

    my ($file) = @_;

    return 0 unless(-s $file);

    open(my $fh, $file) or die "Couldn't open fh to $file, $!";
    while(<$fh>) {
        if(/^\S+\.\d+\/\d+-\d+/) {
            close $fh;
            return 1;
        }
    }
    close $fh;

    return 0;
}


sub log_failure {

    my ($failures, $pfamA_acc, $reason) = @_;

    open(my $fh, ">>", $failures) or die "Couldn't open fh to $failures, $!";
    print $fh "$pfamA_acc\t$reason\n";
    close $fh;
}


sub help {
    print<<EOF;

This script should be used during seed surgery. It will loop
through the families in the list provided on the command line.
The families should be present in the current working directory.

The script will open each SEED alignment in belvu.
After closing the alignment you will be asked the following:

Is the family done, or do you want to edit it further (enter y/n/q/a/b/c/f/o/e/p/w):

  y: yes family is done, move to the 'Done' directory
  n: no family is not done, leave it where it is and move on to next family
  q: quit the script

  a: add match states to SEED (use for families with nested domain)
  b: open SEED alignment again
  c: run create_alignment.pl on the family
  e: run extend.pl on the family
  f: fast pad_ends.pl (same as p, but won't ask your for max_pad value, and will move family to Done) 
  o: go back to original seed (SEED.original)
  p: run pad_ends.pl on the family
  w: run wholeseq.pl on the family

If you choose y or n, the script will move onto the next family. If
you choose one of the other options, the script will run the relevant
script, and then you will be able to choose from the above options again.

Before opening each SEED alignment, the script will make a copy of the
SEED called SEED.original (unless SEED.original is already present). You
can copy this file, but please do not edit SEED.original. The 'o' option
will copy SEED.original to SEED.

The script will create the 'Done' directory if not already present.

-noSEED option:

Use the -noSEED option to handle the Pfam families in the 'noSEED'
directory (the families that were left with no SEED after seed surgery).
With this option the script does NOT look at the SeedSurgery families at
all. It should be run from the SeedSurgery directory (the one that
contains the 'noSEED' directory).

For each family in the 'noSEED' directory the script will:

  1. cp ALIGN SEED
  2. run seedRP.pl (this creates SEED.rp)

If SEED.rp has no sequences in it, then none of the sequences in the
ALIGN can be used to make the SEED alignment. A warning is given and
the family is logged in the 'noSEED_failures.log' file, and the script
moves on to the next family. If SEED.rp has sequences in it, it is
copied to SEED and presented to the curator with the usual editing
options described above. If the curator chooses 'y', the family is moved
to the 'Done' directory, which sits inside the SeedSurgery directory.

Relative paths are used throughout so that the script does not break in
future releases.

Usage:

$0 -list <list of families>
$0 -noSEED

EOF

    exit;
}
