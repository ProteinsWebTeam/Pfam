#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use File::Copy;

#Script used to check seeds during seed surgery
#If the user is happy with the seed, it will get moved to a directory called Done in cwd
#Else there will be options to extend/wholeseq/create_alignment/pad_ends/go back to original seed

my ($list, $help);
GetOptions('list=s' => \$list,
            'help'  => \$help);

if($help or !$list) {
    help();
}
my $done_dir = "Done";
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
        move($pfamA_acc, "$done_dir/$pfamA_acc") or die "Couldn't move $pfamA_acc to $done_dir, $!";
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
        move($pfamA_acc, "$done_dir/$pfamA_acc") or die "Couldn't move $pfamA_acc to $done_dir, $!";
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

Usage:

$0 -list <list of families>

EOF

    exit;
}
