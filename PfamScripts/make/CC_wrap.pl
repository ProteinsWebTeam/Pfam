#!/usr/bin/env perl

#Script to wrap CC lines to 70 characters in DESC files

use strict;
use warnings;
use Text::Wrap;

$Text::Wrap::columns = 70;

my $desc_file = "DESC";
my $tmp_desc = "DESC.b4.CCedit.$$";

unless(-s $desc_file) {
    print "Can't find DESC file in cwd\n";
}

rename($desc_file, $tmp_desc) or die "Couldn't rename $desc_file to $tmp_desc, $!";

my $CC_line;
my $found_CC;
open(TMP, $tmp_desc) or die "Couldn't open $tmp_desc, $!";
open(DESC, ">$desc_file") or die "Couldn't open $desc_file for writing, $!";
while(<TMP>) {
    if(/^CC\s+(.+)/) {
        $CC_line .= $1." ";
        $found_CC=1;
    }
    else {
        if($found_CC) {
            $CC_line =~ s/ +/ /g; #Remove any double spaces
            my $text = wrap( "CC   ", "CC   ", $CC_line . "\n" );
            print DESC "$text";
            $found_CC="";
            print DESC $_;
        }
        else {
            print DESC $_;
        }
    }
}

if($found_CC) {
    $CC_line =~ s/ +/ /g; #Remove any double spaces
    my $text = wrap( "CC   ", "CC   ", $CC_line . "\n" );
    print DESC "$text";
}

close TMP;
close DESC;
