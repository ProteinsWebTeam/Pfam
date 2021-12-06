#!/usr/bin/env perl

#Script to take info from excel spreadsheet from Laks and add it to DESC files
#Need to save the excel file as a tsv file and pass this in on the command line

use strict;
use warnings;
use Text::Wrap;
use Getopt::Long;

my $annotation_file;
GetOptions('excel_file=s' => \$annotation_file);

unless($annotation_file and -s $annotation_file) {
    print STDERR "Need to pass excel file (saved in tsv format) on the command line\nEg $0 -excel_file excel.txt\n";
    exit;
}

$Text::Wrap::columns = 70;

#Go through each family and add info to DESC file
open(FILE, $annotation_file) or die "Couldn't open fh to $annotation_file, $!";
while(<FILE>) {

    next if(/^#/ or /file/);

    my ($name, $pfamA_acc_id, $clan, $de_line, $cc_line, $pmid, $pmc_id, $notes) = ("","","","","","","","");
    ($name, $pfamA_acc_id, $clan, $de_line, $cc_line, $pmid, $pmc_id, $notes) = split(/\t/, $_); 

    unless(-s "$name/DESC") {
        print STDERR "$name/DESC does not exist\n";
        next;
    }
    
    print STDERR "Doing $name\n";

    my $original_desc = "$name/DESC.b4.ann.$$";
    my $new_desc = "$name/DESC";
    rename($new_desc, $original_desc) or die "Couldn't rename $new_desc to $original_desc, $!";

    open(NEW, ">$new_desc") or die "Couldn't open fh to $new_desc, $!";
    open(OLD, $original_desc) or die "Couldn't open fh to $original_desc, $!";
    while(<OLD>) {
        if(/^ID/) {
            if($pfamA_acc_id) {
                print NEW "ID   $name (*Existing entry $pfamA_acc_id*)\n";
            }
            else {
                print NEW "ID   $name\n";
            }
        }
        elsif(/^DE/) {
            $de_line =~ s/"//g; #Remove any annoying " that Excel sometimes adds
            print NEW "DE   $de_line\n";
        }
        elsif(/^AU/) {
            next if(/Aravind/ or /Iyer/); #Don't add these authors twice
            print NEW if(/^AU\s+\S+/); #If any authors are in the DESC, keep them
            print NEW "AU   Aravind L;0000-0003-0771-253X\n";
            print NEW "AU   Iyer LM;0000-0002-4844-2022\n";
        }
        elsif(/^SE/) {
            print NEW "SE   Aravind L\n";
        }
        elsif(/^TP/) {
            print NEW $_;
            if($clan =~ /\S+/) {
                print NEW "CL   *$clan*\n";
            }
        }
        elsif(/^CL/) {
            unless($clan) { #Don't add CL line if already added
                print NEW $_;
            }
        }
        else {
            unless(/^CC/ or /^\*\*/) {
                print NEW $_;
            }
        }
    }
    close OLD;
    
    $cc_line =~ s/"//g; #Remove any annoying " that Excel sometimes adds

    my $CC_text = wrap( "CC   ", "CC   ", $cc_line . "\n" );
    print NEW $CC_text;

    if($notes =~ /\S+/) {
        $notes =~ s/"//g; #Remove any annoying " that Excel sometimes adds
        print NEW "**   $notes";
    }
    if($pmc_id) {
        $pmc_id =~ s/"//g; #Remove any annoying " that Excel sometimes adds
        print NEW "**   Additional refs: $pmc_id\n";
    }

    close NEW;

    #Add references
    chdir($name) or die "Couldn't chdir into $name, $!";
    $pmid =~ s/"//g; #Remove any annoying " that Excel sometimes adds
    my @pmids = split(/[,;]/, $pmid);
    foreach my $pmid (@pmids) {
        system("add_ref.pl $pmid") and die "Problem running add_ref.pl $pmid on $name, $!";
    }
    chdir("../") or die "Couldn't chdir up from $name, $!";
}
close FILE;
