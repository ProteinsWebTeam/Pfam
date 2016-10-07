#! /usr/bin/perl -w

# A script to change the type of a Pfam family

use strict;
use Getopt::Long;


my ($list_file,$type,$acc);
&GetOptions('file=s'  => \$list_file,
	    'type=s' => \$type,
	    'acc=s' => \$acc);

if (! $type){
    print STDERR "Must specify type on command line\n";
    &help;
}

if (! $list_file and ! $acc){
    print STDERR "Must specify input file or accession on command line\n";
    &help;
}


unless ($type  =~ /Domain|Family|Motif|Repeat|Coiled-coil|Disordered/){
    die "Do not recognise type: $type";
}

if ($acc){
    if ($acc =~ /(PF\d{5})/){
	&add_type($acc,$type);
    }
}

if ($list_file){
    open (FH, $list_file) or die "cannot open $list_file";
    while(<FH>){
	if (/(PF\d{5})/){
	    &add_type($1,$type);
	}
    }
    close FH;
}


sub add_type {
    my ($acc,$type)=@_;

    if (! $acc){die "No accession passed to add_type";}
    if (! $type){die "No type passed to add_type";}
    
    print STDERR "Trying to change type of $acc to $type\n";
    system ("pfco $acc") and die "System call failed pfco $acc";

    # Edit DESC file
    open (DESC, "$acc/DESC") or die "Cannot open $acc/DESC file";
    open (DESCNEW, "> $acc/DESCNEW") or die "cannot write to $acc/DESCNEW";
    while(<DESC>){
	if (/^TP   (\S+)/){
	    my $existing_type=$1;
	    if ($existing_type eq $type){
		die "Trying to change to type $type. But family $acc already has that type!";
	    } else {
		print DESCNEW "TP   $type\n";
	    }
	} else {
	    print DESCNEW;
	}
    }
    close DESC;
    close DESCNEW;

    # Overwrite DESC file
    system ("mv $acc/DESC $acc/DESCOLD") and die "Cannot mv $acc/DESC $acc/DESCOLD";
    system ("mv $acc/DESCNEW $acc/DESC") and die "Cannot mv $acc/DESCNEW $acc/DESC";

    # Now check family back in - assuming checked in family was correct and so using -i
    print STDERR "Checking in $acc\n";
    system ("pfci $acc -onlydesc -i -m \'Changed type to $type\'") and die "Failed to pfci $acc -m \'Changed type to $type\'";

    #Clean up
    system ("rm -fr $acc");
}


sub help {
    print STDERR "Usage: $0 -file <filename> -acc <Pfam acc> -type <Domain|Family|Motif|Repeat|Coiled-coil|Disordered>\n";
    exit 0;
}
