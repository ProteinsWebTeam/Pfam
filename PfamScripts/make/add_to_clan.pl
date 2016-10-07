#! /usr/bin/perl

# Script to help adding families to a clan

use strict;
use Getopt::Long;

my ($file,$clan,$family,$reason);
&GetOptions('file=s'   => \$file,
	    'clan=s'   => \$clan,
	    'family=s' => \$family,
	    'reason=s' => \$reason,
    );

if (! $reason){
    die "Must give a reason! e.g. -reason 'Based on SCOP'";
}


if (! $clan){
    print STDERR "Assuming that you have appended clan identifiers in your file of accession!\n";
} else {
    unless ($clan =~ /^CL\d{4}$/){
	die "Your clan $clan does not look like a clan accession number";
    }
}

if (-d $clan){
    die "$clan directory already exists!";
}


if (! $file and ! $family){
    die "please use -file argument to give me a file with list of Pfam accessions or use the -family opetion to give me 1 family";
}


# Get list of all families to add to clan
my @family;
my %fam_clan;
if ($family){
    push @family,$family;
    $fam_clan{$family}=$clan;
}

if ($file){
    open (FH, "$file") or die "Cannot open $file";
    while(<FH>){
	my @line=split('\s+');

	if ($line[0]=~/^(PF\d{5})/){
	    my $family=$1;
	    
	    push @family,$family;

	    if ($line[1]=~/^(CL\d{4})/){
		$fam_clan{$family}=$1;
	    } else {
		$fam_clan{$family}=$clan;
	    }
	    
	} else {
	    warn "Unrecognised line $_";
	}


    }
    close FH;
}


foreach my $element (@family){
    my $clan_id=$fam_clan{$element};
    print STDERR "Add $element to $clan_id\n";

    if (! $clan_id){
	die "No clan identifier given!";
    }
    system("mkdir $clan_id");
    chdir $clan_id;
    add_family($element,$clan_id);
    chdir '..';
    system("rm -fr $clan_id");
}

sub add_family {
    my ($family,$clan)=@_;

    print STDERR "pfco $family\n";
    system ("pfco $family") and die;
    
    print STDERR "Adding CL line to $family\n";
    open (DESC, "$family/DESC") or die "Cannot open $family/DESC";
    open (WRITE, "> $family/NEWDESC") or die "Cannot write to $family/NEWDESC";
    while(<DESC>){
	print WRITE;
	if (/^CL/){
	    die "Found Clan line in $family!!!";
	}
	if (/^TP/){
	    print WRITE "CL   $clan\n";
	}
    }
    close DESC;
    close WRITE;
    
    print STDERR "Replacing DESC file with new version\n";
    system ("mv $family/DESC $family/OLDDESC") and die;
    system ("mv $family/NEWDESC $family/DESC") and die;
    
    # Now try and check family back in.
    print STDERR "pfci $family -add_to_clan -m \'Added family to $clan. $reason\'\n";
    system ("pfci $family -add_to_clan -m \'Added family to $clan. $reason\'") and die;
}
