#!/usr/local/bin/perl -w

#
# author sgj
# heavily borrowed from Ewan's pfam code
#
use lib '/pfam/db/bioperl';
BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Getopt::Std;
use Rfam;
use RfamRCS;
use UpdateRDB;
use vars qw($opt_m);

&getopt("m:");
my $message = $opt_m;

if( $#ARGV == -1 ) {
    &RfamRCS::show_rcs_help(\*STDOUT);
    exit;
}

## first, check that the database is not locked

my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
if( $locked ) {
    my $allowed;
    foreach my $allow ( @{$allow_ref} ) {
	$allowed = 1 if( $allow eq "rfci" );
    }
    unless( $allowed ) {
	die "rfci aborted: database is locked by $locker";
    } 
}

my $acc = shift;
    
if( $acc =~ /\/$/ ) {
    $acc =~ s/\/$//g;
}

if( !(-d $acc) ) {
    die "rfci: [$acc] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if( ! &RfamRCS::check_family_directory_exists($acc) ) {
    die "rfci: Family [$acc] does not have a directory in RCS_MASTER.\nIf this is a new family, check in a new family with pfnew\n";
}

if( ! &RfamRCS::check_family_exists($acc) ) {
    die "rfci: Bad error Family [$acc] has RCS_MASTER directory, but not the correct files internally.\nPlease contact pfam\@sanger.ac.uk to resolve the error\n";
}

my $name = `whoami`;
chop $name;

my ($haslocked,$peoplelocked) = &RfamRCS::user_has_locked_family($acc,$name);

if( $haslocked == 0 ) {
    die "rfci: This family [$acc] was not locked by you[$name], it was locked by [$peoplelocked]\n";
}

if( ! open(DESC,"./$acc/DESC") ) {
    die "rfci: A bad error - cannot open the desc file [./$acc/DESC]. Yikes! [$!]";
}

my $has_ac;

while(<DESC>) {
    if(/^AC/) {
	$has_ac=1;
    }
}    
close(DESC);

if(!$has_ac) {
    die "rfci: Your DESC file has no AC line. This is bad!\n";
}

if( &RfamRCS::move_files_to_rcs_directory($acc, $acc) == 0 ) {
    die "rfci: Could not move RCS files to directory for family [$acc]. Problem!\n";
}

my $comment;
if( !defined $message ) {
    print "Please give a comment for family [$acc]\n";
    print "Finish comment by a . on the line by itself\n"; 

    while( <STDIN> ) {
	chop;
	/^\s*\.\s*$/ && last;
	$comment .= "$_\n";
    }
} else {
    $comment = $message;
}

if( &RfamRCS::check_in_rcs_files($acc, $comment) == 0 ) {
    die "rfci: Could not check in files for $acc. This is a bad error\n";
}

if( &RfamRCS::update_current_directory($acc) == 0 ) {
    die "rfci: Could not update directory for $acc\n";
}
  
## rdb stuff

print STDERR "\nChecking family into RDB\n";

eval {
  my $rdb =  Rfam::switchover_rdb_update();
  my $db = Rfam::default_db();
  my $en = $db->get_Entry_by_acc( $acc);
  my $id = $en->author();
  $rdb->check_in_Entry( $en );
};

$@ and do {
  print STDERR "RFCI: RDB update; Could not update relational database for family $acc [$@]\n";
};
print STDERR "RDB update succesful\n";

&RfamRCS::make_view_files($acc); 

print STDERR "Generating the coloured mark-up\n";

### Do the FULL Alignment
system("cp -f ./$acc/ALIGN ./$acc/$acc.full");
system("mv -f  ./$acc/$acc.full /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/full/");
system("gzip  -f /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/full/$acc.full");

system("/pfam/db/Rfam/scripts/wwwrelease/new_parse_rfam.pl --input_dir /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data --output_dir  /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/markup_align --file_type full --ss_cons_only --family $acc ");

### Do the SEED Alignment
system("cp -f ./$acc/SEED  ./$acc/$acc.full");
system("mv -f  ./$acc/$acc.full /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/seed/");
system("gzip  -f /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/seed/$acc.full");

system("/pfam/db/Rfam/scripts/wwwrelease/new_parse_rfam.pl --input_dir /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data --output_dir /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/markup_align --file_type seed --family $acc");

print STDERR "\n\nChecked in family [$acc]\n";
