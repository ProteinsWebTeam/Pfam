#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $bioperl_dir;
use lib $rfam_mod_dir;

use strict;
use Getopt::Long;
use Rfam;
use Rfam::RfamAlign;

my $nolock;

&GetOptions( "n" => \$nolock );

my $acc = shift;
chdir "$Rfam::current_dir/$acc" or die;

my @ann;
open( DESC, "DESC" ) or die;
while( <DESC> ) {
    chomp;  # remove newline and add it again to make sure
    unless( /^\*\*\s+/ ) {
	push( @ann, "#=GF $_\n" );
    }
}
close DESC;

foreach my $file ( @Rfam::align_file_set ) {
    my $aln = new Rfam::RfamAlign;
    open( AOU, ">$file.tmp" ) or die;
    open( ALN, $file ) or die;
    $aln -> read_stockholm( \*ALN );
    close ALN;
    my $newaln = $aln -> order_by_embl_taxonomy();
    $newaln -> write_stockholm( \*AOU );

    my $numseq = scalar ( $aln -> each_seq() );

    open( ALNOUT, ">$file.ann" ) or die;
    my $seen;
    open( REF, "sreformat --mingap stockholm $file.tmp |" ) or die;
    while( <REF> ) {
	next if( /^\#=GF AU / );
	if( /^\#=G/ and not $seen ) {
	    print ALNOUT @ann;
	    print ALNOUT "#=GF SQ   $numseq\n";
	    $seen = 1;
	}
	elsif( /^\S+\/\d+-\d+/ and not $seen ) {
	    print ALNOUT @ann;
	    print ALNOUT "#=GF SQ   $numseq\n\n";
	    $seen = 1;
	}
	print ALNOUT;
    }
    close REF or die;
    close ALNOUT;

    unlink "$file.tmp" or die;
}


# copy web based stuff around

#system("cp -f $Rfam::current_dir/$acc/ALIGN /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/full/$acc.full");
#system("gzip -f /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/full/$acc.full");

#system("/pfam/db/Rfam/scripts/wwwrelease/new_parse_rfam.pl --input_dir /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data --output_dir  /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/markup_align --file_type full --ss_cons_only --family $acc ");

#system("cp -f $Rfam::current_dir/$acc/SEED /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/seed/$acc.full");
#system("gzip  -f /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/seed/$acc.full");

#system("/pfam/db/Rfam/scripts/wwwrelease/new_parse_rfam.pl --input_dir /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data --output_dir /nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/markup_align --file_type seed --family $acc");

# remove locks, tmp files etc

if( -e "$Rfam::current_dir/$acc/todo.view" ) {
    unlink("$Rfam::current_dir/$acc/todo.view") or warn "can't remove todo.view file\n";
}

unless( $nolock ) {
    system("rfabort -u VIEW $acc") and warn "$acc: abort view lock failed - no lock?";
}
