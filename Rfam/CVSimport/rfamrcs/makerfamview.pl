#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Rfam;
use Rfam::RfamAlign;

my $nolock;

&GetOptions( "n" => \$nolock );

my $acc = shift;

chdir "$Rfam::current_dir/$acc" or die "cant move to the current dir for this family $acc";

my @ann;
open( DESC, "DESC" ) or die "cant open DESC file";
while( <DESC> ) {
    chomp;  # remove newline and add it again to make sure
    unless( /^\*\*\s+/ ) {
	push( @ann, "#=GF $_\n" );
    }
}
close DESC;

foreach my $file ( @Rfam::align_file_set ) {
    my $aln = new Rfam::RfamAlign;
    open( AOU, ">$file.tmp" ) or die "cant open $file.tmp file";
    open( ALN, $file ) or die "cant open $file";
    $aln -> read_stockholm( \*ALN );
    close ALN;
    my $newaln = $aln -> order_by_embl_taxonomy();
    $newaln -> write_stockholm( \*AOU );

    my $numseq = scalar ( $aln -> each_seq() );

    open( ALNOUT, ">$file.ann" ) or die "cant open $file.ann";;
    my $seen;
    open( REF, "sreformat --gapsym '.' -r -u --mingap stockholm $file.tmp |" ) or die "cant run the sreformat";
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

    unlink "$file.tmp" or die "cant remove the $file.tmp";
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
    system("rfabort.pl -u VIEW $acc") ==0 or warn "$acc: abort view lock failed - no lock?";
}
