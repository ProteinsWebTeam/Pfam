#
# Some global variables and methods for doing Rfam things
#
# sgj

package Rfam;

use strict;

use vars qw( $root_dir
	     $current_dir
	     $accession_dir
	     $releases_dir
	     $scripts_dir
	     $acclog_file );

$root_dir      = "/nfs/farm/Rfam";
$current_dir   = "$root_dir/CURRENT";
$accession_dir = "$root_dir/ACCESSION";
$releases_dir  = "$root_dir/RELEASES";
$scripts_dir   = "$root_dir/scripts";
$acclog_file   = "$accession_dir/acclog";


sub get_allaccs {
    my @accs;
    open( F, $acclog_file ) or die "can't open acclog file $acclog_file";
    while(<F>) {
	if( /^(RF\d+)\s+/ ) {
	    push( @accs, $1 );
	}
    }
    close F;
    return @accs;
}

sub acc2id {
    my $acc = shift;
    open( F, $acclog_file ) or die "can't open acclog file $acclog_file";
    while(<F>) {
	if( /^$acc\s+(\S+)/ ) {
	    return $1;
	}
    }
    warn "Can't find $acc in file $acclog_file\n";
    return 0;
}

sub id2acc {
    my $id = shift;
    open( F, $acclog_file ) or die "can't open acclog file $acclog_file";
    while(<F>) {
	if( /^(RF\d+)\s+$id/ ) {
	    return $1;
	}
    }
    warn "Can't find $id in file $acclog_file\n";
    return 0;
}


1;
