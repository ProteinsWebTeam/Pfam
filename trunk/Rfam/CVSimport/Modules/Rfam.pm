#
# Some global variables and methods for doing Rfam things
#
# sgj

package Rfam;

use strict;

use vars qw( @ISA
	     @EXPORT
	     $root_dir
	     $current_dir
	     $accession_dir
	     $releases_dir
	     $rcs_master_dir
	     $rcs_attic_dir
	     $scripts_dir
	     $acclog_file 
	     @view_file_set
             @align_file_set
             @model_file_set
	     @ann_file_set
             @rcs_file_set 
	     @optional_file_set );

@ISA    = qw( Exporter );
@EXPORT = qw( $root_dir
	      $current_dir
	      $accession_dir 
	      $releases_dir 
	      $rcs_master_dir 
	      $rcs_attic_dir 
	      $scripts_dir    
	      $acclog_file
	      @view_file_set
	      @align_file_set
	      @ann_file_set
	      @model_file_set
	      @rcs_file_set 
              @optional_file_set );

$root_dir       = "/nfs/farm/Rfam";
$current_dir    = "$root_dir/CURRENT";
$accession_dir  = "/pfam/db/Rfam/ACCESSION";
$releases_dir   = "/pfam/db/Rfam/RELEASES";
$rcs_master_dir = "$root_dir/RCS_MASTER";
$rcs_attic_dir  = "$root_dir/RCS_ATTIC";
$scripts_dir    = "/pfam/db/Rfam/scripts";
$acclog_file    = "$accession_dir/acclog";

@align_file_set    = ( "SEED", "ALIGN" );
@view_file_set     = ( "SEED.ann", "ALIGN.ann" ); # must be in same order as @align_file_set
@ann_file_set      = ( "DESC" );
@model_file_set    = ( "CM" );
@rcs_file_set      = ( @align_file_set, @ann_file_set, @model_file_set );
@optional_file_set = ();


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
