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
	     $rfamseq_root_dir
	     $rfamseq_current_dir
	     $rfamseq_new_dir
	     $rfamseq_current_inx
	     $rfamseq_new_inx
	     @view_file_set
             @align_file_set
             @model_file_set
	     @ann_file_set
             @rcs_file_set 
	     @output_file_set
	     @scores_file_set
	     @optional_file_set 
	     $view_maker );

@ISA    = qw( Exporter );
@EXPORT = qw( $root_dir
	      $current_dir
	      $accession_dir 
	      $releases_dir 
	      $rcs_master_dir 
	      $rcs_attic_dir 
	      $scripts_dir    
	      $acclog_file
	      $rfamseq_root_dir
	      $rfamseq_current_dir
	      $rfamseq_new_dir
	      $rfamseq_current_inx
	      $rfamseq_new_inx
	      @view_file_set
	      @align_file_set
	      @ann_file_set
	      @model_file_set
	      @rcs_file_set 
              @output_file_set
	      @scores_file_set
	      @optional_file_set 
	      $view_maker );

$root_dir       = "/pfam/db/Rfam";
$current_dir    = "$root_dir/CURRENT";
$accession_dir  = "$root_dir/ACCESSION";
$releases_dir   = "$root_dir/RELEASES";
$rcs_master_dir = "$root_dir/RCS_MASTER";
$rcs_attic_dir  = "$root_dir/RCS_ATTIC";
$scripts_dir    = "$root_dir/scripts";
$acclog_file    = "$accession_dir/acclog";

$rfamseq_root_dir    = "/pfam/db/rfamseq";
$rfamseq_current_dir = "$rfamseq_root_dir/CURRENT";
$rfamseq_new_dir     = "$rfamseq_root_dir/NEW";
$rfamseq_current_inx = "$rfamseq_current_dir/rfamseq.fa.bpi";
$rfamseq_new_inx     = "$rfamseq_new_dir/rfamseq.fa.bpi";

@align_file_set    = ( "SEED", "ALIGN" );
@view_file_set     = ( "SEED.ann", "ALIGN.ann" ); # must be in same order as @align_file_set
@ann_file_set      = ( "DESC" );
@output_file_set   = ( "OUTPUT" );
@model_file_set    = ( "CM" );
@scores_file_set   = ( "scores" );
@rcs_file_set      = ( @align_file_set, @ann_file_set, @model_file_set, @output_file_set, @scores_file_set );

$view_maker = "$scripts_dir/rfamrcs/makerfamview.pl";

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


# this is horrible -- need some db objects SOON!

sub acc2id {
    my $acc = shift;
    open( F, $acclog_file ) or die "can't open acclog file $acclog_file";
    while(<F>) {
	if( /^$acc\s+\[(\S+)\]/ ) {
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
	if( /^(RF\d+)\s+\[$id\]/ ) {
	    return $1;
	}
    }
    warn "Can't find $id in file $acclog_file\n";
    return 0;
}

sub is_acc {
    my $test = shift;
    open( F, $acclog_file ) or die "can't open acclog file $acclog_file";
    while(<F>) {
	if( /^$test\s+\[\S+\]/ ) {
	    return 1;
	}
    }
    return 0;
}

sub is_id {
    my $test = shift;
    open( F, $acclog_file ) or die "can't open acclog file $acclog_file";
    while(<F>) {
	if( /^RF\d+\s+\[$test\]/ ) {
	    return 1;
	}
    }
    return 0;
}




1;
