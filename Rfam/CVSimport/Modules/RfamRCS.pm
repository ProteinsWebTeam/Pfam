# 
# Rfam RCS related things
#
# sgj

package RfamRCS;

use strict;
use Rfam;

use vars qw( @view_file_set
	     @align_file_set
	     @ann_file_set
	     @rcs_file_set );

@align_file_set = ( "SEED", "ALIGN" );
@view_file_set  = ( "SEED.ann", "ALIGN.ann" ); # must be in same order as @align_file_set
@ann_file_set   = ( "DESC" );
@rcs_file_set   = ( @align_file_set, @ann_file_set );


sub view_file_errors {
    my $family = shift;
    my $error;
    foreach my $viewfile ( @view_file_set ) {
	if( not -s "$Rfam::root_dir/CURRENT/$family/$viewfile" ) {
	    warn "$family: $viewfile empty\n";
	    $error ++;
	    next;
	}
	foreach my $famfile ( @rcs_file_set ) {
	    if( -M "$Rfam::current_dir/$family/$viewfile" > -M "$Rfam::current_dir/$family/$famfile" ) {
		warn "$family: $viewfile is older than $famfile\n";
		$error ++;
	    }
	}
    }
    return $error;
}


sub make_align_release_file {
    my $type = shift;
    my $path = shift;
    my( $annfile, $filename );
    if( $type eq "SEED" ) {
	$annfile  = "SEED.ann";
	$filename = "Rfam.seed";
    }
    elsif( $type eq "FULL" ) {
	$annfile  = "ALIGN.ann";
	$filename = "Rfam.full";
    }
    else {
	die "RfamRCS: don't understand the type of file you want";
    }

    foreach my $acc ( &Rfam::get_allaccs() ) {
	my $id = Rfam::acc2id( $acc );
	system "cat $Rfam::current_dir/$id/$annfile >> $path/$filename" and die "PfamRCS: failed to read $Rfam::current_dir/$id/$annfile";
    }
    return 0;
}


1;
