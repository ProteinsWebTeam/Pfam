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
	     $rcs_index_file
	     $lock_file
	     $scripts_dir
	     $acclog_file
	     $rfamseq
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
use Rfam::DB::DB_RCS;
use Rfam::DB::DB_RDB;
use Rfam::UpdateRDB;

$root_dir       = "/pfam/db/Rfam";
$current_dir    = "$root_dir/CURRENT";
$accession_dir  = "$root_dir/ACCESSION";
$releases_dir   = "$root_dir/RELEASES";
$rcs_master_dir = "$root_dir/RCS_MASTER";
$rcs_attic_dir  = "$root_dir/RCS_ATTIC";
$scripts_dir    = "$root_dir/scripts";
$acclog_file    = "$accession_dir/acclog";
$rcs_index_file = "$accession_dir/accmap.dat";
$lock_file      = "$accession_dir/lock";

$rfamseq_root_dir    = "/pfam/db/rfamseq";
$rfamseq_current_dir = "$rfamseq_root_dir/CURRENT";
$rfamseq_new_dir     = "$rfamseq_root_dir/NEW";
$rfamseq_current_inx = "$rfamseq_current_dir/rfamseq.fa.bpi";
$rfamseq_new_inx     = "$rfamseq_new_dir/rfamseq.fa.bpi";
$rfamseq             = "$rfamseq_current_dir/rfamseq.fa";

@align_file_set    = ( "SEED", "ALIGN" );
@view_file_set     = ( "SEED.ann", "ALIGN.ann" ); # must be in same order as @align_file_set
@ann_file_set      = ( "DESC" );
@output_file_set   = ( "OUTPUT" );
@model_file_set    = ( "CM" );
@scores_file_set   = ( "scores" );
@rcs_file_set      = ( @align_file_set, @ann_file_set, @model_file_set, @output_file_set, @scores_file_set );

$view_maker = "$scripts_dir/rfamrcs/makerfamview.pl";

my $rdb_host = "pfam";
my $rdb_driver = "mysql";
my $rdb_user = "rfam";
my $rdb_pass = "mafp1";

my $external_rdb_name = "rfam";
my $switchover_rdb_name = "rfam2";
my $live_rdb_name = "rfamlive";
my $temp_rdb_name = "rfam_temp"; ## to be deleted!!

sub default_db{
    return Rfam::DB::DB_RCS->new( '-current'   => $current_dir,
				  '-attic'     => $rcs_attic_dir, 
				  '-index'     => $rcs_index_file,
				  '-lock_file' => $lock_file,
				  '-rfamseq'   => $rfamseq );
} 


sub external_rdb {
   my ($self) = @_;

   return Rfam::DB::DB_RDB->new('-db_name' => $external_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass);

}



sub switchover_rdb {
   my ($self) = @_;

   return Rfam::DB::DB_RDB->new('-db_name' => $switchover_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass);

}

sub live_rdb {
   my ($self) = @_;

   return Rfam::DB::DB_RDB->new('-db_name' => $live_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass);

}

sub temp_rdb {
   my ($self) = @_;

   return Rfam::DB::DB_RDB->new('-db_name' => $temp_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass);

}

sub external_rdb_update{
    my ($self) = @_;

    my $dont = $ENV{'DONT_UPDATE_PFAM_RDB'};

    if (defined $dont) {
	if ($dont =~ /true/i) {
	    return undef;
	}
	else {
	    my $mess = "UpdateRDB - ";
	    $mess .= "expecting DONT_UPDATE_PFAM_RDB to be true or undefined; ";
	    $mess .= "Found it to be $dont";
	    $self->throw( $mess );
       }
    }
    else { 
	return Rfam::UpdateRDB->new('-db_name' => $external_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdb_host,
				    '-db_user' => $rdb_user,
				    '-db_password' => $rdb_pass);
    }    

}


sub switchover_rdb_update{
    my ($self) = @_;

    my $dont = $ENV{'DONT_UPDATE_PFAM_RDB'};

    if (defined $dont) {
	if ($dont =~ /true/i) {
	    return undef;
	}
	else {
	    my $mess = "UpdateRDB - ";
	    $mess .= "expecting DONT_UPDATE_PFAM_RDB to be true or undefined; ";
	    $mess .= "Found it to be $dont";
	    $self->throw( $mess );
       }
    }
    else {
	return Rfam::UpdateRDB->new('-db_name' => $switchover_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdb_host,
				    '-db_user' => $rdb_user,
				    '-db_password' => $rdb_pass);
    }    

}

sub live_rdb_update{
    my ($self) = @_;

    my $dont = $ENV{'DONT_UPDATE_PFAM_RDB'};

    if (defined $dont) {
	if ($dont =~ /true/i) {
	    return undef;
	}
	else {
	    my $mess = "UpdateRDB - ";
	    $mess .= "expecting DONT_UPDATE_PFAM_RDB to be true or undefined; ";
	    $mess .= "Found it to be $dont";
	    $self->throw( $mess );
       }
    }
    else {
	return Rfam::UpdateRDB->new('-db_name' => $live_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdb_host,
				    '-db_user' => $rdb_user,
				    '-db_password' => $rdb_pass);
    }    

}

sub temp_rdb_update{
    my ($self) = @_;

    my $dont = $ENV{'DONT_UPDATE_PFAM_RDB'};

    if (defined $dont) {
	if ($dont =~ /true/i) {
	    return undef;
	}
	else {
	    my $mess = "UpdateRDB - ";
	    $mess .= "expecting DONT_UPDATE_PFAM_RDB to be true or undefined; ";
	    $mess .= "Found it to be $dont";
	    $self->throw( $mess );
       }
    }
    else {
	return Rfam::UpdateRDB->new('-db_name' => $temp_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdb_host,
				    '-db_user' => $rdb_user,
				    '-db_password' => $rdb_pass);
    }    

}

1;
