#
# Some global variables and methods for doing Rfam things
#
# sgj, jd7, pg5 -- I feel a gludge in the force

package Rfam;

use strict;

use vars qw( @ISA
	     @EXPORT
             $embl
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
             $clan_dir
             $clan_rcs_dir
             $clan_acclog_file
             $clan_index_file
	     $rfamseq
	     $rfamseq_root_dir
	     $rfamseq_current_dir
	     $rfamseq_new_dir
	     $rfamseq_current_inx
	     $rfamseq_new_inx
	     $rfamseq_run_dir
	     $rfamseq_farm
	     $rfamseq_farm_root_dir
	     $rfamseq_farm_current_dir
	     $rfamseq_farm_new_dir
	     $rfamseq_farm_current_inx
	     $rfamseq_farm_new_inx
	     $rfamseq_farm_run_dir
	     @view_file_set
             @align_file_set
             @model_file_set
	     @ann_file_set
             @rcs_file_set 
	     @output_file_set
	     @scores_file_set
	     @optional_file_set 
	     $view_maker 
             $infernal_path
             $scratch_farm
             $rfamseq_farm2_run_dir
             %forbidden_family_terms
             @forbidden_terms
             $hmmLib
             $familyDir
             $prc_path
             $hmmer2_path
             $hmmer3_path
             $cmSeqLib
             $hmmSeqLib
);

@ISA    = qw( Exporter );

use Rfam::DB::DB_RCS;
use Rfam::DB::DB_RDB;
use Rfam::UpdateRDB;
use Rfam::Clans::Clan_RCS;
#mfetch -d version
$embl = "embl_100";

$root_dir       = "/warehouse/pfam01/rfam/Production/Rfam";
$current_dir    = "$root_dir/CURRENT";
$accession_dir  = "$root_dir/ACCESSION";
$releases_dir   = "$root_dir/RELEASES";
$rcs_master_dir = "$root_dir/RCS_MASTER";
$rcs_attic_dir  = "$root_dir/RCS_ATTIC";
$scripts_dir    = "/software/rfam/scripts/";
$acclog_file    = "$accession_dir/acclog";
$rcs_index_file = "$accession_dir/accmap.dat";
$lock_file      = "$accession_dir/lock";

#clan rcs files
$clan_dir   = "$root_dir/CLANS";
$clan_rcs_dir       = "$root_dir/RCS_CLAN";
$clan_acclog_file   = "$root_dir/CLAN_ACCESSION/clanacclog";
$clan_index_file    = "$root_dir/CLAN_ACCESSION/clan_accmap.dat";


#$rfamseq_root_dir    = "/lustre/pfam/rfam/Production/rfamseq";
$rfamseq_root_dir    = "/nfs/pfam_nfs/rfam/rfamseq";
$rfamseq_current_dir = "$rfamseq_root_dir/CURRENT";
$rfamseq             = "$rfamseq_current_dir/rfamseq.fa";
$rfamseq_run_dir     = "/data/blastdb/Rfam/rfamseq"; 

#RFAMSEQ On THE FARM:-redundant
#$rfamseq_farm_root_dir    = "/lustre/pfam/rfam/Production/rfamseq";
#$rfamseq_farm_current_dir = "$rfamseq_farm_root_dir/CURRENT";
#$rfamseq_farm_new_dir     = "$rfamseq_farm_root_dir/NEW";
#$rfamseq_farm_current_inx = "$rfamseq_farm_current_dir/rfamseq.fa.bpi";
#$rfamseq_farm_new_inx     = "$rfamseq_farm_new_dir/rfamseq.fa.bpi";
#$rfamseq_farm             = "$rfamseq_farm_current_dir/rfamseq.fa";
#$rfamseq_farm_run_dir     = "/data/blastdb/Rfam/rfamseq"; 

#RFAMSEQ On FARM2:
#$rfamseq_farm2_run_dir     = "/lustre/scratch103/blastdb/Rfam/rfamseq"; 
$rfamseq_farm2_run_dir     = "/nfs/pfam_nfs/rfam/rfamseq/CURRENT";

#SCRATCH ON THE FARM:
$scratch_farm = "/lustre/scratch101/sanger";

#INFERNAL PATH
$infernal_path = "/software/rfam/share/infernal-1.0/bin";

#HMMER PATH
$hmmer2_path = "/software/pfam/src/hmmer-2.3.2/src";
$hmmer3_path = "/software/pfam/src/hmmer-3.0b2/bin";

######################################################################
#CLAN PATHS:
#$familyDir = "/lustre/pfam/rfam/Curation/RFSEQ10"; #Path to a directory containing all the families
$familyDir = "/warehouse/pfam01/rfam/Curation/RFSEQ10"; #Path to a directory containing all the families
#PRC:
$hmmLib = "/nfs/pfam_nfs/rfam/HMMLIB";
$prc_path = "/software/rfam/share/prc-1.5.4_nuc";

#CMSEARCH
$cmSeqLib = "/nfs/pfam_nfs/rfam/CMSEQLIB";

#HMMSEARCH3
$hmmSeqLib = "/nfs/pfam_nfs/rfam/HMM3SEQLIB";

######################################################################

@align_file_set    = ( "SEED", "ALIGN" );
@view_file_set     = ( "SEED.ann", "ALIGN.ann" ); # must be in same order as @align_file_set
@ann_file_set      = ( "DESC" );
@output_file_set   = ( "OUTPUT" );
@model_file_set    = ( "CM" );
@scores_file_set   = ( "scores" );
@rcs_file_set      = ( @align_file_set, @ann_file_set, @model_file_set, @output_file_set, @scores_file_set );
@optional_file_set = ("TABFILE", "scores.evalue", "out.list", "species");
#@optional_file_set = (@view_file_set); #

$view_maker = "/software/rfam/scripts/rfamrcs/makerfamview.pl";

#for Curation (rfamlive)
our $rdb_host = "pfamdb2a";
our $rdb_driver = "mysql";
our $rdb_user = "pfam";
our $rdb_pass = "mafp1";
our $rdb_port= "3303";

#for rfamlive-special access..
our $rdb_user_admin = "pfamadmin";
our $rdb_pass_admin = "mafpAdmin";

#for Release (rfamdev)
our $rdbHostDev = "pfamdb2a";
our $rdbUserDev = "pfam";
our $rdbPassDev = "mafp1";
our $rdbPortDev= "3301";
our $rdbNameDev= "rfam_10_1";

our $live_rdb_name = "rfamlive";
our $release_rdb_name = "rfam_10_1";
#temp databases used sequpdate switch over
our $backup_rdb_name = "rfamlive_backup";
our $test_rdb_name = "rfamlive_seq10";

######################################################################
#Dictionaries of forbidden terms used by rfmake.pl & ALIGN2SEED.pl
#Terms too common to use as "true" for the histograms & thresholds:
%forbidden_family_terms = (
    AND => 1,
    ARCH => 1,
    ARCHAEA => 1,
    ARCHAEAL => 1,
    BACT => 1,
    BACTERIA => 1,
    BACTERIAL => 1,
    BODY => 1,
    BOX => 1,
    CANDIDATE => 1,
    CD => 1,
    CHROMOSOME => 1,
    DATA => 1,
    DELTA  => 1,
    DNA => 1,
    DOMAIN => 1,
    DS => 1,
    EL => 1,
    ELEMENT => 1,
    EUK => 1,
    EUKARYOTE => 1,
    EUKARYOTIC => 1,
    EXON => 1,
    FAMILY  => 1,
    FAMILIES => 1,
    FOR => 1,
    FROM => 1,
    GENE => 1,
    GENOME => 1,
    HDV => 1,
    Hepatitis => 1,
    INTERGENIC => 1,
    INTRON => 1,
    NUCLEAR => 1,
    OF => 1,
    PHAGE => 1,
    PLANT => 1,
    PRIMER => 1,
    PROMOTER => 1,
    PROTEIN => 1,
    PYRO => 1,
    REG => 1,
    RNA => 1,
    SEQ  => 1,
    SEQUENCE => 1,
    SMALL   => 1,
    SUBUNIT => 1,
    THE => 1,
    TYPE => 1,
    UTR => 1,
    VIRUS => 1
);

@forbidden_terms = qw(
contaminat
pseudogene
pseudo-gene
repeat
repetitive
transpos
);


######################################################################


sub default_db{
    return Rfam::DB::DB_RCS->new( '-current'   => $current_dir,
				  '-attic'     => $rcs_attic_dir, 
				  '-index'     => $rcs_index_file,
				  '-lock_file' => $lock_file,
				  '-rfamseq'   => $rfamseq);
}
			
#clans but same lock file
sub clan_db{
    return Rfam::Clans::Clan_RCS->new( '-clan_current'   => $clan_dir,   
				       '-clan_index'     => $clan_index_file,
				       '-lock_file' => $lock_file );  
}

#curation
sub live_rdb {
   my ($self) = @_;

   return Rfam::DB::DB_RDB->new('-db_name' => $live_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass,
				'-db_port' => $rdb_port );

}

#the only one on dev
sub release_rdb {
   my ($self) = @_;

   return Rfam::DB::DB_RDB->new('-db_name' => $release_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdbHostDev,
				'-db_user' => $rdbUserDev,
				'-db_password' => $rdbPassDev,
				'-db_port' => $rdbPortDev );

}

#curation
sub backup_rdb {
   my ($self) = @_;
   return Rfam::DB::DB_RDB->new('-db_name' => $backup_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass,
				'-db_port' => $rdb_port );

   }

#curation
sub test_rdb {
   my ($self) = @_;
   return Rfam::DB::DB_RDB->new('-db_name' => $test_rdb_name,
				'-db_driver' => $rdb_driver, 
				'-db_host' => $rdb_host,
				'-db_user' => $rdb_user,
				'-db_password' => $rdb_pass,
				'-db_port' => $rdb_port );

   }

#loading to rfamlive Curation -P3303

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
				    '-db_password' => $rdb_pass,
				   '-db_port' => $rdb_port );
    }    
    
}

sub live_rdb_update_admin{
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
				    '-db_user' => $rdb_user_admin,
				    '-db_password' => $rdb_pass_admin,
				   '-db_port' => $rdb_port );

   }    

}

#loading to release databases on Dev -P 3301
sub release_rdb_update{
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
	return Rfam::UpdateRDB->new('-db_name' => $release_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdbHostDev,
				    '-db_user' => $rdbUserDev,
				    '-db_password' => $rdbPassDev);
   }    

}

#test loadings on Curation -P 3303
sub test_rdb_update{
    my ($self, $user) = @_;
    
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
	
	return Rfam::UpdateRDB->new('-db_name' => $test_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdb_host,
				    '-db_user' => $rdb_user,
				    '-db_password' => $rdb_pass,
				   '-db_port' => $rdb_port );

    }
}

sub test_rdb_update_admin{
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
  	return Rfam::UpdateRDB->new('-db_name' => $test_rdb_name,
				    '-db_driver' => $rdb_driver, 
				    '-db_host' => $rdb_host,
				    '-db_user' => $rdb_user_admin,
				    '-db_password' => $rdb_pass_admin,
				   '-db_port' => $rdb_port );

   }    

}


sub external_rdb {
   my ($self) = @_;
   warn "Deprecated method. Call for external_rdb_name db: this database doesn't exist";
}
sub external_rdb_update{
    my ($self) = @_;
    warn "Deprecated method. Call for external_rdb_name db: this database doesn't exist";
}

sub switchover_rdb {
   my ($self) = @_;
   warn "Deprecated method. Call for switchover_rdb_name db: this database doesn't exist";
}
sub switchover_rdb_update{
   my ($self) = @_;
   warn "Deprecated method. Call for switchover_rdb_name db: this database doesn't exist";
}

sub temp_rdb{
    my ($self) = @_;
    warn "Deprecated method. Call for temp_rdb_name db: this database doesn't exist";
}
sub temp_rdb_update{
    my ($self) = @_;
    warn "Deprecated method. Call for temp_rdb_name db: this database doesn't exist";
}

1;
