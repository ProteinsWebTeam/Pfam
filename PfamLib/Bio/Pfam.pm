
#
# BioPerl module for Bio::Pfam
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
# Copyright Ewan Birney
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam - Object factory that provides Pfam based objects

=head1 SYNOPSIS

    $db = Bio::Pfam::default_db(); # gets a Bio::Pfam::DB object from
                                   # defualts

=head1 DESCRIPTION

This is really a package, not an object, providing the defaults for a system
for the Pfam databases. By using only the functionality in this and the
Bio::Pfam::DB module you should be insulated from the changes of the database
layout.

This module allows you to instantiate database objects. It is here where the
defaults of the database system are held. You should have a look at 
Bio::Pfam::DB for what methods you are allowed to use.

=head1 CONTACT

Describe contact details here

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal 
methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam;
use vars qw($AUTOLOAD @ISA);
use strict;
no strict 'vars';          # mmmm - this could be tightened up
                           # specifically export what we want to

use Bio::Pfam::DB;
use Bio::Pfam::DB_RCS;
use Bio::Pfam::DB_Web;
use Bio::Pfam::DB_RDB;
use Bio::Pfam::UpdateRDB;


# set process umask so we creat files that group pfam can kill
# (avoids permission denied problems)
umask 002;

$pfam_root_dir  = $ENV{'PFAM_ROOT_DIR'};
if( ! $pfam_root_dir ) {
    $pfam_root_dir  = '/pfam/db/Pfam';
}

# now rcs specific variables

my $name_of_current_dir = "CURRENT";
my $name_of_master_dir = "RCS_MASTER";
my $name_of_attic_dir = "RCS_ATTIC";
my $name_of_accession_dir = "ACCESSION";
my $name_of_accession_attic_dir = "$name_of_accession_dir/ATTIC";
my $name_of_acc_map_file = "accmap.dat";
my $name_of_acc_map_lock = "accmap.lock";

my $name_of_releases_dir = "RELEASES";
my $name_of_scripts_dir  = "scripts";

# globals, for convenient reading from other modules

$srs_pfamseq = 'pfamseq';
$srs_pfamseq_label = 'PFAMSEQ';
$srs_pfamseqnew_label = 'PFAMSEQNEW';
$srs_pfam = 'pfamnew';
$srs_pfamb = 'pfambnew';
$srs_swisspfam = 'swisspfamnew';

$dc_pfamseq     = 'pfamseq';
$dc_pfamseq_new = 'pfamseq_new';

$rcs_current_dir = "$pfam_root_dir/$name_of_current_dir";
$rcs_master_dir  = "$pfam_root_dir/$name_of_master_dir";
$rcs_attic_dir   = "$pfam_root_dir/$name_of_attic_dir";
$rcs_accession_dir       = "$pfam_root_dir/$name_of_accession_dir";
$rcs_accession_attic_dir = "$pfam_root_dir/$name_of_accession_attic_dir";

$releases_dir = "$pfam_root_dir/$name_of_releases_dir";
$scripts_dir  = "$pfam_root_dir/$name_of_scripts_dir";
$datadir= "/pfam/data1/localdbs";
$pfam_bin_dir = "$pfam_root_dir/bin";

$pfamseq_dir         = "/pfam/db/pfamseq";
$pfamseq_fasta_file  = "$pfamseq_dir/pfamseq";
$pfamseq_inx_file    = "$pfamseq_dir/pfamseq.fa.bpi";
$pfamseq_datinx_file = "$pfamseq_dir/pfamseq.dat.bpi";

$pfamseqnew_dir            = "/pfam/db/pfamseqnew";
$pfamseqnew_fasta_file     = "$pfamseqnew_dir/pfamseq";
$pfamseqnew_inx_file       = "$pfamseqnew_dir/pfamseq.fa.bpi";
$pfamseqnew_datinx_file    = "$pfamseqnew_dir/pfamseq.dat.bpi";
$pfamseqnew_new_fasta_file = "$pfamseqnew_dir/pfamseq_new";
$pfamseqnew_new_inx_file   = "$pfamseqnew_dir/pfamseq_new.fa.bpi";

$pfamseqdev_dir        = "/pfam/db/pfamseqdev";                # shouldn't be used anymore
$pfamseqdev_fasta_file = "$pfamseqdev_dir/pfamseqdev";         #
$pfamseqdev_inx_file   = "$pfamseqdev_dir/pfamseqdev.fa.bpi";  #

$fasta_db        = "$pfamseqnew_dir/pfamseq"; # redundant but don't know where its used
$pfam_temp_dir   = "/pfam/db/tmp/";
$dc_template_dir = "$pfam_root_dir/decypher/templates";
$sequence_update_rdb_temp = "/pfam/data2/db_temp";

# rcs file lists - not used here but in PfamRCS and PfamQC

# the order of the files in each set is important as checkouts occur
# in order, and checkins check all the timestamps!

@annotation_file_set = ("DESC");
@family_file_set     = ("SEED","HMM_ls","HMM_fs","ALIGN","scores");
@view_file_set       = ("SEED.ann","HMM_ls.ann","HMM_fs.ann","ALIGN.ann", "SEED.tree", "ALIGN.tree");
@optional_file_set   = ("trues","notes","BAIT","PRIOR");
@output_file_set     = ("PFAMOUT_ls", "PFAMOUT_fs");

# now web-specific variables

$web_data_root = '/nfs/WWW/htdocs/Software/Pfam/data/';

#now RDB specific variables

$rdb_host = "pfam";
$rdb_driver = "mysql";
$rdb_user = "pfam";
$rdb_pass = "password";


$rdb_select_host = "172.18.19.1";
$rdb_select_user = "pfamro";

$live_rdb_name = "pfamlive";
$mirror_rdb_name = "pfam";
$mirror_new_rdb_name = "pfam2";
$development_rdb_name = "pfamdev";


=head2 default_db

 Title   : default_db
 Usage   : $db = &Bio::Pfam->default_db();
 Function: returns the default Pfam::DB object for this system
           This could be an RCS db or a flat-file based db -
           you shouldnt care
 Example :
 Returns : new Bio::Pfam::DB object (probably something that inheriets it in fact).
 Args    :


=cut

sub default_db{
    return Bio::Pfam::DB_RCS->new('-current' => $rcs_current_dir, 
				  '-attic' => $rcs_attic_dir,
				  '-index' => "$rcs_accession_dir/$name_of_acc_map_file", 
				  '-lock_file' => "$rcs_accession_dir/$name_of_acc_map_lock", 
				  '-fastadb' => $fasta_db, 
				  '-srsdb' => $srs_pfamseq,
				  '-srspfamb' => $srs_pfamb, 
				  '-srsswisspfam' => $srs_swisspfam 
				  );
} 



=head2 local_rcs_db

 Title   : local_rcs_db
 Usage   : $db = &Bio::Pfam->local_rcs_db( $database_root_directory )
 Function: returns the default Pfam::DB object for a database sitting
    under the specified directory. This database should have the same
    structure as the core RCS database. Certain database operations
    will be missing due to lack of information (for example an underlying
    srs pfamseq database)						
 Example :
 Returns : new Bio::Pfam::DB object (probably something that inheriets it in fact).
 Args    :


=cut

sub local_rcs_db{
    my $self = shift;
    my $local_root = shift;

    return Bio::Pfam::DB_RCS->new('-current' => "$local_root/$name_of_current_dir", 
				  '-attic' => "$local_root/$name_of_attic_dir",
				  '-index' => "$local_root/$name_of_accession_dir/$name_of_acc_map_file", 
				  '-lock_file' => "$local_root/$name_of_accession_dir/$name_of_acc_map_lock", 
				  ); 
} 


=head2 checkin_db

 Title   : checkin_db
 Usage   : $db = &Bio::Pfam->checkin_db( $checkin_root )
 Function: returns the default Pfam::DB object for a database (probably
    only a single family directory sitting under the specified directory. 
    Certain database operations may be missing due to lack of information.						
 Example :
 Returns : new Bio::Pfam::DB object (probably something that inherits it in fact).
 Args    :


=cut

sub checkin_db{
    my $self = shift;
    my $local_root = shift;

    return Bio::Pfam::DB_RCS->new('-current' => $local_root, 
				  '-attic' => $rcs_attic_dir,
				  '-index' => "$rcs_accession_dir/$name_of_acc_map_file", 
				  '-lock_file' => "$rcs_accession_dir/$name_of_acc_map_lock", 
				  '-fastadb' => $fasta_db, 
				  '-srsdb' => $srs_pfamseq,
				  '-srspfamb' => $srs_pfamb, 
				  '-srsswisspfam' => $srs_swisspfam 
				  );
} 





=head2 latest_release

 Title   : latest_release
 Usage   : $db = &Bio::Pfam::latest_release();
 Function: returns the latest fixed release of Pfam
           as Pfam::DB object
 Returns : a new Bio::Pfam::DB object (probably something that inheriets it in fact)
 Args    :

=cut

sub latest_release{
   my ($self) = @_;

    return Bio::Pfam::DB_Web->new('-datadir' => $web_data_root,
				  '-srsdb' => $srs_pfamseq, 
				  '-srspfam' => $srs_pfam,
				  '-srspfamb' => $srs_pfamb,
				  '-srsswisspfam' => $srs_swisspfam,
				  '-db_name' => $mirror_rdb_name,
				  '-db_host' => $rdb_host,
				  '-db_driver' => $rdb_driver,
				  '-db_user' => $rdb_user,
				  '-db_password' => $rdb_pass);
}




=head2 live_rdb

 Title   : live_rdb
 Usage   : $db = &Bio::Pfam->live_rdb();
 Function: Returns a PfamRDB object which contains methods for update and 
    query if the live Pfam relational database. This is NOT an implementation
    of DB.pm, rather an interface for writing to the RDB.
 Returns : a new Bio::Pfam::PfamRDB object
 Args    :

=cut

sub live_rdb {
   my ($self) = @_;

   return Bio::Pfam::DB_RDB->new('-db_name' => $live_rdb_name,
				 '-db_driver' => $rdb_driver, 
				 '-db_host' => $rdb_select_host,
				 '-db_user' => $rdb_select_user);

}



=head2 release_rdb

 Title   : release_rdb
 Usage   : $db = &Bio::Pfam->release_rdb();
 Function: Returns a PfamRDB object which contains methods for update and 
    query if the live Pfam relational database. This is NOT an implementation
    of DB.pm, rather an interface for writing to the RDB.
 Returns : a new Bio::Pfam::PfamRDB object
 Args    :

=cut

sub release_rdb {
   my ($self) = @_;

   return Bio::Pfam::DB_RDB->new('-db_name' => $mirror_rdb_name,
				 '-db_driver' => $rdb_driver, 
				 '-db_host' => $rdb_select_host,
				 '-db_user' => $rdb_select_user);

}


=head2 new_release_rdb

 Title   : new_release_rdb
 Usage   : $db = &Bio::Pfam->new_release_rdb();
 Function: Returns a PfamRDB object which contains methods for update and 
    query if the mirror ('new') RDB used to store the new data before it
    goes live
 Args    :

=cut

sub new_release_rdb {
   my ($self) = @_;

   return Bio::Pfam::DB_RDB->new('-db_name' => $mirror_new_rdb_name,
				 '-db_driver' => $rdb_driver, 
				 '-db_host' => $rdb_select_host,
				 '-db_user' => $rdb_select_user);

}


=head2 development_rdb

 Title   : development_rdb
 Usage   : $db = &Bio::Pfam->development_rdb();
 Function: Returns a PfamRDB object which contains methods for update and 
    query if the mirror ('new') RDB used to store the new data before it
    goes live
 Args    :

=cut

sub development_rdb {
   my ($self) = @_;

   return Bio::Pfam::DB_RDB->new('-db_name' => $development_rdb_name,
				 '-db_driver' => $rdb_driver, 
				 '-db_host' => $rdb_select_host,
				 '-db_user' => $rdb_select_user);

}


sub which_select_rdb {
  my( $rdb) = @_;

  my $return_rdb;

  if ($rdb eq "pfam") {

    $return_rdb = release_rdb();

  } elsif ($rdb eq "pfam2") {

    $return_rdb = new_release_rdb();

  } elsif ($rdb eq "pfamdev") {

    $return_rdb = development_rdb();
    
  } elsif ($rdb eq "pfamlive") {

    $return_rdb = live_rdb();

}else{

    $return_rdb = Bio::Pfam::UpdateRDB->new('-db_name' => $rdb,
			      '-db_driver' => $rdb_driver, 
			      '-db_host' => $rdb_host,
			      '-db_user' => $rdb_user,
			      '-db_password' => $rdb_pass);
}

  return $return_rdb;

}



=head2 live_rdb_update

 Title   : live_rdb_update
 Usage   :
 Function:
 Returns : 
 Args    :

=cut

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
	return Bio::Pfam::UpdateRDB->new('-db_name' => $live_rdb_name,
			      '-db_driver' => $rdb_driver, 
			      '-db_host' => $rdb_host,
			      '-db_user' => $rdb_user,
			      '-db_password' => $rdb_pass);
    }    

}



=head2 release_rdb_update

 Title   : release_rdb_update
 Usage   :
 Function:
 Returns : 
 Args    :

=cut

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
	return Bio::Pfam::UpdateRDB->new('-db_name' => $mirror_rdb_name,
					 '-db_driver' => $rdb_driver, 
					 '-db_host' => $rdb_host,
					 '-db_user' => $rdb_user,
					 '-db_password' => $rdb_pass);
    }    

}




=head2 switchover_rdb_update

 Title   : switcover_rdb_update
 Usage   :
 Function:
 Returns : 
 Args    :

=cut

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
	return Bio::Pfam::UpdateRDB->new('-db_name' => $mirror_new_rdb_name,
					 '-db_driver' => $rdb_driver, 
					 '-db_host' => $rdb_host,
					 '-db_user' => $rdb_user,
					 '-db_password' => $rdb_pass);
    }    

}



=head2 development_rdb_update

 Title   : development_rdb_update
 Usage   :
 Function:
 Returns : 
 Args    :

=cut

sub development_rdb_update{
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
	return Bio::Pfam::UpdateRDB->new('-db_name' => $development_rdb_name,
			      '-db_driver' => $rdb_driver, 
			      '-db_host' => $rdb_host,
			      '-db_user' => $rdb_user,
			      '-db_password' => $rdb_pass);
    }    

}

sub which_update_rdb {
  my( $rdb) = @_;

  my $return_rdb;

  if ($rdb eq "pfam") {

    $return_rdb = release_rdb_update();

  } elsif ($rdb eq "pfam2") {

    $return_rdb = switchover_rdb_update();

  } elsif ($rdb eq "pfamdev") {

    $return_rdb = development_rdb_update();
    
  } elsif ($rdb eq "pfamlive") {

    $return_rdb = live_rdb_update();

  } 
  else{

    $return_rdb = Bio::Pfam::UpdateRDB->new('-db_name' => $rdb,
			      '-db_driver' => $rdb_driver, 
			      '-db_host' => $rdb_host,
			      '-db_user' => $rdb_user,
			      '-db_password' => $rdb_pass);
  }

  return $return_rdb;

}


1;

