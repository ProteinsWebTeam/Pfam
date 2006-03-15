
#
# Perl module for UpdateRDB
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

ReadRDB

=head1 DESCRIPTION

This module contains the basic functionality for downloading information
from the Pfam realtional database. It relies upon Bio::Pfam::PfamRDB 
for connection.


=cut


# Let the code begin...
    

package ReadRDB;

use vars qw($AUTOLOAD 
	    @ISA 
	    @EXPORT_OK ); 

use strict;
use Bio::Pfam::PfamRDB;

@ISA = qw(Bio::Pfam::PfamRDB Exporter);
@EXPORT_OK = qw();

sub _initialize {
    my ($self, %params) = @_;

    my $make = $self->SUPER::_initialize( %params );
    # insert fields here

    return $make;
}




=head2 dump_pfamA_to_file

 Title   : dump_pfamA_to_file
 Usage   :
 Function:
    This function takes a filehandle, and dumps the pfamA table to in,
    consisting of the following fields, tab-delimited
    1. Pfam-A accession
    2. Pfam-A identifier
    3. Pfam-A desc.
    4. Length of HMM
    5. No. seqs in seed
    6. No seqs in full
 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_pfamA_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and printf STDERR "Dumping %s.pfamA to file %s\n", $self->_database_name, $file;
   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'pfamA', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}



=head2 dump_pfamB_to_file

 Title   : dump_pfamB_to_file
 Usage   :
 Function:
    This function takes a filename, and dumps the pfamB table to the file.

 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_pfamB_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and printf STDERR "Dumping %s.pfamB to file %s\n", $self->_database_name, $file;
   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'pfamB', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}




=head2 dump_pfamA_reg_full_to_file

 Title   : dump_pfamA_reg_full_to_file
 Usage   :
 Function:
    This function takes a filename, and dumps the pfamA_reg_full table to the file.

 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_pfamA_reg_full_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and 
       printf STDERR "Dumping %s.pfamA_reg_full to file %s\n", $self->_database_name, $file;
   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'pfamA_reg_full', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}



=head2 dump_pfamA_reg_seed_to_file

 Title   : dump_pfamA_reg_seed_to_file
 Usage   :
 Function:
    This function takes a filename, and dumps the pfamA_reg_seed table to the file.

 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_pfamA_reg_seed_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and
       printf STDERR "Dumping %s.pfamA_reg_seed to file %s\n", $self->_database_name, $file;
   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'pfamA_reg_seed', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}



=head2 dump_pfamB_reg_to_file

 Title   : dump_pfamB_reg_to_file
 Usage   :
 Function:
    This function takes a filename, and dumps the pfamA_reg_seed table to the file.

 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_pfamB_reg_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and
       printf STDERR "Dumping %s.pfamB_reg to file %s\n", $self->_database_name, $file;

   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'pfamB_reg', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}



=head2 dump_pfamseq_to_file

 Title   : dump_pfamseq_to_file
 Usage   :
 Function:
    This function takes a filename, and dumps the pfamseq table to the file.

 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_pfamseq_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and 
       printf STDERR "Dumping %s.pfamseq to file %s\n", $self->_database_name, $file;

   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'pfamseq', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}



=head2 dump_other_reg_to_file

 Title   : dump_pfamseq_to_file
 Usage   :
 Function:
    This function takes a filename, and dumps the pfamseq table to the file.

 Returns :
    The number of rows dumped;
    Throws an exception if something went wrong
 Args    :
    1. A file name

 Notes   :

=cut

sub dump_other_reg_to_file {
   my ($self, $file) = @_;
   my ($rows,
       $dbh);

   $dbh = $self->connect();

   $self->report_mode and 
       printf STDERR "Dumping %s.other_reg to file %s\n", $self->_database_name, $file;

   eval {
       $rows = $dbh->do( $self->__dump_to_file_sql( 'other_reg', $file ));
   };

   $self->close_transaction( $@ );
   $@ and $self->throw( $@ );

   return $rows;
}



1;
