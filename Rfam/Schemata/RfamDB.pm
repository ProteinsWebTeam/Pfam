use utf8;
package RfamDB;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::Schema';

__PACKAGE__->load_namespaces;


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Vx0ieMkE0tY9KVpwfNpWpw

sub prepare_seqaccToTaxon {
  my ($self) = @_;
  
  my $dbh = $self->storage->dbh;
  my $sth = $dbh->prepare("SELECT ncbi.ncbi_id, ncbi.align_display_name 
                            FROM rfamseq me 
                            JOIN taxonomy ncbi 
                            ON ncbi.ncbi_id = me.ncbi_id 
                            WHERE ( me.rfamseq_acc = ? )");

  return( $sth );
}


sub prepare_updateTaxonomy {
  my ($self) = @_;
  
  my $dbh = $self->storage->dbh;
  my $sth = $dbh->prepare("UPDATE taxonomy SET tree_display_name = ?, 
                                               align_display_name = ?
                                           WHERE  ncbi_id = ? ");
                                           
  return( $sth );
}

#------------------------------------------------------------------------------
=head2 prepare_fullRegionAndTaxonBySeqAcc

  Title    : prepare_fullRegionAndTaxonBySeqAcc
  Incept   : finnr, Feb 6, 2013 3:01:07 PM
  Usage    : $rfamdb->prepare_fullRegionAndTaxonBySeqAcc( $isSeed )
  Function : Returns a DBI statement handle for executing queries. This statement
           : has two bind values, rfamseq_acc and rfam_acc.
  Args     : 0 or 1 if you want the query to restrict to rows defined by SEED.
  Returns  : DBI statement handle
  
=cut

sub prepare_fullRegionAndTaxonBySeqAcc {
  my ($self, $seed) = @_;
  
  #Build up the select part of the query
  my $sqlStm = "SELECT r.seq_start, r.seq_end, r.bit_score, n.ncbi_id, n.tree_display_name ";
  
  #Now the from, join and conditions.
  $sqlStm .= "  FROM full_region r 
                JOIN rfamseq s ON r.rfamseq_acc=s.rfamseq_acc
                JOIN taxonomy n ON n.ncbi_id = s.ncbi_id 
                WHERE r.rfamseq_acc = ? 
                AND   r.rfam_acc = ? ";
                
  #Optional condition
  if($seed){
    $sqlStm .= "AND r.type = 'seed'";
  }
  
  my $dbh = $self->storage->dbh;
  my $sth = $dbh->prepare($sqlStm);
  return( $sth );
}

=head2 prepare_seqaccToDescription

  Title    : prepare_seqaccToDescription
  Incept   : EPN, Mon Feb 25 10:09:11 2013
  Usage    : $rfamdb->prepare_seqaccToDescription( )
  Function : Returns a DBI statement handle for executing queries. This statement
           : has one bind value: rfamseq_acc.
  Args     : none
  Returns  : DBI statement handle
  
=cut

sub prepare_seqaccToDescription {
  my ($self) = @_;
  
  my $dbh = $self->storage->dbh;
  my $sth = $dbh->prepare("SELECT description
                           FROM rfamseq me
                           WHERE ( me.rfamseq_acc = ? )");

  return( $sth );
}

=head2 prepare_seqaccToSpeciesTaxStringAndID

  Title    : prepare_seqaccToSpeciesTaxStringAndID
  Incept   : EPN, Mon Feb 25 10:27:25 2013
  Usage    : $rfamdb->prepare_seqaccToSpeciesTaxStringAndID( )
  Function : Returns a DBI statement handle for executing queries. This statement
           : has one bind value: rfamseq_acc.
  Args     : none
  Returns  : DBI statement handle
  
=cut

sub prepare_seqaccToSpeciesTaxStringAndID {
  my ($self) = @_;
  
  my $dbh = $self->storage->dbh;
  my $sth = $dbh->prepare("SELECT t.species, t.align_display_name, t.tax_string, t.ncbi_id
                            FROM rfamseq r
                            JOIN taxonomy t
                            ON t.ncbi_id = r.ncbi_id 
                            WHERE ( r.rfamseq_acc = ? )");

  return( $sth );
}

1;
