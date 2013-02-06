use utf8;
package RfamLive;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Schema';

__PACKAGE__->load_namespaces;


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Vx0ieMkE0tY9KVpwfNpWpw

sub prepare_seqaccToTaxon {
  my ($self) = @_;
  
  my $dbh = $self->storage->dbh;
  my $sth = $dbh->prepare("SELECT ncbi.ncbi_id, ncbi.species 
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



# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
