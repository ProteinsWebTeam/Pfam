package RfamDB::ResultSet::Rfamseq;

use strict;
use warnings;

use base 'DBIx::Class::ResultSet';

sub seqaccToTaxon {
  my ($self, $acc) = @_;
  
  my $row = $self->find({ rfamseq_acc => $acc},
                        { join => 'ncbi',
                          '+select' => ['ncbi.ncbi_id', 'ncbi.species'],
                          '+as'     => ['taxid', 'species'] });
                          
  return (defined($row) ? $row : undef);
}


1;
