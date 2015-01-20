package Bio::Rfam::View::Plugin::Counts;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

sub process {
  my $this = shift;
  print "In Bio::Rfam::View::Plugin::Counts::process\n";
  print 'Putting counts in for ' . $this->parent->family->DESC->AC . "\n";
  $this->setSpeciesCount;
}

sub setSepeciesCount {
  my ( $this ) = @_;
  
  
  my $rfamdb = $self->parent->config;
  my $dbh  = $rfamdb->storage->dbh;

  my $family_acc = $this->parent->family->DESC->AC;

  my $countSpeciesSth = $dbh->prepare("update family a set a.number_of_species = (select count(distinct ncbi_id) from full_region f, rfamseq r where r.rfamseq_acc=f.rfamseq_acc and is_significant=1 and a.rfam_acc=f.rfam_acc) where rfam_acc=?" );
  $countSepciesSth->execute($family_acc);
  $countSpeciesSth->finish;
}

1;
