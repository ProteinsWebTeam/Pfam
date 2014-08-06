package RfamDB::ResultSet::PostProcess;

use strict;
use warnings;
use Carp;
use Data::UUID;

use base 'DBIx::Class::ResultSet';

sub initiatePostProcessJob {
  my ($self, $familyObj, $author) = @_;
  
  my $rfam_acc = $familyObj->DESC->AC;
  #First step is to find all existing jobs for the family and kill them off.
  #Do these first, so we do not get subsequent things firing off.
  my @pending = $self->search({ rfam_acc => $rfam_acc,
                                status   => 'PEND' });
  foreach my $p (@pending){
    $p->status('KILL');
    $p->update;
  }
  
  my @running = $self->search({ rfam_acc => $rfam_acc,
                                status   => 'RUN' });
  foreach my $p (@running){
    $p->status('KILL');
    #Todo - kill the job on the farm.
    $p->update;
  }
  
  my $uuid = Data::UUID->new()->create_str();
  $self->create(
    {
      rfam_acc => $rfam_acc,
      status    => 'PEND',
      author    => $author,
      created   => \'NOW()',
      uuid      => $uuid
    },
  );
}
1;