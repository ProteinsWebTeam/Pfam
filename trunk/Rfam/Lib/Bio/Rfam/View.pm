package Bio::Rfam::View;

use Moose;
use Moose::Util::TypeConstraints;

use Carp;

use Bio::Rfam::Family;
use Bio::Rfam::MooseTypes;
# use Bio::Rfam::FamilyIO;

with 'MooseX::Role::Pluggable';

has config => (
  is  => 'ro',
  isa => 'Bio::Rfam::Config',
  required => 1
);

has job_uuid => (
  is  => 'ro',
  isa => 'Str',
  required => 1
);

has family => (
  is  => 'ro',
  isa => 'Bio::Rfam::Family',
  required => 1,
  coerce => 1
);

has job => (
  is  => 'rw',
  isa => 'Object'
);

# the enum here has to marry up with the enum in the database table
has seqdb => (
  is => 'ro',
  isa => enum( [ qw( rfamseq genome refseq ) ] ),
  required => 1
);

# sub example_method_on_view_object {
#   my $this = shift;
#   print 'Bio::Rfam::View::View::view: db: ' . $this->db . "\n";;
#   print 'Bio::Rfam::View::View::view: family: ' . $this->family . "\n";;
# }
#
# sub startJob {
#   my ($self) = @_;
#
#   if(!defined($self->job)){
#     my $jobRow = $self->config
#                        ->rfamlive
#                         ->resultset('PostProcess')
#                           ->search({ rfam_acc => $self->family->DESC->AC,
#                                      uuid     => $self->job_uuid })->first;
#    if(defined($jobRow)){
#      $self->job($jobRow);
#    }else{
#      croak("Falied to find job for ".$self->family->DESC->AC." and uuid ".$self->job_uuid);
#    }
#   }
#   $self->job->start;
# }

1;
