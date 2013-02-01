package Bio::Rfam::View;

use strict;
use warnings;
use Carp;
use Moose;
use Bio::Rfam::Family;
use Bio::Rfam::FamilyIO;
use Moose::Util::TypeConstraints;

with 'MooseX::Role::Pluggable';

coerce 'Bio::Rfam::Family',
  from 'Str',
    via {
      if($_ !~ /^RF\d+$/){
        return;
      }
      my $io = Bio::Rfam::FamilyIO->new;
      my $family = $io->loadRfamFromRDB($_);
      return $family;
    };

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
  is  => 'rw',
  isa => 'Bio::Rfam::Family',
  required => 1,
  coerce => 1
);


has job => (
  is  => 'rw',
  isa => 'Object'
);

sub example_method_on_view_object {
  my $this = shift;
  print 'Bio::Rfam::View::View::view: db: ' . $this->db . "\n";;
  print 'Bio::Rfam::View::View::view: family: ' . $this->family . "\n";;
}

sub startJob {
  my ($self) = @_;
  
  if(!defined($self->job)){
    my $jobRow = $self->config
                       ->rfamlive
                        ->resultset('PostProcess')
                          ->search({ rfam_acc => $self->family->DESC->AC,
                                     uuid     => $self->job_uuid })->first;
   if(defined($jobRow)){
     $self->job($jobRow);
   }else{
     croak("Falied to find job for ".$self->family->DESC->AC." and uuid ".$self->job_uuid);
   }
  }
  $self->job->start;
}

1;
