package Bio::Rfam::SeqDB;

use strict;
use warnings;
use Data::Printer;
use Moose;
use MooseX::NonMoose;
use Moose::Util::TypeConstraints;
use Bio::Rfam::Utils;

extends 'Bio::Easel::SqFile';

has 'dbname' => (
  is => 'ro',
  isa => enum([qw(rfamseq)]),
  required => 1,
  default => 'rfamseq',
);

1;