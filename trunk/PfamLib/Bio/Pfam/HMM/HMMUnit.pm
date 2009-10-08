
package Bio::Pfam::HMM::HMMUnit;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::HMM::HMMMatch';

subtype 'Domain'
    => as 'Int'
    => where { $_ > 0 };
              
#coerce 'Domain'
#  => from 'Str'
#    => via {
#      my $d;
#      if(/(\d+)\/\d+/){
#        $d = $1;
#      }
#      return $d;
#    };
#    
  
subtype 'proteinCoos'
  => as 'Int'
  => where { $_ > 0 && $_ < 100000 }
  => message { 'Protein coordinates are expected to be positive and less the 100,000'};


has 'seqEvalue' => (
  isa      => 'Num',
  is       => 'rw',
);

has 'domain' => (
  isa     => 'Domain',
  is       => 'rw'
);

has 'seqFrom' => (
  isa => 'proteinCoos',
  is   => 'rw',
  required => 1
);

has 'seqTo' => (
  isa => 'proteinCoos',
  is  => 'rw',
  required => 1
);

#has 'indEvalue' => (  
#  isa => 'evalue',
#  is  => 'rw',
#  required => 1,
#);

has 'domEvalue' => (
  isa => 'evalue',
  is  => 'rw',
);

has 'hmmalign' => (
  isa => 'HashRef',
  is  => 'rw',
  default => sub { {} },
);

has 'hmmFrom' => (
  isa => 'proteinCoos',
  is   => 'rw',
  required => 1
);

has 'hmmTo' => (
  isa => 'proteinCoos',
  is  => 'rw',
  required => 1
);

has 'envFrom' => (
  isa => 'proteinCoos',
  is   => 'rw'
);

has 'envTo' => (
  isa => 'proteinCoos',
  is  => 'rw'
);

has 'coreFrom' => (
  isa => 'Str',
  is   => 'rw'
);

has 'coreTo' => (
  isa => 'Str',
  is  => 'rw'
);

has 'aliAcc' => (
  isa => 'Num',
  is  => 'rw'
);

has 'sig' => (
  isa => 'Int',
  is  => 'rw'
);


__PACKAGE__->meta->make_immutable;
1;
