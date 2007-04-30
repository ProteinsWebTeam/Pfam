package Bio::Pfam::WebServices::PfamQueue::hmmer;

use strict;
use warnings;
use base qw(Bio::Pfam::WebServices::PfamQueue);

#our $VERSION = do { my @r = (q$Revision: 1.1 $ =~ /\d+/mxg); sprintf '%d.'.'%03d' x $#r, @r };

sub preinit    { 
  my ($self) = @_;
  $self->priority('hmmer');
  return;
}

1;