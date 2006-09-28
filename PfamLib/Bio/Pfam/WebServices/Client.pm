
=head1 SYNOPSIS

    use Bio::Pfam::Webservices::Client;

    $scop = new Bio::Pfam::Webservices:Client;


=head1 DESCRIPTION

Some description goes in here.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...

package Bio::Pfam::WebServices::Client;
use vars qw($AUTOLOAD @ISA);

use strict;
use warnings;
use Bio::Pfam::Root;
@ISA = qw(Bio::Pfam::Root);


sub proxy {
  my ($self,$proxy) = @_;

  # set a proxy, if defined
  $self->{proxy} = $proxy if defined $proxy;

  return $self->{proxy};
}

sub _response {
  my ($self, $responseDom) = @_;
  # Hand back the LibXML dom.
  if($responseDom){
    $self->{'_response'} = $responseDom;
  }
  return $self->{'_response'} unless(!$self->{'_response'});
}

1;
