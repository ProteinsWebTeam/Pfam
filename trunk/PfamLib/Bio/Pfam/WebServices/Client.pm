
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

# $Author: jt6 $

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

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;

