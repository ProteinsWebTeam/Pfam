
#
# BioPerl module for Bio::Pfam::WebUserDBManager
#
# Author
package Bio::Pfam::WebUserDBManager;

use strict;
use warnings;

use WebUser;
use Data::Dumper;

sub new {
    my $caller = shift;
    my $class = ref($caller) || $caller;
    my %dbiParams = ();
    my $self = { user      => "web_user",
		 host      => "pfam",
		 port      => "3306",
		 database  => "web_user",
		 password  => "web_user",
		 driver    => "mysql",
		 @_,};

    print STDERR Dumper($self);

    $self->{'schema'} = WebUser->connect("dbi".
				       ":".$self->{driver}.
				       ":".$self->{database}.
				       ":".$self->{host}.
				       ":".$self->{port},
				       $self->{user},
				       $self->{password},
				       \%dbiParams);
    return bless($self, $caller);
} 


sub getSchema{
    my $self = shift;
    return $self->{schema};
}

#sub DESTROY {
#  my $self = shift;
#  $self->storage->disconnect;
#}
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut


1;



