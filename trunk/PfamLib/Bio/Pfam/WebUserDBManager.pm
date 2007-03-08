
#
# BioPerl module for Bio::Pfam::WebUserDBManager
#
# $Author

package Bio::Pfam::WebUserDBManager;

use strict;
use warnings;

use base WebUser;
use Data::Dumper;

$SIG{INT} = sub {
    __PACKAGE__->storage->disconnect;
};

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

    $self->{'schema'} = __PACKAGE__->connect("dbi".
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

sub DESTROY {
  my $self = shift;
  $self->storage->disconnect;
}
1;


