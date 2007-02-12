
#
# BioPerl module for Bio::Pfam::PfamLiveDBManager
#

package Bio::Pfam::PfamLiveDBManager;

use base Bio::Pfam::PfamDBManager;
use PfamLive;
use Data::Dumper;
use Carp qw(cluck croak carp);

sub new {
 print "In PfamLive new\n\n";
    my $caller = shift;
    my $class = ref($caller) || $caller;
    my %dbiParams = ();
    my $self = { user      => "pfamro",
		 host      => "pfam",
		 port      => "3306",
		 database  => "pfamlive",
		 driver    => "mysql",
		 @_,};

    carp("The new object contains :\n".Dumper($self)) if($self->{'debug'});
    eval{
     $self->{'schema'} = PfamLive->connect("dbi".
				       ":".$self->{driver}.
				       ":".$self->{database}.
				       ":".$self->{host}.
				       ":".$self->{port},
				       $self->{user},
				       $self->{password},
				       \%dbiParams);
    };
    if($@){
      croak("Failed to get schema for databse:".$self->database.". Error:[$@]\n");     
    }
    return bless($self, $caller);
} 

#
# Select methods specific to Pfamlive should go in here.
#






#
# Specific insert/update methods should go here
#


1;


