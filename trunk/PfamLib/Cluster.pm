=head1 NAME

Cluster

=head1 DESCRIPTION

Single Linked cluster object

=head1 AUTHOR

B<Alex Bateman> Email agb@sanger.ac.uk

=cut

#
# Perl Module for Cluster
#
# Cared for by Alex Bateman <agb@sanger.ac.uk>
#

package Cluster;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' );

#######
# new #
#######

=head1 new

Create new Cluster

    $cluster = new Cluster();

=cut
sub new {
    my $class= shift @_;

    my $cluster = {
		   list => [],
		  };

    bless $cluster, $class;
    return $cluster;
}

#######
# add #
#######

=head1 add

Add list of ids to cluster

    $cluster->add($id1,...,$idn);

=cut
sub add {
  my $self= shift @_;
  push(@{$self->{'list'}},@_);
}

##########
# number #
##########

=head1 number

Will get or set identifying number to the cluster

    $cluster->number($n);

=cut
sub number {
  my $nargs=@_;

  my ($self,$n);
  if ($nargs==1){ # Getting value
    $self=shift @_;
    $n=$self->{'number'};
  } elsif ($nargs==2){ # Setting value
    $self=shift @_;
    $n=shift @_;
    $self->{'number'}=$n;
  }else {
    die "Incorect number of arguments for Cluster::number\n";
  }
  return $n;
}

########
# size #
########

=head1 size

Returns number of elements in a cluster

    $size=$cluster->size();

=cut

sub size {
  my ($self) = @_;
  my @list=$self->each();
  my $size=@list;
  return $size;
}

########
# each #
########
=head1 each

Loops over Cluster and returns array of Ids

@ids = $cluster->each();
=cut

sub each {
  my $self= shift @_;
  my @list=@{$self->{'list'}};
  return @list;
}



#########
# write #
#########
=head1 write

Debugging method: write out cluster

$cluster->write();

=cut
sub write {
  my $self = shift @_;

  my $i=$self->number();

  if (! $i){
    die "No cluster number in Cluster::write\n";
  }

  my $size=$self->size();
  print "  <cluster id=\"$i\" size=\"$size\">\n";
  foreach my $element ($self->each()){
      if( (ref $element) and $element->isa( 'Bio::LocatableSeq' ) ) {
	  print "    <match id=\"",$element->id," start: ",$element->start,", end: ",$element->end,"\"/>\n";
      }
      else {
	  print "    <match id=\"$element\"/>\n";
      }
  }
  print "  </cluster>\n";

}



__END__
