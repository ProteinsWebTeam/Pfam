=head1 NAME

ClusterSet

=head1 DESCRIPTION

The ClusterSet object contains a list of single linked clusters.
This object also contains the crucial method single_link, that
does the clustering given pairs of linked data.

=head1 AUTHOR

B<Alex Bateman> Email agb@sanger.ac.uk

=cut

#
# Perl Module for ClusterSet
#
# Cared for by Alex Bateman <agb@sanger.ac.uk>
#

package ClusterSet;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use Cluster;

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

Create new ClusterSet

    $set = new ClusterSet();

=cut
sub new {
    my ($class)=@_;
    my $set = {
	       "hash" => {}, # This hash keeps track of which cluster each object
                             # is in. Might be necessary to allow objects to belong
                             # to multiple clusters!
	      };

    bless $set, $class;   # Tag object with package name
    return $set;          # Return object
}

#######
# add #
#######

=head1 add

Add one cluster to set

    $set->add($cluster);

=cut
sub add {
  my ($self,$object) = @_;

  # Loop over id list
  foreach my $id ($object->each()){
    # This step will delete old id=>cluster pairs
    $self->{'hash'}->{$id} = $object;
  }
}

###############
# single_link #
###############

=head1 single_link

Takes linked data and adds to Clusters

    $set->single_link($data1,$data2);

=cut
sub single_link {
  my ($set,$data1,$data2) = @_;
  my $debug;

   if ($debug){
      print STDERR "$data1 in cluster adding $data2\n";
    }

  # check to see if we have strand associated info
  my( $revcomp, $obj_bioseq );
  if( ref($data1) and ref($data2) and       # check that these are references
      $data1->isa( 'Bio::LocatableSeq' ) and $data2->isa( 'Bio::LocatableSeq' ) ) { # and if they're seq objects
      $obj_bioseq = 1;
      if( $data1->strand() and $data2->strand() and $data1->strand() != $data2->strand() ) {
	  $revcomp = 1;
#	  print STDERR $data1->id, " ", $data2->id, " reverse complement\n";
      }
  }

  # Check they are different
  if ($data1 eq $data2){return;}

  # If data1 is in cluster and data2 is not add data2 to cluster
  elsif($set->{'hash'}->{$data1} and not $set->{'hash'}->{$data2}){
    if ($debug){
      print STDERR "$data1 in cluster adding $data2\n";
    }

    if( $obj_bioseq ) {
	if( $data1->start > $data1->end ) {
	    if( $revcomp ) {
		# do nothing
	    }
	    else {
		# flip data2
		my( $st, $en ) = ( $data2->start, $data2->end );
		$data2->end( $st );
		$data2->start( $en );
	    }
	}
	else {
	    if( $revcomp ) {
		# flip data2
		my( $st, $en ) = ( $data2->start, $data2->end );
		$data2->end( $st );
		$data2->start( $en );
	    }
	    else {
		# do nothing
	    }
	}
    }

    my $cluster = $set->{'hash'}->{$data1};
    $cluster->add($data2);
    $set->{'hash'}->{$data2} = $cluster;
  }

  # If id2 is in cluster and id1 is not add id1 to cluster
  elsif ($set->{'hash'}->{$data2} and not $set->{'hash'}->{$data1}){
    if ($debug){
      print STDERR "$data2 in cluster adding $data1\n";
    }

    if( $obj_bioseq ) {
	if( $data2->start > $data2->end ) {
	    if( $revcomp ) {
		# do nothing
	    }
	    else {
		# flip data1
		my( $st, $en ) = ( $data1->start, $data1->end );
		$data1->end( $st );
		$data1->start( $en );
	    }
	}
	else {
	    if( $revcomp ) {
		# flip data1
		my( $st, $en ) = ( $data1->start, $data1->end );
		$data1->end( $st );
		$data1->start( $en );
	    }
	    else {
		# do nothing
	    }
	}
    }

    my $cluster = $set->{'hash'}->{$data2};
    $cluster->add($data1);
    $set->{'hash'}->{$data1} = $cluster;
  }

  # If id1 and id2 are in clusters 
  elsif ($set->{'hash'}->{$data2} and $set->{'hash'}->{$data1}){
    # data and data2 in same  cluster
    if ($set->{'hash'}->{$data2} eq $set->{'hash'}->{$data1}){
      return; # Skip
    } elsif ($set->{'hash'}->{$data1} ne $set->{'hash'}->{$data2}){
      if ($debug){
	print STDERR "$data1 and $data2 are in different clusters. Merging\n";
      }
      # data1 is in different cluster to data2. Merge clusters
      $set->merge($set->{'hash'}->{$data1},$set->{'hash'}->{$data2},$revcomp);
    }

  } else {
    # Else make a new cluster
    if ($debug){
      print STDERR "Adding $data1 and $data2 to new cluster\n";
    }

    if( $revcomp ) {
	# flip data2;
	my( $st, $en ) = ( $data2->start, $data2->end );
	$data2->end( $st );
	$data2->start( $en );
    }

    my $new_cluster = new Cluster();
    $new_cluster->add($data1);
    $new_cluster->add($data2);
    $set->add($new_cluster);
  }

  if ($debug){      
    $set->write();
    print "**********************\n";
  }
}

#########
# merge #
#########

=head1 merge

Merge two clusters

    $set->merge($cluster1,$cluster2);

=cut
sub merge {
  my ($self,$object1,$object2,$revcomp) = @_;

  if (! $object1 or ! $object2){
    die "In method ClusterSet::merge need to clusters!\n";
  }

  foreach my $data2 ($object2->each()){
      if( $revcomp ) {
	  # flip data2;
	  my( $st, $en ) = ( $data2->start, $data2->end );
	  $data2->end( $st );
	  $data2->start( $en );
      }
    $object1->add($data2);
    $self->{'hash'}->{$data2}=$object1; # Move over to new cluster
  }

  # Should probably delete objects from second cluster
}


########
# each #
########
=head1 each

Loops over set and returns list of clusters

@clusters = $set->each();
=cut
sub each {
  my($self)=@_;
  my @list;

  my %hash=%{$self->{'hash'}};
  my %unique_hash;
  foreach my $key (keys %hash){
    if ($unique_hash{$hash{$key}}){
      next;
    } else {
      $unique_hash{$hash{$key}}=1;
      push @list, $hash{$key};
    }
  }

  return @list;
}

################
# each_by_size #
################
=head1 each_by_size

Loops over ClusterSet and returns array of Ids sorted from smallest to largest

@ids = $cluster->each_by_size();
=cut

sub each_by_size {
  my $self= shift @_;

  my @list=$self->each();

  my @datakeys;
  foreach my $element (@list){
    push(@datakeys, $element->size());
  }
  #sub bydatakeys {$datakeys[$a] <=> $datakeys[$b];}
  my @sortdata = @list[sort {$datakeys[$a] <=> $datakeys[$b]} $[..$#list];
  @sortdata=reverse @sortdata;
  return @sortdata;
}


#########
# write #
#########

=head1 write

Debugging method: write out clusters, will order clusters by size
Will number clusters at this point if they do not already have a number!

$cluster->write();

=cut
sub write {
  my $self = shift @_;

  #  print STDERR "writing ClusterSet $self\n";
  my $i=1;

  # Would be good to Calculate size of all clusters first. Then print out in size order
  # Make list of elements ordered by size
  # from Perl 4 Camel book page 249.
  my @data=$self->each();
  my @datakeys;
  foreach my $point (@data){
    push (@datakeys,$point->size());
  }
  my @sortdata=@data[sort {$datakeys[$a] <=> $datakeys[$b]} $[..$#data];


  print "<clusterset>\n";
  foreach my $element (reverse @sortdata){
    if (! $element->number()){
      $element->number($i);
      $i++;
    }

    $element->write();
  }
  print "</clusterset>\n";
}

#########
# read #
#########

=head1 read

Method to read clusterset from file into data structure

  $set = new ClusterSet();
  $set->read(\*FILE);

=cut
sub read {
  my $self = shift @_;
  my $fh   = shift @_;

  if (! $fh){
    die 'In ClusterSet::read no file handle given\ne.g. $set->read(\*FILE);\n';
  }

  my $cluster;
  while(<$fh>){
    if (/<cluster id=\"(\S+)\"/){
      my $id=$1;
      $cluster=new Cluster();
      $cluster->number($id);
    } elsif (/<match id=\"(.*)\"\/>/){
      $cluster->add("$1");
    } elsif (/<\/cluster>/){
      $self->add($cluster);
    }
  }
  close ($fh);
}




__END__

