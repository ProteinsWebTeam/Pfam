#
# BioPerl module for Bio::Pfam::DB_RDB
#
# Written by Kevin Howe
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Rfam::DB_RDB - An interface to a pfam database implemented as a relational
database.

=head1 SYNOPSIS

This is a concrete implementation of the abstract database object
found in Bio::Pfam::DB

=head1 DESCRIPTION

An interface to a pfam database implemented as a relational database.
This is a project in develpment. 

At time of writing, the Pfam relational database onyl contains skeleton
information concerning Pfam entries. Therefore, the key methods 
get_EntryA_by_acc, get_EntryA_by_id, (and corresponding EntryB methods)
are not supported. Therefore, the main use of this database module is for
queries about the Pfam structures of sequences in pfamseq


=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. 
This module has a slightly unusual method-naming convention:

    Methods whivh change the underlying Pfam database in any
       way are preceded with a '_'.
    Internal methods, which are usually preceded with a '_',
        are preceded with a '__.'
    

=cut


# Let the code begin...


package Rfam::DB::DB_RDB;

use vars qw($AUTOLOAD @ISA);
use strict;
use DBI;
use FileHandle;

use Rfam::RfamRDB;


@ISA = qw(Rfam::DB::DB);

# _initialize is where the heavy stuff will happen when new is called

sub _initialize {
  my($self, %params) = @_;

  my $make = $self->SUPER::_initialize(%params);
 
  # ok - ready to rock

  $self->_the_RDB( Rfam::DB::RfamRDB->new( %params ));

  # set stuff in self from @args
  return $make; # success - we hope!
}









=head2 query

 Title   : query
 Usage   :
    $rdb->query("select * from * where * = *");
 Function:
    This function allows the caller to query the rdb directly
    with sql. It returns the query results as a
    a list of array references. This mechanism is used for
    generality. Many queries return just a single value. In
    this case, the method returns a single list reference,
    referring to a singleton list.
 Returns : 
    An array of list references
 Args    : A query string
 Notes   :
    This method is not intended to change the database
    in any way. Therefore, only select statementsd are 
    allowed.

=cut

sub query{
    my ($self, $query) = @_;
    my ($dbh, $st, @retlist);

 if ( ($query !~ /^select/i) && ($query !~ /^show/i) &&  ($query !~ /^describe/i) ) {

	$self->throw("DB_RDB::query - only 'select' queries are allowed");
	}

    eval {
	$dbh = $self->_the_RDB->connect();
	$st = $dbh->prepare($query);
	$st->execute();
	while( my @list = $st->fetchrow() ) {
	    # print STDERR "$query: $list[0]\n";
	    push @retlist, \@list;
	}
	$st->finish;
	
	$self->_the_RDB->disconnect;
    };   
    $@ and $self->throw("DB_RDB::query - error with query '$query' [$@]"); 

    return @retlist;
}






=head2 _the_RDB

 Title   : _the_RDB
 Usage   : $rdb = $self->_the_RDB();
 Function:
    Gets/sets the underlying database object for this query layer
 Returns : A object that provides basic connection facilities to the RDB
 Args    : A object that provides basic connection facilities to the RDB (optional)

=cut

sub _the_RDB {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_the_rdb'} = $value;
   }
   return $self->{'_the_rdb'};
}



1;
