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

use vars qw(@ISA);
use strict;
use DBI;
use FileHandle;

use Rfam::DB::RfamRDB;
use Rfam::AnnotatedSequence;

@ISA = qw(Rfam::DB::DB);


sub new {
   my $class = shift;
   my %params = @_;
   my $self = $class->SUPER::new(%params);

   $self->_the_RDB( Rfam::DB::RfamRDB->new( %params ));
   return $self;
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
	print "DB_RDB::query - only 'select' queries are allowed";
	exit();
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
    $@ and print "DB_RDB::query - error with query '$query' [$@]\n"; 

    return @retlist;
}




sub get_AnnotSeqs {
    my ($self, $id_list, $type_list) = @_;
    my @annseqlist;
    
    my $dbh;
    eval {
	$dbh = $self->_the_RDB->connect();
    };
    
    my %type;
    if (not $type_list) {
	$type{ "seed" } = 1;
	$type{ "full" } = 1;
    }
    else {
	foreach my $info_type (@{$type_list}) {
	    if ($info_type =~ /seed/i) {
		$type{ "seed" } = 1;
	    } 
	    elsif ($info_type =~ /full/i) {
		$type{ "full" } = 1;
	    }
	}
    }
    
    my $st_pfamA_reg_full;
    if ($type{"full"}) {
	my $stat = "select rfamseq_acc, rfam_acc, rfam_id, rfam.auto_rfam, seq_start, seq_end, rfamseq.description, bits_score from rfam_reg_full, rfamseq, rfam";
	$stat .= " where rfamseq_acc = ? and rfam.auto_rfam = rfam_reg_full.auto_rfam ";
	$stat .= "and rfam_reg_full.auto_rfamseq = rfamseq.auto_rfamseq order by rfamseq_id";
    
	$st_pfamA_reg_full = $dbh->prepare($stat);
    }
  
    my $st_pfamA_reg_seed;
    if ($type{"seed"}) {
	my $stat = "select rfamseq_acc, rfam_acc, rfam_id, rfam.auto_rfam, seq_start, seq_end, rfamseq.description  from rfam_reg_seed, rfamseq, rfam ";
	$stat .= "where rfamseq_acc = ? and rfam.auto_rfam = rfam_reg_seed.auto_rfam ";
	$stat .= "and rfam_reg_seed.auto_rfamseq = rfamseq.auto_rfamseq order by rfamseq_id";

	$st_pfamA_reg_seed = $dbh->prepare($stat);
    }
  
    foreach my $in_id (@{$id_list}) {
	my $annseq = Rfam::AnnotatedSequence->new();
	my( $sv ) = $self->rfamseq_version( $in_id );
	$annseq->id( $sv );

	if (defined $st_pfamA_reg_full) {
	    $st_pfamA_reg_full->execute($in_id);
	    while ( my($rfamseq_id, $rfam_acc, $rfam_id, $auto_rfam, $seq_start, $seq_end,$desc, $bits_score)
		    = $st_pfamA_reg_full->fetchrow) {
		
		$annseq->addAnnotatedRegion( Rfam::RfamRegion->new('-RFAM_ACCESSION' => $rfam_acc,
								   '-RFAM_ID' => $rfam_id,
								   '-SEQ_ID' => $sv,
								   '-FROM' => $seq_start,
								   '-TO' => $seq_end,
								   '-AUTO_RFAM' => $auto_rfam,
								   '-BITS' => $bits_score,
								
								   '-ANNOTATION' => $desc
								   ));
	    }
	    $st_pfamA_reg_full->finish;
	}
    
	if (defined $st_pfamA_reg_seed) {
	    eval {
		$st_pfamA_reg_seed->execute($in_id);

		while ( my($rfamseq_id, $rfam_acc, $rfam_id,$auto_rfam, $seq_start, $seq_end, $desc)
			= $st_pfamA_reg_seed->fetchrow) {

		    $annseq->addAnnotatedRegion( Rfam::RfamRegion->new('-RFAM_ACCESSION' => $rfam_acc,
								       '-RFAM_ID' => $rfam_id,
								       '-SEQ_ID' => $sv,
								       '-FROM' => $seq_start,
								       '-TO' => $seq_end,
								       '-AUTO_RFAM' => $auto_rfam,
								       
								       '-ANNOTATION' => $desc
								       ));
		    
		}
		$st_pfamA_reg_seed->finish;
	    };
	    if ($@) {
		warn "RDB connection problems [$@]";
	    }
	}
	push( @annseqlist, $annseq );
    }
    
    $self->_the_RDB->disconnect;
    return @annseqlist;
}


sub rfamseq_version {
    my $self = shift;
    my $rfamseq_acc = shift;

    my $dbh;
    eval {
	$dbh = $self->_the_RDB->connect();
    };
    if( $@ ) {
	warn "RDB connection problems [$@]";
    }
    
    my $stat = "select version from rfamseq where rfamseq_acc = ?";
    my $prep = $dbh->prepare($stat);
    
    my @version;
    eval {
	$prep->execute($rfamseq_acc);
	while( my($version) = $prep->fetchrow) {
	    push( @version, $version );
	}
    };
    if( $@ ) {
	warn "RDB connection problems [$@]";
    }
    return @version;
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
