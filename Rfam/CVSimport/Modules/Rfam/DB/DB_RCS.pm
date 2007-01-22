

# Let the code begin...

BEGIN {
    # DB_File isn't always available
    # if it isn't then we don't want to bomb
    if( eval "use DB_File" ) {
        warn "DB_File isn't available.\n";
    }
}

package Rfam::DB::DB_RCS;
use vars qw($AUTOLOAD @ISA);
use strict;
use Fcntl;

use FileHandle;

# Object preamble - inheriets from Bio::Root::Object

use Rfam::DB::DB;
use Rfam::Entry::Entry_RCS;
use Rfam::RfamAlign;
use Bio::SeqFetcher::xdget;
use Bio::Seq;

my $DEAD_FAMILY_STRING = '__DEAD_FAMILY__';

@ISA = qw(Rfam::DB::DB);



sub new {
    my ($caller, %params) = @_;
    my $self = bless {}, ref($caller) || $caller;

    my( $current_dir, $attic_dir, $index_file, $lock_file, $rfamseq ) =
	( ($params{'-CURRENT'}   || $params{'-current'}),
	  ($params{'-ATTIC'}     || $params{'-attic'}),
	  ($params{'-INDEX'}     || $params{'-index'}),
	  ($params{'-LOCK_FILE'} || $params{'-lock_file'}),
	  ($params{'-RFAMSEQ'}   || $params{'-rfamseq'}),
	  );
    
    if( !-d $current_dir ) {
	die("$current_dir is not a directory and therefore cannot be made into a RCS db");
    }
    if( !-d $attic_dir ) {
	die("$attic_dir is not a directory, so there is nowhere to look for dead entries");
    }
    if( tie( my %testtie,'DB_File',$index_file,O_RDONLY,0666) == 0){
	die("Could not open $index_file as a DB_file [$!] [$?]");
	untie(%testtie);
    }
    if(! $lock_file =~ /\w/ ) {
	die("No lock file provided!");
    }

    my $make = $self->SUPER::new(%params);
    $make->_current_dir( $current_dir );
    $make->_attic_dir( $attic_dir );
    $make->_index_file( $index_file );
    $make->_lock_file( $lock_file );
    $make->_rfamseq( $rfamseq );

    return $make;
}


sub acc2id {
    my $self = shift;
    my $acc  = shift;
    
    my $accmap = $self->_get_tied_accmap();
    my $id = $accmap->{$acc};

    if( not defined $id ) {
	die "Rfam::DB::DB_RCS: accession [$acc] doesnot exist";
    }
    return $id;
}


sub id2acc {
    my $self = shift;
    my $id   = shift;
    
    my $accmap = $self->_get_tied_accmap();
    my %idmap  = reverse %{$accmap};
    my $acc    = $idmap{$id};

    if( not defined $acc ) {
	die "Rfam::DB::DB_RCS: accession [$id] doesnot exist";
    }
    return $acc;
}


sub is_acc {
    my $self = shift;
    my $acc  = shift;
    
    my $accmap = $self->_get_tied_accmap();
    if( exists $accmap->{$acc} ) {
	return 1;
    }
    else {
	return 0;
    }
}


sub is_id {
    my $self = shift;
    my $id   = shift;
    
    my $accmap = $self->_get_tied_accmap();
    my %idmap  = reverse %{$accmap};
    if( exists $idmap{$id} ) {
	return 1;
    }
    else {
	return 0;
    }
}


sub get_allacc {
    my $self   = shift;
    my $accmap = $self->_get_tied_accmap();

    my @acc;
    foreach my $acc ( sort keys %{$accmap} ) {
	if( $accmap->{$acc} =~ /$DEAD_FAMILY_STRING/ ) {
	    next;
	}
	push( @acc, $acc );
    }
    return @acc
}



sub get_rfamseq {
    my $self = shift;
    my $id = shift;
    my $start = shift;
    my $end   = shift;
    my $reverse;

    my $seqinx = $self->_rfamseq_inx_cache;

    $reverse = 1 if( $end and $end < $start );

    if( $end ) {
        my $options = "";
        my( $getstart, $getend ) = ( $start, $end );
        if( $reverse ) {
            ( $getstart, $getend ) = ( $end, $start );
            $options .= "-r ";
        }
        $options .= "-a $getstart -b $getend ";
        $seqinx->options( $options );
    }

    my $seq = new Bio::Seq;
    eval {
        $seq = $seqinx -> get_Seq_by_acc( $id );
    };
    if( $@ or not $seq ) {
        warn "$id not found in your seq db\n";
        return 0;       # failure
    }

    if( $end ) {
        $seq->id( $seq->id."/$start-$end" );
    }

    return $seq;
}


sub _rfamseq_inx_cache {
    my $self = shift;
    if( !$self->{-rfamseq_inx_cache} ) {
	$self->{-rfamseq_inx_cache} = Bio::SeqFetcher::xdget->new( '-db' => [ $self->_rfamseq ] );
    }
    else {
	$self->{-rfamseq_inx_cache}->options('');
    }
    return $self->{-rfamseq_inx_cache};
}
	


=head2 get_Entry_by_acc

 Title   : get_EntryA_by_acc
 Usage   : $en = $db->get_EntryA_by_acc('PF00076');
 Function: get a Bio::Pfam::EntryA object by acc
 Returns : Bio::Pfam::EntryA object
 Args    : string of the accession number of the family


=cut

sub get_Entry_by_acc {
   my ($self, $acc, $dead_please) = @_;
   my ($entry,$currentdir, $atticdir, $dir, $fname, $isdead);
   $currentdir = $self->_current_dir();

   if( ! -d  "$currentdir/$acc" ) {
       $atticdir = $self->_attic_directory();
       if (! -d "$atticdir/$acc") {
	   die("$acc is not a valid accession for this Rfam database");
       }
       else {
           if ($dead_please) {
               $dir = "$atticdir/$acc";
               $fname = "DEAD";
           }   
           else {
               die("$acc is the accession of a dead family");
           }
       }
   }
   else {
       $dir = "$currentdir/$acc";
       $fname = "DESC";
   }

   $entry = Rfam::Entry::Entry_RCS->new();
   $entry->acc($acc);

   $entry->_directory("$dir");
   $entry->_desc_filename("$fname");
 
   return $entry;
}

#########

=head2 _attic_directory

 Title   : _attic_directory
 Usage   : $acc = $db->attic_directory("/f/f/d"); 
 Function: This type of database maintains a 'attic' dir path for the
           storage of dead families. This method gets/sets it
 Returns : string of the attic dir

=cut

sub _attic_directory {
    my ($self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_attic'} = $value;
    }
    return $self->{'dbrcsbase_attic'};
}


=head2 _add_accession

 Title   : _add_accession
 Usage   : $self->_add_accession('PF001002','newfamilyname')
 Function: adds an accession. Assummes you have the lock
 Example : 
 Returns : 
 Args    :


=cut

sub _add_accession{
   my ($self,$acc,$family) = @_;
   my ($obj,%accmap);
   if( $self->{'is_locked'} != 1 ) {
       die("Have not locked the database - cannot add an accession!");
   }

   # now get and tie the hash, saving the object
   
   if( ($obj = tie( %accmap, 'DB_File' ,$self->{'dbrcsbase_index'}, O_RDWR,0666)) == 0){
       die("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
       
   }
   if( exists $accmap{$acc} ) {
       die("$acc already exists in the database");
   }

   $accmap{$acc} = $family;
   $obj->sync();
   undef $obj;
   untie(%accmap);
}

=head2 _move_accession

 Title   : _move_accession
 Usage   : $self->_move_accession('RF00102','newfamilyname')
 Function: change id of an accession. Assumes you have the lock
 Example : 
 Returns : 
 Args    :


=cut

sub _move_accession{
   my ($self,$acc,$family) = @_;
   my ($obj,%accmap);
   if( $self->{'is_locked'} != 1 ) {
       die("Have not locked the database - cannot add an accession!");
   }

   # now get and tie the hash, saving the object
   
   if( ($obj = tie( %accmap, 'DB_File' ,$self->{'dbrcsbase_index'}, O_RDWR,0666)) == 0){
       die("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
       
   }

   $accmap{$acc} = $family;
   $obj->sync();
   undef $obj;
   untie(%accmap);
}

=head2 _kill_accession

 Title   : _kill_accession
 Usage   : $self->_kill_accession('PF001002')
 Function: Kills an accession *only in the db file*, not in 
           the underlying database. Assummes you have the lock
 Example :
 Returns : 
 Args    :


=cut

sub _kill_accession{
   my ($self,$accession) = @_;
   my ($obj,%accmap, $oldid);
   if( $self->{'is_locked'} != 1 ) {
       die("Have not locked the database - cannot kill a accession!");
   }

   # now get and tie the hash, saving the object
   
   if( ($obj = tie(%accmap,'DB_File',$self->{'dbrcsbase_index'},O_RDWR,0666)) == 0){
       die("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
       
   }
   if( !exists $accmap{$accession} ) {
       die("$accession does not exist in db!");
   }
   else {
       $oldid = $accmap{$accession};
   }

   $accmap{$accession} = "$DEAD_FAMILY_STRING $oldid";
   $obj->sync();
   $obj = undef;
   untie(%accmap);
}

=head2 _get_lock

 Title   : _get_lock
 Usage   : if( ($lockee = $db->_get_lock()) ) {
               die("Lock already got by $lockee");
           }
 Function:
 Example :
 Returns : undef for success, or lockee for failure
 Args    :


=cut

sub _get_lock{
   my ($self) = @_;
   my ($line,$name,$obj,%accmap,$index_file);

   my ($me) = `whoami`;
   chomp($me);

   if( !-e $self->_lock_file ) {
       # get the lock now!

       open( _LOCK_, ">".$self->_lock_file ) || die "Could not open lock file $!";
       print _LOCK_ "$me has locked the database\n";
       close(_LOCK_);
       $self->{'is_locked'} = 1;

       return undef;
   } else {
       open( _LOCK_, $self->_lock_file ) || die "Could not open lock file $!";
       $line = <_LOCK_>;
       $line =~ /(\S+)/;
       $name = $1;
       close(_LOCK_);
       return $name;
   }
}


=head2 _unlock

 Title   : _unlock
 Usage   : $self->_unlock();
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _unlock{
   my ($self,@args) = @_;
   my ($key);

   if( $self->{'is_locked'} != 1 ) {
       die("Trying to release the lock when you haven't got it!");
   }

   if( ! -e $self->_lock_file ) {
       die("Bad error - you have the lock but there is no lock file!");
   }

   unlink($self->_lock_file);
   $self->{'is_locked'} = 0;

   return 0;
}


sub _index_file {
    my( $self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_index'} = $value;
    }
 
    return $self->{'dbrcsbase_index'};
}


sub _current_dir {
    my ($self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_current'} = $value;
    }
 
    return $self->{'dbrcsbase_current'};
}

sub _attic_dir {
    my ($self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_attic'} = $value;
    }
 
    return $self->{'dbrcsbase_attic'};
}

sub _lock_file {
    my ($self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_lock'} = $value;
    }
 
    return $self->{'dbrcsbase_lock'};
}

sub _rfamseq {
    my ($self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_rfamseq'} = $value;
    }
 
    return $self->{'dbrcsbase_rfamseq'};
}


=head2 _get_tied_accmap

 Title   : _get_tied_accmap
 Usage   : $refhash = $db->_get_tied_accmap()
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _get_tied_accmap{
   my ($self) = @_;
   my ($obj,%accmap,$out,$temp);

   if( defined $self->_accmap_cache() ) {
       return $self->_accmap_cache();
   }

   # now get and tie the hash, saving the object
   
   if( tie(%accmap,'DB_File',$self->{'dbrcsbase_index'},O_RDONLY,0666) == 0){
       die("Could not open $self->{'dbrcsbase_index'} as a DB_file $!");
   }
   $out = {};
   %{$out} = %accmap;
   untie(%accmap);
   $self->_accmap_cache($out);

   return $out;
}

=head2 _accmap_cache

 Title   : _accmap_cache
 Usage   : $obj->_accmap_cache($newval)
 Function: 
 Example : 
 Returns : value of _accmap_cache
 Args    : newvalue (optional)


=cut

sub _accmap_cache{
   my ($self,$value) = @_;
   if( defined $value) {
      $self->{'_accmap_cache'} = $value;
    }
    return $self->{'_accmap_cache'};

}

