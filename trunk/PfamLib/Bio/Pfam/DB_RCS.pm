
#
# BioPerl module for Bio::Pfam::DB_RCS
#
# Written by Ewan Birney
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::DB_RCS - An interface to a pfam database implemented as a set of
directoties under RCS control

=head1 SYNOPSIS

This is a concrete implementation of the abstract database object
found in Bio::Pfam::DB

=head1 DESCRIPTION

This concrete implementation of DB.pm implements the underlying database
as a set of directories, one for each family 

=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...

BEGIN {
    # DB_File isn't always available
    # if it isn't then we don't want to bomb
    if( eval "use DB_File" ) {
	warn "DB_File isn't available. Some Bio::Pfam functionality won't work\n";
    }
}

package Bio::Pfam::DB_RCS;
use vars qw($AUTOLOAD @ISA);
use strict;
use Fcntl;
use Bio::Pfam::SeqPfam;
use FileHandle;

# Object preamble - inheriets from Bio::Root::Object

use Bio::Pfam::DB;
use Bio::Pfam::EntryA_RCS;
use Bio::Pfam::EntryB_Impl;
use Bio::Pfam::PfamAnnSeqFactory;

my $DEAD_FAMILY_STRING = '__DEAD_FAMILY__';

@ISA = qw( Bio::Pfam::DB );

sub new {
    my($class,@args) = @_;
    my(%testtie);
    my $self = $class->SUPER::new(@args);

    my($current_dir, $attic_dir, $index_file,$lock_file, $fastadb, $srsdb, $srspfamb, $srsswiss) = 
	$self->_rearrange([qw(CURRENT
			      ATTIC
			      INDEX
			      LOCK_FILE
			      FASTADB
			      SRSDB
			      SRSPFAMB
			      SRSSWISSPFAM
			      )],@args);
 
    if( ! -d $current_dir ) {
	$self->throw("$current_dir is not a directory and therefore cannot be made into a RCS based CURRENT db");
    }
    if (! -d $attic_dir ) {
	$self->throw("$attic_dir is not a directory, so there is nowhere to look for dead entries");
    }
    
    if( tie(%testtie,'DB_File',$index_file,O_RDONLY,0666) == 0){
	$self->throw("Could not open $index_file as a DB_file [$!] [$?]");
    }
    if(! $lock_file =~ /\w/ ) {
	$self->throw("No lock file provided!");
    }

    untie(%testtie);

    $self->{'dbrcsbase_current'} = $current_dir;
    $self->{'dbrcsbase_attic'} = $attic_dir;
    $self->{'dbrcsbase_index'} = $index_file;
    $self->{'dbrcsbase_lock_file'} = $lock_file;
    $self->{'dbrcsbase_srsdb'} = $srsdb;
    $self->{'dbrcsbase_srspfamb'} = $srspfamb;
    $self->{'dbrcsbase_fastadb'} = $fastadb;
    $self->{'dbrcsbase_srsswisspfam'} = $srsswiss;

    return $self;
}


=head2 id2acc

 Title   : id2acc
 Usage   : $acc = $db->id2acc("pkinase"); 
 Function: converts an id to an accession. Only works for
	   current ids - does not work for previous ids
 Returns : string of the accession, or undef if no accession
 Throws  : Exceptions thrown if this $id does not exist or due to RCS misconfiguration
 Args    :

=cut

sub id2acc{
   my ($self,$id) = @_;
   
   my ($accmap,%idmap,$acc,$name);

   my $index_file = $self->{'dbrcsbase_index'};

   if( $id =~ /$DEAD_FAMILY_STRING/ ) {
     $self->throw("Id $DEAD_FAMILY_STRING indicates that this family was killed.");
   }

   $accmap = $self->_get_tied_accmap();

   %idmap = reverse %{$accmap};

   $acc = $idmap{$id};
   if( !defined $acc) {
     $self->throw("Id $id does not exist in $index_file");
   }

   return $acc;
}

=head2 acc2id

 Title   : acc2id
 Usage   : $id = $db->acc2id("PF00001");
 Function: converts an accession to an id
 Returns : the id, throws an exception if no accession, or dead accession
 Args    :

=cut

sub acc2id{
   my ($self,$acc) = @_;
   my ($accmap,$id,$name);
   my $index_file = $self->{'dbrcsbase_index'};
   #open the index file for Reading, get out the hash and return it.

   $accmap = $self->_get_tied_accmap();

   $id = $name = $accmap->{$acc};
   if( !defined $id ) {
       $self->throw("Accession $acc does not exist in $index_file");
   }

   if( $id =~ /$DEAD_FAMILY_STRING\s+(\S+)/ ) {
       $name = $1;
   }

   return $name;
}


=head2 is_dead_acc

 Title   : is_dead_acc
 Usage   : if( $db->is_dead_acc('PF00034') ) { print "Is a dead accession" }
 Example :
 Returns : 1 if dead, undef if not
 Args    : accesion number


=cut

sub is_dead_acc{
  my ($self,$acc) = @_;
  my ($accmap,$id,$name);
  my $index_file = $self->{'dbrcsbase_index'};
  #open the index file for Reading, get out the hash and return it.


  $accmap = $self->_get_tied_accmap();

  $id = $accmap->{$acc};
  if( !defined $id ) {
    $self->throw("Accession $acc does not exist in $index_file");
  }
  
  if( $id =~ m/$DEAD_FAMILY_STRING/ ) {
    return 1;
  } else {
    return undef;
  }
}


=head2 get_AnnotSeqs

 Title   : get_AnnotSeqs
 Usage   : @annotseqs = $db->get_AnnotSeqs( $id1, $id2, ... );
 Function: gets an Annotated sequence from underlying swisspfam db
 Returns : Bio::Pfam::AnnotatedSequence (list of)
 Args    : A swisspfam (pfamseq) identifier (list of)
 Notes   : 
    A pfamseq identifier (ref to a list of)
    A list of region types needed
 Notes   : 
    1. The AnnotatedSequence returned has no enclosed sequence,
    just annotation. If the sequence is required, use 
    get_Seq_pfamseq to get a Bio::Pfam::SeqPfam and insert it into
    the AnnotatedSequence method using 
    AnnotatedSequence->sequence().

    2. Region-type items can be:
        'seed'
        'full'
        'pfamb'
        'other'
    When the list is empty, all regions are returned. If not, then 
    if the specified region types are availalble they will be 
    returned, but the types specified will not be returned

=cut


sub get_AnnotSeqs{
   my ($self, $input_ids, $type_list) = @_;

   my ($fh, $annotseq, $fac, $swisspfamdb, @retlist);

   $swisspfamdb = $self->{'dbrcsbase_srsswisspfam'};

   if ($type_list) {
       foreach my $item (@$type_list) {
	   if ($item !~ /full/i and $item !~ /pfamb/i) {
	       $self->throw("Bio::Pfam::DB_RCS->get_AnnotSeqs does not support region type $item");
	   }
       }
   }

   $fac = Bio::Pfam::PfamAnnSeqFactory->instance();

   foreach my $in_id (@{$input_ids}) {
       $annotseq = $fac->createAnnotatedSequence();
       
       $fh = new FileHandle;
       
       if( $fh->open("getz -e '[$swisspfamdb-id:$in_id]' |") == 0) {
	   $self->throw("Cannot fork getz process off");
       }
       
       $fac->addSwissPfamToAnnSeq( $annotseq, $fh, $type_list);
       
       
       if( $fh->close() == 0 ) {
	   $self->throw("Getz pipe failed to close");
       }

       push @retlist, $annotseq;
   }

   return @retlist;

}


=head2 get_Bait_file

 Title   : get_Bait_file
 Usage   : $file = $db->get_Bait_file( $accession )
 Function: Parses the bait file for the given accesison number and returns a filled
    SFRObject
 Returns : A ref to a filehandle glob
 Args    : A Pfam Accession number

=cut

sub get_Bait_File{
   my ($self, $accession) = @_;
       
   my $fh = FileHandle->new();

   $fh->open( "<".$self->{'dbrcsbase_current'}."/".$self->acc2id( $accession )."/BAIT") or return undef;

   return $fh;    
}



=head2 get_EntryA_by_id

 Title   : get_EntryA_by_id
 Usage   : $en = $db->get_EntryA_by_id('pkinase');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family


=cut

sub get_EntryA_by_id{
   my ($self, $id, $dead_please) = @_;
   my ($entry,$currentdir,$atticdir,$dir, $fname);

   # ok - load up entry object directory
   $currentdir = $self->_current_directory();

   if( ! -d  "$currentdir/$id" ) {
       $atticdir = $self->_attic_directory();
       if (! -d "$atticdir/$id") {
	   $self->throw("$id is not a valid id for this Pfam database");
       }
       else {
	   if ($dead_please) {
	       $dir = "$atticdir/$id";
	       $fname = "DEAD";
	   }   
	   else {
	       $self->throw("$id is the id of a dead family");
	   }
       }
   }
   else {
       $dir = "$currentdir/$id";
       $fname = "DESC";
   }

   $entry = Bio::Pfam::EntryA_RCS->new();
   $entry->id($id);
   $entry->_directory("$dir");
   $entry->_desc_filename("$fname");
   
   return $entry;
}



=head2 get_EntryB_by_id

 Title   : get_EntryB_by_id
 Usage   : $en = $db->get_EntryB_by_id('Pfam-B_101');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family


=cut

sub get_EntryB_by_id{
    my ($self,$id) = @_;
    my ($out, $pfambdb);
    
    $pfambdb = $self->{'dbrcsbase_srspfamb'};
    
    open TEST, "getz -e '[$pfambdb-id:$id]' |";
    while (<TEST>) {
	if (/^AC/) {
	    $out = Bio::Pfam::EntryB_Impl->new();
	    $out->_datasource("getz -e '[$pfambdb-id:$id]' |");
	    $out->id($id);
	}
    }
    close TEST;

    if (not defined $out) {
	$self->throw("There is no Pfam-B entry for id $id");
    }
    else {
	return $out;
    }
}




=head2 get_EntryA_by_acc

 Title   : get_EntryA_by_acc
 Usage   : $en = $db->get_EntryA_by_acc('PF00076');
 Function: get a Bio::Pfam::EntryA object by acc
 Returns : Bio::Pfam::EntryA object
 Args    : string of the accession number of the family


=cut

sub get_EntryA_by_acc{

   my ($self,$acc, $dead_please) = @_;
   my ($entry,$id,$currentdir, $atticdir, $dir, $fname, $isdead);

   eval {
       $id = $self->acc2id($acc);
       $isdead = $self->is_dead_acc( $acc );
   };
   if ($@) {
       $self->throw("$acc is not a valid accession for this Pfam database");
   }
   
   if ($isdead) {
       if ($dead_please) {
	   $atticdir = $self->_attic_directory();
	   $dir = "$atticdir/$id";
	   $fname = "DEAD";
       }
       else {
	   $self->throw("$acc is the accession of a dead family (with id $id)");
       }
   }
   else {
     
       $currentdir = $self->_current_directory();
       if( ! -d  "$currentdir/$id" ) {
	   $self->throw("$acc is not a valid accession for this Pfam database");
       }
       else {
	   $dir = "$currentdir/$id";
	   $fname = "DESC";
       }
   }

   $entry = Bio::Pfam::EntryA_RCS->new();
   $entry->id($id);
   $entry->_directory("$dir");
   $entry->_desc_filename("$fname");

   return $entry;
}




=head2 get_EntryB_by_acc

 Title   : get_EntryB_by_id
 Usage   : $en = $db->get_EntryB_by_id('Pfam-B_101');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family


=cut

sub get_EntryB_by_acc{
   my ($self,$acc) = @_;
   my ($out, $pfambdb);

   $pfambdb = $self->{'dbrcsbase_srspfamb'};
   
   open TEST, "getz -e '[$pfambdb-acc:$acc]' |";
   while (<TEST>) {
       if (/^AC/) {
	   $out = Bio::Pfam::EntryB_Impl->new();
	   $out->_datasource("getz -e '[$pfambdb-acc:$acc]' |");
	   $out->acc($acc);
       }
   }
   close TEST;

   if (not defined $out) {
       $self->throw("There is no Pfam-B entry for acc $acc");
   }
   else {
       return $out;
   }
}



=head2 get_dead_accs

 Title   : get_dead_accs
 Usage   : @list = $db->get_dead_accs('ALPHA');
 Function: Gets a list of accession numbers of dead families in current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_dead_accs{
   my ($self,$type) = @_;
   my ($accmap,@acc,@tempacc,$acc,$name);
   my $index_file = $self->{'dbrcsbase_index'};

   if( !defined $type ) {
       $type = 'ALPHA';
   }

   #open the index file for Reading, get out the hash and return it.

   $accmap = $self->_get_tied_accmap();

   @acc = keys %{$accmap};

   # remove everything that does not have __DEAD_FAMILY__

   foreach $acc ( @acc ) {
     if( $accmap->{$acc} !~ /$DEAD_FAMILY_STRING/ ) {
       next;
     }
     push(@tempacc,$acc);
   }
   @acc = @tempacc;

   # now sort it etc.

   SWITCH : {
       $type =~ /ALPHA/ && do {
	   @acc = sort { lc($accmap->{$a}) cmp lc($accmap->{$b}) } @acc;
	   last SWITCH;
       };
       $type =~ /DATE/ && do {
	   @acc = sort {my ($aa,$bb); $a =~ /^PF+0+(\d+)$/; $aa = $1; $b =~ /^PF+0+(\d+)$/; $bb = $1; return $aa <=> $bb; } @acc;
	   last SWITCH;
       };
       $self->throw("No proper type for this $type");
   }

   return @acc;
}



=head2 get_allacc

 Title   : get_allacc
 Usage   : @list = $db->getallacc('ALPHA');
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_allacc{
   my ($self,$type) = @_;
   my ($accmap,@acc,@tempacc,$acc,$name);
   my $index_file = $self->{'dbrcsbase_index'};

   if( !defined $type ) {
       $type = 'ALPHA';
   }

   #open the index file for Reading, get out the hash and return it.

   $accmap = $self->_get_tied_accmap();

   @acc = keys %{$accmap};

   # remove everything with __DEAD_FAMILY__

   foreach $acc ( @acc ) {
     if( $accmap->{$acc} =~ /$DEAD_FAMILY_STRING/ ) {
       next;
     }
     push(@tempacc,$acc);
   }
   @acc = @tempacc;

   # now sort it etc.

   SWITCH : {
       $type =~ /ALPHA/ && do {
	   @acc = sort { lc($accmap->{$a}) cmp lc($accmap->{$b}) } @acc;
	   last SWITCH;
       };
       $type =~ /DATE/ && do {
	   @acc = sort {my ($aa,$bb); $a =~ /^PF+0+(\d+)$/; $aa = $1; $b =~ /^PF+0+(\d+)$/; $bb = $1; return $aa <=> $bb; } @acc;
	   last SWITCH;
       };
       $self->throw("No proper type for this $type");
   }

   return @acc;
}




=head2 get_Seq_pfamseq

 Title   : get_Seq_pfamseq($id)
 Usage   : $seq = $db->get_Seq_pfamseq($id)
 Function: gets a Bio::Pfam::SeqPfam object from the underlying database
 Returns : a newly made Bio::Pfam::SeqPfam object
 Args    :
    1. name of sequence
    2. Whether the name is an accession number or id ('id' or 'acc')
    3. Whether the actual sequence itself is required

=cut

sub get_Seq_pfamseq{
   my ($self,$input_id, $lookup, $seqrequired) = @_;
   my ($temp,$seq,$fh,$out, $srspfamseq, $id, $acc, $readingseq, $desc, $org, $seqcmd);

   if (not defined $lookup) {
       $lookup = 'id';
   }
   $seqcmd = ($seqrequired)?"-f 'seq' -sf fasta":"";

   $srspfamseq = $self->{'dbrcsbase_srsdb'};

   $fh = new FileHandle;
   if( $fh->open("getz -f 'acc' -f 'id' -f 'des' -f 'org' $seqcmd '[$srspfamseq-$lookup:$input_id]' |") == 0) {
       $self->throw("Cannot fork getz process off (DB_RCS::get_filehandle_pfamseq)");
   }
   $seq = "";
   $desc = "";
   $id = undef;
   $readingseq = 0;

   while (<$fh>) {
       /^\/\// and last;

       if ($readingseq) {
	   /^>/ && $self->throw("Request $id gave multiple sequences in db. Problem!");	   
	   s/\W//g;
	   $seq .= $_;
       }
       elsif (/^AC\s+(\S+);/) {
	   $acc = $1;
       }
       elsif (/^ID\s+(\S+)/) {
	   $id = $1;
       }
       elsif (/^DE\s+(.*)/) {
	   if ($desc) {
	       $desc .= " $1";
	   }
	   else {
	       $desc = $1;
	   }
       }
       elsif (/^OS\s+(.*)/) {
	   if ($org) {
	       $org .= " $1";
	   }
	   else {
	       $org = $1;
	   }
       }
       elsif ( /^>(\S+)\s/ ) {
	   $readingseq = 1;
       }
   }

   if( $fh->close() == 0 ) {
       $self->throw("Getz pipe failed to close");
   }

   $org =~ s/\.$//; $org = lc $org;
   $desc =~ s/\.$//; $desc = lc $desc;

   ($acc and $id and $desc and $org) or
       $self->throw("DB_RCS::get_Seq_pfamseq - Information missing");

   $out = Bio::Pfam::SeqPfam->new('-seq' => $seq,
				  '-start' => '1',
				  '-end' => length($seq),
				  '-id'=> $id,
				  '-acc' => $acc,
				  '-organism' => $org,
				  '-desc' => $desc);

   return $out;
}


sub pfamseq_dbsize {
    my $self = shift;
    my($dir) = $self->_get_fasta_pfamseq() =~ /(\S+)\//;
    open( F, "$dir/DBSIZE" ) or die;
    my $size = <F>;
    chomp $size;
    return $size;
}


=head2 pfamseq_stats

 Title   : pfamseq_stats
 Usage   : ($residues,$sequences) = $db->pfamseq_stats();
 Function: Calculate statistics for underlying pfamseq database
           
 Returns : An array of number of residues and sequences
         : in pfamseq
 Args    :


=cut

sub pfamseq_stats {
  my ($self) = @_;
  my ($residues,$sequences);
  my($file)=$self->_get_fasta_pfamseq();
  # Use seqstat to make stats!
   my ($nseq,$fromlen,$tolen,$status,$avlen);
  open (TMP,"seqstat $file |");
  while (<TMP>) {
    if (/^Total \# residues:\s+(\d+)/) {$residues = $1;}
    if (/^Number of sequences:\s+(\d+)/) {$sequences = $1;}
  }
  close(TMP) || $self->throw("Couldn't close pipe");
  return ($residues,$sequences);
}



=head2 _current_directory

 Title   : _current_directory
 Usage   : $acc = $db->current_directory("/f/f/d"); 
 Function: This type of database maintains a 'current' directory path,
           and this method gets and sets it
 Returns : string of the current dir

=cut

sub _current_directory {
    my ($self, $value ) = @_;

    if (defined $value) {
	$self->{'dbrcsbase_current'} = $value;
    }
    return $self->{'dbrcsbase_current'};
}



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




=head2 _get_fasta_pfamseq

 Title   : get_fasta_pfamseq
 Usage   : $filename = $db->_get_fasta_pfamseq()
 Function: returns the filename (full path) of pfamseq, the 
           underlying sequence database of pfam
 Returns : a string of the filename
 Args    :


=cut

sub _get_fasta_pfamseq{
   my ($self) = @_;

   return $self->{'dbrcsbase_fastadb'};
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
       $self->throw("Have not locked the database - cannot add an accession!");
   }

   # now get and tie the hash, saving the object
   
   if( ($obj = tie(%accmap,'DB_File',$self->{'dbrcsbase_index'},O_RDWR,0666)) == 0){
       $self->throw("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
       
   }
   if( exists $accmap{$acc} ) {
       $self->throw("$acc already exists in the database");
   }

   $accmap{$acc} = $family;
   $obj->sync();
   untie(%accmap);
}

=head2 _move_accession

 Title   : _move_accession
 Usage   : $self->_move_accession('PF001002','newname')
 Function: Moves an accession *only in the db file*, not in 
           the underlying database. Assummes you have the lock
 Example :
 Returns : 
 Args    :


=cut

sub _move_accession{
   my ($self,$accession,$to) = @_;
   my ($obj,%accmap);
   if( $self->{'is_locked'} != 1 ) {
       $self->throw("Have not locked the database - cannot add an accession!");
   }

   # now get and tie the hash, saving the object
   
   if( ($obj = tie(%accmap,'DB_File',$self->{'dbrcsbase_index'},O_RDWR,0666)) == 0){
       $self->throw("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
       
   }
   if( !exists $accmap{$accession} ) {
       $self->throw("$accession does not exist in db!");
   }

   $accmap{$accession} = $to;
   $obj->sync();
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
       $self->throw("Have not locked the database - cannot kill a accession!");
   }

   # now get and tie the hash, saving the object
   
   if( ($obj = tie(%accmap,'DB_File',$self->{'dbrcsbase_index'},O_RDWR,0666)) == 0){
       $self->throw("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
       
   }
   if( !exists $accmap{$accession} ) {
       $self->throw("$accession does not exist in db!");
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
               $db->throw("Lock already got by $lockee");
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


   if( !-e $self->{'dbrcsbase_lock_file'} ) {
       # get the lock now!

       open(_LOCK_,">$self->{'dbrcsbase_lock_file'}") || die "Could not open lock file $!";
       print _LOCK_ "$me has locked the database\n";
       close(_LOCK_);
       $self->{'is_locked'} = 1;

       return undef;
   } else {
       open(_LOCK_,"$self->{'dbrcsbase_lock_file'}") || die "Could not open lock file $!";
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
       $self->throw("Trying to release the lock when you haven't got it!");
   }

   if( ! -e $self->{'dbrcsbase_lock_file'} ) {
       $self->throw("Bad error - you have the lock but there is no lock file!");
   }

   unlink($self->{'dbrcsbase_lock_file'});
   $self->{'is_locked'} = 0;

   return 0;
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
       $self->throw("Could not open $self->{'dbrcsbase_index'} as a DB_file $! Leaving the lock on");
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





