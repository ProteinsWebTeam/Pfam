

# Let the code begin...


package Database::DB_RCS;
use vars qw($AUTOLOAD @ISA);
use strict;
use Fcntl;
use DB_File;
use FileHandle;

# Object preamble - inheriets from Bio::Root::Object

use Database::DB;
use Database::Entry_RCS;

my $DEAD_FAMILY_STRING = '__DEAD_FAMILY__';

@ISA = qw(Database::DB);

sub new {

  my ($caller, %params) = @_;
  
  my $self = bless {}, ref($caller) || $caller;

   my($current_dir) = ( ($params{'-CURRENT'} || $params{'-current'}) );

 if( ! -d $current_dir ) {
      $self->throw("$current_dir is not a directory and therefore cannot be made into a RCS based CURRENT db");
  }
  my $make = $self->SUPER::new(%params);
  $make->_current_directory($current_dir);
  return $make;
}





=head2 get_Entry_by_acc

 Title   : get_EntryA_by_acc
 Usage   : $en = $db->get_EntryA_by_acc('PF00076');
 Function: get a Bio::Pfam::EntryA object by acc
 Returns : Bio::Pfam::EntryA object
 Args    : string of the accession number of the family


=cut

sub get_Entry_by_acc{

   my ($self,$acc, $dead_please) = @_;
   my ($entry,$id,$currentdir, $atticdir, $dir, $fname, $isdead);


     
       $currentdir = $self->_current_directory();

       if( ! -d  "$currentdir/$acc" ) {
	 print "$acc is not a valid accession for this Rfam database\n";
       }
       else {
	   $dir = "$currentdir/$acc";
	   $fname = "DESC";
       }
#   }

   $entry = Database::Entry_RCS->new();
   $entry->acc($acc);

   $entry->_directory("$dir");
   $entry->_desc_filename("$fname");
 
   return $entry;
}

sub _current_directory {
    my ($self, $value ) = @_;

    if (defined $value) {
        $self->{'dbrcsbase_current'} = $value;
    }
 
    return $self->{'dbrcsbase_current'};
}





