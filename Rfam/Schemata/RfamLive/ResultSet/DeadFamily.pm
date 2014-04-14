package RfamLive::ResultSet::DeadFamily;

use strict;
use warnings;

use base 'DBIx::Class::ResultSet';

sub getAllDeadFamilyAcc {
  my ($self) = @_;
  
  my @rows = $self->search;
  my $accs = [];
  foreach my $r (@rows){
    push(@$accs, $r->rfam_acc);
  }
  
  return($accs);
}

sub createFromFamilyRow{
	my ($entry, $comment, $forward, $user) =@_;
	
	my $fam = $self->find({ rfam_acc =>$familyObj->DESC->AC} );
				
	unless ($fam) {
		croak ("Failed to find a database entry in the family table for "
			. $familyObj->DESC->AC
			. ". This is really bad!\n");
	}	
   
	my @row;

	push (@row, {	rfam_acc	=> $rfam_acc,
					rfam_id		=> $rfam_id,
					comment		=> $comment,
					forward_to	=> $forward,
					title		=> $title,
					user		=> $user } );
	$self->populate(\@row);
	
}

1;
