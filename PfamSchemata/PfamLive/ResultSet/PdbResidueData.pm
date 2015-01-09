package PfamLive::ResultSet::PdbResidueData;

use strict;
use warnings;
use Carp;
use Data::Printer;
use DBIx::Class::ResultClass::HashRefInflator;

use base 'DBIx::Class::ResultSet';

sub loadPdbResidueData {
	my ($self, $data, $dbh) = @_;
	if (!$data){
	croak "No data loaded\n";
	}

  my $count = 0;
  my @row;

#loop through the data array and put into a hash in the row array

	foreach my $residue (@$data){

#check that pfamseq_acc exists as some in pdb are not in pfam
		my $pfamseq_acc = $dbh->getSchema->resultset('Pfamseq')->find( { 'pfamseq_acc' => $residue->[8] } );
		unless ($pfamseq_acc){next;};

		push (@row, {
			pdb_id => $residue->[0],
			chain => $residue->[1],
			serial => $residue->[2],
			pdb_res => $residue->[3],
			pdb_seq_number => $residue->[4],
			pdb_insert_code => $residue->[5],
			observed => $residue->[6],
			dssp_code => $residue->[7],
			pfamseq_acc => $residue->[8],
			pfamseq_res => $residue->[9],
			pfamseq_seq_number => $residue->[10]
		} );
		$count++;
    		next if(scalar(@row) < 10000);
#when the row array reaches 10,000 populate the db
   	 	$self->populate(\@row);
    		@row = ();
	}
 	 $self->populate(\@row);

}

1;
