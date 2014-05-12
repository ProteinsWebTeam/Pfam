package Bio::Rfam::View::Plugin::SecondaryStructure;

use Data::Dumper;
use Moose;
with 'MooseX::Role::Pluggable::Plugin';

use IO::Compress::Gzip qw(gzip $GzipError);

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
  print 'Work on this ' . $self->parent->family->SEED->path . "\n";

  $self->makeRchie;
}

#Make Rchie arc diagrams
#
sub makeRchie {
	print "job id: $$\n";
	my ($self) = @_;
	
	my $rfamdb = $self->parent->config->rfamlive;
	my $rfam_acc = $self->parent->family->DESC->AC;
	
	my $location = "/nfs/nobackup2/xfam/rfam";
	my $seed_loc = "$location/$rfam_acc";
	my $rchie_img = "$location/$rfam_acc.rchie.png";
	my $msa = $self->parent->family->SEED;
    $msa->write_msa($seed_loc);
	
	my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );
	if (!defined($famRow)) {
		croak ("Failed to find entry in the Family table for $rfam_acc.");
	}

	my $Rchie_cmd = "stockholm2Arc.R $seed_loc $rchie_img 2> $location/$$.err";
	print "Making arc diagram for $rfam_acc: $Rchie_cmd\n";
	system ($Rchie_cmd);
	if ($? == -1) {
		croak ("Failed to generate Rchie image for $rfam_acc!\n");
	}
	my $fileGzipped;
	gzip $rchie_img => \$fileGzipped;
	#my $resultset = $rfamdb->resultset('Family')->find({"me.rfam_acc" => $rfam_acc,
	#													"secondary_structure_images.type" => "rchie"},
	#														{join =>"secondary_structure_images"});	
	
	my $resultset = $rfamdb->resultset('SecondaryStructureImage')->find(
										{rfam_acc => $rfam_acc,
										type => "rchie"},
										{key => 'acc_and_type'});
	if (!$resultset) {
		$rfamdb->resultset('SecondaryStructureImage')->create({
				rfam_acc => $rfam_acc,
				type => 'rchie',
				image => $fileGzipped
		});
	} else {
		$resultset->update({
				rfam_acc => $rfam_acc,
				type => 'rchie',
				image => $fileGzipped}
		);
	}
	#my $famRow = $rfamdb->resultset('Family')->find({"rfam_acc" => $rfam_acc});	
	#my $row = $rfamdb->resultset('SecondaryStructureImage')->update_or_create(
	#	{
	#		rfam_acc => $rfam_acc,
	#		type => 'rchie',
	#		image => $fileGzipped
	#	},
	#	{key => 'acc_and_type'}
	#	);
	#$resultset->update_or_create( { secondary_structure_images =>[ 
	#		{image =>$fileGzipped,
	#		type => 'rchie',
	#		rfam_acc => $rfam_acc}]});

}
1;
