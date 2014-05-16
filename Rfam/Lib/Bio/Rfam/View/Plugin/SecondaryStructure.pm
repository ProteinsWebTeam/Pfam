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
 # print 'Work on this ' . $self->parent->family->SEED->path . "\n";

  $self->makeRchie;
}


#Make coloured base pair diagrams
#
sub makeBling {
	my ($self) = @_;
	my $rfamdb = $self->parent->config->rfamlive;
	my $rfam_acc = $self->parent->family->DESC->AC;
}


#Make Rchie arc l
#
sub makeRchie {
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
	print "Making arc diagram for $rfam_acc\n";
	system ($Rchie_cmd);
	if ($? == -1) {
		croak ("Failed to generate Rchie image for $rfam_acc!\n");
	}
	my $fileGzipped;
	gzip $rchie_img => \$fileGzipped;
	
	my $resultset = $rfamdb->resultset('SecondaryStructureImage')->find(
										{rfam_acc => $rfam_acc,
										type => 'rchie'},
										{key => 'acc_and_type'});
	if (!$resultset) {
		$rfamdb->resultset('SecondaryStructureImage')->create({
				rfam_acc => $rfam_acc,
				type => 'rchie',
				image => $fileGzipped
		});
	} else {
		$rfamdb->resultset('SecondaryStructureImage')->update({
				rfam_acc => $rfam_acc,
				type => 'rchie',
				image => $fileGzipped}
		);
	}
	unlink($seed_loc);
	unlink($rchie_img);
}
1;
