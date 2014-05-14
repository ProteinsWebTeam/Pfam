package Bio::Rfam::View::Plugin::Tree;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

use IO::Compress::Gzip qw(gzip $GzipError);
use Data::Dumper;
has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
  $self->makeSeedTrees;
}

#Make phylogenetic tree for seed
#
sub makeSeedTrees {
	my ($self) = @_;
	my ($treeMethod,$cmd,$taxcmd,$infile,$outfile);

	my $rfamdb = $self->parent->config->rfamlive;
	my $rfam_acc = $self->parent->family->DESC->AC;
	
	my $location = "/nfs/nobackup2/xfam/rfam";
	my $seedfile = "$location/$rfam_acc";
	my $tree = "$location/$rfam_acc.outtree";
	my $taxseed = "$location/$rfam_acc.tax";
	my $taxtree = "$location/$rfam_acc.taxtree";
	my $msa = $self->parent->family->SEED;

	my $famRow = $rfamdb->resultset('Family')->find( {rfam_acc => $rfam_acc} );
	if (!defined($famRow)) {
		croak ("Failed to find entry in the family table for $rfam_acc!\n");
	}
	
    $treeMethod = 'fasttree';
	$msa->write_msa("$seedfile","afa");
	$msa->seqToSpeciesNames($rfamdb);
	$msa->write_msa("$taxseed","afa");
	$cmd = "FastTree -nt -nj -boot 100 < $seedfile > $tree 2> /dev/null";
    $taxcmd = "FastTree -nt -nj -boot 100 <$taxseed> $taxtree 2> /dev/null";

	system ($cmd) == 0 or croak ("Problem with generating the seed tree....this is not good!\n");
	system ($taxcmd) == 0 or croak ("Problem with generating the seedtax tree...this is not good!\n");
	
	my ($fileGzipped,$taxGzipped);
	gzip $tree => \$fileGzipped;
	gzip $taxtree => \$fileGzipped;
	
	my $row = $rfamdb->resultset('AlignmentAndTree')->find(
		{
			rfam_acc => $rfam_acc
		});
	if (!$row) {
		$rfamdb->resultset('AlignmentAndTree')->create({ rfam_acc => $rfam_acc,
					type =>'seed',
					tree => $fileGzipped,
					treemethod => $treeMethod
	
		});
		$rfamdb->resultset('AlignmentAndTree')->create({	rfam_acc => $rfam_acc,
					type => 'seedtax',
					tree => $taxGzipped,
					treemethod => $treeMethod
		});
	} else {
		$row->update({ rfam_acc => $rfam_acc,
					type =>'seed',
					tree => $fileGzipped,
					treemethod => $treeMethod
	
		});
		$row->update({	rfam_acc => $rfam_acc,
					type => 'seedtax',
					tree => $taxGzipped,
					treemethod => $treeMethod
		});

	}
	unlink($seedfile);
	unlink($taxseed);
	unlink($tree);
	unlink($taxtree);
}

1;
1;

























