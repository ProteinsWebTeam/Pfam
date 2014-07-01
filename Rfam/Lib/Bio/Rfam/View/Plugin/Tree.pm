package Bio::Rfam::View::Plugin::Tree;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

use IO::Compress::Gzip qw(gzip $GzipError);
use Data::Dumper;
use Carp;
 
has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
  #$self->makeSeedTrees;
}

#Make phylogenetic tree for seed
#
sub makeSeedTrees {
	my ($self) = @_;
	my ($treeMethod,$cmd,$taxcmd,$infile,$outfile,$type);

	my $rfamdb = $self->parent->config->rfamlive;
	my $rfam_acc = $self->parent->family->DESC->AC;
#Set up some locations for writing files to:
#
	my $location = "/nfs/nobackup2/xfam/rfam";
	my $seedfile = "$location/$rfam_acc";
	my $tree = "$location/$rfam_acc.outtree";
	my $taxtree = "$location/$rfam_acc.taxtree";

#Get SEED file, and check the family exists in the database:
	my $msa = $self->parent->family->SEED;
	
	my $famRow = $rfamdb->resultset('Family')->find( {rfam_acc => $rfam_acc} );
	if (!defined($famRow)) {
		croak ("Failed to find entry in the family table for $rfam_acc!\n");
	}
#FastTree needs an aligned fasta file, so convert the seed file:
#
    $treeMethod = 'fasttree';
	$msa->write_msa("$seedfile","afa");
	
	print "Making trees for $rfam_acc\n";

	$cmd = "FastTree -nt -nj -boot 100 < $seedfile > $tree 2> /dev/null";
	system ($cmd) == 0 or croak ("Problem with generating the seed tree....this is not good!\n");

#We generate two trees: one with the sequence accessions as labels, and one with the species. We can't use the seed file
#labelled with the species names to generate a tree, as FastTree can't cope with non-unique sequence names, so we convert
#the sequence accession tree:
#
	specifyTree($msa, $rfamdb ,$tree,$taxtree);	
	
	my ($fileGzipped,$taxGzipped);
	gzip $tree => \$fileGzipped;
	gzip $taxtree => \$taxGzipped;

#Update the database:
	
	my $row = $rfamdb->resultset('AlignmentAndTree')->find_or_create(
		{
			rfam_acc => $rfam_acc,
			type => 'seed',
		},
		{key => 'rfam_acc_and_type'}
		);
	$row->update( { tree => $fileGzipped,
					type => 'seed',},
					{key => 'rfam_acc_and_type'});

	my $taxrow = $rfamdb->resultset('AlignmentAndTree')->find_or_create(
		{
			rfam_acc => $rfam_acc,
			type => 'seedTax',
		},
		{ key => 'rfam_acc_and_type'}
		);
	
	$taxrow->update( { tree => $taxGzipped,
					type => 'seedTax',},
					{key => 'rfam_acc_and_type'}
				);

#Cleanup:
#
	unlink($seedfile);
	unlink($tree);
	unlink($taxtree);
}
#This bit of code is based on Eric's seqToSpeciesNames. Needed as FastTree won't allow non-unique names in a tree

sub specifyTree {
	my ($self, $rfamdb, $tree,$taxtree) = @_;
	my %seenSpecies;
	my %accToSpecies;

#Get sequence identifiers from the seed and query the database for the species (align_display_name)	

	my $sth = $rfamdb->prepare_seqaccToTaxon;
	for( my $i = 0; $i < $self->nseq; $i++){
		 my $nse = $self->get_sqname($i);

	my ($is_nse, $name, $start, $end) = Bio::Rfam::Utils::nse_breakdown($nse);
	if(! $is_nse) { die "ERROR $nse not in name/start-end format"; }
	
	$sth->execute($name);
	my $row = $sth->fetchrow_hashref;
	if(!exists($seenSpecies{$row->{align_display_name}})){
		$seenSpecies{$row->{align_display_name}} = 1;
	}
	my $speciesName = $row->{align_display_name}.'.'.$seenSpecies{$row->{align_display_name}};
	
	$accToSpecies{$name} = $speciesName;
	$seenSpecies{$row->{align_display_name}}++;
	}

#Now get the original tree and replace all the accessions with the species:

	open(TREE, "$tree") or croak ("Can't open tree file to convert seqs to species...this is not good!\n");
	my @ar =<TREE>;
	my $t = join("", @ar);
	foreach my $k (keys %accToSpecies){
        $t =~ s/$k/$accToSpecies{$k}/g;
    }
	open(TAXTREE, ">>$taxtree");
	print TAXTREE "$t\n";
}



1;
1;




































