package Bio::Rfam::View::Plugin::PDB;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

use Data::Printer;
use Carp;
use File::Temp;
sub process{
	my $self = shift;
	$self->mapPDB;
}
#

sub mapPDB {

	my ($self) = @_;	
	#Sorry Rob, a hardcoded path. Need to put this someplace more permanent.
	#This file is the fasta file of all the non-protein nucleic acid sequences in the PDB.
	#Those with modified bases have been excluded.
	#Documentation on Confluence as to how to produce this fasta file.
	#
	my $PDB_fasta = '/nfs/production/xfam/rfam/RELEASE_FILES/RFAM_12/PDB_RFAM12.fa';
	#my $PDB_fasta = '/nfs/production/xfam/rfam/CURATION/PDB/pdb_trimmed_noillegals.fa';	
	my $config = $self->parent->config;
	# my $client = Bio::Rfam::SVN::Client->new({config => $config});
	my $familyIO = Bio::Rfam::FamilyIO->new;	
	my $familyObj = $self->parent->family;
	my $rfam_acc = $familyObj->DESC->AC;
	my $prefix = "$rfam_acc.XXXX";
        my @hex_colour = qw(1fc01f c00f0f bdc000 c008ae 00bac0 8484c0 93c090 c0af92 8e2511 f29242 8585e6 ff87fa 008700 454545 0003c0 ebeb30 ff87a4 0064f4);


	# Generate a bunch of temporary files for the cmscan job:
	#
	my $out = File::Temp->new(TEMPLATE => $prefix,
							 DIR => '/tmp',
							 SUFFIX => '.out');
	my $tblout = File::Temp->new(TEMPLATE => $prefix,
								DIR => '/tmp',
								SUFFIX => '.tblout');

	my $logfile =  File::Temp->new(TEMPLATE => $prefix,
									DIR => '/tmp',
									SUFFIX => '.log');

	my $rfamdb = $self->parent->config->rfamlive;

	# File to put the CM which we extract from the SVN:
	#
	my $cm_file = "/tmp/$prefix.CM";
	my $cm = $familyObj->CM;

    #$familyIO->writeAnnotatedCM($cm, $familyObj,$cm_file,0);
    $familyIO->writeAnnotatedCM( $familyObj,$cm_file,0);
    
	#$familyIO->writeCM($cm, $cm_file);
	
	# cmpress the CM and run the cmscan job:
	#
	my $cmpress = $config->infernalPath . "cmpress -F $cm_file";
        my $cmscan  = $config->infernalPath . 'cmscan';

	system ($cmpress) == 0
          or die 'ERROR: failed to run cmpress';

	my $cmscan_cmd = "$cmscan -o  $out --tblout $tblout --cut_ga $cm_file $PDB_fasta >>$logfile";
	system ( $cmscan_cmd) == 0
          or die 'ERROR: failed to run cmscan';

 	unlink glob "/tmp/$prefix.CM*";	
	#Array to store our PDB matches in:
	my @pdbs;

 	open (CMSCAN,"<$tblout") or die "Can't open CMscan results\n";
	while (<CMSCAN>) {
		my $line = $_;
		chomp $line;
		next if ($line =~ /^#/);
		my @result = split (/\s+/, $line);
		my ($pdb_id, $chain) = split(/_/, $result[2]);
                my $colour = $hex_colour[rand @hex_colour];
		push @pdbs, {rfam_acc => $rfam_acc,
					pdb_id =>$pdb_id,
					chain => $chain,
					pdb_start => $result[7],
					pdb_end => $result[8],
					bit_score => $result[14],
					evalue_score => $result[15],
					cm_start => $result[5],
					cm_end => $result[6],
                                        hex_colour => $colour };
	} 
	my $resultset = $rfamdb->resultset('PdbFullRegion')->search({rfam_acc => $rfam_acc});
 	
	#Delete all existing matches so we don't have duplicates:
	#
	$resultset->delete_all;
	
	#Load new PDB hits into DB:	
	for my $hit (@pdbs) {
	        p $hit;
        	$resultset->create( {rfam_acc =>$hit->{rfam_acc},
							pdb_id => $hit->{pdb_id},
							chain => $hit->{chain},
							pdb_start => $hit->{pdb_start},
							pdb_end => $hit->{pdb_end},
							bit_score => $hit->{bit_score},
							evalue_score => $hit->{evalue_score},
							cm_start => $hit->{cm_start},
							cm_end => $hit->{cm_end},
                                                        hex_colour => $hit->{hex_colour}
							});
	}

} 
	
	
