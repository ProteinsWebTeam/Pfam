#
# Try and bring all of the structure markup methods into
# a single module
#


=head1 NAME

Pfam2Structure

=head1 = SYNOPSIS

Try and bring all of the structure markup methods into a single module.

=cut

package Bio::Pfam::Structure::Pfam2Structure;

BEGIN{
    if ($ENV{'HTTP_X_FORWARDED_HOST'}){
	require SangerPaths; import SangerPaths qw(pubseq);
	require SRS;
    }
}

use strict;
use Bio::Pfam;
use Bio::Graphics;
use Bio::Pfam::Web::PfamWWWConfig;

use GD;
use Log::Log4perl qw(get_logger);
use vars qw($logger);
BEGIN {
    $logger = get_logger(__PACKAGE__);
}

=head2 map_PfamA_2_PDB

 Title    : map_PfamA_2_PDB
 Usage    : &map_PfamA_2_PDB($rdb, $pdb_id, $coos_format);
 Function : Takes an RDB ro object and gets the sws_acc, domain names/s-e for the given pdb id.
 Returns  : hash pdb_id, chain, seq_id, domain_index, "domain_acc/start_end"
 Args     : rdb handle, pdb ID

=cut

sub map_PfamA_2_PDB {
	my ($rdb, $pdb, $coos_format) = @_;
	die "Your pdb identifier, [$pdb] is incorrect, expecting in the form 2abl\n" if ($pdb !~ /\S{4}/);
	my $seq_number;
	if ($coos_format =~ /PDB/i){
		$seq_number = "pdb_seq_number";
	}
	else{
		$seq_number = "pfamseq_seq_number";
	}

	# Work out the sequences in the structure.
	my @seqs = $rdb->query("select distinct chain, pfamseq_acc from msd_data join pdb, pfamseq where pdb.auto_pdb=msd_data.auto_pdb and msd_data.auto_pfamseq=pfamseq.auto_pfamseq and pdb_id=\"$pdb\"");

	my %mapping;
	# Work out the domains in each sequence. 
	
	my $domain_index = 0;
	foreach my $seq (@seqs) {
		my @domains=&pfamA_on_sequence($rdb, $$seq[1]);
		for my $domain (@domains) {
			$$seq[0] = "_" if ($$seq[0] !~ /\S+/); #gets rid of null chains
			my ($acc, $start, $end);
			if($domain =~ /(PF\d+)\/(\d+)-(\d+)/){
				$acc = $1;
				$start = $2;
				$end = $3;
			}
			else{
				die "Bad domain definition!!";
			}
			# Now work out the region of the domains that are ordered in the structure.

			
			my @ordered;
			if ($$seq[0] eq "_"){
				@ordered = $rdb->query("select $seq_number from msd_data join pdb, pfamseq where msd_data.auto_pdb=pdb.auto_pdb and pfamseq.auto_pfamseq=msd_data.auto_pfamseq and pfamseq_acc=\"$$seq[1]\" and pdb_id =\"$pdb\" and pfamseq_seq_number>=$start and pfamseq_seq_number<=$end "); 
			}
			else {
				@ordered = $rdb->query("select $seq_number from msd_data join pdb, pfamseq where msd_data.auto_pdb=pdb.auto_pdb and pfamseq.auto_pfamseq=msd_data.auto_pfamseq and pfamseq_acc=\"$$seq[1]\" and pdb_id =\"$pdb\" and pfamseq_seq_number>=$start and pfamseq_seq_number<=$end and chain=\"$$seq[0]\"");
			}
			if($#ordered == -1) {
				#None of the domain is represented
				next;
			}
			elsif($#ordered == ($end - $start + 1)){
				#Whole of the domain is ordered
				$mapping{$pdb}{$$seq[0]}{$$seq[1]}{$domain_index}{$domain}++;
			}
			else{
				# there are discrepancies, need to walk through the domain to find the breaks
				my $s = ${$ordered[0]}[0];
				my $e = ${$ordered[0]}[0];
				for(my $n = 1; $n <= $#ordered; $n++){
					if (${$ordered[$n]}[0] != ($e + 1)){
						$mapping{$pdb}{$$seq[0]}{$$seq[1]}{$domain_index}{"$acc/$s-$e"}++;
						$s=$e=${ordered[$n]}[0];
					}
					else {
						$e = ${$ordered[$n]}[0];
					}
				}
				$mapping{$pdb}{$$seq[0]}{$$seq[1]}{$domain_index}{"$acc/$s-$e"}++;
			}
			$domain_index++;
		}
	}
	return %mapping;
}

=head2 map_PDB_2_PfamA

 Title    : map_PDB_2_PfamA
 Usage    : &map_PDB_2_PfamA($rdb, $pfamA);
 Function : Takes an RDB ro object and gets all pdbs with contain structures of that id.
 Returns  : hash containg pdb_id, chain, seq_id, domain_index, "domain/start_end"
 Args     : rdb handle, pfam accession or id

=cut

sub map_PDB_2_PfamA{

}





=head2 assign_colours

 Title    : assign_colours
 Usage    : &assign_colours(%map_hash)
 Function : Takes a map produced from either "map" method
 Returns  : hash pdb_id, chain, seq_id, "domain/start_end"
 Args     : rdb handle, pdb ID

=cut

sub assign_colours {
	my %map_hash = @_;
	my %acc_seen;
	my $colour_counter = 1;
	foreach my $pdb (sort{$a cmp $b}keys %map_hash){
		foreach my $chain(sort{$a cmp $b}keys %{$map_hash{$pdb}}){
			foreach my $seq_id (sort{$a cmp $b}keys %{$map_hash{$pdb}{$chain}}){
				foreach my $index (sort{$a <=> $b}keys %{$map_hash{$pdb}{$chain}{$seq_id}}){
					foreach my $dse (keys %{$map_hash{$pdb}{$chain}{$seq_id}{$index}}){
						my $acc;
						if($dse =~/(PF\d+)\/\d+-\d+/){
							$acc = $1;
						}
						if (!$acc_seen{$acc}){
							$acc_seen{$acc} = $colour_counter;
							$colour_counter++;
						}
						$map_hash{$pdb}{$chain}{$seq_id}{$index}{$dse} = $acc_seen{$acc};
					}
				}
			}
		}
	}
	return (%map_hash);
}

=head2 pfamA_on_sequence

 Title    : pfamA_on_sequence
 Usage    : &pfamA_on_sequence($rdb, $seq_acc);
 Function : Lists all of the pfamAs on the give sequence.
 Returns  : hash containing all the domain/start-end
 Args     : rdb handle, sequence_acc

=cut

sub pfamA_on_sequence {
	my ($rdb, $seq_acc) =@_;
	my @tmp_domains = $rdb->query("select distinct pfamA_acc, seq_start, seq_end from pfamA_reg_full join pfamA, pfamseq where pfamseq.auto_pfamseq=pfamA_reg_full.auto_pfamseq and pfamA_reg_full.auto_pfamA=pfamA.auto_pfamA and significant =1 and in_full=1 and pfamseq_acc =\"$seq_acc\"");
	my @domains;
	# order the domains and put in the format of domain/start-end
	foreach my $dom (sort{$$a[1] <=> $$b[1]}@tmp_domains){
		push(@domains, $$dom[0]."/".$$dom[1]."-".$$dom[2]);
	}
	return @domains;
}


=head2 get_interactions

 Title    : get_interactions
 Usage    : &get_interactions($rdb, $pdb_id, $chain, $dse)
 Function : Takes an RDB ro object and performs a series of RDB queries that gives chain .
 Returns  : hash pdb_id, chain, seq_id, "domain/start_end"
 Args     : rdb handle, pdb ID

=cut

sub get_interactions {
	my ($rdb, $pdb_id, $chain, $dse, $coos_format, $out_coos_format) = @_;
	my ($start, $end, $seq_number);
	# Convert null chain from _ to  ;
	
	$chain = "" if ($chain eq "_");
	if($dse =~ /\S+\/(\d+)-(\d+)/){
		$start = $1;
		$end = $2;
	}

	# set up which column we are going to query on
	
	if ($coos_format =~/PDB/i){
		$seq_number="pdb_seq_number_A";
	}
	else {
		$seq_number="pfamseq_seq_number_A";
	}

	#Use the new interaction table
	#print STDERR "$start $end\n";
	my @int_res = $rdb->query("select distinct chain_B, pdb_seq_number_B, pfamseq_seq_number_B from interactions join pdb where pdb.auto_pdb=interactions.auto_pdb  and $seq_number>=$start and $seq_number<=$end and pdb_id=\"$pdb_id\" and chain_A=\"$chain\"");
	
    #Put all of these entries into hash from the array of array_refs.
	my %ints_coos;
	if($out_coos_format =~/PDB/i){ 
		foreach my $i (@int_res){
			$$i[0] = "_" if (!$$i[0]);
			push(@{$ints_coos{$$i[0]}}, $$i[1]);
		}
	}
	else{
		foreach my $i (@int_res){
			$$i[0] = "_" if (!$$i[0]);
			push(@{$ints_coos{$$i[0]}}, $$i[2]);
		}

	}

	return %ints_coos;
}

=head2 get_interaction_pairs

 Title    : get_interaction_pair
 Usage    : &get_interactions($rdb, $pdb_id, $chain, $dse)
 Function : Takes an RDB ro object and performs a series of RDB queries that gives chain .
 Returns  : hash pdb_id, chain, seq_id, "domain/start_end"
 Args     : rdb handle, pdb ID

=cut

sub get_interaction_pairs {
	my ($rdb, $pdb_id, $chain1, $dse1, $chain2, $dse2, $coos_format, $out_coos_format) = @_;
	my ($start1, $end1, $seq_number_A, $seq_number_B , $start2, $end2, $coos_A, $coos_B);
	# Convert null chain from _ to  ;
	
	$chain1 = "" if ($chain1 eq "_");
	$chain2 = "" if ($chain2 eq "_");

	if($dse1 =~ /\S+\/(\d+)-(\d+)/){
		$start1 = $1;
		$end1 = $2;
	}
	if($dse2 =~ /\S+\/(\d+)-(\d+)/){
		$start2 = $1;
		$end2 = $2;
	}
	
	# set up which column we are going to query on
	if ($coos_format =~/PDB/i){
		$seq_number_A="pdb_seq_number_A";
		$seq_number_B="pdb_seq_number_B";

	}
	else {
		$seq_number_A="pfamseq_seq_number_A";
		$seq_number_B="pfamseq_seq_number_B";
	}

	if ($out_coos_format =~/PDB/i){
		$coos_A = "pdb_seq_number_A";
		$coos_B = "pdb_seq_number_B";
	}
	else{
		$coos_A = "pfamseq_seq_number_A";
		$coos_B = "pfamseq_seq_number_B";
	}
	#print "select distinct chain_A, $coos_A, chain_B, $coos_B from interaction join pdb where pdb.auto_pdb=interaction.auto_pdb and $seq_number_A>=$start1 and $seq_number_A<=$end1 and pdb_id=\"$pdb_id\" and chain_A=\"$chain1\" and $seq_number_B>=$start2 and $seq_number_B<=$end2 and chain_B=\"$chain2\"\n";

	# Use the new interaction table
	my @int_res = $rdb->query("select distinct chain_A, $coos_A, chain_B, $coos_B from interactions join pdb where pdb.auto_pdb=interactions.auto_pdb and $seq_number_A>=$start1 and $seq_number_A<=$end1 and pdb_id=\"$pdb_id\" and chain_A=\"$chain1\" and $seq_number_B>=$start2 and $seq_number_B<=$end2 and chain_B=\"$chain2\"");

	my %ints_coos;
	foreach my $i (@int_res){
		push(@{$ints_coos{$$i[1]}}, $$i[3]);
	}
	return %ints_coos;
}


=head2 write_domain_int_rasmol_script

 Title    : write_domain_int_rasmol_script 
 Usage    : &write_domain_int_rasmol_script($rdb, $pdb_id, $style, $detail)
 Function : The is basically a wrapper for a load of internal methods that write the
			different parts of a rasmol script. However, it does contain the part 
			adds the interacting residues 
 Returns  : Script
 Args     : pfam ro rdb handle
			pdb identifier to be marked-up
			Any rasmol style, spacefill or sticks
			detail, 0/1 to whether connecting lines are added for the interacting atoms.
=cut

sub write_domain_int_rasmol_script {
	my ($rdb, $pdb_id, $style, $detail, $filename) = @_;
	#  Writing a rasmol script involves four basic parts.
	#
	#  1. Marking up the structure with pfam domains
	#  2. Displaying the interaction
	#  3. Finishing off the setting
	#  4. Appending the pdb file to the script.
	#
	# Points 1,3 and 4 are generic, 2 is specific
	
	#Point 1.
	my $rms = &_rasmol_pfam($rdb, $pdb_id);
	
	#Point 2 - specific to this method
	my @interactions = $rdb->query("select auto_int_atoms, chain_A, pdb_seq_number_A from interactions join pdb where interactions.auto_pdb=pdb.auto_pdb and  pdb_id=\"$pdb_id\"");
								
	if ($style){
	# Now transform into a hash.
	my (%int_residues, @autos);
	foreach my $int (@interactions){
		$$int[1] = "_" if (!$$int[1]);
		push(@autos, $$int[0]);
		push(@{$int_residues{$$int[1]}}, $$int[2]);
	}
	
	# Find continous regions to mark up
	foreach my $c (keys %int_residues){
		#now look for continous regions
		my ($start, $end, $c_tmp);
		if($c eq "_") {
			$c_tmp = "";
		}
		else{
			$c_tmp = $c;
		}
		my ($previous_res, $start_res);
		foreach my $res (sort{$a <=> $b}@{$int_residues{$c}}){
			if (!$start_res) {
				$start_res = $res;
			}elsif( $res == $previous_res){
				next;
			}elsif($res == ($previous_res + 1)){
				;
			}else{
				$rms .= "select (*$c_tmp && $start_res-$previous_res)\n$style\n";
				$start_res = $res;
			}
			$previous_res = $res;
		}
		$rms .= "select (*$c_tmp && $start_res-$previous_res)\n$style\n";
	}
	}

	# If we go for detail, need all of the atom data.
	#if ($detail){
	#	my $query = join(" or auto_int_atoms=", @autos);
	#	my @atoms = $rdb->query("select pdb_atom, partner_pdb_atom from interaction_atoms where (auto_int_atoms=$query)");
	#	foreach my $atom_pair ( @atoms){
	#		print $rms "monitor $$atom_pair[0] $$atom_pair[1]\n";
	#	}
	#}
	
	#Points 3 and 4.
	my $script = _rasmol_pdb_append($rms, $pdb_id, $filename);
	return $script;
}



=head2 write_specific_domain_int_rasmol_script

 Title    : write_specific_domain_int_rasmol_script 
 Usage    : &write_specific_domain_int_rasmol_script($rdb, $pdb_id, $int, $style, $detail)
 Function : The is basically a wrapper for a load of internal methods that write the
			different parts of a rasmol script. However, it does contain the part 
			adds the interacting residues that are specific to the supplied interaction
 Returns  : Script
 Args     : pfam ro rdb handle
			pdb identifier to be marked-up
			Any rasmol style, spacefill or sticks
			detail, 0/1 to whether connecting lines are added for the interacting atoms.
=cut

sub write_specific_domain_int_rasmol_script {
	my ($rdb, $pdb_id, $int, $style, $detail, $filename) = @_;
	#  Writing a rasmol script involves four basic parts.
	#
	#  1. Marking up the structure with pfam domains
	#  2. Displaying the interaction
	#  3. Finishing off the setting
	#  4. Appending the pdb file to the script.
	#
	# Points 1,3 and 4 are generic, 2 is specific
	
	#Point 1.
	my $rms = &_rasmol_pfam($rdb, $pdb_id);
	
	#Point 2 - specific to this method
	#int will be a string similar to P01892/1hhg:A/MHC_I/25-203~P04582/1hhg:C/GP120/192-200
	#Assuming these are sequence coos.

	my ($intA, $intB) = split(/~/, $int);
	my @intA = split(/\//, $intA);
	my @intB = split(/\//, $intB);
	my ($aStart, $aEnd) = split(/-/, $intA[3]);
	my ($aPdb, $aChain) = split(/:/, $intA[1]);
	my ($bStart, $bEnd) = split(/-/, $intB[3]);
	my ($bPdb, $bChain) = split(/:/, $intB[1]);
	
	my @interactions = $rdb->query("select chain_A, pdb_seq_number_A, chain_B, pdb_seq_number_B from interactions join pdb where interactions.auto_pdb=pdb.auto_pdb and  pdb_id=\"$pdb_id\" and pfamseq_seq_number_A>=$aStart and pfamseq_seq_number_B>=$bStart and pfamseq_seq_number_A<=$aEnd and pfamseq_seq_number_B<=$bEnd and chain_A=\"$aChain\" and chain_B=\"$bChain\"");
	$logger->debug("select chain_A, pdb_seq_number_A, chain_B, pdb_seq_number_B from interactions join pdb where interactions.auto_pdb=pdb.auto_pdb and  pdb_id=\"$pdb_id\" and pfamseq_seq_number_A>=$aStart and pfamseq_seq_number_B>=$bStart and pfamseq_seq_number_A<=$aEnd and pfamseq_seq_number_B<=$bEnd and chain_A=\"$aChain\" and chain_B=\"$bChain\"");	
	if ($style){
	# Now transform into a hash.
	my (%int_residues, @autos);
		foreach my $int (@interactions){
		$$int[0] = "_" if (!$$int[0]);
		$$int[2] = "_" if (!$$int[2]);
		push(@{$int_residues{$$int[0]}}, $$int[1]);
		push(@{$int_residues{$$int[2]}}, $$int[3]);
		}
	
	# Find continous regions to mark up
	foreach my $c (keys %int_residues){
		#now look for continous regions
		my ($start, $end, $c_tmp);
		if($c eq "_") {
			$c_tmp = "";
		}
		else{
			$c_tmp = $c;
		}
		my ($previous_res, $start_res);
		foreach my $res (sort{$a <=> $b}@{$int_residues{$c}}){
			if (!$start_res) {
				$start_res = $res;
			}elsif( $res == $previous_res){
				next;
			}elsif($res == ($previous_res + 1)){
				;
			}else{
				$rms .= "select (*$c_tmp && $start_res-$previous_res)\n$style\n";
				$start_res = $res;
			}
			$previous_res = $res;
		}
		$rms .= "select (*$c_tmp && $start_res-$previous_res)\n$style\n";
	}
	}

	#Points 3 and 4.
	my $script = _rasmol_pdb_append($rms, $pdb_id, $filename);
	return $script;
}














=head2 _rasmol_pfam

 Title    : _rasmol_pfam 
 Usage    : _rasmol_pfam($rdb, $pdb_id)
 Function : 
 Returns  : 
 Args     : 

=cut


sub _rasmol_pfam{
	my ($rdb, $pdb_id) = @_;
	# Get all of the pfam domains for the structure.
	my %mapping = &map_PfamA_2_PDB($rdb, $pdb_id, "PDB");	
	# Assign colours to the domains;
	my %mapping_col = &assign_colours(%mapping);
	my %colours  = &get_colour_hash();
	# Right, this map should contain everything that we need to generate the structure marked up with pfams.
	
my $rms =
"\nload pdb inline 
set ambient 40
set specular on
set specpower 8

reset
slab off
rotate x 0 
rotate y 0 
rotate z 0 

set axes off
set boundingbox off
set unitcell off
set bondmode and
dots off

# Avoid Colour Problems!
select all
colour bonds none
colour backbone none
colour hbonds none
colour ssbonds none
colour ribbons none
colour white
backbone 100
wireframe off\n ";


#Now do the mapping bit.
	foreach my $p (keys %mapping_col){
		foreach my $c (sort{$a cmp $b}keys %{$mapping_col{$p}}){
			foreach my $s (sort{$a cmp $b}keys %{$mapping_col{$p}{$c}}){
				foreach my $i (sort{$a <=> $b}keys %{$mapping_col{$p}{$c}{$s}}){
					foreach my $dse (keys %{$mapping_col{$p}{$c}{$s}{$i}}){
						if($dse =~/(PF\d+)\/(\d+)-(\d+)/){
							my $acc = $1;
							my $start = $2;
							my $end = $3;
							my $chain;
							if ($c eq "_"){
								$chain = "";
							}
							else{
								$chain = $c;
							}
							$rms .= "select (*".$chain." && ".$start."-".$end.")\n";
							$rms .= "colour [".$colours{$mapping_col{$p}{$c}{$s}{$i}{$dse}}."]\n";
						}
					}
				}
			}
		}
	}
	return $rms;
}

=head2
 
 Title    : _rasmol_pdb_append 
 Usage    : &_rasmol_pdb_append(\*filehandle, $pdb_id, $filename)
 Function : This method finishes of a rasmol script and appends the pdb file using getz
 Returns  : Nothing
 Args     : Filehandle that is currently being used to write the rasmol script, 
            the pdb id and the name of the file. 

=cut

sub _rasmol_pdb_append{
	my ($rms, $pdb_id, $filename) = @_;

#Atoms
$rms.= "
set shadow off

# Bonds
#select all
#wireframe off

# Ribbons
ribbons off

# Backbone

# Labels
labels off

# Monitors
set monitors off

#ssbonds off
#hbonds off
exit\n";

	if (-e "/usr/local/ensembl/bin/getz"){
	    open(PDB, "/usr/local/ensembl/bin/getz -view PDBdata \"[pdb:$pdb_id]\" |");
	    while(<PDB>){
		$rms.=$_;
	    }
	    
	}
	elsif(-e "/usr/local/pubseq/bin/getz"){
	    open(PDB, "/usr/local/pubseq/bin/getz -view PDBdata \"[pdb:$pdb_id]\" |");
	    while(<PDB>){
		$rms.=$_;
	    }
	}else{
	    $rms .= SRS::getz("-view", "PDBdata", "[PDB:$pdb_id]");
	}
	
	if ($filename){
		open(RMS, ">$filename");
		print RMS "$rms\n";
		close(RMS);
	}
	return $rms;
} 

=head2 write_plain_rasmol_script

 Title    : write_plain_rasmol_script 
 Usage    : &write_plain_rasmol_script($rdb, $pdb_id, $style, $detail)
 Function : The is basically a wrapper for a load of internal methods that write the
			different parts of a rasmol script. However, it does contain the part 
			adds the interacting residues 
 Returns  : Nothing
 Args     : pfam ro rdb handle
			pdb identifier to be marked-up
			Any rasmol style, spacefill or sticks
			detail, 0/1 to whether connecting lines are added for the interacting atoms.
=cut

sub write_plain_rasmol_script {
	my ($rdb, $pdb_id, $filename) = @_;
	#  Writing a rasmol script involves four basic parts.
	#
	#  1. Marking up the structure with pfam domains
	#  3. Finishing off the setting
	#  4. Appending the pdb file to the script.
	#
	# Points 1,3 and 4 are generic, 2 is specific
	
	#Point 1.
	my $rms = &_rasmol_pfam($rdb, $pdb_id);
	
	#Points 3 and 4.
	$rms = _rasmol_pdb_append($rms, $pdb_id, $filename);
	return ($rms);	
}






#####################
# Colouring Section #
#####################

sub get_colour_hash{
	#This path needs to be changed!
    eval{

	open(HEX_COLOUR, "/pfam/db/Pfam/scripts/structure/hex_colours") || die "Could not open hex_colours file...:[$!]\n";
	
    };
    $logger->debug("LOOKING FOR HEX");
    if($@){
	eval{
	    open(HEX_COLOUR, "$Bio::Pfam::Web::PfamWWWConfig::file_root/data/hex_colours") || die "Could not open $Bio::Pfam::Web::PfamWWWConfig::file_root/data/hex_colours :[$!]\n";
	};
	if($@){
	    $logger->error("Can not find hex colours anywhere!!!!!: [$@]");
	}
    }
    my %colour;
    while(<HEX_COLOUR>){
	chomp;
	my ($index, $hex, $rgb) = split(/~/, $_);
	$colour{$index}=$rgb
	}
    close(HEX_COLOUR) || die "could not close hex_colours file:[$!]\n";
    return %colour;
}

sub get_colour_hash_triples{
	#This path needs to be changed!
	#open(TRI_COLOUR, "/nfs/team71/pfam/rdf/scripts/hex_colours") || die "Could not open hex_colours file:[$!]\n";
	
	eval{
	    open(TRI_COLOUR, "/pfam/db/Pfam/scripts/structure/hex_colours") || die "Could not open hex_colours file locally...:[$!]\n";
	};
	$logger->debug("LOOKING FOR HEX");
	if($@){
	    eval{
		open(TRI_COLOUR, "$Bio::Pfam::Web::PfamWWWConfig::file_root/data/hex_colours") || die "Could not open $Bio::Pfam::Web::PfamWWWConfig::file_root/data/hex_colours :[$!]\n";
	    };
	    if($@){
		$logger->error("Can not find hex colours anywhere!!!!!: [$@]");
	    }
	}
	my %colour;
	while(<TRI_COLOUR>){
		chomp;
		my ($index, $hex, $rgb) = split(/~/, $_);
		$colour{$index}="#$hex";
	}
	close(TRI_COLOUR) || die "could not close hex_colours file:[$!]\n";
	return %colour;
}



sub RGBtoHSV {
	my $rgb = shift;
	my ($r,$g,$bl)= split(/,/, $rgb);
	$r = $r/255;
  	$g = $g/255;
	$bl = $bl/255;
	
	my ($h,$s,$v,$min,$max,$delta);
	my @rgb = ($r,$g,$bl);
	#my ($x,$y);
	my @list = sort{$a<=>$b}@rgb;
	$max = $list[2];
	$min = $list[0];
    $v = $max;                              
    $delta = $max - $min;

	if( $delta == 0 ) {
		$s = 0;
		$h = 0;
		return($h,$s,$v);
	}
	else{
	
		$s = $delta/$v;
		
		my $del_R = ((($max-$r)/6)+($delta/2))/$delta;
		my $del_G = ((($max-$g)/6)+($delta/2))/$delta;
		my $del_B = ((($max-$bl)/6)+($delta/2))/$delta;

		if( $r == $max ) {
			$h = $del_B - $del_G; 
		} 
		elsif( $g == $max ) {
			$h = 1/3 + $del_R - $del_B;
		} 
		else {
			$h = 2/3 + $del_G - $del_R;
		}
   	
		if ($h < 0){
			$h = $h + 1;
		}
		elsif ($h > 1){
			$h = $h - 1;
		}
		return($h,$s,$v);
	}
}


sub make_thumbnails {
	my ($rdb, $pdb_id, $map, $image_path) = @_;
	my $scale = 1;
	my %mapping_col;
	if(!$map){
		my %pfamA_mapping = &map_PfamA_2_PDB($rdb, $pdb_id, 1,1);
		%mapping_col = &assign_colours(%pfamA_mapping);
	}else{
		%mapping_col = %{$map};
	}
	my ($max_seq, @seq_objects) = &make_sequence_objects($rdb, $pdb_id,0,0, %mapping_col);	
	my %images = &draw_interactions($rdb, $pdb_id, $scale, $image_path, 0, 0, $max_seq, @seq_objects);
	return %images;
}

sub make_specific_image {
	my ($rdb, $pdb_id, $map, $image_path, $seq1, $seq2, $scale) = @_;
	my %mapping_col;
	if(!$map){
		my %pfamA_mapping = &map_PfamA_2_PDB($rdb, $pdb_id, 1,1);
		%mapping_col = &assign_colours(%pfamA_mapping);
	}else{
		%mapping_col = %{$map};
	}
	my ($max_seq, @seq_objects) = &make_sequence_objects($rdb, $pdb_id, $seq1, $seq2, %mapping_col);	
	my %images = &draw_interactions($rdb, $pdb_id, $scale, $image_path, 0, 0, $max_seq, @seq_objects);
	return %images;
}

sub make_sequence_objects {
	my ($rdb, $pdb_id, $seq1, $seq2, %mapping_col) = @_;
	my @tmp = keys %mapping_col;
	my ($max_seq, @pfamA_seq_objects);
	#Get the mapping for all of the structure.
	foreach my $p (keys %mapping_col){
		foreach my $c (sort{$a cmp $b}keys %{$mapping_col{$p}}){
			foreach my $s (sort{$a cmp $b}keys %{$mapping_col{$p}{$c}}){
				my $colour;
			INT:
			foreach my $i (sort{$a <=> $b}keys %{$mapping_col{$p}{$c}{$s}}){
				#As the fragments that make up the domain could
				#come in any order, the ultimate start/end need
				#to be searched for.
				my ($acc, $start, $end);
				$start = 100000; # Never get a protein sequence this long! 
				$end = 0;
					
				foreach my $dse (keys %{$mapping_col{$p}{$c}{$s}{$i}}){
					if($dse =~/(PF\d+)\/(\d+)-(\d+)/){
						$acc = $1;
						my $tmp_start = $2;
						my $tmp_end = $3;
						$start = $tmp_start if ($tmp_start < $start); 
						$end = $tmp_end if ($tmp_end > $end);
					 }
				$colour = $mapping_col{$p}{$c}{$s}{$i}{$dse};
				}
				# add all or only specific seq_objects
				my ($entry, $id);
				eval{
					$entry = $rdb->get_EntryA_by_acc( $acc );
					$id = $entry->id();
				};
				if($seq1 && $seq2){
					next INT if(($seq1 ne "$s/$pdb_id:$c/$id/$start-$end") and ($seq2 ne "$s/$pdb_id:$c/$id/$start-$end"));
				}
				# If we get here, make sequence object for each of these domians;
				my @sequence = SRS::getz("-f", "seq", "[PFAMSEQ-acc:$s]");
				my $sequence;
				foreach(@sequence){
				    chomp;
				    if(/SQ\s+Sequence/){
					next;
				    }elsif(/\/\//){
					next;
				    }elsif(/\S+/){
					$_ =~ s/\s+//g;
					$sequence .= $_;
				    }
				}
				
				my $seq = substr($sequence, $start, $end-$start+1);
			        my $seqobj = Bio::Pfam::SeqPfam->new(-id => "$s/$pdb_id:$c/$id/$start-$end",
								     -seq => $seq,
								     -start => $start,
								     -end => $end,
								     -desc => $colour,);
				
				if (!$max_seq){
				    $max_seq = $seqobj;
				}elsif(length($max_seq->seq) < length($seqobj->seq)){ 
				    $max_seq = $seqobj;
				}
				push(@pfamA_seq_objects, $seqobj); 
			    }
			    }
		}
	}
	return ($max_seq, @pfamA_seq_objects);
}


sub draw_interactions {
	my($rdb, $pdb_id, $scale, $image_path, $seq1_offset, $seq2_offset, $max_seq, @pfamA_seq_objects) = @_;
	my %colours  = &get_colour_hash_triples();
	my $max_offset = 0;
	my %images;
	## Only offset one sequence
	if ($seq1_offset == $seq2_offset){
		$seq1_offset = 0;
		$seq2_offset = 0;
	}
	elsif($seq1_offset > $seq2_offset){
		$seq1_offset = $seq1_offset - $seq2_offset;
		$seq2_offset = 0;
		$max_offset = $seq1_offset;
	}
	else{
		$seq2_offset = $seq2_offset - $seq1_offset;
		$seq1_offset = 0;
		$max_offset = $seq2_offset;
	}
	
	my $seq_coos = &normalise_max($max_seq, $max_offset);
	
	my $width;
	if ($scale < 1) {
		$scale = 1;
	}elsif ($scale > 6){
		$scale = 6;
	}

	if ($scale == 1){
		$width = length($seq_coos->seq) * 1;
	}elsif ($scale == 2){
		$width = length($seq_coos->seq) * 3;
	}elsif ($scale == 3){
		$width = length($seq_coos->seq) * 7;
	}elsif ($scale == 4){
		$width = length($seq_coos->seq) * 9;
	}else{
		$width = length($seq_coos->seq) * 25;
	}

	my %text_size = ( 1 => \&GD::gdSmallFont,
			   2 => \&GD::gdSmallFont,
			   3 => \&GD::gdSmallFont,
			   4 => \&GD::gdLargeFont,
			   5 => \&GD::gdGiantFont,
			   6 => \&GD::gdGiantFont);
	#Now go through each domain start-end and get the interacting residues.  However, unlike the previous
	#getacts, I need both the interacting residues.  For each of these residues, I need to make a segment
	#and add it to the track.
	for(my $s1 = 0; $s1 < $#pfamA_seq_objects; $s1++){
		my $seq1 = &normalise($pfamA_seq_objects[$s1], $seq1_offset);
		for(my $s2 = ($s1 + 1); $s2 <= $#pfamA_seq_objects; $s2++){
			
			my $panel = Bio::Graphics::Panel->new(
        			-segment   => $seq_coos,
					-key_style => 'between',
					-width     => $width,
					-bgcolour  => '#FFFFCC',
					-grid		 => 1,
					-gridcolor => '#CCCCCC',
					-spacing   => '2',
					);
		
			my @seq1_info=split(/\//, $seq1->id);
			my $chain1 = $1 if ($seq1_info[1] =~ /\S+:(\S+)/);
			my @seq2_info=split(/\//, $pfamA_seq_objects[$s2]->id);
			my $chain2 = $1 if ($seq2_info[1] =~ /\S+:(\S+)/);
			my %interaction = &get_interaction_pairs($rdb, $pdb_id, $chain1, $seq1_info[2]."/".$seq1_info[3],$chain2,$seq2_info[2]."/".$seq2_info[3], "SEQ", "SEQ");   

			if(%interaction){
			my $seq2 = &normalise($pfamA_seq_objects[$s2], $seq2_offset);
			my $bit1 = $seq1->id;
			my $bit2 = $seq2->id;
			$bit1 =~ s/\//~/g;
			$bit2 =~ s/\//~/g;
			#make a seqment for each interaction.  These need to be normalised.
			open(TXT, ">$image_path/$pdb_id.$bit1~$bit2.txt") || die;
			print TXT "SEQUENCE1\tRESIDUE\tBOND\tRESIDUE\tSEQUENCE2\n";
			my (@segments, @s1_partner, @s2_partner); 
			foreach my $s1r (keys %interaction){
				foreach my $s2r (@{$interaction{$s1r}}){
					push(@segments, Bio::Graphics::Feature->new(-start=>($s1r - $pfamA_seq_objects[$s1]->start + 1 + $seq1_offset),-stop=>($s2r - $pfamA_seq_objects[$s2]->start + 1 + $seq2_offset), -type=>'int', -desc=>(($s1r - $pfamA_seq_objects[$s1]->start + $seq1_offset)-($s2r - $pfamA_seq_objects[$s2]->start + $seq2_offset))));

					push(@s1_partner, Bio::Graphics::Feature->new(-start=>($s1r - $pfamA_seq_objects[$s1]->start + 1 + $seq1_offset),-stop=>($s1r - $pfamA_seq_objects[$s1]->start + 1 + $seq1_offset), -desc=>$s2r));
					push(@s2_partner, Bio::Graphics::Feature->new(-start=>($s2r - $pfamA_seq_objects[$s2]->start + 1 + $seq2_offset),-stop=>($s2r - $pfamA_seq_objects[$s2]->start + 1 + $seq2_offset), -desc=>$s1r));

					#my $db = $s1r - $pfamA_seq_objects[$s1]->start + 1;
					print TXT $seq1->id."\t$s1r\tBOND\t$s2r\t".$seq2->id."\n";
				}
			}
			$panel->add_track($seq1,
                    -glyph => 'protein_arrow',
                    -bump => 0,
                    -double=>1,
                    -tick => 1,
					-relative_coords => 1,
					-constant => $pfamA_seq_objects[$s1]->start - 1,
					);

			$panel->add_track($seq1,
                    -glyph  => 'protein',
                    -fgcolor => $colours{$seq1->desc},
                    -label  => 1,
					#-font => &{$text_size{$scale}},
                   );
			if($scale == 6){
				my $s1_partner = Bio::Graphics::Feature->new(-segments=>[@s1_partner], -desc=>'ints');
				$panel->add_track($s1_partner,
                    	-glyph  => 'int',
						-bgcolor=> $colours{$seq2->desc},
						-font2color => $colours{$seq2->desc},
						-fontcolor => $colours{$seq2->desc},
						-fgcolor => $colours{$seq2->desc},
						-font	=> GD::gdSmallFont,
						-bump	=> 1,
						-description   => 1,
						-label => 0,
                   	);
			}
			$panel->add_track($seq1,
                    -glyph  => 'generic',
                    -bgcolor => $colours{$seq1->desc},
					-font2color => 'black',
                    -label  => 0,
					-height => 4,
					#-font => &{$text_size{$scale}},
                   );

			my $f  = Bio::Graphics::Feature->new(-segments=>[@segments],-type=>'int_set', -description=>"interaction");	
			$panel->add_track($f,
                      -glyph    =>  'crosses',
                      -fgcolor  => 'black',
                      -font2color => 'black',
					  -bgcolor => 'black',
                      -bump     => 0,
                      -height   => 30,
					  -direction => '+',
					  -linewidth => 4,
                     );

				
			$panel->add_track($seq2,
                    -glyph  => 'generic',
                    -bgcolor=> $colours{$seq2->desc},
                    -label  => 0,
					-height => 4,
					#-font => &{$text_size{$scale}},
                   );
			if($scale == 6){
				my $s2_partner = Bio::Graphics::Feature->new(-segments=>[@s2_partner], -desc=>'ints');
				$panel->add_track($s2_partner,
                    	-glyph  => 'int',
						-bgcolor=> $colours{$seq1->desc},
						-font2color => $colours{$seq1->desc},
						-fontcolor => $colours{$seq1->desc},
						-fgcolor => $colours{$seq1->desc},
						#-font	=> &gdSmallFont,
						-bump	=> 1,
						-description   => 1,
						-label => 0,
                   	);
			}

			$panel->add_track($seq2,
                    -glyph  => 'protein',
                    -fgcolor => $colours{$seq2->desc},
                    -label  => 1,
					#-font 	=> &{$text_size{$scale}}
                   );
			$panel->add_track($seq2,
                    -glyph => 'protein_arrow',
                    -bump => 0,
                    -double=>1,
                    -tick => 1,
					-relative_coords => 1,
					-constant => $pfamA_seq_objects[$s2]->start - 1,
					);
		
		
			#print "$image_path    $pdb_id.$bit1~$bit2.$scale.$seq1_offset.$seq2_offset.png\n";
			open(PNG, ">$image_path/$pdb_id.$bit1~$bit2.$scale.$seq1_offset.$seq2_offset.png") || die;
			my $png = $panel->png(\*PNG);
			print PNG $png;
			close(PNG);
			push(@{$images{$seq1->id."~".$seq2->id}}, "$pdb_id.$bit1~$bit2.$scale.$seq1_offset.$seq2_offset.png");
			}	
			close( TXT );
		}
	}

	return %images;	
}

sub normalise_max {
my ($seq, $offset) = @_;
my $newseq = $seq->seq;
my $extend = "X" x $offset;
$newseq .= $extend;
my $seq_extend = Bio::Pfam::SeqPfam->new(-id => $seq->id,
									-seq => $newseq,
									-desc => $seq->description,
									-start => 1,
									-end => ($seq->end - $seq->start + 1 + $offset) );
return $seq_extend;
}

sub normalise_ali {
my ($seq) = @_;
my $length = length($seq->seq);
my $seq_norm= Bio::Pfam::SeqPfam->new(-id => $seq->id."/".$seq->start."-".$seq->end,
									-seq => $seq->seq,
									-desc => $seq->description,
									-start => 1,
									-end => $length );

return $seq_norm;
}


sub normalise {
my ($seq, $offset) = @_;
my $seq_norm= Bio::Pfam::SeqPfam->new(-id => $seq->id,
									-seq => $seq->seq,
									-desc => $seq->description,
									-start => 1 + $offset,
									-end => ($seq->end - $seq->start + 1 +$offset) );

return $seq_norm;
}

sub _shatter_png {
my ($pdb, $int, $rdb, $scale, $tabWidth) = @_;
my ($seq1, $seq2) = split(/~/, $int);
my $fseq1 = $seq1;
my $fseq2 = $seq2;
$fseq1  =~ s/\//~/g;
$fseq2  =~ s/\//~/g;

# See if the large png exists

if (!-e "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png" || !-s "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png"){
	#need to make the image
    my %colours  = get_colour_hash();
    my %pfamA_mapping = map_PfamA_2_PDB($rdb, $pdb, "SEQ");
    my %pfamA_c_mapping = &assign_colours(%pfamA_mapping);
    &make_specific_image($rdb, $pdb, \%pfamA_c_mapping, "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints", $seq1, $seq2, $scale);

}

my @small_images;
my $i_width = $tabWidth/3;
#die "BIGIMAGE had no size\n" if (!-s "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png");
#open(BIGIMAGE, "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png");# || die "could not open $Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png:[$!]\n";
#open(BIGIMAGE, "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/PF00725~.991.png");
#print STDERR "Opened big image $Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png\n";
#print STDERR "opening $Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png";

my $file = "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1~$fseq2.$scale.0.0.png";
my $image = GD::Image->newFromGif($file);


my ($width, $height) = $image->getBounds();

for (my $w=0; $width>$w*$i_width; $w+=0.5){
		my $offset=$w*$i_width;
		#need to reduce.
		my $this_i_width = 300;
		$this_i_width = ($width - $offset) if(($width - $offset) < $i_width);
		my $myImage = new GD::Image($this_i_width, $height);
		$myImage->copy($image,0,0,$offset,0, $this_i_width, $height);
		my $temp_file = "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$pdb.$fseq1.$fseq2.$scale.0.0.$w.png";
		my $file;
		if($temp_file =~ /(\S+.png)/){
		    $file = $1;
		}
		open(NEW, ">$file") || die "Could not open image $file:[$!]\n";;
		print NEW $myImage->png;
		close(NEW);
		push(@small_images, "$pdb.$fseq1.$fseq2.$scale.0.0.$w.png")
	}
close(BIGIMAGE);
#list all of the images
return @small_images;
}

sub interaction_alignment {	
	my($rdb, $domain, $id, $int_domains, $out_path) = @_;
	my %colours  = &get_colour_hash_triples(); # Pfam2Structure::
	my $acc = $rdb->id2acc($id);
	
	my %domains_auto;
	my @tmp_domains;
	if($int_domains){
		@tmp_domains = @{$int_domains};
	}
	push(@tmp_domains, $domain);
	my $doms = "\"";
	$doms .= join("\" or pfamA_acc=\"",@tmp_domains); 
	$doms .= "\"";
	my @dom_autos = $rdb->query("select pfamA_acc, auto_pfamA from pfamA where (pfamA_acc=$doms)");
	foreach(@dom_autos){
		$domains_auto{$$_[0]}=$$_[1];
	}

	# Get all of the sequences that have a certain type of interaction.
	my $query;
	if (!@{$int_domains}){
		$query = "";	
	}else{
		$_ = $$int_domains[0];
		$query = " and (auto_pfamA_B=\"".$domains_auto{$_}."\"";
		for(my $x = 1; $x <= $#$int_domains; $x++){
			$query .= " or auto_pfamA_B=\"".$domains_auto{$$int_domains[$x]}."\"";
		}
		$query .= ")";
	}

	my @interactions = $rdb->query("select distinct auto_pfamseq_A, pfamseq_acc_A, pfamseq_seq_number_A, auto_pfamA_A, pdb_id, chain_A, auto_pfamseq_B, pfamseq_seq_number_B from interactions join pdb where pdb.auto_pdb=interactions.auto_pdb and auto_pfamA_A=\"".$domains_auto{$domain}."\" $query");
#print "Content-type: text/html\n\n";

if (!@interactions){
	print "<p>No interactions found\n";
}else{
# Get all of the sequence auto_numbers so that the domain alignment can be creconstructed.
	my $auto_seqs;
	my %interaction;
	my $first = 1; 
	my ($auto_pfamA_A, $auto_int_seqs);
	my (%seqA_in_query, %seqB_in_query);
	
	foreach (@interactions) {
		# Need to know partner domain !!!!
		
		push(@{$interaction{$$_[1]}{$$_[4]."_".$$_[5]}{$$_[6]}{$$_[7]}}, "$$_[2]");
		
		if ($first){
			$auto_seqs = "pfamseq.auto_pfamseq=$$_[0]";
			$seqA_in_query{$$_[0]}++;
			$auto_int_seqs = "pfamseq.auto_pfamseq=$$_[6]";
			$seqB_in_query{$$_[6]}++;
			$auto_pfamA_A = $$_[3];
			$first = 0;
			
		}else{
			if (!$seqA_in_query{$$_[0]}){
				$auto_seqs .= " or pfamseq.auto_pfamseq=$$_[0]";
				$seqA_in_query{$$_[0]}++;
			}
			if (!$seqB_in_query{$$_[6]}){
				$auto_int_seqs .= " or pfamseq.auto_pfamseq=$$_[6]";
				$seqB_in_query{$$_[6]}++;
			}
		}
	}
	my @domain_sequence = $rdb->query("select pfamseq_acc, seq_start, seq_end, sequence from pfamA_reg_full join pfamseq where pfamseq.auto_pfamseq=pfamA_reg_full.auto_pfamseq and ($auto_seqs) and auto_pfamA=$auto_pfamA_A and significant=1 and in_full=1");
	
	my @interacting_domains = $rdb->query("select pfamseq.auto_pfamseq, pfamA_id, seq_start, seq_end from pfamA_reg_full join pfamseq, pfamA where pfamseq.auto_pfamseq=pfamA_reg_full.auto_pfamseq and pfamA.auto_pfamA=pfamA_reg_full.auto_pfamA and ($auto_int_seqs) and pfamA.auto_pfamA=pfamA_reg_full.auto_pfamA and significant=1 and in_full=1");
	my %interacting_domains;
	#print "$#interacting_domains interacting domains\n";
	#Write out a fa file......
	open(FA, ">$out_path/fa.$$") || $logger->logdie("could not open fasta file $out_path/fa.$$");
	foreach my $ds (@domain_sequence){
		my $dom_seq = substr($$ds[3], ($$ds[1] - 1), ($$ds[2]-$$ds[1]+ 1));
		print FA ">".$$ds[0]."/".$$ds[1]."-".$$ds[2]."\n".$dom_seq."\n";
	}
	close(FA);
	# Align these sequences to the HMM_ls
#	$logger->debug("doc_root ", $Bio::Pfam::Web::PfamWWWConfig::doc_root, " DOCUMENT_ROOT ", $ENV{'DOCUMENT_ROOT'});
	system("$Bio::Pfam::Web::PfamWWWConfig::doc_root/../bin-offline/HMM/bin/hmmfetch  $Bio::Pfam::Web::PfamWWWConfig::file_root/data/Pfam_ls $id > $Bio::Pfam::Web::PfamWWWConfig::file_root/temp/$id.$$.hmm") and $logger->logdie("Error running hmmfetch for $id");
	
	open(ALIGN ,"$Bio::Pfam::Web::PfamWWWConfig::doc_root/../bin-offline/HMM/bin/hmmalign -q $Bio::Pfam::Web::PfamWWWConfig::file_root/temp/$id.$$.hmm $out_path/fa.$$ |") || $logger->logdie("Could not open ALIGN for reading");
	my %align;
	my @dse;
	while(<ALIGN>){
	    if (/\#/){
		next;
	    }elsif(/\/\//){
		next;
	    }elsif(/(\S+\s+)(\S+)/){
		$align{$1} .= $2;
		push(@dse, $1);
	    }
	}
	close(ALIGN);
	open(ALIGN, ">$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$acc.$$.align") || $logger->logdie("could not open align");
	foreach my $dse (@dse){
	    print ALIGN "$dse".$align{$dse}."\n";
	}
	close (ALIGN);
	
	open(ALIGN2, "$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$acc.$$.align") || $logger->logdie("could not open align");
	# Area into Bio::AlignPfam
	my $pfamaln = new Bio::Pfam::AlignPfam->new;
	$pfamaln->read_Pfam(\*ALIGN2);
	
	# Get the alignment length.  From this we can make the glyph panel or a blank interaction line
	my $length = $pfamaln->length();
	my $blank = "-" x ($length + 1);
	my %domains_seen;
	my $domain_count = 0;
	my $seq_coos = Bio::Pfam::SeqPfam->new(-id => "align",
					       -seq => $blank,
					       -start => 1,
					       -end => $length
);
		my $width = ($length + 20)  * 8;
		my $panel = Bio::Graphics::Panel->new(
						      -segment   => $seq_coos,
						      -key_style => 'between',
						      -width     => $width,
						      -bgcolour  => '#FFCC99',
						      -grid		 => 1,
						      -gridcolor => '#CCCCCC',
						      -spacing   => '2',
						      -key_style => 'bottom',
						      -key_color => 'white',
						      );
		my %seq_add_to_panel;
		my $intDomains =  join ("~", @{$int_domains});
		if($intDomains){
			open (ALIGN, ">$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$domain~$intDomains.sth") || $logger->logdie($!);
		}else{
			open (ALIGN, ">$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$domain~.sth") || $logger->logdie($!);
		}

	print ALIGN "# STOCKHOLM 1.0\n";
	print ALIGN "#=GF ID iPfam:$domain\n";
	if ($intDomains =~ /\S+/){
	  print ALIGN "#=GF CC Interactions for $domain with $intDomains\n";
      }else{
	print ALIGN "#=GF CC Interactions for $domain with all domains\n";
	}
	my (%seqIds, %intsIds);
	foreach my $seq ($pfamaln->each_seq()) {  
	    my $key = $seq->id()."/".$seq->start()."-".$seq->end();
	   $seqIds{$key} = $seq->seq;
			foreach my $pdb_chain_int (keys %{$interaction{$seq->id()}}){
				# my @segments;
				# Now for each interacting domain
				my $string = $seq->seq;
				$string =~ s/\S/-/g;
				my %segments;	
				foreach my $seqB (keys %{$interaction{$seq->id()}{$pdb_chain_int}}){
					my ($domainB_id, $domainB_start, $domainB_end); 
					foreach my $seqB_res (keys %{$interaction{$seq->id()}{$pdb_chain_int}{$seqB}}){
						foreach my $domainB (@interacting_domains){
							if(($$domainB[0] == $seqB) and ($$domainB[2]<=$seqB_res) and ($$domainB[3]>=$seqB_res)){
								$domainB_id = $$domainB[1];
								$domainB_start = $$domainB[2];
								$domainB_end = $$domainB[3];
								last;
							}

						}
						foreach my $int_res (@{$interaction{$seq->id()}{$pdb_chain_int}{$seqB}{$seqB_res}}){
							if(($int_res >= $seq->start) && ($int_res <= $seq->end)){						 
								my $col = $pfamaln->column_from_residue_number($seq->id, $int_res);
								if ($col <= length($string)){
									 substr($string, ($col - 1), 1, "*");
								}
								push(@{$segments{"$domainB_id:$domainB_start:$domainB_end"}} , Bio::Graphics::Feature->new(-start=>$col, -stop=>$col));
							}
						}
						
						$intsIds{$key}{"$pdb_chain_int,$domainB_id/$domainB_start-$domainB_end"} = $string;
					}	
				}
				if (%segments){
					my $key = $seq->id.":".$seq->start.":".$seq->end;
					if(!$seq_add_to_panel{$key}){
						#Normalised and add the sequence if not part of the track
						my $norm_seq = &normalise_ali($seq);
						#add the track to the panel and the 
						$panel->add_track($norm_seq,
										-glyph  => 'protein',
										-fgcolor => 'black',
										-label  => 1,
										#-font => &gdMediumBoldFont,
                   						);
						$seq_add_to_panel{$key}++;
					}	
					#Now add the segments;
					foreach my $int_domain_detail (keys %segments){
						my @int_domain_detail = split(/:/, $int_domain_detail);
						my $key;
						if(!$domains_seen{$int_domain_detail[0]}){
							$domain_count++;
							$domains_seen{$int_domain_detail[0]} = $domain_count;
							$key = $int_domain_detail[0];
						}else{
							$key = "";
						}
						my $ints  = Bio::Graphics::Feature->new(-segments=>[@{$segments{$int_domain_detail}}],-name=>$pdb_chain_int, -desc=>$pdb_chain_int);	
					$panel->add_track($ints,
                      	-glyph    =>  'generic',
                      	-fgcolor  => 'black',
					  	-bgcolor => $colours{$domains_seen{$int_domain_detail[0]}},
                      	-bump     => 0,
						-key => $key,
						-label  => 1,
						);
					}
				}	
			}
		}	
		#print "here\n";
		# Finally print the png.
		open(PNG, ">$Bio::Pfam::Web::PfamWWWConfig::file_root/ints/$domain~$intDomains.png") || $logger->logdie($!);
		my $png = $panel->png(\*PNG);	
		print PNG $png;
		close(PNG);
	my $max_length = 0;
	foreach my $seq (keys %seqIds){
		  foreach my $int (keys %{$intsIds{$seq}}) {
	    $max_length = length("#=GR $seq IR $int") if ($max_length < length("#=GR $seq IR $int"));
	    }
	}
	$max_length += 3;
	foreach my $seq (keys %seqIds){
	  #print STDERR "Sequence: $seq\n";
	  print ALIGN sprintf("%-${max_length}s %s\n", $seq, $seqIds{$seq});
	  foreach my $int (keys %{$intsIds{$seq}}) {
	    print ALIGN sprintf("%-${max_length}s %s\n", "#=GR $seq IR $int", $intsIds{$seq}{$int});
	  }

	  
	  
	}
	print ALIGN "//\n";	
	close(ALIGN);
	}	
}

1;
