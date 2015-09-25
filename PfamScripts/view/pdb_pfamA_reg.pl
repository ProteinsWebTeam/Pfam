#!/usr/bin/env perl

#Script to transform pdb data from pdb_residue_data table and
#populate pdb_pfamA_table


use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;


my ($pfamA_acc);
GetOptions('pfamA=s' => \$pfamA_acc);

unless($pfamA_acc and $pfamA_acc =~ /^PF\d{5}$/) {
  die "Need to specify a pfamA_acc (-pfamA <pfamA_acc>)\n";
}


#Database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;



#Get all pdb residue data for the sequences in the family
my $pdbData;
my @pdbResult = $pfamDB->getSchema->resultset("PdbResidueData")->search(
  {
    "uniprot_reg_full.pfama_acc" => $pfamA_acc,
    "uniprot_reg_full.in_full"    => 1,
    observed                          => 1
  },
  {
    join   => [qw(uniprot_reg_full)],
    select => [  qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number pdb_insert_code) ],
    as => [ qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number pdb_insert_code)]
  }
);

#Go through and store data in $pdbData
foreach my $row (@pdbResult) {
  #Sometimes there is >1 pdb_insert_code for a single uniprot reside, so let's take the first one
  unless ( $pdbData->{ $row->get_column('pfamseq_acc') }->{ $row->get_column('pfamseq_seq_number') }->{ $row->get_column('pdb_id') . "_" . $row->get_column('chain') } ) {
    my $pdb_number_icode=$row->get_column('pdb_seq_number');
    if($row->get_column('pdb_insert_code')) {
      $pdb_number_icode.=$row->get_column('pdb_insert_code');
    }
    $pdbData->{ $row->get_column('pfamseq_acc') }->{ $row->get_column('pfamseq_seq_number') }->{ $row->get_column('pdb_id') . "_" . $row->get_column('chain') } = $pdb_number_icode;
  }
}


#Go though each sequence in the family, and map to any pdb data
my @famRegions=$pfamDB->getSchema->resultset("UniprotRegFull")->search({ pfamA_acc => $pfamA_acc });
my @upload;
foreach my $region (@famRegions) {
  my $uniprot_acc=$region->uniprot_acc->uniprot_acc;

  #Get all pdbid_chains that map to this sequence
  my %uniqueMaps;
  foreach my $pos ( keys %{ $pdbData->{ $uniprot_acc } } ) {
    foreach my $pdb_chain ( keys %{$pdbData->{$uniprot_acc}->{$pos}} ) {
      $uniqueMaps{$pdb_chain} = 1;
    }       
  } 

  #Fing the pdb region, if any, that maps to the pfamA
  my ($pdb_res_start, $pdb_start_icode, $pdb_res_end, $pdb_end_icode);
  foreach my $pdb_chain (keys %uniqueMaps) {
    for(my $i=$region->seq_start; $i<=$region->seq_end; $i++) {
      if($pdbData->{$uniprot_acc}->{$i}->{$pdb_chain}) {
        if($pdb_res_start) {
          #If we get here it means the pdb structure does not cover the whole pfamA region
          my $pdb_seq_number_icode=$pdbData->{$uniprot_acc}->{$i}->{$pdb_chain};
          ($pdb_res_end, $pdb_end_icode) = ("", "");
          ($pdb_res_end, $pdb_end_icode) = $pdb_seq_number_icode =~ /(\S?\d+)(\w+)?/; #eg 0, 0A, -5, -5A, 10, 10A
          die "Couldn't extract pdb_res_end from [$pdb_seq_number_icode]\n" unless(defined($pdb_res_end));
        }
        else {
          #We have a start pdb residue
          my $pdb_seq_number_icode=$pdbData->{$uniprot_acc}->{$i}->{$pdb_chain};
          ($pdb_res_start, $pdb_start_icode) = $pdb_seq_number_icode =~ /(\S?\d+)(\w+)?/;
          die "Couldn't extract pdb_res_start from [$pdb_seq_number_icode]\n" unless(defined($pdb_res_start));

          #See if pfamA region end maps to a pdb residue, if it does we can exit the loop
          if($pdbData->{$uniprot_acc}->{$region->seq_end}->{$pdb_chain}) {
            my $pdb_seq_number_icode2=$pdbData->{$uniprot_acc}->{$region->seq_end}->{$pdb_chain};
            ($pdb_res_end, $pdb_end_icode) = $pdb_seq_number_icode2 =~ /(\S?\d+)(\w+)?/; #eg 0, 0A, -5, -5A, 10, 10A
            die "Couldn't extract pdb_res_end from [$pdb_seq_number_icode2]\n" unless(defined($pdb_res_end));
            last;
          }
        }
      }
    }

    if($pdb_res_start and $pdb_res_end) {
      my ($pdb_id, $chain) = $pdb_chain =~ /(\S+)_(\S+)?/;
      push(@upload, {
          auto_uniprot_reg_full => $region->auto_uniprot_reg_full,
          pdb_id              => $pdb_id,
          pfama_acc          => $pfamA_acc,
          pfamseq_acc        => $uniprot_acc,
          chain               => $chain,
          pdb_res_start       => $pdb_res_start,
          pdb_start_icode     => $pdb_start_icode,
          pdb_res_end         => $pdb_res_end,
          pdb_end_icode       => $pdb_end_icode,
          seq_start           => $region->seq_start,
          seq_end             => $region->seq_end,
        });
    }
  }
}

if(@upload) {
  my $u = @upload;
  print "$pfamA_acc: Uploading $u rows into pdb_pfamA_reg table\n";
}
else {
  print "$pfamA_acc: No data to upload to pdb_pfamA_reg table\n";
}

#Delete old regions if any
$pfamDB->getSchema->resultset('PdbPfamAReg')->search( { pfama_acc => $pfamA_acc} )->delete;

#Upload new ones
$pfamDB->getSchema->resultset("PdbPfamAReg")->populate(\@upload);

