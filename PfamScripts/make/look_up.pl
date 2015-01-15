#!/usr/bin/env perl
# Ruth, penny, Kirstie, 15th Aug, 2012
# look up pfamseq_acc (UniProt acc)
# to find what pfamA fams it is in
# find scores, coordinates, full length
# look up a pdb entry to find which seq it is on and which fams it is in
# given either a Uniprot accession or a PDB id
# look_up.pl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my $help;
&GetOptions("help" => \$help );

help() if($help);

#Get input sequence accession or pdb id from user
my $input= shift(@ARGV);

#Check input exists
unless($input) {
  die "Need to specify sequence accession or pdb id on the command line\n\n";
  help();
}

#Set up db
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Report all Pfam-A matches for sequence acc or pdb id
if($input =~ /^\S{6,10}$/) { #Does input look like a sequence acc

  print STDERR "Looking up sequence accession: $input\n\n";

  #Query db for Pfam-A info
  my $st = $dbh->prepare("select a.pfamA_acc, pfamA_id, seq_start, seq_end, sequence_bits_score, domain_bits_score, length from pfamA a join pfamA_reg_full_significant r on a.pfamA_acc=r.pfamA_acc join pfamseq s on r.pfamseq_acc=s.pfamseq_acc where s.pfamseq_acc='$input' and in_full=1 order by seq_start") or die "Failed to prepare statement:".$dbh->errstr."\n";
  
  $st->execute() or die "Couldn't execute statement ".$st->errstr."\n";
  
  my $array_ref = $st->fetchall_arrayref();
  
  my $c;
  foreach my $row (@$array_ref) {
    my ($pfamA_acc, $pfamA_id, $seq_start, $seq_end, $sequence_bits_score, $domain_bits_score, $length) = ($row->[0], $row->[1], $row->[2], $row->[3], $row->[4], $row->[5], $row->[6]);

    #Print header
    print underline("Pfam-acc Pfam-id          Seq-start Seq-end Seq-score Dom-score Length\n") unless($c);

    #Print results (format them to make them line up nicely)
    print sprintf("%-8s %-16s %9s %7s %9s %9s %-6s\n", $pfamA_acc, $pfamA_id, $seq_start, $seq_end, $sequence_bits_score, $domain_bits_score, $length); 
    $c++;
  }

  #Print number of Pfam-A matches found, if any
  if($c) {
    print "\n$c pfamA accession(s)\n";
  }
  else {
    #Check if the sequence is in pfamlive
     my $st2 = $dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc='$input'") or die "Failed to prepare statement:".$dbh->errstr."\n";
  
     $st2->execute() or die "Couldn't execute statement ".$st->errstr."\n";
  
     my $ref = $st2->fetchall_arrayref();
     my $in_db;
     foreach my $row (@$ref) {
       $in_db=1;
     }
     if($in_db) { #Sequence is in db, but no Pfam-A matches
       print "No Pfam-A matches found in pfamlive for $input\n";
     }
     else { #Sequence is not in the db
       print "The sequence $input is not in pfamlive\n";
     }
  }
}
elsif ($input =~ /^\S{4}$/) { #Does sequence look like a pdb id
  print STDERR "Looking up pdb id: $input\n\n";
  
  #Query db for Pfam-A info
  my $st = $dbh->prepare("select a.pfamA_acc, pfamA_id, s.pfamseq_acc, d.chain, r.seq_start, r.seq_end, pdb_res_start, pdb_res_end, sequence_bits_score, domain_bits_score, length from pfamA a join pfamA_reg_full_significant r on a.pfamA_acc=r.pfamA_acc join pfamseq s on r.pfamseq_acc=s.pfamseq_acc join pdb_pfamA_reg d on s.pfamseq_acc=d.pfamseq_acc and a.pfamA_acc=d.pfamA_acc where d.pdb_id='$input' and in_full=1 order by chain, seq_start") or die "Failed to prepare statement:".$dbh->errstr."\n";

  #my $st = $dbh->prepare("select pfamA_acc, pfamA_id, pfamseq_acc, chain, r.seq_start, r.seq_end, pdb_res_start, pdb_res_end, sequence_bits_score, domain_bits_score, length from pfamA a, pfamA_reg_full_significant r, pfamseq s, pdb_pfamA_reg d where a.auto_pfamA=r.auto_pfamA and r.auto_pfamseq=s.auto_pfamseq and s.auto_pfamseq=d.auto_pfamseq and a.auto_pfamA=d.auto_pfamA and pdb_id='$input' and in_full=1 order by chain, seq_start") or die "Failed to prepare statement:".$dbh->errstr."\n";

  $st->execute() or die "Couldn't execute statement ".$st->errstr."\n";
  my $array_ref = $st->fetchall_arrayref();

  my $c;
  foreach my $row (@$array_ref) {
    my ($pfamA_acc, $pfamA_id, $pfamseq_acc, $chain, $seq_start, $seq_end, $pdb_start, $pdb_end, $sequence_bits_score, $domain_bits_score, $length) = ($row->[0], $row->[1], $row->[2], $row->[3], $row->[4], $row->[5], $row->[6], $row->[7], $row->[8],  $row->[9], $row->[10]);

    #Print header
    print underline("Chain Seq-acc Pfam-acc Pfam-id         Pfam-start Pfam-end Pdb-start Pdb-end Seq-score Dom-score Length\n") unless($c);	 
    

    #Print results (format them to make them line up nicely)
    print sprintf("%-5s %-7s %-8s %-16s %9s %8s %9s %7s %9s %9s %6s\n", $chain, $pfamseq_acc, $pfamA_acc, $pfamA_id, $seq_start, $seq_end, $pdb_start, $pdb_end, $sequence_bits_score, $domain_bits_score, $length);

    $c++;
  }

  #Print number of Pfam-A matches found, if any
  if($c) {
    print "\n$c pfamA accession(s)\n";
  }
  else {
    #Check pdb id is in the db
    my $st2 = $dbh->prepare("select pdb_id from pdb where pdb_id='$input'") or die "Failed to prepare statement:".$dbh->errstr."\n";
    
    $st2->execute() or die "Couldn't execute statement ".$st->errstr."\n";
    
    my $ref = $st2->fetchall_arrayref();
    my $in_db;
    foreach my $row (@$ref) {
      $in_db=1;
    }
    if($in_db) { #pdb_id is in db, but no Pfam-A matches
       print "No Pfam-A matches found in pfamlive for $input\n";
     }
     else { #pdb_id is not in the db
       print "The pdb structure $input is not in pfamlive\n";
     }
  }
} else { #Doesn't look like a sequence accession or pdb id
  die "$input is not in a recognised format for a sequence accession or pdb id\n";
}

sub underline{
    my $msg = shift;
    my $lines = $msg;
    $lines =~ s/\S/-/g;
    return "$msg$lines";
}



sub help {
  print STDERR << "EOF";

This script looks up details of either a protein in the LIVE database or a pdb in the latest release database, given a Uni
Prot accession or a PDB id.


Usage:
  $0 <UniProt accession> OR <PDB id>

No additional options

Eg:
$0 3ky9
$0 P15498

Details provided: 
For UniProt:-
Pfam-accession  Pfam ID  Seq-start  Seq-end  Seq-Score  Dom-Score  Length

For PDB:-
Chain  UniProt-accession  Pfam-accession  Pfam-ID  Seq-start  Seq-end  PDB-start  PDB-end  Seq-Score  Dom-Score  Length


EOF

  exit(1);
}

