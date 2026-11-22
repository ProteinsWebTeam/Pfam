#!/usr/bin/env perl

# Calculate Pfam-A coverage statistics directly from PfamLive.
#
# Usage: database_stats.pl <num_seqs> <num_res>

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

$| = 1;

my ( $pfamseq_sequences, $pfamseq_residues ) = @ARGV;

unless ( @ARGV == 2
  && defined $pfamseq_sequences
  && $pfamseq_sequences =~ /^\d+$/
  && $pfamseq_sequences > 0 )
{
  die "Please specify the number of sequences in pfamseq.\n";
}

unless ( defined $pfamseq_residues
  && $pfamseq_residues =~ /^\d+$/
  && $pfamseq_residues > 0 )
{
  die "Please specify the number of residues in pfamseq.\n";
}

unless ( $pfamseq_residues > $pfamseq_sequences ) {
  die "The number of sequences is greater than the number of residues!!!\n";
}

my $config = Bio::Pfam::Config->new;
my $pfamDB =
  Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my ($pfamAfamilies) =
  $dbh->selectrow_array('SELECT COUNT(*) FROM pfamA');

# Regions that occur in Pfam-A.full are represented by in_full=1.  Sorting by
# sequence and coordinates allows overlapping (principally nested) regions to
# be merged while using constant memory.
my $regions_sth = $dbh->prepare(
  q{
    SELECT pfamseq_acc, seq_start, seq_end
      FROM pfamA_reg_full_significant
     WHERE in_full = 1
     ORDER BY pfamseq_acc, seq_start, seq_end
  },
  { mysql_use_result => 1 }
) or die "Failed to prepare Pfam-A region query: " . $dbh->errstr . "\n";

$regions_sth->execute
  or die "Failed to retrieve Pfam-A regions: " . $regions_sth->errstr . "\n";

my ( $current_sequence, $current_start, $current_end );
my ( $pfama_sequences, $pfama_residues ) = ( 0, 0 );

while ( my ( $sequence, $start, $end ) = $regions_sth->fetchrow_array ) {
  die "Invalid Pfam-A region for $sequence: $start-$end\n"
    unless defined $sequence
    && defined $start
    && defined $end
    && $start > 0
    && $end >= $start;

  if ( !defined $current_sequence || $sequence ne $current_sequence ) {
    $pfama_residues += $current_end - $current_start + 1
      if defined $current_sequence;
    $pfama_sequences++;
    ( $current_sequence, $current_start, $current_end ) =
      ( $sequence, $start, $end );
  }
  elsif ( $start <= $current_end ) {
    $current_end = $end if $end > $current_end;
  }
  else {
    $pfama_residues += $current_end - $current_start + 1;
    ( $current_start, $current_end ) = ( $start, $end );
  }
}

$pfama_residues += $current_end - $current_start + 1
  if defined $current_sequence;
$regions_sth->finish;

my $pfama_seq_cov =
  sprintf( '%.2f', $pfama_sequences / $pfamseq_sequences * 100 );
my $pfama_res_cov =
  sprintf( '%.2f', $pfama_residues / $pfamseq_residues * 100 );

print "Statistics for Pfam-A\n";
print "*******************************\n\n";

print "\nTotal:\n\tResidues = $pfamseq_residues\n";
print "\tSequences = $pfamseq_sequences\n\n";

print "Pfam-A\n======\n\n";
print "\tNumber of families = $pfamAfamilies\n";
print "\tResidues = $pfama_residues\n";
print "\tSequences = $pfama_sequences\n";
print "\tPercent pfamseq residues = $pfama_res_cov\n";
print "\tPercent Pfamseq sequences = $pfama_seq_cov\n\n";

my $version_sth = $dbh->prepare(
  q{
    UPDATE version
       SET pfamA_coverage = ?,
           pfamA_residue_coverage = ?,
           number_families = ?
  }
) or die "Failed to prepare version update: " . $dbh->errstr . "\n";

$version_sth->execute( $pfama_seq_cov, $pfama_res_cov, $pfamAfamilies )
  or die "Failed to update version statistics: " . $version_sth->errstr . "\n";
