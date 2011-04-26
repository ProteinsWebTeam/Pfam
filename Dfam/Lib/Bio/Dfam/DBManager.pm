
#
# BioPerl module for Bio::Dfam::DBManager
#
# $Author$

package Bio::Dfam::DBManager;
use strict;
use warnings;
use DfamDB;
use Data::Dumper;
use Carp;

sub new {
  my $caller    = shift;
  my $class     = ref($caller) || $caller;
  my %dbiParams = ();
  my $self      = {
    user     => "dfam_web_ro",
    host     => "dfamdb1",
    port     => "3306",
    database => "dfam_21_0_web",
    driver   => "mysql",
    debug    => 0,
    @_,
  };

  #print STDERR Dumper($self);

  eval {
    $self->{'schema'} =
      DfamDB->connect( "dbi" . ":"
        . $self->{driver} . ":"
        . $self->{database} . ":"
        . $self->{host} . ":"
        . $self->{port},
      $self->{user}, $self->{password}, \%dbiParams );
  };
  if ($@) {
    croak("Failed to get schema for databse:"
        . $self->database
        . ". Error:[$@]\n" );
  }

  return bless( $self, $caller );
}

sub getSchema {
  my $self = shift;
  return $self->{schema};
}

#sub DESTROY {
#  my $self = shift;
#  $self->{'schema'}->storage->disconnect;
#}

#Select methods should go in here.

#General shared by all Dfam DB managers
sub id2acc {
  my ( $self, $id ) = @_;
  my $result =
  $self->getSchema->resultset("Dfama")->find( { "dfama_id" => $id } );
  if ( $result && $result->dfama_acc ) {
    return ( $result->dfama_acc );
  }
}

sub acc2id {
  my ( $self, $acc ) = @_;
  my $result =
    $self->getSchema->resultset("Dfama")->find( { "dfama_acc" => $acc } );
  if ( $result && $result->dfama_id ) {
    return ( $result->dfama_id );
  }
}

sub clanId2Acc {
  my ( $self, $id ) = @_;
  my $result =
  $self->getSchema->resultset("Clans")->find( { "clan_id" => $id } );
  if ( $result && $result->clan_acc ) {
    return ( $result->clan_acc );
  }
}

sub clanAcc2Id {
  my ( $self, $acc ) = @_;
  my $result =
    $self->getSchema->resultset("Dfama")->find( { "clan_acc" => $acc } );
  if ( $result && $result->clan_id ) {
    return ( $result->clan_id );
  }
}

sub getVersion {
  my ($self) = @_;
  my $version = $self->getSchema->resultset("Version")->search()->first;

  if ( $version and $version->dfam_release_date ) {
    return $version;
  }
  else {
    croak("Failed to get the row from the Version table\n");
  }
}

#Try to find in the incoming family is part of a clan
sub getClanDataByDfam {
  my ( $self, $family ) = @_;
  my $result;
  if ( $family =~ /PF\d+/ ) {

    #Looks like an accession
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $result = $self->getSchema->resultset("ClanMembership")->find(
      { "auto_dfama.dfama_acc" => $family },
      {
        join     => [qw/auto_dfama auto_clan/],
        prefetch => [qw/auto_clan/]
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {

    #Looks like we have a family id
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $result = $self->getSchema->resultset("ClanMembership")->find(
      { "auto_dfama.dfama_id" => $family },
      {
        join     => [qw( auto_dfama auto_clan)],
        prefetch => [qw( auto_clan )]
      }
    );

  }
  else {
    cluck("$family does not look like a dfamA accession or id");
  }

  #Return something if we have found something
  if ( $result && $result->auto_clan->clan_acc ) {
    carp("Found clan information for\n") if $self->{'debug'};
    return ($result);
  }
  carp("Did not find any clan information for $family") if $self->{'debug'};
}

sub getClanData {
  my ( $self, $clan ) = @_;
  my $clanData;
  if ( $clan =~ /CL\d{4}/ ) {
    carp("Looking up information for $clan. I think this is an accession")
      if $self->{'debug'};
    $clanData =
      $self->getSchema->resultset("Clans")->find( { "clan_acc" => $clan } );

  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    carp("Looking up information for $clan. I think this is an id")
      if $self->{'debug'};
    $clanData =
      $self->getSchema->resultset("Clans")->find( { "clan_id" => $clan } );
  }
  else {
    cluck("$clan does not look like a clan accession or id");
  }
  if ( ref($clanData) ) {
    carp("Found clan information for $clan") if $self->{'debug'};
    return ($clanData);
  }
  carp("Did not find clan information for $clan") if $self->{'debug'};
}

sub getClanMembership {
  my ( $self, $clan ) = @_;
  my @clanData;
  if ( $clan =~ /CL\d{4}/ ) {
    carp("Looking up information for $clan. I think this is an accession")
      if $self->{'debug'};
    @clanData = $self->getSchema->resultset("ClanMembership")->search(
      { "auto_clan.clan_acc" => $clan },
      {
        join     => [qw/auto_clan auto_dfama/],
        prefetch => [qw/auto_clan auto_dfama/]
      }
    );

  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    carp("Looking up information for $clan. I think this is an id")
      if $self->{'debug'};
    @clanData = $self->getSchema->resultset("ClanMembership")->search(
      { "auto_clan.clan_id" => $clan },
      {
        join     => qw( auto_clan auto_dfama ),
        prefetch => qw( auto_clan auto_dfama )
      }
    );
  }
  else {
    cluck("$clan does not look like a clan accession or id");
  }

  if (scalar(@clanData)) {
    carp("Found clan information for $clan") if $self->{'debug'};
    return ( \@clanData );
  }
  else {
    carp("Did not find clan membership for $clan") if $self->{'debug'};
  }
}

sub getAllClanData {
  my ($self) = shift;
  my @clanData = $self->getSchema->resultset("Clans")->search();
  return \@clanData;
}

sub getAllDeadClanData {
  my ($self) = shift;
  my @deadClanData = $self->getSchema->resultset("DeadClans")->search();
  return \@deadClanData;
}

sub getDfamData {
  my ( $self, $family ) = @_;
  my $familyData;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $familyData =
      $self->getSchema->resultset("Dfama")->find( { "dfama_acc" => $family } );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData =
      $self->getSchema->resultset("Dfama")->find( { "dfama_id" => $family } );
  }
  else {
    cluck("$family does not look like a family accession or id");
  }
  if ( ref($familyData) ) {
    carp("Found family information for $family") if $self->{'debug'};
    return ($familyData);
  }
  carp("Did not find family information for $family") if $self->{'debug'};
}

sub getAllDfamFamilyData {
  my ($self) = @_;
  my @familyData;
  carp("Looking up information for all families.") if $self->{'debug'};
  @familyData = $self->getSchema->resultset("Dfama")->search();

  if (@familyData) {
    carp("Found family data") if $self->{'debug'};
    return ( \@familyData );
  }
  else {
    carp("Failed to get family data") if $self->{'debug'};

  }
}

sub getAllDeadFamilyData {
  my ($self) = @_;
  my @familyData;
  carp("Looking up information for all dead families.") if $self->{'debug'};
  @familyData = $self->getSchema->resultset("DeadFamilies")->search();

  if (@familyData) {
    carp("Found dead family data") if $self->{'debug'};
    return ( \@familyData );
  }
  else {
    carp("Failed to get dead family data") if $self->{'debug'};

  }
}

sub getAllDfamFamilyDataLike {
  my ($self, $term) = @_;
  my @familyData;
  carp("Looking up information for all families with and id like .") if $self->{'debug'};
  @familyData = $self->getSchema->resultset("Dfama")->search( { dfama_id => { 'like' => $term } });

  if (@familyData) {
    carp("Found family data") if $self->{'debug'};
    return ( \@familyData );
  }
}


sub getDfamInterPro {
  my ( $self, $family ) = @_;
  my $familyData;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("Dfama")->find(
      { "dfamA_acc" => $family },
      {
        join     => qw( interpros ),
        prefetch => qw( interpros )
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("Dfama")->find(
      { "dfamA_id" => $family },
      {
        join     => qw( interpros ),
        prefetch => qw( interpros )
      }
    );
  }
  else {
    cluck("$family does not look like a family accession or id");
  }
  if ( ref($familyData) ) {
    carp("Found family information for $family") if $self->{'debug'};
    return ($familyData);
  }
  carp("Did not find family information for $family") if $self->{'debug'};
}

sub getDfamGO {
  my ( $self, $family ) = @_;
  my @familyGO;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("Dfama")->search(
      { "dfamA_acc" => $family },
      {
        join     => qw( gene_ontologies ),
        prefetch => qw( gene_ontologies )
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("Dfama")->search(
      { "dfamA_id" => $family },
      {
        join     => qw( gene_ontologies ),
        prefetch => qw( gene_ontologies )
      }
    );
  }
  else {
    cluck("$family does not look like a family accession or id");
  }
  if ( scalar(@familyGO) ) {
    carp("Found family information for $family") if $self->{'debug'};
    return ( \@familyGO );
  }
  carp("Did not find family information for $family") if $self->{'debug'};
}

sub getNSEInFull {
  my ( $self, $family ) = @_;
  my @familyNSE;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    @familyNSE =
      $self->getSchema->resultset("DfamA_reg_full_significant")->search(
      {
        "dfamA.dfamA_acc" => $family,
        "in_full"         => 1
      },
      {
        join     => [qw( dfamA dfamseq )],
        prefetch => [qw( dfamA dfamseq )]
      }
      );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyNSE =
      $self->getSchema->resultset("DfamA_reg_full_significant")->search(
      {
        "dfamA.dfamA_id" => $family,
        "in_full"        => 1
      },
      {
        join     => [qw( dfamA dfamseq )],
        prefetch => [qw( dfamA dfamseq )]
      }
      );

  }
  else {
    cluck("$family does not look like a family accession or id");
  }
  if ( scalar(@familyNSE) ) {
    carp("Found family information for $family") if $self->{'debug'};
    return ( \@familyNSE );
  }
  carp("Did not find family information for $family") if $self->{'debug'};
}

sub getNSEseed {
  my ( $self, $family ) = @_;
  my @familyNSE;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    @familyNSE = $self->getSchema->resultset("DfamA_reg_seed")->search(
      { "dfamA.dfamA_acc" => $family },
      {
        join     => [qw( dfamA dfamseq )],
        prefetch => [qw( dfamA dfamseq )]
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyNSE = $self->getSchema->resultset("DfamA_reg_seed")->search(
      { "dfamA.dfamA_id" => $family },
      {
        join     => [qw( dfamA dfamseq )],
        prefetch => [qw( dfamA dfamseq )]
      }
    );

  }
  else {
    cluck("$family does not look like a family accession or id");
  }
  if ( scalar(@familyNSE) ) {
    carp("Found family information for $family") if $self->{'debug'};
    return ( \@familyNSE );
  }
  carp("Did not find family information for $family") if $self->{'debug'};
}

sub getOverlapingFullDfamRegions {
  my ( $self, $regionsHash, $overlaps ) = @_;

#select dfamseq_id, seq_start, seq_end, dfamA_id from dfamA a, dfamA_reg_full r, dfamseq s
#where dfamseq_acc="Q4C4F7" and
#((130 <= r.seq_start and 130 >= r.seq_end) or ( 204 >= r.seq_start and 204 <= r.seq_end) or (130 < r.seq_start and 204 >r.seq_end))
#and s.auto_dfamseq=r.auto_dfamseq and r.auto_dfamA=a.auto_dfamA and in_full=1;

  my $dbh = $self->getSchema->storage->dbh;
  my $sth = $dbh->prepare(
"select distinct seq_start, seq_end, dfamA_acc, dfamA_id from dfamA a, dfamA_reg_full_significant r, dfamseq s where dfamseq_acc= ? and
  ((? >= r.ali_start and ? <= r.ali_end) or ( ? >= r.ali_start and ? <= r.ali_end) or (? < r.ali_start and ? >r.ali_end))
  and s.auto_dfamseq=r.auto_dfamseq and r.auto_dfamA=a.auto_dfamA and dfamA_acc != ? and in_full=1"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    my %seen;
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {
      next if($seen{$region->{from}.":".$region->{to}.":".$region->{ali}});
      $seen{$region->{from}.":".$region->{to}.":".$region->{ali}}++;
      $sth->execute(
        $seqAcc,       $region->{from}, $region->{from},
        $region->{to}, $region->{to},   $region->{from},
        $region->{to}, $region->{family}
      );
      foreach my $row ( @{ $sth->fetchall_arrayref } ) {
        push(
          @{ $region->{overlap} },
          {
            from      => $row->[0],
            to        => $row->[1],
            family    => $row->[2],
            family_id => $row->[3],
            ali       => 'FULL'
          }
        );
      }
      push( @{ $overlaps->{$seqAcc} }, $region ) if ( $region->{overlap} );
    }
  }
}

sub getOverlapingSeedDfamRegions {
  my ( $self, $regionsHash, $overlaps ) = @_;

#select dfamseq_id, seq_start, seq_end, dfamA_id from dfamA a, dfamA_reg_full r, dfamseq s
#where dfamseq_acc="Q4C4F7" and
#((130 <= r.seq_start and 130 >= r.seq_end) or ( 204 >= r.seq_start and 204 <= r.seq_end) or (130 < r.seq_start and 204 >r.seq_end))
#and s.auto_dfamseq=r.auto_dfamseq and r.auto_dfamA=a.auto_dfamA and in_full=1;

  my $dbh = $self->getSchema->storage->dbh;
  my $sth = $dbh->prepare(
"select distinct seq_start, seq_end, dfamA_acc, dfamA_id from dfamA a, dfamA_reg_seed r, dfamseq s where dfamseq_acc= ? and
  ((? >= r.seq_start and ? <= r.seq_end) or ( ? >= r.seq_start and ? <= r.seq_end) or (? < r.seq_start and ? >r.seq_end))
  and s.auto_dfamseq=r.auto_dfamseq and r.auto_dfamA=a.auto_dfamA and dfamA_acc != ?"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    my %seen;
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {
      next if($seen{$region->{from}.":".$region->{to}.":".$region->{ali}});
      $seen{$region->{from}.":".$region->{to}.":".$region->{ali} }++;
      $sth->execute(
        $seqAcc,       $region->{from}, $region->{from},
        $region->{to}, $region->{to},   $region->{from},
        $region->{to}, $region->{family}
      );
      my $foundOverlap = 0;
      foreach my $row ( @{ $sth->fetchall_arrayref } ) {
        push(
          @{ $region->{overlap} },
          {
            from      => $row->[0],
            to        => $row->[1],
            family    => $row->[2],
            family_id => $row->[3],
            ali       => 'SEED'
          }
        );
        $foundOverlap++;
      }
      push( @{ $overlaps->{$seqAcc} }, $region ) if ($foundOverlap);
    }
  }
}

sub findLowerEvalueRegion {
  my ($self, $seqAcc, $region, $clanAcc, $evalue) = @_;
  
  if($seqAcc =~ /(\S+)\.\d+/){
    $seqAcc = $1;
  }
       
  
  my $dbh = $self->getSchema->storage->dbh;
  
  unless($evalue){
    print STDERR "Performing look up to find evalue of other family\n";
    my $sthE = $dbh->prepare("SELECT domain_evalue_score FROM 
                                      dfamA a, 
                                      dfamA_reg_full_significant r,
                                      dfamseq s
                               WHERE  dfamseq_acc= ? 
                               AND seq_start = ? 
                               AND seq_end = ?
                               AND s.auto_dfamseq=r.auto_dfamseq 
                               AND r.auto_dfamA=a.auto_dfamA
                               AND dfamA_acc = ?");
    $sthE->execute($seqAcc, $region->{from}, $region->{to}, $region->{family});
    my $row = $sthE->fetchrow_arrayref;
    unless(defined( $row ) and $row->[0]){
      die "Failed to get evalue for $seqAcc\n";  
    }
    $evalue = $row->[0];
    print STDERR "$evalue\n";
  }
  
  
  
  my $sthO = $dbh->prepare("SELECT domain_evalue_score FROM 
                              dfamA a, 
                              clans c, 
                              clan_membership m, 
                              dfamA_reg_full_significant r, 
                              dfamseq s 
                            WHERE  dfamseq_acc= ? AND
                            ((? >= r.seq_start and ? <= r.seq_end) or 
                            ( ? >= r.seq_start and ? <= r.seq_end) or 
                            ( ? < r.seq_start and ? >r.seq_end)) and
                            s.auto_dfamseq=r.auto_dfamseq and 
                            r.auto_dfamA=a.auto_dfamA and 
                            dfamA_acc != ? and
                            clan_acc = ? and
                            c.auto_clan = m.auto_clan and
                            m.auto_dfamA = r.auto_dfamA") or confess $dbh->errstr;
 
  $sthO->execute($seqAcc, $region->{from}, $region->{from}, 
                 $region->{to}, $region->{to}, 
                 $region->{from}, $region->{to}, $region->{family}, $clanAcc );

  my $regBetter = 0 ;  
  foreach my $row ( @{ $sthO->fetchall_arrayref } ) {
    if($row->[0] < $evalue){
      $regBetter =1;
      last; 
    }
  }
  
  return $regBetter;
}


sub getDfamRegionsForSeq {
  my ( $self, $seq ) = @_;
  my @dfamRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @dfamRegions =
      $self->getSchema->resultset("DfamaRegFullSignificant")->search(
      {
        "dfamseq.dfamseq_acc" => $seq,
        "in_full"             => 1
      },
      {
        join     => [qw( dfama dfamseq )],
        prefetch => [qw( dfama dfamseq )],
        order_by => 'seq_start'
      }
      );

  }
  elsif ( $seq =~ /\S+_\S+/ ) {
    carp("Looking up Dfam information for $seq. I think this is a seq id")
      if $self->{'debug'};
    @dfamRegions =
      $self->getSchema->resultset("DfamaRegFullSignificant")->search(
      {
        "dfamseq.dfamseq_id" => $seq,
        "in_full"            => 1
      },
      {
        join     => [qw( dfama dfamseq )],
        prefetch => [qw( dfama dfamseq )]
      }
      );

  }
  elsif ( $seq =~ /\S{33}/ ) {
    carp(
      "Looking up Dfam information for $seq. I think this is a seq MD5 checksum"
    ) if $self->{'debug'};
    @dfamRegions =
      $self->getSchema->resultset("DfamaRegFullSignificant")->search(
      {
        "dfamseq.dfamseq_md5" => $seq,
        "in_full"             => 1
      },
      {
        join     => [qw( dfama dfamseq )],
        prefetch => [qw( dfama dfamseq )]
      }
      );
  }
  else {
    cluck("$seq does not look like a family accession or id");
  }
  if ( scalar(@dfamRegions) ) {
    carp("Found family information for $seq") if $self->{'debug'};
    return ( \@dfamRegions );
  }
  carp("Did not find family information for $seq") if $self->{'debug'};
}


sub getDfamseqById {
  my ($self, $id) = @_;
  my $seqObj = $self->getSchema->resultset("Dfamseq")->find({ dfamseq_id => $id });   
  return $seqObj;
}

sub getAllDfamseqAccsIds {
  my ($self) = @_;
  my @dfamseqData;

  carp("Looking up information for all sequences.") if $self->{'debug'};
  @dfamseqData =
    $self->getSchema->resultset("Dfamseq")
    ->search( {}, { select => [qw(auto_dfamseq dfamseq_acc dfamseq_id)] } );

  if (@dfamseqData) {
    carp("Found sequence data") if $self->{'debug'};
    return ( \@dfamseqData );
  }
  else {
    carp("Failed to get sequence data") if $self->{'debug'};
  }
}

sub getAllGiMap {
  my ($self) = @_;
  my @GiData;
  carp("Looking up information for all ncbi seq.") if $self->{'debug'};
  @GiData = $self->getSchema->resultset("NcbiMap")->search();

  if (@GiData) {
    carp("Found gi data") if $self->{'debug'};
    return ( \@GiData );
  }
  else {
    carp("Failed to get Gi data") if $self->{'debug'};
  }
}

sub getAllGiData {
  my ($self) = @_;
  my @GiData;
  carp("Looking up information for all ncbi seq.") if $self->{'debug'};
  @GiData = $self->getSchema->resultset("NcbiSeq")->search();

  if (@GiData) {
    carp("Found gi data") if $self->{'debug'};
    return ( \@GiData );
  }
  else {
    carp("Failed to get Gi data") if $self->{'debug'};
  }
}

sub getAllSecondaryAccs {
  my ($self) = @_;
  my @SecAccData;
  carp("Looking up secondary acc information for all dfamseq.")
    if $self->{'debug'};
  @SecAccData = $self->getSchema->resultset("Secondary_dfamseq_acc")->search();

  if (@SecAccData) {
    carp("Found family data") if $self->{'debug'};
    return ( \@SecAccData );
  }
  else {
    carp("Failed to get secondary accs data") if $self->{'debug'};
  }
}

sub getAllPdbData {
  my ($self) = @_;
  my @pdbData;
  carp("Looking up information for all structures.") if $self->{'debug'};
  @pdbData = $self->getSchema->resultset("Pdb")->search();

  if (@pdbData) {
    carp("Found pdb data") if $self->{'debug'};
    return ( \@pdbData );
  }
  else {
    carp("Failed to get pdb data") if $self->{'debug'};
  }
}

sub getDfamARegFullByAuto {
  my ( $self, $auto_dfamA_reg_full ) = @_;
  my $result =
    $self->getSchema->resultset("DfamaRegFullSignificant")
    ->find( { "auto_dfamA_reg_full" => $auto_dfamA_reg_full, in_full => 1 } );
  if ($result) {
    return ($result);
  }

  carp(
"Did not find region in dfamA_reg_full with auto_dfamA_reg_full '$auto_dfamA_reg_full'\n"
  ) if $self->{'debug'};

}

sub getNestedDomain {
  my ( $self, $acc ) = @_;

  #Look to see if the family exists
  my $dfam =
    $self->getSchema->resultset("Dfama")->find( { dfama_acc => $acc } );

  my @nestedFams;
  if ( $dfam and $dfam->auto_dfama ) {

    #Now search Nested domains in either auto_dfamA or nests_auto_dfama
    my @results = $self->getSchema->resultset("NestedDomains")->search(
      [
        { nests_auto_dfama => $dfam->auto_dfama },
        { auto_dfama       => $dfam->auto_dfama }
      ]
    );

    #Now store the other dfamA_acc
    foreach my $r (@results) {
      if ( $r->auto_dfama->auto_dfama ne $dfam->auto_dfama ) {
        my $ndfam =
          $self->getSchema->resultset("Dfama")
          ->find( { auto_dfama => $r->auto_dfama->auto_dfama } );
        if ($ndfam) {
          push( @nestedFams, $ndfam->dfama_acc );
        }
      }
      else {
        my $ndfam =
          $self->getSchema->resultset("Dfama")
          ->find( { auto_dfama => $r->nests_auto_dfama } );
        push( @nestedFams, $ndfam->dfama_acc );
      }
    }
  }
  return \@nestedFams;
}

sub getAllNestedDomains {
  my ( $self ) = @_;  
  my @results = $self->getSchema->resultset("NestedDomains")->search({});
  
  return(\@results);
}



sub getRegs {
  my( $self, $dfamseq) = @_;
  
  my @results = $self->getSchema->resultset("OtherReg")->search({ auto_dfamseq => $dfamseq});  
  return \@results;
  
}

sub getOtherRegs {
  my( $self, $dfamseq) = @_;
  
  my @results = $self->getSchema->resultset("OtherReg")->search({ auto_dfamseq => $dfamseq});  
  return \@results;
  
}

sub getContextRegionsForSeq {
  my ( $self, $seq ) = @_;
  my @dfamContextRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @dfamContextRegions =
      $self->getSchema->resultset("ContextDfamRegions")->search(
      {
        "auto_dfamseq.dfamseq_acc" => $seq,
      },
      {
        join     => [qw( dfama auto_dfamseq )],
        prefetch => [qw( dfama )]
      }
      );
  }
  return(\@dfamContextRegions);
}


sub getMarkupForSeq {
  my ( $self, $seq ) = @_;
  my @markups;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @markups =
      $self->getSchema->resultset("DfamseqMarkup")->search(
      {
        "dfamseq.dfamseq_acc" => $seq,
      },
      {
        join     => [qw( markup dfamseq )],
        prefetch => [qw( markup dfamseq )]
      }
      );
  }
  return(\@markups);
}

sub getDisulphidesForSeq {
  my ( $self, $seq ) = @_;
  my @markups;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @markups =
      $self->getSchema->resultset("DfamseqDisulphide")->search(
      {
        "auto_dfamseq.dfamseq_acc" => $seq,
      },
      {
        join     => [qw( auto_dfamseq )]
      }
      );
  }
  return(\@markups);
}

sub getDfambRegForSeq {
  my ( $self, $seq ) = @_;
  my @dfamBRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @dfamBRegions =
      $self->getSchema->resultset("DfambReg")->search(
      {
        "auto_dfamseq.dfamseq_acc" => $seq,
      },
      {
        join     => [qw( dfamb auto_dfamseq )],
        prefetch => [qw( dfamb )]
      }
      );
  }
  return(\@dfamBRegions);
}

sub getScoopData {
  my($self, $famDataObj, $score) = @_;
  
  $score = $score || '0.0';
  # DfamA relationship based on SCOOP
  my @ataSCOOP = $self->getSchema
                       ->resultset('Dfama2dfamaScoopResults')
                         ->search( { -and => [
                                  -or => [
                                    "dfamA1.auto_dfama" => $famDataObj->auto_dfama,
                                    "dfamA2.auto_dfama" => $famDataObj->auto_dfama,
                                  ],
                               score       => { '>', $score } ]
                               },
                             { join        => { 'dfamA1' => {
                                                    'clan_memberships' => 'auto_clan' },
                                                'dfamA2' => {
                                                    'clan_memberships' => 'auto_clan' },
                                                  },
                                #[ qw( dfamA1 dfamA2 ) ],
                               select      => [ qw( dfamA1.dfama_id 
                                                    dfamA2.dfama_id
                                                    dfamA1.dfama_acc
                                                    dfamA2.dfama_acc
                                                    auto_clan.clan_acc
                                                    auto_clan.clan_id
                                                    auto_clan_2.clan_acc
                                                    auto_clan_2.clan_id
                                                    score ) ],
                               as          => [ qw( l_dfama_id
                                                    r_dfama_id
                                                    l_dfama_acc
                                                    r_dfama_acc
                                                    l_clan_acc
                                                    l_clan_id
                                                    r_clan_acc
                                                    r_clan_id
                                                    score ) ],
                              order_by     => 'score DESC',
                             } );
  
  return \@ataSCOOP 
}

#Specific insert/update methods should go here

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;

