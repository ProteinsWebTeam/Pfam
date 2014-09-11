
#
# BioPerl module for Bio::Pfam::PfamDBManager
#
# $Author

package Bio::Pfam::PfamDBManager;
use strict;
use warnings;
use PfamDB;
use Data::Dumper;
use Carp;

sub new {
  my $caller    = shift;
  my $class     = ref($caller) || $caller;
  my %dbiParams = ();
  my $self      = {
    user     => "pfam_web_ro",
    host     => "pfamdb1",
    port     => "3306",
    database => "pfam_21_0_web",
    driver   => "mysql",
    debug    => 0,
    @_,
  };

  #print STDERR Dumper($self);

  eval {
    $self->{'schema'} =
      PfamDB->connect( "dbi" . ":"
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

#General shared by all Pfam DB managers
sub id2acc {
  my ( $self, $id ) = @_;
  my $result =
  $self->getSchema->resultset("Pfama")->find( { "pfama_id" => $id } );
  if ( $result && $result->pfama_acc ) {
    return ( $result->pfama_acc );
  }
}

sub acc2id {
  my ( $self, $acc ) = @_;
  my $result =
    $self->getSchema->resultset("Pfama")->find( { "pfama_acc" => $acc } );
  if ( $result && $result->pfama_id ) {
    return ( $result->pfama_id );
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
    $self->getSchema->resultset("Pfama")->find( { "clan_acc" => $acc } );
  if ( $result && $result->clan_id ) {
    return ( $result->clan_id );
  }
}

sub getVersion {
  my ($self) = @_;
  my $version = $self->getSchema->resultset("Version")->search()->first;

  if ( $version and $version->pfam_release_date ) {
    return $version;
  }
  else {
    croak("Failed to get the row from the Version table\n");
  }
}

#Try to find in the incoming family is part of a clan
sub getClanDataByPfam {
  my ( $self, $family ) = @_;
  my $result;
  if ( $family =~ /PF\d+/ ) {

    #Looks like an accession
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $result = $self->getSchema->resultset("ClanMembership")->find(
      { "auto_pfama.pfama_acc" => $family },
      {
        join     => [qw/auto_pfama auto_clan/],
        prefetch => [qw/auto_clan/]
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {

    #Looks like we have a family id
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $result = $self->getSchema->resultset("ClanMembership")->find(
      { "auto_pfama.pfama_id" => $family },
      {
        join     => [qw( auto_pfama auto_clan)],
        prefetch => [qw( auto_clan )]
      }
    );

  }
  else {
    cluck("$family does not look like a pfamA accession or id");
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
        join     => [qw/auto_clan auto_pfama/],
        prefetch => [qw/auto_clan auto_pfama/]
      }
    );

  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    carp("Looking up information for $clan. I think this is an id")
      if $self->{'debug'};
    @clanData = $self->getSchema->resultset("ClanMembership")->search(
      { "auto_clan.clan_id" => $clan },
      {
        join     => qw( auto_clan auto_pfama ),
        prefetch => qw( auto_clan auto_pfama )
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

sub getPfamData {
  my ( $self, $family ) = @_;
  my $familyData;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $familyData =
      $self->getSchema->resultset("Pfama")->find( { "pfama_acc" => $family } );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData =
      $self->getSchema->resultset("Pfama")->find( { "pfama_id" => $family } );
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

sub getAllPfamFamilyData {
  my ($self) = @_;
  my @familyData;
  carp("Looking up information for all families.") if $self->{'debug'};
  @familyData = $self->getSchema->resultset("Pfama")->search();

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

sub getAllPfamFamilyDataLike {
  my ($self, $term) = @_;
  my @familyData;
  carp("Looking up information for all families with and id like .") if $self->{'debug'};
  @familyData = $self->getSchema->resultset("Pfama")->search( { pfama_id => { 'like' => $term } });

  if (@familyData) {
    carp("Found family data") if $self->{'debug'};
    return ( \@familyData );
  }
}


sub getPfamInterPro {
  my ( $self, $family ) = @_;
  my $familyData;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("Pfama")->search(
      { "pfamA_acc" => $family },
      {
        join     => qw( interpros )
      }
    )->first;

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("Pfama")->search(
      { "pfamA_id" => $family },
      {
        join     => qw( interpros )
      }
    )->first;
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

sub getPfamGO {
  my ( $self, $family ) = @_;
  my @familyGO;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("Pfama")->search(
      { "pfamA_acc" => $family },
      {
        join     => qw( gene_ontologies ),
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("Pfama")->search(
      { "pfamA_id" => $family },
      {
        join     => qw( gene_ontologies ),
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
      $self->getSchema->resultset("PfamA_reg_full_significant")->search(
      {
        "pfamA.pfamA_acc" => $family,
        "in_full"         => 1
      },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
      }
      );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyNSE =
      $self->getSchema->resultset("PfamA_reg_full_significant")->search(
      {
        "pfamA.pfamA_id" => $family,
        "in_full"        => 1
      },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
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
    @familyNSE = $self->getSchema->resultset("PfamA_reg_seed")->search(
      { "pfamA.pfamA_acc" => $family },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyNSE = $self->getSchema->resultset("PfamA_reg_seed")->search(
      { "pfamA.pfamA_id" => $family },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
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


sub getSignalPeptideRegion {
    my ($self, $regions_hash) = @_;
    
    my $dbh = $self->getSchema->storage->dbh;
    my $sth = $dbh->prepare("select seq_start, seq_end from other_reg o join pfamseq s on o.pfamseq_acc=s.pfamseq_acc where type_id=\"sig_p\" and pfamseq_acc=?"
  ) or confess $dbh->errstr;

    my %overlap;
    foreach my $pfamseq_acc (keys %$regions_hash) {
      $sth->execute($pfamseq_acc);  
      foreach my $row ( @{ $sth->fetchall_arrayref} ) {
        my ($sigp_start, $sigp_end) = ($row->[0], $row->[1]);
	if($sigp_end >= $regions_hash->{$pfamseq_acc}->{start}) {
	  $overlap{$pfamseq_acc}="$sigp_start-$sigp_end";
	  last;
	}
      }
    }
 
    return \%overlap;
}

sub getOverlapingFullPfamRegions {
  my ( $self, $regionsHash, $overlaps ) = @_;

#select pfamseq_id, seq_start, seq_end, pfamA_id from pfamA a, pfamA_reg_full r, pfamseq s
#where pfamseq_acc="Q4C4F7" and
#((130 <= r.seq_start and 130 >= r.seq_end) or ( 204 >= r.seq_start and 204 <= r.seq_end) or (130 < r.seq_start and 204 >r.seq_end))
#and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and in_full=1;

  my $dbh = $self->getSchema->storage->dbh;
  my $sth = $dbh->prepare(
"select distinct seq_start, seq_end, pfamA_acc, pfamA_id from pfamA a, pfamA_reg_full_significant r, pfamseq s where pfamseq_acc= ? and
  ((? >= r.ali_start and ? <= r.ali_end) or ( ? >= r.ali_start and ? <= r.ali_end) or (? < r.ali_start and ? >r.ali_end))
  and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and pfamA_acc != ? and in_full=1"
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

sub getOverlapingSeedPfamRegions {
  my ( $self, $regionsHash, $overlaps ) = @_;

#select pfamseq_id, seq_start, seq_end, pfamA_id from pfamA a, pfamA_reg_full r, pfamseq s
#where pfamseq_acc="Q4C4F7" and
#((130 <= r.seq_start and 130 >= r.seq_end) or ( 204 >= r.seq_start and 204 <= r.seq_end) or (130 < r.seq_start and 204 >r.seq_end))
#and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and in_full=1;

  my $dbh = $self->getSchema->storage->dbh;
  my $sth = $dbh->prepare(
"select distinct seq_start, seq_end, pfamA_acc, pfamA_id from pfamA a, pfamA_reg_seed r, pfamseq s where pfamseq_acc= ? and
  ((? >= r.seq_start and ? <= r.seq_end) or ( ? >= r.seq_start and ? <= r.seq_end) or (? < r.seq_start and ? >r.seq_end))
  and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and pfamA_acc != ?"
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
                                      pfamA a, 
                                      pfamA_reg_full_significant r,
                                      pfamseq s
                               WHERE  pfamseq_acc= ? 
                               AND seq_start = ? 
                               AND seq_end = ?
                               AND s.auto_pfamseq=r.auto_pfamseq 
                               AND r.auto_pfamA=a.auto_pfamA
                               AND pfamA_acc = ?");
    $sthE->execute($seqAcc, $region->{from}, $region->{to}, $region->{family});
    my $row = $sthE->fetchrow_arrayref;
    unless(defined( $row ) and $row->[0]){
      die "Failed to get evalue for $seqAcc\n";  
    }
    $evalue = $row->[0];
    print STDERR "$evalue\n";
  }
  
  
  
  my $sthO = $dbh->prepare("SELECT domain_evalue_score FROM 
                              pfamA a, 
                              clans c, 
                              clan_membership m, 
                              pfamA_reg_full_significant r, 
                              pfamseq s 
                            WHERE  pfamseq_acc= ? AND
                            ((? >= r.seq_start and ? <= r.seq_end) or 
                            ( ? >= r.seq_start and ? <= r.seq_end) or 
                            ( ? < r.seq_start and ? >r.seq_end)) and
                            s.auto_pfamseq=r.auto_pfamseq and 
                            r.auto_pfamA=a.auto_pfamA and 
                            pfamA_acc != ? and
                            clan_acc = ? and
                            c.auto_clan = m.auto_clan and
                            m.auto_pfamA = r.auto_pfamA") or confess $dbh->errstr;
 
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


sub getPfamRegionsForSeq {
  my ( $self, $seq ) = @_;
  my @pfamRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamaRegFullSignificant")->search(
      {
        "auto_pfamseq.pfamseq_acc" => $seq,
        "in_full"             => 1
      },
      {
        join     => [qw( auto_pfama auto_pfamseq )],
        prefetch => [qw( auto_pfama auto_pfamseq )],
        order_by => 'seq_start'
      }
      );

  }
  elsif ( $seq =~ /\S+_\S+/ ) {
    carp("Looking up Pfam information for $seq. I think this is a seq id")
      if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamaRegFullSignificant")->search(
      {
        "auto_pfamseq.pfamseq_id" => $seq,
        "in_full"            => 1
      },
      {
        join     => [qw( auto_pfama auto_pfamseq )],
        prefetch => [qw( auto_pfama auto_pfamseq )]
      }
      );

  }
  elsif ( $seq =~ /\S{33}/ ) {
    carp(
      "Looking up Pfam information for $seq. I think this is a seq MD5 checksum"
    ) if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamaRegFullSignificant")->search(
      {
        "pfamseq.pfamseq_md5" => $seq,
        "in_full"             => 1
      },
      {
        join     => [qw( pfama pfamseq )],
        prefetch => [qw( pfama pfamseq )]
      }
      );
  }
  else {
    cluck("$seq does not look like a family accession or id");
  }
  if ( scalar(@pfamRegions) ) {
    carp("Found family information for $seq") if $self->{'debug'};
    return ( \@pfamRegions );
  }
  carp("Did not find family information for $seq") if $self->{'debug'};
}


sub getPfamseqById {
  my ($self, $id) = @_;
  my $seqObj = $self->getSchema->resultset("Pfamseq")->find({ pfamseq_id => $id });   
  return $seqObj;
}

sub getAllPfamseqAccsIds {
  my ($self) = @_;
  my @pfamseqData;

  carp("Looking up information for all sequences.") if $self->{'debug'};
  @pfamseqData =
    $self->getSchema->resultset("Pfamseq")
    ->search( {}, { select => [qw(auto_pfamseq pfamseq_acc pfamseq_id)] } );

  if (@pfamseqData) {
    carp("Found sequence data") if $self->{'debug'};
    return ( \@pfamseqData );
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
  carp("Looking up secondary acc information for all pfamseq.")
    if $self->{'debug'};
  @SecAccData = $self->getSchema->resultset("Secondary_pfamseq_acc")->search();

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

sub getPfamARegFullByAuto {
  my ( $self, $auto_pfamA_reg_full ) = @_;
  my $result =
    $self->getSchema->resultset("PfamaRegFullSignificant")
    ->find( { "auto_pfamA_reg_full" => $auto_pfamA_reg_full, in_full => 1 } );
  if ($result) {
    return ($result);
  }

  carp(
"Did not find region in pfamA_reg_full with auto_pfamA_reg_full '$auto_pfamA_reg_full'\n"
  ) if $self->{'debug'};

}

sub getNestedDomain {
  my ( $self, $acc ) = @_;

  #Look to see if the family exists
  my $pfam =
    $self->getSchema->resultset("Pfama")->find( { pfama_acc => $acc } );

  my @nestedFams;
  if ( $pfam and $pfam->pfama_acc ) {

    #Now search Nested domains in either pfamA_acc or nests_pfama_acc
    my @results = $self->getSchema->resultset("NestedDomains")->search(
      [
        { nests_pfama_acc => $pfam->pfama_acc },
        { pfama_acc       => $pfam->pfama_acc }
      ]
    );

    #Now store the other pfamA_acc
    foreach my $r (@results) {
      if ( $r->pfama_acc->pfama_acc ne $pfam->pfama_acc ) {
        my $npfam =
          $self->getSchema->resultset("Pfama")
          ->find( { pfama_acc => $r->pfama_acc->pfama_acc } );
        if ($npfam) {
          push( @nestedFams, $npfam->pfama_acc );
        }
      }
      else {
        my $npfam =
          $self->getSchema->resultset("Pfama")
          ->find( { pfama_acc => $r->nests_pfama_acc } );
        push( @nestedFams, $npfam->pfama_acc );
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
  my( $self, $pfamseq) = @_;
  
  my @results = $self->getSchema->resultset("OtherReg")->search({ auto_pfamseq => $pfamseq});  
  return \@results;
  
}

sub getOtherRegs {
  my( $self, $pfamseq) = @_;
  
  my @results = $self->getSchema->resultset("OtherReg")->search({ auto_pfamseq => $pfamseq});  
  return \@results;
  
}

sub getContextRegionsForSeq {
  my ( $self, $seq ) = @_;
  my @pfamContextRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @pfamContextRegions =
      $self->getSchema->resultset("ContextPfamRegions")->search(
      {
        "auto_pfamseq.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( pfama auto_pfamseq )],
        prefetch => [qw( pfama )]
      }
      );
  }
  return(\@pfamContextRegions);
}


sub getMarkupForSeq {
  my ( $self, $seq ) = @_;
  my @markups;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @markups =
      $self->getSchema->resultset("PfamseqMarkup")->search(
      {
        "pfamseq.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( markup pfamseq )],
        prefetch => [qw( markup pfamseq )]
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
      $self->getSchema->resultset("PfamseqDisulphide")->search(
      {
        "auto_pfamseq.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( auto_pfamseq )]
      }
      );
  }
  return(\@markups);
}

sub getPfambRegForSeq {
  my ( $self, $seq ) = @_;
  my @pfamBRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @pfamBRegions =
      $self->getSchema->resultset("PfambReg")->search(
      {
        "auto_pfamseq.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( pfamb auto_pfamseq )],
        prefetch => [qw( pfamb )]
      }
      );
  }
  return(\@pfamBRegions);
}

sub getScoopData {
  my($self, $famDataObj, $score) = @_;
  
  $score = $score || '0.0';
  # PfamA relationship based on SCOOP
  my @ataSCOOP = $self->getSchema
                       ->resultset('Pfama2pfamaScoopResults')
                         ->search( { -and => [
                                  -or => [
                                    "pfamA1.auto_pfama" => $famDataObj->auto_pfama,
                                    "pfamA2.auto_pfama" => $famDataObj->auto_pfama,
                                  ],
                               score       => { '>', $score } ]
                               },
                             { join        => { 'pfamA1' => {
                                                    'clan_memberships' => 'auto_clan' },
                                                'pfamA2' => {
                                                    'clan_memberships' => 'auto_clan' },
                                                  },
                                #[ qw( pfamA1 pfamA2 ) ],
                               select      => [ qw( pfamA1.pfama_id 
                                                    pfamA2.pfama_id
                                                    pfamA1.pfama_acc
                                                    pfamA2.pfama_acc
                                                    auto_clan.clan_acc
                                                    auto_clan.clan_id
                                                    auto_clan_2.clan_acc
                                                    auto_clan_2.clan_id
                                                    score ) ],
                               as          => [ qw( l_pfama_id
                                                    r_pfama_id
                                                    l_pfama_acc
                                                    r_pfama_acc
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

