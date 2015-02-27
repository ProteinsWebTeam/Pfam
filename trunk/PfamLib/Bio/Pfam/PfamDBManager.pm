
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
use DDP;

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
  $self->getSchema->resultset("PfamA")->find( { "pfama_id" => $id } );
  if ( $result && $result->pfama_acc ) {
    return ( $result->pfama_acc );
  }
}

sub acc2id {
  my ( $self, $acc ) = @_;
  my $result =
    $self->getSchema->resultset("PfamA")->find( { "pfama_acc" => $acc } );
  if ( $result && $result->pfama_id ) {
    return ( $result->pfama_id );
  }
}

sub clanId2Acc {
  my ( $self, $id ) = @_;
  my $result =
  $self->getSchema->resultset("Clan")->find( { "clan_id" => $id } );
  if ( $result && $result->clan_acc ) {
    return ( $result->clan_acc );
  }
}

sub clanAcc2Id {
  my ( $self, $acc ) = @_;
  my $result =
    $self->getSchema->resultset("PfamA")->find( { "clan_acc" => $acc } );
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
      { "pfama_acc.pfama_acc" => $family },
      {
        join     => [qw/pfama_acc clan_acc/],
        prefetch => [qw/clan_acc/]
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {

    #Looks like we have a family id
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $result = $self->getSchema->resultset("ClanMembership")->find(
      { "pfama_acc.pfama_id" => $family },
      {
        join     => [qw( pfama_acc clan_acc)],
        prefetch => [qw( clan_acc )]
      }
    );

  }
  else {
    cluck("$family does not look like a pfamA accession or id");
  }

  #Return something if we have found something
  if ( $result && $result->clan_acc->clan_acc ) {
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
      $self->getSchema->resultset("Clan")->find( { "clan_acc" => $clan } );

  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    carp("Looking up information for $clan. I think this is an id")
      if $self->{'debug'};
    $clanData =
      $self->getSchema->resultset("Clan")->find( { "clan_id" => $clan } );
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
      { "clan_acc.clan_acc" => $clan },
      {
        join     => [qw/clan_acc pfama_acc/],
        prefetch => [qw/clan_acc pfama_acc/]
      }
    );

  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    carp("Looking up information for $clan. I think this is an id")
      if $self->{'debug'};
    @clanData = $self->getSchema->resultset("ClanMembership")->search(
      { "clan_acc.clan_id" => $clan },
      {
        join     => qw( clan_acc pfama_acc ),
        prefetch => qw( clan_acc pfama_acc )
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
  my @clanData = $self->getSchema->resultset("Clan")->search();
  return \@clanData;
}

sub getAllDeadClanData {
  my ($self) = shift;
  my @deadClanData = $self->getSchema->resultset("DeadClan")->search();
  return \@deadClanData;
}

sub getPfamData {
  my ( $self, $family ) = @_;
  my $familyData;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $familyData =
      $self->getSchema->resultset("PfamA")->find( { "pfama_acc" => $family } );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData =
      $self->getSchema->resultset("PfamA")->find( { "pfama_id" => $family } );
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
  @familyData = $self->getSchema->resultset("PfamA")->search();

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
  @familyData = $self->getSchema->resultset("DeadFamily")->search();

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
  @familyData = $self->getSchema->resultset("PfamA")->search( { pfama_id => { 'like' => $term } });

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
    $familyData = $self->getSchema->resultset("PfamA")->search(
      { "pfamA_acc" => $family },
      {
        join     => qw( interpros )
      }
    )->first;

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("PfamA")->search(
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
    @familyGO = $self->getSchema->resultset("PfamA")->search(
      { "pfamA_acc" => $family },
      {
        join     => qw( gene_ontologies ),
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("PfamA")->search(
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
      $self->getSchema->resultset("PfamARegFullSignificant")->search(
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
      $self->getSchema->resultset("PfamARegFullSignificant")->search(
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
    @familyNSE = $self->getSchema->resultset("PfamARegSeed")->search(
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
    @familyNSE = $self->getSchema->resultset("PfamARegSeed")->search(
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
    my $sth = $dbh->prepare("select seq_start, seq_end from other_reg o join pfamseq s on o.pfamseq_acc=s.pfamseq_acc where type_id=\"sig_p\" and s.pfamseq_acc=?"
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
"select distinct seq_start, seq_end, ali_start, ali_end, domain_bits_score, a.pfamA_acc, pfamA_id from pfamA a, pfamA_reg_full_significant r where r.pfamseq_acc=? and
  ((? >= r.ali_start and ? <= r.ali_end) or ( ? >= r.ali_start and ? <= r.ali_end) or (? < r.ali_start and ? >r.ali_end)) and
  r.pfamA_acc=a.pfamA_acc and a.pfamA_acc != ? and in_full=1"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    my %seen;
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {

      #Use start, ends for seed, and alignment co-ordinates for full
      my ($st, $en);
      if($region->{ali} eq "SEED") {
	$st=$region->{from};
	$en=$region->{to};
      }
      else {
	$st=$region->{ali_from};
	$en=$region->{ali_to};
      }
      unless($st and $en) {
	die "$seqAcc ". $region->{ali} . " has no start/end\n";
      }
      
      next if($seen{"$st:$en:".$region->{ali}});
      $seen{"$st:$en:".$region->{ali}}++;


      $sth->execute($seqAcc, $st, $st, $en, $en, $st, $en, $region->{family});
      foreach my $row ( @{ $sth->fetchall_arrayref } ) {
        push(
          @{ $region->{overlap} },
          {
	   from      => $row->[0],
	   to        => $row->[1],
	   ali_from  => $row->[2],
	   ali_to    => $row->[3],
	   score     => $row->[4],
	   family    => $row->[5],
	   family_id => $row->[6],
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
"select distinct seq_start, seq_end, a.pfamA_acc, pfamA_id from pfamA a, pfamA_reg_seed r where r.pfamseq_acc=? and
  ((? >= r.seq_start and ? <= r.seq_end) or ( ? >= r.seq_start and ? <= r.seq_end) or (? < r.seq_start and ? >r.seq_end))
  and r.pfamA_acc=a.pfamA_acc and a.pfamA_acc != ?"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    my %seen;
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {

      #Use start, ends for seed, and alignment co-ordinates for full
      my ($st, $en);
      if($region->{ali} eq "SEED") {
	$st=$region->{from};
	$en=$region->{to};
      }
      else {
	$st=$region->{ali_from};
	$en=$region->{ali_to};
      }
      unless($st and $en) {
	die "$seqAcc ". $region->{ali} . " has no start/end\n";
      }
      
      next if($seen{"$st:$en:".$region->{ali}});
      $seen{"$st:$en:".$region->{ali} }++;

      $sth->execute($seqAcc, $st, $st, $en, $en, $st, $en, $region->{family});
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
    my $sthE = $dbh->prepare("SELECT domain_evalue_score FROM 
                                      pfamA a, 
                                      pfamA_reg_full_significant r
                               WHERE  pfamseq_acc= ? 
                               AND seq_start = ? 
                               AND seq_end = ?
                               AND r.pfamA_acc=a.pfamA_acc
                               AND r.pfamA_acc = ?");
    $sthE->execute($seqAcc, $region->{from}, $region->{to}, $region->{family});
    my $row = $sthE->fetchrow_arrayref;
    unless(defined( $row ) and $row->[0]){
      die "Failed to get evalue for $seqAcc\n";  
    }
    $evalue = $row->[0];
  }
  
  
  
  my $sthO = $dbh->prepare("SELECT domain_evalue_score FROM 
                              pfamA a, 
                              clan c, 
                              clan_membership m, 
                              pfamA_reg_full_significant r
                            WHERE  pfamseq_acc= ? AND
                            ((? >= r.ali_start and ? <= r.ali_end) or 
                            ( ? >= r.ali_start and ? <= r.ali_end) or 
                            ( ? < r.ali_start and ? >r.ali_end)) and
                            r.pfamA_acc=a.pfamA_acc and 
                            r.pfamA_acc != ? and
                            m.clan_acc = ? and
                            c.clan_acc = m.clan_acc and
                            m.pfamA_acc = r.pfamA_acc") or confess $dbh->errstr;
 
  $sthO->execute($seqAcc, $region->{ali_from}, $region->{ali_from}, 
                 $region->{ali_to}, $region->{ali_to}, 
                 $region->{ali_from}, $region->{ali_to}, $region->{family}, $clanAcc );

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
      $self->getSchema->resultset("PfamARegFullSignificant")->search(
      {
        "pfamseq_acc" => $seq,
        "in_full"             => 1
      },
      {
        order_by => 'seq_start'
      }
      );

  }
  elsif ( $seq =~ /\S+_\S+/ ) {
    carp("Looking up Pfam information for $seq. I think this is a seq id")
      if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamARegFullSignificant")->search(
      {
        "pfamseq_acc.pfamseq_id" => $seq,
        "in_full"            => 1
      },
      {
        join     => [qw( pfamA_acc pfamseq_acc )],
        prefetch => [qw( pfamA_acc pfamseq_acc )]
      }
      );

  }
  elsif ( $seq =~ /\S{33}/ ) {
    carp(
      "Looking up Pfam information for $seq. I think this is a seq MD5 checksum"
    ) if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamARegFullSignificant")->search(
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
    ->search( {}, { select => [qw(pfamseq_acc pfamseq_id)] } ); #removed first item from this array (auto_pfamseq) - may cause problems if scripts calling this method expect a 3 item array in return

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
  @SecAccData = $self->getSchema->resultset("SecondaryPfamseqAcc")->search();

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

#probably not needed any more.....

#sub getPfamARegFullByAuto {
#  my ( $self, $auto_pfamA_reg_full ) = @_;
#  my $result =
#    $self->getSchema->resultset("PfamaRegFullSignificant")
#    ->find( { "auto_pfamA_reg_full" => $auto_pfamA_reg_full, in_full => 1 } );
#  if ($result) {
#    return ($result);
#  }
#
#  carp(
#"Did not find region in pfamA_reg_full with auto_pfamA_reg_full '$auto_pfamA_reg_full'\n"
#  ) if $self->{'debug'};
#
#}

sub getNestedDomain {
  my ( $self, $acc ) = @_;

  #Look to see if the family exists
  my $pfam =
    $self->getSchema->resultset("PfamA")->find( { pfama_acc => $acc } );

  my @nestedFams;
  if ( $pfam and $pfam->pfama_acc ) {

    #Now search Nested domains in either pfamA_acc or nests_pfama_acc
    my @results = $self->getSchema->resultset("NestedDomain")->search(
      [
        { nests_pfama_acc => $pfam->pfama_acc },
        { pfama_acc       => $pfam->pfama_acc }
      ]
    );
 

    #Now store the other pfamA_acc
    foreach my $r (@results) {
      if ( $r->pfama_acc->pfama_acc ne $pfam->pfama_acc ) {
        my $npfam =
          $self->getSchema->resultset("PfamA")
          ->find( { pfama_acc => $r->pfama_acc->pfama_acc } );
        if ($npfam) {
          push( @nestedFams, $npfam->pfama_acc );
        }
      }
      else {

#TODO - this is hashed out as it caused check in to fail - @nested families should contain accs. Remove this when all testing done
#        my $npfam =
#          $self->getSchema->resultset("PfamA")
#          ->find( { pfama_acc => $r->nests_pfama_acc } );

#        push( @nestedFams, $npfam->pfama_acc );

	my $npfam = $r->nests_pfama_acc->pfama_acc;
        push( @nestedFams, $npfam);
      }
    }
  }
  return \@nestedFams;
}

sub getAllNestedDomains {
  my ( $self ) = @_;  
  my @results = $self->getSchema->resultset("NestedDomain")->search({});
  
  return(\@results);
}



sub getRegs {
  my( $self, $pfamseq) = @_;
  
  my @results = $self->getSchema->resultset("OtherReg")->search({ pfamseq_acc => $pfamseq});  
  return \@results;
  
}

sub getOtherRegs {
  my( $self, $pfamseq) = @_;
  
  my @results = $self->getSchema->resultset("OtherReg")->search({ pfamseq_acc => $pfamseq});  
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
        "pfamseq_acc.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( pfama pfamseq_acc )],
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
        pfamseq_acc => $seq,
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
        "pfamseq_acc.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( pfamseq_acc )]
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
      $self->getSchema->resultset("PfamBReg")->search(
      {
        "pfamseq_acc.pfamseq_acc" => $seq,
      },
      {
        join     => [qw( pfamb pfamseq_acc )],
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
                       ->resultset('PfamA2pfamAScoop')
                         ->search( { -and => [
                                  -or => [
                                    "pfamA1.pfamA_acc" => $famDataObj->pfamA_acc,
                                    "pfamA2.pfamA_acc" => $famDataObj->pfamA_acc,
                                  ],
                               score       => { '>', $score } ]
                               },
                             { join        => { 'pfamA1' => {
                                                    'clan_memberships' => 'clan_acc' },
                                                'pfamA2' => {
                                                    'clan_memberships' => 'clan_acc' },
                                                  },
                                #[ qw( pfamA1 pfamA2 ) ],
                               select      => [ qw( pfamA1.pfama_id 
                                                    pfamA2.pfama_id
                                                    pfamA1.pfama_acc
                                                    pfamA2.pfama_acc
                                                    clan_acc.clan_acc
                                                    clan_acc.clan_id
                                                    clan_acc_2.clan_acc
                                                    clan_acc_2.clan_id
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

