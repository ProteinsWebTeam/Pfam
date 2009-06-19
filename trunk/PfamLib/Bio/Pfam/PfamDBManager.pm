
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

  print STDERR "HERE" . Dumper($self);

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
    $self->getSchema->resultset("Pfama")->find( { "pfamA_id" => $id } );
  if ( $result && $result->pfamA_acc ) {
    return ( $result->pfamA_acc );
  }
}

sub acc2id {
  my ( $self, $acc ) = @_;
  my $result =
    $self->getSchema->resultset("Pfama")->find( { "pfamA_acc" => $acc } );
  if ( $result && $result->pfamA_id ) {
    return ( $result->pfamA_id );
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
    $result = $self->getSchema->resultset("Clan_membership")->find(
      { "pfam.pfamA_acc" => $family },
      {
        join     => [qw/pfam clans/],
        prefetch => [qw/clans/]
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {

    #Looks like we have a family id
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $result = $self->getSchema->resultset("Clan_membership")->find(
      { "pfam.pfamA_id" => $family },
      {
        join     => [qw( pfam clans)],
        prefetch => [qw( clans )]
      }
    );

  }
  else {
    cluck("$family does not look like a pfamA accession or id");
  }

  #Return something if we have found something
  if ( $result && $result->clan_acc ) {
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
    @clanData = $self->getSchema->resultset("Clan_membership")->search(
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

  if ($#clanData) {
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

sub getPfamInterPro {
  my ( $self, $family ) = @_;
  my $familyData;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("Pfam")->find(
      { "pfamA_acc" => $family },
      {
        join     => qw( interpro ),
        prefetch => qw( interpro )
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    $familyData = $self->getSchema->resultset("Pfam")->find(
      { "pfamA_id" => $family },
      {
        join     => qw( interpro ),
        prefetch => qw( interpro )
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

sub getPfamGO {
  my ( $self, $family ) = @_;
  my @familyGO;
  if ( $family =~ /PF\d{5}/ ) {
    carp("Looking up information for $family. I think this is an accession")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("Pfam")->search(
      { "pfamA_acc" => $family },
      {
        join     => qw( go ),
        prefetch => qw( go )
      }
    );

  }
  elsif ( $family =~ /\S{1,16}/ ) {
    carp("Looking up information for $family. I think this is an id")
      if $self->{'debug'};
    @familyGO = $self->getSchema->resultset("Pfam")->search(
      { "pfamA_id" => $family },
      {
        join     => qw( go ),
        prefetch => qw( go )
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

sub getOverlapingFullPfamRegions {
  my ( $self, $regionsHash, $overlaps ) = @_;

#select pfamseq_id, seq_start, seq_end, pfamA_id from pfamA a, pfamA_reg_full r, pfamseq s
#where pfamseq_acc="Q4C4F7" and
#((130 <= r.seq_start and 130 >= r.seq_end) or ( 204 >= r.seq_start and 204 <= r.seq_end) or (130 < r.seq_start and 204 >r.seq_end))
#and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and in_full=1;

  my $dbh = $self->getSchema->storage->dbh;
  my $sth = $dbh->prepare(
"select distinct seq_start, seq_end, pfamA_acc, pfamA_id from pfamA a, pfamA_reg_full_significant r, pfamseq s where pfamseq_acc= ? and
  ((? <= r.ali_start and ? >= r.ali_end) or ( ? >= r.ali_start and ? <= r.ali_end) or (? < r.ali_start and ? >r.ali_end))
  and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and pfamA_acc != ? and in_full=1"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {
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
"select distinct seq_start, seq_end, pfamA_acc, pfamA_acc from pfamA a, pfamA_reg_seed r, pfamseq s where pfamseq_acc= ? and
  ((? <= r.seq_start and ? >= r.seq_end) or ( ? >= r.seq_start and ? <= r.seq_end) or (? < r.seq_start and ? >r.seq_end))
  and s.auto_pfamseq=r.auto_pfamseq and r.auto_pfamA=a.auto_pfamA and pfamA_acc != ?"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {
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

sub getPfamRegionsForSeq {
  my ( $self, $seq ) = @_;
  my @pfamRegions;
  if ( $seq =~ /\S{6}/ ) {
    carp("Looking up information for $seq. I think this is an accession")
      if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamA_reg_full_significant")->search(
      {
        "pfamseq.pfamseq_acc" => $seq,
        "in_full"             => 1
      },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
      }
      );

  }
  elsif ( $seq =~ /\S+_\S+/ ) {
    carp("Looking up Pfam information for $seq. I think this is a seq id")
      if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamA_reg_full_significant")->search(
      {
        "pfamseq.pfamseq_id" => $seq,
        "in_full"            => 1
      },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
      }
      );

  }
  elsif ( $seq =~ /\S{33}/ ) {
    carp(
      "Looking up Pfam information for $seq. I think this is a seq MD5 checksum"
    ) if $self->{'debug'};
    @pfamRegions =
      $self->getSchema->resultset("PfamA_reg_full_significant")->search(
      {
        "pfamseq.pfamseq_md5" => $seq,
        "in_full"             => 1
      },
      {
        join     => [qw( pfamA pfamseq )],
        prefetch => [qw( pfamA pfamseq )]
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
  @GiData = $self->getSchema->resultset("Ncbi_map")->search();

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
  @GiData = $self->getSchema->resultset("Ncbi_seq")->search();

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
    $self->getSchema->resultset("PfamA_reg_full")
    ->find( { "auto_pfamA_reg_full" => $auto_pfamA_reg_full } );
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
  if ( $pfam and $pfam->auto_pfama ) {

    #Now search Nested domains in either auto_pfamA or nests_auto_pfama
    my @results = $self->getSchema->resultset("NestedDomains")->search(
      [
        { nests_auto_pfama => $pfam->auto_pfama },
        { auto_pfama       => $pfam->auto_pfama }
      ]
    );

    #Now store the other pfamA_acc
    foreach my $r (@results) {
      if ( $r->auto_pfama->auto_pfama ne $pfam->auto_pfama ) {
        my $npfam =
          $self->getSchema->resultset("Pfama")
          ->find( { auto_pfama => $r->auto_pfama->auto_pfama } );
        if ($npfam) {
          push( @nestedFams, $npfam->pfama_acc );
        }
      }
      else {
        my $npfam =
          $self->getSchema->resultset("Pfama")
          ->find( { auto_pfama => $r->nests_auto_pfama } );
        push( @nestedFams, $npfam->pfama_acc );
      }
    }
  }
  return \@nestedFams;
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

