#
# BioPerl module for Bio::Pfam::PfamLiveDBManager
#
# $Author: rdf $

package Bio::Pfam::PfamLiveDBManager;

use Data::Dumper;
use Carp qw(cluck croak carp confess);

use base Bio::Pfam::PfamDBManager;
use PfamLive;
use strict;
use warnings;
use Compress::Zlib;

sub new {
  my $caller    = shift;
  my $class     = ref($caller) || $caller;
  my %dbiParams = ();
  my $self      = {
    user     => "pfamro",
    host     => "pfamdb2a",
    port     => "3303",
    database => "pfamH3live",
    driver   => "mysql",
    @_,
  };

  carp( "The new object contains :" . Dumper($self) ) if ( $self->{'debug'} );
  eval {
    $self->{'schema'} =
      PfamLive->connect( "dbi" . ":"
        . $self->{driver} . ":"
        . $self->{database} . ":"
        . $self->{host} . ":"
        . $self->{port},
      $self->{user}, $self->{password}, \%dbiParams );
  };
  if ($@) {
    croak("Failed to get schema for database:"
        . $self->{'database'}
        . ". Error:[$@]\n" );
  }
  return bless( $self, $caller );
}

#
# Select methods specific to Pfamlive should go in here.
#

sub getClanLockData {
  my ( $self, $clan ) = @_;

  my $lockData;
  if ( $clan =~ /CL\d{4}/ ) {
    $lockData = $self->getSchema->resultset("Clan_locks")->find(
      { "clans.clan_acc" => $clan },
      {
        join     => [qw/clans/],
        prefetch => [qw/clans/]
      }
    );
  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    $self->getSchema->resultset->find(
      { "clans.clan_id" => $clan },
      {
        join     => [qw/clans/],
        prefetch => [qw/clans/]
      }
    );
  }
  else {
    croak("$clan does not look like a clan accession or identifer\n");
  }

  return $lockData if ( ref($lockData) );
}

#
# Specific insert/update methods should go here
#
sub updateClanMembership {
  my ( $self, $autoClan, $autoPfamA ) = @_;
  my ($result);
  carp(
    "Updating clan membership with auto_clan: $autoClan, auto_pfamA: $autoPfamA"
  ) if ( $self->{'debug'} );
  if ( $autoClan && $autoPfamA ) {
    $result = $self->getSchema->resultset('ClanMembership')->find_or_create(
      {
        auto_clan  => $autoClan,
        auto_pfama => $autoPfamA
      },
      { 
        key => 'clanMembConst'
      }
    );
  }
  else {
    cluck(
      "Can not update clan_membership without both auto_pfamA and auto_clan!");
  }
  return ($result);
}

sub removeFamilyFromClanMembership {
  my ( $self, $autoClan, $autoPfamA ) = @_;
  my ($result);
  carp(
    "Removing family from clan membership: $autoClan, auto_pfamA: $autoPfamA")
    if ( $self->{'debug'} );
  if ( $autoClan && $autoPfamA ) {
    $result = $self->getSchema->resultset('Clan_membership')->find(
      {
        auto_clan  => $autoClan,
        auto_pfamA => $autoPfamA
      }
    )->delete;

    print STDERR "\n\n****** $result ******\n\n"
      if ( $result->isa("DBIx::Class::Row") );
  }
  else {
    cluck(
"Can not remove family from clan_membership without both auto_pfamA and auto_clan!"
    );
  }
  return ($result);
}

sub removeClan {
  my ( $self, $autoClan ) = @_;

  my $result;
  if ($autoClan) {
    $result =
      $self->getSchema->resultset('Clans')->find( { auto_clan => $autoClan } )
      ->delete;
  }
  else {
    cluck(
"Can not remove family from clan_membership without both auto_pfamA and auto_clan!"
    );
  }
  return ($result);
}

#TODO
sub updateClan {
  my( $self, $clanObj) = @_;
  
  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }
  
  my $clan = $self->getSchema
                  ->resultset('Clans')->find( { clan_acc => $clanObj->DESC->AC } );
  $clan->update({
      clan_acc         => $clanObj->DESC->AC,
      clan_id          => $clanObj->DESC->ID,
      previous_id      => defined($clanObj->DESC->PI) ? $clanObj->DESC->PI : '',
      clan_description => $clanObj->DESC->DE,
      clan_author      => $clanObj->DESC->AU,
      clan_comment     => defined($clanObj->DESC->CC) ? $clanObj->DESC->CC : '',
    });
  
  #Add the auto number to the clan Obj.
  $clanObj->rdb( { auto => $clan->auto_clan } );
}

sub createClan {
  my( $self, $clanObj, $depositor) = @_;
  
  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }
  
  my $clan = $self->getSchema->resultset('Clans')->create(
    {
      clan_acc        => $clanObj->DESC->AC,
      clan_id         => $clanObj->DESC->ID,
      previous_id     => defined($clanObj->DESC->PI) ? $clanObj->DESC->PI : '',
      clan_desription => $clanObj->DESC->DE,
      clan_author     => $clanObj->DESC->AU,
      deposited_by    => $depositor,
      clan_comment    => defined($clanObj->DESC->CC) ? $clanObj->DESC->CC : '',
      created         => \'NOW()',
      competed        => 0
    });
  
  unless ( $clan and $clan->isa('PfamLive::Clans') ) {
    confess( 'Failed to get row for ' . $clanObj->DESC->ID . "....." );
  }

  #Add the auto number to the clan Obj.
  $clanObj->rdb( { auto => $clan->auto_clan } );
}

sub updatePfamA {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  my $pfamA =
    $self->getSchema->resultset('Pfama')
    ->find( { pfamA_acc => $famObj->DESC->AC } );

  unless ( $pfamA and $pfamA->isa('PfamLive::Pfama') ) {
    confess( 'Failed to get row for ' . $famObj->DESC->AC . "$pfamA....." );
  }

  #In the results set object, update all of the fields from the DESC object
  $pfamA->pfama_acc( $famObj->DESC->AC );
  $pfamA->pfama_id( $famObj->DESC->ID );
  $pfamA->description( $famObj->DESC->DE );
  $pfamA->author( $famObj->DESC->AU );
  $pfamA->seed_source( $famObj->DESC->SE );
  $pfamA->type( $famObj->DESC->TP );
  $pfamA->sequence_tc( $famObj->DESC->CUTTC->{seq} );
  $pfamA->domain_tc( $famObj->DESC->CUTTC->{dom} );
  $pfamA->sequence_ga( $famObj->DESC->CUTGA->{seq} );
  $pfamA->domain_ga( $famObj->DESC->CUTGA->{dom} );
  $pfamA->sequence_nc( $famObj->DESC->CUTNC->{seq} );
  $pfamA->domain_nc( $famObj->DESC->CUTNC->{dom} );
  $pfamA->buildmethod( $famObj->DESC->BM );
  $pfamA->searchmethod( $famObj->DESC->SM );
  $pfamA->comment( $famObj->DESC->CC );
  $pfamA->previous_id( $famObj->DESC->PI ? $famObj->DESC->PI : '' );

  #Now update the HMM stuff;
  $pfamA->mu( $famObj->HMM->mu );
  $pfamA->tau( $famObj->HMM->tau );
  $pfamA->lambda( $famObj->HMM->lambda );
  $pfamA->model_length( $famObj->HMM->length );

  #Now update the numbers in the SEED and FULL
  $pfamA->num_seed( $famObj->SEED->no_sequences );
  $pfamA->num_full( $famObj->ALIGN->no_sequences );

  $famObj->rdb( { auto => $pfamA->auto_pfama } );
  $pfamA->update;

}

sub createPfamA {
  my ( $self, $famObj, $depositor ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  my $pfamA = $self->getSchema->resultset('Pfama')->create(
    {
      pfama_acc    => $famObj->DESC->AC,
      pfama_id     => $famObj->DESC->ID,
      description  => $famObj->DESC->DE,
      author       => $famObj->DESC->AU,
      deposited_by => $depositor,
      seed_source  => $famObj->DESC->SE,
      type         => $famObj->DESC->TP,
      sequence_tc  => $famObj->DESC->CUTTC->{seq},
      domain_tc    => $famObj->DESC->CUTTC->{dom},
      sequence_ga  => $famObj->DESC->CUTGA->{seq},
      domain_ga    => $famObj->DESC->CUTGA->{dom},
      sequence_nc  => $famObj->DESC->CUTNC->{seq},
      domain_nc    => $famObj->DESC->CUTNC->{dom},
      buildmethod  => $famObj->DESC->BM,
      searchmethod => $famObj->DESC->SM,
      comment      => $famObj->DESC->CC,
      previous_id  => $famObj->DESC->PI ? $famObj->DESC->PI : '',
      mu           => $famObj->HMM->mu,
      tau          => $famObj->HMM->tau,
      lambda       => $famObj->HMM->lambda,
      model_length => $famObj->HMM->length,
      num_seed     => $famObj->SEED->no_sequences,
      num_full     => $famObj->ALIGN->no_sequences,
      created      => \'NOW()',
    }
  );

  unless ( $pfamA and $pfamA->isa('PfamLive::Pfama') ) {
    confess( 'Failed to get row for ' . $famObj->DESC->ID . "$pfamA....." );
  }

  #Add the auto number to the famObj.
  $famObj->rdb( { auto => $pfamA->auto_pfama } );
}

sub movePfamA {
  my ( $self, $fromFamily, $toFamily ) = @_;

  my $fromFamObj = $self->getPfamData($fromFamily);

  $fromFamObj->previous_id(
    defined( $fromFamObj->previous_id )
    ? $fromFamObj->previous_id . " $fromFamily;"
    : "$fromFamily;"
  );
  $fromFamObj->pfama_id($toFamily);
  $fromFamObj->update;

}

sub deletePfamA {
  my ( $self, $family, $comment, $forward, $user ) = @_;
  my $pfamA =
    $self->getSchema->resultset('Pfama')->find( { pfama_acc => $family } );

  unless ( $pfamA and $pfamA->isa('PfamLive::Pfama') ) {
    confess( 'Failed to get row for ' . $family . "$pfamA....." );
  }
  $self->getSchema->resultset('Pfama')->find( { pfama_acc => $family } )
    ->delete;

  #Now make the dead_families entry
  $self->getSchema->resultset('DeadFamilies')->create(
    {
      pfama_id   => $pfamA->pfama_id,
      pfama_acc  => $pfamA->pfama_acc,
      comment    => $comment,
      forward_to => $forward,
      user       => $user,
      killed     => \'NOW()'
    }
  );

}

sub deleteClan {
  my ( $self, $clanAcc, $comment, $forward, $user ) = @_;
  my $clan =
    $self->getSchema->resultset('Clans')->find( { clan_acc => $clanAcc } );

  unless ( $clan and $clan->isa('PfamLive::Clans') ) {
    confess( 'Failed to get row for ' . $clanAcc . "( Got $clan )....." );
  }
  my $clanMembership = $self->getClanMembership($clanAcc);
  
  my $memberString;
  foreach my $mem (@$clanMembership){
    $memberString .= $mem->auto_pfama->pfama_acc." ";    
  }
  
  $clan->delete;

  #Now make the dead_clans entry
  $self->getSchema->resultset('DeadClans')->create(
    {
      clan_id          => $clan->clan_id,
      clan_acc         => $clan->clan_acc,
      clan_description => $clan->clan_description,
      clan_membership  => $memberString,
      comment          => $comment,
      forward_to       => $forward,
      user             => $user,
      killed           => \'NOW()'
    }
  );

}

sub updatePfamARegSeed {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }
  my @oldRegions = $self->getSchema->resultset('PfamaRegSeed')->search(
    { auto_pfama => $auto },
    {
      select => [qw(me.auto_pfamseq pfamseq_acc seq_version)],
      as     => [qw( auto_pfamseq pfamseq_acc seq_version )],
      join   => [qw(auto_pfamseq)]
    }
  );

  my %seqacc2auto;
  foreach my $r (@oldRegions) {
    $seqacc2auto{ $r->get_column('pfamseq_acc') . "."
        . $r->get_column('seq_version') } = $r->get_column('auto_pfamseq');
  }

  $self->getSchema->resultset('PfamaRegSeed')->search( { auto_pfama => $auto } )
    ->delete;

  my $dbh = $self->getSchema->storage->dbh;

  my $seq_sth = $dbh->prepare(
    'select auto_pfamseq from pfamseq where pfamseq_acc = ? and seq_version = ?'
  );
  my $up_sth = $dbh->prepare(
    'insert into pfamA_reg_seed 
      (auto_pfamseq, auto_pfamA, seq_start, seq_end) values ( ?, ?, ?, ?)'
  );

  foreach my $seq ( $famObj->SEED->each_seq ) {
    my $sauto;
    if ( $seqacc2auto{ $seq->id . "." . $seq->seq_version } ) {
      $sauto = $seqacc2auto{ $seq->id . "." . $seq->seq_version };
    }
    else {
      $seq_sth->execute( $seq->id, $seq->version );
      my $row = $seq_sth->fetchrow_arrayref;
      unless ($row) {
        confess( "Failed to find entry in pfamseq for "
            . $seq->id . "."
            . $seq->seq_version
            . "\n" );
      }
      $sauto = $row->[0];
    }
    $up_sth->execute( $sauto, $auto, $seq->start, $seq->end );
  }
}

sub updatePfamARegFull {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
#Get all previous regions for this family from pfamA_reg_fill_significant and
#pfamA_reg_full insignificant and hash the seq acc + version to index

  #Signfiicant table first
  my @oldRegions =
    $self->getSchema->resultset('PfamaRegFullSignificant')->search(
    { auto_pfama => $auto },
    {
      select => [qw(me.auto_pfamseq pfamseq_acc seq_version)],
      as     => [qw( auto_pfamseq pfamseq_acc seq_version )],
      join   => [qw(auto_pfamseq)]
    }
    );

  my %seqacc2auto;
  foreach my $r (@oldRegions) {
    $seqacc2auto{ $r->get_column('pfamseq_acc') . "."
        . $r->get_column('seq_version') } = $r->get_column('auto_pfamseq');
  }

  #Now the insignificant table
  my @oldInRegions =
    $self->getSchema->resultset('PfamaRegFullInsignificant')->search(
    { auto_pfama => $auto },
    {
      select => [qw(me.auto_pfamseq pfamseq_acc seq_version)],
      as     => [qw( auto_pfamseq pfamseq_acc seq_version )],
      join   => [qw(auto_pfamseq)]
    }
    );

  foreach my $r (@oldInRegions) {
    $seqacc2auto{ $r->get_column('pfamseq_acc') . "."
        . $r->get_column('seq_version') } = $r->get_column('auto_pfamseq');
  }

#-------------------------------------------------------------------------------
#Now delete all regions in the two tables

#The pdb region has a FK to pfamA_reg_full_significant so this need be deleted first.
  $self->getSchema->resultset('PdbPfamaReg')->search( { auto_pfama => $auto } )
    ->delete;

  $self->getSchema->resultset('PfamaRegFullSignificant')
    ->search( { auto_pfama => $auto } )->delete;

  $self->getSchema->resultset('PfamaRegFullInsignificant')
    ->search( { auto_pfama => $auto } )->delete;

#-------------------------------------------------------------------------------
#As we are going to have to perform this upto 100K times
#it is much faster to use place holders
  my $dbh = $self->getSchema->storage->dbh;

  my $seq_sth = $dbh->prepare(
    'select auto_pfamseq from pfamseq where pfamseq_acc = ? and seq_version = ?'
  );

  my $upSigSth = $dbh->prepare(
    'INSERT INTO pfamA_reg_full_significant
    (auto_pfamA, 
    auto_pfamseq, 
    seq_start, 
    seq_end, 
    ali_start, 
    ali_end, 
    model_start,
    model_end,
    domain_bits_score,
    domain_evalue_score,
    sequence_bits_score,
    sequence_evalue_score,
    in_full ) 
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'
  );

  my $upInsigSth = $dbh->prepare(
    'INSERT INTO pfamA_reg_full_insignificant
    (auto_pfamA, 
    auto_pfamseq, 
    seq_start, 
    seq_end, 
    model_start,
    model_end,
    domain_bits_score,
    domain_evalue_score,
    sequence_bits_score,
    sequence_evalue_score) 
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'
  );

#-------------------------------------------------------------------------------
#All of the quires are set up now prepare the data

#Find out which sequences have made it in to the full alignment from the scores file
  my $inFullHash;
  my $regions = $famObj->scores->regions;
  foreach my $seq ( keys %{$regions} ) {
    foreach my $se ( @{ $regions->{$seq} } ) {
      $inFullHash->{ $seq . "/" . $se->{start} . "-" . $se->{end} }++;
    }
  }

  foreach my $seq ( @{ $famObj->PFAMOUT->eachHMMSeq } ) {

    #Get the auto number for this sequence
    my $sauto;
    if ( $seqacc2auto{ $seq->name } ) {
      $sauto = $seqacc2auto{ $seq->name };
    }
    else {
      my ( $acc, $version ) = $seq->name =~ /(\S+)\.(\d+)/;
      $seq_sth->execute( $acc, $version );
      my $row = $seq_sth->fetchrow_arrayref;
      unless ($row) {
        confess( "Failed to find entry in pfamseq for "
            . $seq->id . "."
            . $seq->seq_version
            . "\n" );
      }
      $sauto = $row->[0];
    }

    if ( $seq->bits >= $famObj->DESC->CUTGA->{seq} ) {
      foreach my $u ( @{ $seq->hmmUnits } ) {

        #Is it significant dom?
        if ( $u->bits >= $famObj->DESC->CUTGA->{dom} ) {
          $upSigSth->execute(
            $auto,
            $sauto,
            $u->envFrom,
            $u->envTo,
            $u->seqFrom,
            $u->seqTo,
            $u->hmmFrom,
            $u->hmmTo,
            $u->bits,
            $u->evalue,
            $seq->bits,
            $seq->evalue,
            $inFullHash->{ $u->name . "/" . $u->envFrom . "-" . $u->envTo }
            ? 1
            : 0
          );

        }
        else {

          #Although the sequence is significant this regions is insignificant
          $upInsigSth->execute(
            $auto,       $sauto,    $u->envFrom, $u->envTo,
            $u->hmmFrom, $u->hmmTo, $u->bits,    $u->evalue,
            $seq->bits,  $seq->evalue
          );

        }
      }
    }
    else {

      #Sequence is insignifcant....Therefore all the domains have to be.
      foreach my $u ( @{ $seq->hmmUnits } ) {
        $upInsigSth->execute(
          $auto,     $sauto,   $u->envFrom, $u->envTo,  $u->hmmFrom,
          $u->hmmTo, $u->bits, $u->evalue,  $seq->bits, $seq->evalue
        );
      }
    }
  }
}

sub updateNcbiPfamA {

  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;

    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }


  $self->getSchema->resultset('NcbiPfamaReg')
    ->search( { auto_pfama => $auto } )->delete;


  my $dbh = $self->getSchema->storage->dbh;

  my $upSth = $dbh->prepare(
    'INSERT INTO ncbi_pfamA_reg
    (auto_pfamA,        
     gi,                   
     seq_start,            
     seq_end,
     ali_start,
     ali_end,              
     model_start,          
     model_end,            
     domain_bits_score,    
     domain_evalue_score,  
     sequence_bits_score,  
     sequence_evalue_score)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)');


  foreach my $seq ( @{ $famObj->PFAMOUT->eachHMMSeq } ) {

      if ( $seq->bits >= $famObj->DESC->CUTGA->{seq} ) {


	  foreach my $u ( @{ $seq->hmmUnits } ) {
	      
	      #Is it significant dom?
	      if ( $u->bits >= $famObj->DESC->CUTGA->{dom} ) {
		  $upSth->execute(
				  $auto,
				  $seq->name,
				  $u->envFrom,
				  $u->envTo,
				  $u->seqFrom,
				  $u->seqTo,
				  $u->hmmFrom,
				  $u->hmmTo,
				  $u->bits,
				  $u->evalue,
				  $seq->bits,
				  $seq->evalue,				  				  				  
				  );
		  
	      }	      	      	      
	  }
      }      
  }
}


sub updateMetaPfamA {

  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;

    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }



  my @oldRegions =
    $self->getSchema->resultset('MetaPfamaReg')->search ({ auto_pfama => $auto },
    { select => [qw(me.auto_metaseq metaseq_acc)],
      as     => [qw( auto_metaseq metaseq_acc)],
      join   => [qw(auto_metaseq)]
    }
    );

  #Get mapping of auto_metaseq to metaseq_acc
  my %seqacc2auto;
  foreach my $r (@oldRegions) {
      $seqacc2auto{ $r->get_column('metaseq_acc') } = $r->get_column('auto_metaseq');
  }
 

  my $metaseq = $self->getSchema->resultset('Metaseq')
      ->find( { metaseq_acc => $famObj->DESC->ID } );


  $self->getSchema->resultset('MetaPfamaReg')
    ->search( { auto_pfama => $auto } )->delete;


  my $dbh = $self->getSchema->storage->dbh;

  my $seq_sth = $dbh->prepare('select auto_metaseq from metaseq where metaseq_acc = ? ');

  my $upSth = $dbh->prepare(
    'INSERT INTO meta_pfamA_reg
    (auto_pfamA,        
     auto_metaseq,                   
     seq_start,            
     seq_end,
     ali_start,
     ali_end,              
     model_start,          
     model_end,            
     domain_bits_score,    
     domain_evalue_score,  
     sequence_bits_score,  
     sequence_evalue_score)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)');


  foreach my $seq ( @{ $famObj->PFAMOUT->eachHMMSeq } ) {

    #Get the auto number for this sequence
    my $sauto;
    if ( $seqacc2auto{ $seq->name } ) {
      $sauto = $seqacc2auto{ $seq->name };
    }
    else {
      $seq_sth->execute( $seq->name );
      my $row = $seq_sth->fetchrow_arrayref;
      unless ($row) {
        confess( "Failed to find entry in metaseq for "
            . $seq->name . "\n" );
      }
      $sauto = $row->[0];
    }




      if ( $seq->bits >= $famObj->DESC->CUTGA->{seq} ) {


	  foreach my $u ( @{ $seq->hmmUnits } ) {
	      
	      #Is it significant dom?
	      if ( $u->bits >= $famObj->DESC->CUTGA->{dom} ) {
		  $upSth->execute(
				  $auto,
				  $sauto,
				  $u->envFrom,
				  $u->envTo,
				  $u->seqFrom,
				  $u->seqTo,
				  $u->hmmFrom,
				  $u->hmmTo,
				  $u->bits,
				  $u->evalue,
				  $seq->bits,
				  $seq->evalue,				  				  				  
				  );
		  
	      }	      	      	      
	  }
      }      
  }
}

sub updatePfamALitRefs {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
#Add the references to the literature reference table if it is not there.
#Then added the information pfamA_literature_reference table.
  $self->getSchema->resultset('PfamaLiteratureReferences')
    ->search( { auto_pfamA => $auto } )->delete;

  foreach my $ref ( @{ $famObj->DESC->REFS } ) {
    my $dbRef =
      $self->getSchema->resultset('LiteratureReferences')->find_or_create(
      {
        pmid    => $ref->{RM},
        title   => $ref->{RT} ? $ref->{RT} : '',
        author  => $ref->{RA} ? $ref->{RA} : '',
        journal => $ref->{RL} ? $ref->{RL} : ''
      }
      );
    unless ( $dbRef->auto_lit ) {
      confess( "Failed to find references for pmid " . $ref->{RM} . "\n" );
    }
    $self->getSchema->resultset('PfamaLiteratureReferences')->create(
      {
        auto_pfama  => $auto,
        auto_lit    => $dbRef,
        comment     => $ref->{RC} ? $ref->{RC} : '',
        order_added => $ref->{RN}
      }
    );
  }
}

#-------------------------------------------------------------------------------

=head2 subname 

  Title    :
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub updatePfamADbXrefs {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
  $self->getSchema->resultset('PfamaDatabaseLinks')
    ->search( { auto_pfamA => $auto } )->delete;

  foreach my $dbLink ( @{ $famObj->DESC->DBREFS } ) {
    $self->getSchema->resultset('PfamaDatabaseLinks')->create(
      {
        auto_pfama   => $auto,
        db_id        => $dbLink->{db_id},
        comment      => $dbLink->{db_comment} ? $dbLink->{db_comment} : '',
        db_link      => $dbLink->{db_link},
        other_params => $dbLink->{other_params} ? $dbLink->{other_params} : ''
      }
    );
  }
}

#-------------------------------------------------------------------------------

=head2 subname 

  Title    :
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub updatePfamANested {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('NestedDomains')
    ->search( { auto_pfamA => $auto } )->delete;

  $self->getSchema->resultset('NestedLocations')
    ->search( { auto_pfamA => $auto } )->delete;

  foreach my $n ( @{ $famObj->DESC->NESTS } ) {
    my $otherPfamA =
      $self->getSchema->resultset('Pfama')->find( { pfamA_acc => $n->{dom} } );

    my $otherAuto;
    if ( $otherPfamA->pfama_id ) {
      $otherAuto = $otherPfamA->auto_pfama;
    }
    else {
      confess( "Did not find an mysql entry for " . $n->{dom} . "\n" );
    }

    #Now look up the sequence
    my ( $seqAcc, $version ) = $n->{seq} =~ /(\S+)\.(\d+)/;
    my $seq = $self->getSchema->resultset('Pfamseq')->find(
      {
        pfamseq_acc => $seqAcc,
        seq_version => $version
      }
    );

    unless ( $seq and $seq->auto_pfamseq ) {
      confess(
        'Could not find sequence ' . $n->{seq} . ' in the pfamseq table' );
    }

    $self->getSchema->resultset('NestedDomains')->create(
      {
        auto_pfama       => $auto,
        nests_auto_pfama => $otherAuto
      }
    );

    $self->getSchema->resultset('NestedLocations')->create(
      {
        auto_pfama        => $auto,
        nested_auto_pfama => $otherAuto,
        nested_pfama_acc  => $otherPfamA->pfama_acc,
        pfamseq_acc       => $seq->pfamseq_acc,
        seq_version       => $seq->seq_version,
        seq_start         => $n->{from},
        seq_end           => $n->{to},
        auto_pfamseq      => $seq->auto_pfamseq
      }
    );

  }

}

sub updateEdits {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('Edits')->search( { auto_pfama => $auto } )
    ->delete;

  foreach my $n ( @{ $famObj->DESC->EDITS } ) {

    #Now look up the sequence
    my ( $seqAcc, $version ) = $n->{seq} =~ /(\S+)\.(\d+)/;
    my $seq = $self->getSchema->resultset('Pfamseq')->find(
      {
        pfamseq_acc => $seqAcc,
        seq_version => $version
      }
    );

    unless ( $seq and $seq->auto_pfamseq ) {
      confess(
        'Could not find sequence ' . $n->{seq} . ' in the pfamseq table' );
    }

    $self->getSchema->resultset('Edits')->create(
      {
        auto_pfama     => $auto,
        auto_pfamseq   => $seq->auto_pfamseq,
        pfamseq_acc    => $seq->pfamseq_acc,
        seq_version    => $seq->seq_version,
        original_start => $n->{oldFrom},
        original_end   => $n->{oldTo},
        new_start      => $n->{newFrom} ? $n->{newFrom} : '',
        new_end        => $n->{newTo} ? $n->{newTo} : ''
      }
    );
  }
}

sub uploadPfamAHMM {
  my ( $self, $famObj, $hmmString ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('PfamaHmm')->update_or_create(
    {
      auto_pfama => $auto,
      hmm        => $hmmString
    }
  );

}

sub uploadPfamAFull {
  my ( $self, $famObj, $fileString ) = @_;
  $self->uploadAlignmentAndTrees( $famObj, $fileString, "full" );
}

sub uploadPfamASeed {
  my ( $self, $famObj, $fileString ) = @_;
  $self->uploadAlignmentAndTrees( $famObj, $fileString, "seed" );
}

sub uploadAlignmentAndTrees {
  my ( $self, $famObj, $string, $type ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('AlignmentsAndTrees')->update_or_create(
    {
      auto_pfama => $auto,
      alignment  => Compress::Zlib::memGzip($string),
      type       => $type
    }
  );

}



sub updateClanDbXrefs {
  my ( $self, $clanObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }

#-------------------------------------------------------------------------------
#Get the index for the clan

  my $auto;
  if ( $clanObj->rdb->{auto} ) {
    $auto = $clanObj->rdb->{auto};
  }
  else {
    my $clan =
      $self->getSchema->resultset('Clans')
      ->find( { clan_acc => $clanObj->DESC->AC } );

    if ( $clan->clan_id ) {
      $auto = $clan->auto_pfama;
      $clanObj->rdb( { auto => $clan->auto_clan } );
    }
    else {
      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
  $self->getSchema->resultset('ClanDatabaseLinks')
    ->search( { auto_clan => $auto } )->delete;

  foreach my $dbLink ( @{ $clanObj->DESC->DBREFS } ) {
    $self->getSchema->resultset('ClanDatabaseLinks')->create(
      {
        auto_clan    => $auto,
        db_id        => $dbLink->{db_id},
        comment      => $dbLink->{db_comment} ? $dbLink->{db_comment} : '',
        db_link      => $dbLink->{db_link},
        other_params => $dbLink->{other_params} ? $dbLink->{other_params} : ''
      }
    );
  }
}

sub updateClanLitRefs {
  my ( $self, $clanObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }

#-------------------------------------------------------------------------------
#Get the index for the clan

  my $auto;
  if ( $clanObj->rdb->{auto} ) {
    $auto = $clanObj->rdb->{auto};
  }
  else {
    my $clan =
      $self->getSchema->resultset('Clans')
      ->find( { clan_acc => $clanObj->DESC->AC } );

    if ( $clan->clan_id ) {
      $auto = $clan->auto_pfama;
      $clanObj->rdb( { auto => $clan->auto_clan } );
    }
    else {
      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
#Add the references to the literature reference table if it is not there.
#Then added the information pfamA_literature_reference table.
  $self->getSchema->resultset('ClanLitRefs')->search( { auto_clan => $auto } )
    ->delete;

  foreach my $ref ( @{ $clanObj->DESC->REFS } ) {
    my $dbRef =
      $self->getSchema->resultset('LiteratureReferences')->find_or_create(
      {
        pmid    => $ref->{RM},
        title   => $ref->{RT} ? $ref->{RT} : '',
        author  => $ref->{RA} ? $ref->{RA} : '',
        journal => $ref->{RL} ? $ref->{RL} : ''
      }
      );
    unless ( $dbRef->auto_lit ) {
      confess( "Failed to find references for pmid " . $ref->{RM} . "\n" );
    }
    $self->getSchema->resultset('ClanLitRefs')->create(
      {
        auto_clan   => $auto,
        auto_lit    => $dbRef,
        comment     => $ref->{RC} ? $ref->{RC} : '',
        order_added => $ref->{RN}
      }
    );
  }
}


sub uploadPfamAInternal {
  my ( $self, $famObj, $seedString, $fullString ) = @_;
  
  #-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $pfamA =
      $self->getSchema->resultset('Pfama')
      ->find( { pfamA_id => $famObj->DESC->ID } );

    if ( $pfamA->pfama_id ) {
      $auto = $pfamA->auto_pfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('PfamaInternal')->update_or_create(
    {
      auto_pfama => $auto,
      seed       => defined( $seedString ) ? Compress::Zlib::memGzip($seedString) : '',
      full       => defined( $fullString ) ? Compress::Zlib::memGzip($fullString) : '',
    }
  );
}

sub resetInFull {
  my ( $self, $auto ) = @_;  
  
  my @regions =
    $self->getSchema->resultset('PfamaRegFullSignificant')->search(
    { auto_pfama => $auto } );
  
  foreach my $r (@regions){
    $r->update( {in_full => 1 });
  }
    
}

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

