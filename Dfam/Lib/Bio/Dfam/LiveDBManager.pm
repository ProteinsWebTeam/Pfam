#
# BioPerl module for Bio::Dfam::LiveDBManager
#
# $Author: rdf $

package Bio::Dfam::LiveDBManager;

use Data::Dumper;
use Carp qw(cluck croak carp confess);

use base Bio::Dfam::DBManager;
use DfamLive::Schema;
use strict;
use warnings;
use Compress::Zlib;

sub new {
  my $caller    = shift;
  my $class     = ref($caller) || $caller;
  my %dbiParams = ();
  my $self      = {
    user     => "root",
    host     => "127.0.0.1",
    port     => "3306",
    database => "dfam_live",
    driver   => "mysql",
    password => "remmhAdmin",
    @_,
  };

  carp( "The new object contains :" . Dumper($self) ) if ( $self->{'debug'} );
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
    croak("Failed to get schema for database:"
        . $self->{'database'}
        . ". Error:[$@]\n" );
  }
  return bless( $self, $caller );
}

#
# Select methods specific to Dfamlive should go in here.
#

sub getClanLockData {
  my ( $self, $clan ) = @_;

  my $lockData;
  if ( $clan =~ /CL\d{4}/ ) {
    $lockData = $self->getSchema->resultset("ClanLocks")->find(
      { "auto_clan.clan_acc" => $clan },
      {
        join     => [qw/auto_clan/],
        prefetch => [qw/auto_clan/]
      }
    );
  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    $self->getSchema->resultset->find(
      { "auto_clan.clan_id" => $clan },
      {
        join     => [qw/auto_clan/],
        prefetch => [qw/auto_clan/]
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
  my ( $self, $autoClan, $autoDfamA ) = @_;
  my ($result);
  carp(
    "Updating clan membership with auto_clan: $autoClan, auto_dfamA: $autoDfamA"
  ) if ( $self->{'debug'} );
  if ( $autoClan && $autoDfamA ) {
    $result = $self->getSchema->resultset('ClanMembership')->find_or_create(
      {
        auto_clan  => $autoClan,
        auto_dfama => $autoDfamA
      },
      { key => 'clanMembConst' }
    );
  }
  else {
    cluck(
      "Can not update clan_membership without both auto_dfamA and auto_clan!");
  }
  return ($result);
}

sub removeFamilyFromClanMembership {
  my ( $self, $autoClan, $autoDfamA ) = @_;
  my ($result);
  carp(
    "Removing family from clan membership: $autoClan, auto_dfamA: $autoDfamA")
    if ( $self->{'debug'} );
  if ( $autoClan && $autoDfamA ) {
    $result = $self->getSchema->resultset('ClanMembership')->find(
      {
        auto_clan  => $autoClan,
        auto_dfamA => $autoDfamA
      }
    )->delete;

    print STDERR "\n\n****** $result ******\n\n"
      if ( $result->isa("DBIx::Class::Row") );
  }
  else {
    cluck(
"Can not remove family from clan_membership without both auto_dfamA and auto_clan!"
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
"Can not remove family from clan_membership without both auto_dfamA and auto_clan!"
    );
  }
  return ($result);
}

#TODO
sub updateClan {
  my ( $self, $clanObj ) = @_;

  unless ( $clanObj and $clanObj->isa('Bio::Dfam::Clan::Clan') ) {
    confess("Did not get a Bio::Dfam::Clan::Clan object");
  }

  my $clan =
    $self->getSchema->resultset('Clans')
    ->find( { clan_acc => $clanObj->DESC->AC } );
  $clan->update(
    {
      clan_acc    => $clanObj->DESC->AC,
      clan_id     => $clanObj->DESC->ID,
      previous_id => defined( $clanObj->DESC->PI ) ? $clanObj->DESC->PI : '',
      clan_description => $clanObj->DESC->DE,
      clan_author      => $clanObj->DESC->AU,
      clan_comment => defined( $clanObj->DESC->CC ) ? $clanObj->DESC->CC : '',
    }
  );

  #Add the auto number to the clan Obj.
  $clanObj->rdb( { auto => $clan->auto_clan } );
}

sub createClan {
  my ( $self, $clanObj, $depositor ) = @_;

  unless ( $clanObj and $clanObj->isa('Bio::Dfam::Clan::Clan') ) {
    confess("Did not get a Bio::Dfam::Clan::Clan object");
  }

  my $clan = $self->getSchema->resultset('Clans')->create(
    {
      clan_acc    => $clanObj->DESC->AC,
      clan_id     => $clanObj->DESC->ID,
      previous_id => defined( $clanObj->DESC->PI ) ? $clanObj->DESC->PI : '',
      clan_description => $clanObj->DESC->DE,
      clan_author      => $clanObj->DESC->AU,
      deposited_by     => $depositor,
      clan_comment => defined( $clanObj->DESC->CC ) ? $clanObj->DESC->CC : '',
      created      => \'NOW()',
      competed     => 0
    }
  );

  unless ( $clan and $clan->isa('DfamDB::Clans') ) {
    confess( 'Failed to get row for ' . $clanObj->DESC->ID . "....." );
  }

  #Add the auto number to the clan Obj.
  $clanObj->rdb( { auto => $clan->auto_clan } );

}

sub updateDfam {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::Dfam') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  my $dfamA =
    $self->getSchema->resultset('Dfama')
    ->find( { dfamA_acc => $famObj->DESC->AC } );

  unless ( $dfamA and $dfamA->isa('DfamDB::Dfama') ) {
    confess( 'Failed to get row for ' . $famObj->DESC->AC . "$dfamA....." );
  }

  #In the results set object, update all of the fields from the DESC object
  $dfamA->dfam_acc( $famObj->DESC->AC );
  $dfamA->dfam_id( $famObj->DESC->ID );
  $dfamA->description( $famObj->DESC->DE );
  $dfamA->author( $famObj->DESC->AU );
  $dfamA->seed_source( $famObj->DESC->SE );
  $dfamA->type( $famObj->DESC->TP );
  $dfamA->sequence_tc( $famObj->DESC->CUTTC->{seq} );
  $dfamA->domain_tc( $famObj->DESC->CUTTC->{dom} );
  $dfamA->sequence_ga( $famObj->DESC->CUTGA->{seq} );
  $dfamA->domain_ga( $famObj->DESC->CUTGA->{dom} );
  $dfamA->sequence_nc( $famObj->DESC->CUTNC->{seq} );
  $dfamA->domain_nc( $famObj->DESC->CUTNC->{dom} );
  $dfamA->buildmethod( $famObj->DESC->BM );
  $dfamA->searchmethod( $famObj->DESC->SM );
  $dfamA->comment( $famObj->DESC->CC );
  $dfamA->previous_id( $famObj->DESC->PI ? $famObj->DESC->PI : '' );

  #Now update the HMM stuff;
  $dfamA->msv_mu( $famObj->HMM->msvStats->{mu} );
  $dfamA->msv_lambda( $famObj->HMM->msvStats->{lambda} );
  $dfamA->viterbi_mu( $famObj->HMM->viterbiStats->{mu} );
  $dfamA->viterbi_lambda( $famObj->HMM->viterbiStats->{lambda} );
  $dfamA->forward_tau( $famObj->HMM->forwardStats->{tau} );
  $dfamA->forward_lambda( $famObj->HMM->forwardStats->{lambda} );
  $dfamA->model_length( $famObj->HMM->length );

  #Now update the numbers in the SEED and FULL
  $dfamA->num_seed( $famObj->SEED->no_sequences );
  $dfamA->num_full( $famObj->ALIGN->no_sequences );

  $famObj->rdb( { auto => $dfamA->auto_dfama } );
  $dfamA->update;

  if ( $famObj->DESC->CL ) {
    $self->resetClanCompeteFlag( $famObj->DESC->CL );
  }

}

sub createDfam {
  my ( $self, $famObj, $depositor ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family') ) {
    confess("Did not get a Bio::Dfam::Family object");
  }

  my $dfamA = $self->getSchema->resultset('Dfam')->create(
    {
      dfam_acc      => $famObj->DESC->AC,
      dfam_id       => $famObj->DESC->ID,
      description    => $famObj->DESC->DE,
      author         => $famObj->DESC->AU,
      deposited_by   => $depositor,
      seed_source    => $famObj->DESC->SE,
      type           => $famObj->DESC->TP,
      sequence_tc    => $famObj->DESC->CUTTC->{seq},
      domain_tc      => $famObj->DESC->CUTTC->{dom},
      sequence_ga    => $famObj->DESC->CUTGA->{seq},
      domain_ga      => $famObj->DESC->CUTGA->{dom},
      sequence_nc    => $famObj->DESC->CUTNC->{seq},
      domain_nc      => $famObj->DESC->CUTNC->{dom},
      buildmethod    => $famObj->DESC->BM,
      searchmethod   => $famObj->DESC->SM,
      comment        => $famObj->DESC->CC,
      previous_id    => $famObj->DESC->PI ? $famObj->DESC->PI : '',
      msv_mu         => $famObj->HMM->msvStats->{mu},
      msv_lambda     => $famObj->HMM->msvStats->{lambda},
      viterbi_mu     => $famObj->HMM->viterbiStats->{mu},
      viterbi_lambda => $famObj->HMM->viterbiStats->{lambda},
      forward_tau    => $famObj->HMM->forwardStats->{tau},
      forward_lambda => $famObj->HMM->forwardStats->{lambda},
      model_length   => $famObj->HMM->length,
      num_seed       => $famObj->SEED->no_sequences,
      num_full       => $famObj->ALIGN->no_sequences,
      created        => \'NOW()',
    }
  );

  unless ( $dfamA and $dfamA->isa('DfamDB::Result::Dfam') ) {
    confess( 'Failed to get row for ' . $famObj->DESC->ID . "$dfamA....." );
  }

  #Add the auto number to the famObj.
  #$famObj->rdb( { auto => $dfamA->auto_dfam } );

  #If the family is part of a
  if ( $famObj->DESC->CL ) {
    $self->resetClanCompeteFlag( $famObj->DESC->CL );
  }
}

sub moveDfamA {
  my ( $self, $fromFamily, $toFamily ) = @_;

  my $fromFamObj = $self->getDfamData($fromFamily);

  $fromFamObj->previous_id(
    defined( $fromFamObj->previous_id )
    ? $fromFamObj->previous_id . " $fromFamily;"
    : "$fromFamily;"
  );
  $fromFamObj->dfama_id($toFamily);
  $fromFamObj->update;

}

sub deleteDfamA {
  my ( $self, $family, $comment, $forward, $user ) = @_;
  my $dfamA =
    $self->getSchema->resultset('Dfama')->find( { dfama_acc => $family } );

  unless ( $dfamA and $dfamA->isa('DfamDB::Dfama') ) {
    confess( 'Failed to get row for ' . $family . "$dfamA....." );
  }
  $self->getSchema->resultset('Dfama')->find( { dfama_acc => $family } )
    ->delete;

  #Now make the dead_families entry
  $self->getSchema->resultset('DeadFamilies')->create(
    {
      dfama_id   => $dfamA->dfama_id,
      dfama_acc  => $dfamA->dfama_acc,
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

  unless ( $clan and $clan->isa('DfamDB::Clans') ) {
    confess( 'Failed to get row for ' . $clanAcc . "( Got $clan )....." );
  }
  my $clanMembership = $self->getClanMembership($clanAcc);

  my $memberString;
  foreach my $mem (@$clanMembership) {
    $memberString .= $mem->auto_dfama->dfama_acc . " ";
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

sub updateDfamRegSeed {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

  my @oldRegions = $self->getSchema->resultset('DfamRegSeed')->search(
    { dfam_acc => $famObj->DESC->AC },
    {
      select => [qw(me.auto_dfamseq dfamseq_acc)],
      as     => [qw( auto_dfamseq dfamseq_acc )],
      join   => [qw(auto_dfamseq)]
    }
  );

  my %seqacc2auto;
  foreach my $r (@oldRegions) {
    $seqacc2auto{ $r->get_column('dfamseq_acc') . "."
        . $r->get_column('seq_version') } = $r->get_column('auto_dfamseq');
  }

  $self->getSchema->resultset('DfamRegSeed')->search( { dfam_acc => $famObj->DESC->AC } )
    ->delete;

  my $dbh = $self->getSchema->storage->dbh;

  my $seq_sth = $dbh->prepare(
    'select auto_dfamseq from dfamseq where dfamseq_acc = ?'
  );
  my $up_sth = $dbh->prepare(
    'insert into dfam_reg_seed 
      (auto_dfamseq, dfam_acc, seq_start, seq_end) values ( ?, ?, ?, ?)'
  );

  foreach my $seq ( $famObj->SEED->each_seq ) {
     $up_sth->execute( $seq->id, $famObj->DESC->AC, $seq->start, $seq->end );
  }
}

sub updateDfamRegFull {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family') ) {
    confess("Did not get a Bio::Dfam::Family object");
  }



#-------------------------------------------------------------------------------
#Get all previous regions for this family from dfamA_reg_fill_significant and
#dfamA_reg_full insignificant and hash the seq acc + version to index

#  #Signfiicant table first
#  my @oldRegions =
#    $self->getSchema->resultset('DfamRegFullSignificant')->search(
#    {dfam_acc => $famObj->DESC->AC },
#    {
#      select => [qw(me.auto_dfamseq dfamseq_acc)],
#      as     => [qw( auto_dfamseq dfamseq_acc)],
#      join   => [qw(auto_dfamseq)]
#    }
#    );
#
#  my %seqacc2auto;
#  foreach my $r (@oldRegions) {
#    $seqacc2auto{ $r->get_column('dfamseq_acc') . "."
#        . $r->get_column('seq_version') } = $r->get_column('auto_dfamseq');
#  }
#
#  #Now the insignificant table
#  my @oldInRegions =
#    $self->getSchema->resultset('DfamRegFullInsignificant')->search(
#    { dfam_acc => $famObj->DESC->AC },
#    {
#      select => [qw(me.auto_dfamseq dfamseq_acc)],
#      as     => [qw(auto_dfamseq dfamseq_acc)],
#      join   => [qw(dfamseq)]
#    }
#    );
#
#  foreach my $r (@oldInRegions) {
#    $seqacc2auto{ $r->get_column('dfamseq_acc') . "."
#        . $r->get_column('seq_version') } = $r->get_column('auto_dfamseq');
#  }


  $self->getSchema->resultset('DfamRegFullSignificant')
    ->search( { dfam_acc => $famObj->DESC->AC } )->delete;

  $self->getSchema->resultset('DfamRegFullInsignificant')
    ->search( { dfam_acc => $famObj->DESC->AC } )->delete;

#-------------------------------------------------------------------------------
#As we are going to have to perform this upto 100K times
#it is much faster to use place holders
  my $dbh = $self->getSchema->storage->dbh;

  my $seq_sth = $dbh->prepare(
    'select auto_dfamseq from dfamseq where dfamseq_acc = ?'
  );

  my $upSigSth = $dbh->prepare(
    'INSERT INTO dfam_reg_full_significant
    (dfam_acc, 
    auto_dfamseq, 
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
    'INSERT INTO dfam_reg_full_insignificant
    (dfam_acc, 
    auto_dfamseq, 
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
  #my $regions = $famObj->TABLE->eachHMMUnit;
  #foreach my $seq ( keys %{$regions} ) {
  #  foreach my $se ( $famObj->TABLE->eachHMMUnit ) {
  #    $inFullHash->{ $seq . "/" . $se->seqFrom . "-" . $se->seqTo }++;
  #  }
  #}

  foreach my $seq ( @{ $famObj->TABLE->eachHMMSeq } ) {

#    #Get the auto number for this sequence
#    my $sauto;
#    if ( $seqacc2auto{ $seq->name } ) {
#      $sauto = $seqacc2auto{ $seq->name };
#    }
#    else {
#      my ( $acc, $version ) = $seq->name =~ /(\S+)\.(\d+)/;
#      $seq_sth->execute( $acc, $version );
#      my $row = $seq_sth->fetchrow_arrayref;
#      unless ($row) {
#        confess( "Failed to find entry in dfamseq for "
#            . $seq->id . "."
#            . $seq->seq_version
#            . "\n" );
#      }
#      $sauto = $row->[0];
#    }
    
    if ( $seq->bits >= $famObj->DESC->CUTGA->{seq} ) {
      foreach my $u ( @{ $seq->hmmUnits } ) {
        #print STDERR $seq->bits.", ".$famObj->DESC->CUTGA->{seq}."\n";
        #Is it significant dom?
        if ( $u->bits >= $famObj->DESC->CUTGA->{dom} ) {
          $upSigSth->execute(
            $famObj->DESC->AC,
            $u->name,
            $u->envFrom,
            $u->envTo,
            $u->seqFrom,
            $u->seqTo,
            $u->hmmFrom,
            $u->hmmTo,
            $u->bits,
            $u->evalue,
            $seq->bits,
            $seq->sequenceEvalue,
            $inFullHash->{ $u->name . "/" . $u->envFrom . "-" . $u->envTo }
            ? 1
            : 0
          );

        }
        else {

          #Although the sequence is significant this regions is insignificant
          $upInsigSth->execute(
            $famObj->DESC->AC,       $u->name,    $u->envFrom, $u->envTo,
            $u->hmmFrom, $u->hmmTo, $u->bits,    $u->evalue,
            $seq->bits,  $seq->sequenceEvalue
          );

        }
      }
    }
    else {

      #Sequence is insignifcant....Therefore all the domains have to be.
      foreach my $u ( @{ $seq->hmmUnits } ) {
        $upInsigSth->execute(
          $famObj->DESC->AC,     $u->name,   $u->envFrom, $u->envTo,  $u->hmmFrom,
          $u->hmmTo, $u->bits, $u->evalue,  $seq->bits, $seq->sequenceEvalue
       );
      }
    }
  }
}

sub updateDfamWikipedia {
  
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family') ) {
    confess("Did not get a Bio::Dfam::Family object");
  }

#-------------------------------------------------------------------------------
#Add the page to the wikipedia table if it is not there.
#Then added the information dfamA_literature_reference table.
  $self->getSchema->resultset('DfamWiki')
    ->search( { dfam_acc => $famObj->DESC->AC } )->delete;
  if($famObj->DESC->WIKI and ref($famObj->DESC->WIKI) eq 'HASH'){
    foreach my $page ( keys %{ $famObj->DESC->WIKI } ) {
      my $wiki =
        $self->getSchema->resultset('Wikipedia')->find_or_create(
        {
          title   => $page,
        }
        );
      
      unless ( $wiki->auto_wiki ) {
        confess( "Failed to find or create row for wiki page".$page."\n" );
      }
      
      $self->getSchema->resultset('DfamWiki')->create(
        {
          dfam_acc => $famObj->DESC->AC,
          auto_wiki    => $wiki->auto_wiki
        }
      );
    }
  }
}


sub updateDfamLitRefs {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family') ) {
    confess("Did not get a Bio::Dfam::Family object");
  }

#-------------------------------------------------------------------------------
#Add the references to the literature reference table if it is not there.
#Then added the information dfamA_literature_reference table.
  $self->getSchema->resultset('DfamLiteratureReference')
    ->search( { dfam_acc => $famObj->DESC->AC } )->delete;

  foreach my $ref ( @{ $famObj->DESC->REFS } ) {
    my $dbRef =
      $self->getSchema->resultset('LiteratureReference')->find_or_create(
      {
        pmid    => $ref->{RM},
        title   => $ref->{RT} ? $ref->{RT} : '',
        author  => $ref->{RA} ? $ref->{RA} : '',
        journal => $ref->{RL} ? $ref->{RL} : ''
      }
    );
    
    unless ( $dbRef->pmid ) {
      confess( "Failed to find references for pmid " . $ref->{RM} . "\n" );
    }
    $self->getSchema->resultset('DfamLiteratureReference')->create(
      {
        dfam_acc    => $famObj->DESC->AC,
        pmid        => $dbRef,
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

sub updateDfamADbXrefs {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the dfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $dfamA =
      $self->getSchema->resultset('Dfama')
      ->find( { dfamA_id => $famObj->DESC->ID } );

    if ( $dfamA->dfama_id ) {
      $auto = $dfamA->auto_dfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
  $self->getSchema->resultset('DfamaDatabaseLinks')
    ->search( { auto_dfamA => $auto } )->delete;

  foreach my $dbLink ( @{ $famObj->DESC->DBREFS } ) {
    $self->getSchema->resultset('DfamaDatabaseLinks')->create(
      {
        auto_dfama   => $auto,
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

sub updateDfamANested {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the dfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $dfamA =
      $self->getSchema->resultset('Dfama')
      ->find( { dfamA_id => $famObj->DESC->ID } );

    if ( $dfamA->dfama_id ) {
      $auto = $dfamA->auto_dfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('NestedDomains')
    ->search( { auto_dfamA => $auto } )->delete;

  $self->getSchema->resultset('NestedLocations')
    ->search( { auto_dfamA => $auto } )->delete;

  foreach my $n ( @{ $famObj->DESC->NESTS } ) {
    my $otherDfamA =
      $self->getSchema->resultset('Dfama')->find( { dfamA_acc => $n->{dom} } );

    my $otherAuto;
    if ( $otherDfamA->dfama_id ) {
      $otherAuto = $otherDfamA->auto_dfama;
    }
    else {
      confess( "Did not find an mysql entry for " . $n->{dom} . "\n" );
    }

    #Now look up the sequence
    my ( $seqAcc, $version ) = $n->{seq} =~ /(\S+)\.(\d+)/;
    my $seq = $self->getSchema->resultset('Dfamseq')->find(
      {
        dfamseq_acc => $seqAcc,
        seq_version => $version
      }
    );

    unless ( $seq and $seq->auto_dfamseq ) {
      confess(
        'Could not find sequence ' . $n->{seq} . ' in the dfamseq table' );
    }

    $self->getSchema->resultset('NestedDomains')->create(
      {
        auto_dfama       => $auto,
        nests_auto_dfama => $otherAuto
      }
    );

    $self->getSchema->resultset('NestedLocations')->create(
      {
        auto_dfama        => $auto,
        nested_auto_dfama => $otherAuto,
        nested_dfama_acc  => $otherDfamA->dfama_acc,
        dfamseq_acc       => $seq->dfamseq_acc,
        seq_version       => $seq->seq_version,
        seq_start         => $n->{from},
        seq_end           => $n->{to},
        auto_dfamseq      => $seq->auto_dfamseq
      }
    );

  }

}

sub updateEdits {
  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the dfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $dfamA =
      $self->getSchema->resultset('Dfama')
      ->find( { dfamA_id => $famObj->DESC->ID } );

    if ( $dfamA->dfama_id ) {
      $auto = $dfamA->auto_dfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('Edits')->search( { auto_dfama => $auto } )
    ->delete;

  foreach my $n ( @{ $famObj->DESC->EDITS } ) {

    #Now look up the sequence
    my ( $seqAcc, $version ) = $n->{seq} =~ /(\S+)\.(\d+)/;
    my $seq = $self->getSchema->resultset('Dfamseq')->find(
      {
        dfamseq_acc => $seqAcc,
        seq_version => $version
      }
    );

    unless ( $seq and $seq->auto_dfamseq ) {
      confess(
        'Could not find sequence ' . $n->{seq} . ' in the dfamseq table' );
    }

    if (  $n->{newFrom}
      and $n->{newTo}
      and $n->{newFrom} >= 1
      and $n->{newTo} > 1 )
    {
      $self->getSchema->resultset('Edits')->create(
        {
          auto_dfama     => $auto,
          auto_dfamseq   => $seq->auto_dfamseq,
          dfamseq_acc    => $seq->dfamseq_acc,
          seq_version    => $seq->seq_version,
          original_start => $n->{oldFrom},
          original_end   => $n->{oldTo},
          new_start      => $n->{newFrom},
          new_end        => $n->{newTo}
        }
      );
    }
    else {
      $self->getSchema->resultset('Edits')->create(
        {
          auto_dfama     => $auto,
          auto_dfamseq   => $seq->auto_dfamseq,
          dfamseq_acc    => $seq->dfamseq_acc,
          seq_version    => $seq->seq_version,
          original_start => $n->{oldFrom},
          original_end   => $n->{oldTo},
        }
      );
    }

  }
}


sub uploadDfamAHMM {
  my ( $self, $famObj, $hmmString ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the dfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $dfamA =
      $self->getSchema->resultset('Dfama')
      ->find( { dfamA_id => $famObj->DESC->ID } );

    if ( $dfamA->dfama_id ) {
      $auto = $dfamA->auto_dfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('DfamaHmm')->update_or_create(
    {
      auto_dfama => $auto,
      hmm        => $hmmString
    }
  );

}

sub uploadDfamAFull {
  my ( $self, $famObj, $fileString ) = @_;
  $self->uploadAlignmentAndTrees( $famObj, $fileString, "full" );
}

sub uploadDfamASeed {
  my ( $self, $famObj, $fileString ) = @_;
  $self->uploadAlignmentAndTrees( $famObj, $fileString, "seed" );
}

sub uploadAlignmentAndTrees {
  my ( $self, $famObj, $string, $type ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the dfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $dfamA =
      $self->getSchema->resultset('Dfama')
      ->find( { dfamA_id => $famObj->DESC->ID } );

    if ( $dfamA->dfama_id ) {
      $auto = $dfamA->auto_dfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('AlignmentsAndTrees')->update_or_create(
    {
      auto_dfama => $auto,
      alignment  => Compress::Zlib::memGzip($string),
      type       => $type
    }
  );

}

sub updateClanDbXrefs {
  my ( $self, $clanObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $clanObj and $clanObj->isa('Bio::Dfam::Clan::Clan') ) {
    confess("Did not get a Bio::Dfam::Clan::Clan object");
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
      $auto = $clan->auto_dfama;
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


sub updateClanWikipedia {
  my ( $self, $clanObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $clanObj and $clanObj->isa('Bio::Dfam::Clan::Clan') ) {
    confess("Did not get a Bio::Dfam::Clan::Clan object");
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
      $auto = $clan->auto_dfama;
      $clanObj->rdb( { auto => $clan->auto_clan } );
    }
    else {
      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
    }
  }
 
#-------------------------------------------------------------------------------
#Add the page to the wikipedia table if it is not there.
#Then added the information dfamA_literature_reference table.
  $self->getSchema->resultset('ClanWiki')
    ->search( { auto_clan => $auto } )->delete;
    
  if($clanObj->DESC->WIKI and ref($clanObj->DESC->WIKI) eq 'HASH'){
    foreach my $page ( keys %{ $clanObj->DESC->WIKI } ) {
      my $wiki =
        $self->getSchema->resultset('Wikipedia')->find_or_create(
        {
          title   => $page,
        }
        );
      
      unless ( $wiki->auto_wiki ) {
        confess( "Failed to find or create row for wiki page".$page."\n" );
      }
      
      $self->getSchema->resultset('ClanWiki')->create(
        {
          auto_clan  => $auto,
          auto_wiki    => $wiki->auto_wiki
        }
      );
    }
  }
}


sub updateClanLitRefs {
  my ( $self, $clanObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $clanObj and $clanObj->isa('Bio::Dfam::Clan::Clan') ) {
    confess("Did not get a Bio::Dfam::Clan::Clan object");
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
      $auto = $clan->auto_dfama;
      $clanObj->rdb( { auto => $clan->auto_clan } );
    }
    else {
      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
    }
  }

#-------------------------------------------------------------------------------
#Add the references to the literature reference table if it is not there.
#Then added the information dfamA_literature_reference table.
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

sub uploadDfamAInternal {
  my ( $self, $famObj, $seedString, $fullString ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Dfam::Family::DfamA') ) {
    confess("Did not get a Bio::Dfam::Family::DfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the dfamA family

  my $auto;
  if ( $famObj->rdb->{auto} ) {
    $auto = $famObj->rdb->{auto};
  }
  else {
    my $dfamA =
      $self->getSchema->resultset('Dfama')
      ->find( { dfamA_id => $famObj->DESC->ID } );

    if ( $dfamA->dfama_id ) {
      $auto = $dfamA->auto_dfama;
      $famObj->rdb->{auto} = $auto;
    }
    else {
      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
    }
  }

  $self->getSchema->resultset('DfamaInternal')->update_or_create(
    {
      auto_dfama => $auto,
      seed => defined($seedString) ? Compress::Zlib::memGzip($seedString) : '',
      full => defined($fullString) ? Compress::Zlib::memGzip($fullString) : '',
    }
  );
}

sub resetInFull {
  my ( $self, $auto ) = @_;

  my @regions =
    $self->getSchema->resultset('DfamaRegFullSignificant')
    ->search( { auto_dfama => $auto } );

  foreach my $r (@regions) {
    $r->update( { in_full => 1 } );
  }

}

sub resetClanCompeteFlag {
  my ( $self, $clan ) = @_;

  my $clanData;
  if ( $clan =~ /^(\d+)$/ ) {

    #Looks like an auto incremeneted key
    my $clanData =
      $self->getSchema->resultset("Clans")->find( { "auto_clan" => $clan } );
  }
  else {
    $clanData = $self->getClanData($clan);
  }

  unless ( $clanData->isa('DfamDB::Clans') ) {
    croak("Failed to get a clan row obkect for $clan");
  }

  $clanData->update( { competed => 0 } );

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

