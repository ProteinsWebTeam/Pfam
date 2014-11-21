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
use POSIX;
use DateTime;
use DateTime::Format::MySQL;

sub new {
  my $caller    = shift;
  my $class     = ref($caller) || $caller;
  my %dbiParams = ();
  my $self      = {
    user     => "pfamro",
    host     => "mcs11",
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
    $lockData = $self->getSchema->resultset("ClanLocks")->find(
      { "clan_acc.clan_acc" => $clan },
      {
        join     => [qw/clan_acc/],
        prefetch => [qw/clan_acc/]
      }
    );
  }
  elsif ( $clan =~ /\S{1,16}/ ) {
    $self->getSchema->resultset->find(
      { "clan_acc.clan_id" => $clan },
      {
        join     => [qw/clan_acc/],
        prefetch => [qw/clan_acc/]
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
  my ( $self, $clan, $pfamA ) = @_;
  my ($result);
  carp(
    "Updating clan membership with clan: $clan, pfamA: $pfamA"
  ) if ( $self->{'debug'} );
  if ( $clan && $pfamA ) {
    $result = $self->getSchema->resultset('ClanMembership')->find_or_create(
      {
        clan_acc  => $clan,
        pfamA_acc => $pfamA
      },
      { key => 'clanMembConst' }
    );
  }
  else {
    cluck(
      "Can not update clan_membership without both pfamA and clan!");
  }
  return ($result);
}

sub removeFamilyFromClanMembership {
  my ( $self, $clan, $pfamA ) = @_;
  my ($result);
  carp(
    "Removing family from clan membership: $clan, pfamA: $pfamA")
    if ( $self->{'debug'} );

  if ( $clan && $pfamA ) {
    $result = $self->getSchema->resultset('ClanMembership')->find(
      {
        clan_acc  => $clan,
        pfamA_acc => $pfamA
      }
    )->delete;

    print STDERR "\n\n****** $result ******\n\n"
      if ( $result->isa("DBIx::Class::Row") );
  }
  else {
    cluck(
"Can not remove family from clan_membership without both pfamA and clan!"
    );
  }
  return ($result);
}

sub removeClan {
  my ( $self, $clan ) = @_;

  my $result;
  if ($clan) {
    $result =
      $self->getSchema->resultset('Clan')->find( { clan_acc => $clan } )
      ->delete;
  }
  else {
    cluck(
"Can not remove family from clan_membership without both pfamA_acc and clan_acc!"
    );
  }
  return ($result);
}

#TODO
sub updateClan {
  my ( $self, $clanObj ) = @_;

  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }

  my $clan =
    $self->getSchema->resultset('Clan')
    ->find( { clan_acc => $clanObj->DESC->AC } );
  $clan->update(
    {
      clan_acc    => $clanObj->DESC->AC,
      clan_id     => $clanObj->DESC->ID,
      previous_id => defined( $clanObj->DESC->PI ) ? $clanObj->DESC->PI : '',
      clan_description => $clanObj->DESC->DE,
      clan_author      => $clanObj->DESC->AU,
      clan_comment => defined( $clanObj->DESC->CC ) ? $clanObj->DESC->CC : '',
      updated      => DateTime::Format::MySQL->format_datetime( DateTime->now )
    }
  );

}

sub createClan {
  my ( $self, $clanObj, $depositor ) = @_;

  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }

  my $clan = $self->getSchema->resultset('Clan')->create(
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

  unless ( $clan and $clan->isa('PfamLive::Result::Clans') ) {
    confess( 'Failed to get row for ' . $clanObj->DESC->ID . "....." );
  }

}

sub updatePfamA {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  my $pfamA =
    $self->getSchema->resultset('PfamA')
    ->find( { pfamA_acc => $famObj->DESC->AC } );

  unless ( $pfamA and $pfamA->isa('PfamLive::Result::PfamA') ) {
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
  $pfamA->msv_mu( $famObj->HMM->msvStats->{mu} );
  $pfamA->msv_lambda( $famObj->HMM->msvStats->{lambda} );
  $pfamA->viterbi_mu( $famObj->HMM->viterbiStats->{mu} );
  $pfamA->viterbi_lambda( $famObj->HMM->viterbiStats->{lambda} );
  $pfamA->forward_tau( $famObj->HMM->forwardStats->{tau} );
  $pfamA->forward_lambda( $famObj->HMM->forwardStats->{lambda} );
  $pfamA->model_length( $famObj->HMM->length );

  #Now update the numbers in the SEED and FULL
  $pfamA->num_seed( $famObj->SEED->num_sequences );
  $pfamA->num_full( $famObj->scores->numRegions );
  $pfamA->updated( DateTime::Format::MySQL->format_datetime( DateTime->now ) );

  $pfamA->update;

  if ( $famObj->DESC->CL ) {
    $self->resetClanCompeteFlag( $famObj->DESC->CL );
  }

}

sub createPfamA {
  my ( $self, $famObj, $depositor ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

  my $pfamA = $self->getSchema->resultset('PfamA')->create(
    {
      pfama_acc      => $famObj->DESC->AC,
      pfama_id       => $famObj->DESC->ID,
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
      num_seed       => $famObj->SEED->num_sequences,
      num_full       => $famObj->scores->numRegions,
      created        => \'NOW()',
    }
  );

  unless ( $pfamA and $pfamA->isa('PfamLive::Result::Pfama') ) {
    confess( 'Failed to get row for ' . $famObj->DESC->ID . "$pfamA....." );
  }

  #If the family is part of a
  if ( $famObj->DESC->CL ) {
    $self->resetClanCompeteFlag( $famObj->DESC->CL );
  }
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
    $self->getSchema->resultset('PfamA')->search( { pfama_acc => $family },
    { join => [ { pfama_wikis => 'auto_wiki' } ] } )->single;

  unless ( $pfamA and $pfamA->isa('PfamLive::Result::PfamA') ) {
    confess( 'Failed to get row for ' . $family . "$pfamA....." );
  }

  my $wiki_page;    #Store wiki link as need this for the website
  foreach my $article ( $pfamA->articles ) {
    $wiki_page = $article->title;
    last;
  }

  $self->getSchema->resultset('PfamA')->find( { pfama_acc => $family } )
    ->delete;

  #Now make the dead_families entry
  $self->getSchema->resultset('DeadFamily')->create(
    {
      pfama_id   => $pfamA->pfama_id,
      pfama_acc  => $pfamA->pfama_acc,
      comment    => $comment,
      forward_to => $forward,
      user       => $user,
      killed     => \'NOW()',
      title      => $wiki_page
    }
  );

}

sub deleteClan {
  my ( $self, $clanAcc, $comment, $forward, $user ) = @_;
  my $clan =
    $self->getSchema->resultset('Clan')->find( { clan_acc => $clanAcc } );

  unless ( $clan and $clan->isa('PfamLive::Result::Clan') ) {
    confess( 'Failed to get row for ' . $clanAcc . "( Got $clan )....." );
  }
  my $clanMembership = $self->getClanMembership($clanAcc);

  my $memberString;
  foreach my $mem (@$clanMembership) {
    $memberString .= $mem->pfama_acc->pfama_acc . " ";
  }

  $clan->delete;

  #Now make the dead_clans entry
  $self->getSchema->resultset('DeadClan')->create(
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
  my ( $self, $famObj) = @_;

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }
  #Delete all the seed regions
  $self->getSchema->resultset('PfamARegSeed')
    ->search( { pfamA_acc => $famObj->DESC->AC } )->delete;

  #Determing all surrogate keys for the sequences in the SEED alignment
  #which are stored in a nosql database - Redis.
  my %seqacc2auto;

  #Get all the sequences that we need to work on
  my @seqs;
  $self->getSchema->storage->dbh->do('SET foreign_key_checks=0');
  my @rows;
  foreach my $seq ( $famObj->SEED->each_seq ) {
    push(
      @rows,
      {
        pfamseq_acc => $seq->id,
        pfama_acc   => $famObj->DESC->AC,
        seq_start    => $seq->start,
        seq_end      => $seq->end
      }
    );
  }

  $self->getSchema->resultset('PfamARegSeed')->populate( \@rows );
  $self->getSchema->storage->dbh->do('SET foreign_key_checks=1');
}

sub updatePfamARegFull {
  my ( $self, $famObj, $redis ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }

#-------------------------------------------------------------------------------
#Get the index for the pfamA family

#not needed
#  my $auto;
#  if ( $famObj->rdb->{auto} ) {
#    $auto = $famObj->rdb->{auto};
#  }
#  else {
#    my $pfamA =
#      $self->getSchema->resultset('Pfama')
#      ->find( { pfamA_id => $famObj->DESC->ID } );
#
#    if ( $pfamA->pfama_id ) {
#      $auto = $pfamA->auto_pfama;
#      $famObj->rdb->{auto} = $auto;
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
#    }
#  }

#-------------------------------------------------------------------------------
  my %seqacc2auto;
  my @seqs;
  #foreach my $seq ( @{ $famObj->PFAMOUT->eachHMMSeq } ) {
  #  my $key = $seq->name;
  #  my $value;
  #  eval{
  #    $value = $redis->get($key); 
  #  };
  #  if($@){
  #    confess("Failed to get auto mapping [$key] from redis as redis threw an error:\n\n$@");
  #  }
    
  #  if(!$value){
  #    confess("Failed to get auto mapping for sequence: $key\n");
  #  }
  #  #This is exploding the size!!!!
  #  #$seqacc2auto{ $key } = $value;
  #}
#-------------------------------------------------------------------------------
#Now delete all regions in the two tables

#The pdb region has a FK to pfamA_reg_full_significant so this need be deleted first.
  $self->getSchema->resultset('PdbPfamAReg')->search( { pfama_acc => $famObj->DESC->AC } )
    ->delete;

  $self->getSchema->resultset('PfamARegFullSignificant')
    ->search( { pfama_acc => $famObj->DESC->AC } )->delete;

  $self->getSchema->resultset('PfamARegFullInsignificant')
    ->search( { pfama_acc => $famObj->DESC->AC } )->delete;

#-------------------------------------------------------------------------------

  #All of the quires are set up now prepare the data

#Find out which sequences have made it in to the full alignment from the scores file
  my $inFullHash;
  my $regions = $famObj->scores->regions;
  my $rc      = 0;
  foreach my $seq ( keys %{$regions} ) {
    foreach my $se ( @{ $regions->{$seq} } ) {
      $inFullHash->{ $seq . "/" . $se->{start} . "-" . $se->{end} }++;
      $rc++;
    }
  }

  $self->getSchema->resultset("PfamARegFullSignificant")
    ->search( { pfamA_acc => $famObj->DESC->AC } )->delete;
  $self->getSchema->resultset("PfamARegFullInsignificant")
    ->search( { pfamA_acc => $famObj->DESC->AC } )->delete;
  $self->getSchema->storage->dbh->do('SET foreign_key_checks=0');

#-------------------------------------------------------------------------------
#All of the quires are set up now prepare the data

  my ( @significant, @insignificant );

  foreach my $seq ( @{ $famObj->PFAMOUT->eachHMMSeq } ) {
    next unless ($seq);

    #Get the auto number for this sequenceq->name;
    my $sauto;
    eval{
      $sauto = $redis->get($seq->name); 
    };
    if($@){
      confess("Failed to get auto mapping [".$seq->name."] from redis as redis threw an error:\n\n$@");
    }

    if (!$sauto){

      confess( "Failed to find entry in pfamseq for "
          . $seq->name . "."
          . $seq->seq_version
          . "\n" );
    }

    #
    if ( $seq->bits >= $famObj->DESC->CUTGA->{seq} ) {
      foreach my $u ( @{ $seq->hmmUnits } ) {

        #Is it significant dom?
        if ( $u->bits >= $famObj->DESC->CUTGA->{dom} ) {
          push(
            @significant,
            {
              pfamA_acc   => $famObj->DESC->AC,
              pfamseq_acc => $sauto,
              seq_start    => $u->envFrom,
              seq_end      => $u->envTo,
              ali_start    => $u->seqFrom,
              ali_end               => $u->seqTo,
              model_start           => $u->hmmFrom,
              model_end             => $u->hmmTo,
              domain_bits_score     => $u->bits,
              domain_evalue_score   => $u->evalue,
              sequence_bits_score   => $seq->bits,
              sequence_evalue_score => $seq->evalue,
              in_full =>
                $inFullHash->{ $u->name . "/" . $u->envFrom . "-" . $u->envTo }
              ? 1
              : 0
            }
          );
        }
        else {
          push(
            @insignificant,
            {
              pfamA_acc             => $famObj->DESC->AC,
              pfamseq_acc           => $sauto,
              seq_start             => $u->envFrom,
              seq_end               => $u->envTo,
              model_start           => $u->hmmFrom,
              model_end             => $u->hmmTo,
              domain_bits_score     => $u->bits,
              domain_evalue_score   => $u->evalue,
              sequence_bits_score   => $seq->bits,
              sequence_evalue_score => $seq->evalue
            }
          );
        }
      }
    }
    else {

      #Sequence is insignifcant....Therefore all the domains have to be.
      foreach my $u ( @{ $seq->hmmUnits } ) {
        push(
          @insignificant,
          {
            pfamA_acc             => $famObj->DESC->AC,
            pfamseq_acc           => $sauto,
            seq_start             => $u->envFrom,
            seq_end               => $u->envTo,
            model_start           => $u->hmmFrom,
            model_end             => $u->hmmTo,
            domain_bits_score     => $u->bits,
            domain_evalue_score   => $u->evalue,
            sequence_bits_score   => $seq->bits,
            sequence_evalue_score => $seq->evalue
          }
        );

      }
    }
    if ( scalar(@insignificant) > 1000 ) {
      $self->getSchema->resultset("PfamARegFullInsignificant")
        ->populate( \@insignificant );
      @insignificant = ();
    }
    if ( scalar(@significant) > 1000 ) {
      $self->getSchema->resultset("PfamARegFullSignificant")
        ->populate( \@significant );
      @significant = ();
    }

  }
  $self->getSchema->resultset("PfamARegFullInsignificant")
    ->populate( \@insignificant );
  $self->getSchema->resultset("PfamARegFullSignificant")
    ->populate( \@significant );
  $self->getSchema->storage->dbh->do('SET foreign_key_checks=1');

}

sub updatePfamAWikipedia {

  my ( $self, $famObj ) = @_;

#-------------------------------------------------------------------------------
#Check we have the correct object

  unless ( $famObj and $famObj->isa('Bio::Pfam::Family::PfamA') ) {
    confess("Did not get a Bio::Pfam::Family::PfamA object");
  }


#-------------------------------------------------------------------------------
#Add the page to the wikipedia table if it is not there.
#Then added the information pfamA_literature_reference table.
  $self->getSchema->resultset('PfamAWiki')->search( { pfama_acc => $famObj->DESC->AC } )
    ->delete;
  if ( $famObj->DESC->WIKI and ref( $famObj->DESC->WIKI ) eq 'HASH' ) {
    foreach my $page ( keys %{ $famObj->DESC->WIKI } ) {
      my $wiki =
        $self->getSchema->resultset('Wikipedia')
        ->find_or_create( { title => $page, } );

      unless ( $wiki->auto_wiki ) {
        confess( "Failed to find or create row for wiki page" . $page . "\n" );
      }

      $self->getSchema->resultset('PfamAWiki')->find_or_create(
        {
          pfama_acc  => $famObj->DESC->AC,
          auto_wiki  => $wiki->auto_wiki
        }
      );
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
#Add the references to the literature reference table if it is not there.
#Then added the information pfamA_literature_reference table.
  $self->getSchema->resultset('PfamALiteratureReference')
    ->search( { pfama_acc => $famObj->DESC->AC } )->delete;
  if ( $famObj->DESC->REFS and ref( $famObj->DESC->REFS ) eq 'ARRAY' ) {
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
      unless ( $dbRef->auto_lit ) {
        confess( "Failed to find references for pmid " . $ref->{RM} . "\n" );
      }
      $self->getSchema->resultset('PfamALiteratureReference')->create(
        {
          pfama_acc   => $famObj->DESC->AC,
          auto_lit    => $dbRef,
          comment     => $ref->{RC} ? $ref->{RC} : '',
          order_added => $ref->{RN}
        }
      );
    }
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
  $self->getSchema->resultset('PfamADatabaseLink')
    ->search( { pfama_acc => $famObj->DESC->AC } )->delete;
  if ( $famObj->DESC->DBREFS and ref( $famObj->DESC->DBREFS ) eq 'ARRAY' ) {
    foreach my $dbLink ( @{ $famObj->DESC->DBREFS } ) {
      $self->getSchema->resultset('PfamADatabaseLink')->create(
        {
          pfama_acc    => $famObj->DESC->AC,
          db_id        => $dbLink->{db_id},
          comment      => $dbLink->{db_comment} ? $dbLink->{db_comment} : '',
          db_link      => $dbLink->{db_link},
          other_params => $dbLink->{other_params}
          ? $dbLink->{other_params}
          : ''
        }
      );
    }
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

 # my $auto;
 # if ( $famObj->rdb->{auto} ) {
 #   $auto = $famObj->rdb->{auto};
 # }
 # else {
 #   my $pfamA =
 #     $self->getSchema->resultset('Pfama')
 #     ->find( { pfamA_id => $famObj->DESC->ID } );
#
#    if ( $pfamA->pfama_id ) {
#      $auto = $pfamA->auto_pfama;
#      $famObj->rdb->{auto} = $auto;
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
#    }
#  }

  $self->getSchema->resultset('NestedDomain')
    ->search( { pfamA_acc => $famObj->DESC->AC } )->delete;

  $self->getSchema->resultset('NestedLocation')
    ->search( { pfamA_acc => $famObj->DESC->AC } )->delete;

  if ( $famObj->DESC->NESTS and ref( $famObj->DESC->NESTS ) eq 'ARRAY' ) {
    foreach my $n ( @{ $famObj->DESC->NESTS } ) {
      my $otherPfamA =
        $self->getSchema->resultset('PfamA')
        ->find( { pfamA_acc => $n->{dom} } );

      my $otherAuto;
      if ( $otherPfamA->pfamA_id ) {
        $otherAuto = $otherPfamA->pfamA_acc;
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

      unless ( $seq and $seq->pfamseq_acc ) {
        confess(
          'Could not find sequence ' . $n->{seq} . ' in the pfamseq table' );
      }

      $self->getSchema->resultset('NestedDomain')->create(
        {
          pfamA_acc       => $famObj->DESC->AC,
          nests_pfamA_acc => $otherPfamA->pfamA_acc
        }
      );

      $self->getSchema->resultset('NestedLocation')->create(
        {
          pfamA_acc         => $famObj->DESC->AC,
          nested_pfamA_acc  => $otherPfamA->pfamA_acc,
          pfamseq_acc       => $seq->pfamseq_acc,
          seq_version       => $seq->seq_version,
          seq_start         => $n->{from},
          seq_end           => $n->{to},
        }
      );
    }
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

#  my $auto;
#  if ( $famObj->rdb->{auto} ) {
#    $auto = $famObj->rdb->{auto};
#  }
#  else {
#    my $pfamA =
#      $self->getSchema->resultset('Pfama')
#      ->find( { pfamA_id => $famObj->DESC->ID } );
#
#    if ( $pfamA->pfama_id ) {
#      $auto = $pfamA->auto_pfama;
#      $famObj->rdb->{auto} = $auto;
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
#    }
#  }

  $self->getSchema->resultset('Edit')->search( { pfamA_acc => $famObj->DESC->AC } )
    ->delete;

  if ( $famObj->DESC->EDITS and ref( $famObj->DESC->EDITS ) eq 'ARRAY' ) {
    foreach my $n ( @{ $famObj->DESC->EDITS } ) {

      #Now look up the sequence
      my ( $seqAcc, $version ) = $n->{seq} =~ /(\S+)\.(\d+)/;
      my $seq = $self->getSchema->resultset('Pfamseq')->find(
        {
          pfamseq_acc => $seqAcc,
          seq_version => $version
        }
      );

      unless ( $seq and $seq->pfamseq_acc ) {
        confess(
          'Could not find sequence ' . $n->{seq} . ' in the pfamseq table' );
      }

      if (  $n->{newFrom}
        and $n->{newTo}
        and $n->{newFrom} >= 1
        and $n->{newTo} > 1 )
      {
        $self->getSchema->resultset('Edit')->create(
          {
            pfamA_acc     => $famObj->DESC->AC,
            pfamseq_acc    => $seq->pfamseq_acc,
            seq_version    => $seq->seq_version,
            original_start => $n->{oldFrom},
            original_end   => $n->{oldTo},
            new_start      => $n->{newFrom},
            new_end        => $n->{newTo}
          }
        );
      }
      else {
        $self->getSchema->resultset('Edit')->create(
          {
            pfamA_acc      => $famObj->DESC->AC,
            pfamseq_acc    => $seq->pfamseq_acc,
            seq_version    => $seq->seq_version,
            original_start => $n->{oldFrom},
            original_end   => $n->{oldTo},
          }
        );
      }
    }
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

#  my $auto;
#  if ( $famObj->rdb->{auto} ) {
#    $auto = $famObj->rdb->{auto};
#  }
#  else {
#    my $pfamA =
#      $self->getSchema->resultset('Pfama')
#      ->find( { pfamA_id => $famObj->DESC->ID } );
#
#    if ( $pfamA->pfama_id ) {
#      $auto = $pfamA->auto_pfama;
#      $famObj->rdb->{auto} = $auto;
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
#    }
#  }

  $self->getSchema->resultset('PfamAHmm')->update_or_create(
    {
      pfamA_acc => $famObj->DESC->AC,
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

#  my $auto;
#  if ( $famObj->rdb->{auto} ) {
#    $auto = $famObj->rdb->{auto};
#  }
#  else {
#    my $pfamA =
#      $self->getSchema->resultset('Pfama')
#      ->find( { pfamA_id => $famObj->DESC->ID } );
#
#    if ( $pfamA->pfama_id ) {
#      $auto = $pfamA->auto_pfama;
#      $famObj->rdb->{auto} = $auto;
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
#    }
#  }

  $self->getSchema->resultset('AlignmentAndTree')->update_or_create(
    {
      pfamA_acc => $famObj->DESC->AC,
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

#  my $auto;
#  if ( $clanObj->rdb->{auto} ) {
#    $auto = $clanObj->rdb->{auto};
#  }
#  else {
#    my $clan =
#      $self->getSchema->resultset('Clan')
#      ->find( { clan_acc => $clanObj->DESC->AC } );
#
#    if ( $clan->clan_id ) {
#      $auto = $clan->auto_pfama;
#      $clanObj->rdb( { auto => $clan->auto_clan } );
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
#    }
#  }

#-------------------------------------------------------------------------------
  $self->getSchema->resultset('ClanDatabaseLink')
    ->search( { clan_acc => $clanObj->DESC->AC } )->delete;

  foreach my $dbLink ( @{ $clanObj->DESC->DBREFS } ) {
    $self->getSchema->resultset('ClanDatabaseLink')->create(
      {
        clan_acc     => $clanObj->DESC->AC,
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

  unless ( $clanObj and $clanObj->isa('Bio::Pfam::Clan::Clan') ) {
    confess("Did not get a Bio::Pfam::Clan::Clan object");
  }

#-------------------------------------------------------------------------------
#Get the index for the clan

#  my $auto;
#  if ( $clanObj->rdb->{auto} ) {
#    $auto = $clanObj->rdb->{auto};
#  }
#  else {
#    my $clan =
#      $self->getSchema->resultset('Clans')
#      ->find( { clan_acc => $clanObj->DESC->AC } );
#
#    if ( $clan->clan_id ) {
#      $auto = $clan->auto_pfama;
#      $clanObj->rdb( { auto => $clan->auto_clan } );
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
#    }
#  }

#-------------------------------------------------------------------------------
#Add the page to the wikipedia table if it is not there.
#Then added the information pfamA_literature_reference table.
  $self->getSchema->resultset('ClanWiki')->search( { clan_acc => $clanObj->DESC->AC } )
    ->delete;

  if ( $clanObj->DESC->WIKI and ref( $clanObj->DESC->WIKI ) eq 'HASH' ) {
    foreach my $page ( keys %{ $clanObj->DESC->WIKI } ) {
      my $wiki =
        $self->getSchema->resultset('Wikipedia')
        ->find_or_create( { title => $page, } );

      unless ( $wiki->auto_wiki ) {
        confess( "Failed to find or create row for wiki page" . $page . "\n" );
      }

      $self->getSchema->resultset('ClanWiki')->create(
        {
          clan_acc => $clanObj->DESC->AC,
          auto_wiki => $wiki->auto_wiki
        }
      );
    }
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

#  my $auto;
#  if ( $clanObj->rdb->{auto} ) {
#    $auto = $clanObj->rdb->{auto};
#  }
#  else {
#    my $clan =
#      $self->getSchema->resultset('Clans')
#      ->find( { clan_acc => $clanObj->DESC->AC } );
#
#    if ( $clan->clan_id ) {
#      $auto = $clan->auto_pfama;
#      $clanObj->rdb( { auto => $clan->auto_clan } );
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $clanObj->DESC->ID . "\n" );
#    }
#  }

#-------------------------------------------------------------------------------
#Add the references to the literature reference table if it is not there.
#Then added the information pfamA_literature_reference table.
  $self->getSchema->resultset('ClanLitRef')->search( { clan_acc => $clanObj->DESC->AC } )
    ->delete;

  foreach my $ref ( @{ $clanObj->DESC->REFS } ) {
    my $dbRef =
      $self->getSchema->resultset('LiteratureReference')->find_or_create(
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
    $self->getSchema->resultset('ClanLitRef')->create(
      {
        clan_acc    => $clanObj->DESC->AC,
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

#  my $auto;
#  if ( $famObj->rdb->{auto} ) {
#    $auto = $famObj->rdb->{auto};
#  }
#  else {
#    my $pfamA =
#      $self->getSchema->resultset('Pfama')
#      ->find( { pfamA_id => $famObj->DESC->ID } );
#
#    if ( $pfamA->pfama_id ) {
#      $auto = $pfamA->auto_pfama;
#      $famObj->rdb->{auto} = $auto;
#    }
#    else {
#      confess( "Did not find an mysql entry for " . $famObj->DESC->ID . "\n" );
#    }
#  }

  $self->getSchema->resultset('PfamAInternal')->update_or_create(
    {
      pfama_acc => $famObj->DESC->AC,
      seed => defined($seedString) ? Compress::Zlib::memGzip($seedString) : '',
      full => defined($fullString) ? Compress::Zlib::memGzip($fullString) : '',
    }
  );
}

sub resetInFull {
  my ( $self, $pfama_acc ) = @_;

  my @regions =
    $self->getSchema->resultset('PfamARegFullSignificant')
    ->search( { pfamA_acc => $pfama_acc } );

  foreach my $r (@regions) {
    $r->update( { in_full => 1 } );
  }

}

sub resetClanCompeteFlag {
  my ( $self, $clan ) = @_;

  my $clanData;
  if ( $clan =~ /^CL(\d{4})$/ ) {

    #Looks like an auto incremeneted key
    $clanData =
      $self->getSchema->resultset("Clan")->find( { "clan_acc" => $clan } );
  } else {
    $clanData = $self->getClanData($clan);
  }

  unless ( $clanData->isa('PfamLive::Result::Clan') ) {
    croak("Failed to get a clan row object for $clan");
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

