#!/usr/bin/env perl

use strict;
use warnings;

use Log::Log4perl qw(get_logger :levels);
use Data::Dump qw(dump);
use Getopt::Long;
use Storable qw(nfreeze);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess;
use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::Motif;
use Bio::Pfam::Sequence::Markup;
use Bio::Pfam::Drawing::Layout::Config::PfamaConfig;

Log::Log4perl->init(
  \<<EOF
log4perl.rootLogger=DEBUG, SCREEN
# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);


my ($chunk, $chunkSize, $acc);
GetOptions( "chunk=i" => \$chunk,
            "chunkSize=i" => \$chunkSize,
            "acc=s" => \$acc) or die "Invalid option\n";



my $logger = get_logger();
$logger->level($INFO);

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}
my $dbh = $pfamDB->getSchema->storage->dbh;
$logger->debug("Got pfamlive database connection");

if($acc and $acc =~ /PF\d{5}/){
  my $pfamA = $pfamDB->getPfamData($acc);
  my @seqsRS = $pfamDB->getSchema->resultset('Pfamseq')->search(
    { 
      "pfama_reg_full_significants.auto_pfamA" => $pfamA->auto_pfama,
      "pfama_reg_full_significants.in_full" => 1
    },
    {
      join => [qw(pfama_reg_full_significants)]
    }
    );
  
  makeStorables( \@seqsRS, $pfamDB, $logger );

}elsif($chunk and $chunkSize){
  my $rangeFrom = (($chunk-1) * $chunkSize)+1; 
  my $rangeTo   = (($chunk) * $chunkSize); 
  $logger->debug("Calculating architectures in the range of $rangeFrom to $rangeTo.");
#-------------------------------------------------------------------------------

my $currentSeq = $rangeFrom;
  while($currentSeq < $rangeTo){ 
    $pfamDB->getSchema->txn_begin;
    my $nextCurrentSeq = ($currentSeq + 1000) > $rangeTo ? $rangeTo : $currentSeq + 1000; 
    $logger->debug("Working on $currentSeq to $nextCurrentSeq");
    my @seqsRS = $pfamDB->getSchema->resultset('Pfamseq')->search(
      {'me.auto_pfamseq' => [ -and => {'>=', $currentSeq },{'<=',$nextCurrentSeq }]});   
    $currentSeq = $nextCurrentSeq;  
    makeStorables( \@seqsRS, $pfamDB, $logger );
    $pfamDB->getSchema->txn_commit;
  }
}else{
   die "No range or accession entered.\n";
}








sub makeStorables{
  my ( $seqsRef, $pfamDB, $logger ) = @_;


  my $nestings;

  foreach my $n ( @{ $pfamDB->getAllNestedDomains } ) {
    my $npfamA =
      $pfamDB->getSchema->resultset('Pfama')->find( {auto_pfama => $n->nests_auto_pfama} )->pfama_acc;
    my $pfamA = $n->auto_pfama->pfama_acc;
    $nestings->{$pfamA}->{$npfamA}++;
  }
  my %seen;
  foreach my $seq (@$seqsRef) {
    next if(exists($seen{$seq->pfamseq_acc}));
    $seen{$seq->pfamseq_acc}++;
    $logger->info("Working on sequence:".$seq->pfamseq_acc);
      my ( @markups, @motifs, @regions );
    #PfamA region statement
    my $pfamaRegionsRef = $pfamDB->getPfamRegionsForSeq( $seq->pfamseq_acc );
    if($pfamaRegionsRef){
    for ( my $i = 0 ; $i < scalar @$pfamaRegionsRef ; $i++ ) {

      #for ( my $i = 0 ; $i < 2 ; $i++ ) {
      my $region = $pfamaRegionsRef->[$i];
      $logger->debug( $region->pfama_acc . "/"
          . $region->seq_start . "-"
          . $region->seq_end );
      push(
        @regions,
        Bio::Pfam::Sequence::Region->new(
          {
            start       => $region->seq_start,
            end         => $region->seq_end,
            aliStart    => $region->ali_start,
            aliEnd      => $region->ali_end,
            modelStart  => $region->model_start,
            modelEnd    => $region->model_end,
            modelLength => $region->model_length,
            metadata    => Bio::Pfam::Sequence::MetaData->new(
              {
                accession   => $region->pfama_acc,
                identifier  => $region->pfama_id,
                type        => $region->type,
                description => $region->description,
                score       => $region->domain_evalue_score,
                scoreName   => 'e-value',
                start       => $region->seq_start,
                end         => $region->seq_end,
                aliStart    => $region->ali_start,
                aliEnd      => $region->ali_end,
                database    => 'pfam'
              }
            ),
            type => 'pfama'
          }
        )
      );

      for ( my $j = ( $i + 1 ) ; $j < scalar(@$pfamaRegionsRef) ; $j++ ) {
        next unless ( $nestings->{ $region->pfama_acc } );
        if (
              defined( $pfamaRegionsRef->[$j] )
          and $pfamaRegionsRef->[$j]->seq_start >= $regions[$#regions]->start
          and $pfamaRegionsRef->[$j]->seq_end <= $regions[$#regions]->end
          and defined(
            $nestings->{ $region->pfama_acc }
              ->{ $pfamaRegionsRef->[$j]->pfama_acc }
          )
          )
        {

          #It nests
          $regions[$#regions]->end( $pfamaRegionsRef->[$j]->seq_start - 1 );
          $regions[$#regions]->aliEnd( $pfamaRegionsRef->[$j]->seq_start - 1 );
          push(
            @regions,
            Bio::Pfam::Sequence::Region->new(
              {
                start       => $pfamaRegionsRef->[$j]->seq_end + 1,
                end         => $region->seq_end,
                aliStart    => $pfamaRegionsRef->[$j]->seq_end + 1,
                aliEnd      => $region->ali_end,
                modelStart  => $region->model_start,
                modelEnd    => $region->model_end,
                modelLength => $region->model_length,
                metadata    => Bio::Pfam::Sequence::MetaData->new(
                  {
                    accession   => $region->pfama_acc,
                    identifier  => $region->pfama_id,
                    type        => $region->type,
                    description => $region->description,
                    score       => $region->domain_evalue_score,
                    scoreName   => 'e-value',
                    start       => $region->seq_start,
                    end         => $region->seq_end,
                    aliStart    => $region->ali_start,
                    aliEnd      => $region->ali_end,
                    database    => 'pfam'
                  }
                ),
                type => 'pfama'
              }
            )
          );
          push(
            @markups,
            Bio::Pfam::Sequence::Markup->new(
              {
                start    => $pfamaRegionsRef->[$j]->seq_start - 1,
                end      => $pfamaRegionsRef->[$j]->seq_end + 1,
                type     => 'Nested',
                metadata => Bio::Pfam::Sequence::MetaData->new(
                  {
                    database => 'pfam',
                    start    => $pfamaRegionsRef->[$j]->seq_start - 1,
                    end      => $pfamaRegionsRef->[$j]->seq_end + 1,
                    type     => 'Link between discontinous regions'
                  }
                ),
              }
            )
          );
        }
      }
      }
    }

    #Context regions
    my $contextRegRef = $pfamDB->getContextRegionsForSeq( $seq->pfamseq_acc );
    foreach my $region (@$contextRegRef) {
      $logger->debug( $region->pfama_acc . "/"
          . $region->seq_start . "-"
          . $region->seq_end );
      push(
        @regions,
        Bio::Pfam::Sequence::Region->new(
          {
            start    => $region->seq_start,
            end      => $region->seq_end,
            metadata => Bio::Pfam::Sequence::MetaData->new(
              {
                accession   => $region->pfama_acc,
                identifier  => $region->pfama_id,
                description => $region->description,
                score       => $region->domain_score,
                database    => 'pfam',
                type        => 'context'
              }
            ),
            type => 'context'
          }
        )
      );
    }

#-------------------------------------------------------------------------------
#Motifs
#transmembrane regions etc
    my $otherRegRef = $pfamDB->getOtherRegs( $seq->auto_pfamseq );
    foreach my $motif (@$otherRegRef) {
      $logger->debug("Found other region");
      push(
        @motifs,
        Bio::Pfam::Sequence::Motif->new(
          {
            start    => $motif->seq_start,
            end      => $motif->seq_end,
            type     => $motif->type_id,
            metadata => Bio::Pfam::Sequence::MetaData->new(
              {
                database => $motif->source_id,
                start    => $motif->seq_start,
                end      => $motif->seq_end,
                type     => $motif->type_id
              }
            ),
          }
        )
      );
      $motifs[$#motifs]->metadata->score( $motif->score )
        if ( $motif->score and $motif->score > 0 );
      $motifs[$#motifs]
        ->metadata->description( 'orientation:' . $motif->orientation )
        if ( $motif->orientation and $motif->orientation > 0 );
    }

    #Pfam-Bs will have to be added again after Pfam-B genereation
    #if ( $config->includePfamBInSequenceObj ) {
    my  $includePfamB = 1;
    if( $includePfamB ){
      my $pfambRegRef = $pfamDB->getPfambRegForSeq( $seq->pfamseq_acc );
      foreach my $motif (@$pfambRegRef) {
        $logger->debug("Found Pfam-B region");
        push(
          @motifs,
          Bio::Pfam::Sequence::Motif->new(
            {
              start    => $motif->seq_start,
              end      => $motif->seq_end,
              type     => 'pfamb',
              metadata => Bio::Pfam::Sequence::MetaData->new(
                {
                  database   => 'pfam',
                  identifier => $motif->pfamb_id,
                  accession  => $motif->pfamb_acc,
                  start      => $motif->seq_start,
                  end        => $motif->seq_end,
                  type       => 'Pfam-B'
                }
              ),
            }
          )
        );
      }
    }

#-------------------------------------------------------------------------------
#Markups

    #disulphides
    my $disulphideRef = $pfamDB->getDisulphidesForSeq( $seq->pfamseq_acc );
    foreach my $ds (@$disulphideRef) {
      $logger->debug("Found disulphide");
      if ( $ds->bond_end ) {
        push(
          @markups,
          Bio::Pfam::Sequence::Markup->new(
            {
              start    => $ds->bond_start,
              end      => $ds->bond_end,
              type     => 'disulphide',
              metadata => Bio::Pfam::Sequence::MetaData->new(
                {
                  database => 'UniProt',
                  start    => $ds->bond_start,
                  end      => $ds->bond_end,
                  type     => 'disulphide'
                }
              ),
            }
          )
        );
      }
      else {
        push(
          @markups,
          Bio::Pfam::Sequence::Markup->new(
            {
              start    => $ds->bond_start,
              type     => 'disulphide',
              metadata => Bio::Pfam::Sequence::MetaData->new(
                {
                  database    => 'UniProt',
                  start       => $ds->bond_start,
                  description => 'Intramolecular',
                  type        => 'disulphide'
                }
              ),
            }
          )
        );
      }
    }

    #    #add active site residues + others
    my $markupRef = $pfamDB->getMarkupForSeq( $seq->pfamseq_acc );
    foreach my $markup (@$markupRef) {
      $logger->debug("Found markup");
      my $ann = $markup->label;
      $ann .= ( $markup->annotation ? ':' . $markup->annotation : '' );

      push(
        @markups,
        Bio::Pfam::Sequence::Markup->new(
          {
            start    => $markup->residue,
            residue  => substr( $seq->sequence, $markup->residue - 1, 1 ),
            type     => $markup->label,
            metadata => Bio::Pfam::Sequence::MetaData->new(
              {
                start       => $markup->residue,
                description => substr( $seq->sequence, $markup->residue - 1, 1 )
                  . " "
                  . $ann,
                database => (
                  $markup->label eq 'Pfam predicted active site'
                  ? 'pfam'
                  : 'UniProt'
                ),
              }
            )
          }
        )
      );
    }

#-------------------------------------------------------------------------------
#Sequence
    my $meta = Bio::Pfam::Sequence::MetaData->new(
      {
        organism    => $seq->species,
        taxid       => $seq->ncbi_taxid->ncbi_taxid,
        accession   => $seq->pfamseq_acc,
        identifier  => $seq->pfamseq_id,
        description => $seq->description,
        database    => 'uniprot'
      }
    );

    my $seqObj = Bio::Pfam::Sequence->new(
      {
        metadata => $meta,
        length   => $seq->length,
        regions  => \@regions,
        motifs   => \@motifs,
        markups  => \@markups,
      }
    );

    my $str = nfreeze($seqObj);

    $pfamDB->getSchema->resultset('PfamAnnseq')->update_or_create(
      {
        auto_pfamseq    => $seq->auto_pfamseq,
        annseq_storable => $str
      }
    );
  }
}