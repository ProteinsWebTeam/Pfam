package Bio::Pfam::ViewProcess::Storable;

use strict;
use warnings;
use POSIX qw(ceil);
use Getopt::Long;
use Log::Log4perl qw(:easy);
use Storable qw(nfreeze);
use Moose;
use Moose::Util::TypeConstraints;
use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::Motif;
use Bio::Pfam::Sequence::Markup;
use Bio::Pfam::Drawing::Layout::Config::PfamaConfig;
use DDP;

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => (
  default => 'calStore',
);

sub submitToFarm {
  my ($self, $noJobs) = @_;
  
  my $rs = $self->pfamdb->getSchema->resultset('Pfamseq')->search({});
  my $count = $rs->count;
  my $chunkSize = ceil($count/$noJobs);
  
  #Now submit the jobs
  my $queue = 'production-rh6';
  my $resource = "rusage[mem=2500]";
  my $memory = 2500;  
  my $fh = IO::File->new();
  $fh->open( "| bsub -q $queue  -M $memory -R $resource -o ".
              $self->options->{statusdir}."/store.\%J.\%I.log  -JStore\"[1-$noJobs]%70\"");
  $fh->print( "makeStorables.pl -chunk \$\{LSB_JOBINDEX\} -chunkSize $chunkSize -statusdir ".$self->options->{statusdir}."\n");
  $fh->close;
  $self->logger->debug("Status is:".$self->statusFile."\n");
  while(! $self->statusCheck($self->statusFile, $noJobs)){
    $self->logger->info('Waiting for jobs to complete.');
    sleep(600);
  }
}

sub updateSeqRange {
  my( $self ) = @_;
  my $chunk = $self->options->{chunk};
  my $chunkSize = $self->options->{chunkSize};
  $self->touchStatus($self->statusFile.".$chunk");
  open(S, '+<', $self->options->{statusdir}.'/'.$self->statusFile.".$chunk") or 
      $self->logger->logdie("Could not open status file:[$!]"); 
  my $rangeFrom = ( ( $chunk - 1 ) * $chunkSize ) + 1;
  while(<S>){
    chomp;
    if(/\d+/ and ($rangeFrom < $_)){
        $rangeFrom = $_;
    }
  }
  
  my $rangeTo   = ( ($chunk) * $chunkSize );
  $self->logger->debug(
    "Building storables in the range of $rangeFrom to $rangeTo.");
  
#-------------------------------------------------------------------------------

  my $currentSeq = $rangeFrom;
  while ( $currentSeq < $rangeTo ) {
    $self->pfamdb->getSchema->txn_begin;
    my $nextCurrentSeq =
      ( $currentSeq + 3000 ) > $rangeTo ? $rangeTo : $currentSeq + 3000;
    $self->logger->debug("Working on $currentSeq to $nextCurrentSeq");

        my @seqsRS = $self->pfamdb->getSchema->resultset('Pfamseq')->search(
        {}, { rows => $chunkSize, page => $chunk}     
    );
    $self->updateStorables( \@seqsRS );
    $self->pfamdb->getSchema->txn_commit;
    print S "$nextCurrentSeq\n";
    $currentSeq = $nextCurrentSeq;
    
  }
  print S "$rangeTo\n";
  close(S);
  $self->touchStatus($self->statusFile.".$chunk.done");
}

sub updateSingleFamily {
  my($self) = @_;
  my $pfamA  = $self->pfamdb->getPfamData($self->options->{acc});
  my @seqsRS = $self->pfamdb->getSchema->resultset('Pfamseq')->search(
    {
      "pfama_reg_full_significants.pfamA_acc" => $pfamA->pfama_acc,
      "pfama_reg_full_significants.in_full"    => 1
    },
    { join => [qw(pfama_reg_full_significants)] }
  );
  
  #TODO - Need to do this in chunks to deal with scale, getting 200K hits may kill
  #Also, this query is going to give duplicate sequences when family occurs more
  #than once.
  $self->updateStorables( \@seqsRS );
}


sub updateStorables {
  my ( $self, $modSeqsRef) = @_;


  my $nestings;
  foreach my $n ( @{ $self->pfamdb->getAllNestedDomains } ) {
    my $npfamA =
      $self->pfamdb->getSchema->resultset('PfamA')->find( { pfama_acc => $n->nests_pfama_acc } )
      ->pfama_acc;
    my $pfamA = $n->pfama_acc->pfama_acc;
    $nestings->{$pfamA}->{$npfamA}++;
  }
  foreach my $seq (@$modSeqsRef) {
      my ( @markups, @motifs, @regions );
    #PfamA region statement
    my $pfamaRegionsRef = $self->pfamdb->getPfamRegionsForSeq( $seq->pfamseq_acc );
    if(defined($pfamaRegionsRef) and ref($pfamaRegionsRef) eq 'ARRAY'){
    for ( my $i = 0 ; $i < scalar @$pfamaRegionsRef ; $i++ ) {

      #for ( my $i = 0 ; $i < 2 ; $i++ ) {
      my $region = $pfamaRegionsRef->[$i];
      $self->logger->debug( $region->pfama_acc->pfama_acc . "/"
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
            modelLength => $region->pfama_acc->model_length,
            metadata    => Bio::Pfam::Sequence::MetaData->new(
              {
                accession   => $region->pfama_acc->pfama_acc,
                identifier  => $region->pfama_acc->pfama_id,
                type        => $region->pfama_acc->type,
                description => $region->pfama_acc->description,
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
  #TODO - section below hashed out as table doesn't exist - double check that we don't need this before deleting it
  #Context regions
#    my $contextRegRef = $self->pfamdb->getContextRegionsForSeq( $seq->pfamseq_acc );
#    foreach my $region (@$contextRegRef) {
#      $self->logger->debug( $region->pfama_acc . "/"
#          . $region->seq_start . "-"
#          . $region->seq_end );
#      push(
#        @regions,
#        Bio::Pfam::Sequence::Region->new(
#          {
#            start    => $region->seq_start,
#            end      => $region->seq_end,
#            metadata => Bio::Pfam::Sequence::MetaData->new(
#              {
#                accession   => $region->pfama_acc,
#                identifier  => $region->pfama_id,
#                description => $region->description,
#                score       => $region->domain_score,
#                database    => 'pfam',
#                type        => 'context'
#              }
#            ),
#            type => 'context'
#          }
#        )
#      );
#    }

#-------------------------------------------------------------------------------
#Motifs
#transmembrane regions etc
    my $otherRegRef = $self->pfamdb->getOtherRegs( $seq->pfamseq_acc );
    foreach my $motif (@$otherRegRef) {
      #$self->logger->debug("Found other region");
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

#TODO - is this still needed? hashed out for now 
#      my $pfambRegRef = $self->pfamdb->getPfambRegForSeq( $seq->pfamseq_acc );
#      foreach my $motif (@$pfambRegRef) {
#        $self->logger->debug("Found Pfam-B region");
#        push(
#          @motifs,
#          Bio::Pfam::Sequence::Motif->new(
#            {
#              start    => $motif->seq_start,
#              end      => $motif->seq_end,
#              type     => 'pfamb',
#              metadata => Bio::Pfam::Sequence::MetaData->new(
#                {
#                  database   => 'pfam',
#                  identifier => $motif->pfamb_id,
#                  accession  => $motif->pfamb_acc,
#                  start      => $motif->seq_start,
#                  end        => $motif->seq_end,
#                  type       => 'Pfam-B'
#                }
#              ),
#            }
#          )
#        );
#      }

#-------------------------------------------------------------------------------
#Markups

    #disulphides
    my $disulphideRef = $self->pfamdb->getDisulphidesForSeq( $seq->pfamseq_acc );
    foreach my $ds (@$disulphideRef) {
      $self->logger->debug("Found disulphide");
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
    my $markupRef = $self->pfamdb->getMarkupForSeq( $seq->pfamseq_acc );
    foreach my $markup (@$markupRef) {
      $self->logger->debug("Found markup");
      my $ann = $markup->label;
      $ann .= ( $markup->annotation ? ':' . $markup->annotation : '' );
      push(
        @markups,
        Bio::Pfam::Sequence::Markup->new(
          {
            start    => $markup->residue,
            residue  => substr( $seq->get_column('sequence'), $markup->residue - 1, 1 ),
            type     => $markup->label,
            metadata => Bio::Pfam::Sequence::MetaData->new(
              {
                start       => $markup->residue,
                description => substr( $seq->get_column('sequence'), $markup->residue - 1, 1 )
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
        taxid       => $seq->ncbi_taxid,
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

    $self->pfamdb->getSchema->resultset('PfamAnnseq')->update_or_create(
      {
        pfamseq_acc    => $seq->pfamseq_acc,
        annseq_storable => $str
      }
    );
  }
}
1;
