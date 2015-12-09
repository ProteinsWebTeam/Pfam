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
use Cwd;
use File::Slurp;

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => (
  default => 'calStore',
);

sub submitToFarm {
  my ($self) = @_;
 
#first get database dump and split it if it doesn't already exist
  my $dbh = $self->pfamdb->getSchema->storage->dbh; 
  my $host = $self->pfamdb->{host};
  my $user = $self->pfamdb->{user};
  my $pass = $self->pfamdb->{password};
  my $port = $self->pfamdb->{port};
  my $db = $self->pfamdb->{database};
  unless (-s "pfamseq_dump_store"){
  $self->logger->debug("Getting pfamseq data from database");
  my $cmd = "mysql -h $host -u $user -p$pass -P $port $db --quick -e \"select pfamseq_acc, pfamseq_id, sequence, species, ncbi_taxid, description, length from pfamseq\" > pfamseq_dump_store";
  system($cmd) and $self->logger->logdie("Could not obtain data from pfamseq");
  system("split -d -l 1000000 pfamseq_dump_store pfamseq_dump_store_") and $self->logger->logdie("Could not split pfamseq_dump_store");
  }

  #get number of jobs
  my $dir = getcwd;
  my $noJobs = 0;
  my @files = read_dir($dir);
  foreach my $file (@files){
    if ( $file =~ /pfamseq_dump_store_\d+/ ){
        $noJobs++;
    }
  }
  $self->logger->debug("Submitting $noJobs storable farm jobs");
  
  #Now submit the jobs
  my $queue = 'production-rh6';
  my $resource = "rusage[mem=4000]";
  my $memory = 4000;  
  my $fh = IO::File->new();
  $fh->open( "| bsub -q $queue  -M $memory -R $resource -o ".
              $self->options->{statusdir}."/store.\%J.\%I.log  -JStore\"[1-$noJobs]%41\"");
  $fh->print( "makeStorables.pl -chunk \$\{LSB_JOBINDEX\} -statusdir ".$self->options->{statusdir}."\n");

  $fh->close;
  $self->logger->debug("Status is:".$self->statusFile."\n");
  while(! $self->statusCheck($self->statusFile, $noJobs)){
    $self->logger->info('Waiting for jobs to complete.');
    sleep(60);
  }
}

sub updateSeqRange {
  my( $self ) = @_;
  my $chunk = $self->options->{chunk};

  #if chunk <10 need to add a zero so correct input file is found, also files start at 0 not 1 so decrement chunk size
  $chunk--;
  if ($chunk < 10){
    $chunk = 0 . $chunk;
  }

  #parse file
  my %pfamseq;
  my $file = 'pfamseq_dump_store_' . $chunk;
  $self->logger->debug("Working on file $file");
  my @lines = read_file($file);

 foreach my $line (@lines){
    my @data = split(/\t+/, $line);
    my $seq = $data[0];
 #skip header (applies to first file only)
    if ($seq eq 'pfamseq_acc'){
        next;
    }
    my $acc = $data[0];
    my $id = $data[1];
    my $sequence = $data[2];
    my $species = $data[3];
    my $taxid = $data[4];
    my $de = $data[5];
    my $len = $data[6];
    $pfamseq{$acc}{'id'}=$id;
    $pfamseq{$acc}{'seq'}=$sequence;
    $pfamseq{$acc}{'species'}=$species;
    $pfamseq{$acc}{'taxid'}=$taxid;
    $pfamseq{$acc}{'description'}=$de;
    $pfamseq{$acc}{'length'}=$len;

  }


    $self->updateStorables( \%pfamseq );
    $self->touchStatus($self->statusFile.".$chunk.done");
} #end of update sequence range

#TODO - this will no longer work as updateStorables expects a hash containing all the data from pfamseq
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
      $self->pfamdb->getSchema->resultset('PfamA')->find( { pfama_acc => $n->nests_pfama_acc->pfama_acc } )
      ->pfama_acc;

    my $pfamA = $n->pfama_acc->pfama_acc;
    $nestings->{$pfamA}->{$npfamA}++;
  }

  my $dbh = $self->pfamdb->getSchema->storage->dbh;
  my $delete_sth=$dbh->prepare("delete from pfam_annseq where pfamseq_acc=?");

  #TODO to rescue it - make a seen hash - pfamseq_accs in pfam_annseq and only do the following unless in seen

  foreach my $acc (keys %$modSeqsRef){
      my $id = $modSeqsRef->{$acc}{'id'};
      my $sequence = $modSeqsRef->{$acc}{'seq'};
      my $species = $modSeqsRef->{$acc}{'species'};
      my $taxid = $modSeqsRef->{$acc}{'taxid'};
      my $description = $modSeqsRef->{$acc}{'description'};
      my $length = $modSeqsRef->{$acc}{'length'};
      chomp $length;
      my ( @markups, @motifs, @regions );

    #PfamA region statement
    my $pfamaRegionsRef = $self->pfamdb->getPfamRegionsForSeq( $acc );

    if(defined($pfamaRegionsRef) and ref($pfamaRegionsRef) eq 'ARRAY'){
    for ( my $i = 0 ; $i < scalar @$pfamaRegionsRef ; $i++ ) {

      my $region = $pfamaRegionsRef->[$i];
      $self->logger->debug( $region->pfama_acc->pfama_acc . " $acc/"
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
        next unless ( $nestings->{ $region->pfama_acc->pfama_acc } );
        if (
              defined( $pfamaRegionsRef->[$j] )
          and $pfamaRegionsRef->[$j]->seq_start >= $regions[$#regions]->start
          and $pfamaRegionsRef->[$j]->seq_end <= $regions[$#regions]->end
          and defined(
            $nestings->{ $region->pfama_acc->pfama_acc }
              ->{ $pfamaRegionsRef->[$j]->pfama_acc->pfama_acc }
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
#-------------------------------------------------------------------------------
#Motifs
#transmembrane regions etc
    my $otherRegRef = $self->pfamdb->getOtherRegs( $acc );

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

#-------------------------------------------------------------------------------
#Markups

    #disulphides
    my $disulphideRef = $self->pfamdb->getDisulphidesForSeq( $acc );
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
    my $markupRef = $self->pfamdb->getMarkupForSeq( $acc );
    foreach my $markup (@$markupRef) {
      $self->logger->debug("Found markup");
      my $ann = $markup->label;
      $ann .= ( $markup->annotation ? ':' . $markup->annotation : '' );
      push(
        @markups,
        Bio::Pfam::Sequence::Markup->new(
          {
            start    => $markup->residue,
            residue  => substr( $sequence, $markup->residue - 1, 1 ),
            type     => $markup->label,
            metadata => Bio::Pfam::Sequence::MetaData->new(
              {
                start       => $markup->residue,
                description => substr( $sequence, $markup->residue - 1, 1 )
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
        organism    => $species,
        taxid       => $taxid,
        accession   => $acc,
        identifier  => $id,
        description => $description,
        database    => 'uniprot'
      }
    );

    my $seqObj = Bio::Pfam::Sequence->new(
      {
        metadata => $meta,
        length   => $length,
        regions  => \@regions,
        motifs   => \@motifs,
        markups  => \@markups,
      }
    );

    my $str = nfreeze($seqObj);

    $delete_sth->execute($acc) or $self->logger->logdie("Couldn't execute statement ".$delete_sth->errstr);
      
    $self->pfamdb->getSchema->resultset('PfamAnnseq')->update_or_create(
      {
        pfamseq_acc    => $acc,
        annseq_storable => $str
      }
    );
  }#end of loop through each seq
}
1;
