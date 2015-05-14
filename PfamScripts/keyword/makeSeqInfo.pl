#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use POSIX qw(ceil);
use Data::Printer;
use Getopt::Long;

my ( $datadir, $chunk, $chunkSize, $all, $nojobs );

GetOptions(
  'all'      => \$all,
  'size=i'   => \$chunkSize,
  'chunk=i'  => \$chunk,
  'dir=s'    => \$datadir,
  'nojobs=i' => \$nojobs
) or die "Illegal option passed in\n";

my $config = Bio::Pfam::Config->new;
my $pfamdb = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

if ($all) {
  $nojobs = 50 if(!$nojobs);
  submitToFarm( $pfamdb, $nojobs, $datadir );
}
elsif ( $chunk and $chunkSize ) {

  #Setup the data directory
  unless ( -d $datadir . "/" . $chunk ) {
    mkdir( $datadir . "/" . $chunk )
      or die "Could not make $datadir/$chunk, [$!]\n";
  }
  $datadir .= "/" . $chunk;
  my $dbh = $pfamdb->getSchema->storage->dbh;

  #The query!
  my $sthRegions =
    $dbh->prepare( "SELECT DISTINCT s.description, species "
      . "FROM pfamA_reg_full_significant r, pfamseq s "
      . "WHERE s.pfamseq_acc=r.pfamseq_acc "
      . "AND in_full=1 AND pfamA_acc=?" );

  #Now work out the range of accessions
  my $pfamAll = $pfamdb->getSchema->resultset('PfamA')->search(
    {},
    {
      order_by => 'pfama_id',
      page     => 1,
      rows     => $chunkSize
    }
  );

  my $pager = $pfamAll->pager;
  print STDERR "Working on page $chunk out of " . $pager->last_page . "\n";
  my @pfams = $pfamAll->page($chunk)->all;

  foreach my $p (@pfams) {
    print STDERR "Working on " . $p->pfama_acc . "\n";
    next if ( -s $datadir . "/" . $p->pfama_acc . ".res.kw" );

    $sthRegions->execute( $p->pfama_acc );
    my $allSeqs = $sthRegions->fetchall_arrayref;
    my ( %words, %species );
    foreach my $row (@$allSeqs) {
      my @w = split( /\s+/, $row->[0] );
      foreach my $w (@w) {
        $w = lc($w);
        $w =~ s/\,$//;        #Remove trailing commas
        $w =~ s/\.$//;        #Remove trailing commas
        $words{$w}++;
      }
      $species{ $row->[1] }++;
    }
    open( R, '>', $datadir . "/" . $p->pfama_acc . ".res.kw" )
      or die "Could not open keyword file :[$!]\n";
    open( S, '>', $datadir . "/" . $p->pfama_acc . ".res.sp" )
      or die "Could not open species file :[$!]\n";

    foreach my $w ( keys %words ) {
      print R $w . " ";
    }

    foreach my $s ( keys %species ) {
      print S $s . " ";
    }
    close(R);
    close(S);
  }
}

sub submitToFarm {
  my ( $pfamdb, $noJobs, $datadir ) = @_;

  my $rs = $pfamdb->getSchema->resultset('PfamA')->search( {} );
  my $chunkSize = ceil( $rs->count / $noJobs );

  #Now submit the jobs
  my $queue = 'production-rh6';
  my $resource = "-M3500 -R rusage[mem=3500]";
  my $fh     = IO::File->new();

  $fh->open( "| bsub -q $queue  "
      . $resource . " -o "
      . $datadir
      . "/seqinfo.\%J.\%I.log  -Jseqinfo\"[1-$noJobs]\"" );
  $fh->print(
"makeSeqInfo.pl -chunk \$\{LSB_JOBINDEX\} -size $chunkSize -dir $datadir\n"
  );
  $fh->close;
}
