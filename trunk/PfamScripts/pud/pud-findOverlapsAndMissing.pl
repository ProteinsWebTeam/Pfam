#!/usr/bin/env perl
#
# A script to determine overlaps across all Pfam families found within a single directory
# During overlap resolution, all families should be in a single directory.  The script is
# designed to operate in one of two ways.
#
#  1. Slow and accurate, where all information is read into the file.
#  2. Faster, with the possibility that some overlap information will be missed.
#       - requires that the slow step has been run at least once.
#
# Some other points about these overlaps.
#   1.  All nestings where the families involved will be expanded to the whole
#       of the clan.
#   2.  Information collected from the DESC file will be stored between runs
#       and only re-read for families that still have overlaps.
#   3.  As of Feb 2015, clans are competed using E-value. Prior to this they were competed
#       by this script using bit score.
#   4.  Overlaps are filtered according to parameters in the config file, there is an option
#       to switch this off.

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Cwd;
use Data::Dumper;

use Bio::Range;

use Date::Object;
use Getopt::Long;
use Storable;

use Bio::Pfam::FamilyIO;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
my $logger = get_logger();

#-------------------------------------------------------------------------------
# Get the Pfam Config and check all is well.
my $config = Bio::Pfam::Config->new;

unless ($config)
{
  die
  "Failed to obtain a Pfam Config object, check that the environment variable PFAM_CONFIG is set and the file is there!\n";
}

#-------------------------------------------------------------------------------

my $no_compete;    # Use this option to not do the clan competition step
my ( $statusdir, $clans, $clan2fam, $fast, $oldOverlaps, $families, $ciFamilies, $all, $noFilter );

GetOptions(
  'fast'        => \$fast,
  'previous'    => \$oldOverlaps,
  'no_compete!' => \$no_compete,
  'statusdir=s' => \$statusdir,
  'datadir=s'   => \$families,
  'checked=s'   => \$ciFamilies,
  'all' => \$all,  # By default, the script checks for overlaps among sequences that belong to the reference proteomes
  # Use -all if you want to check against the whole pfamseq
  'no_filter' => \$noFilter,    # Do not apply filter rules for auto-resolving overlaps
) or $logger->logdie("Invalid options passed in!\n");

if ($fast)
{
unless ($oldOverlaps)
{
$logger->warn("-fast requires information about the old overlaps");
}
}

unless ($statusdir)
{
$logger->logdie("Need to know where to put status information");
}

my $posOverlaps;
if ($fast)
{
#Which families have overlaps?
$posOverlaps = parsePreviousOverlaps($oldOverlaps);
}
else
{
opendir( DIR, $families ) or $logger->logdie("Could not opendir $families");
my @dirs = grep { $_ =~ /^PF\d{5}$/ } readdir(DIR);
close(DIR);

#now these all have 'possible' overlaps
$posOverlaps = \@dirs;
}

my $familiesData = {};
if ( $fast and -e $statusdir . "/family.dat" )
{
$familiesData = retrieve( $statusdir . "/family.dat" );
}

#Some initial set up things.
my $pfamDB     = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $date       = new Date::Object( time() );
my $filePrefix = $date->year . $date->month . $date->day . "overlaps";
my $nestClans  = 1;
my $overlapLength;

#Retrieve DESC data from all of the possible overlapping families.
getDescData( $familiesData, $posOverlaps );

if ( defined($ciFamilies) )
{
opendir( DIR, $ciFamilies ) or $logger->logdie("Could not opendir $ciFamilies");
my @ciDirs = grep { $_ =~ /^PF\d{5}$/ } readdir(DIR);
close(DIR);

#now these all have 'possible' overlaps
getDescData( $familiesData, \@ciDirs );
}

#Save the information for later
store( $familiesData, $statusdir . "/family.dat" );

#Now go and get all of the region information
getRegions( $families, $posOverlaps, $familiesData );

#Now see if there are any overlaps between these regions.
checkForOverlap($familiesData);

#Filter the overlaps according to the auto-resolve paramaters found inside the config file
unless ($noFilter)
{
my $lengthLimit = $config->sequenceOverlapRule;
my $numberLimit = $config->familyOverlapRule;

filterOverlaps( $lengthLimit, $numberLimit );
}

#-------------------------------------------------------------------------------

sub getRegions {
my ( $families, $posOverlaps, $familiesData ) = @_;

# if all flag is used, then skip filtering out non reference proteomes regions

open( R, ">$statusdir/$filePrefix.allRegions.txt" )
  or die "Could not open $statusdir/$filePrefix.allRegions.txt\n";

foreach my $fDir ( @{$posOverlaps} ) {
  my $id = $familiesData->{$fDir}->{id};
  open( SEED, "$families/$fDir/SEED" );
  while (<SEED>) {
    if (/(\w+)\.?\S*\/(\d+)\-(\d+)/) {
      if ( !$all ) {
        my $rs = $pfamDB->getSchema->resultset('Pfamseq')
        ->search( { 'pfamseq_acc' => $1 }, { select => 'ref_proteome' } );

        my $isInRefProt;

        if ( $rs != 0 ) {
          $isInRefProt = $rs->next->ref_proteome;
        }
        else {
          print STDERR "Warning: Sequence $id not found in Pfam live, can not check if it belongs to reference proteomes\n";
          next;
        }

        if ( $isInRefProt == 1 ) {
          $familiesData->{$fDir}->{seed}++;
          print R "$1\t$2\t$3\t$fDir\t$id\tSEED\t**\t**\n";
        }
      }
      else {
        $familiesData->{$fDir}->{seed}++;
        print R "$1\t$2\t$3\t$fDir\t$id\tSEED\t**\t**\n";
      }
    }
  }
  close(SEED);

  open( SCORES, "$families/$fDir/scores" )
    or warn "Could not open scores file $fDir\n";
  while (<SCORES>) {
    if (/(\S+)\s+(\w+).?\S*\/(\d+\-\d+)\s+(\d+)\-(\d+)\s+(\S+)/) {
      if ( !$all ) {
        my $rs = $pfamDB->getSchema->resultset('Pfamseq')
        ->search( { 'pfamseq_acc' => $2 }, { select => 'ref_proteome' } );

        my $isInRefProt;

        if ( $rs != 0 ) {
          $isInRefProt = $rs->next->ref_proteome;
        }
        else {
          print STDERR "Warning: Sequence $id not found in Pfam live, can not check if it belongs to reference proteomes\n";
          next;
        }

        if ( $isInRefProt == 1 ) {
          print R "$2\t$4\t$5\t$fDir\t$id\tALIGN\t$1\t$6\n";
          $familiesData->{$fDir}->{align}++;
        }
      }
      else {
        print R "$2\t$4\t$5\t$fDir\t$id\tALIGN\t$1\t$6\n";
        $familiesData->{$fDir}->{align}++;
      }
    }

  }
  close(SCORES);
}
}

sub getDescData
{
  my ( $familiesData, $posOverlaps ) = @_;

  open( SKIP, ">$statusdir/$filePrefix.skipping" )
    or die "Could not open $statusdir/$filePrefix.skipping\n";

  my $io = Bio::Pfam::FamilyIO->new;
  FAM:
  foreach my $fDir ( @{$posOverlaps} )
  {
    open( D, "$families/$fDir/DESC" ) or die "Could not open $fDir/DESC:[$!]\n";

    #Now read the DESC to see it we have nested domains;

    my $descObj;
    eval { $descObj = $io->parseDESC( \*D ); };
    if ($@)
    {
      print SKIP "$fDir\n";
      close(D);
      next FAM;
    }

    $familiesData->{$fDir}->{acc}   = $descObj->AC;
    $familiesData->{$fDir}->{id}    = $descObj->ID;
    $familiesData->{$fDir}->{seed}  = 0;
    $familiesData->{$fDir}->{align} = 0;
    if ( $descObj->NESTS )
    {
      foreach my $n ( @{ $descObj->NESTS } )
      {
        push( @{ $familiesData->{$fDir}->{allowed} },         $n->{'dom'} );
        push( @{ $familiesData->{ $n->{'dom'} }->{allowed} }, $fDir );
      }
    }

    if ( $descObj->CL )
    {
      $clans->{$fDir} = $descObj->CL;
      push( @{ $clan2fam->{ $descObj->CL } }, $fDir );
    }
    close(D);
  }
  close(SKIP);
}

sub checkForOverlap
{

  print STDERR "Sorting all regions\n";
  system("sort -k1,1 -k8,8g $statusdir/$filePrefix.allRegions.txt > $statusdir/$filePrefix.allRegionsSorted.txt")  #Sort by seq accession first, then by evalue second
    and $logger->logdie("Failed to sort regions.");
  print STDERR "Finished sorting, looking for overlaps\n";

  open( SORTEDREG, "$statusdir/$filePrefix.allRegionsSorted.txt" )
    or die "Could not open allRegionsSorted.txt:[$!]\n";

  my $previousAcc = '';
  my $regions;
  my %overlaps;

  open( OVERLAPS, ">$statusdir/$filePrefix.overlaps" )
  || die "Could not open overlap file: $!";

  while (<SORTEDREG>)
  {
    chomp;
    my @line = split( /\s+/, $_ );

    # If we have got to a new accession check all regions. Note there is possibly
    # a bug here.  The final set of regions will never get inspected.
    if ( $previousAcc and $line[0] ne $previousAcc )
    {
      checkRegions( $previousAcc, $regions, $clans, \%overlaps );
      $regions = undef;
    }

    push(
      @{$regions},
      { fam    => $line[4],
        acc    => $line[3],
        start  => $line[1],
        end    => $line[2],
        score  => $line[6],
        ali    => $line[5],
        evalue => $line[7], 
        skip   => 0           # Identify region that get outcompeted
      }
    );
    $previousAcc = $line[0];
  }
  checkRegions( $previousAcc, $regions, $clans, \%overlaps );
  close SORTEDREG;
  # Write out summary of all overlaps
  open( F, ">$statusdir/$filePrefix.familyOverlaps" );
  printf( F "%-7s\t%-20s\t%-8s\t%-8s\t%-8s\t%s\n", "acc", "id", "NoSeed", "NoFull", "NoOverlaps", "OverlapFams" );
  foreach my $f ( keys %$familiesData )
  {
    my ( $overlaps, $list );
    $overlaps = 0;
    $list     = '';
    foreach my $of ( keys %{ $overlaps{$f} } )
    {
      $list .= $of . ",";
      $overlaps += $overlaps{$f}->{$of};
    }
    $list = '-' unless ( $list =~ /\S+/ );

    printf( F "%-7s\t%-20s\t%-8d\t%-8d\t%-8s\t%s\n",
      $f,
      $familiesData->{$f}->{id},
      $familiesData->{$f}->{seed},
      $familiesData->{$f}->{align},
      $overlaps, $list );
  }
}

sub checkRegions {
  my ( $protein, $regions, $clans, $overlaps ) = @_;

  # Do clan competition first to set all the skip flags
  # This means if there are two overlapping regions that are to families in the same clan
  # only the match with the lowest evalue will be kept
  if ( !$no_compete ) {
    for ( my $i = 0; $i <= $#{$regions}; $i++ ) {
      for ( my $j = $i + 1; $j <= $#{$regions}; $j++ ) {
        if ( $clans->{ $regions->[$i]->{acc} } and $clans->{ $regions->[$j]->{acc} } ) {
          if ( $clans->{ $regions->[$i]->{acc} } eq $clans->{ $regions->[$j]->{acc} } ){
            next if( $regions->[$i]->{evalue} eq '**' or $regions->[$j]->{evalue} eq '**'); #Keep any SEED overlaps
            next if($regions->[$i]->{start} > $regions->[$j]->{end} or $regions->[$j]->{start} > $regions->[$i]->end); #No need to compete if regions do not overlap

            $regions->[$j]->{skip} = 1; #$regions->[$i] will have the lower evalue, so $regions->[$j] will be outcompeted by it	      
          }
        }
      }
    }
  }

  # OK now do the real overlap checking section.
  for ( my $i = 0; $i <= $#{$regions}; $i++ ) {
    if ( $regions->[$i]->{skip} ) { #Region is outcompeted
      next;
    }
    REGION:
    for ( my $j = $i + 1; $j <= $#{$regions}; $j++ ) {

      # Ignore if regions are in same family
      if ( $regions->[$i]->{fam} eq $regions->[$j]->{fam} ) { next REGION; }

      if ( $regions->[$j]->{skip} ) { #Region is outcompeted
        next REGION;
      }

      if (     $clans->{ $regions->[$i]->{acc} } and $clans->{ $regions->[$j]->{acc} } ) {
        if ( $clans->{ $regions->[$i]->{acc} } eq $clans->{ $regions->[$j]->{acc} } ) {
          next REGION;
        }
      }

      # Possibly don't need both of these next two tests! Although they don't both get run!

      # Ignore if in list of allowed families (nested doms)
      if ( $familiesData->{ $regions->[$i]->{acc} }->{allowed} ) {
        foreach my $aFam ( @{ $familiesData->{ $regions->[$i]->{acc} }->{allowed} } ) {
          if ( $aFam eq $regions->[$j]->{acc} ) {
            next REGION;
          }
        }
      }

      # Ignore if in list of allowed families (nested doms)
      if ( $familiesData->{ $regions->[$j]->{acc} }->{allowed} ) {
        foreach my $aFam ( @{ $familiesData->{ $regions->[$j]->{acc} }->{allowed} } ) {
          if ( $aFam eq $regions->[$i]->{acc} ) {
            #print STDERR "Ignored due to allowed families B\n";
            next REGION;
          }
        }
      }

      if ($nestClans) {
        if ( $clans->{ $regions->[$j]->{acc} } ) {
          foreach my $relFam ( @{ $clan2fam->{ $clans->{ $regions->[$j]->{acc} } } } ) {
            next if ( $relFam eq $regions->[$j]->{acc} );
            next unless ( $familiesData->{$relFam}->{allowed} );
            foreach my $aFam ( @{ $familiesData->{$relFam}->{allowed} } ) {
              if ( $regions->[$i]->{acc} eq $aFam ) {
                #print STDERR "Ignored due to nesting\n";
                next REGION;
              }
            }
          }
        }

        if ( $clans->{ $regions->[$i]->{acc} } ) {
          foreach my $relFam ( @{ $clan2fam->{ $clans->{ $regions->[$i]->{acc} } } } ) {
            next if ( $relFam eq $regions->[$j]->{acc} );
            next unless ( $familiesData->{$relFam}->{allowed} );
            foreach my $aFam ( @{ $familiesData->{$relFam}->{allowed} } ) {
              if ( $regions->[$j]->{acc} eq $aFam ) {
                #print STDERR "Ignored due to nesting\n";
                next REGION;
              }
            }
          }
        }
      }


      if ( $regions->[$i]->{start} <= $regions->[$j]->{start} && $regions->[$i]->{end} >= $regions->[$j]->{start} ) {

        my $string
        = "(1) In "
        . $regions->[$j]->{fam} . " "
        . $regions->[$j]->{acc} . " "
        . $regions->[$j]->{ali} . " " . ": "
        . $protein . "/"
        . $regions->[$j]->{start} . "-"
        . $regions->[$j]->{end} . " ("
        . $regions->[$j]->{score}
        . " bits) overlaps with "
        . $regions->[$i]->{fam} . " "
        . $regions->[$i]->{acc} . " "
        . $regions->[$i]->{ali} . " "
        . $protein . "/"
        . $regions->[$i]->{start} . "-"
        . $regions->[$i]->{end} . " ("
        . $regions->[$i]->{score}
        . " bits)\n";

        #print $string;
        print OVERLAPS $string;
        $overlaps->{ $regions->[$i]->{acc} }->{ $regions->[$j]->{acc} . ":" . $regions->[$j]->{fam} }++;
        $overlaps->{ $regions->[$j]->{acc} }->{ $regions->[$i]->{acc} . ":" . $regions->[$i]->{fam} }++;

      }
      elsif (    $regions->[$i]->{start} <= $regions->[$j]->{end}
        && $regions->[$i]->{end} >= $regions->[$j]->{end} )
      {

        my $string
        = "(2) In "
        . $regions->[$j]->{fam} . " "
        . $regions->[$j]->{acc} . " "
        . $regions->[$j]->{ali} . " " . ": "
        . $protein . "/"
        . $regions->[$j]->{start} . "-"
        . $regions->[$j]->{end} . " ("
        . $regions->[$j]->{score}
        . " bits) overlaps with "
        . $regions->[$i]->{fam} . " "
        . $regions->[$i]->{acc} . " "
        . $regions->[$i]->{ali} . " "
        . $protein . "/"
        . $regions->[$i]->{start} . "-"
        . $regions->[$i]->{end} . " ("
        . $regions->[$i]->{score}
        . " bits)\n";

        #print $string;
        print OVERLAPS $string;
        $overlaps->{ $regions->[$i]->{acc} }->{ $regions->[$j]->{acc} . ":" . $regions->[$j]->{fam} }++;
        $overlaps->{ $regions->[$j]->{acc} }->{ $regions->[$i]->{acc} . ":" . $regions->[$i]->{fam} }++;

      }
      elsif (    $regions->[$i]->{start} >= $regions->[$j]->{start}
        && $regions->[$i]->{end} <= $regions->[$j]->{end} )
      {

        my $string
        = "(3) In "
        . $regions->[$j]->{fam} . " "
        . $regions->[$j]->{acc} . " "
        . $regions->[$j]->{ali} . " " . ": "
        . $protein . "/"
        . $regions->[$j]->{start} . "-"
        . $regions->[$j]->{end} . " ("
        . $regions->[$j]->{score}
        . " bits) overlaps with "
        . $regions->[$i]->{fam} . " "
        . $regions->[$i]->{acc} . " "
        . $regions->[$i]->{ali} . " "
        . $protein . "/"
        . $regions->[$i]->{start} . "-"
        . $regions->[$i]->{end} . " ("
        . $regions->[$i]->{score}
        . " bits)\n";

        print OVERLAPS $string;
        $overlaps->{ $regions->[$i]->{acc} }->{ $regions->[$j]->{acc} . ":" . $regions->[$j]->{fam} }++;
        $overlaps->{ $regions->[$j]->{acc} }->{ $regions->[$i]->{acc} . ":" . $regions->[$i]->{fam} }++;

      }
    }
  }
}

sub filterOverlaps
{
  print STDERR "Filtering overlaps\n";

  my ( $lengthLimit, $numberLimit ) = @_;

# Filter the overlaps according to the auto-resolve paramaters found inside the config file
  #
# Calculate for each family the number of overlaps whose length is less than 20% of the lowest scoring matching region length

  my ( $familyA,     $familyB,     $regionA,           $regionB,       $scoreA,
    $scoreB,      $lengthA,     $lengthB,           $overlapLength, $temp_length,
    $overlapPerc, $temp_family, %resolvedPerFamily, $familySize,    $numberPerc,
  );

  open( OVERLAPS, "$statusdir/$filePrefix.overlaps" ) || die "Could not open overlap file: $!";

  while (<OVERLAPS>)
  {
    if ((  $_
        =~ /(PF\d{5}).*:\s(\S+)\.\d*\/(\d+)-(\d+)\s\((\S+)\sbits\).*(PF\d{5}).*\s(\S+)\.\d*\/(\d+)-(\d+)\s\((\S+)\sbits\)/
      )
        and ( $_ !~ /SEED.*SEED/ )
    )
    {
      $familyA = $1;
      $regionA = new Bio::Range( -start => $3, -end => $4, -strand => +1 );
      $scoreA  = $5 ne "**" ? $5 : 100000;
      $lengthA = $regionA->length;

      $familyB = $6;
      $regionB = new Bio::Range( -start => $8, -end => $9, -strand => +1 );
      $scoreB  = $10 ne "**" ? $10 : 100000;
      $lengthB = $regionB->length;

      my ($s, $e, $d) = $regionA->intersection($regionB);
      $overlapLength = ($e - $s) + 1;

      $temp_length = $scoreA >= $scoreB ? $lengthB : $lengthA;
      $temp_family = $scoreA >= $scoreB ? $familyB : $familyA;

      $overlapPerc = sprintf( "%.2f", $overlapLength / $temp_length * 100 );

      if ( $overlapPerc < $lengthLimit )
      {
        $resolvedPerFamily{$temp_family}++;
      }

    }
  }

  close OVERLAPS;

  # Re-read the same file and print the overlaps that satisfy our restrictions in a new output file.

  open( OVERLAPS, "$statusdir/$filePrefix.overlaps" ) || die "Could not open overlap file: $!";
  open( OF, ">$statusdir/$filePrefix.overlaps.filtered" )
  || die "Could not write the overlaps.filtered file: $!";

  while (<OVERLAPS>)
  {
    if ((  $_
        =~ /(PF\d{5}).*:\s(\S+)\.\d*\/(\d+)-(\d+)\s\((\S+)\sbits\).*(PF\d{5}).*\s(\S+)\.\d*\/(\d+)-(\d+)\s\((\S+)\sbits\)/
      )
        and ( $_ !~ /SEED.*SEED/ )
    )
    {
      $familyA = $1;
      $regionA = new Bio::Range( -start => $3, -end => $4, -strand => +1 );
      $scoreA  = $5 ne "**" ? $5 : 100000;
      $lengthA = $regionA->length;

      $familyB = $6;
      $regionB = new Bio::Range( -start => $8, -end => $9, -strand => +1 );
      $scoreB  = $10 ne "**" ? $10 : 100000;
      $lengthB = $regionB->length;

      #$overlapLength = $regionA->intersection($regionB)->length;
      my ($s, $e, $d) = $regionA->intersection($regionB);
      $overlapLength = ($e - $s) + 1;

      $temp_length = $scoreA >= $scoreB ? $lengthB : $lengthA;
      $temp_family = $scoreA >= $scoreB ? $familyB : $familyA;

      $overlapPerc = sprintf( "%.2f", $overlapLength / $temp_length * 100 );

      $familySize = $familiesData->{$temp_family}->{align};

      if ( $resolvedPerFamily{$temp_family} )
      {
        $numberPerc = sprintf( "%.4f", $resolvedPerFamily{$temp_family} / $familySize * 100 );

        unless ( $overlapPerc < $lengthLimit and $numberPerc < $numberLimit )
        {
          # print OF "RESOLVE:\t$overlapPerc\t$numberPerc\t" . $_;

          # print OUT "$overlapPerc\t$numberPerc\n";
          print OF $_;
        }
      }
      else
      {
        print OF $_;
      }
    }
    else
    {
      print OF $_;    # This should print the SEED .. SEED overlaps
    }
  }

  close OVERLAPS;
  close OF;

  # Write out family summary for all filtered overlaps
  my ( %overlapsPerFamily, %familiesPerFamily, @temp, $temp_string );

  open( OF, "$statusdir/$filePrefix.overlaps.filtered" );
  while (<OF>)
  {
    if (/(PF\d{5}).*:\s(\w+)\.?\d*\/(\d+)-(\d+)\s\((\S+)\sbits\).*(PF\d{5}).*\s(\w+)\.?\d*\/(\d+)-(\d+)\s\((\S+)\sbits\)/
    )
    {
      $familyA = $1;
      $familyB = $6;

      $overlapsPerFamily{$familyA}++;
      $overlapsPerFamily{$familyB}++;

      if ( !$familiesPerFamily{$familyA} )
      {
        $familiesPerFamily{$familyA} .= $familyB . ";";
      }
      elsif ( index( $familiesPerFamily{$familyA}, $familyB ) == -1 )
      {
        $familiesPerFamily{$familyA} .= $familyB . ";";
      }

      if ( !$familiesPerFamily{$familyB} )
      {
        $familiesPerFamily{$familyB} .= $familyA . ";";
      }
      elsif ( index( $familiesPerFamily{$familyB}, $familyA ) == -1 )
      {
        $familiesPerFamily{$familyB} .= $familyA . ";";
      }
    }
  }

  close OF;

  open( OFS, ">$statusdir/$filePrefix.familyOverlaps.filtered" );
  printf( OFS "%-7s\t%-20s\t%-8s\t%-8s\t%-8s\t%s\n", "acc", "id", "NoSeed", "NoFull", "NoOverlaps", "OverlapFams" );

  foreach my $family ( keys %overlapsPerFamily )
  {
    undef @temp;
    undef $temp_string;
    @temp = split( /;/, $familiesPerFamily{$family} );

    foreach my $temp (@temp)
    {
      $temp_string .= $temp . ":" . $familiesData->{$temp}->{id} . ",";
    }

    printf( OFS "%-7s\t%-20s\t%-8s\t%-8s\t%-8s\t%s\n",
      $family,
      $familiesData->{$family}->{id},
      $familiesData->{$family}->{seed},
      $familiesData->{$family}->{align},
      $overlapsPerFamily{$family}, $temp_string );
  }

  close OFS;

}

