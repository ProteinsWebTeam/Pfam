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

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Cwd;
use Data::Dumper;
use Date::Object;
use Getopt::Long;
use Storable;

use Bio::Pfam::FamilyIO;
use Bio::Pfam::Config;

#Start up the logger
my $logger = get_logger();

#-------------------------------------------------------------------------------
# Get the Pfam Config and check all is well.
  my $config = Bio::Pfam::Config->new;

  unless ($config) {
    die
"Failed to obtain a Pfam Config object, check that the environment variable PFAM_CONFIG is set and the file is there!\n";
  }
  unless ( $config->location eq 'WTSI' or $config->location eq 'JFRC' or $config->location eq 'EBI' ) {
    warn "Unkown location.....things will probably break\n";
  }
  unless ( -d $config->hmmer3bin ) {
    die "Could not find the HMMER3 bin directory," . $config->hmmer3bin . "\n";
  }

#-------------------------------------------------------------------------------

my $no_compete;    # Use this option to not do the clan competition step
my ( $statusdir, $clans, $clan2fam, $fast, $oldOverlaps, $families, $ciFamilies, $all );

GetOptions( 'fast'        => \$fast,
            'previous'    => \$oldOverlaps,
            'no_compete!' => \$no_compete,
            'statusdir=s' => \$statusdir,
            'datadir=s'   => \$families,
            'checked=s'   => \$ciFamilies,
            'all'         => \$all
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
my $date       = new Date::Object( time() );
my $filePrefix = $date->year . $date->month . $date->day . "overlaps";
my $nestClans  = 1;

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

#-------------------------------------------------------------------------------

sub getRegions
{
    my ( $families, $posOverlaps, $familiesData ) = @_;

    # if refprot flag is used, then load all reference proteomes sequence accessions
    my %refprotAccs;    # hash for storing the version of each sequence accession
    unless ($all)
    {
        my $refprotFile = $config->refprotLoc . "/refprot";

        open( REFPROT, $refprotFile) or die("can not open file $refprotFile, $!");

        print "Reading reference proteomes sequence accessions...\n";

        while (<REFPROT>)
        {
            if ( $_ =~ /^>(\S+\.\S+)/ )
            {
                $refprotAccs{$1} = 1;
            }
        }

        close REFPROT;
    }

    open( R, ">$statusdir/$filePrefix.allRegions.txt" )
      or die "Could not open $statusdir/$filePrefix.allRegions.txt\n";

    foreach my $fDir ( @{$posOverlaps} )
    {
        my $id = $familiesData->{$fDir}->{id};
        open( S, "$families/$fDir/SEED" );
        while (<S>)
        {
            if (/(\S+)\/(\d+)\-(\d+)/)
            {
                if (!$all)
                {
                    if ( $refprotAccs{$1} )
                    {
                        $familiesData->{$fDir}->{seed}++;
                        print R "$1\t$2\t$3\t$fDir\t$id\tSEED\t**\n";
                    }
                }
                else
                {
                    $familiesData->{$fDir}->{seed}++;
                    print R "$1\t$2\t$3\t$fDir\t$id\tSEED\t**\n";
                }
            }
        }
        close(S);

        open( S, "$families/$fDir/scores" )
          or warn "Could not open scores file $fDir\n";
        while (<S>)
        {
            if (/(\S+)\s+(\S+)\/(\d+\-\d+)\s+(\d+)\-(\d+)/)
            {
                if (!$all)
                {
                    if ( $refprotAccs{$2} )
                    {
                        print R "$2\t$4\t$5\t$fDir\t$id\tALIGN\t$1\n";
                        $familiesData->{$fDir}->{align}++;
                    }
                }
                else
                {
                    print R "$2\t$4\t$5\t$fDir\t$id\tALIGN\t$1\n";
                    $familiesData->{$fDir}->{align}++;
                }
            }
        }
        close(S);
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
    system("sort -k1,1 -k7,7nr $statusdir/$filePrefix.allRegions.txt > $statusdir/$filePrefix.allRegionsSorted.txt")
      and $logger->logdie("Failed to sort regions.");
    print STDERR "Finished sorting, looking for overlaps\n";

    open( S, "$statusdir/$filePrefix.allRegionsSorted.txt" )
      or die "Could not open allRegionsSorted.txt:[$!]\n";

    my $previousAcc = '';
    my $regions;
    my %overlaps;

    open( OVERLAPS, ">$statusdir/$filePrefix.overlaps" )
      || die "Could not open overlap file: $!";

    while (<S>)
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
            { fam   => $line[4],
              acc   => $line[3],
              start => $line[1],
              end   => $line[2],
              score => $line[6],
              ali   => $line[5],
              skip  => 0           # Identify region that get outcompeted
            }
        );
        $previousAcc = $line[0];
    }
    checkRegions( $previousAcc, $regions, $clans, \%overlaps );

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

sub checkRegions
{
    my ( $protein, $regions, $clans, $overlaps ) = @_;

    # Do all competition comparisons first to set all the skip flags
    # This means if there are two overlapping regions that are to families in the same clan
    # only the highest scoring match will be kept
    if ( !$no_compete )
    {
        for ( my $i = 0; $i <= $#{$regions}; $i++ )
        {
            for ( my $j = $i + 1; $j <= $#{$regions}; $j++ )
            {
                if (     $clans->{ $regions->[$i]->{acc} }
                     and $clans->{ $regions->[$j]->{acc} } )
                {
                    if ( $clans->{ $regions->[$i]->{acc} } eq $clans->{ $regions->[$j]->{acc} } )
                    {
                        my $remove = 0;

                        # Short circuit tests to try and speed up
                        #if ($regions->[$i]->{end} < $regions->[$j]->{start}){next;}
                        #if ($regions->[$i]->{start} > $regions->[$j]->{end}){next;}

                        #print STDERR "Both regions are in the same clan\n";
                        # I should delete the lowest scoring one if they overlap!
                        if (    $regions->[$i]->{start} <= $regions->[$j]->{start}
                             && $regions->[$i]->{end} >= $regions->[$j]->{start} )
                        {
                            $remove = 1;
                        }

                        elsif (    $regions->[$i]->{start} <= $regions->[$j]->{end}
                                && $regions->[$i]->{end} >= $regions->[$j]->{end} )
                        {
                            $remove = 1;
                        }

                        elsif (    $regions->[$i]->{start} >= $regions->[$j]->{start}
                                && $regions->[$i]->{end} <= $regions->[$j]->{end} )
                        {
                            $remove = 1;
                        }

                        if ($remove)
                        {
                            my $score_i = $regions->[$i]->{score};
                            my $score_j = $regions->[$j]->{score};

                            if ( $score_i eq '**' or $score_j eq '**' )
                            {

                                # Ignore removal one is a SEED sequence
                            }
                            elsif ( $score_i < $score_j )
                            {

                        #print "I $score_i lt J $score_j Making $regions->[$i]->{acc} $protein/$regions->[$i]->{start}-$regions->[$i]->{end} skip!\n";
                                $regions->[$i]->{skip} = 1;
                            }
                            else
                            {

                        #print "I $score_i gt J $score_j Making $regions->[$j]->{acc} $protein/$regions->[$j]->{start}-$regions->[$j]->{end} skip!\n";
                                $regions->[$j]->{skip} = 1;
                            }
                        }
                    }
                }
            }
        }
    }

    # OK now do the real overlap checking section.
    for ( my $i = 0; $i <= $#{$regions}; $i++ )
    {
        if ( $regions->[$i]->{skip} )
        {
            next;
        }
      REGION:
        for ( my $j = $i + 1; $j <= $#{$regions}; $j++ )
        {

            # Ignore if regions are in same family
            if ( $regions->[$i]->{fam} eq $regions->[$j]->{fam} ) { next REGION; }

            if ( $regions->[$j]->{skip} )
            {

                #print "Skipping because region was already outcompeted!\n";
                next REGION;
            }

            if (     $clans->{ $regions->[$i]->{acc} }
                 and $clans->{ $regions->[$j]->{acc} } )
            {
                if ( $clans->{ $regions->[$i]->{acc} } eq $clans->{ $regions->[$j]->{acc} } )
                {
                    next REGION;
                }
            }

            # Possibly don't need both of these next two tests! Although they don't both get run!

            # Ignore if in list of allowed families
            if ( $familiesData->{ $regions->[$i]->{acc} }->{allowed} )
            {
                foreach my $aFam ( @{ $familiesData->{ $regions->[$i]->{acc} }->{allowed} } )
                {
                    if ( $aFam eq $regions->[$j]->{acc} )
                    {

                        #print  "Ignored due to allowed nested families A $protein\n";
                        #print "$regions->[$i]->{acc} with $regions->[$j]->{acc}\n";
                        next REGION;
                    }
                }
            }

            # Ignore if in list of allowed families
            if ( $familiesData->{ $regions->[$j]->{acc} }->{allowed} )
            {
                foreach my $aFam ( @{ $familiesData->{ $regions->[$j]->{acc} }->{allowed} } )
                {
                    if ( $aFam eq $regions->[$i]->{acc} )
                    {

                        #print STDERR "Ignored due to allowed families B\n";
                        next REGION;
                    }
                }
            }

            if ($nestClans)
            {
                if ( $clans->{ $regions->[$j]->{acc} } )
                {
                    foreach my $relFam ( @{ $clan2fam->{ $clans->{ $regions->[$j]->{acc} } } } )
                    {
                        next if ( $relFam eq $regions->[$j]->{acc} );
                        next unless ( $familiesData->{$relFam}->{allowed} );
                        foreach my $aFam ( @{ $familiesData->{$relFam}->{allowed} } )
                        {
                            if ( $regions->[$i]->{acc} eq $aFam )
                            {

                                #print STDERR "Ignored due to nesting\n";
                                next REGION;
                            }
                        }
                    }
                }

                if ( $clans->{ $regions->[$i]->{acc} } )
                {
                    foreach my $relFam ( @{ $clan2fam->{ $clans->{ $regions->[$i]->{acc} } } } )
                    {
                        next if ( $relFam eq $regions->[$j]->{acc} );
                        next unless ( $familiesData->{$relFam}->{allowed} );
                        foreach my $aFam ( @{ $familiesData->{$relFam}->{allowed} } )
                        {
                            if ( $regions->[$j]->{acc} eq $aFam )
                            {
                                #print STDERR "Ignored due to nesting\n";
                                next REGION;
                            }
                        }
                    }
                }
            }

            # Sigh. I'm not sure why we need this. I thought these should all be skipped earlier :(
            if ( $regions->[$i]->{skip} )
            {
                next REGION;
            }
            if ( $regions->[$j]->{skip} )
            {
                next REGION;
            }

            # print Dumper($regions->[$i]);
            # print Dumper($regions->[$j]);
            if (    $regions->[$i]->{start} <= $regions->[$j]->{start}
                 && $regions->[$i]->{end} >= $regions->[$j]->{start} )
            {

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

                #}

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

                #print $string;
                print OVERLAPS $string;
                $overlaps->{ $regions->[$i]->{acc} }->{ $regions->[$j]->{acc} . ":" . $regions->[$j]->{fam} }++;
                $overlaps->{ $regions->[$j]->{acc} }->{ $regions->[$i]->{acc} . ":" . $regions->[$i]->{fam} }++;
            }
        }
    }
}

