#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use DBI;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Range;
use Getopt::Long;
use Log::Log4perl qw(:easy);

# lengthLimit (default 20):	filter out overlaps whose length is less than 20% of the lowest scoring matching region length
#							as long as
# numberLimit (default 1):	the total number of filtered overlaps is less than 1% of the total number
my ( $statusdir, $lengthLimit, $numberLimit, $latestOverlapFile, $filename );

#-------------------------------------------------------------------------------
# Start up the logger
my $logger = get_logger();

#-------------------------------------------------------------------------------
# Deal with options
GetOptions( 'statusdir=s' => \$statusdir, 'lengthLimit=i' => \$lengthLimit, 'numberLimit=i' => \$numberLimit )
  or $logger->logdie("Invalid options passed in!\n");

unless ($lengthLimit)
{
    $lengthLimit = 20;    # set default value
}

unless ($numberLimit)
{
    $numberLimit = 1;    # set default value
}

#-------------------------------------------------------------------------------
# Get the Pfam Config
my $config = Bio::Pfam::Config->new;

#-------------------------------------------------------------------------------
# Connecting to pfam_live database
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

# Prepare the query
my $getFamilySizeQuery = "SELECT num_full FROM pfamA WHERE pfamA_acc=?";
my $getFamilySizeHandler = $dbh->prepare($getFamilySizeQuery) or die "cannot execute: " . $dbh->errstr();

#-------------------------------------------------------------------------------
# Getting latest output file produced by pud-findOverlapsAndMissing.pfam_live

my @overlapFiles = glob( $statusdir . "/*overlaps.overlaps" );

foreach my $file ( sort { $a cmp $b } @overlapFiles )
{
    $latestOverlapFile = $file;
}

if ( $latestOverlapFile =~ /\/(\d+overlaps\.overlaps$)/ )
{
    $filename = $1;
}

print "Most recent file found inside statusdir is $filename\n";

#-------------------------------------------------------------------------------
# Filter out overlaps according to the specified limit parameters
#
# Calculate for each family the number of overlaps whose length is less than 20% of the lowest scoring matching region length

my ( $familyA,     $familyB,     $regionA,           $regionB,       $scoreA,
     $scoreB,      $lengthA,     $lengthB,           $overlapLength, $temp_length,
     $overlapPerc, $temp_family, %resolvedPerFamily, $familySize,    $numberPerc,
   );

open( OVERLAPS, $latestOverlapFile ) or die("Can not open $latestOverlapFile\n");

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

        $overlapLength = $regionA->intersection($regionB)->length;

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

open( OVERLAPS, $latestOverlapFile ) or die("Can not open $latestOverlapFile\n");
open( OUT, ">$statusdir" . "/$filename.filtered" );

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

        $overlapLength = $regionA->intersection($regionB)->length;

        $temp_length = $scoreA >= $scoreB ? $lengthB : $lengthA;
        $temp_family = $scoreA >= $scoreB ? $familyB : $familyA;

        $overlapPerc = sprintf( "%.2f", $overlapLength / $temp_length * 100 );

        $getFamilySizeHandler->execute($temp_family) or die "Can't execute statement: $DBI::errstr";
        $familySize = $getFamilySizeHandler->fetchrow_array();

        if ( $resolvedPerFamily{$temp_family} )
        {
            $numberPerc = sprintf( "%.4f", $resolvedPerFamily{$temp_family} / $familySize * 100 );

            if ( $overlapPerc >= $lengthLimit or $numberPerc >= $numberLimit )
            {
                print OUT $_;

                # print OUT "$overlapPerc\t$numberPerc\n";
            }
        }
        else
        {
            print OUT $_;
        }
    }
    else
    {
        print OUT $_; # This should print the SEED .. SEED overlaps
    }
}

close OVERLAPS;
close OUT;
