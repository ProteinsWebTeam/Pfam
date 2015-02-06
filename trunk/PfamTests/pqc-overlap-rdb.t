#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
use Bio::Pfam::Config;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;
use File::Basename;
use Cwd 'abs_path';

BEGIN
{
    use_ok('PfamLive') || print "Failed to load Bio::Pfam::PfamLive!\n";
    use_ok('Bio::Pfam::PfamQC') || print "Failed to load Bio::Pfam::PfamQC!\n";
}

# ------------------------------------------------------------------------------
# initial setup things
# ------------------------------------------------------------------------------
my $familyDir = "./data/";

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

my $familyIO = Bio::Pfam::FamilyIO->new;
my $family   = "PF01740";
my $famObj   = $familyIO->loadPfamAFromLocalFile( $family, $familyDir );

my ( %ignore, $compete, $all, $noFilter );
my $lengthLimit = 20;
my $numberLimit = 1;

# ------------------------------------------------------------------------------
# create test database
# ------------------------------------------------------------------------------
my $file = basename($0);
my $path = abs_path($0);

my $dataDir = undef;
( $dataDir = $path ) =~ s|$file|data/|;

my $db_file = undef;
( $db_file = $path ) =~ s|$file|data/sqlite/pfamTest.db|;
unlink $db_file;

diag("Deploying schema");
my $schema = PfamLive->connect("dbi:SQLite:$db_file");
isa_ok( $schema, 'PfamLive', 'Check that we have a DBIx::Class object' );

my $dbh = $schema->storage->dbh;
$schema->deploy;

my $sth = $dbh->prepare(
    "INSERT INTO pfamA_reg_full_significant
    ('auto_pfama_reg_full','pfamA_acc','pfamseq_acc','seq_start','seq_end','ali_start','ali_end','model_start','model_end','domain_bits_score','domain_evalue_score','sequence_bits_score','sequence_evalue_score','in_full')
    VALUES
    (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
                       );

$sth->execute( '1',  'PF11380', 'F5H104', '153',  '258',  '153',  '258',  '153',  '258',  '150', '0', '0', '0', '0' );
$sth->execute( '2',  'PF11380', 'B7Z266', '240',  '300',  '240',  '300',  '240',  '300',  '150', '0', '0', '0', '0' );
$sth->execute( '3',  'PF11380', 'B7Z575', '150',  '250',  '150',  '250',  '150',  '250',  '150', '0', '0', '0', '0' );
$sth->execute( '4',  'PF11380', 'B7Z6M6', '200',  '300',  '200',  '300',  '200',  '300',  '150', '0', '0', '0', '0' );
$sth->execute( '5',  'PF11380', 'G7P2F9', '500',  '600',  '500',  '600',  '500',  '600',  '150', '0', '0', '0', '0' );
$sth->execute( '6',  'PF11380', 'F6T116', '500',  '600',  '500',  '600',  '500',  '600',  '150', '0', '0', '0', '0' );
$sth->execute( '7',  'PF11380', 'G1RBK1', '500',  '600',  '500',  '600',  '500',  '600',  '150', '0', '0', '0', '0' );
$sth->execute( '8',  'PF11380', 'H2QV81', '100',  '200',  '100',  '200',  '100',  '200',  '150', '0', '0', '0', '0' );
$sth->execute( '9',  'PF11380', 'G3SKH5', '500',  '700',  '500',  '700',  '500',  '700',  '150', '0', '0', '0', '0' );
$sth->execute( '10', 'PF11380', 'F6WCS4', '200',  '300',  '200',  '300',  '200',  '300',  '150', '0', '0', '0', '0' );
$sth->execute( '11', 'PF11380', 'O43511', '180',  '300',  '180',  '300',  '180',  '300',  '150', '0', '0', '0', '0' );
$sth->execute( '12', 'PF11380', 'F7ASI9', '200',  '220',  '200',  '220',  '200',  '220',  '150', '0', '0', '0', '0' );
$sth->execute( '13', 'PF11380', 'F7ECS0', '200',  '250',  '200',  '250',  '200',  '250',  '150', '0', '0', '0', '0' );
$sth->execute( '14', 'PF11380', 'H0WVJ7', '380',  '420',  '380',  '420',  '380',  '420',  '150', '0', '0', '0', '0' );
$sth->execute( '15', 'PF11380', 'Q5NVA6', '200',  '350',  '200',  '350',  '200',  '350',  '150', '0', '0', '0', '0' );
$sth->execute( '16', 'PF11380', 'H2PN67', '1000', '1100', '1000', '1100', '1000', '1100', '150', '0', '0', '0', '0' );

# ------------------------------------------------------------------------------
# test family_overlaps_with_db subroutine
# ------------------------------------------------------------------------------
# my $res = Bio::Pfam::PfamQC::family_overlaps_with_db( "PF01740", \%ignore, undef, $pfamDB, $famObj, $compete, $all, $noFilter );
# print "RES: $res\n";

# ------------------------------------------------------------------------------
# test filterOverlaps subroutine
# ------------------------------------------------------------------------------

# 5 long overlaps, all should remain
my @overlapLines = ( "Sequence [F5H104] overlap M-factor PF01740/97-286 FULL with DUF3184 PF11380/153-258 FULL\n",
                     "Sequence [B7Z266] overlap M-factor PF01740/105-294 FULL with DUF3184 PF11380/240-300 FULL\n",
                     "Sequence [B7Z575] overlap M-factor PF01740/97-286 FULL with DUF3184 PF11380/150-250 FULL\n",
                     "Sequence [B7Z6M6] overlap M-factor PF01740/123-312 FULL with DUF3184 PF11380/200-300 FULL\n",
                     "Sequence [G7P2F9] overlap M-factor PF01740/536-725 FULL with DUF3184 PF11380/500-600 FULL\n",
                   );

is( Bio::Pfam::PfamQC::filterOverlaps( $family, $famObj, \@overlapLines, $dbh ),
    5, "filterOverlaps long overlaps test" );

# 5 short overlaps, all should be allowed
@overlapLines = ( "Sequence [F6T116] overlap M-factor PF01740/590-725 FULL with DUF3184 PF11380/500-600 FULL\n",
                  "Sequence [G1RBK1] overlap M-factor PF01740/380-510 FULL with DUF3184 PF11380/500-600 FULL\n",
                  "Sequence [H2QV81] overlap M-factor PF01740/150-500 FULL with DUF3184 PF11380/100-200 FULL\n",
                  "Sequence [G3SKH5] overlap M-factor PF01740/690-800 FULL with DUF3184 PF11380/500-700 FULL\n",
                  "Sequence [F6WCS4] overlap M-factor PF01740/50-205 FULL with DUF3184 PF11380/200-300 FULL\n",
                );

is( Bio::Pfam::PfamQC::filterOverlaps( $family, $famObj, \@overlapLines, $dbh ),
    0, "filterOverlaps short overlaps test" );

# 11 short overlaps, all should remain, more than 1% of family size
@overlapLines = (
    "Sequence [F6T116] overlap M-factor PF01740/590-725 FULL with DUF3184 PF11380/500-600 FULL\n",
    "Sequence [G1RBK1] overlap M-factor PF01740/380-510 FULL with DUF3184 PF11380/500-600 FULL\n",
    "Sequence [H2QV81] overlap M-factor PF01740/150-500 FULL with DUF3184 PF11380/100-200 FULL\n",
    "Sequence [G3SKH5] overlap M-factor PF01740/690-800 FULL with DUF3184 PF11380/500-700 FULL\n",
    "Sequence [F6WCS4] overlap M-factor PF01740/50-205 FULL with DUF3184 PF11380/200-300 FULL\n",
    "Sequence [O43511] overlap M-factor PF01740/10-200 FULL with DUF3184 PF11380/180-300 FULL\n",
    "Sequence [F7ASI9] overlap M-factor PF01740/50-500 FULL with DUF3184 PF11380/200-220 FULL\n",
    "Sequence [F7ECS0] overlap M-factor PF01740/100-800 FULL with DUF3184 PF11380/200-250 FULL\n",
    "Sequence [H0WVJ7] overlap M-factor PF01740/400-1000 FULL with DUF3184 PF11380/380-420 FULL\n",
    "Sequence [Q5NVA6] overlap M-factor PF01740/300-900 FULL with DUF3184 PF11380/200-350 FULL\n",
    "Sequence [H2PN67] overlap M-factor PF01740/500-1100 FULL with DUF3184 PF11380/1000-1100 FULL\n",

                );

is( Bio::Pfam::PfamQC::filterOverlaps( $family, $famObj, \@overlapLines, $dbh ),
    11, "filterOverlaps short overlaps, more than 1% test" );

# ------------------------------------------------------------------------------
done_testing();
