#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use DateTime;
use Data::Printer;
use File::Path qw(make_path remove_tree);
use Cwd qw(abs_path);
use Config::General qw(SaveConfig);

#The aim of this script is to generate a mini-pfam infrasturcture, based on a few families.
#The following will be performed:
# 1) Defining the test set of families
# 2) Obtaining a copy of these families files and entering into a repository.
# 3) Extracting parts of the mysql database

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

#Get the user options
my ( @families, @clans, $number, $outdir, $help );

&GetOptions(
  "outdir=s"       => \$outdir,
  "family=s"       => \@families,
  "clan=s"         => \@clans,
  "num_fam=i"      => \$number,
  "h|help"         => \$help,
  )
  or $logger->logdie("Invalid option! Run $0 -help");

if($help){
  help();
}
$number = 10 unless ($number);

if(!$outdir){
  $logger->warn("No output directory specified");
  help();
}else{
  $outdir = abs_path($outdir);
}
#Now we have the clans and families defined by the user,
#get the list of families.

$logger->info("Connecting to the databases.");

#Get the connection to the pfam database.
my $config        = Bio::Pfam::Config->new;
my $connectParams = $config->pfamliveAdmin;
my $pfamDB        = Bio::Pfam::PfamLiveDBManager->new( %{$connectParams} );
my $dbh           = $pfamDB->getSchema->storage->dbh;

#Check each family is valid.
foreach my $f (@families) {
  my $dbFam =
    $pfamDB->getSchema->resultset('PfamA')->find( { pfama_acc => $f } );
  if ( $dbFam and $dbFam->pfama_acc eq $f ) {

    #Found entry....good
    $logger->debug("Adding $f");
  }
  else {
    $logger->logdie("Could not find entry $f\n");
  }
}

#check the clan is valid and get the membership
foreach my $c (@clans) {
  my @clanDbMemb =
    $pfamDB->getSchema->resultset('ClanMembership')
    ->search( { clan_acc => $c } );
  if ( $#clanDbMemb < 1 ) {
    $logger->debug("Could not find clan $c");
  }
  else {
    foreach my $f (@clanDbMemb) {
      $logger->debug( "Adding " . $f->pfama_acc->pfama_acc );
      push( @families, $f->pfama_acc->pfama_acc );
    }
  }
}

#Now randomly add families in, to make a up the number to the desired number of
#families
if ( $number and scalar(@families) < $number ) {
  my $extra       = $number - scalar(@families);
  my @allFamilies = $pfamDB->getSchema->resultset('PfamA')->search( {} );
  my $max         = scalar(@allFamilies);

  #Get a hash, keyed off accessions
  my %addedFamilies = map { $_ => 1 } @families;
  while ( scalar( @families < $number ) ) {
    my $family = $allFamilies[ rand($max) ];
    next if ( $family->num_full > 10000 );
    if ( !exists( $addedFamilies{ $family->pfama_acc } ) ) {
      push( @families, $family->pfama_acc );
      $addedFamilies{ $family->pfama_acc } = 1;
    }
  }
}

$logger->info( "Building pfam based on:" . join( ", ", @families ) );

#Now get an svn export of each family!
my $client = Bio::Pfam::SVN::Client->new;

#Temporary directory where we are going to put clean copies of families and clans
my $dest = "$outdir/mini_pfam$$/trunk";
foreach my $d (qw(Families Clans FamiliesPending ClansPending)) {
  make_path( $dest . '/' . $d );
}

#Now get out all of the families
foreach my $family (@families) {
  make_path( $dest . "/Families/$family/" );
  $client->exportFamily( $family, $dest . "/Families/$family/" );

  #Fix timestamps.....
  foreach my $file (qw(SEED HMM OUTPUT PFAMOUT scores ALIGN DESC)) {

    #Fudge the access time and modification times
    my ( $atime, $mtime );
    $atime = $mtime = time;
    utime $atime, $mtime, "$dest/Families/$family/$file";
    sleep(1);
  }
}

#Now get all the clans
foreach my $clan (@clans) {
  make_path( $dest . "/Clans/$clan/" );
  $client->exportClan( $clan, $dest . "/Clans/$clan/" );
}

#Now generate a SVN repository and check it out.
my $dt     = DateTime->now;
my $dbName = 'pfam_test_' . $dt->ymd("_");
make_path("$outdir/$dbName/repos");
make_path("$outdir/$dbName/checkout");
make_path("$outdir/$dbName/Conf");
system("svnadmin create $outdir/$dbName/repos")
  and die "Failed to create empty SVN repository\n";
system("svn import -q -m 'test db import' $dest file://$outdir/$dbName/repos");
system("svn co -q file://$outdir/$dbName/repos $outdir/$dbName/checkout");
remove_tree($dest);

# Now build a corresponding MySQL database!

my %newConnectParams = %{ $config->pfamliveAdmin };
my $newConnectParams = \%newConnectParams;

my $username = getpwuid( $< );
$newConnectParams->{database} = $dbName.'_'.$username;

$logger->info("Generating Schema.");

#dump an empty database.
system( "mysqldump -u "
    . $connectParams->{adminuser} . " -p"
    . $connectParams->{adminpassword} . " -h "
    . $connectParams->{host} . " -P"
    . $connectParams->{port} . " -d "
    . $connectParams->{database}
    . " > /tmp/schema.dmp" );

$dbh->do("drop database if exists $newConnectParams->{database}")
  or $dbh->errstr;
$dbh->do("create database $newConnectParams->{database}") or $dbh->errstr;
my $pfamDBNew = Bio::Pfam::PfamLiveDBManager->new( %{$newConnectParams} );
my $dbhNew    = $pfamDBNew->getSchema->storage->dbh;

system( "mysql -u "
    . $newConnectParams->{adminuser} . " -p"
    . $newConnectParams->{adminpassword} . " -h "
    . $newConnectParams->{host} . " -P"
    . $newConnectParams->{port} . " "
    . $newConnectParams->{database}
    . " < /tmp/schema.dmp" );

unlink("/tmp/schema.dmp");

#Construct a temporary table to so that we can join against.
$logger->info("Building temporary table.");
$dbhNew->do(
      "create temporary table _families ( pfamA_acc varchar(7) NOT NULL, "
    . "UNIQUE KEY _fam_pfamA_acc (pfamA_acc)) ENGINE=InnoDB" );
my $sth = $dbhNew->prepare("INSERT INTO _families (pfamA_acc) values (?)");
$logger->info( "Building pfam based on:" . join( ", ", @families ) );
for my $f (@families) {
  $sth->execute($f);
}
$sth->finish;

#We may want to add the nested domain data here, so that we can have complete pfamA info.

#Join against the temporary table
$logger->info("Inserting Pfam-A related data.");

$dbhNew->do( "insert into pfamA select t.* from "
    . $connectParams->{database}
    . ".pfamA t, _families f where t.pfamA_acc=f.pfamA_acc" );

#now go through all of the live pfamA related tables and do an insert
$dbhNew->do("SET FOREIGN_KEY_CHECKS = 0;");
foreach my $table (
  qw(pfamA_literature_reference pfamA_wiki _pfamA_internal pfamA_reg_seed pfamA_reg_full_significant pfamA_reg_full_insignificant
  clan_membership edits interpro gene_ontology nested_domains nested_locations current_pfam_version released_pfam_version pdb_pfamA_reg pfamA_HMM)
  )
{
  $dbhNew->do( "insert into $table select t.* from "
      . $connectParams->{database}
      . ".$table t, pfamA f where t.pfamA_acc=f.pfamA_acc" );
}
$dbhNew->do( "insert into wikipedia select distinct t.* from "
    . $connectParams->{database}
    . ".wikipedia t, pfamA_wiki f where t.auto_wiki=f.auto_wiki" );
$dbhNew->do( "insert into literature_reference select distinct t.* from "
    . $connectParams->{database}
    . ".literature_reference t, pfamA_literature_reference f where t.auto_lit=f.auto_lit"
);

$dbhNew->do( "insert into clan select distinct t.* from "
    . $connectParams->{database}
    . ".clan t, clan_membership f where t.clan_acc=f.clan_acc" );

$logger->info("Inserting Clan related data.");
foreach my $table (
  qw(clan_database_links clan_lit_ref clan_wiki released_clan_version))
{
  $dbhNew->do( "insert into $table select t.* from "
      . $connectParams->{database}
      . ".$table t, clan f where t.clan_acc=f.clan_acc" );
}
$dbhNew->do( "replace wikipedia select distinct t.* from "
    . $connectParams->{database}
    . ".wikipedia t, clan_wiki f where t.auto_wiki=f.auto_wiki" );
$dbhNew->do( "replace literature_reference select t.* from "
    . $connectParams->{database}
    . ".literature_reference t, clan_lit_ref f where t.auto_lit=f.auto_lit" );

$logger->info("Inserting pfamseq and pdb related data.");

#Use replace to get round duplicates...
my $seqSth =
  $dbhNew->prepare( "replace pfamseq select * from "
    . $connectParams->{database}
    . ".pfamseq where pfamseq_acc=?" );
my $pdbSth =
  $dbhNew->prepare( "replace pdb_residue_data select * from "
    . $connectParams->{database}
    . ".pdb_residue_data where pfamseq_acc=?" );
my $othSth =
  $dbhNew->prepare( "replace other_reg select * from "
    . $connectParams->{database}
    . ".other_reg where pfamseq_acc=?" );
my $disSth =
  $dbhNew->prepare( "replace pfamseq_disulphide select * from "
    . $connectParams->{database}
    . ".pfamseq_disulphide where pfamseq_acc=?" );
my $marSth =
  $dbhNew->prepare( "replace pfamseq_markup select * from "
    . $connectParams->{database}
    . ".pfamseq_markup where pfamseq_acc=?" );

####proteomes 
$logger->info("Inserting proteome related data.");
my $proseqSth = 
    $dbhNew->prepare("replace proteome_pfamseq select * from "
     . $connectParams->{database}   
    . ".proteome_pfamseq where pfamseq_acc=?" );
my $proregSth = 
    $dbhNew->prepare("replace proteome_regions select * from "
     . $connectParams->{database}   
    . ".proteome_regions where pfamseq_acc=?" );

my $comproSth =
    $dbhNew->prepare("replace complete_proteomes select c.* from "
     . $connectParams->{database}   
    . ".complete_proteomes c, proteome_pfamseq p where c.auto_proteome = p.auto_proteome and p.pfamseq_acc=?" );


my $commitPoint = 10000;
my $count = 0;
#This probably would benefit from auto_commit being turn off.
$dbhNew->begin_work;
foreach my $table (
  qw(pfamA_reg_seed pfamA_reg_full_significant pfamA_reg_full_insignificant edits)
  )
{
   print STDERR "select distinct o.pfamseq_acc from ".$connectParams->{database}.".$table o, pfamA a where a.pfamA_acc=o.pfamA_acc\n";
  my $autoSeq = $dbhNew->prepare("select distinct o.pfamseq_acc from ".$connectParams->{database}.".$table o, pfamA a where a.pfamA_acc=o.pfamA_acc");
  $autoSeq->execute;
  my $data = $autoSeq->fetchall_arrayref;
  foreach my $d (@$data) {
    $seqSth->execute( $d->[0] );
    $pdbSth->execute( $d->[0] );
    $othSth->execute( $d->[0] );
    $disSth->execute( $d->[0] );
    $marSth->execute( $d->[0] );
    $proseqSth->execute( $d->[0] );
    $proregSth->execute( $d->[0] );
    $comproSth->execute( $d->[0] );

  }
  $autoSeq->finish;
  $count++;
  
  if ( $count > $commitPoint ) {
    $dbhNew->commit;
    $dbhNew->begin_work;
    $commitPoint += 10000;
  }
}
$seqSth->finish;
$dbhNew->commit;

$logger->info("Inserting taxonomy and more pdb related data.");
$dbhNew->do( "insert into ncbi_taxonomy select distinct t.* from "
    . $connectParams->{database}
    . ".ncbi_taxonomy t, pfamseq s where s.ncbi_taxid=t.ncbi_taxid" );

$dbhNew->do( "insert into pdb select distinct t.* from "
    . $connectParams->{database}
    . ".pdb t, pdb_residue_data r where r.pdb_id=t.pdb_id" );
    
foreach my $table (qw(pdb_image)) {
  $dbhNew->do( "insert into $table select t.* from "
      . $connectParams->{database}
      . ".$table t, pdb f where t.pdb_id=f.pdb_id" );
}

$dbhNew->do("SET FOREIGN_KEY_CHECKS = 1;");

foreach
  my $t (qw(taxonomy evidence dead_clan dead_family markup_key version))
{
  $dbhNew->do(
    "insert into $t select * from " . $connectParams->{database} . ".$t" );
}



#Now we have the database and the repository, we need to put the hooks in place
#read in and then write out with new mysql and svn parameters.
my ($conf) = $ENV{PFAM_CONFIG} =~ m/([\d\w\/\-\.]+)/;
my $c      = new Config::General($conf);
my %ac     = $c->getall;

#Find the path of this script and modify to the svnhooks
$ac{Model}->{Pfamlive}->{database} = $newConnectParams->{database};
$ac{svnRepos} = "file://$outdir/$dbName/repos/";
$ac{svnFamilies} = "Families";
$ac{svnClans}       =  "Clans";
$ac{svnSequence}    =  "Sequences";
$ac{svnNewClans}    =  "ClansPending";
$ac{svnNewFamilies} =  "FamiliesPending";

SaveConfig("$outdir/$dbName/Conf/$dbName.config", \%ac);


my @bits = split('/', abs_path($0));
my $script = join('/', @bits[0..($#bits-2)], 'svnhooks');
$logger->info("Note that the path for the pre-commit code is: $script");

my $hookdir = "$outdir/$dbName/repos/hooks";
my $prc = "$hookdir/pre-commit";
open(F,">", $prc);

#print out the shell script of the commit hook;
print F<<EOF;
#!/bin/sh
export PFAM_CONFIG=$outdir/$dbName/Conf/$dbName.config
export PERL5LIB=$ENV{PERL5LIB}
export PATH=$script:$ENV{PATH}
REPOS="\$1"
REV="\$2"

pre-commit.pl -txn "\$REV" -repos "\$REPOS" || exit 1

EOF
close(F);
chmod( 0755, $prc );


my $pc = "$hookdir/post-commit";
open(F,">", $pc) or die "Could not open $pc:[$!]\n";

#print out the shell script of the commit hook;
print F<<EOF;
#!/bin/sh
export PFAM_CONFIG=$outdir/$dbName/Conf/$dbName.config
export PERL5LIB=$ENV{PERL5LIB}
export PATH=$script:$ENV{PATH}
REPOS="\$1"
REV="\$2"
post-commit.pl -rev "\$REV" -repos "\$REPOS" || exit 1

EOF
close(F);
chmod( 0755, $pc );



sub help {

print<<EOF;

usage: $0 <options>

Options -
  outdir      : The location where the test files will be placed.
  num_fam     : The numbers of families you want in the test repository.
  family      : Name of a family that you want to include, us repeatedly for multiple entries.
  clan        : Name of a clan that you want to include, us repeatedly for multiple entries.
  help        : print this message.

e.g. $0 -num_fam 10 -family PF00001 -family PF00002 -clan CL0003 -outdir test


EOF

exit;
}
