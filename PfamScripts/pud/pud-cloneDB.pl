#!/usr/bin/env perl

# Clone a pfam live database to [pfam_release]
# Usage
#   pud-cloneDB.pl -schema (create database and copy schema)
#   pud-cloneDB.pl -data   (copy data only)
# Split to allow manual patches to the schema (TODO - add this to avoid a manual step)

use strict;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

# use Log::Log4perl qw(:easy); # do we want this?

my ($schema, $data);

GetOptions("schema" => \$schema,
           "data" =>   \$data);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my ($host, $user, $pass, $port, $live_dbname) = map { $config->pfamlive->{$_} } qw(host adminuser adminpassword port database);
my $clone_dbname = 'pfam_release';

if ($schema) {

    if (grep { $_ eq $clone_dbname } $dbh->func('_ListDBs')) {
        die "Already have a database named $clone_dbname";
    }

    my $command = qq(create database $clone_dbname);
    my $res = `$command`;
    if ($?) {
        die qq(Failure to import schema from $live_dbname to $clone_dbname: $res);
    }

    #my $rc = $dbh->func('createdb', $clone_dbname, 'admin');
    #warn $rc;

    my $command = qq(mysqldump -h$host -P$port -u$user -p$pass -d $live_dbname | mysql -h$host -P$port -u$user -p$pass pfam_release);
    my $res = `$command`;
    if ($?) {
        die qq(Failure to import schema from $live_dbname to $clone_dbname: $res);
    }

    my $command = qq(alter table pfam_release.pfamseq drop foreign key FK_pfamseq_1);
    my $res = `$command`;
    if ($?) {
        die qq(Failure to drop keys on pfamseq: $res);
    }

    # TODO - custom database surgery
}

if ($data) {

    unless (grep { $_ eq $clone_dbname } $dbh->func('_ListDBs')) {
        die "No database named $clone_dbname";
    }

    my @tables = qw(pfamA evidence markup_key wikipedia pfamseq clan literature_references
                    _lock clan_database_links clan_lit_refs clan_membership clan_wiki
                    dead_clan dead_family pfamA_database_links pfamA_literature_references
                    pfamA_wiki current_pfam_version nested_domains version);

    foreach my $table (@tables) {
        my $command = qq(mysqldump -h$host -P$port -u$user -p$pass pfam_live $table | mysql -h$host -P$port -u$user -p$pass pfam_release);
        my $res = `$command`;
        if ($?) {
	    die qq(Failure to copy table $table: $res);
        }
    }
}
