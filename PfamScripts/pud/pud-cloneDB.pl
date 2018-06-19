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

my ($schema, $data, $check);

GetOptions("schema" => \$schema,
           "data" =>   \$data,
           "check" =>  \$check);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my ($host, $user, $pass, $port, $live_dbname) = map { $config->pfamlive->{$_} } qw(host adminuser adminpassword port database);
my $clone_dbname = 'pfam_release';

my $check_error;

if ($schema) {

    if (grep { $_ eq $clone_dbname } $dbh->func('_ListDBs')) {
        die "Already have a database named $clone_dbname";
    }

    my $command = "mysql -h$host -P$port -u$user -p$pass $live_dbname -e 'create database $clone_dbname'";
    my $res = `$command`;
    if ($?) {
        die qq(Failure to import schema from $live_dbname to $clone_dbname: $res);
    }

    #my $rc = $dbh->func('createdb', $clone_dbname, 'admin');
    #warn $rc;

    my $command = qq(mysqldump -h$host -P$port -u$user -p$pass -d $live_dbname | mysql -h$host -P$port -u$user -p$pass $clone_dbname);
    my $res = `$command`;
    if ($?) {
        die qq(Failure to import schema from $live_dbname to $clone_dbname: $res);
    }

    my $command = "mysql -h$host -P$port -u$user -p$pass $live_dbname -e 'alter table $clone_dbname.pfamseq drop foreign key FK_pfamseq_1'";
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

    my @tables = qw(pfamA evidence markup_key wikipedia pfamseq clan literature_reference
                    _lock clan_database_links clan_lit_ref clan_membership
                    dead_clan dead_family pfamA_database_links pfamA_literature_reference
                    pfamA_wiki current_pfam_version nested_domains version pfamA_reg_seed 
                    released_pfam_version released_clan_version author pfamA_author);

    foreach my $table (@tables) {
        if (my @matched_cols = columns_matched($table)) {
            my $col_str = join(",", @matched_cols);
            my $sth = $dbh->prepare(qq{insert into $clone_dbname.$table ($col_str) select $col_str from pfam_live.$table});
            $sth->execute or die qq(Failure to copy $table);
        } else {
            my $command = qq(mysqldump -h$host -P$port -u$user -p$pass pfam_live $table | mysql -h$host -P$port -u$user -p$pass $clone_dbname);
            my $res = `$command`;
            if ($?) {
	        die qq(Failure to copy table $table: $res);
            }
        }
    }
}

if ($check) {
    my @tables = qw(pfamA evidence markup_key wikipedia pfamseq clan literature_reference
                    _lock clan_database_links clan_lit_ref clan_membership clan_wiki
                    dead_clan dead_family pfamA_database_links pfamA_literature_reference
                    pfamA_wiki current_pfam_version nested_domains version pfamA_reg_seed
                    author pfamA_author);

    foreach my $table (@tables) {
        print "Table $table: ";
        my $live_rows = $dbh->selectall_arrayref(qq(select count(*) from pfam_live.$table))->[0]->[0];
        my $release_rows = $dbh->selectall_arrayref(qq(select count(*) from $clone_dbname.$table))->[0]->[0];
        if ($live_rows == $release_rows) {
            print "OK [$live_rows]\n";
        } else {
            print "ERROR [$live_rows / $release_rows]\n";
            $check_error++;
        }
    }
}

exit(1) if $check && $check_error; # In case we want this to be run in shell and test for success

sub columns_matched
{
    my ($table) = @_;

    my %columns_live = map { $_->[0], 1 } @{ $dbh->selectall_arrayref(qq(SELECT column_name from information_schema.columns where table_schema = "pfam_live" and table_name = "$table")) };
    my %columns_rel = map { $_->[0], 1 } @{ $dbh->selectall_arrayref(qq(SELECT column_name from information_schema.columns where table_schema = "$clone_dbname" and table_name = "$table")) };
    my %matched;
    foreach my $col (keys %columns_live) {
        $matched{$col} = 1 if exists $columns_rel{$col};
    }
    if (scalar (keys %columns_live) eq scalar(keys %columns_rel)) {
        return ();
    } else {
        return keys %matched;
    }
}
