#!/usr/local/bin/perl

use strict;
use warnings;
use LWP::Simple;
use Net::SCP;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $c = Bio::Pfam::Config->new;

#Fetch ftp://ftp.ncbi.nih.gov/ncbi-asn1/protein_fasta index.

my $storeDir = $c->localDbsLoc."/ncbi";
# Store them in /lustre/pfam/pfam/Production/localdbs/ncbi
# fsa_aa.gz 

#Fetch all files from build single fasta.

foreach file {
  add to 
#

#+---------------+------------------+------+-----+---------+-------+
#| Field         | Type             | Null | Key | Default | Extra |
#+---------------+------------------+------+-----+---------+-------+
#| gi            | int(10) unsigned | NO   | PRI |         |       | 
#| secondary_acc | varchar(12)      | NO   |     |         |       | 
#| tertiary_acc  | varchar(23)      | YES  |     | NULL    |       | 
#| md5           | varchar(32)      | NO   | MUL |         |       | 
#| description   | text             | NO   |     |         |       | 
#| length        | mediumint(8)     | NO   |     | 0       |       | 
#| sequence      | blob             | NO   |     |         |       | 
#+---------------+------------------+------+-----+---------+-------+ 
}

#scp bit will be something like this
my $pfamDB =  Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $tmp = "/tmp";
my $scp = Net::SCP->new( { "host"=> $pfamDB->{host} } );
$scp->put("file", "$tmp/file") or die "Couldn't scp file " .  $scp->{errstr};


my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $c->pfamliveAdmin });
my $dbh = $pfamDB->getSchema->storage->dbh;

$dbh->delete(content of ncbi_seq);
$dbh->prepapre("UPLOAD STATEMENT"); 

copy ncbi file to  /lustre/pfam/pfam/Production/pfamseq24
Index with esl-sfetch

The copy file + index to
Remove ncbi* first then copy
/nfs/pfam_nfs/pfam/ncbi/
