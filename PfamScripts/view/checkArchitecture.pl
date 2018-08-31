#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(get_logger :levels);
use Data::Dump qw(dump);
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess;


Log::Log4perl->init(
  \<<EOF
log4perl.rootLogger=DEBUG, SCREEN
# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);


my $logger = get_logger();
$logger->level($DEBUG);

my $config = Bio::Pfam::Config->new;
my $view = Bio::Pfam::ViewProcess->new;


my $dbh = $view->pfamdb->getSchema->storage->dbh;
$logger->debug("Got pfamlive database connection");

my @pfamARS =  $view->pfamdb->getSchema->resultset('Pfama')->search();

my $acc2id;
my $auto2row;
foreach my $p (@pfamARS){
  $acc2id->{$p->pfama_acc} = $p->pfama_id;
  $auto2row->{$p->auto_pfama} = $p;
}

#For every architecture, check that the names are still valid.

my @archs = $view->pfamdb->getSchema->resultset('Architecture')->search();
my $archs;
my $fromToArchs;

my %invalid;
foreach my $a (@archs){
  my @acclist = split(/\s+/, $a->architecture_acc);
  my $valid = 1;
  my @idlist;
  foreach my $acc (@acclist){
    if(exists($acc2id->{$acc})){
      push(@idlist, $acc2id->{$acc});  
    }else{
      #Architecture is invalid
      warn "Need to delete ".$a->auto_architecture."\n"; 
      $valid = 0;
    }
  }
  my $idString = join("~", @idlist);
  if($idString ne $a->architecture and $valid){
    $a->update({architecture => $idString});    
  }
  if(exists($archs->{$a->architecture_acc})){
    warn  "Need to delete ".$a->auto_architecture." and update pfamseq to ".$archs->{$a->architecture_acc}."\n";     
    $fromToArchs->{$a->auto_architecture} = $archs->{$a->architecture_acc};
  }else{
    $archs->{$a->architecture_acc} = $a->auto_architecture
  }
}

print dump($fromToArchs);
exit;

#Update pfamseq when duplicate architecture has been removed
#Remove duplicated architectures caused by a race condition.
my $pfamseqSth = $dbh->prepare("update pfamseq set auto_architecture = ? where auto_architecture = ?");
my $architectureSth = $dbh->prepare("delete from architecture where auto_architecture = ?");
while( my ($from, $to) = each %{$fromToArchs}){
  $pfamseqSth->execute($to, $from);
  $architectureSth->execute($from);
}
$pfamseqSth->finish;
$architectureSth->finish;

exit;
#Update the counts in the architecture table
$dbh->do("update architecture a set a.no_seqs=(select count(*) from pfamseq s where s.auto_architecture=a.auto_architecture)");



#Remove any obsolete architectures, where the counts are zero

#Generate the pfamA_architecture table

#Update the counts in the PfamA table.
#my $archCountSth = $dbh->prepare("UPDATE architecture SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=?) where auto_architecture=?");


#-------------------------------------------------------------------------------
#Update the pfamA_architecture file for each modified family.


#my $pfamArchSth = $dbh->prepare(
#      "INSERT INTO pfamA_architecture (auto_pfamA, auto_architecture) "
#    . " SELECT DISTINCT r.auto_pfamA, auto_architecture FROM pfamA_reg_full_significant r, pfamseq s "
#    . " WHERE s.auto_pfamseq=r.auto_pfamseq AND in_full=1 AND auto_pfamA= ? " )
#  or $logger->logdie(
#  "Failed to prepare statment to update pfamA_architectures:" . $dbh->errstr );
#
#foreach my $autoPfama (@autoPfamAs) {
#  $pfamDB->getSchema->resultset('PfamaArchitecture')->search({ auto_pfamA => $autoPfama })->delete;
#  $pfamArchSth->execute($autoPfama)
#    or
#    $logger->logdie( "Failed to execute pfamA_arch statement:" . $dbh->errstr );
#  my $rs = $pfamDB->getSchema->resultset('PfamaArchitecture')->search({ auto_pfamA => $autoPfama });
#  my $pfamA = $pfamDB->getSchema->resultset('Pfama')->find({auto_pfama => $autoPfama});
#  $pfamA->update({ number_archs => $rs->count } );
#}
#
##-------------------------------------------------------------------------------
##Update the clan_archtiecture table
#
##Determine the list of clans affected by the updated families
#my @clanMembers = 
#  $pfamDB->getSchema->resultset('ClanMembership')->search( {
#    'auto_pfama' => [ @autoPfamAs ],
#  },
#  {
#    columns  => [qw(me.auto_clan)],
#    distinct => 1,
#  });
#
#my $clanArchSth = $dbh->prepare(
# "INSERT INTO clan_architecture (auto_clan, auto_architecture) ".
# " SELECT DISTINCT c.auto_clan, auto_architecture from clan_membership c, pfamA_reg_full_significant r, pfamseq s ".
# " WHERE s.auto_pfamseq=r.auto_pfamseq AND c.auto_pfamA=r.auto_pfamA AND in_full=1 AND c.auto_clan= ? ");
#
#my $clanUpdateNumArch = $dbh->prepare(
#  "UPDATE clans c SET number_archs = (SELECT COUNT(DISTINCT auto_architecture) FROM clan_architecture a ".
#  " WHERE c.auto_clan=a.auto_clan) where c.auto_clan= ? ");

#Need to update the architecture table names as well as remove architectures that
#do not match anything in pfamseq any more....
  
#my $archCountSth = $dbh->prepare("UPDATE architecture SET no_seqs = (select count(*) from pfamseq s where s.auto_architecture=?) where auto_architecture=?");
  
  #foreach my $archStr ( keys %cachedArchs){
  #  $archCountSth->execute($cachedArchs{$archStr}->auto_architecture, $cachedArchs{$archStr}->auto_architecture);  
  #}