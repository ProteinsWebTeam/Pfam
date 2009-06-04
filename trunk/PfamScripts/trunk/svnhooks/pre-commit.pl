#!/software/bin/perl
#
#We have to set this in thes scripts as they do not assume anyones environment variables.
#
# pre-commit.pl -txn 1388 -repos /Users/finnr/Work/Repository


use strict;
use warnings;

use SVN::Look;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::SVN::Commit;
use Data::Dumper;

my( $rev, $txn, $repos, $debug, $help );

GetOptions( "rev=s"   => \$rev,
            "txn=s"   => \$txn,
            "repos=s" => \$repos );

if( $rev and $txn ){
  die "Can not define both and revision and a transaction\n";  
}
            
my %params;
if($rev){
  $params{rev} = $rev; 
}else{
  $params{txn} = $txn;  
}

$params{repos} = $repos;

my $txnlook = Bio::Pfam::SVN::Commit->new( \%params );

unless($txnlook and $txnlook->isa('SVN::Look')){
  die "Failed to get a SVN::Look object for txn:$txn and repos:$repos\n";
}

my $msg = $txnlook->log_msg();
unless( $msg ){
  die "No commit message passed in!\n"; 
}

my $config = Bio::Pfam::Config->new;
my $connect = $config->pfamlive;
print STDERR Dumper($connect);
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{ $connect }
);

print STDERR "*** $msg ***\n";

#$msg = "$msg";

#Now see what has changed
my @added_files   = $txnlook->added();
my @updated_files = $txnlook->updated();
my @deleted_files = $txnlook->deleted();
my @changed_files = $txnlook->changed();
foreach my $f (@updated_files){
  print STDERR "Updated:".$f."\n";
}
foreach my $f (@added_files){
  print STDERR "Added:".$f ."\n";
}
foreach my $f (@changed_files){
  print STDERR "Changed:".$f ."\n";
}
foreach my $f (@deleted_files){
  print STDERR "Deleted:".$f ."\n";
}

if($msg =~ /^PFCI:/){
  $txnlook->commitFamily( $pfamDB );
}elsif($msg =~ /PFCIATC:(CL\d{4})\:(PF\d{5})/){
  my($clan, $fam);
  $clan = $1;
  $fam  = $2;
  #Add the clan data to the database
  $txnlook->updateClanMembership($pfamDB, $clan, $fam);
  #Then commit the family
  $txnlook->commitFamily( $pfamDB );
}elsif( $msg =~ /^PFNEW:/ ){
  $txnlook->commitNewFamily( $pfamDB ); 
}elsif( $msg =~ /^CLNEW:/ ){
  $txnlook->commitNewClan( $pfamDB ); 
}elsif( $msg =~ /^(PF|CL)NEWMOV:/ ){
  ;
}elsif( $msg =~ /^CLNEWACC:/ ){
  ;}elsif( $msg =~ /^AUTOMB/){
  ;  
}elsif( $msg =~ /^PFANN:/ ) {
  $txnlook->commitDesc;  
}elsif( $msg =~ /^PFMOV:/ ) {
  $txnlook->moveFamily( $pfamDB );  
}elsif( $msg =~ /^PFKIL:/ ) {
  $txnlook->deleteFamily( $pfamDB );
}else{
  die "Do not know here this commit has come from, [$msg]!\n"; 
}

exit(0);


