#!/usr/local/bin/perl
#
#We have to set this in thes scripts as they do not assume anyones environment variables.
#
#use lib "/Users/finnr/Work/perl/lib/perl5/site_perl/"
use lib "/Users/finnr/tmp";

use lib "/Users/finnr/Work/PfamCodeBase/PfamLib";
use lib "/Users/finnr/Work/perl/bioperl-1.4";
use lib "/Users/finnr/Work/perl/lib/perl5/site_perl";
use lib "/Users/finnr/Work/perl/lib/perl5/site_perl/5.8.8/darwin-thread-multi-2level";
use lib "/Users/finnr/Work/perl/System/Library/Perl/5.8.8";
use lib "/Users/finnr/Work/PfamCodeBase/PfamH3Lib"; 

$ENV{PFAM_CONFIG} = "/Users/finnr/Work/PfamCodeBase/PfamConfigJFRC/PfamCode/pfam_code.conf";

# pre-commit.pl -txn 1388 -repos /Users/finnr/Work/Repository


$ENV{DYLD_LIBRARY_PATH} = "/Users/finnr/Work/Server/modules:/Users/finnr/Work/Software/lib";

use lib "/Users/finnr/tmp";

use strict;
use warnings;

#use SVN::Look;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::SVN::Commit;

my( $rev, $txn, $repos, $debug, $help );

GetOptions( "rev=s"   => \$rev,
            "txn=s"   => \$txn,
            "repos=s" => \$repos );

if( $rev and $txn ){
  die "Can not define both and revision and a transaction\n";  
}
            
unless(($txn and $repos) or ($rev and $repos)){
  die "Need to define both txn and repos: try $0 -help";  
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

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{ $connect }
);


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
}elsif( $msg =~ /^PFNEW:/ ){
  
}elsif( $msg =~ /^PFANN:/ ) {
    
}elsif( $msg =~ /^PFMOV:/ ) {
  $txnlook->moveFamily( $pfamDB );  
}elsif( $msg =~ /^PFKIL:/ ) {
    
}else{
  die "Do not know here this commit has come from!\n"; 
}

exit(0);


