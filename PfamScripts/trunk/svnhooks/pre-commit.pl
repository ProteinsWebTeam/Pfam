#!/usr/local/bin/perl
#
#We have to set this in thes scripts as they do not assume anyones environment variables.
#
#use lib "/Users/finnr/Work/perl/lib/perl5/site_perl/"
#use lib "/Users/finnr/Work/PfamCodeBase/PfamH3Lib:/Users/finnr/Work/PfamCodeBase/PfamLib:/Users/finnr/Work/perl/bioperl-1.4:/Users/finnr/Work/perl/lib/perl5/site_perl:/Users/finnr/Work/perl/lib/perl5/site_perl/5.8.8/darwin-thread-multi-2level:/Users/finnr/Work/perl/System/Library/Perl/5.8.8";

#$ENV{DYLD_LIBRARY_PATH} = "/Users/finnr/Work/Server/modules:/Users/finnr/Work/Software/lib";
use strict;
use warnings;

use SVN::Look;
use Getopt::Long;
use Bio::Pfam::Config;
#use Bio::Pfam::QualityControl;

my( $txn, $repos, $debug, $help );

GetOptions( "txn=s"   => \$txn,
            "repos=s" => \$repos,
            "debug"   => \$debug,
            "help"    => \$help   );
            
unless($txn and $repos){
  die "Need to define both txn and repos: try $0 -help";  
}

my $txnlook = SVN::Look->new( $repos, -t => $txn );

unless($txnlook and $txnlook->isa('SVN::Look')){
  die "Failed to get a SVN::Look object for txn:$txn and repos:$repos\n";
}

my $msg = $txnlook->log_msg();
unless( $msg ){
  die "No commit message passed in!\n"; 
}


#Now see what has changed
my @added_files   = $txnlook->added();
my @updated_files = $txnlook->updated();
my @deleted_files = $txnlook->deleted();
my @changed_files = $txnlook->changed();


if($msg =~ /^PFCI:/){
  
}elsif( $msg =~ /^PFNEW:/ ){
  
}elsif( $msg =~ /^PFANN:/ ) {
    
}elsif( $msg =~ /^PFMOV:/ ) {
    
}elsif( $msg =~ /^PFKIL:/ ) {
    
}elsif( $msg =~ /^PFANN:/ ) {
    
}else{
  die "Do not know here this commit has come from!\n"; 
}


#my @added_files   = $txnlook->added();
#
#print STDERR "Added files @added_files\n";
#
#my @updated_files = $txnlook->updated();
#print STDERR "Updated files @updated_files\n";
#foreach my $file (@updated_files){
#  print STDERR $txnlook->cat($file);
#  $file =~ s/DESC/ALIGN/;
#  print STDERR  $txnlook->cat($file); 
#}



#my @deleted_files = $txnlook->deleted();
#print STDERR "Deleted files @deleted_files\n";
#my @changed_files = $txnlook->changed();
#print STDERR "Changed files @changed_files\n";
#my $file_contents = $txnlook->cat('/path/to/file/in/repository');



#print STDERR "Load $repos through Pfam Middleware\n";

die;
exit(0);


