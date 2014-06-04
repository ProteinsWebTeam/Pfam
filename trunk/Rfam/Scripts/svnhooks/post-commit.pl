#!/usr/bin/env perl
#
# This script is run by the svn repository as part of the post-commmit hook. The
# methods in this script deal automatically adding accessions to the respository 
# and moving new entries into the repository.
#
use strict;
use warnings;

use Getopt::Long;
use Bio::Rfam::Config;
use Bio::Rfam::SVN::Commit;
use Bio::Rfam::SVN::Client;
use Bio::Rfam::ClanIO;
use Bio::Rfam::FamilyIO;
use Mail::Mailer;
use Cwd;

my ( $rev, $repos );
GetOptions(
  "rev=s"   => \$rev,
  "repos=s" => \$repos
);

my $revlook;
eval {
  unless ( $rev and $repos )
  {
    die "Need to define both rev and repos: try $0 -help";
  }

  my %params;
  $params{rev}   = $rev;
  $params{repos} = $repos;

  $revlook = Bio::Rfam::SVN::Commit->new( \%params );
  unless ( $revlook and $revlook->isa('SVN::Look') ) {
    die "Failed to get a SVN::Look object for rev:$rev and repos:$repos\n";
  }

  my $config = Bio::Rfam::Config->new;

  #Now see what has just been done for this revision.
  my $logMessage = $revlook->log_msg;
  print STDERR $logMessage."\n";
  # The following actions will trigger a SVN post-commit
  #------------------------------------------------------

# 1. Adding a new model to the repository.
#    This will add the accession to the DESC file and trigger off the view process.

  if ( $logMessage =~ /^NEW\:(\S+)/ ) {
    
    my $family = $1;
    
    print STDERR "Working on $family\n";
    
    my $rfamdb = $config->rfamlive;
    my $entry = $rfamdb->resultset('Family')->find({rfam_id => $family });
    
    
    my $client = Bio::Rfam::SVN::Client->new;
    my $cwd = getcwd;
    
    my $tmpDir = File::Temp->newdir( 'CLEANUP' => 1 );
    
    #Move the entry
    my $dest = $tmpDir->dirname;
    chdir($dest) or die "Could not change into $dest dir:$!";
    open( M, ">.default" . $$ . "rfnewmove" )
      or die "Could not open $dest/.default" . $$ . "rfnewmove:$!";
    print M "Moving from pending to main repository.";
    close(M);
    print STDERR $entry->rfam_acc."\n";
    $client->addRFNEWMOVELog;
    $client->moveNewFamily( $family, $entry->rfam_acc );

    #Now checkout and automatically add the accession to the DESC file
    open( M, ">.default" . $$ . "rfnewmove" )
      or die "Could not open $dest/.default" . $$ . "rfnewmove:$!";
    print M "Automatically adding accession.";
    close(M);
    $client->addRFNEWMOVELog;
    #Now checkout and add the accession to the DESC file!
    $client->checkoutFamily( $entry->rfam_acc, $dest );

    #parse the DESC file
    my $familyIO = Bio::Rfam::FamilyIO->new;
    my $descObj = $familyIO->parseDESC("$dest/DESC");
    $descObj->AC( $entry->rfam_acc );
    $familyIO->writeDESC( $descObj, $dest );

    #Commit back in
    $client->commitFamily($dest);
    chdir($cwd);
  }elsif ( $logMessage =~ /^CLNEW\:(\S+)/ ) {
    
    my $clan_id = $1;
    
    print STDERR "Working on $clan_id\n";
    
    my $rfamdb = $config->rfamlive;
    my $clan = $rfamdb->resultset('Clan')->find({ id => $clan_id });
    
    
    my $client = Bio::Rfam::SVN::Client->new;
    my $cwd = getcwd;
    
    my $tmpDir = File::Temp->newdir( 'CLEANUP' => 1 );
    
    #Move the entry
    my $dest = $tmpDir->dirname;
    #Now checkout and automatically add the accession to the DESC file
    open( M, ">.default" . $$ . "rclnewmove" )
      or die "Could not open $dest/.default" . $$ . "rclnewmove:$!";
    print M "Automatically adding accession.";
    close(M);
    $client->addRCLNEWMOVELog;
    #Now checkout and add the accession to the DESC file!
    $client->checkoutClan( $clan->clan_acc, $dest );

    #parse the CLANDESC file
    my $clanIO = Bio::Rfam::ClanIO->new;
    my $descObj = $clanIO->parseDESC("$dest/CLANDESC");
    $descObj->AC( $clan->clan_acc );
    $clanIO->writeDESC( $descObj, $dest );

    #Commit back in
    $client->commitClan($dest);
    
    
    
    chdir($cwd);
  }elsif( $logMessage =~ /^CI\:/ ){
    
      print STDERR "Working on CI/membership\n";
     my $tmpDir = File::Temp->newdir( 'CLEANUP' => 1 );
     my $dest = $tmpDir->dirname();
     
     
     my @updated_files = $revlook->updated();
     use DDP;
     p(@updated_files);
     my $family;
     foreach my $file (@updated_files){
       if($file =~ /(RF\d{5})/){
          $family = $1;
          last;  
       }
     }
     
     my $client = Bio::Rfam::SVN::Client->new;
     #parse the DESC file
     my $familyIO = Bio::Rfam::FamilyIO->new;
     print STDERR "Checking out $family to $dest\n";
     
     $client->checkoutFamily( $family, $dest );
     print STDERR "Parsing DESC\n";
     my $descObj = $familyIO->parseDESC("$dest/DESC");
    
     #Now validate clan membership.
     validateClanMembership($descObj, $config, $client);
     
  }else {
    #No other commits require post-commit processing
    ;
  }
};

if ($@) {
  print STDERR $@;
  my %header = (
    To      => ['jgt@ebi.ac.uk', 'rdf@ebi.ac.uk'],
    From    => 'rfam@ebi.ac.uk',
    Subject => 'Error in post-commit '
  );
  $header{Cc} = $revlook->author . '@ebi.ac.uk' if ($revlook);

  my $message = "Error with the post commit\n";
  $message .= "Revision: $rev "    if ($rev);
  $message .= "Repository: $repos" if ($repos);
  $message .= "\n$@\n";
  
  _mail(\%header, $message);
  exit(1);
}

sub validateClanMembership{ 
  my ($descObj, $config, $client) = @_;
  
  my($add, $remove);
  $add = $remove = 0;
  my $rfamdb = $config->rfamlive;
  $client = Bio::Rfam::SVN::Client->new if(!$client);
  my @row = $rfamdb->resultset('ClanMembership')->search({ rfam_acc => $descObj->AC });
  #Need to put a transaction around this block
  my $guard = $rfamdb->txn_scope_guard;

  if(defined($descObj->CL) and $descObj->CL =~ /CL\d{5}/ ){
    if(scalar(@row)){
      if($row[0]->clan_acc->clan_acc ne $descObj->CL){
        die "Major error ".$row[0]->clan_acc." does not match ".$descObj->CL."\n";
      }
    }else{
      #The clan must have been added, so add to the table.
      $rfamdb->resultset('ClanMembership')
              ->create({rfam_acc => $descObj->AC, 
                        clan_acc => $descObj->CL});
      $add = 1;
    }  
  }elsif(scalar(@row)){
    foreach my $row (@row){
      $row->delete;
      $remove = 1;  
    }
  }
  if($add or $remove){
    #Check out the clan and update the MEMB-ership.
    
    my $tmpDir = File::Temp->newdir( 'CLEANUP' => 1 );
    my $dest = $tmpDir->dirname;
    my $clan_acc;
    if($descObj->CL){
     $clan_acc = $descObj->CL; 
    }else{
     $clan_acc = $row[0]->clan_acc->clan_acc; 
    }
    #Now checkout and add the accession to the DESC file!
    $client->checkoutClan( $clan_acc, $dest );
    #parse the CLANDESC file
    my $clanIO = Bio::Rfam::ClanIO->new;
    my $clanDescObj = $clanIO->parseDESC("$dest/CLANDESC");
    
    if($add){
     #In the case of new clans, this will be undefined.
     if(!defined($clanDescObj->MEMB)){
        $clanDescObj->MEMB([]); 
     } 
     push(@{$clanDescObj->MEMB}, $descObj->AC);
    }elsif($remove){
      my @newMemb;
      foreach my $mem (@{$clanDescObj->MEMB}){
        if($mem ne $descObj->AC){
          push(@newMemb, $mem);  
        }  
      }
      $clanDescObj->MEMB(\@newMemb);
    }
    $clanIO->writeDESC( $clanDescObj, $dest );
    
        
    #Now checkout and automatically add the accession to the DESC file
    open( M, ">.default" . $$ . "rclmem" )
      or die "Could not open $dest/.default" . $$ . "rclmem:$!";
    print M "Automatically updating memberhsip, ";
    print M ($add == 1 ? 'add' : 'removed');
    print M " ".$descObj->AC;
    close(M);
    $client->addRCLMEMLog;
    
    #Commit back in
    $client->commitClan($dest);
  }
  $guard->commit;
}

sub _mail {
 my($headerRef, $message) = @_;
 
 my $mailer = Mail::Mailer->new();
 $mailer->open( $headerRef );
 print $mailer $message;
 $mailer->close; 
  
}

=head1 COPYRIGHT

File: post-commit.pl

Author: Rob Finn (finnr@janelia.hhmi.org)

Copyright (c) 2010, Howard Hughes Medical Institute, All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.
   3. Neither the name of the Howard Hughes Medical Institute nor the names of 
   its contributors may be used to endorse or promote products derived from 
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY 
IMPLIED WARRANTIES OF MERCHANTABILITY, NON-INFRINGEMENT, OR FITNESS FOR A 
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; REASONABLE ROYALTIES; 
OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING 
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE. 

=cut
