# Client.pm
#
# Author:        rdf
# Maintainer:    $Id: Client.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 27, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $

=head1 NAME

Bio::Pfam::SVN::Client - a modules that enables access to the Pfam SVN

=cut

package Bio::Pfam::SVN::Client;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: Client.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: SVN.pm

Copyright (c) 2007: Genome Research Ltd.

Author: Rob Finn (rdf@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

use strict;
use warnings;
use Cwd;
use Carp;
use Term::ReadPassword;
use SVN::Client;
use File::Copy;
use File::Temp;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
#-------------------------------------------------------------------------------

=head1 METHODS


=cut

sub new {
  my ( $caller, $params ) = @_;
  my $class = ref($caller) || $caller;

  my ( $user, $passwd );
  if ( $params and ref($params) eq 'HASH' ) {
    if ( $params->{'user'} ) {
      $user = $params->{'user'};
    }
    if ( $params->{'passwd'} ) {
      $user = $params->{'passwd'};
    }
  }

  #This has been taken directly from SVN::Client Pod, except
  #I have made it mask the typing in of a password.  Longer term
  #I am going to have to be able to discet SVN::Client so that
  #it is possible to provide a username/password. This will be
  #important for bluk check-ins!  However, the pasword may be stored....

  my $passWordRequest = sub {
    my $cred     = shift;
    my $realm    = shift;
    my $status   = shift;
    my $auth     = shift;
    my $may_save = shift;
    my $pool     = shift;

    print "Enter authentication info for realm: $realm\n";
    print "Username: ";
    my $username = <>;
    chomp($username);

    $cred->may_save(1);
    $cred->username($username);
    my $password = read_password('password: ');
    redo unless defined $password;
    $cred->password($password);

  };

  my $self;
  $self->{txn} = SVN::Client->new(
    auth => [
      SVN::Client::get_simple_provider(),
      SVN::Client::get_ssl_server_trust_file_provider(),
      SVN::Client::get_username_provider(),
      SVN::Client::get_simple_prompt_provider( $passWordRequest, 2 ),
      SVN::Client::get_ssl_server_trust_prompt_provider(
        \&_ssl_server_trust_prompt
      ),

      SVN::Client::get_ssl_server_trust_prompt_provider(
        \&_ssl_server_trust_prompt
      ),
      SVN::Client::get_ssl_client_cert_prompt_provider(
        \&_ssl_client_cert_prompt, 2
      ),
      SVN::Client::get_ssl_client_cert_pw_prompt_provider(
        \&_ssl_client_cert_pw_prompt, 2
      ),
      SVN::Client::get_username_prompt_provider( \&_username_prompt, 2 ),
      SVN::Client::get_ssl_server_trust_file_provider()
    ]
  );

  $self->{checkFile} = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      confess("$path is invalid");
    }
  };

  $self->{config} = Bio::Pfam::Config->new;
  return ( bless( $self, $class ) );
}

sub _ssl_server_trust_prompt {
  my ( $cred, $realm, $failures, $cert_info, $may_save, $pool ) = @_;

  print "Error validating server certificate for '$realm':\n";

  print " - The certificate is not issued by a trusted authority. Use the\n",
    "   fingerprint to validate the certificate manually!\n"
    if ( $failures & $SVN::Auth::SSL::UNKNOWNCA );

  print " - The certificate hostname does not match.\n"
    if ( $failures & $SVN::Auth::SSL::CNMISMATCH );

  print " - The certificate is not yet valid.\n"
    if ( $failures & $SVN::Auth::SSL::NOTYETVALID );

  print " - The certificate has expired.\n"
    if ( $failures & $SVN::Auth::SSL::EXPIRED );

  print " - The certificate has an unknown error.\n"
    if ( $failures & $SVN::Auth::SSL::OTHER );

  printf(
    "Certificate information:\n"
      . " - Hostname: %s\n"
      . " - Valid: from %s until %s\n"
      . " - Issuer: %s\n"
      . " - Fingerprint: %s\n",
    map $cert_info->$_,
    qw(hostname valid_from valid_until issuer_dname fingerprint)
  );

  print( $may_save
    ? "(R)eject, accept (t)emporarily or accept (p)ermanently? "
    : "(R)eject or accept (t)emporarily? "
  );

  my $choice = lc( substr( <STDIN> || 'R', 0, 1 ) );

  if ( $choice eq 't' ) {
    $cred->may_save(0);
    $cred->accepted_failures($failures);
  }
  elsif ( $may_save and $choice eq 'p' ) {
    $cred->may_save(1);
    $cred->accepted_failures($failures);
  }
}

sub _ssl_client_cert_prompt {
  my ( $cred, $realm, $may_save, $pool ) = @_;
  print "Client certificate filename: ";
  chomp( my $filename = <STDIN> );
  $cred->cert_file($filename);

}

sub _ssl_client_cert_pw_prompt {
  my ( $cred, $realm, $may_save, $pool ) = @_;
  my $password = read_password("Password for '%s': ");
  redo unless defined $password;
  $cred->password( _read_password($password) );
}

sub _username_prompt {
  my ( $cred, $realm, $may_save, $pool ) = @_;

  print "Authentication realm: $realm\n" if defined $realm and length $realm;
  print "Username: ";
  chomp( my $username = <STDIN> );
  $username = '' unless defined $username;
  $cred->username($username);

}

sub checkFamilyExists {
  my ( $self, $family, $config ) = @_;

  my $url     = $self->familyLocation . "/" . $family;
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      die "$path is invalid\n";
    }
  };
  eval { $self->{txn}->info( $url, undef, 'HEAD', $codeRef, 0 ); };

  if ($@) {

    #Check to see if the family has been killed, if so give details and exit
    if ( $config and $config->location eq 'WTSI' ) {
      my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

      my @dead =
        $pfamDB->getSchema->resultset("DeadFamilies")
        ->search( { pfama_acc => $family } );
      foreach my $dead (@dead) {

        print "$family ("
          . $dead->pfama_id
          . ") was killed by "
          . $dead->user . " on "
          . $dead->killed . "\n";

        if ( $dead->comment ) {
          print "Comment: " . $dead->comment . "\n";
        }
        else {
          print "Comment:\n";
        }

        if ( $dead->forward_to ) {
          print "Members of this family have been forwarded to "
            . $dead->forward_to . "\n";
        }
        else {
          print "Members of this family have not been forwarded\n";
        }

        exit 0;
      }
    }
    
    confess("\n*** $family does not exist in the respository ***\n\n".
            "Looking at $url.\n[$@]\n");
  }
}

sub checkNewFamilyDoesNotExists {
  my ( $self, $family ) = @_;

  my $url     = $self->newFamilyLocation . "/" . $family;
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      die "$path is invalid\n";
    }
  };

  #This should throw a warning!
  eval { $self->{txn}->info( $url, undef, 'HEAD', $codeRef, 0 ); };

  unless ($@) {
    confess( "$family exist in the respository, at $url.  "
        . "This should not happen, as the new family should be moved into "
        . "the repoistory shortly after being added.  Please try again in a "
        . "few minutes, if the problem persists, then something is wrong\n" );
  }
}

sub checkClanExists {
  my ( $self, $clan, $config ) = @_;

  my $url     = $self->clanLocation . "/" . $clan;
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      die "$path is invalid\n";
    }
  };
  eval { $self->{txn}->info( $url, undef, 'HEAD', $codeRef, 0 ); };

  if ($@) {

    #Check to see if the clan has been killed, if so give details and exit
    if ( $config and $config->location eq 'WTSI' ) {
      my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
      my @dead =
        $pfamDB->getSchema->resultset("DeadClans")
        ->search( { clan_acc => $clan } );
      foreach my $dead (@dead) {
        print "$clan ("
          . $dead->clan_id
          . ") was killed by "
          . $dead->user . " on "
          . $dead->killed . "\n";
        if ( $dead->comment ) {
          print "Comment: " . $dead->comment . "\n";
        }
        else {
          print "Comment:\n";
        }

        if ( $dead->forward_to ) {
          print "Members of this clan have been forwarded to "
            . $dead->forward_to . "\n";
        }
        else {
          print "Members of this clan have not been forwarded\n";
        }
        exit 0;
      }
    }
    confess( "\n*** $clan does not exist in the respository *** \n\n"
        . "Looking at $url.\n[$@]\n" );
  }
}

sub checkNewClanDoesNotExists {
  my ( $self, $clan ) = @_;

  my $url     = $self->newClanLocation . "/" . $clan;
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      die "$path is invalid\n";
    }
  };

  #This should throw a warning!
  eval { $self->{txn}->info( $url, undef, 'HEAD', $codeRef, 0 ); };

  unless ($@) {
    confess( "$clan exist in the respository, at $url.  "
        . "This should not happen, as the new clan should be moved into "
        . "the repoistory shortly after being added.  Please try again in a "
        . "few minutes, if the problem persists, then something is wrong\n" );
  }
}

sub checkFamilyDoesNotExist {
  my ( $self, $family ) = @_;

  my $url     = $self->familyLocation . "/" . $family;
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    if ($info) {
      die "Found information for $path\n";
    }
  };

  eval { $self->{txn}->info( $url, undef, 'HEAD', $codeRef, 0 ); };

  unless ($@) {
    confess("$family seems to exist in the respository at $url.\n[$@]\n");
  }
}

sub checkAllFamilyFiles {
  my ( $self, $family ) = @_;

  my $url = $self->familyLocation . "/" . $family;

  #Todo Change this for a call
  foreach my $file ( keys %{ $self->{config}->{files}->{family} } ) {
    eval {

#$self->{txn}->info($url."/".$file, undef, $self->revision , $self->_checkFile, 0 );
      $self->{txn}
        ->info( $url . "/" . $file, undef, 'HEAD', $self->_checkFile, 0 );
    };
    if ($@) {

      #Todo, should change this to confess
      warn
"$file for $family does not exist in the respository at $url.  This is very bad [$@]\n";
    }
  }
}

sub checkAllClanFiles {
  my ( $self, $clan ) = @_;

  my $url = $self->clanLocation . "/" . $clan;

  #Todo Change this for a call
  foreach my $file ( keys %{ $self->{config}->{files}->{clan} } ) {
    eval {
      $self->{txn}->info( $url . "/" . $file,
        undef, $self->revision, $self->_checkFile, 0 );
    };
    if ($@) {

      #Todo, should change this to confess
      warn
"$file for $clan does not exist in the respository at $url.  This is very bad\n";
    }
  }
}

sub checkoutFamily {
  my ( $self, $family, $dest ) = @_;
  my $url = $self->familyLocation . "/" . $family;

  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  if ($@) {
    confess("Failed to check out family, $family:[$!]\n");
  }
}

sub checkoutAllFamilies {
  my ( $self, $dest ) = @_;
  my $url = $self->familyLocation;

  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  if ($@) {
    confess("Failed to check out all families:[$!]\n");
  }
}

sub checkoutFamilyDESC {
  my ( $self, $family, $dest ) = @_;
  my $url = $self->familyLocation . "/" . $family . "/DESC";

  print STDERR "$url";
  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  if ($@) {
    confess("Failed to check out family DESC, $family:[$!]\n");
  }
}

sub checkoutClan {
  my ( $self, $clan, $dest ) = @_;
  my $url      = $self->clanLocation . "/" . $clan;
  my $destClan = $dest . "/" . $clan;
  eval {
    $self->{txn}
      ->checkout( $url, $destClan, $self->revision ? $self->revision : 'HEAD',
      1 );
  };

  if ($@) {
    confess("Failed to check out clan, $clan, at $destClan from $url:[$@]\n");
  }

}

sub checkoutAllClans {
  my ( $self, $dest ) = @_;
  my $url = $self->clanLocation;

  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  if ($@) {
    confess("Failed to check out all clans from $url:[$@]\n");
  }

}

sub catFile {
  my ( $self, $dir, $filename, $fh, $rev ) = @_;

  my $url;
  if ( $dir =~ /PF\d{5}/ ) {

    #Looks like a Pfam family
    $url = $self->familyLocation . "/" . $dir;
  }
  elsif ( $dir =~ /CL\d{4}/ ) {

    #Looks like a Pfam clan
    $url = $self->clanLocation . "/" . $dir;
  }
  unless ($fh) {
    $fh = \*STDOUT;
  }
  $self->{txn}->cat(
    $fh,
    $url . "/" . $filename,
    defined($rev) ? $rev : ( $self->revision ? $self->revision : 'HEAD' )
  );
}

sub log {
  my ( $self, $dir, $rev ) = @_;
  my $url;
  if ( $dir =~ /PF\d{5}/ ) {

    #Looks like a Pfam family
    $url = $self->familyLocation . "/" . $dir;
  }
  elsif ( $dir =~ /CL\d{4}/ ) {

    #Looks like a Pfam clan
    $url = $self->clanLocation . "/" . $dir;
  }

  my $subRef = sub {
    my ( $changed_paths, $revision, $author, $date, $message ) = @_;
    chomp($message);
    print STDOUT
"-------------------------------------------------------------------------------\n";
    print STDOUT
      "revision $revision\ndate: $date;  author: $author;\n$message\n";
  };

  print $self->{txn}
    ->log( $url, 1, defined($rev) ? $rev : $self->revision, 0, 0, $subRef );
  print STDOUT
"===============================================================================\n";

}

sub commitFamily {
  my ( $self, $family ) = @_;

  #And finally commit them.
  my $cinfo;
  eval { $cinfo = $self->{txn}->commit( $family, 1 ); };

  if ($@) {
    confess("Failed to commit family, $family: [$@]\n");
  }

  #Now check that something happen!
  $self->_checkCommitObj($cinfo);

}

sub addFamily {
  my ( $self, $family, $newFamilyId ) = @_;

  unless ($family) {
    confess("Did not get the local directory name for the family\n");
  }

  unless ($newFamilyId) {
    confess("Did not get the new family identifier\n");
  }

  unless ( -d $family ) {
    confess("$family is not a directory\n");
  }

  #Repository location of where to put new families
  my $url = $self->newFamilyLocation;

  #Generate a new tempdir
  my $dir  = File::Temp->newdir();
  my $dest = $dir . "/FamiliesPending";

  #Now check out the latest version the holding area.
  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  if ($@) {
    confess("Failed to check out new families dir, $url to $dest:[$!]\n");
  }

 #Copy the directory to pending families directory. Only copy the files we need!
  unless ( -e "$dest/$newFamilyId" ) {
    mkdir("$dest/$newFamilyId")
      or confess("Could not make directory $dest/$family:[$!]\n");
  }

  foreach my $file ( @{ $self->{config}->mandatoryFamilyFiles } ) {
    copy( "$family/$file", "$dest/$newFamilyId/$file" )
      or confess("Failed to copy $family/$file to $dest/$newFamilyId/$file");
  }

  #Now add the families.
  eval { $self->{txn}->add( "$dest/$newFamilyId", 1 ); };

  if ($@) {
    confess(
"\n*** Failed to add family, $newFamilyId to the respository ***\n\n[$@]\n"
    );
  }

  #And finally commit them.
  my $cinfo;
  eval { $cinfo = $self->{txn}->commit( "$dest/$newFamilyId", 1 ); };

  if ($@) {
    confess("\n*** Failed to commit new family to $newFamilyId ***\n\n[$@]\n");
  }

  #Now check that something happen!
  $self->_checkCommitObj($cinfo);

}

sub update {
  my ( $self, $family ) = @_;

  #Okay, for each of the manditory files, run an SVN update.
  foreach my $file ( @{ $self->{config}->mandatoryFamilyFiles } ) {
    $self->{txn}
      ->update( $family . "/" . $file, $self->{config}->svnRevision, 1 );
  }

}

sub killFamily {
  my ( $self, $family ) = @_;

  #Okay, get the URL of the families location
  my $url;
  if ( $family =~ /PF\d{5}/ ) {

    #Looks like a Pfam family
    $url = $self->familyLocation . "/" . $family;
  }

  #Now delete the family
  my $kinfo;
  eval { $kinfo = $self->{txn}->delete( $url, 0 ); };

  if ($@) {
    confess("Failed to delete family, $family: [$@]\n");
  }

  #Now check that something happen to the repository!
  $self->_checkCommitObj($kinfo);
}

sub killClan {
  my ( $self, $clan ) = @_;

  #Okay, get the URL of the clan location
  my $url;
  if ( $clan =~ /CL\d{4}/ ) {

    #Looks like a clan
    $url = $self->clanLocation . "/" . $clan;
  }

  #Now delete the clan
  my $kinfo;
  eval { $kinfo = $self->{txn}->delete( $url, 0 ); };

  if ($@) {
    confess("Failed to delete clan, $clan: [$@]\n");
  }

  #Now check that something happen to the repository!
  $self->_checkCommitObj($kinfo);
}

#-------------------------------------------------------------------------------

=head2 commitClan 

  Title    : commitClan
  Usage    : $client->commitClan($pathToClan); 
  Function : Triggers the commit of the clan from the client to the SVN.
           : If the commit fails, it will print the trace from the server pre-commit
           : and if no changes are made, it will warn that no changes have been made
           : as part of the commit. As such, not database upload will be triggered.  
  Args     : Path to the clan.
  Returns  : Nothing
  
=cut

sub commitClan {
  my ( $self, $clan ) = @_;
  my $cinfo;
  eval { $cinfo = $self->{txn}->commit( $clan, 1 ); };

  #Catch any error and report.
  if ($@) {
    die "\n*** MAJOR ERROR during Clan commit! FAILURE***\n\n$@";
  }

  $self->_checkCommitObj($cinfo);

}

sub addClan {
  my ( $self, $clan, $newClanId ) = @_;

  unless ($clan) {
    confess("Did not get the local directory name for the clan\n");
  }

  unless ($newClanId) {
    confess("Did not get the new clan identifier for $clan\n");
  }

  unless ( -d $clan ) {
    confess("$clan is not a directory\n");
  }

  #Repository location of where to put new families
  my $url = $self->newClanLocation;

  #Generate a new tempdir
  my $dir  = File::Temp->newdir();
  my $dest = $dir . "/FamiliesPending";

  #Now check out the latest version the holding area.
  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  if ($@) {
    confess("Failed to check out new clans dir, $url to $dest:[$!]\n");
  }

 #Copy the directory to pending families directory. Only copy the files we need!
  unless ( -e "$dest/$newClanId" ) {
    mkdir("$dest/$newClanId")
      or confess("Could not make directory $dest/$newClanId:[$!]\n");
  }

  #TODO - Change this to come from the config
  #  foreach my $file (@{ $self->{config}->mandatoryFamilyFiles }){
  foreach my $file (qw(CLANDESC)) {
    copy( "$clan/$file", "$dest/$newClanId/$file" )
      or confess("Failed to copy $clan/$file to $dest/$newClanId/$file");
  }

  #Now add the Clan.
  eval { $self->{txn}->add( "$dest/$newClanId", 1 ); };

  if ($@) {
    confess(
      "\n*** Failed to add clan, $newClanId to the respository ***\n\n[$@]\n");
  }

  #And finally commit them.
  my $cinfo;
  eval {
    $cinfo = $self->{txn}->commit("$dest/$newClanId", 1);
  };
  
  if($@){
    confess("\n*** Failed to commit new clan  to $newClanId - Possible duplicate name? ***\n\n[$@]\n");   
  }

  #Now check that something happen!
  $self->_checkCommitObj($cinfo);

}

sub moveNewFamily {
  my ( $self, $familyId, $familyAcc, $rev ) = @_;

  my $oldUrl = $self->newFamilyLocation . "/" . $familyId;
  my $newUrl = $self->familyLocation . "/" . $familyAcc;

  my $cinfo;
  eval {
    $cinfo =
      $self->{txn}->move( $oldUrl,
      defined($rev) ? $rev : ( $self->revision ? $self->revision : 'HEAD' ),
      $newUrl, 0 );
  };

  #Catch any error and report.
  if ($@) {
    die "\n*** MAJOR ERROR during post commit for pfnew! FAILURE***\n\n$@";
  }

  $self->_checkCommitObj($cinfo);
}

sub moveNewClan {
  my ( $self, $clanId, $clanAcc, $rev ) = @_;

  my $oldUrl = $self->newClanLocation . "/" . $clanId;
  my $newUrl = $self->clanLocation . "/" . $clanAcc;

  my $cinfo;

  eval {
    $cinfo =
      $self->{txn}->move( $oldUrl,
      defined($rev) ? $rev : ( $self->revision ? $self->revision : 'HEAD' ),
      $newUrl, 0 );
  };

  #Catch any error and report.
  if ($@) {
    die "\n*** MAJOR ERROR during post commit for clnew! FAILURE***\n\n$@";
  }

  $self->_checkCommitObj($cinfo);
}

# AFAIK This is no longer required.
sub moveFamily {
  my ( $self, $oldFamily, $newFamily, $rev ) = @_;
  my $oldUrl = $self->familyLocation . "/" . $oldFamily;
  my $newUrl = $self->familyLocation . "/" . $newFamily;
  $self->{txn}->move( $oldUrl,
    defined($rev) ? $rev : ( $self->revision ? $self->revision : 'HEAD' ),
    $newUrl, 0 );
}

# Priavte methods that should not be used from outside this module
#-------------------------------------------------------------------------------

=head2 subname 

  Title    :
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub _checkCommitObj {
  my ( $self, $cinfo ) = @_;

  #Check that in the commit info object that the revision is not -1 or 0
  if ($cinfo) {
    unless ( $cinfo->revision > 0 ) {
      warn "Nothing changed upon commit\n";
    }
  }
  else {
    confess("Failed to get a commit info object!\n");
  }

}

sub _checkFile {
  my ($self) = @_;
  return ( $self->{checkFile} );
}

sub revision {
  my ($self) = @_;
  return ( $self->{config}->svnRevision );
}

sub familyLocation {
  my ($self) = @_;
  my $familyUrl = $self->{config}->svnRepos . $self->{config}->svnFamilies;
  return ($familyUrl);
}

sub newFamilyLocation {
  my ($self) = @_;
  my $newFamilyUrl =
    $self->{config}->svnRepos . $self->{config}->svnNewFamilies;
  return ($newFamilyUrl);
}

sub clanLocation {
  my ($self) = @_;
  my $clanUrl = $self->{config}->svnRepos . $self->{config}->svnClans;
  return ($clanUrl);
}

sub newClanLocation {
  my ($self) = @_;
  my $clanUrl = $self->{config}->svnRepos . $self->{config}->svnNewClans;
  return ($clanUrl);
}

# Below here are all of the log message callbacks. There is a certain amount
# of repetition due tot he fact they are usig callbacks and therefore are
# bespoke code refs depending on the log. The prefixes are really important
# for triggering svn hooks, so be careful!
#-------------------------------------------------------------------------------

=head2 subname 

  Title    : add
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub addPFNEWLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfnew" ) {
      open( M, ".default" . $$ . "pfnew" )
        or die "Could not open .default" . $$ . "pfnew";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }
    else {
      die "Failed to open file pfnew message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFNEW:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFNEWATCLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfnew" ) {
      open( M, ".default" . $$ . "pfnew" )
        or die "Could not open .default" . $$ . "pfnew";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for the changes to this family\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFNEWATC:" . $message;

  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFCILog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfci" ) {
      open( M, ".default" . $$ . "pfci" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for the changes to this family\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFCI:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFKILLLog {
  my ($self) = @_;
  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfkill" ) {
      open( M, ".default" . $$ . "pfkill" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }
    else {
      die
        "No pfkill message as to why the family is being removed from pfam!\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFKILL:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFCIATCLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $clan;
    unless ( -s ".atc" . $$ ) {
      die "Could not find the file containing the clan accession!\n";
    }
    open( C, ".atc" . $$ ) or die "Could not open .atc\n";
    while (<C>) {
      $clan = $_;
      last;
    }
    close(C);

    my $message;

    #See if we have a default$$ messge to use.
    if ( -s ".default" . $$ . "pfci" ) {
      open( M, ".default" . $$ . "pfci" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for the changes to this family\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFCIATC:" . $message;
    $$passmessage .= "\nPFCIATC:" . $clan;

  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFCIRMCLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $clan;
    unless ( -s ".rmc" . $$ ) {
      die "Could not find the file containing the clan accession!\n";
    }
    open( C, ".rmc" . $$ ) or die "Could not open .rmc\n";
    while (<C>) {
      $clan = $_;
      last;
    }
    close(C);

    my $message;

    #See if we have a default$$ messge to use.
    if ( -s ".default" . $$ . "pfci" ) {
      open( M, ".default" . $$ . "pfci" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for the changes to this family\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFCIRMC:" . $message;
    $$passmessage .= "\nPFCIRMC:" . $clan;

  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFANNLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfci" ) {
      open( M, ".default" . $$ . "pfci" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for changes to family DESC files\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFANN:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFMOVELog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfmove" ) {
      open( M, ".default" . $$ . "pfmove" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      confess "Could not find a message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFMOV:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addPFNEWMOVELog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "pfnewmove" ) {
      open( M, ".default" . $$ . "pfnewmove" )
        or die "Could not open .default" . $$ . "pfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      confess "Could not find a message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "PFNEWMOV:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addCLNEWLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "clnew" ) {
      open( M, ".default" . $$ . "clnew" )
        or die "Could not open .default" . $$ . "clnew";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }
    else {
      die "Failed to open file clnew message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CLNEW:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addCLNEWMOVELog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "clnewmove" ) {
      open( M, ".default" . $$ . "clnewmove" )
        or die "Could not open .default" . $$ . "clnewmove";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      confess "Could not find a message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CLNEWMOV:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addCLNEWACCLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "clnewmove" ) {
      open( M, ".default" . $$ . "clnewmove" )
        or die "Could not open .default" . $$ . "clnewmove";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      confess "Could not find a message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CLNEWACC:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addCLMOVELog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "clmove" ) {
      open( M, ".default" . $$ . "clmove" )
        or die "Could not open .default" . $$ . "clci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      confess "Could not find a message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CLMOV:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addCLKILLLog {
  my ($self) = @_;
  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "clkill" ) {
      open( M, ".default" . $$ . "clkill" )
        or die "Could not open .default" . $$ . "clkill";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }
    else {
      die "No clkill message as to why the clan is being removed from pfam!\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CLKILL:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

sub addAUTOMBLog {
  my ($self) = @_;

  #Here we just need to provide the message;
  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding
    $$passmessage .= "AUTOMB:Automatically added family to clan";
  };

  $self->{txn}->log_msg($commit);

}

sub addAUTORMMBLog {
  my ($self) = @_;

  #Here we just need to provide the message;
  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding
    $$passmessage .= "AUTOMB:Automatically removed family from clan";
  };

  $self->{txn}->log_msg($commit);

}

sub addAUTORMCLLog {
  my ($self) = @_;

  #Here we just need to provide the message;
  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding
    $$passmessage .=
      "AUTORMCL:Automatically removed clan from family DESC file";
  };

  $self->{txn}->log_msg($commit);

}

sub addCLCILog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "clci" ) {
      open( M, ".default" . $$ . "clci" )
        or die "Could not open .default" . $$ . "clci:[$!]\n";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for the changes to this family\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CLCI:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

1;
