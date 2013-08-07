=head1 NAME

Bio::Rfam::SVN::Client - a module that enables access to the Rfam SVN

=cut

package Bio::Rfam::SVN::Client;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: SVN.pm

Copyright (c) 2013: 

Author: Rob Finn (rdf@ or finnr@)

=cut

use strict;
use warnings;
use Cwd;
use Carp;
use Term::ReadPassword;
use SVN::Client;
use File::Copy;
use File::Temp;

use Bio::Rfam::Config;
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
  #I have made it to mask the typing in of a password.

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

  #Code reference that allows the checking that a file is present.
  $self->{checkFile} = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      confess("$path is invalid");
    }
  };

  $self->{config} = Bio::Rfam::Config->new;
  return ( bless( $self, $class ) );
}

#Authentication call back methods!

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

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
=head2 checkFamilyExists

  Title    : checkFamilyExists
  Incept   : finnr, Jan 24, 2013 3:19:25 PM
  Usage    : $client->checkFamilyExists($rfam_acc);
  Function : Checks that a family is present in the repository
  Args     : A Rfam accession
  Returns  : Nothing
  
=cut

sub checkFamilyExists {
  my ( $self, $family) = @_;

  my $url     = $self->familyLocation . "/" . $family;
  
  #Set up the call back code reference
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      die "$path is invalid\n";
    }
  };
  
  #Now see if that famuly is present!
  my $revision = $self->revision;
  eval {
     $self->{txn}->info( $url, undef, $revision, $codeRef, 0 ); 
  };

  if ($@) {
    #Check to see if the family has been killed, if so give details and exit
    if ( $self->{config}->location eq 'EBI' ) {
      my $rfamDB = $self->config->rfamlive;

      my @dead =
        $rfamDB->resultset("DeadModel")->search( { rfam_acc=> $family } );
      foreach my $dead (@dead) {

        print "$family "
          ." was killed by "
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
  return 1;
}

#------------------------------------------------------------------------------
=head2 checkNewFamilyDoesNotExist

  Title    : checkNewFamilyDoesNotExists
  Incept   : finnr, Jan 24, 2013 3:23:56 PM
  Usage    : $client->checkNewFamilyDoesNotExist($name)
  Function : Tests that a directory is not already present in the PendingFamilies
           : location.
  Args     : Name of the directory to be tested.
  Returns  : Nothing
  
=cut

sub checkNewFamilyDoesNotExist {
  my ( $self, $entry ) = @_;

  my $url     = $self->newFamilyLocation . "/" . $entry;
  my $codeRef = sub {
    my ( $path, $info, $pool ) = @_;
    unless ($info) {
      die "$path is invalid\n";
    }
  };

  #This should throw a warning!
  eval { $self->{txn}->info( $url, undef, 'HEAD', $codeRef, 0 ); };

  unless ($@) {
    confess( "$entry exist in the respository, at $url.  "
        . "This should not happen, as the new family should be moved into "
        . "the repoistory shortly after being added.  Please try again in a "
        . "few minutes, if the problem persists, then something is wrong!\n" );
  }
}

#------------------------------------------------------------------------------
=head2 checkFamilyDoesNotExist

  Title    : checkFamilyDoesNotExists
  Incept   : finnr, Jan 24, 2013 3:43:26 PM
  Usage    : $client->checkFamilyDoesNotExist($name)
  Function : Tests that a directory is not already present in the Families
           : location.
  Args     : Name of the directory to be tested.
  Returns  : Nothing
  
=cut

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

#------------------------------------------------------------------------------
=head2 

  Title    :
  Incept   : finnr, Jan 24, 2013 5:50:30 PM
  Usage    : 
  Function : 
  Args     : 
  Returns  : 
  
=cut

sub checkAllFamilyFiles {
  my ( $self, $family ) = @_;

  my $url = $self->familyLocation . "/" . $family;

  #Todo Change this for a call
  foreach my $file ( keys %{ $self->{config}->{files}->{family} } ) {
    eval {

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

=head2 checkoutFamily

  Title    : checkoutFamily
  Usage    : $client->checkoutFamily('RF00001', '/path/to/local/checkout');
  Function : Checks out a family to the specified directory
  Args     : family accession, path to checkout directory
  Returns  : Nothing
  
=cut

sub checkoutFamily {
  my ( $self, $entry, $dest ) = @_;
  
  #Work out the repo path for the family
  my $url = $self->familyLocation . "/" . $entry;
  
  #try and check it out
  eval {
    $self->{txn}
      ->checkout( $url, $dest, $self->revision ? $self->revision : 'HEAD', 1 );
  };

  #Confess if we observe a proble,
  if ($@) {
    confess("Failed to check out family, ".$self->modelLocation."/$entry, :[$@]\n");
  }
}

=head2 checkoutAllFamilies

  Title    : checkoutAllFamilies
  Usage    : $client->checkoutAllFamilies($path)
  Function : Check out all families in the repository
  Args     : A destination path
  Returns  : Nothing
  
=cut

sub checkoutAllFamilies {
  my ( $self, $dest ) = @_;
  my $url = $self->familyLocation;

  my $revision = $self->{config}->revision;
  
  eval {
    $self->{txn}
      ->checkout( $url, $dest, $revision, 1 );
  };

  if ($@) {
    confess("Failed to check out all families:[$!]\n");
  }
}

=head2 

  Title    : 
  Usage    :   
  Function : 
  Args     : 
  Returns  : 
  
=cut

sub catFile {
  my ( $self, $dir, $filename, $fh, $rev ) = @_;

  my $url =  $self->familyLocation . "/" . $dir;
  my $revision = $self->revision;
  
  unless ($fh) {
    $fh = \*STDOUT;
  }
  $self->{txn}->cat(
    $fh,
    $url . "/" . $filename,
    defined($rev) ? $rev : ( $revision )
  );
}

sub log {
  my ( $self, $dir, $rev ) = @_;
  my $url = $self->familyLocation . "/" . $dir;
 
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

=head2 commitFamily

  Title    : 
  Usage    :   
  Function : 
  Args     : 
  Returns  : 
  
=cut

sub commitFamily {
  my ( $self, $entry ) = @_;

 my @files;
 push( @files, $entry );
 foreach my $file ( @{ $self->{config}->mandatoryFiles } ) {
    push( @files, "$entry/$file" );
  }
  
  #And finally commit them.
  my $cinfo;
  eval { $cinfo = $self->{txn}->commit( \@files, 1 ); };

  if ($@) {
    confess("Failed to commit family, $entry: [$@]\n");
  }
  #Now check that something happen!
  $self->_checkCommitObj($cinfo);

}


sub commitFamilyDESC {
  my ( $self, $family ) = @_;

  #And finally commit them.
  my $cinfo;
  eval { $cinfo = $self->{txn}->commit( $family."/DESC", 1 ); };

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
  my $dest = $dir . "/ModelsPending";

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

  foreach my $file ( @{ $self->{config}->mandatoryFiles } ) {
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
  my @files;
  push( @files, "$dest/$newFamilyId" );
  foreach my $file ( @{ $self->{config}->mandatoryFiles } ) {
    push( @files, "$dest/$newFamilyId/$file" );
  }
  

  eval { $cinfo = $self->{txn}->commit( \@files, 1 ); };

  if ($@) {
    confess("\n*** Failed to commit new family to $newFamilyId ***\n\n[$@]\n");
  }
  
  #Now check that something happen!
  $self->_checkCommitObj($cinfo);
}

=head2 update

  Title    : update
  Usage    : $client->update($family);
  Function : Updates all of the mandiatory files for a family
  Args     : Path to a working copy of a family
  Returns  : Nothing
  
=cut

sub update {
  my ( $self, $family ) = @_;

  my $revision = $self->revision;
  #Okay, for each of the manditory files, run an SVN update.
  foreach my $file ( @{ $self->{config}->mandatoryFiles } ) {
    $self->{txn}
      ->update( $family . "/" . $file, $revision, 1 );
  }

}

=head2 killFamily 

  Title    : killFamily
  Usage    : $client->killFamily($rfam_acc)
  Function : Delete a family from the repository
  Args     : Rfam accession
  Returns  : Nothing
  
=cut

sub killFamily {
  my ( $self, $family ) = @_;

  #Okay, get the URL of the families location
  my $url;
  if ( $family =~ /RF\d{5}/ ) {

    #Looks like a Dfam entry
    $url = $self->modelLocation . "/" . $family;
  }else{
    confess("\n*** $family does not look like a Rfam accession. ***\n\n");
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

=head2 moveNewFamily

  Title    : moveNewFamily
  Usage    : $client->moveNewFamily('initialName', 'RFXXXXX', $rev)
  Function : Moves a newly commit family from the PendingFamilies dir to the Families
           : dir. It then checks the return svn_commit_info_object.
  Args     : The name of the directory to move, the newly assigned rfam accession, 
           : a revision (optional).
  Returns  : Nothing.
  
=cut

sub moveNewFamily {
  my ( $self, $familyOld, $familyNew, $rev ) = @_;

  my $oldUrl = $self->newModelLocation . "/" . $familyOld;
  my $newUrl = $self->modelLocation . "/" . $familyNew;

  my $cinfo;
  eval {
    $cinfo =
      $self->{txn}->move( $oldUrl,
      defined($rev) ? $rev : ( $self->revision ? $self->revision : 'HEAD' ),
      $newUrl, 0 );
  };

  #Catch any error and report.
  if ($@) {
    die "\n*** MAJOR ERROR during post commit for rfnew! FAILURE***\n\n$@";
  }

  $self->_checkCommitObj($cinfo);
}

# Priavte methods that should not be used from outside this module
#-------------------------------------------------------------------------------

=head2 _checkCommitObj

  Title    : _checkCommitObj
  Usage    : $client->_checkCommitObj
  Function : When we have done something to the repository, check that it really
           : has happend. If nothing has happned then the revision is not set
           : on the item object. 
  Args     : A svn_client_commit_info_t object.
  Returns  : Nothing
  
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

=head2 _checkFile 

  Title    : _checkFile
  Usage    : $client->_checkFile
  Function : Internal accessor to the code references that checks that a file
           : in the repository exists.
  Args     : None
  Returns  : code reference
  
=cut

sub _checkFile {
  my ($self) = @_;
  return ( $self->{checkFile} );
}


#--------------------------------
#These could go in the config

sub revision {
  my ($self) = @_;
  my $revision = $self->{config}->svnRevision ? $self->{config}->svnRevision : 'HEAD';
  return ( $revision );
}

=head2 familyLocation

  Title    : familyLocation
  Usage    : $client->familyLocation
  Function : Returns the path in the repository where families are stored.
  Args     : None
  Returns  : String that is the URL of the families.
  
=cut


sub familyLocation {
  my ($self) = @_;
  
  my $familyUrl;
  #As config URLs are inconsistent with trailing /, see if it is there.
  if($self->{config}->svnRepos =~ m|.*/$|){
    $familyUrl = $self->{config}->svnRepos . $self->{config}->svnFamilies;
  }else{
    $familyUrl = $self->{config}->svnRepos .'/'. $self->{config}->svnFamilies;
  }
  return ($familyUrl);
}

=head2 newFamilyLocation

  Title    : newFamilyLocation
  Usage    : $client->newFamilyLocation
  Function : Returns the path in the repository where new families are initially added.
  Args     : None
  Returns  : String that is the URL of the new families.
  
=cut

sub newFamilyLocation {
  my ($self) = @_;
  my $newFamilyUrl =
    $self->{config}->svnRepos . $self->{config}->svnNewFamilies;
  return ($newFamilyUrl);
}

# Below here are all of the log message callbacks. There is a certain amount
# of repetition due tot he fact they are usig callbacks and therefore are
# bespoke code refs depending on the log. The prefixes are really important
# for triggering svn hooks, so be careful!
#-------------------------------------------------------------------------------

=head2 addRFNEWLog

  Title    : addRFNEWLog
  Usage    : $client->addRFNEWLog
  Function : Internal call back method for setting the message for the
           : svn history when a new family is added to the repository.
  Args     : None
  Returns  : Nothing
  
=cut

sub addRFNEWLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "rfnew" ) {
      open( M, ".default" . $$ . "rfnew" )
        or die "Could not open .default" . $$ . "rfnew";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }
    else {
      die "Failed to open file rfnew message\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= "NEW:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

=head2 addRFCILog

  Title    : addRFCILog
  Usage    : $client->addRFCILog();
  Function : Internal call back method for setting the check-in message for the
           : svn history.
  Args     : None
  Returns  : Nothing
  
=cut

sub addRFCILog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "rfci" ) {
      open( M, ".default" . $$ . "rfci" )
        or die "Could not open .default" . $$ . "rfci";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }

    #Else ask for a message
    if ( !defined $message ) {
      print "Please give a comment for the changes to this entry\n";
      print "Finish comment by a . on the line by itself\n";
      while (<STDIN>) {
        chomp;
        /^\s*\.\s*$/ && last;
        $message .= "$_\n";
      }
    }

    #Now add the message to the scalar ref
    $$passmessage .= "CI:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

=head2 addRFKILLLog

  Title    : addRFKILLLog
  Usage    : $client->addRFKILLLog();
  Function : Internal call back method for setting the message for the
           : svn history when killing a family.
  Args     : None
  Returns  : Nothing
  
=cut

sub addRFKILLLog {
  my ($self) = @_;
  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "rfkill" ) {
      open( M, ".default" . $$ . "rfkill" )
        or die "Could not open .default" . $$ . "rfkill";
      while (<M>) {
        $message .= $_;
      }
      close(M);
    }
    else {
      die
        "No rfkill message as to why the entry is being removed from Rfam!\n";
    }

    #Now add the message to the scalar ref
    $$passmessage .= $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

=head2 addRFANNLog

  Title    : addRFANNLog
  Usage    : $client->addRFANNLog
  Function : Internal call back method for setting the message for the
           : svn history when a family DESC file is updated.
  Args     : None
  Returns  : Nothing
  
=cut

sub addRFANNLog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "rfci" ) {
      open( M, ".default" . $$ . "rfci" )
        or die "Could not open .default" . $$ . "rfci";
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
    $$passmessage .= "ANN:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

=head2 addRFMOVELog

  Title    : addRFMOVELog
  Usage    : $client->addRFMOVELog
  Function : Internal call back method for setting the message for the
           : svn history when a new family is moved, i.e. ID changed.
  Args     : None
  Returns  : Nothing
  
=cut

sub addRFMOVELog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "rfmove" ) {
      open( M, ".default" . $$ . "rfmove" )
        or die "Could not open .default" . $$ . "rfmove";
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
    $$passmessage .= "MOV:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

=head2 addRFNEWMOVELog

  Title    : addRFNEWMOVELog
  Usage    : $client->addRFNEWMOVELog
  Function : Internal call back method for setting the message for the
           : svn history when a new family is moved from Pending to Families dir.
  Args     : None
  Returns  : Nothing
  
=cut

sub addRFNEWMOVELog {
  my ($self) = @_;

  my $commit = sub {
    my $passmessage = shift;    #Scalar reference passed by svn binding

    my $message;

    #See if we have a default messge to use.
    if ( -s ".default" . $$ . "rfnewmove" ) {
      open( M, ".default" . $$ . "rfnewmove" )
        or die "Could not open .default" . $$ . "rfnewmove";
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
    $$passmessage .= "NEWMOV:" . $message;
  };

  #Add the commit sub reference
  $self->{txn}->log_msg($commit);
}

1;
