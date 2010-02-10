#!/usr/local/bin/perl

# Script to try and set-up Pfam to download and configure users on external machines
# to run Pfam.

use strict;
use warnings;

#-------------------------------------------------------------------------------
# Gobals that are used to thoughout the script that deal with the set-up
my $debug = 1;

#SVN parameters
my $pfamSvnUrl = 'https://pfamsvn.sanger.ac.uk';
my $pfamCode   = '/svn/code/trunk';
my $pfamData   = '/svn/pfam/trunk';
my $pfamseqLoc = '/Data/Sequences';

#SVN modules
my %pfamCodeOrg = (
  'Scripts' => ['PfamScripts'],
  'Modules' => [ 'PfamSchemata', 'PfamLib' ],
  'Conf'    => ['PfamConfig']
);

#Desired executables
my $executables = {
  'ispell' => {
    'Exec' => ['ispell'],
    'Fail' => 0
  },
  'hmmer' => {
    'Exec' => [qw(hmmbuild hmmsearch hmmalign jackhmmer hmmscan)],
    'Fail' => 0
  },
  'easel' => {
    'Exec' => ['esl-sfetch'],
    'Fail' => 0
  },

  'mafft' => {
    'Exec' => ['mafft'],
    'Fail' => 1
  },
  'muscle' => {
    'Exec' => ['muscle'],
    'Fail' => 1
  },
  'clustal' => {
    'Exec' => ['clustalw'],
    'Fail' => 1
  }
};

my $bioPerlVersion = '1.4';    #Not this is the way that is is stored!
open( CONF, ">" . $ENV{HOME} . "/.my.pfam_conf" )
  or die "Could not open " . $ENV{HOME} . "/.my.pfam_conf:[$!]";
close(CONF);

#-------------------------------------------------------------------------------
printBanner();

#-------------------------------------------------------------------------------
#Determining shell
print STDERR "Determining user environment\n";
my ( $shell, $shellConfigFH ) = determineShell();

#-------------------------------------------------------------------------------
print STDERR "Checking external dependencies\n";
requireMods();

#-------------------------------------------------------------------------------
print STDERR "Checking BioPerl\n";
checkBioPerl();

#-------------------------------------------------------------------------------
print STDERR "Checking external software dependencies\n";
checkSoftware();

#-------------------------------------------------------------------------------
print STDERR "Going to get code and sequence database from SVN\n";

#-------------------------------------------------------------------------------
print STDERR "Enter the path where you would like to install Pfam code:";
my $pfamCodeDir = makeDir();

#-------------------------------------------------------------------------------
my $ctx = '';
svnCode( \$ctx, $pfamCodeDir );

print STDERR "$ctx\n";
#-------------------------------------------------------------------------------
# Now sort out the sequence database

print STDERR
"Enter the path where you would like to store the Pfam sequence database, pfamseq (Note, this ~10GB of data)?:";
my $pfamSeqDir = makeDir();

my $checkout = svnSeqs( \$ctx, $pfamSeqDir );

#-------------------------------------------------------------------------------
#Now index the sequence database
my $module = 'pfamseq';
unless ( -e $pfamSeqDir . "/" . $module . "/pfamseq.ssi" and $checkout == 0 ) {
  print STDERR "Indexing the sequence database\n";
  unlink( $pfamSeqDir . "/" . $module . "/pfamseq.ssi" )
    if ( -e $pfamSeqDir . "/" . $module . "/pfamseq.ssi" );

  #Move to the sequence dir
  chdir( $pfamSeqDir . "/" . $module )
    or die "Could not change into $pfamSeqDir/" . $module . ":[$!]\n";

  #Now run sfetch
  system( $executables->{'easel'}->{'Path'} . "/esl-sfetch --index pfamseq" )
    and die "Failed to run esl-sfetch:[$!]\n";
}

#-------------------------------------------------------------------------------
#Write out configuration files - Config::General and shell like one!
my $conf = new Config::General(
  $pfamCodeDir . "/Conf/PfamConfig/PfamRepos/pfam_code.conf" );
my %config = $conf->getall;

print STDERR
  "Please enter a name for your location (WTSI and JFRC are reserved):";
my $loc = ( <STDIN> || 'UNK' );
chomp($loc);
$config{location} = $loc;

print STDERR
  "Please enter a web proxy (if you do not use one, just hit return):";
my $proxy = ( <STDIN> || '' );
chomp($proxy);
$config{proxy} = $proxy;

print STDERR "Please enter the path to store the ispell dictionary:";
my $dicDir = makeDir();
unless ( -e "$dicDir/dictionary" ) {
  print STDERR
"You can get the latest dictionary from Pfam by mailing pfam-help\@sanger.ac.uk\n";
}
$config{dictionary} = $dicDir;

#Now set some other configs from what we have alrady gathered
#hmmer3 location
$config{hmmer3bin} = $executables->{hmmer}->{Path};

#Pfamseq location
$config{pfamseq}->{location}     = $pfamSeqDir . "/pfamseq";
$config{pfamseq}->{farmLocation} = $pfamSeqDir . "/pfamseq";

$conf->save_file( $ENV{HOME} . "/.my.pfam_conf", \%config );

#-------------------------------------------------------------------------------
#Now write out the bits for the shell
my $perlLibs =
    $pfamCodeDir
  . "/Modules/PfamLib:"
  . $pfamCodeDir
  . "/Modules/PfamSchemata:\$PERL5LIB\n";

my $path;
foreach my $bin ( keys(%$executables) ) {
  if ( $executables->{$bin}->{'Path'} ) {
    $path .= $executables->{$bin}->{'Path'} . ":";
  }
}

foreach my $d (qw(make svn search make)) {
  $path .= $pfamCodeDir . "/Scripts/PfamScripts/" . $d . ":";
}

$path .= "\$PATH\n";

if ( $shell eq 'bash' ) {
  print $shellConfigFH "export PATH=" . $path;
  print $shellConfigFH "export PERL5LIB=" . $perlLibs;
  print $shellConfigFH "export PFAM_CONFIG=" . $ENV{HOME} . "/.my.pfam_conf\n";
}
else {
  print $shellConfigFH "setenv PATH " . $path;
  print $shellConfigFH "setenv PERL5LIB " . $perlLibs;
  print $shellConfigFH "setenv PFAM_CONFIG " . $ENV{HOME} . "/.my.pfam_conf\n";
}

#-------------------------------------------------------------------------------
printBanner();
print 'Configuration complete.  Please add the following two lines to your  shell (e.g. ~/.'
  . $shell
  . "rc), then grab a fresh window and start building and editing families.\n";
my $firstBit;
if ( $shell eq 'csh' ) {
  $firstBit = "\nsource ";
}
elsif ( $shell eq 'bash' ) {
  $firstBit = "\n. ";
}
else {
  die
"Something has gone wrong as your should have already been determined .....\n";
}

print "\n\n#PFAM CONFIGURATION $firstBit "
  . $ENV{HOME}
  . "/.pfam."
  . $shell . "\n\n";

#-------------------------------------------------------------------------------
sub makeDir {
  my $location = <STDIN>;
  chomp($location);
  $location =~ s/^.*(\s+)$//g; #removing any trailing white space.
  
  if ( -d $location ) {
    print "$location exists\n";
  }
  else {
    print STDERR "$location does not exist, do you want to make it? [Y|n]:";
    my $r = lc( substr( <STDIN> || 'n', 0, 1 ) );
    if ( $r ne 'y' ) {
      die "Not directory to perform check-outs into\n";
    }
    else {
      my $dirTree;
      foreach my $p ( split( /\//, $location ) ) {
        if ( defined($p) and $p =~ /\S+/ ) {
          $dirTree .= "/$p";
          unless ( -d $dirTree ) {
            mkdir($dirTree) or die "Could not make $dirTree because:$!\n";
          }
        }
        else {
          $dirTree .= "/" if ( !$dirTree );
        }
      }
    }
  }
  return ($location);
}

#-------------------------------------------------------------------------------
sub checkSoftware {

  foreach my $name ( keys %$executables ) {
    foreach my $p ( split( /\:/, $ENV{PATH} ) ) {
      my $t =
        File::Spec::Functions::catfile( $p,
        $executables->{$name}->{'Exec'}->[0] );
      if ( -e $t ) {
        print STDERR "Found $name on path, $p\n" if ($debug);
        $executables->{$name}->{'Path'} = $p;
        last;
      }
    }
    unless ( $executables->{$name}->{'Path'} ) {
      print STDERR "Please enter the path where $name can be found:";
      my $p = <STDIN>;
      chomp($p);
      if (!-d $p and $p =~ /\S+/ ) {
        die "$p is not a directory\n";
      }
      $executables->{$name}->{'Path'} = $p;
    }

    foreach my $e ( @{ $executables->{$name}->{'Exec'} } ) {
      if ( -e $executables->{$name}->{'Path'} . "/" . $e ) {
        print STDERR "Found $e\n" if ($debug);
      }
      else {
        if ( $executables->{$name}->{'Fail'} == 1 ) {
          warn
"Could not find $e, but this is not absolutely necessary, but some programs my fail!\n";
        }
        else {
          die "Can not get Pfam working without $e\n";
        }
      }
    }
  }
}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
=head2 svnCode 

  Title    : svnCode
  Usage    : svnCode(\$scalar, $pathOfNewOrExistingDir) 
  Function : This sets up the SVN client and either upadtes the code base or 
             checks out afresh.
  Args     : scalar reference, path where different nodules are to be checked
           : out into.
  Returns  : Nothing.
  
=cut

sub svnCode {
  my $ctxRef         = shift;
  my $pfamCodeDir = shift;
  
  print STDERR "CTX is ".$ctxRef."\n"; 
  $$ctxRef = new SVN::Client(
    auth => [
      SVN::Client::get_simple_provider(),
      SVN::Client::get_username_provider(),
      SVN::Client::get_ssl_server_trust_prompt_provider(
        \&_ssl_server_trust_prompt
      ),
      SVN::Client::get_ssl_server_trust_prompt_provider(
        \&_ssl_server_trust_prompt
      ),
    ]
  );

  foreach my $dir ( keys %pfamCodeOrg ) {

    #Make the directory to check out in locally
    if ( !-d $pfamCodeDir . "/" . $dir ) {
      mkdir( $pfamCodeDir . "/" . $dir )
        or die "Could not make " . $pfamCodeDir . "/" . $dir . ":$!\n";
    }

    #Now check out the module
    foreach my $module ( @{ $pfamCodeOrg{$dir} } ) {
      if ( -e $pfamCodeDir . "/" . $dir . "/" . $module ) {
        print STDERR "Going to update $module\n" if ($debug);
        $$ctxRef->update( $pfamCodeDir . "/" . $dir . "/" . $module, 'HEAD', 1 );
      }
      else {

        #TODO - inspect if the module is already present, then update.
        print STDERR "Going to check out $module\n" if ($debug);
        $$ctxRef->checkout(
          $pfamSvnUrl . $pfamCode . "/" . $module,
          $pfamCodeDir . "/" . $dir . "/" . $module,
          'HEAD', 1
        );
      }
    }
  }
}

#-------------------------------------------------------------------------------
sub svnSeqs {
  my ( $ctxRef, $pfamSeqDir ) = @_;
  my $module   = 'pfamseq';
  my $checkout = 0;

  if ( -e $pfamSeqDir . "/" . $module ) {
    print STDERR
"Going to update $module, and if out of date, will take some time to run\n"
      if ($debug);

    my $function = sub {
      my ( $path, $status ) = @_;
      my $actual_status = $status->text_status();    # svn_wc_status_kind enum
      open( S, "/tmp/$$.status" ) or die "Could not open /tmp/$$.status:[$!]\n";
      print S "Status: |$actual_status|\n";
      close(S);
    };

    $$ctxRef->status( $pfamSeqDir . "/" . $module . "/pfamseq",
      'HEAD', $function, 1, 0, 1, 0, );

    if ( -s "/tmp/$$.status" ) {
      print STDERR
"It appears that you have a copy of pfamseq, but it is not in sync with the repository\n";
      print STDERR
"Due to the size of pfamseq, it is far better to delete the directory and check it out"
        . "afresh than to perform an update on the file.  This generates huge delta that will potential"
        . "break things.  Are you happy that the directory  that  "
        . $pfamSeqDir . "/"
        . $module
        . "is removed? [Y|n]\n";
      my $response = lc( substr( <STDIN> || 'N', 0, 1 ) );
      if ( $response eq 'n' ) {
        print STDERR "Okay, going to try an update.....\n";
        $$ctxRef->update( $pfamSeqDir . "/" . $module, 'HEAD', 1 );
        $checkout = 0;
      }
      else {
        $checkout = 1;

      #Clear out pfamseq and all the other cruft that comes with an SVN checkout
        File::Path::remove_tree( $pfamSeqDir, { keep_root => 1 } );
      }
    }
    else {

      #Pfamseq appears to be upto date
      $checkout = 0;
    }
  }
  else {
    $checkout = 1;
  }

  if ($checkout) {

    #We have decided that we need a new checkout.
    print STDERR "Going to check out $module, this will take some time\n"
      if ($debug);
    $$ctxRef->checkout(
      $pfamSvnUrl . $pfamData . $pfamseqLoc . "/" . $module,
      $pfamSeqDir . "/" . $module,
      'HEAD', 1
    );
  }
  return ($checkout);
}

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
sub determineShell {

  #Determine in the user is using a Bourne-like or C-like shell.
  my ($s) = $ENV{SHELL} =~ m|.*/(\S+)$|;
  my $shell;
  if ( $s eq 'bash'
    or $s eq 'sh'
    or $s eq 'dash'
    or $s eq 'ash'
    or $s eq 'ksh'
    or $s eq 'zsh' )
  {
    print STDERR "Found Bourne-like shell: " . $ENV{SHELL} . "\n" if ($debug);
    $shell = 'bash';
  }
  elsif ( $s eq 'csh' or $s eq 'tcsh' ) {
    print STDERR "Found C-like shell: " . $ENV{SHELL} . "\n" if ($debug);
    $shell = 'csh';
  }
  else {
    die "Could not determine shell!";

    #TODO - get user to override before dying!
  }

  #Now open a file for writing Pfam specific shell config into.
  #TODO - check that is does not exist.
  open( my $shellConfigFH, ">" . $ENV{HOME} . "/.pfam." . $shell )
    or die "Could not open "
    . $ENV{HOME}
    . "/.pfam."
    . $shell
    . " for writing:$!\n";

  return ( $shell, $shellConfigFH );
}

#-------------------------------------------------------------------------------
sub checkBioPerl {

  my $error = 0;

  foreach my $mod (
    qw( Bio::Root::Root
    Bio::Root::Version;
    Bio::SimpleAlign
    Bio::LocatableSeq
    Bio::Seq)
    )
  {
    print STDERR "Checking bioperl module |$mod|\n" if ($debug);

    unless ( eval "require $mod" ) {

      print "Please install $mod: $@\n";
      $error++;
    }
  }

  if ( $error == 0 ) {
    my $found = substr( $Bio::Root::Version::VERSION, 0, 5 );
    $found =~ s/00//g;

    unless ( $bioPerlVersion eq $found ) {
      print "$found\n";
      print STDERR
        "Wrong version of bioperl found - wanted a flavour of $bioPerlVersion, "
        . "but found "
        . $found . "\n";
      $error++;
    }
  }

  if ($error) {
    $bioPerlVersion =~ s/00//g;
    die
"The version of bioperl required is $bioPerlVersion, see http://bio.perl.org/wiki/Main_Page"
      . " for more information and check that the directory contiaing the root Bio directory is on your PERL5LIB"
      . " and before any other versions you may have.";
  }
}

#-------------------------------------------------------------------------------

=head2 subname 

  Title    :
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub requireMods {
  my $error = 0;

  foreach my $mod (
    qw(
    AutoLoader
    Carp::Clan
    Carp::Heavy
    Class::Accessor
    Class::Accessor::Chained::Fast
    Class::Accessor::Fast
    Class::Accessor::Grouped
    Class::C3
    Class::C3::Componentised
    Class::C3::XS
    Class::Inspector
    Class::MOP
    Class::MOP::Attribute
    Class::MOP::Class
    Class::MOP::Class::Immutable::Trait
    Class::MOP::Instance
    Class::MOP::Method
    Class::MOP::Method::Accessor
    Class::MOP::Method::Constructor
    Class::MOP::Method::Generated
    Class::MOP::Method::Inlined
    Class::MOP::Method::Wrapped
    Class::MOP::Module
    Class::MOP::Object
    Class::MOP::Package
    Class::Struct
    Compress::Raw::Zlib
    Compress::Zlib
    Config
    Config::General
    Cwd
    DBIx::Class
    DBIx::Class::Componentised
    DBIx::Class::Core
    DBIx::Class::Exception
    DBIx::Class::InflateColumn
    DBIx::Class::PK
    DBIx::Class::PK::Auto
    DBIx::Class::Relationship
    DBIx::Class::Relationship::Accessor
    DBIx::Class::Relationship::Base
    DBIx::Class::Relationship::BelongsTo
    DBIx::Class::Relationship::CascadeActions
    DBIx::Class::Relationship::HasMany
    DBIx::Class::Relationship::HasOne
    DBIx::Class::Relationship::Helpers
    DBIx::Class::Relationship::ManyToMany
    DBIx::Class::Relationship::ProxyMethods
    DBIx::Class::ResultSet
    DBIx::Class::ResultSetColumn
    DBIx::Class::ResultSource
    DBIx::Class::ResultSource::Table
    DBIx::Class::ResultSourceHandle
    DBIx::Class::ResultSourceProxy
    DBIx::Class::ResultSourceProxy::Table
    DBIx::Class::Row
    DBIx::Class::Schema
    DBIx::Class::StartupCheck
    Data::Dump
    Data::Dumper
    Data::OptList
    Data::Page
    Data::UUID
    Devel::GlobalDestruction
    DynaLoader
    English
    Errno
    Error
    Exporter::Heavy
    Fcntl
    File::Basename
    File::Copy
    File::Find
    File::Glob
    File::GlobMapper
    File::Path
    File::Rsync
    File::Rsync::Config
    File::Spec
    File::Spec::Functions
    File::Spec::Unix
    File::Temp
    File::stat
    FileHandle
    Getopt::Long
    HTTP::Date
    HTTP::Headers
    HTTP::Message
    HTTP::Request
    HTTP::Response
    HTTP::Status
    IO
    IO::Compress::Adapter::Deflate
    IO::Compress::Base
    IO::Compress::Base::Common
    IO::Compress::Gzip
    IO::Compress::Gzip::Constants
    IO::Compress::RawDeflate
    IO::Compress::Zlib::Extra
    IO::File
    IO::Handle
    IO::Seekable
    IO::Select
    IO::Uncompress::Adapter::Inflate
    IO::Uncompress::Base
    IO::Uncompress::Gunzip
    IO::Uncompress::RawInflate
    IPC::Open3
    IPC::Run
    IPC::Run::Debug
    IPC::Run::IO
    IPC::Run::Timer
    JSON
    JSON::XS
    LWP
    LWP::Debug
    LWP::MemberMixin
    LWP::Protocol
    LWP::Simple
    LWP::UserAgent
    List::MoreUtils
    List::Util
    Log::Log4perl
    MRO::Compat
    Module::Find
    Moose
    Moose::Error::Default
    Moose::Exporter
    Moose::Meta::Class
    Moose::Meta::Class::Immutable::Trait
    Moose::Meta::Instance
    Moose::Meta::Method
    Moose::Meta::Method::Accessor
    Moose::Meta::Method::Augmented
    Moose::Meta::Method::Constructor
    Moose::Meta::Method::Delegation
    Moose::Meta::Method::Destructor
    Moose::Meta::Method::Overridden
    Moose::Meta::Role
    Moose::Meta::Role::Application
    Moose::Meta::Role::Application::RoleSummation
    Moose::Meta::Role::Application::ToClass
    Moose::Meta::Role::Application::ToInstance
    Moose::Meta::Role::Application::ToRole
    Moose::Meta::Role::Composite
    Moose::Meta::Role::Method
    Moose::Meta::Role::Method::Conflicting
    Moose::Meta::Role::Method::Required
    Moose::Meta::TypeCoercion
    Moose::Meta::TypeCoercion::Union
    Moose::Meta::TypeConstraint
    Moose::Object
    Moose::Util
    Moose::Util::MetaRole
    Moose::Util::TypeConstraints
    Moose::Util::TypeConstraints::OptimizedConstraints
    POSIX
    Params::Util
    SVN::Client
    SVN::Core
    SVN::Wc
    Scalar::Util
    Scope::Guard
    SelectSaver
    Storable
    Sub::Exporter
    Sub::Install
    Sub::Name
    Symbol
    Sys::Hostname
    Term::ReadLine
    Term::ReadPassword
    Text::Tabs
    Text::Wrap
    Time::HiRes
    Time::Local
    URI
    URI::Escape
    XSLoader
    )
    )
  {
    print STDERR "Checking |$mod|\n" if ($debug);

    unless ( eval "require $mod" ) {

      print "Please install $mod using CPAN: $@\n";
      $error++;
    }
  }
  if ($error) {
    die "Not all Perl modules are installed!"
      . "please install and re-run config or check you PERL5LIB\n";
  }
  else {
    "Your Perl looks good!\n";
  }
}

#-------------------------------------------------------------------------------
sub printBanner {

  print '
      __________   _____                   _____________   ___________   
      \______   \_/ ____\____    _____    /   _____/\   \ /   /\      \  
       |     ___/\   __\\__  \  /     \   \_____  \  \   Y   / /   |   \ 
       |    |     |  |   / __ \|  Y Y  \  /        \  \     / /    |    \
       |____|     |__|  (____  /__|_|  / /_______  /   \___/  \____|__  /
                             \/      \/          \/                   \/ 
                        _________       __                
                       /   _____/ _____/  |_ __ ________  
                       \_____  \_/ __ \   __\  |  \____ \ 
                       /        \  ___/|  | |  |  /  |_> >
                      /_______  /\___  >__| |____/|   __/ 
                              \/     \/           |__|    
'

}

