
# Build.PL
# jt6 20070208 WTSI.
#
# The main build script for the PfamWeb installation
#
# $Id: Build.PL,v 1.4 2007-02-27 17:06:05 jt6 Exp $

use strict;
use warnings;

use lib qw( inc );
use PfamWeb::Builder;

use File::Path;
use File::Copy;
use Cwd 'realpath';

use Data::Dump qw(dump);

# this is the default location for all of the configuration files in the
# distribution
my $configDir  = "conf";

# this is the default location for the main configuration file
my $configFile = "$configDir/pfamweb.jcf";

# and for the apache config file
my $httpdFile = "$configDir/httpd.jcf";

#-------------------------------------------------------------------------------
#- setup Module::Build ---------------------------------------------------------
#-------------------------------------------------------------------------------

# configure Module::Build (or rather our subclass of it) itself

my $build = PfamWeb::Builder
  ->new(
    # these are modules that are required for the build process, as opposed
    # to just the normal operation of the site
    build_requires  => {
                          "Config::General" => 0.28,
                          "DBI"             => 0,
                       },
    
    dist_author     => 'John Tate <jt6@sanger.ac.uk>',

		# define the command line options for the installation
		get_options     => {
                   		    "accept_defaults" => { type    => "!"  },
                   		    "config_file"     => { type    => ":s" },
                     	    "help|usage"      => { type    => "!"  },
                     	    "no_help"         => { type    => "!",
                                         	       default => 0    },
                     	    "verbose_log"     => { type    => "+",
                                         	       default => 0    },
                     	 },

		license         => "perl",
		module_name     => "PfamWeb",

		# recommended but not essential prerequisites. Since there
		# doesn't seem to be any sensible way to detect BioPerl if
		# it's not installed as a Bundle, it's recommended rather than
		# required
		recommends      => {
                    		  "Bundle::BioPerl"                            => 0,
                    		  "Catalyst::Devel"                            => 1.0,
                    		  "Module::Build"                              => 0.28,
                    	 },

		# these are the absolutely required prerequisites
		requires        => {
            						  "Bio::Das::Lite"                             => 1.47,
            						  "Catalyst::Runtime"                          => 5.7,
            						  "Catalyst::Action::RenderView"               => 0,
            						  "Catalyst::Model::DBIC::Schema"              => 0,
            						  "Catalyst::Plugin::Cache::FastMmap"          => 0,
            						  "Catalyst::Plugin::Email"                    => 0,
            						  "Catalyst::Plugin::HTML::Widget"             => 0,
            						  "Catalyst::Plugin::Session::State::Cookie"   => 0,
            						  "Catalyst::Plugin::Session::Store::FastMmap" => 0,
            						  "Class::Data::Accessor"                      => 0,
            						  "Config::General"                            => 0.28,
            						  "DateTime"                                   => 0,
            						  "DBIx::Class"                                => 0,
            						  "DBIx::Class::HTMLWidget.pm"                 => 0,
            						  "GD"                                         => 0,
            						  "GD::Graph"                                  => 0,
            						  "HTML::Tree"                                 => 0,
            						  "HTML::Widget"                               => 0,
            						  "JSON"                                       => 0,
            						  "LWP::Parallel::UserAgent.pm"                => 0,
            						  "SOAP::Data::Builder"                        => 0,
            						  "XML::Feed"                                  => 0,
          						 },

		# for what they're worth...
		test_files      => [ glob('t/*.t') ],
	);

#--------------------------------------

# add our extra file types (such as "*.conf") to the list of file
# types that get copied into blib and then installed alongside
# everything else.
#
# Because of the way that MB works, these are actually directories
# that will be processed using methods in PfamBuilder. The methods are
# responsible for knowing what to do with the files in each directory.

$build->add_build_element( "conf" );
$build->add_build_element( "data" );
$build->add_build_element( "htdocs" );

#--------------------------------------

# set up the directory structure for the install tree. If we don't set
# these paths specifically, MB will install everything as if it were a
# regular CPAN module, which breaks the catalyst app pretty terminally

#$build->install_base();

$build->install_base_relpaths( "lib"    => "lib" );
$build->install_base_relpaths( "conf"   => "conf" );
$build->install_base_relpaths( "data"   => "data" );
$build->install_base_relpaths( "htdocs" => "htdocs" );

#--------------------------------------

# see if we should use the whatever defaults we find
$ENV{PERL_MM_USE_DEFAULT} = 1 if $build->args( "accept_defaults" );

#--------------------------------------

# should we just provide some help ?
if( $build->args( "help" ) or $build->args( "usage" ) ) {
  usage($build);
  exit;
}

# or should we just shut up and not print any help messages between prompts ?
my $quiet = $build->args( "no_help" );

#-------------------------------------------------------------------------------
#- start the configuration -----------------------------------------------------
#-------------------------------------------------------------------------------

# a little hello... (ASCII-art courtesy of http://www.network-science.de/ascii/)
print <<'EOFloc' unless $quiet;
 ____   __              __        __   _     
|  _ \ / _| __ _ _ __ __\ \      / /__| |__  
| |_) | |_ / _` | '_ ` _ \ \ /\ / / _ \ '_ \ 
|  __/|  _| (_| | | | | | \ V  V /  __/ |_) |
|_|   |_|  \__,_|_| |_| |_|\_/\_/ \___|_.__/ 

This is the installation and configuration script for the Pfam website application.
It will ask a series of questions and, based on your responses, generate a script
that can be used to configure and then install the website.

You can see some help on using it and the accepted command-line options by running:

  perl Build.PL --help

You will need to make sure that you are using a perl executable which can see the
various prerequisite modules that you should have installed (such as
Catalyst::Runtime). Alternatively you can set the PERL5LIB environment variable to
add the appropriate library directories to @INC before you start.

Initially, the default values for this script are taken from the configuration file
in this distribution. You can override the defaults by changing the values at the
various prompts when you run this script, or by editing the configuration files
directly. If you edit the files you can point the script at the modified files and
tell it to accept all of the values in them by doing:

  perl Build.PL --config_file=../my_pfamweb.conf --accept_defaults

Unless you specify "--accept_defaults", the script will prompt you for the various
parameters that we need, offering default values from the configuration file that
it reads.

EOFloc

$build->prompt( "Hit return when you're ready to start.", "" ) unless $quiet;

#--------------------------------------

# see if Config::General is installed and, if not, print a helpful message to
# explain why we can't go any further

if( $build->check_installed_version( "Config::General" ) ) {

  eval {
    require Config::General;
  };
  if( $@ ) {
    die "(EE) ERROR: couldn't load Config::General, although Module::Build thinks it's installed: $@";
  }
    
} else {
 
  print <<EOFcgMissing;

(EE) ERROR: module Config::General is not installed

The PfamWeb application is configured using Apache-style configuration files,
which are parsed using the Config::General perl module. Because this
installation reads and re-writes the configuration files that are supplied
in the distribution, we need to have Config::General installed before you can
run this script.

Please install Config::General and try again.

EOFcgMissing
 
  exit 1;
}

# we're going to load the configuration file and use it to generate the default
# values for the prompts. First we need to find the main config file

# if the user specified the value on the command line, we'll use that
if( $build->args( "config_file" ) ) {
  $configFile = $build->args( "config_file" );
  print "\nWe're taking default values from the file \"$configFile\", as specified.\n";
} else {
  $configFile = $build->prompt( "Where is the default configuration file ?",
                                $configFile );
}

# make sure we can read it and then parse it in 
die "(EE) ERROR: couldn't read configuration file \"$configFile\": $!"
  unless -f $configFile;  

my( $configCG, %config, $parsed );
eval {
  # throws an exception if there's a real parsing error
  $configCG = new Config::General( -ConfigFile      => $configFile,
                                   -IncludeRelative => 1 );

  # otherwise we should get a Config::General module...
  %config = $configCG->getall;

  # but we still need some sanity checks, since CG seems happy to parse any
  # old junk and turn it into a hash. Let's look for some bits of the config
  # that really should be there...
  $parsed = defined $config{name} and
            exists $config{Controller}->{Annotate}->{annotationEmail} and
            exists $config{View}->{TT}->{VARIABLES}->{blocks};

};

if( $@ or not $parsed ) {
  print STDERR <<EOFcgError;

(EE) ERROR: there was a problem parsing configuration file \"$configFile\".
            Please check that the file you specified is valid: $@

EOFcgError
  exit 1;
}

#-------------------------------------------------------------------------------
#- configure pfamweb.conf ------------------------------------------------------
#-------------------------------------------------------------------------------

# where should we put everything ?

print <<EOFloc unless $quiet;

--------------------------------------------------------------------------------

The PfamWeb application consists of several different types of file. We have
raw perl code, Template Toolkit template files, static HTML content, such as 
CSS files and images, and finally configuration files. 

We will install all of these different files under a single directory, the 
installation root. For example, you might choose to put PfamWeb in a
subdirectory of your web-server, e.g. /usr/local/httpd/PfamWeb, or somewhere
entirely separate, e.g. /opt/webapps/PfamWeb.  

EOFloc

# store the starting value of the installation root
$build->notes( "defaultInstallRoot", $config{installRoot} );

my( $validResponse, $rawResponse );

# loop until we get a response that we like...
until( $validResponse ) {
  $rawResponse = 
    $build->prompt( "In what directory do you want to install the PfamWeb application ?",
                    $config{installRoot} );
  
  # trim off trailing slashes
  $rawResponse =~ s/(.*?)\/*$/$1/;

  if( -d $rawResponse ) {
    $validResponse= $build->y_n( "(WW) WARNING: that directory already exists; do you want to continue ? (Y/n)", "Y" );
  } elsif( -f $1 ) {
    $validResponse= $build->y_n( "(WW) WARNING: a file with that name already exists; do you want to continue ? (y/N)", "N" );
  } else {
    $validResponse = 1;
  }

}

# store the value directly in our config hash...
$config{installRoot} = $rawResponse;

# ... set the install_base...
$build->install_base( $rawResponse );

# and set the INCLUDE_PATH value in the View::TT config
$config{View}->{TT}->{INCLUDE_PATH} = $rawResponse . "/data/templates";

print <<EOFloc unless $quiet;

OK. We will install everything there and you will end up with a directory
structure that looks something like this:

EOFloc

unless( $quiet ) {
  foreach my $dir ( qw[ conf data htdocs lib ] ) {
    print "  $config{installRoot}/$dir\n";
  }
}

#--------------------------------------

# we'll store the starting value for the application root as a note, so 
# that we can do a search and replace for it at the Build stage
$build->notes( "defaultAppRoot", $config{View}->{TT}->{CONSTANTS}->{root} );
  
print <<EOFloc unless $quiet;

--------------------------------------------------------------------------------

Now we need to decide the root of the application as viewed in a browser.

When the application is running, users will access it with a URL something
like:

  http://<your site>/pfam

The path component of the URL (in this case "/pfam") is the application root
and we will need to set that in various files during this installation. Note
that the application root should start with a slash but should not have a 
trailing slash.

EOFloc

$rawResponse = 
  $build->prompt( "What is the application root for your site ?",
                  $config{View}->{TT}->{CONSTANTS}->{root} );
  
# trim off trailing slashes
$rawResponse =~ s/(.*?)\/?$/$1/;

# and store in the value "root" in the TT constants section
$config{View}->{TT}->{CONSTANTS}->{root} = $rawResponse;

#--------------------------------------
# annotation email

print <<EOFloc unless $quiet;

OK. The URL for the index page of your site will be:

  http://<your site>$rawResponse

--------------------------------------------------------------------------------

Next we need to get various values that are used in the application
configuration.

Firstly, values required for the annotation comments feature. Each family page
has an "add annotation" link, which leads to a form via which users can supply
comments. The comments are then emailed to the email address of your choice.

EOFloc

$config{Controller}->{Annotate}->{annotationEmail} =
  $build->prompt( "To what email address should we send annotation comments ?",
                  $config{Controller}->{Annotate}->{annotationEmail} );

#--------------------------------------
# the tmp area

print <<EOFloc unless $quiet;

OK. We'll send annotation emails to "$config{Controller}->{Annotate}->{annotationEmail}".

--------------------------------------------------------------------------------

Various features of the site require a web-accessible temp directory. The
easiest way to achieve that is to make a directory called something like "tmp"
in the htdocs directory of the web-server and set user and filesystem
permissions to allow the web-server process to write files in that directory.

We need to have both the filesystem path for that directory, as well as the
URL for it.

EOFloc

# get the file system path to the temp directory
# NOTE: COMPILE_DIR is the same as the temp directory...

my $tmpDirPath;
$validResponse = 0;
until( $validResponse ) {
  $tmpDirPath =
    $build->prompt( "What is the absolute path for the temporary directory ?",
                    $config{View}->{TT}->{COMPILE_DIR} );

  # see if the directory is already there, otherwise offer to create it
  if( -d $tmpDirPath ) {
    $validResponse = $build->y_n( "(WW) WARNING: that directory already exists; do you want to continue ? (Y/n)", "Y" );
  } else {
    if( $build->y_n( "(WW) WARNING: that directory doesn't exist; should we create it now ? (Y/n)", "Y" ) ) {
      # make sure we catch possible errors with creating the directory
      eval { mkpath $tmpDirPath };
      if( $@ ) {
        print "(WW) WARNING: couldn't create \"$tmpDirPath\": $@\n";
        print "              You will need to create the specified temp directory before starting the server.\n";
      } else {
        print "Successfully created temp directory \"$tmpDirPath\".\n";
      }
    } else {
      print "(WW) WARNING: you will need to create the specified temp directory before starting the server.\n";
    }
    $validResponse = 1;
  }  
}

# and get the URL for the same directory
my $tmpDirURL =
  $build->prompt( "What is the URL for the temporary directory ?",
                  $config{View}->{TT}->{CONSTANTS}->{tmp} );

$config{View}->{TT}->{COMPILE_DIR} = $tmpDirPath;
$config{View}->{TT}->{CONSTANTS}->{tmp} = $tmpDirURL;

#--------------------------------------

print <<EOFloc unless $quiet;

--------------------------------------------------------------------------------

We now need to get some database connection parameters.

The site requires two separate databases. The first is the Pfam database,
which you can retrieve from the Sanger FTP site. This should be accessed using 
a read-only account. We assume that you do not need a password for connecting
to the Pfam database using a read-only account.

The second database, web_user, is used for storing information like the news
items and DAS sources. This must be accessed by an account with read/write
access and you will need to supply both a username and password for that
account.

First, let's get parameters for the Pfam database. 

EOFloc

$validResponse = 0;
until( $validResponse ) {

  my $pfamDSN = "dbi:mysql";
  
  $pfamDSN .= ":" .
    $build->prompt( "What is the name of the Pfam database ?",
                    "pfam_21_0" );
  
  $pfamDSN .= ":" .
    $build->prompt( "What is the name of the machine on which the Pfam database is running ?",
                    "pfam" );
  
  $pfamDSN .= ":" .
    $build->prompt( "What is the port number for connecting to the Pfam database ?",
                    "3306" );
  
  my $pfamUser =
    $build->prompt( "What is the username for connecting to the Pfam database ?",
                    "pfamro" );
  
  $config{Model}->{PfamDB}->{connect_info}[0] = $pfamDSN;
  $config{Model}->{PfamDB}->{connect_info}[1] = $pfamUser;
  
    print <<EOFloc unless $quiet;

We will try to connect to the Pfam database using the following parameters:

  dsn:      $pfamDSN
  username: $pfamUser

Now the "web_user" database details.

EOFloc

  my $wuDSN = "dbi:mysql";
  
  $wuDSN .= ":" .
    $build->prompt( "What is the name of the web_user database ?",
                    "web_user" );
  
  $wuDSN .= ":" .
    $build->prompt( "What is the name of the machine on which it is running ?",
                    "pfam" );
  
  $wuDSN .= ":" .
    $build->prompt( "What is the port for connecting to it ?",
                    "3306" );
  
  my $wuUser =
    $build->prompt( "What is the username for the read/write account ?",
                    "web_user" );
  
  my $wuPass =
    $build->prompt( "What is the password for the read/write account ?",
                    "web_user" );
  
  $config{Model}->{WebUser}->{connect_info}[0] = $wuDSN;
  $config{Model}->{WebUser}->{connect_info}[1] = $wuUser;
  $config{Model}->{WebUser}->{connect_info}[2] = $wuPass;
  
  print <<EOFloc unless $quiet;

We will try to connect to the web_user database using the following parameters:

  dsn:      $wuDSN
  username: $wuUser
  password: $wuPass

EOFloc
  
  # should we test the connections ?
  if( $build->y_n( "Would you like to try connect to the databases now ?", "N" ) ) {
  
    eval {
      require DBI;
    };
    if( $@ ) {
      print "(WW) WARNING: couldn't load DBI: $@\n";
      print "              You will need to make sure that perl can find DBI before starting the server.\n";
    } else {
      # we got DBI to load, so let's check the connection parameters
      my $pfamDBH = DBI->connect( $pfamDSN, $pfamUser, undef, { PrintError => 0 } )
        or print "(WW) WARNING: couldn't connect to the Pfam database: $DBI::errstr\n";  
      my $wuDBH = DBI->connect( $wuDSN, $wuUser, $wuPass, { PrintError => 0 } )
        or print "(WW) WARNING: couldn't connect to web_user database: $DBI::errstr\n";  

      if( $pfamDBH and $wuDBH ) {
        print "\nSuccessfully opened connections to both databases.\n";
        
        # we're less interested in errors on disconnect...
        eval {
          $pfamDBH->disconnect;
          $wuDBH->disconnect;
        };
        $validResponse = 1;

      } else {
        $validResponse = not $build->y_n( "Do you want to try entering the connection parameters again ? (Y/n)", "Y" );
      } 
    }
  } else {
    print "\nOK, we won't try testing the connections.\n" unless $quiet;
    $validResponse = 1;
  }

} # end of "until valid response" for the database connection testing

#-------------------------------------------------------------------------------
# dump out the configuration

# now we should have a valid configuration hash, which we'll store in the MB
# object so that we can retrieve it in Build actions. That way we avoid having
# to re-parse the config file itself at least
$build->config_data( "configHash" => \%config );

# and we'll write it to a new file
( my $newConfigFile = $configFile ) =~ s/jcf/conf/;

print <<EOFloc unless $quiet;

--------------------------------------------------------------------------------

We now have all of the information that we need to write the configuration file
for your server and we can write it to file. The default location for the new
file is:

  \"$newConfigFile\"
  
but before we write the file we'll check if there's a pre-existing 
configuration file called the same name. If there is, you'll have the option 
to overwrite it or rename it. The default setting for the script is to 
overwrite the pre-existing file.

EOFloc

$newConfigFile = backupFile( $newConfigFile );

print "\nWriting your configuration to \"$newConfigFile\".\n";
$configCG->save_file( $newConfigFile, \%config );

# stash the name of that file, so we can delete it when we do a "Build clean"
$build->add_to_cleanup( $newConfigFile );

#-------------------------------------------------------------------------------
#- configure httpd.conf --------------------------------------------------------
#-------------------------------------------------------------------------------

# try parsing the starting file

print <<EOFloc;

--------------------------------------------------------------------------------

Next we'll try configure the snippet of Apache configuration file that will
set up the application in your mod_perl server. We will try to load the Apache
configuration file from the distribution and then prompt for the values that we
need to customize it for your installation.

EOFloc

# figure out if we need to move an existing file aside
( my $newHttpdFile = $httpdFile ) =~ s/jcf/conf/;

# make sure we can read and parse the httpd.conf from the distribution
my( $httpdCG, %httpdConfig );
eval {
  # throws an exception if there's a real parsing error
  $httpdCG= new Config::General( -ConfigFile      => $httpdFile,
                                 -IncludeRelative => 1 );

  # otherwise we should get a Config::General module...
  %httpdConfig = $httpdCG->getall;

  # sanity checks
  $parsed = grep /mod_perl/, @{$httpdConfig{LoadModule}} and
            exists $httpdConfig{PerlSetEnv};

};

if( $@ or not $parsed ) {
  print <<EOFhttpdProblem;

(EE) ERROR: there was a problem loading the Apache configuration file from
            the distribution (\"$httpdFile\"): $@ 
            
If this is a parsing problem, or the file is just missing, then this is a
problem with the distribution...

EOFhttpdProblem
  exit 1;
}

# make sure we delete the file on cleanup
$build->add_to_cleanup( $newHttpdFile );

#--------------------------------------

# get values that we need for configuring httpd.conf

print <<EOFloc unless $quiet;
First, we need to know the location of the program "muscle". The distribution
includes a linux binary for muscle, but you will need to compile or download
your own version if you're not running the server on linux.

We need to know where muscle will eventually be installed. If you've already got
a working version of muscle, give the full path for that binary in answer to the
next question. If you will be installing muscle later, the script will complain
that it can't find the binary but will still accept the value that you enter. 

EOFloc

my $muscleBin;
foreach ( @{$httpdConfig{PerlSetEnv}} ) {
  next unless m/^PFAM_MUSCLE_BIN\s+(.*)/;
  $muscleBin = $1 and last;
}

$validResponse = 0;
until( $validResponse ) {
  $rawResponse = $build->prompt( "Where is the muscle binary ?", $muscleBin );
  if( -e $rawResponse and -x $rawResponse ) {
    print "\nOK. Found the muscle binary.\n";
    $validResponse = 1;
  } else {
    print "(WW) WARNING: no such file or directory.\n";
    
    if( $build->y_n( "Do you want to keep the location you specified ($rawResponse) ? (Y/n)", "Y" ) ) {
      print "\nOK. Accepting the location for muscle that you provided ($rawResponse).\n";
      $validResponse = 1;
    } else {    
      $validResponse = 0;
    }    
  }
}

# set the correct values in the PerlSetEnv lines; we know the rest from our
# earlier questions
# NB PFAMWEB_CONFIG must point to the location of the configuration file in the
# installed server...

$httpdConfig{PerlSetEnv} = [
  "PFAM_DOMAIN_IMAGES $config{View}->{TT}->{CONSTANTS}->{tmp}",
  "PFAM_FAMILY_ROOT   $config{View}->{TT}->{CONSTANTS}->{root}",
  "PFAM_MUSCLE_BIN    $rawResponse",
  "PFAMWEB_CONFIG     $config{installRoot}/$newConfigFile" ];

# set the location handler for PfamWeb using values that we already retrieved
$httpdConfig{Location} =
  {
     $config{View}->{TT}->{CONSTANTS}->{root} => 
       { PerlResponseHandler => "PfamWeb",
         SetHandler          => "perl-script" }
  };

# edit the mod_rewrite rules. We can handle both the RewriteRule and Alias lines
# using the same pattern
my @rules;
foreach ( @{$httpdConfig{RewriteRule}}, @{$httpdConfig{Alias}} ) {
  s/(??{$build->notes("defaultAppRoot")})/$config{View}->{TT}->{CONSTANTS}->{root}/eog;
}

# finally, set the Directory directive that tells Apache to serve the static
# content
$httpdConfig{Directory} = {
   $config{installRoot} => {
                             Allow   => "from all",
                             Options => "none",
                             Order   => "allow,deny"
                           }
};

#--------------------------------------

# we'll need the locations for both the pfam-core and pfam-model components...

print <<EOFloc;

--------------------------------------------------------------------------------

There are two ancilliary modules that we need to install for the site. The
pfam-core directory contains general-purpose modules, for tasks such as drawing
Pfam graphics or sequence manipulation. The pfam-model directory contains
DBIx::Class models for the Pfam and web_user databases, which are used
throughout the PfamWeb code for accessing the two databases.

We need to copy the contents of these two directories when we install everything
and we need to configure mod_perl to find them.

If you want to you can merge the two directories together when we install, by
giving the same path for both pfam-core and pfam-model. Just be aware that at
WTSI we keep them separate, so it's possible that could encounter some file name
clashes when you merge the sets of modules together. Please let us know if you
notice any problems.

EOFloc

# we'll offer to put the pfam-core and pfam-model directories parallel to 
# the PfamWeb directory, i.e. hanging off the same parent directory, which is...
( my $installRootParent = $config{installRoot} ) =~ s/^((\/.*?)+)\/.*/$1/;

my $pfamCoreDir;
$validResponse = 0;
until( $validResponse ) {
  $pfamCoreDir =
    $build->prompt( "Where should we put the contents of the pfam-core directory ?",
                    "$installRootParent/pfam-core" );

  # trim off trailing slashes
  $pfamCoreDir =~ s/(.*?)\/*$/$1/;

  # see if the directory is already there
  if( -d $pfamCoreDir ) {
    $validResponse = $build->y_n( "(WW) WARNING: that directory already exists; do you want to continue ? (Y/n)", "Y" );
  } elsif( -f $1 ) {
    $validResponse= $build->y_n( "(WW) WARNING: a file with that name already exists; do you want to continue ? (y/N)", "N" );
  } else {
    $validResponse = 1;
  }
}

my $pfamModelDir;
$validResponse = 0;
until( $validResponse ) {
  $pfamModelDir =
    $build->prompt( "Where should we put the contents of the pfam-model directory ?",
                    "$installRootParent/pfam-model" );

  # trim off trailing slashes
  $pfamModelDir =~ s/(.*?)\/*$/$1/;

  # see if the directory is already there
  if( -d $pfamModelDir ) {
    $validResponse = $build->y_n( "(WW) WARNING: that directory already exists; do you want to continue ? (Y/n)", "Y" );
  } elsif( -f $1 ) {
    $validResponse= $build->y_n( "(WW) WARNING: a file with that name already exists; do you want to continue ? (y/N)", "N" );
  } else {
    $validResponse = 1;
  }
}

# make a note of the directories where we're copying pfam-core and pfam-model
$build->notes( "pfamCoreDir",  $pfamCoreDir );
$build->notes( "pfamModelDir", $pfamModelDir );

print <<EOFloc unless $quiet;

OK. We'll install pfam-core and pfam-model in the following locations:

  pfam-core:  $pfamCoreDir
  pfam-model: $pfamModelDir

--------------------------------------------------------------------------------

Finally, we'll write out the Apache configuration file.

EOFloc

#--------------------------------------

# we can complete the editing of httpd.conf by tweaking the PerlSwitches
# directives
$httpdConfig{PerlSwitches} = [ "-wT",
                               "-I$pfamCoreDir",
                               "-I$pfamModelDir",
                               "-I$config{installRoot}/lib" ];

#--------------------------------------

# write out httpd.conf

$newHttpdFile = backupFile( $newHttpdFile );

print "\nWriting your Apache configuration to \"$newHttpdFile\".\n";
$httpdCG->save_file( $newHttpdFile, \%httpdConfig );

#-------------------------------------------------------------------------------
#- and we're done --------------------------------------------------------------
#-------------------------------------------------------------------------------

$build->create_build_script;

print <<EOFloc10 unless $quiet;

--------------------------------------------------------------------------------

# TODO copy pfam-core and pfam-model across to blib

Done.

EOFloc10

exit;

#===============================================================================
#= methods =====================================================================
#===============================================================================

# tries to move the specified file to $file.orig.n, where n is incremented until
# that file doesn't exist. If the original file can't be moved, ask they user
# if they want to overwrite it. If they don't want to overwrite, get another
# filename from them. Ultimately, the return value is the name of the file
# that the file should be written to. This may be the same as the one handed
# to the method in the first place...

sub backupFile {
  my $file = shift;

  my $validResponse = 0;
  until( $validResponse ) {
    
    if( -f $file ) {
      if( $build->y_n( "The file \"$file\" already exists. Do you want to overwrite it ? (Y/n)", "Y" ) ) {
        print "\n(WW) WARNING: overwriting \"$file\".\n";
        $validResponse = 1;
      } else {
  
        # generate a name for the backup
        my $backupNumber = 0;
        while( -f "$file.orig.$backupNumber" ) { $backupNumber++ }
        my $backup = "$file.orig.$backupNumber";
        
        print "\nOK. We'll try to rename the pre-existing file to \"$backup\".\n";
        
        if( move( $file, $backup ) ) {
          print "\nSuccessfully moved pre-existing file.\n";  
        } else {
          print "\n(WW) WARNING: couldn't move the pre-existing file: $!";
          $newConfigFile = $build->prompt( "Where do you want to save the new file ?",
                                           "$file" );
        }
      }
    } else {
      $validResponse = 1;
    } 
  }

  return $file;
}


sub usage {
  print STDERR << 'EOFhelp';

These are the command-line arguments for Build.PL:

  accept_defaults     accept all of the defaults that are read from the default
                      configuration file. If this switch is omitted you will be
                      prompted for the required values
  config_file=<file>  use the specified file as the starting point for the
                      server configuration
  help / usage        prints this message
  no_help             don't print any information; just prompt for values

Other arguments affect the running of the generated Build script:

  verbose_log         makes the output from Build more verbose. Adding 
                      "--verbose_log" multiple times will increase the amount of
                      output.

In addition to the standard Build actions, you can use the following custom
actions:

  check_config
  
    Causes the script to try parsing the specified configuration file.
  
    Requires the argument "config_dir", specifying the configuration file to be
    verified, e.g.
    
      ./Build check_config --config_file=conf/pfamweb.conf

    Any errors generated from parsing the file are displayed by default, but to
    see the data structure that is generated from the configuration file (as
    displayed using Data::Dump::dump), add the argument "--verbose_log".

EOFhelp
}

#-------------------------------------------------------------------------------
