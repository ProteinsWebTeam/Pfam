
# Build.PL
# jt6 20070208 WTSI.
#
# The main build script for the PfamWeb installation
#
# $Id: Build.PL,v 1.3 2007-02-16 18:01:25 jt6 Exp $

use strict;

use File::Path;
use File::Copy;

use PfamBuilder;

use Data::Dump qw(dump);

# this is the default location for the main configuration file
my $configDir  = "conf";
my $configFile = "$configDir/pfamweb.jcf";

#-------------------------------------------------------------------------------
#- setup Module::Build ---------------------------------------------------------
#-------------------------------------------------------------------------------

# configure Module::Build (or rather our subclass of it) itself

my $build = PfamBuilder
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
                     	    "verbose_log"     => { type    => "+",
                                         	       default => 0 },
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
            						  "Bio::DasLite"                               => 1.0,
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

#-------------------------------------------------------------------------------
#- start the configuration -----------------------------------------------------
#-------------------------------------------------------------------------------

# a little hello... (ASCII-art courtesy of http://www.network-science.de/ascii/)
print <<'EOFloc0';
 ____   __              __        __   _     
|  _ \ / _| __ _ _ __ __\ \      / /__| |__  
| |_) | |_ / _` | '_ ` _ \ \ /\ / / _ \ '_ \ 
|  __/|  _| (_| | | | | | \ V  V /  __/ |_) |
|_|   |_|  \__,_|_| |_| |_|\_/\_/ \___|_.__/ 

This is the installation and configuration script for the Pfam website application.
It will ask a series of questions and based on your responses, it will generate a
script that can be used to configure and then install the website.

You can see some help on using it and the accepted command-line options by running:

  perl Build.PL --help

You will need to make sure that you are using a perl executable which can see the
various prerequisite modules that you should have installed (such as
Catalyst::Runtime), or have set the PERL5LIB environment variable to add the
appropriate library directories to @INC.

Initially, the default values for this script are taken from the configuration file
in this distribution. You can override the defaults by changing the values at the
various prompts when you run this script, or by editing the configuration files
directly. If you edit the files you can point the script at the modified files and
tell it to accept all of the values in them by doing:

  perl Build.PL --config_file=../my_pfamweb.conf --accept_defaults

Unless you specify "--accept_defaults", the script will prompt you for the various
parameters that we need, offering default values from the configuration file that
it reads.

EOFloc0

$build->prompt( "Hit return when you're ready to start.", "" );

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
if( $build->args( "config" ) ) {
  $configFile = $build->args( "config_file" );  
} else {
  $configFile = $build->prompt( "Where is the default configuration file ?",
                                $configFile );
}

# make sure we can read it and then parse it in 
die "(EE) ERROR: couldn't read configuration file \"$configFile\": $!"
  unless -f $configFile;  

my( $configGeneral, %config, $parsed );
eval {
  # throws an exception if there's a real parsing error
  $configGeneral = new Config::General( -ConfigFile      => $configFile,
                                        -IncludeRelative => 1 );

  # otherwise we should get a Config::General module...
  %config = $configGeneral->getall;

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
#- talk to the user... ---------------------------------------------------------
#-------------------------------------------------------------------------------

# where should we put everything ?

print <<EOFloc1;

The PfamWeb application consists of several different types of file. We have
raw perl code, Template Toolkit template files, static HTML content, such as 
CSS files and images, and finally configuration files. 

We will install all of these different files under a single directory, the 
installation root. For example, you might choose to put PfamWeb in a
subdirectory of your web-server, e.g. /usr/local/httpd/PfamWeb, or somewhere
entirely separate, e.g. /opt/webapps/PfamWeb.  

EOFloc1

my( $validResponse, $rawResponse );

until( $validResponse ) {
  $rawResponse = 
    $build->prompt( "In what directory do you want to install the PfamWeb application ?",
                    $config{installRoot} );
  
  # trim off trailing slashes
  $rawResponse =~ s/(.*?)\/*$/$1/;

  if( -d $rawResponse ) {
    $validResponse= $build->y_n( "(WW) WARNING: that directory already exists; do you want to continue ?", "Y" );
  } elsif( -f $1 ) {
    $validResponse= $build->y_n( "(WW) WARNING: a file with that name already exists; do you want to continue ?", "N" );
  } else {
    $validResponse = 1;
  }

}

# store the value directly in our config hash...
$config{installRoot} = $rawResponse;

# and set the INCLUDE_PATH value in the View::TT config
$config{View}->{TT}->{INCLUDE_PATH} = $rawResponse . "/data/templates";

print <<EOFloc2;

OK. We will install everything there and you will end up with a directory
structure that looks something like this:

EOFloc2

foreach my $dir ( qw[ conf data htdocs lib ] ) {
  print "  $config{installRoot}/$dir\n";
}

#--------------------------------------

# we'll store the starting value for the application root as a note, so 
# that we can do a search and replace for it at the Build stage
$build->notes( "default_app_root", $config{View}->{TT}->{CONSTANTS}->{root} );
  
print <<EOFloc3;

Now we need to decide the root of the application in terms of the website
configuration.

When the application is running, users will access it with a URL something
like:

  http://www.sanger.ac.uk/pfam

The path component of the URL (in this case "/pfam") is the application root
and we will need to set that in various files during this installation. Note
that the application root should start with a slash but should not have a 
trailing slash.

EOFloc3

$rawResponse = 
  $build->prompt( "What is the application root for your site ?",
                  $config{View}->{TT}->{CONSTANTS}->{root} );
  
# trim off trailing slashes
$rawResponse =~ s/(.*?)\/?$/$1/;

# and store in the value "root" in the TT constants section
$config{View}->{TT}->{CONSTANTS}->{root} = $rawResponse;

#--------------------------------------
# annotation email

print <<EOFloc4;

OK. The URL for the index page of your site will be:

  http://<your site>$rawResponse

Now we need to get various values that are used in the application
configuration.

Firstly, values required for the annotation comments feature. Each family page
has an "add annotation" link, which leads to a form via which users can 
supply comments. The comments are then emailed to the email address of your
choice.

EOFloc4

$config{Controller}->{Annotate}->{annotationEmail} =
  $build->prompt( "To what email address should we send annotation comments ?",
                  $config{Controller}->{Annotate}->{annotationEmail} );

#--------------------------------------
# the tmp area

print <<EOFloc5;

Various features of the site require a web-accessible temp directory. The
easiest way to achieve that is to make a directory called something like "tmp"
in the htdocs directory of the web-server and set user and filesystem
permissions to allow the web-server process to write files in that directory.

We need to have both the filesystem path for that directory, as well as the
URL for it.

EOFloc5

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
    $validResponse = $build->y_n( "(WW) WARNING: that directory already exists; do you want to continue ?", "Y" );
  } else {
    if( $build->y_n( "(WW) WARNING: that directory doesn't exist; should we create it now ?", "Y" ) ) {
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

print <<EOFloc6;

Next, we need to get some database connection parameters.

The site requires two separate databases. The first is the Pfam database,
which you can retrieve from the Sanger FTP site. This should be accessed using 
a read-only account. We assume that you do not need a password for connecting
to the Pfam database using a read-only account.

The second database, web_user, is used for storing information like the news
items and DAS sources. This must be accessed by an account with read/write
access and you will need to supply both a username and password for that
account.

First, let's get parameters for the Pfam database. 

EOFloc6

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
  
    print <<EOFloc7;

We will try to connect to the Pfam database using the following parameters:

  dsn:      $pfamDSN
  username: $pfamUser

Now the "web_user" database details.

EOFloc7

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
  
  print <<EOFloc8;

We will try to connect to the web_user database using the following parameters:

  dsn:      $wuDSN
  username: $wuUser
  password: $wuPass

EOFloc8
  
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
        print "Successfully opened connections to both databases.\n";
        
        # we're less interested in errors on disconnect...
        eval {
          $pfamDBH->disconnect;
          $wuDBH->disconnect;
        };
        $validResponse = 1;

      } else {
        $validResponse = not $build->y_n( "Do you want to try entering the connection parameters again ?", "Y" );
      } 
    }
  } else {
    print "\nOK, we won't try testing the connections.\n";
    $validResponse = 1;
  }

} # end of "until valid response" for the database connection testing

#--------------------------------------

# need to edit move httpd.conf to httpd.conf.orig and then edit it back to
# httpd.conf, fixing, primarily, the value for PFAMWEB_CONF so that it points
# at the correct file, i.e. the one we just created 

#-------------------------------------------------------------------------------
#- dump out the configuration --------------------------------------------------
#-------------------------------------------------------------------------------

# now we should have a valid configuration hash, which we'll store in the MB
# object so that we can retrieve it in Build actions. That way we avoid having
# to re-parse the config file itself at least
$build->config_data( "configHash" => \%config );

# and we'll write it to a new file
( my $newConfigFile = $configFile ) =~ s/jcf/conf/;

print <<EOFloc9;

Now we need to try writing out the configuration that we just generated. The
default location for the new file is

  \"$newConfigFile\"
  
but before we write the file we'll check if there's a pre-existing configuration
file called the same name. If there is, you'll have the option to overwrite
it or rename it. The default setting for the script is to overwrite the 
pre-existing file.

EOFloc9

$validResponse = 0;
until( $validResponse ) {
  
  if( -f $newConfigFile ) {
    print "The file \"$newConfigFile\" already exists.\n";
    if( $build->y_n( "Do you want to overwrite the pre-existing configuration file ?", "Y" ) ) {
      print "\n(WW) WARNING: overwriting \"$newConfigFile\".\n";
      $validResponse = 1;
    } else {

      # generate a name for the backup file
      my $backupNumber = 0;
      while( -f "$newConfigFile.$backupNumber" ) { $backupNumber++ }
      print "\nOK. We'll try to rename the pre-existing file as \"$newConfigFile.$backupNumber\".\n";
      
      # try renaming the existing config file to the backup filename
      if( move( $newConfigFile, "$newConfigFile.$backupNumber" ) ) {
        print "\nSuccessfully moved pre-existing config file to \"$newConfigFile.$backupNumber\".\n";  
      } else {
        print "\n(WW) WARNING: couldn't move the pre-existing config file: $!";
        $newConfigFile = $build->prompt( "Where do you want to save the new configuration ?",
                                         "$newConfigFile" );
      }
    }
  } else {
    print "\nNo pre-existing configuration file found. ";
    $validResponse = 1;
  } 
}

print "Writing your configuration to \"$newConfigFile\".\n\n";
$configGeneral->save_file( $newConfigFile, \%config );

#--------------------------------------

# and we're done

$build->create_build_script;

print "\nDone.\n\n";
exit;

#===============================================================================
#= methods =====================================================================
#===============================================================================

sub usage {
  print STDERR << 'EOFhelp';

--------------------------------------------------------------------------------

This is the PfamWeb build script.

Command-line arguments can be specified like

  perl Build.PL --accept_defaults

These are the accepted arguments:

  accept_defaults     accept all of the defaults that are built into the script. If
                      this is omitted you will be prompted for missing values
  config_file=<file>  use the specified file as the basis for the server configuration       
  help / usage        prints this message

Other arguments affect the running of the generated Build script:

  verbose_log         makes the output from Build more verbose. Adding "--verbose_log"
                      multiple times will increase the amount of output.

In addition to the standard Build actions, you can use the following custom actions:

  check_config

    causes the script to try parsing the main configuration file. The file will be
    loaded either from the directory specified by the "configs_dir" argument or from
    the default configuration directory. Any errors generated from parsing the file
    are displayed by default, but to see the data structure that is generated from
    the configuration file, add the argument "--verbose_log".

--------------------------------------------------------------------------------

EOFhelp
}

#-------------------------------------------------------------------------------
