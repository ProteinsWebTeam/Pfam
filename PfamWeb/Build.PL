
# Build.PL
# jt6 20070208 WTSI.
#
# The main build script for the PfamWeb installation
#
# $Id: Build.PL,v 1.1 2007-02-14 14:18:19 jt6 Exp $

use strict;

use PfamBuilder;

use Data::Dump qw(dump);

# these are WTSI site defaults
my %defaults = (
				app_root => "/catalyst/PfamWeb",
				install_base => "/nfs/WWWdev/SANGER_docs/catalyst/PfamWeb",
				configs_dir => "../PfamConfig",
			   );

#-------------------------------------------------------------------------------
# configure Module::Build (or rather our subclass of it) itself

my $build = PfamBuilder
  ->new(
		dist_author   => 'John Tate <jt6@sanger.ac.uk>',

		# define the command line options for the installation
		get_options   => {
						  accept_defaults => { type    => "!"  },
						  app_root        => { type    => "=s",
											   default => $defaults{app_root} },
						  configs_dir     => { type    => "=s" },
						  "help|usage"    => { type    => "!"  },
						  verbose_log     => { type    => "+",
											   default => 0 },
						 },
		
		license       => "perl",
		module_name   => "PfamWeb",

		# recommended but not essential prerequisites. Since there
		# doesn't seem to be any sensible way to detect BioPerl if
		# it's not installed as a Bundle, it's recommended rather than
		# required
		recommends    => {
						  "Bundle::BioPerl"                            => 0,
						  "Catalyst::Devel"                            => 1.0
						 },

		# these are the absolutely required prerequisites
		requires      => {
						  "Catalyst::Runtime"                          => 5.7,
						  "Catalyst::Action::RenderView"               => 0,
						  "Catalyst::Model::DBIC::Schema"              => 0,
						  "Catalyst::Plugin::Cache::FastMmap"          => 0,
						  "Catalyst::Plugin::Email"                    => 0,
						  "Catalyst::Plugin::HTML::Widget"             => 0,
						  "Catalyst::Plugin::Session::State::Cookie"   => 0,
						  "Catalyst::Plugin::Session::Store::FastMmap" => 0,
						  "Class::Data::Accessor"                      => 0,
						  "Config::General"                            => 0,
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
		test_files    => [ glob('t/*.t') ],
	   );

# we're going to store the default stem for the root URL of the
# application, along with the one that the user specifies at
# runtime. If the two differ, we'll need to process the static files;
# if not, we can get away with copying them.
$build->notes( "default_app_root", $defaults{app_root} );
$build->notes( "user_app_root", $build->args( "app_root" ) );

#-------------------------------------------------------------------------------
# help ?

if( $build->args( "help" ) or $build->args( "usage" ) ) {
  usage($build);
  exit;
}

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# set up the directory structure for the install tree. If we don't set
# these paths specifically, MB will install everything as if it were a
# regular CPAN module, which breaks the catalyst app pretty terminally

#$build->install_base();

$build->install_base_relpaths( "lib"    => "lib" );
$build->install_base_relpaths( "conf"   => "conf" );
$build->install_base_relpaths( "data"   => "data" );
$build->install_base_relpaths( "htdocs" => "htdocs" );

#-------------------------------------------------------------------------------
# see if we should use the script defaults

$ENV{PERL_MM_USE_DEFAULT} = 1 if $build->args->{accept_defaults};

#-------------------------------------------------------------------------------
# figure out where we're looking for the configuration files

my $configDir = $build->args( "configs_dir" );

unless( -d $configDir ) {

  my $correct = 0;
  my $firstTry = 1;

  # loop until we get a good config directory
  while( $firstTry or not $correct ) {
	$configDir = $build->prompt( "Where is the main configuration file, pfamweb.conf ?",
								  $defaults{configs_dir} );
	
	$correct = -d $configDir and -f "$configDir/pfamweb.conf";

	unless( $correct ) {
	  print STDERR "(WW) WARNING: can't load pfamweb.conf from there."
		. ( $firstTry ? " Try again or quit with Ctrl-C\n" : "\n" );
	}

	$firstTry = 0;
  }

}

print "(ii) Taking configuration files from $configDir\n";
$build->notes( configDir => $configDir );

#-------------------------------------------------------------------------------
# and we're done

$build->create_build_script;

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

  accept_defaults  accept all of the defaults that are built into the script. If
                   this is omitted you will be prompted for missing values
  app_root=...     the root of the URL where the site will run.
                   default: "/catalyst/PfamWeb"
  configs_dir=...  string; the path to the directory containing configuration files.
                   This will be used to find "pfamweb.conf" and at build time
                   all files with the "conf" suffix will be copied into the build
                   directory
                   default: "../PfamConfig"
  help / usage     prints this message

Other arguments affect the running of the generated Build script:

  verbose_log      makes the output from Build more verbose. Adding "--verbose_log"
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
