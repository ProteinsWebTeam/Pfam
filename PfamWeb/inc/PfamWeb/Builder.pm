
# PfamBuilder.pm
# jt 20070208 WTSI.
#
# A custom Module::Builder subclass to handle installation of the
# PfamWeb application
#
# $Id: Builder.pm,v 1.1 2007-02-14 14:18:19 jt6 Exp $

package PfamBuilder;

use strict;
use warnings;

use base "Module::Build";

use File::Path;
use File::Copy;
use File::Find;
use Config::General;

use Data::Dump qw(dump);

# a "global" so that we have somewhere to keep track of the MB object
my $build;

#-------------------------------------------------------------------------------
#- new elements ----------------------------------------------------------------
#-------------------------------------------------------------------------------

# this essentially just copies *.conf from the user-specified
# configurations source directory into the blib directory, from where
# they're installed when we run Build install

sub process_conf_files {
  my $this = shift;

  print "(ii) creating a conf directory in blib\n";
  mkpath "blib/conf";

  # get the location of the configuration files from our note from the
  # main build script
  print "(ii) copying config files from " . $this->notes( "configDir" ) . "\n";
  my $configDir = $this->notes( "configDir" );

  # read through the directory, looking only for *.conf files to copy
  # into blib
  opendir( CONF, $configDir )
    or die "(EE) ERROR: couldn't read from the config directory: $!";

  foreach my $file ( grep /\.conf$/, readdir CONF ) {
    print "$configDir/$file -> blib/conf/$file\n"
	  if $this->args( "verbose_log" );

    copy( "$configDir/$file", "blib/conf" )
      or warn "(WW) WARNING: couldn't copy configuration file \"$file\": $!";
  }

  closedir CONF;
}

#-------------------------------------------------------------------------------
# copy template files across

sub process_data_files {
  my $this = shift;

  # get a copy of the MB object for use in the callback
  $build = $this unless defined $build;

  print "(ii) copying template files\n";
  mkpath "blib/data";

  # walk the tree from "data" downwards and copy any template file
  # that we find into the blib tree
  finddepth( { wanted     => \&forEachTemplate,
			   no_chdir   => 1 },
			 "data" );

}

#----------------------------------------
# callback for handling templates

sub forEachTemplate {

  print "\$File::Find::dir:  |$File::Find::dir|\n"  if $build->args( "verbose_log" ) > 1;
  print "\$_:                |$_|\n"                if $build->args( "verbose_log" ) > 1;
  print "\$File::Find::name: |$File::Find::name|\n" if $build->args( "verbose_log" ) > 1;

  # copy only templates
  return if not m/\.tt$/;

  # make the directory before we try to copy stuff into it...
  mkpath "blib/$File::Find::dir"
	or warn "(WW) WARNING: couldn't create directory \"blib/$File::Find::dir\": $!"
	  unless -d "blib/$File::Find::dir";

  print "$File::Find::name -> blib/$File::Find::dir\n"
	if $build->args( "verbose_log" );

  copy( "$File::Find::name", "blib/$File::Find::dir" )
	or warn "(WW) WARNING: couldn't copy template file \"$_\": $!";

}

#-------------------------------------------------------------------------------
# copy the contents of htdocs (CSS, images, javascripts, jars) across

sub process_htdocs_files {
  my $this = shift;

  # get a copy of the MB object for use in the callback
  $build = $this unless defined $build;

  print "(ii) copying static files\n";
  mkpath "blib/htdocs";

  # walk the tree from "data" downwards and copy any template file
  # that we find into the blib tree
  finddepth( { wanted     => \&forEachStaticFile,
			   no_chdir   => 1 },
			 "htdocs" );

}

#----------------------------------------
# callback for handling CSS and JS files

sub forEachStaticFile {

  print "\$File::Find::dir:  |$File::Find::dir|\n"  if $build->args( "verbose_log" ) > 1;
  print "\$_:                |$_|\n"                if $build->args( "verbose_log" ) > 1;
  print "\$File::Find::name: |$File::Find::name|\n" if $build->args( "verbose_log" ) > 1;

  # skip CVS directories entirely
  return if $File::Find::dir =~ /CVS$/ or -d;

  # make the directory before we try to copy stuff into it...
  mkpath "blib/$File::Find::dir"
	or warn "(WW) WARNING: couldn't create directory \"blib/$File::Find::dir\": $!"
	  unless -d "blib/$File::Find::dir";

  # handle the CSS/JS files. See if the user has changed the default
  # setting for the application root URL: if it's changed, we need to
  # edit the files; if not, copy them.

  if( m/\.(css|js)$/ and $build->notes( "user_app_root" ) ne $build->notes( "default_app_root" ) ) {

	print "processing $File::Find::name -> blib/$File::Find::dir\n"
	  if $build->args( "verbose_log" );

	open( IN, $File::Find::name )
	  or warn "(WW) WARNING: couldn't open ".uc($1)." file \"$File::Find::name\": $!"
		and return;

	open( OUT, ">blib/$File::Find::name" )
	  or warn "(WW) WARNING: couldn't write ".uc($1)." file \"blib/$File::Find::name\": $!"
		and return;

	# get the default value and the new value of the URL root from the
	# notes and build a hideous regex to replace one with the other
	while( <IN> ) {
	  s/(??{$build->notes("default_app_root")})/$build->notes("user_app_root")/eog;
	  print OUT $_;
	}

	close IN;
	close OUT;
	
  } else {

	# not a file that we need to firkle with; just copy it into blib
	print "copying $File::Find::name -> blib/$File::Find::name\n"
	  if $build->args( "verbose_log" );

	copy( "$File::Find::name", "blib/$File::Find::dir" )
	  or warn "(WW) WARNING: couldn't copy static file \"$_\": $!";
  }

}

#-------------------------------------------------------------------------------
#- actions ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

# a pretty dumb check that the configuration file is in good
# shape. Config::General seems happy to process any old junk, so in
# the end this is not much more than a cursory look for unclosed
# blocks, egregiously broken syntax, etc.

sub ACTION_check_config {
  my $this = shift;

  print "(ii) test processing config file \"" . $this->notes( "configDir" ) . "/pfamweb.conf\"\n";

  my $conf = new Config::General( $this->notes( "configDir" ) . "/pfamweb.conf" );
  my %config = $conf->getall;

  dump( \%config ) if $this->args( "verbose_log" );

  print "(ii) done parsing configuration\n";
}

#-------------------------------------------------------------------------------

1;
