
# PfamBuilder.pm
# jt 20070208 WTSI.
#
# A custom Module::Builder subclass to handle installation of the
# PfamWeb application
#
# $Id: Builder.pm,v 1.4 2007-02-27 17:06:34 jt6 Exp $

package PfamWeb::Builder;

use strict;
use warnings;

use base "Module::Build";

use File::Path;
use File::Copy;
use File::Find;
use Config::General;

use Data::Dump qw(dump);

# a "global" so that we have somewhere to keep track of the MB object
# and similarly the config hash that we built
my( $build, $config );

#-------------------------------------------------------------------------------
#- new elements ----------------------------------------------------------------
#-------------------------------------------------------------------------------

# copied directly from Module::Build::Base::_find_file_by_type

sub local_find_file_by_type  {
  my( $this, $type, $dir ) = @_;
  return { map {$_, $_}
    map $this->localize_file_path( $_ ),
    grep !/\.\#/, @{ $this->rscan_dir( $dir, qr{\.$type$} ) } };
}

sub find_conf_files {
  return shift->local_find_file_by_type( "conf", "conf" );
}

sub find_data_files {
  return shift->local_find_file_by_type( "tt", "data" );
}

sub process_htdocs_files {
  my $this = shift;

  print "***** processing htdocs files...\n";
        
  # shortcuts...
  my $defRoot = $this->notes( "defaultAppRoot" );
  my $appRoot = $this->config_data( "configHash" )->{View}->{TT}->{CONSTANTS}->{root};
  
  my $htdocsDir = File::Spec->catdir( $this->blib, "htdocs" );
  File::Path::mkpath( $htdocsDir );
  
  my $files = $this->find_htdocs_files;
  print "--------------------------------------------------\n";
  print dump $files;
  print "--------------------------------------------------\n";

  while( my( $from, $to ) = each %$files ) {
    print "***** checking |$from| -> |$to|\n";
    next if $this->up_to_date( $from, $to );
    print "***** |$from| -> |$to| needs update\n";
    
    if( m/\.(css|js)$/ and $defRoot ne $appRoot ) {
        
    	print "processing $from\n" if $build->args( "verbose_log" );
    
    	open( IN, $from )
    	  or warn "(WW) WARNING: couldn't open file \"$from\": $!" and return;    
    	open( OUT, ">to" )
    	  or warn "(WW) WARNING: couldn't write file \"$to\": $!"  and return;
    
    	# get the default value and the new value of the URL root from the
    	# notes and build a hideous regex to replace one with the other
    	while( <IN> ) {
    	  s/(??{$defRoot})/$appRoot/eog;
    	  print OUT $_;
    	}
    
    	close IN;
    	close OUT;
  	
    } else {

    	# not a file that we need to firkle with; just copy it into blib
    	print "copying $from -> $to\n" if $this->args( "verbose_log" );
    
      $this->copy_if_modified( from => $from, to => $to );
    }

    $this->add_to_cleanup( $to );
  }

}

sub find_htdocs_files {
  my $this = shift;
  
return { map {$_, File::Spec->catdir( $this->blib, $_ ) }
         map $this->localize_file_path($_),
         grep !/\.\#/,
         @{ $this->rscan_dir( "htdocs", sub { ! -d and $File::Find::dir !~ /CVS$/ } ) } };
}

#-------------------------------------------------------------------------------
# copy the contents of htdocs (CSS, images, javascripts, jars) across

sub old_process_htdocs_files {
  my $this = shift;

  # get a copy of the MB object for use in the callback
  $build = $this unless defined $build;

  # retrieve the configuration hash that we build from user input
  $config = $this->config_data( "configHash" ) unless defined $config;

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

  # see if the derived file is up to date
  return if $build->up_to_date( "$File::Find::name", "blib/$File::Find::name" );

  # make the directory before we try to copy stuff into it...
  mkpath "blib/$File::Find::dir"
	or warn "(WW) WARNING: couldn't create directory \"blib/$File::Find::dir\": $!"
	  unless -d "blib/$File::Find::dir";

  # handle the CSS/JS files. See if the user has changed the default
  # setting for the application root URL: if it's changed, we need to
  # edit the files; if not, copy them.

  if( m/\.(css|js)$/ and 
      $build->notes( "defaultAppRoot" ) ne $config->{View}->{TT}->{CONSTANTS}->{root} ) {

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
  	  s/(??{$build->notes("defaultAppRoot")})/$config->{View}->{TT}->{CONSTANTS}->{root}/eog;
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

#sub ACTION_install {
#  my $this = shift;
#  require ExtUtils::Install;
#  $this->depends_on( "build" );
#  ExtUtils::Install::install(
#    $this->install_map,
#    !$this->quiet,
#    0,
#    $this->{args}{uninst}||0 );
#}
#
#sub ACTION_fakeinstall {
#  my ($self) = @_;
#  require ExtUtils::Install;
#  $self->depends_on('build');
#  ExtUtils::Install::install($self->install_map, !$self->quiet, 1, $self->{args}{uninst}||0);
#}


# override the "install" action entirely

sub ACTION_fakeinstall {
  my $this = shift;
 
  $config = $this->config_data( "configHash" ) unless defined $config;
  
 my $map = {
    "blib/lib"        => $config->{installRoot} . "/lib",
    "blib/htdocs"     => $config->{installRoot} . "/htdocs",
    "blib/data"       => $config->{installRoot} . "/data",
    "blib/conf"       => $config->{installRoot} . "/conf",
    "blib/pfam-core"  => $this->notes( "pfamCoreDir" ),
    "blib/pfam-model" => $this->notes( "pfamModelDir" ),
    "read"            => "",
    "write"           => ""
  };
  require ExtUtils::Install;
  ExtUtils::Install::install( $map, 0, 1, 0 );
 
#my $map = $this->install_map;

#print dump( $map ) . "\n"; 
  
#  print "copying pfam-core to " . $this->notes( "pfamCoreDir" ) . "\n";
#  print "copying pfam-model to " . $this->notes( "pfamModelDir" ) . "\n";
#
#  # retrieve the configuration hash that we build from user input
#  $config = $this->config_data( "configHash" ) unless defined $config;
#
#  print "mkdir " . $config->{installRoot} . "\n";
#  
#  print "copying conf, lib, htdocs, data to " . $config->{installRoot} . "\n";
}

#-------------------------------------------------------------------------------
# a pretty dumb check that the configuration file is in good
# shape. Config::General seems happy to process any old junk, so in
# the end this is not much more than a cursory look for unclosed
# blocks, egregiously broken syntax, etc.

sub ACTION_check_config {
  my $this = shift;
  
  my $configFile = $this->args( "config_file" );

  unless( $configFile ) {
    print STDERR <<EOFccUsage;    
(EE) ERROR: no configuration file specified.

Usage: Build check_config --config_file=<config_file>
EOFccUsage
    exit 1;
  }      
  unless( -f $configFile ) {
    print STDERR "(EE) ERROR: no configuration file found at \"$configFile\": $1\n";
    exit 1;
  }
  
  print "(ii) test processing config file \"$configFile\"...\n";

  my $conf = new Config::General( $configFile );
  my %config = $conf->getall;

  dump( \%config ) if $this->args( "verbose_log" );

  print "(ii) done parsing configuration\n";
}

#-------------------------------------------------------------------------------

1;
