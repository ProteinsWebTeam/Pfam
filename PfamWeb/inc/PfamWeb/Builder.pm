
# PfamBuilder.pm
# jt 20070208 WTSI.
#
# A custom Module::Builder subclass to handle installation of the
# PfamWeb application
#
# $Id: Builder.pm,v 1.9 2007-03-08 14:13:11 jt6 Exp $

package PfamWeb::Builder;

use strict;
use warnings;

use base "Module::Build";

use File::Basename;
use File::Path;
use File::Spec::Functions;
use ExtUtils::Install;

use Data::Dump qw(dump);

#-------------------------------------------------------------------------------
#- new elements ----------------------------------------------------------------
#-------------------------------------------------------------------------------

# copy the contents of htdocs (CSS, images, javascripts, jars) across

sub process_htdocs_files {
  my $this = shift;

  # shortcuts...
  my $defRoot = $this->notes( "defaultAppRoot" );
  my $appRoot = $this->notes( "appRoot" );

  # make a directory for all of this...  
  mkpath( catdir( $this->blib, "htdocs" ) );
  
  my $files = $this->find_htdocs_files;

  while( my( $from, $to ) = each %$files ) {
    next if $this->up_to_date( $from, $to );
    
    # only need to edit CSS and JS files, and only if the default application 
    # root has been changed by the user
    if( $from =~ /\.(css|js)$/ and $defRoot ne $appRoot ) {
        
      print "processing $from -> $to\n";
    
    	open( IN, $from )
    	  or warn "(WW) WARNING: couldn't open file \"$from\": $!" and return;    
    	  
  	  # make sure the output directory exists...
      eval {
    	  mkpath( dirname( $to ), 0, 0777 ) unless -d $to;    	  
    	  open( OUT, ">$to" );
      };
      if( $@ ) {
    	  warn "(WW) WARNING: couldn't write file \"$to\": $!";
    	  return;
      }
    
    	# get the default value and the new value of the URL root from the
    	# notes and build a hideous regex to replace one with the other
    	while( <IN> ) {
    	  s/(??{$defRoot})/$appRoot/eog;
    	  print OUT $_;
    	}
    
    	close IN;
    	close OUT;
  	
    } else {
      $this->copy_if_modified( from => $from, to => $to );
    }

  }

}

#-------------------------------------------------------------------------------
# We've implicitly added process_conf, process_data and process_htdocs methods
# by adding the new build elements in the mail Build.PL script, and these 
# "find" methods are used by MB to allow the various process_* methods to work.

# this one is copied directly from Module::Build::Base::_find_file_by_type
sub local_find_file_by_type  {
  my( $this, $type, $dir ) = @_;
  return { map {$_, $_}
    map $this->localize_file_path( $_ ),
    grep !/\.\#/, @{ $this->rscan_dir( $dir, qr{\.$type$} ) } };
}

# this is the same idea, but a bit more specialised: find any file in the htdocs
# directory, but ignore CVS directories
sub find_htdocs_files {
  my $this = shift;
  
return { map {$_, catdir( $this->blib, $_ ) }
         map $this->localize_file_path($_),
         grep !/\.\#/,
         @{ $this->rscan_dir( "htdocs", sub { ! -d and $File::Find::dir !~ /CVS$/ } ) } };
}

#--------------------------------------

# and using that...
sub find_conf_files {
  return shift->local_find_file_by_type( "conf", "conf" );
}

sub find_data_files {
  return shift->local_find_file_by_type( "tt", "data" );
}

sub find_pfamCore_files {
  my $this = shift;
  return $this->local_find_file_by_type( "pm", "pfamCore" ); 
}

sub find_pfamModel_files {
  my $this = shift;
  return $this->local_find_file_by_type( "pm", "pfamModel" );
}

#-------------------------------------------------------------------------------
#- actions ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

sub ACTION_install {
  my $this = shift;
  
  $this->depends_on( "build" );
  ExtUtils::Install::install( $this->install_map, 0, 0, 0 ); 
}

#--------------------------------------

sub ACTION_fakeinstall {
  my $this = shift;

#  foreach my $type ( $this->install_types ) {
#    my $dest = $this->install_destination( $type );
#    print "|$type| ---> |$dest|\n"; 
#  }
#
#  print "install_base_relpaths:\n" . ( dump $this->install_base_relpaths ) . "\n";
#  print "install_path:\n" . ( dump $this->install_path ) . "\n";
#  print "install_map:\n" . ( dump $this->install_map ) . "\n";

  $this->depends_on( "build" );
  ExtUtils::Install::install( $this->install_map, 0, 1, 0 ); 
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
