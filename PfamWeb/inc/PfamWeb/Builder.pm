
# PfamBuilder.pm
# jt 20070208 WTSI.
#
# $Id: Builder.pm,v 1.10 2007-03-15 14:35:16 jt6 Exp $

=head1 NAME

PfamWeb::Builder - Module::Build subclass for installing PfamWeb

= cut

package PfamWeb::Builder;

=head1 DESCRIPTION

A custom Module::Builder subclass to handle installation of the
PfamWeb application. Used by the Build.PL installation script, this
module provides the extra methods that L<Module::Build> requires to
install the various extra file types and directories in the PfamWeb
distribution, such as the static HTML content in htdocs.

$Id: Builder.pm,v 1.10 2007-03-15 14:35:16 jt6 Exp $

=cut

use strict;
use warnings;

use base "Module::Build";

use File::Basename;
use File::Path;
use File::Spec::Functions;
use ExtUtils::Install;

use Data::Dump qw(dump);

#-------------------------------------------------------------------------------

=head1 METHODS

L<Module::Build> allows a package author to add extra elements to the
installation process. We're using that to allow L<Module::Build> to
install extra file types and to edit them along the way. These methods
are called automatically by the L<Module::Build> calls, as used by the
Build.PL installation script.

=cut

#-------------------------------------------------------------------------------
#- new elements ----------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 process_htdocs_files

Copies the contents of htdocs (CSS, images, javascripts, jars) into
the build library directory (blib).

=cut

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

=pod

We've implicitly added L<process_conf>, L<process_data> and
L<process_htdocs> methods by adding the new build elements in the mail
Build.PL script, and these "find" methods are used by MB to allow the
various process_* methods to work.

=head2 local_find_file_by_type

This method is copied directly from L<Module::Build::Base::_find_file_by_type>.

=cut

sub local_find_file_by_type  {
  my( $this, $type, $dir ) = @_;
  return { map {$_, $_}
    map $this->localize_file_path( $_ ),
    grep !/\.\# /, @{ $this->rscan_dir( $dir, qr{\.$type$} ) } };
}

=head2 local_find_file_by_type

This is the same idea, but a bit more specialised: find any file in
the htdocs directory, but ignore CVS directories.

=cut

sub find_htdocs_files {
  my $this = shift;

  return { map {$_, catdir( $this->blib, $_ ) }
           map $this->localize_file_path($_),
           grep !/\.\#/,
           @{ $this->rscan_dir( "htdocs", sub { ! -d and $File::Find::dir !~ /CVS$/ } ) } };
}

#--------------------------------------

=head2 find_TYPE_files

These methods use L<local_find_file_by_type> to find specified file
types in the specified directories.

=cut

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

=head1 ACTIONS

=head2 ACTION_install

As per L<Module::Build>, this method uses
L<ExtUtils::Install::install> to install files from the blib directory
into the directory structure determined by L<install_map>.

=cut

sub ACTION_install {
  my $this = shift;

  $this->depends_on( "build" );
  ExtUtils::Install::install( $this->install_map, 0, 0, 0 );
}

#--------------------------------------

=head2 ACTION_fakeinstall

Does the same as L<install> but doesn't actually copy any files. Just
describes what would be done if you ran L<install>.

=cut

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

=head2 ACTION_check_config

A pretty dumb check that the configuration file is in good
shape. Config::General seems happy to process any old junk, so in the
end this is not much more than a cursory look for unclosed blocks,
egregiously broken syntax, etc.

=cut

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

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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

1;
