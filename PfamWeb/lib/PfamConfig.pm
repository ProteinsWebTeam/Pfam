
# PfamConfig.pm
# jt 20060316 WTSI

# this is a class which extends Module::PortablePath to change the
# default location for the configuration file.
#
# We're using Module::PortablePath to retrieve the locations of
# external libraries from a configuration file, rather than having to
# hard-code them into "use lib" lines in each source file

# $Id: PfamConfig.pm,v 1.2 2006-07-14 13:14:41 jt6 Exp $

package PfamConfig;

use strict;
use warnings;

use base "Module::PortablePath";

sub import {

  # we expect to find the configuration file in the root directory of
  # the catalyst application
  
  $Module::PortablePath::CONFIGS = {
	 default => "/nfs/WWWdev/SANGER_docs/catalyst/PfamWeb/pfamweb.ini",
  };

  Module::PortablePath::import(@_);
}

1;

