
# PfamLive.pm
# jt 20061109 WTSI

# The base class for the whole database model. Config comes from the
# catalyst application class.

# $Id: PfamLive.pm,v 1.2 2007-02-14 11:36:04 rdf Exp $

package PfamLive;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes();

$SIG{INT} = sub {
   print STDERR "Caught Ctrl-C, trying to cleanly disconnect\n";
  __PACKAGE__->storage->disconnect;
};
1;
