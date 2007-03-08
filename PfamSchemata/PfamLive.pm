
# PfamLive.pm
# jt6 20061109 WTSI
#
# $Id: PfamLive.pm,v 1.3 2007-03-08 14:16:31 jt6 Exp $
#
# $Author: jt6 $

=head1 NAME

PfamLive - DBIC schema definition class for the pfamlive database

=cut

package PfamLive;

=head1 DESCRIPTION

The base class for the pfamlive database model. Config comes from the catalyst 
application class.

$Id: PfamLive.pm,v 1.3 2007-03-08 14:16:31 jt6 Exp $

=cut

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes();

#-------------------------------------------------------------------------------

$SIG{INT} = sub {
   print STDERR "Caught Ctrl-C, trying to cleanly disconnect\n";
  __PACKAGE__->storage->disconnect;
};

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
