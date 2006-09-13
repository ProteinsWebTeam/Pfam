
# PfamDB.pm
# jt6 20060808 WTSI
#
# $Id: PfamDB.pm,v 1.3 2006-09-13 08:34:42 jt6 Exp $

=head1 NAME

PfamWeb::Model::PfamDB - a wrapper around the pfam database schema

=cut

package PfamWeb::Model::PfamDB;

=head1 DESCRIPTION

The model for the database is defined in a L<DBIx::Class::Schema>,
C<PfamDB>. This is a wrapper around that schema, which imports
auto-generates the L<Catalyst::Model> classes at runtime, using the
schema classes to define the components.

As the base class for the database model, this class also instantiates
a L<Bio::DasLite> object, which can be retrieved by controllers using
C<getDasLite>, something like

=over

=item C<$c->model("PfamDB")->getDasLite>

=back

The object is instantiated in the C<BEGIN> block and uses
configuration parameters that are set in the YAML application
configuration file.

$Id: PfamDB.pm,v 1.3 2006-09-13 08:34:42 jt6 Exp $

=cut

use strict;
use warnings;

use base qw/Catalyst::Model::DBIC::Schema/;

use Bio::DasLite;

#-------------------------------------------------------------------------------

BEGIN {
  __PACKAGE__->{dasLiteObj} = Bio::DasLite->new( { dsn        => PfamWeb->config->{dasDsn},
												   timeout    => PfamWeb->config->{dasTo},
												   http_proxy => PfamWeb->config->{dasProxy}
												 } );
}

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getDasLite

Returns the L<Bio::DasLite> object that was instantiated at runtime.

=cut

sub getDasLite {
  return __PACKAGE__->{dasLiteObj};
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
