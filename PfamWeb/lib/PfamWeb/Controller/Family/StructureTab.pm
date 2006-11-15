# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the Pfam family structure tab.
#
# $Id: StructureTab.pm,v 1.1 2006-11-15 11:06:23 rdf Exp $

=head1 NAME

PfamWeb::Controller::Family::StructureTab - controller for family-related
sections of the site

=cut

package PfamWeb::Controller::Family::StructureTab;

=head1 DESCRIPTION

This is intended to be the base class for everything related to family
across the site. The L<begin|/"begin : Private"> method will try to
extract a clan ID or accession from the captured URL and then try to
load a family object from the model into the stash.

Generates a B<tabbed page>.

$Id: StructureTab.pm,v 1.1 2006-11-15 11:06:23 rdf Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

sub structureTab : Path {
  my($this, $c) = @_;

  $c->log->debug( "Family::StructureTab::structureTab: acc: |"
		  . $c->stash->{acc}  . "|" .  $c->stash->{entryType}. "|");

  $c->forward( "_getMapping" );
  $c->stash->{template} = "components/blocks/family/structureTab.tt";
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
