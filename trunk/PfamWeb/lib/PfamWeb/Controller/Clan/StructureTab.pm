# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: StructureTab.pm,v 1.1 2006-11-13 15:34:39 rdf Exp $

=head1 NAME

PfamWeb::Controller::Clan - controller for clan-related
sections of the site

=cut

package PfamWeb::Controller::Clan::StructureTab;

=head1 DESCRIPTION

This is intended to be the base class for everything related to clans
across the site. The L<begin|/"begin : Private"> method will try to
extract a clan ID or accession from the captured URL and then try to
load a Clan object from the model into the stash.

Generates a B<tabbed page>.

$Id: StructureTab.pm,v 1.1 2006-11-13 15:34:39 rdf Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Clan";

#-------------------------------------------------------------------------------

sub structureTab : Path {
  my($this, $c) = @_;

  $c->log->debug( "Clan::StructureTab::structureTab: acc: |"
		  . $c->stash->{clan}->clan_acc . "|" );

  $c->forward( "_getMapping" );
  $c->stash->{template} = "components/blocks/clan/structureTab.tt";
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
