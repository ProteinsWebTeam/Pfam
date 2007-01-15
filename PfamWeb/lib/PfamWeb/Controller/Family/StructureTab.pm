
# StructureTab.pm
# jt6 20060411 WTSI
#
# Controller to build the Pfam family structure tab.
#
# $Id: StructureTab.pm,v 1.2 2007-01-15 15:11:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::StructureTab - controller for the 
structure tab of the family pages.

=cut

package PfamWeb::Controller::Family::StructureTab;

=head1 DESCRIPTION

This controller retrieves the mapping between Pfam, UniProt and PDB
residues and hands off to a template that constructs the "structure"
tab of the family section.

$Id: StructureTab.pm,v 1.2 2007-01-15 15:11:10 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Populates the stash with the mapping and hands off to the appropriate template.
Relies on the C<begin> method from the base class (Family) to get the Pfam 
accession, etc.

=cut

sub default : Path {
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
