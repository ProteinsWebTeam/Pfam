package iPfamDB::Pfama;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfama");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "numberinalign",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "description",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "domcount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 5 },
  "ligcount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "nacount",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "interpro_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "interpro_abstract",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("pfama_acc");
__PACKAGE__->has_many(
  "domains",
  "iPfamDB::Domain",
  { "foreign.pfam_acc" => "self.pfama_acc" },
);
__PACKAGE__->has_many(
  "gene_ontologies",
  "iPfamDB::GeneOntology",
  { "foreign.pfama_acc" => "self.pfama_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XtevrNzDUrMidWIR6KIMLw


# You can replace this text with custom content, and it will be preserved on regeneration

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
