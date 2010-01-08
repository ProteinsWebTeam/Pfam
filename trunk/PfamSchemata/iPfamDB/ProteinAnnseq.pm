package iPfamDB::ProteinAnnseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_annseq");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "annseq_storable",
  {
    data_type => "MEDIUMBLOB",
    default_value => "",
    is_nullable => 0,
    size => 16777215,
  },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->belongs_to("accession", "iPfamDB::Protein", { accession => "accession" });


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2010-01-08 13:31:47
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:knSJpzBg4NSPdS58Eq+l2w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
