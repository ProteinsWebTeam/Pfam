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


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rZxK5agvzH8fLu2RmfHi5g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
