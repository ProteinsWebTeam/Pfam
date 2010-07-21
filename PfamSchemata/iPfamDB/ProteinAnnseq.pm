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


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:T3GIDPjdLtHnovSo8E6GwA
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/ProteinAnnseq.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/ProteinAnnseq.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
