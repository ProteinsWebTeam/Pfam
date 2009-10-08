package PfamDB::PfamseqMarkup;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamseq_markup");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_markup",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 3 },
  "residue",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "annotation",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamDB::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);
__PACKAGE__->belongs_to(
  "auto_markup",
  "PfamDB::MarkupKey",
  { auto_markup => "auto_markup" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZVfL/BvVO+PL4UHj6B9wfg


__PACKAGE__->set_primary_key( qw/auto_pfamseq auto_markup residue/ );

__PACKAGE__->has_one(
  'pfamseqs',
  'PfamDB::Pfamseq',
  { 'foreign.auto_pfamseq' => 'self.auto_pfamseq' },
  { cascade_delete => 0 }
);

__PACKAGE__->might_have(
  'pfama_reg_full_significants',
  'PfamDB::PfamaRegFullSignificant',
  { 'foreign.auto_pfamseq' => 'self.auto_pfamseq' },
  { cascade_delete => 0 }
);

__PACKAGE__->might_have(
  'pfamb_regs',
  'PfamDB::PfambReg',
  { 'foreign.auto_pfamseq' => 'self.auto_pfamseq' },
  { cascade_delete => 0 }
);

1;
