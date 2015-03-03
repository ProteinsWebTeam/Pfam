use utf8;
package PfamLive::Result::PfamAHmm;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamAHmm

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_HMM>

=cut

__PACKAGE__->table("pfamA_HMM");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 hmm

  data_type: 'mediumblob'
  is_nullable: 1

=head2 logo

  data_type: 'mediumblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "hmm",
  { data_type => "mediumblob", is_nullable => 1 },
  "logo",
  { data_type => "mediumblob", is_nullable => 1 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ou8wMBSsSrbGt+WSmu6LHw

__PACKAGE__->set_primary_key("pfama_acc");
__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);

sub getHMMAsFile {
    my ($self, $filename, $path) = @_;
    $filename = 'HMM' unless($filename);
    $path = "." unless($path);
    open( A, ">", "$path/$filename") or die "Could not open $path/$filename:[$!]\n";
    print A $self->hmm;
    close A;
    unless ( -e "$path/$filename" and -s "$path/$filename" ) {
        die "Failed to generate $path/$filename containing the HMM.";
    }
}

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
