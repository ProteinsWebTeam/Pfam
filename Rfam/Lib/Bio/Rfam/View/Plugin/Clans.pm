package Bio::Rfam::View::Plugin::Clans;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

use IO::Compress::Gzip qw(gzip $GzipError);
use Data::Printer;
use Carp;

use Bio::Rfam::QC;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::View;
use Bio::Rfam::SVN::Client;
use Bio::Rfam::QC;
use Data::Printer; 

sub process {
  my $self = shift;
  $self->competeClan;
}

sub competeClan {
	my ($self) = @_;
	my @clan_members;
	#my @clan_members = qw(RF00015 RF00016 RF00017);

	my $config = $self->parent->config;
	my $rfamdb = $self->parent->config->rfamlive;
	my $familyObj = $self->parent->family;
	my $rfam_acc = $self->parent->family->DESC->AC;
   	my $clan_rs = $rfamdb->resultset('ClanMembership')->find({rfam_acc => $rfam_acc});
	my $clan_acc = $clan_rs->clan_acc->clan_acc;
    my $clan_rfam_rs = $rfamdb->resultset('ClanMembership')->search({clan_acc => $clan_acc});
    while (my $row = $clan_rfam_rs->next) {
		#p $row->rfam_acc->rfam_acc;
		my $clan_member = $row->rfam_acc->rfam_acc;
		push @clan_members, $clan_member;
	}
	p @clan_members;
	open (my $CLOVERLAP, ">/tmp/CLOVERLAP") or die "Could not open /tmp/CLOVERLAP: [$!]\n";
	my $error = Bio::Rfam::QC::findClanOverlaps($familyObj,$rfamdb, $config,  \@clan_members);
}



1;
1;




































