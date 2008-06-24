package treefam::cgiaux;

use strict;
use warnings;

use Exporter;
use treefam::nhx;
use treefam::cgi;
use treefam::tfbase qw/mfa2nhx :desc/;
use treefam::generic qw/:basic last_time writefa readfa/;

use vars qw(@ISA @EXPORT);

@ISA    = qw(Exporter treefam::cgi);
@EXPORT = qw();

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = treefam::cgi->new;
	bless($self, $class);
	return $self;
}
sub prefix
{
	my ($self, $session) = @_;
	return $self->root . "/data/curated/c.$session";
}
sub checkout
{
	my $self = shift;
	my $ac = shift;
	my $session = shift;
	my $str = (@_)? shift : 'clean';
	my %desc;
	my $pre = $self->prefix($session);
	$self->db->get_desc_hash($ac, \%desc) if ($ac =~ /^TF1/);
	$desc{OLD_AC} = $ac; $desc{SUBMIT} = 'N';
	write_file("$pre.desc", get_desc_str(\%desc));
	write_file("$pre.nhx", $self->db->get($ac, "$str.nhx"));
	write_file("$pre.mfa", $self->db->get($ac, "$str.mfa"));
	write_file("$pre.nucl.mfa", $self->db->get($ac, "$str.nucl.mfa"));
}
sub remove
{
	my ($self, $session) = @_;
	my $pre = $self->prefix($session);
	unlink("$pre.nhx", "$pre.mfa", "$pre.nucl.mfa", "$pre.desc");
}
sub copy_tree
{
	my ($self, $session, $tree) = @_;
	write_file($self->prefix($session).".nhx", $tree);
	$self->shrink_align($session, $tree);
}
sub shrink_align # just auxiliary function, used by copy_tree
{
	my ($self, $session, $tree) = @_;
	my (@seq, @seqout, %leaves);
	foreach my $s (".nucl.mfa", ".mfa") {
		@seq = (); @seqout = ();
		readfa($self->prefix($session).$s, \@seq);
		get_leaves($tree, \%leaves);
		foreach my $p (@seq) {
			push(@seqout, $p) if ($leaves{$p->{N}});
		}
		writefa($self->prefix($session).$s, \@seqout);
	}
}
sub build_tree
{
	my ($self, $session, $is_cons, $method) = @_;
	my %methods_aa = (JTT=>'jtt', jtt=>'jtt', Kimura=>'kimura', kimura=>'kimura', mismatch=>'mm', mm=>'mm');
	my %methods_nt = (dS=>'ds', ds=>'ds', dN=>'dn', dn=>'dn', dM=>'dm', dm=>'dm');
	my $pre = $self->prefix($session);
	my $file;
	my $cons = ($is_cons)? "-c $pre.nhx" : '';
	if ($methods_aa{$method}) {
		$method = $methods_aa{$method};
		$file = "$pre.mfa";
	} elsif ($methods_nt{$method}) {
		$method = $methods_nt{$method};
		$file = "$pre.nucl.mfa";
	} else {
		warn("[treefam::cgi::build_tree] unrecognized method $method");
		return;
	}
	write_file("$pre.nhx", mfa2nhx(read_file($file), "-gb100 -t $method $cons"));
}
sub clean_bad_session
{
	my $self = shift;
	my ($fh, $count, @list);
	unless (opendir($fh, $self->root."/data/curated")) {
		warn("[treefam::cgiaux::clean_bad_session] fail to find curated directory");
		return 0;
	}
	@list = grep { /^c\.\d+\.desc/ } readdir($fh);
	closedir($fh);
	$count = 0;
	foreach my $x (@list) {
		next unless ($x =~ /^c\.(\d+)\.desc/ );
		$count += $self->clean_a_bad_session($1);
	}
	return $count;
}
sub clean_a_bad_session
{
	my ($self, $session) = @_;
	$session = detaint($session, '\d+');
	my $pre = $self->prefix($session);
	my %hash = ();
	return 0 if (-f "$pre.desc" && last_time("$pre.desc") < 12 * 3600);
	if (-f "$pre.desc") {
		load_desc("$pre.desc", \%hash);
		return 0 if ($hash{SUBMIT} && $hash{SUBMIT} eq 'Y');
	}
	$self->remove($session);
	return 1;
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
