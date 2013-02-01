package Bio::Rfam::View::Plugin::Alignment;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self   = shift;
  my $family = $self->parent->family;
  my $msa    = $family->SEED;
  my $desc   = $family->DESC;

  write_avg_pid   ($msa);
  write_avg_sqlen ($msa);
  write_seed_annot($msa, "seed-annot.sto");

  # TODO: output 'seed-annot-species.sto', same as 'seed-annot..sto'
  # but with species names
}
sub write_avg_pid { 
    my $msa = shift;
    printf("Average ID:     %5.2f\n", ($msa->average_pid * 100));
    return;
}

sub write_avg_sqlen { 
    my $msa = shift;
    printf("Average length: %.2f\n",  ($msa->average_sqlen));
    return;
}

sub write_seed_annot {
    my $msa  = shift;
    my $desc = shift;

    my $io = Bio::Rfam::FamilyIO->new;

    # TODO, write writeDESC2Array function, no need to output to file
    $io->writeDESC($desc);
    open(IN, "DESC") || die "ERROR unable to open DESC"; 
    my $line;
    my $tag;
    my $value;
    while($line = <IN>) { 
	chomp $line;
	if($line =~ /(^\S+)\s+(.*$)/) { 
	    ($tag, $value) = ($1, $2);
	    $msa->addGF($tag, $value);
	}
	else { 
	    die "ERROR unable to parse DESC line $line"; 
	}
    }
    $msa->write_msa("$family.sto.ann");
}
1;
