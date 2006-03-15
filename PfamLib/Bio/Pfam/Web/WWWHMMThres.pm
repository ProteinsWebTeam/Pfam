
#
# Perl Module for HMMThres
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package Bio::Pfam::Web::WWWHMMThres;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use HMMThresUnit;
use HMMSequence;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' );



my %fields = (
	      families => undef,
	      accmap   => undef,
    #Insert field names here as field => undef,
);


sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	'_permitted' => \%fields,
	%fields, };
    $self->{'families'} = {};
    bless $self, $class;
    return $self;
}


sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) || carp "$self is not an object - can't therefore find a member!";
    my $name = $AUTOLOAD;
    $name =~ /::DESTROY/ && return;
    $name =~ s/.*://;
    unless (exists $self->{'_permitted'}->{$name} ) {
	carp "In type $type, can't access $name - probably passed a wrong variable into HMMThres";
    }
    if (@_) {
	return $self->{$name} = shift;
    } else {
	return $self->{$name};
    }
}

sub addThresUnit {
    my $self = shift;
    my $unit = shift;
    $self->{'families'}->{$unit->name()} = $unit;
    $self->{'accmap'}->{$unit->acc()} = $unit;
}

sub getThresUnit_name {
    my $self = shift;
    my $name = shift;   
    return $self->{'families'}->{$name};
}

sub getThresUnit_acc {
    my $self = shift;
    my $name = shift;

    return $self->{'accmap'}->{$name};
}

sub filter_results_acc {
    my $self = shift;
    my $hmmres = shift;
    my ($new,$out,$seq,$unit,$touched,$domthr,$seqthr,$thr,$nseq,$name);

    $new = $hmmres->new(); # make sure we use the correct class! funky-skunky!
    foreach $seq ( $hmmres->eachHMMSequence()) {
	$nseq = new HMMSequence;
	$nseq->name($seq->name());

	$new->addHMMSequence($nseq);

	foreach $unit ( $seq->eachHMMUnit() ) {
	    $name = $unit->hmmname();

	    if( ! defined $self->getThresUnit_name($name) ) {
		carp "Cannot get a threshold for $unit->{'hmmname'} for $name";
		next;
	    }
#	    print "NAME: $name <BR>";
	    $thr = $self->getThresUnit_name($name); 

#	    # we need to set the accession number of the unit; everything hangs on this...
	    $unit->hmmacc( $thr->acc() ); 

#	    print sprintf("Looking at %f vs %f and %f vs %f<p>\n",$unit->seqbits(),$thr->seq(),$unit->bits(),$thr->dom());
#	    print "SEQ: " . $thr->seq(). " , BITS: " .$thr->dom(). " <BR>";
	    if( ($unit->seqbits() >= $thr->seq()) && ($unit->bits() >= $thr->dom()) ) {

		$new->addHMMUnit($unit); 
	    }
	}

    }
    return $new;
}

sub filter_negative_acc {
    my $self = shift;
    my $hmmres = shift;
    my ($new,$out,$seq,$unit,$touched,$domthr,$seqthr,$thr,$nseq,$name);

    $new = $hmmres->new(); # make sure we use the correct class! funky-skunky!
    foreach $seq ( $hmmres->eachHMMSequence()) {
	$nseq = new HMMSequence;
	$nseq->name($seq->name());

	$new->addHMMSequence($nseq);
	
	foreach $unit ( $seq->eachHMMUnit() ) {

	    $name = $unit->hmmname();
	    
	    if( ! defined $self->getThresUnit_name($name) ) {
		carp "Cannot get a threshold for $name";
		next;
	    }

	    $thr = $self->getThresUnit_name($name);
	    # we need to set the accession number of the unit; everything hangs on this...
	    $unit->hmmacc( $thr->acc() );


	    if( !(($unit->seqbits() >= $thr->seq()) && ($unit->bits() >= $thr->dom())) ) {
		$new->addHMMUnit($unit);
	    }
	}

    }
        
    return $new;
}





sub read_ga_nc {
    my $file = shift;
    my ($ls_ga,$ls_nc, $fs_ga,$fs_nc , $unit,$name,$acc,  $ls_gas,$ls_gad,$ls_ncs,$ls_ncd, $fs_gas,$fs_gad,$fs_ncs,$fs_ncd  );
    
    $ls_ga = new Bio::Pfam::Web::WWWHMMThres;
    $ls_nc = new Bio::Pfam::Web::WWWHMMThres;
    $fs_ga = new Bio::Pfam::Web::WWWHMMThres;
    $fs_nc = new Bio::Pfam::Web::WWWHMMThres;
	my $db = &Bio::Pfam::Web::PfamWWWConfig::get_database();
	my @res = $db->query("select pfamA_acc, pfamA_id, ls_sequence_GA, ls_domain_GA, fs_sequence_GA, fs_domain_GA, ls_sequence_NC, ls_domain_NC, fs_sequence_NC, fs_domain_NC from pfamA");
	foreach (@res) {
	my($acc, $name, $ls_gas, $ls_gad, $fs_gas, $fs_gad, $ls_ncs, $ls_ncd, $fs_ncs, $fs_ncd) = @{$_};
#	print "EEP: $ls_sequence_GA, $ls_domain_GA, $fs_sequence_GA, $fs_domain_GA, $ls_sequence_NC, $ls_domain_NC, $fs_sequence_NC, $fs_domain_NC <BR>";
	#}
	#print "DONE <P>";
    #while(<$file>) {
#	($acc,$name,$gas,$gad,$ncs,$ncd) = split(/[\s:]+/);
     # ($acc,$name,$ls_gas,$ls_gad,$ls_ncs,$ls_ncd, $fs_gas,$fs_gad,$fs_ncs,$fs_ncd) = split(/[\s:]+/);
 
      #chop($ls_ncd);
  #    print "$acc,$name,$ls_gas,$ls_gad,$ls_ncs,$ls_ncd, $fs_gas,$fs_gad,$fs_ncs,$fs_ncd <BR>";
#	print "now...Adding $name $acc<p> : $_<p>";

      #    print "$acc,$name,$ls_gas,$ls_gad,$ls_ncs,$ls_ncd, $fs_gas,$fs_gad,$fs_ncs,$fs_ncd <P>";
      
	$unit = new HMMThresUnit;
	$unit->name($name);
	$unit->acc($acc);
	$unit->seq($ls_gas);
	$unit->dom($ls_gad);
	$ls_ga->addThresUnit($unit);


	$unit = new HMMThresUnit;
	$unit->name($name);
	$unit->acc($acc);
	$unit->seq($ls_ncs);
	$unit->dom($ls_ncd);
	$ls_nc->addThresUnit($unit);

	$unit = new HMMThresUnit;
	$unit->name($name);
	$unit->acc($acc);
	$unit->seq($fs_gas);
	$unit->dom($fs_gad);
	$fs_ga->addThresUnit($unit);


	$unit = new HMMThresUnit;
	$unit->name($name);
	$unit->acc($acc);
	$unit->seq($fs_ncs);
	$unit->dom($fs_ncd);
	$fs_nc->addThresUnit($unit);


    }
#print "DONE <P>"; 
    return ($ls_ga,$ls_nc, $fs_ga,$fs_nc);

}

1;  # says use was ok
__END__

=head1 NAME

HMMThres

=head1 DESCRIPTION

Description for B<HMMThres>

=head1 AUTHOR

B<Ewan Birney> Email birney@sanger.ac.uk

=over

=item addThresUnit

No current documentation

=item getThresUnit

No current documentation

=item read_ga_nc

No current documentation

=item filter_results

No current documentation

=item filter_negative_acc

No current documentation
