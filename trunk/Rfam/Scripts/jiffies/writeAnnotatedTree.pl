#!/usr/bin/env perl
# 
# writeAnnotatedSeed.pl:  Generates annotated SEED files for use as release flatfiles
#
# 
use strict;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::CM;
use Bio::Rfam::Config;
use Bio::Rfam::SVN::Client;
use Data::Printer;

my $config = Bio::Rfam::Config->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});
my $rfamdb = $config->rfamlive;

my $family = $ARGV[0];

my $familyIO = Bio::Rfam::FamilyIO->new;
my $familyObj = $familyIO->loadRfamFromSVN($family, $client);

my $msa = $familyObj->SEED;

$msa->write_msa("$family.seed","afa");
my $tree = "$family.tree";
my $bittree = "$family.bittree";
my $taxtree = "$family.taxtree";
my $cmd = "FastTree -nt -nj -boot 100 < $family.seed > $tree 2> /dev/null";

system ($cmd) == 0 or croak ("Problem with generating the seed tree....this is not good!\n");


addBitscoreToTree($msa, $rfamdb, $tree, $family);
specifyTree($msa, $rfamdb ,$bittree,$taxtree); 

sub addBitscoreToTree {
        my ($self, $rfamdb, $tree, $family) =@_;
        my %nseToBitscore;
        for (my $i = 0; $i < $self->nseq; $i++) {
                my $nse = $self->get_sqname($i);
                my ($is_nse, $name, $start, $end) = Bio::Rfam::Utils::nse_breakdown($nse);
                if (! $is_nse) {die "ERROR $nse not in name/start-end format:"}
                my $sth = $rfamdb->prepare_nseFamilyToBitscore;
                $sth->execute($family, $name, $start, $end);
                my $row = $sth->fetchrow_hashref;
                $row->{'nse'} = $nse;
                $nseToBitscore{$nse} = $row->{'bit_score'};
        }
        open (TREE, "<$tree") or croak ("Can't open tree file...this is VERY BAD!\n");
        my @ar = <TREE>;
        my $t = join ("", @ar);
        foreach my $k (keys %nseToBitscore) {
                $t =~ s/($k)/$nseToBitscore{$k}_$1/g;
        }
        open (BITTREE, ">>$bittree");
        print BITTREE "$t\n";
        close (BITTREE);
}
sub specifyTree {
        my ($self, $rfamdb, $bittree,$taxtree) = @_;
        my %seenSpecies;
        my %accToSpecies;

#Get sequence identifiers from the seed and query the database for the species (align_display_name)     

        my $sth = $rfamdb->prepare_seqaccToTaxon;
        for( my $i = 0; $i < $self->nseq; $i++){
                 my $nse = $self->get_sqname($i);
        my ($is_nse, $name, $start, $end) = Bio::Rfam::Utils::nse_breakdown($nse);
        if(! $is_nse) { die "ERROR $nse not in name/start-end format"; }
        
        $sth->execute($name);
        my $row = $sth->fetchrow_hashref;
        if(!exists($seenSpecies{$row->{align_display_name}})){
                $seenSpecies{$row->{align_display_name}} = 1;
        }
        my $speciesName = $row->{align_display_name}.'.'.$seenSpecies{$row->{align_display_name}};
        
        $accToSpecies{$nse} = $speciesName;
        $seenSpecies{$row->{align_display_name}}++;
        }
        


#Now get the original tree and replace all the accessions with the species:

        open(BITTREE, "<$bittree") or croak ("Can't open tree file to convert seqs to species...this is not good!\n");
        my @ar =<BITTREE>;
        my $t = join("", @ar);
        foreach my $k (keys %accToSpecies){
        $t =~ s/($k)/$1_$accToSpecies{$k}/g;
    }
        open(TAXTREE, ">>$taxtree");
        print TAXTREE "$t\n";
}
