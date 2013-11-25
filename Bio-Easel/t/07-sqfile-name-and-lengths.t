use strict;
use warnings FATAL => 'all';
use Test::More tests => 8;


BEGIN {
    use_ok( 'Bio::Easel::SqFile' ) || print "Bail out!\n";
}

# test new 
my $infile = "./t/data/trna-100.fa";
my $sqfile = Bio::Easel::SqFile->new({
   fileLocation => $infile, 
});
isa_ok($sqfile, "Bio::Easel::SqFile");

# test nseq_ssi
my $nseq;
$nseq = $sqfile->nseq_ssi();
is ($nseq, 100);

# test fetch_seq_name_and_length_given_ssi_number
my ($seqname, $L);
($seqname, $L) = $sqfile->fetch_seq_name_and_length_given_ssi_number(33);
is ($seqname, "tRNA5-sample39");
is ($L, 72);

# test fetch_seq_name_given_ssi_number
$seqname = $sqfile->fetch_seq_name_given_ssi_number(40);
is ($seqname, "tRNA5-sample45");

# test fetch_seq_name_given_ssi_number
$L = $sqfile->fetch_seq_length_given_ssi_number(40);
is ($L, 73);

# test fetch_seq_length_given_name
my $len = $sqfile->fetch_seq_length_given_name("tRNA5-sample31");
is ($len, 73, "fetch_seq_length_given_name() failed");
