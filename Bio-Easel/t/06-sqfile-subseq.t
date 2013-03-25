use strict;
use warnings FATAL => 'all';
use Test::More tests => 5;


BEGIN {
    use_ok( 'Bio::Easel::SqFile' ) || print "Bail out!\n";
}

# test new 
my $infile = "./t/data/trna-100.fa";
my $sqfile = Bio::Easel::SqFile->new({
   fileLocation => $infile, 
});
isa_ok($sqfile, "Bio::Easel::SqFile");

# test sqfile
my $sqfile2 = $sqfile->sqfile;
isa_ok($sqfile2, "ESL_SQFILE");

# test path
my $path = $sqfile->path;
is($path, "./t/data/trna-100.fa");

# test fetch_subseq_to_fasta_string with no line length
my $seqstring = $sqfile->fetch_subseq_to_fasta_string("tRNA5-sample33", 13, 31);
is ($seqstring, ">tRNA5-sample33/13-31\nAAGUGGCAUCGCACUUGAC\n");
