use strict;
use warnings FATAL => 'all';
use Test::More tests => 6;


BEGIN {
    use_ok( 'Bio::Easel::SQFILE' ) || print "Bail out!\n";
}

# test new 
my $infile = "./t/data/test.fa";
my $sqfile = Bio::Easel::SQFILE->new({
   fileLocation => $infile, 
});
isa_ok($sqfile, "Bio::Easel::SQFILE");

# test sqfile
my $sqfile2 = $sqfile->sqfile;
isa_ok($sqfile2, "ESL_SQFILE");

# test path
my $path = $sqfile->path;
is($path, "./t/data/test.fa");

# test fetch_seq_to_fasta_string with no line length
my $seqstring = $sqfile->fetch_seq_to_fasta_string("CP000857.1/1802194-1802277");
is ($seqstring, ">CP000857.1/1802194-1802277\nCUCACAUCAGAUUUCCUGGUGUAACGAAUUUUCAAGUGCUUCUUGCAUAAGCAAGUUUGA\nUCCCGACCCGUAGGGCCGGGAUUU\n");

# test fetch_seq_to_fasta_string with unlimited line length
$seqstring = $sqfile->fetch_seq_to_fasta_string("CP000857.1/1802194-1802277", -1);
is ($seqstring, ">CP000857.1/1802194-1802277\nCUCACAUCAGAUUUCCUGGUGUAACGAAUUUUCAAGUGCUUCUUGCAUAAGCAAGUUUGAUCCCGACCCGUAGGGCCGGGAUUU\n");



