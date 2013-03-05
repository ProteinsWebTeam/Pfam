use strict;
use warnings FATAL => 'all';
use Test::More tests => 4;


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

# test fetch_seq
my $outfile = "./t/data/out.fa";
$sqfile->fetch_seq("CP000857.1/1802194-1802277", $outfile);
unlink $outfile


