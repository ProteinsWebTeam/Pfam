use strict;
use warnings FATAL => 'all';
use Test::More tests => 11;

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

# test fetch_seq_to_fasta_string with no line length
my $seqstring = $sqfile->fetch_seq_to_fasta_string("tRNA5-sample33");
is ($seqstring, ">tRNA5-sample33\nAUAACCACAGCGAAGUGGCAUCGCACUUGACUUCCGAUCAAGAGACCGCGGUUCGAUUCC\nGCUUGGUGAUA\n");

# test fetch_seq_to_fasta_string with unlimited line length
$seqstring = $sqfile->fetch_seq_to_fasta_string("tRNA5-sample33", -1);
is ($seqstring, ">tRNA5-sample33\nAUAACCACAGCGAAGUGGCAUCGCACUUGACUUCCGAUCAAGAGACCGCGGUUCGAUUCCGCUUGGUGAUA\n");

# test fetch_consecutive_seqs
$seqstring = $sqfile->fetch_consecutive_seqs(3, "", 60);
is ($seqstring,">tRNA5-sample34\nGUCCACAAAGCGUAAUGGUCAGCGUAGCCAACCUCAAGUUGGCAGGUCUUUGUUCGAUUC\nACAGUGUGGAC\n>tRNA5-sample35\nUCAAGGGGCGUAACUUCGGUAGCGUACCUUUCUGGCAAGAGGGAGAUUUGGGGUUCAACU\nCCCUACUUGAU\n>tRNA5-sample36\nUUGCCGAUGCGCCAGUGGGGAGGCGGACGUUCUGUCACUACGUAGGUCCGUUGUUCAAUA\nCAGUGUCGGCAAC\n");

# create seq file
my $tmpfile = "t/data/tmp.trna-30.fa";
$sqfile->fetch_consecutive_seqs(30, "tRNA5-sample31", 60, $tmpfile);
# open it 
my $tmpsqfile = Bio::Easel::SqFile->new({
   fileLocation => $tmpfile, 
});
isa_ok($tmpsqfile, "Bio::Easel::SqFile");

# test fetch_seq_to_fasta_string with no line length
$seqstring = $tmpsqfile->fetch_seq_to_fasta_string("tRNA5-sample33");
is ($seqstring, ">tRNA5-sample33\nAUAACCACAGCGAAGUGGCAUCGCACUUGACUUCCGAUCAAGAGACCGCGGUUCGAUUCC\nGCUUGGUGAUA\n");

# test fetch_consecutive_seqs
$seqstring = $tmpsqfile->fetch_consecutive_seqs(3, "", 60);
is ($seqstring,">tRNA5-sample34\nGUCCACAAAGCGUAAUGGUCAGCGUAGCCAACCUCAAGUUGGCAGGUCUUUGUUCGAUUC\nACAGUGUGGAC\n>tRNA5-sample35\nUCAAGGGGCGUAACUUCGGUAGCGUACCUUUCUGGCAAGAGGGAGAUUUGGGGUUCAACU\nCCCUACUUGAU\n>tRNA5-sample36\nUUGCCGAUGCGCCAGUGGGGAGGCGGACGUUCUGUCACUACGUAGGUCCGUUGUUCAAUA\nCAGUGUCGGCAAC\n");

# close file, and then fetch again, which should open it 
# note we get first three seqs this time
$tmpsqfile->close_sqfile();
$seqstring = $tmpsqfile->fetch_consecutive_seqs(3, "", 60);
is ($seqstring, ">tRNA5-sample31\nGCUGACUUAUCGGAGAAGGCCACUAGGGGAGCUUGCCAUGCUUUCUACUCGAGCGCGAUC\nCUCGAAGUCAGCG\n>tRNA5-sample32\nUCGGCCUUGGUGUAAUGGUGUAUCACGGGAGGUUGCCGUCCUCCUAGGACCGGUUGGAUC\nCCGGUAGGCUGAC\n>tRNA5-sample33\nAUAACCACAGCGAAGUGGCAUCGCACUUGACUUCCGAUCAAGAGACCGCGGUUCGAUUCC\nGCUUGGUGAUA\n");

# clean up files we just created
unlink ($tmpfile);
unlink ($tmpfile . ".ssi");


