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

# test sqfile
my $sqfile2 = $sqfile->sqfile;
isa_ok($sqfile2, "ESL_SQFILE");

# test path
my $path = $sqfile->path;
is($path, "./t/data/trna-100.fa");

# test fetch_subseq_to_fasta_string with no line length
my $seqstring = $sqfile->fetch_subseq_to_fasta_string("tRNA5-sample33", 13, 31);
is ($seqstring, ">tRNA5-sample33/13-31\nAAGUGGCAUCGCACUUGAC\n");

# test fetch_subseqs
my @AA = ();
@{$AA[0]} = ("tRNA5-sample33/13-31", 13, 31, "tRNA5-sample33");
@{$AA[1]} = ("revcomp", 31, 13, "tRNA5-sample33");
$seqstring = $sqfile->fetch_subseqs(\@AA, -1);
is ($seqstring, ">tRNA5-sample33/13-31\nAAGUGGCAUCGCACUUGAC\n>revcomp\nGTCAAGTGCGATGCCACTT\n");

# fetch same subseqs to a temporary file 
my $tmpfile = "t/data/tmp.fa";
$seqstring = $sqfile->fetch_subseqs(\@AA, 60, $tmpfile);
# open it 
my $tmpsqfile = Bio::Easel::SqFile->new({
   fileLocation => $tmpfile, 
});
isa_ok($tmpsqfile, "Bio::Easel::SqFile");

# now fetch these seq again, revcomp it
@AA = ();
@{$AA[0]} = ("revrevcomp", 19, 1, "revcomp"); 
$seqstring = $tmpsqfile->fetch_subseqs(\@AA, -1);
is ($seqstring, ">revrevcomp\nAAGTGGCATCGCACTTGAC\n");

# clean up files we just created
unlink ($tmpfile);
unlink ($tmpfile . ".ssi");

