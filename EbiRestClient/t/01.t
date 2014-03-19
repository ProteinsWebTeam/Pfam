
use strict;
use warnings FATAL => 'all';

use Test::More; # tests => 2;
use Test::Exception;
use JSON qw( from_json );

BEGIN {
  use_ok( 'Bio::Pfam::EbiRestClient' );
}

my $rc = Bio::Pfam::EbiRestClient->new;
isa_ok( $rc, 'Bio::Pfam::EbiRestClient', 'got a client object' );

isa_ok( $rc->_ua, 'LWP::UserAgent', 'got a LWP::UserAgent from the object' );

ok( $rc->base_url( 'http://www.ebi.ac.uk/Tools/services/rest/pfamscan' ), 'can set base_url to valid URL' );
throws_ok { $rc->base_url('wibble') } qr/does not pass the type constraint/, "can't set base_url to invalid URL";

ok( ( defined( $rc->_can_accept ) and $rc->_can_accept ne '' ), '"can_accept" is sensible' );

throws_ok { $rc->_request( { method => 'WIBBLE', path => 'parameters' } ) } qr/The 'method' parameter/,  'invalid request method fails';

my $content;
lives_ok { $content = $rc->_request( { method => 'GET', path => 'parameters' } ) } 'valid request method';
ok( $content =~ m/<parameters>/, 'retrieved parameters list' );

my $valid_sequence = 'MAGAASPCANGCGPSAPSDAEVVHLCRSLEVGTVMTLFYSKKSQRPERKTFQVKLETRQITWSRGADKIEGAIDIREIKEIRPGKTSRDFDRYQEDPAFRPDQSHCFVILYGMEFRLKTLSLQATSEDEVNMWIRGLTWLMEDTLQAATPLQIERWLRKQFYSVDRNREDRISAKDLKNMLSQVNYRVPNMRFLRERLTDLEQRTSDITYGQFAQLYRSLMYSAQKTMDLPFLEASALRAGERPELCRVSLPEFQQFLLEYQGELWAVDRLQVQEFMLSFLRDPLREIEEPYFFLDEFVTFLFSKENSIWNSQLDEVCPDTMNNPLSHYWISSSHNTYLTGDQFSSESSLEAYARCLRMGCRCIELDCWDGPDGMPVIYHGHTLTTKIKFSDVLHTIKEHAFVASEYPVILSIEDHCSIAQQRNMAQYFKKVLGDTLLTKPVDIAADGLPSPNQLKRKILIKHKKLAEGSAYEEVPTSVMYSENDISNSIKNGILYLEDPVNHEWYPHYFVLTSSKIYYSEETSSDQGNEDEEEPKEASGSTELHSNEKWFHGKLGAGRDGRHIAERLLTEYCIETGAPDGSFLVRESETFVGDYTLSFWRNGKVQHCRIHSRQDAGTPKFFLTDNLVFDSLYDLITHYQQVPLRCNEFEMRLSEPVPQTNAHESKEWYHASLTRAQAEHMLMRVPRDGAFLVRKRNEPNSYAISFRAEGKIKHCRVQQEGQTVMLGNSEFDSLVDLISYYEKHPLYRKMKLRYPINEEALEKIGTAEPDYGALYEGRNPGFYVEANPMPTFKCAVKALFDYKAQREDELTFTKSAIIQNVEKQEGGWWRGDYGGKKQLWFPSNYVEEMVSPAALEPEREHLDENSPLGDLLRGVLDVPACQIAVRPEGKNNRLFVFSISMASVAHWSLDVAADSQEELQDWVKKIREVAQTADARLTEGKMMERRKKIALELSELVVYCRPVPFDEEKIGTERACYRDMSSFPETKAEKYVNKAKGKKFLQYNRLQLSRIYPKGQRLDSSNYDPLPMWICGSQLVALNFQTPDKPMQMNQALFLAGGHCGYVLQPSVMRDEAFDPFDKSSLRGLEPCAICIEVLGARHLPKNGRGIVCPFVEIEVAGAEYDSIKQKTEFVVDNGLNPVWPAKPFHFQISNPEFAFLRFVVYEEDMFSDQNFLAQATFPVKGLKTGYRAVPLKNNYSEGLELASLLVKIDVFPAKQENGDLSPFGGASLRERSCDASGPLFHGRAREGSFEARYQQPFEDFRISQEHLADHFDGRDRRTPRRTRVNGDNRL';

my $valid_fasta = <<EOF_fasta;
>seq
$valid_sequence
EOF_fasta

my $invalid_sequence = <<EOF_invalid_sequence;
>seq
M?GAASPCANGCGPSAPSDAEVVHLCRSLEVGTVMTLFYSKKSQRPERKTFQVKLETRQITWSRGADKIEGAIDIREIKEIRPGKTSRDFDRYQEDPAFRPDQSHCFVILYGMEFRLKTL
EOF_invalid_sequence
chomp $invalid_sequence;

throws_ok { $rc->_validate_search_params( { sequence => $invalid_sequence } ) } qr/^The 'sequence' parameter/, 'correctly failed to validate search params with invalid sequence';
lives_ok  { $rc->_validate_search_params( { sequence => $valid_sequence   } ) } 'successfully validated search params with valid plain sequence';
lives_ok  { $rc->_validate_search_params( { sequence => $valid_fasta      } ) } 'successfully validated search params with valid FASTA sequence';

throws_ok { $rc->_validate_search_params( { sequence => $valid_sequence, evalue => 'a'  } ) } qr/^The 'evalue' parameter/, 'correctly failed to validate search params with invalid E-value (not a number)';
throws_ok { $rc->_validate_search_params( { sequence => $valid_sequence, evalue => -1.0 } ) } qr/^The 'evalue' parameter/, 'correctly failed to validate search params with invalid E-value (negative)';
lives_ok  { $rc->_validate_search_params( { sequence => $valid_sequence, evalue => 1.0  } ) } 'successfully validated search params with valid E-value';

throws_ok { $rc->_validate_search_params( { sequence => $valid_sequence, database => 'x' } ) } qr/^The 'database' parameter/, 'correctly failed to validate search params with invalid database';
lives_ok  { $rc->_validate_search_params( { sequence => $valid_sequence, database => 'pfam-a' } ) } 'successfully validated search params with valid database';

my $request_params = $rc->_validate_search_params( { sequence => $valid_sequence, database => 'pfam-a' } );
my $expected_params = {
  cutOffOption => 'ga-score',
  database     => 'pfam-a',
  email        => 'xfam@ebi.ac.uk',
  sequence     => $valid_sequence,
  format       => undef,
};
is_deeply( $request_params, $expected_params, 'got expected request parameters (GA cutoff)' );

$request_params = $rc->_validate_search_params( { sequence => $valid_sequence, evalue => 0.01, database =>'pfam-a' } );
$expected_params = {
  evalue       => 0.01,
  database     => 'pfam-a',
  email        => 'xfam@ebi.ac.uk',
  sequence     => $valid_sequence,
  format       => undef,
};
is_deeply( $request_params, $expected_params, 'got expected request parameters (E-value cutoff)' );

is( $rc->status('pfamscan-R20130902-170903-0922-57574401'), 'NOT_FOUND', 'got "NOT_FOUND" for non-existent job' );
#                job ID deliberately broken here -------^

my ( $job_id, $status, $output, $json );

SKIP: {
  skip "straight to RNA search", 8 if $ENV{NO_PROTEIN_SEARCH};

  lives_ok { $job_id = $rc->search( { sequence => $valid_sequence, format => 'json' } ) } 'successfully submitted a search';

  like( $job_id, qr/^pfamscan-[\w+\-]+(oy|pg|hx|es)/, 'got a valid job ID' );

  diag( "job ID: $job_id" );

  $status = $rc->status($job_id);
  diag( "status: $status" );

  ok( ( $status eq 'RUNNING' or $status eq 'FINISHED' ), 'got status correctly' );

  diag( 'sleeping for 20 seconds to allow job to complete' );
  sleep 20;

  is( $rc->status($job_id), 'FINISHED', 'got "FINISHED" status correctly' );

  ok( $output = $rc->result( $job_id ), 'got results successfully' );

  lives_ok { $json = from_json( $output ) } 'successfully parsed JSON output';

  cmp_ok( scalar @$json, ">=", 1, 'search found at least one match to sequence' );
  cmp_ok( $json->[0]->{model_length}, ">=", 1, 'first hit has an expected attribute' );
}

# to test the output format URL argument, we need to use a different search service...
$rc->base_url( 'http://wwwdev.ebi.ac.uk/Tools/services/rest/infernal_cmscan' );

# and a different test sequence
my $rna_sequence = 'AGTTACGGCCATACCTCAGAGAATATACCGTATCCCGTTCGATCTGCGAAGTTAAGCTCTGAAGGGCGTCGTCAGTACTATAGTGGGTGACCATATGGGAATACGACGTGCTGTAGCTT';

lives_ok { $job_id = $rc->search( { sequence => $rna_sequence } ) } 'successfully submitted an RNA search';

diag( 'sleeping for 20 seconds to allow job to complete' );
sleep 20;

ok( $output = $rc->result( $job_id, 'out' ), 'got results successfully' );
like( $output, qr/INFERNAL/, 'text output looks right' );

$output = $rc->result( $job_id, 'tblout' );
unlike( $output, qr/INFERNAL/, 'tblout output looks right' );

done_testing();

