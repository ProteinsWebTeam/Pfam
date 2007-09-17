#!/software/bin/perl -w

use strict;
use Bio::Index::Fasta;

my $inxfile = shift;
my $inx = Bio::Index::Fasta->new( $inxfile, 'WRITE' );

$inx -> id_parser( \&sv_parser );
$inx -> make_index( @ARGV );

# enables us to add keys as accession AND accession.version - cool!
sub sv_parser {
    if( $_[0] =~ /^>\s*(\S+)\.(\d+)/ ) {
        return $1, "$1.$2";
    } else {
        return;
    }
}
