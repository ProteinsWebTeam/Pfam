#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use Bio::SeqIO;
use Rfam::UpdateRDB;

my $dbname = 'rfamlive';
my $noempty;
GetOptions( "db=s" => \$dbname,
	    "noempty" => \$noempty );

my $db = Rfam::UpdateRDB->new( '-db_name' => $dbname,
			       '-db_driver' => 'mysql',
			       '-db_host' => 'pfam',
			       '-db_user' => 'rfam',
			       '-db_password' => 'mafp1' );

$db->empty_tables( ['rfamseq'] ) if( !$noempty );

foreach my $file ( @ARGV ) {
    if( $file =~ /.gz$/ ) {
	open( F, "gunzip -c $file|" ) or die;
    }
    else {
	open( F, $file ) or die;
    }
    my $in = Bio::SeqIO->new( -fh => \*F,
			      -format => 'embl' );
    while( my $s = $in->next_seq ) {
	$db->add_rfamseq( $s );
    }
}
