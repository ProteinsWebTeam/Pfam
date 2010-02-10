#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

unless ( $ARGV[0] ) {
  help();
}

my $clan = shift;

if ( $clan !~ /^(CL\d{4})$/ ) {
    if($config->location eq 'WTSI'){
	my $connect = $config->pfamlive;
	my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $connect });
	my $clanAcc = $pfamDB->clanId2Acc($clan);
	unless($clanAcc =~ /^CL\d{4}/){
	    warn "You passed in something that did not look like a clan accession.\n"; 
	    warn "Because you are at WTSI, tried to map it to a clan accession, but failed.\n";
	    help();
	}
	$clan = $clanAcc;  
    }
    else {
	warn "Looks like you have passed in a clan id rather than a clan accession, [$clan]\n";
        help();
    }
}

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkClanExists($clan, $config);
$client->log($clan);
$client->catFile( $clan, "CLANDESC" );

sub help {

  print<<EOF;

usage: $0 <CLAN ACCESSION>

Prints the SVN revision history for the family. 

EOF
 

exit;  
}
