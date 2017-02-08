#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::SVN::Client;
use Data::Printer;
use RfamLive;

my $config = Bio::Rfam::Config->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});
# create a new connection to RfamLive
my $rfamdb = $config->rfamlive;
my $familyIO = Bio::Rfam::FamilyIO->new;


my $rfam_acc;

if (@ARGV==1){
	$rfam_acc=$ARGV[0];
	
	# look for a family entry in the database
	my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );
	
	if (!defined($famRow)) {
		croak ("Failed to find entry in the Family table for $rfam_acc.");
	}

   	# load family object from the svn 
	my $familyObj = $familyIO->loadRfamFromSVN($rfam_acc, $client);
	# update database_link table
	$rfamdb->resultset('DatabaseLink')->find_or_createFromFamilyObj($familyObj);
}

# fetch families with no GO terms and update database 
else{

	#fetch family accessions
	my %families;
	my $rfamdbh=$rfamdb->storage->dbh;

	my $query=qq(select distinct rfam_acc from database_link
		     where rfam_acc not in (select distinct rfam_acc from database_link where db_id='GO') 
		     and rfam_acc > 'RF02545';);

	my $db_cursor=$rfamdbh->prepare($query);
	$db_cursor->execute();

	my $results=$db_cursor->fetchall_arrayref;
	
	foreach my $entry (@$results){
    		$families{$entry->[0]}=1;
	}
	
	# loop over all family accessions
	foreach my $rfam_acc (keys %families){
        	# load family from the SVN repository 
        	my $familyObj = $familyIO->loadRfamFromSVN($rfam_acc, $client);
		print $familyObj->{DESC}->{AC};
		# update database_link table 
		$rfamdb->resultset('DatabaseLink')->find_or_createFromFamilyObj($familyObj);
        }
}
