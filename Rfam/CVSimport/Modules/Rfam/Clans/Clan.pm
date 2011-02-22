package Rfam::Clans::Clan; 

#methods in this package:
#slurpClanDesc 
#writeClanDesc 
#generateClanDesc 
#check_into_DB 
#checkForExistingLitRefEntry


use strict;
use warnings;
use Cwd;
use DBI;

use Rfam;
use RfamUtils; 

use vars qw( 
             @ISA
             @EXPORT
             @clanTags
             @clanLitTags
);

@ISA    = qw( Exporter );

#loads into rfamlive
my @rfamDatabase = ("dbi:mysql:rfamlive:$Rfam::rdb_host:$Rfam::rdb_port", $Rfam::rdb_user, $Rfam::rdb_pass);

#Tags used in CLANDESC files -- the order here matters!!!:
@clanTags = qw(
AC
ID
DE
PI
AU
DR
RN
CC
WK
MB
**
);

@clanLitTags = qw(
RA
RL
RM
RT
);

######################################################################
#slurpDesc: read a CLANDESC into a hash.
#           Input is a CLANDESC filename, Output is a pointer to a hash.
sub slurpClanDesc {
    
    my $descFile=shift;
    $descFile='CLANDESC' if not defined $descFile;
    my %desc;
    
    foreach my $t (@clanTags){
	$desc{$t}='';
    }
    
    my %litTags;
    foreach my $t (@clanLitTags){
	$litTags{$t}='';
    }
    
    open(DE, "< $descFile") or die "FATAL: failed to open $descFile file!\n[$!]";
    my ($refNo, @refs, @members);
    while(<DE>){
	chomp;
	if(/^RN\s+\[(\d+)\]/){
	    $refNo=int($1)-1 if defined $1 && $1>0 && length($1)>0;
	}
	elsif(/^(R\S)\s+(.+)/ && defined $refNo){#	elsif((/^(R\S)\s+(.+)\s+/ || /^(R\S)\s+(.+)/) && defined $refNo){
	    next if length($refNo) == 0 or length($1) == 0 or length($2) == 0;
	    if (not defined $refs[$refNo]{$1}){
		$refs[$refNo]{$1} = $2;
	    }
	    else {
		$refs[$refNo]{$1} .= "\n$1   " . $2;
	    }
	}
	elsif(/^(R\S)\s+(.+)/ && not defined $refNo){
	    print STDERR "WARNING: your references are munged, you need an RN line first!\n";
	}
	elsif(/^MB\s+(RF\S+);/){
	    push(@members, $1);
	}
	elsif(/^(\S{2})\s+(.+)/){
	    if (defined $desc{$1} && length($desc{$1}) == 0){
		$desc{$1}.=$2;
	    }
	    elsif(defined $desc{$1}){
		$desc{$1}.="\n$1   " . $2;
	    }
	    else {
		print STDERR "WARNING: Strange Desc File line, [$1] is an undefined tag: [$_]\n";
	    }
	}
    }
    print STDERR "WARNING: your CLANDESC file contains no references!\n" if not defined $refNo;
    $desc{'RN'}=\@refs if defined $refNo;
    if (@members>1){#Need more than 1 memeber!
	$desc{'MB'}=\@members;
    }
    else {
	die "FATAL: your CLANDESC does not contain enough members! Add some lines like [MB   RF00001;].";
    }
    return \%desc;
}
    
###################################
#Add a check that essential tags have been written:
sub writeClanDesc {
    my $desc = shift;
    my $fp = shift;
    
    my %allowed;
    foreach my $t (@clanTags, @clanLitTags){
	$allowed{$t}='';
    }
    
    foreach my $t (@clanTags){
	
	#References are awkward:
	if ($t eq 'RN' && defined $desc->{'RN'}){
	    for (my $i=0; $i< scalar(@{$desc->{'RN'}}); $i++ ){
		my $ref = ${$desc->{'RN'}}[int($i)];
		printf $fp "RN   [%d]\n", $i+1;
		foreach my $rTag (qw(RM RT RA RL) ){
		    if (defined $ref->{$rTag} && length($ref->{$rTag})>0){
			$ref->{$rTag} =~ s/\t/ /g;
			print $fp "$rTag   " . $ref->{$rTag} . "\n";
			print STDERR "WARNING: a tab character or a terminal whitespace is screwing up one of your $rTag lines in your CLANDESC file!\n" if ($ref->{$rTag}=~/\s$/ || $ref->{$rTag}=~/\t/);
		    }
		    else {
			printf STDERR "WARNING: no $rTag entry for reference [%d]\n", $i+1;
		    }
		}
	    }
	}
	elsif ($t eq 'MB'){
	    foreach my $mb ( sort {$a cmp $b} @{$desc->{'MB'}} ){
		printf $fp "MB   %s;\n", $mb;
	    }
	}
	else {
	    #All the other tags are fairly easy to take care of:
	    if (defined $desc->{$t} && length($desc->{$t})>0){
		$desc->{$t} =~ s/\t/ /g;
		print $fp "$t   " . $desc->{$t} . "\n";
		print STDERR "WARNING: a tab character or a terminal whitespace is screwing up one of your $t lines in your CLANDESC file!\n" if ($desc->{$t}=~/\s$/ || $desc->{$t}=~/\t/);
	    }
	}
    }
    return 1;
}

###################################
sub generateClanDesc {
    
    my %desc;
    my %user2name = (
	agb  => 'Bateman A',
	jd7  => 'Daub J',
	pg5  => 'Gardner PP',
	sgj  => 'Griffiths-Jones SR',
	io3  => 'Osuch I',
	aw10 => 'Wilkinson A'
	);
    my $user =  getlogin() if defined getlogin();
    $user =  'pg5' if not defined $user2name{$user}; #This is soo evil! ;-)
    
    die "FATAL: CLANDESC file exists but may be empty, delete or fix script..." if -e 'CLANDESC';
    
    my $pwd = getcwd; 
    my @pwd = split(/\//,$pwd); 
    my $id=pop(@pwd);
    $id = 'ncRNA-like' if not defined $id or length($id)==0;
    $desc{'DE'}=$id;
    $desc{'AU'}=$user2name{$user};
    $desc{'WK'}='http://en.wikipedia.org/wiki/Non-coding_RNA';
    push(@{$desc{'MB'}},('RFXXXXX;', 'RFXXXXX;') );
    return \%desc;
}

######################################################################

#check_into_DB: Load the data contained in clandesc into the database:
sub check_into_DB {
    my $desc = shift;
    my $table;
    my $rfdbh = DBI->connect(
	@rfamDatabase, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
    
 

    my $ccLines = $desc->{CC};
    $ccLines =~ s/\nCC\s{3}/\n /g;
    $ccLines =~ s/\n//g;

    ###########################
    # 1 CLAN TABLE#
    ############################

    #check to see if clan exists:
    my $asth = $rfdbh->prepare( 'Select auto_clan from clans where clan_acc=?' )
	or die '(EE) ERROR: couldn\'t prepare query asth: ' . $rfdbh->errstr;
    
    my $bsth = $rfdbh->prepare( 'Replace into clans (auto_clan, clan_acc, clan_id, clan_description, clan_author, clan_comment) values (?,?,?,?,?,?)' )
	or die '(EE) ERROR: couldn\'t prepare query bsth: ' . $rfdbh->errstr;
    
    my $csth = $rfdbh->prepare( 'Insert into clans (clan_acc, clan_id, clan_description, clan_author, clan_comment) values (?,?,?,?,?)' )
	or die '(EE) ERROR: couldn\'t prepare query csth: ' . $rfdbh->errstr;

    my $autoClan;
    #check for existing entry
    $asth->execute($desc->{AC});
    $autoClan=$asth->fetchrow();
    $asth->finish();

    if ($autoClan){
	#do the replace
	$bsth->execute($autoClan, $desc->{AC},$desc->{ID},$desc->{DE},$desc->{AU},$ccLines);
	  print STDERR "(WW) WARNING: error whilst updating clans table: ". $rfdbh->errstr . "\n"if $DBI::err;
	$bsth->finish();
    }else{
	#insert and get autoclan
	$csth->execute($desc->{AC},$desc->{ID},$desc->{DE},$desc->{AU},$ccLines);
	$autoClan=$csth->{mysql_insertid};
	print STDERR "$autoClan autoclan new\n";
	print STDERR "(WW) WARNING: error whilst inserting new data  into clans table: ". $rfdbh->errstr . "\n"if $DBI::err;  
	$csth->finish();
    }
    
    if (!$autoClan){
	die "Problem with insert/update on the clans table...";
    }

    printf "Checking acc=[%s] id=[%s] de=[%s] into rfamlive RDB with auto_id $autoClan\n", $desc->{AC},$desc->{ID},$desc->{DE};
    
    #############################
    #2. clan_membership table
    ############################

    #Find the auto_rfam number for each member:
    my $sthRfQuery = $rfdbh->prepare('Select auto_rfam, type from rfam where rfam_acc=?' )
	or die '(EE) ERROR: couldn\'t prepare query sthRfQuery: ' . $rfdbh->errstr;

    my $dsth = $rfdbh->prepare ('Insert into clan_membership (auto_clan, auto_rfam) VALUES (?, ?)' )
	or die '(EE) ERROR: couldn\'t prepare query dsth: ' . $rfdbh->errstr;
    
    my %mb2auto_rfam;
    my %familytypes;
    my $type;
 
    foreach my $mb ( @{$desc->{MB}} ){
	
	$sthRfQuery->execute($mb);
	print STDERR "(WW) WARNING: error whilst retreiving data from the rfam table for $mb: ". $rfdbh->errstr . "\n"if $DBI::err;

	my $result = $sthRfQuery->fetchall_arrayref;
	my $rfamId='';
	foreach my $row (@$result){
	    #key rfamacc
	    $mb2auto_rfam{$mb} = $row->[0] if defined $row->[0];
	    $familytypes{$row->[1]}.=" $mb ";
	    
	}
    }

    $sthRfQuery->finish();

    if ( keys %familytypes > 1){
	print STDERR "\nWARNING: clan built from Rfam families of more than one type:\n";
	foreach my $k (keys (%familytypes)){
	    print STDERR "type=> ", $k, "   families=>", $familytypes{$k}, "\n"; 
	}
	print STDERR "\n";
    }
    
    #delete any existing data for this clan:
    my $delete_cm = $rfdbh->prepare( 'Delete from clan_membership where auto_clan=?' )
	or die '(EE) ERROR: couldn\'t prepare query to delete: ' . $rfdbh->errstr;
    $delete_cm->execute($autoClan);
    $delete_cm->finish();

    #Fill the clan_membership table:
    foreach my $mb ( @{$desc->{MB}} ){
	$dsth->execute($autoClan, $mb2auto_rfam{$mb}) if defined $mb2auto_rfam{$mb};
	warn "WARNING: failed to find an auto_rfam id for $mb!" if not defined $mb2auto_rfam{$mb};
    }

    $dsth->finish();
    print STDERR "Updated the clan_membership table\n";
    
    
    ##################################
    # 3 Database links table:
    #####################################


    my $gsth = $rfdbh->prepare('Insert into clan_database_links (auto_clan, db_id, comment, DB_link, other_params) VALUES (?, ?, ?, ?, ?)')
		or die '(EE) ERROR: couldn\'t prepare query fsth: ' . $rfdbh->errstr;

    #delete existing data
    my $delete_cdb = $rfdbh->prepare( 'Delete from clan_database_links where auto_clan=?' )
	or die '(EE) ERROR: couldn\'t prepare query to delete: ' . $rfdbh->errstr;
    $delete_cdb->execute($autoClan);
    $delete_cdb->finish();

    #load new data
    if (defined $desc->{DR} and $desc->{DR} =~ /\S+/){	
	my ($dbcomment, $dbother);
	my $dbLinks = $desc->{DR};
	my @dbLinks = split(/\nDR\s+/, $dbLinks);
	foreach my $dbL (@dbLinks){
	    if($dbL =~ /(\S+);\s+(\S+)/){
		$gsth->execute($autoClan, $1, $dbcomment, $2, $dbother);
	    }
	}

	$gsth->finish();
    }

 print STDERR "Updated the clan_database_links table\n";

    #############################
    # 4 Literature references table:
    ##############################

    #Check if refs aleady exist in the DB and uses their index or inserts them into table    
    if ($desc->{RN}){

	my $delete_clr = $rfdbh->prepare( 'Delete from clan_literature_references where auto_clan=?' )
	    or die '(EE) ERROR: couldn\'t prepare query to delete: ' . $rfdbh->errstr;
	$delete_clr->execute($autoClan);
	$delete_clr->finish();
	
	my $orderAdded=1;
	foreach my $ref (@{$desc->{RN}}){
	    
	    #1. Check if it's already in the literature references table:
	    my $existingAutoLit = checkForExistingLitRefEntry($ref->{RM}, $rfdbh);
	    
	    my $fillClanLiteratureReferencesQuery = qq(
	        insert into clan_literature_references (auto_clan, auto_lit, comment, order_added) values ($autoClan, ?, ?, ?);
               );

	    my $fsth;
	    if (defined $existingAutoLit && RfamUtils::isInteger($existingAutoLit) && $existingAutoLit > 0){#literature_references entry already exists:
		$fsth = $rfdbh->prepare($fillClanLiteratureReferencesQuery);
		$fsth->execute($existingAutoLit,'',$orderAdded);
	    }
	    else {#fill literature_references:
		
		#function?:
		my $fillLiteratureReferencesQuery = qq(
	        insert into literature_references (medline, title, author, journal) values (?, ?, ?, ?);
                );
		
		my $refTitle = $ref->{RT};
		$refTitle =~ s/\nRT\s{3}/\n/g;
		my $refAuthor = $ref->{RA};
		$refAuthor =~ s/\nRA\s{3}/\n/g;
		my $refJournal = $ref->{RL};
		$refJournal =~ s/\nRL\s{3}/\n/g;
		
		$fsth = $rfdbh->prepare($fillLiteratureReferencesQuery);
		$fsth->execute($ref->{RM},$refTitle,$refAuthor,$refJournal);
		
		$existingAutoLit = $fsth->{mysql_insertid};
		$fsth = $rfdbh->prepare($fillClanLiteratureReferencesQuery);
		$fsth->execute($existingAutoLit,'',$orderAdded);
		
	    }
	    $fsth->finish();
	    $orderAdded++;
	}
	
	
    } #end of literature ref table
#my $result = ($success ? $dbh->commit : $dbh->rollback);
 
    print STDERR "Updated the clan_literature_links table\n"; 
    $rfdbh->disconnect;
}

######################################################################
#checkForExistingLitRefEntry: checks if an entry for the given pubmed
#                             ID already exists in the
#                             literature_references table. Returns
#                             either false or the auto_lit id.
sub checkForExistingLitRefEntry {
    my ($pubMedId, $rfdbh) = @_;
    my $existingAutoLit=0;
    
    my $checkForAutoLitQuery = qq(
	        select auto_lit from literature_references where medline=?;
            );
    my $sth = $rfdbh->prepare($checkForAutoLitQuery);
    $sth->execute($pubMedId);
    
    my $result = $sth->fetchall_arrayref;
    $sth->finish();

    foreach my $row (@$result){
	$existingAutoLit .= $row->[0] if defined $row->[0];
    }
    return $existingAutoLit;
}

######################################################################

#sub is_in_clan { #returns a clan acc
#sub clanacc2clanid {
#sub clanid2clanacc {

######################################################################

=head1 AUTHOR

Paul Gardner, <pg5@sanger.ac.uk>
Jennifer Daub <jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2009: Genome Research Ltd.

Authors: Paul Gardner (pg5@sanger.ac.uk) & Jennifer Daub <jd7@sanger.ac.uk>

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;





