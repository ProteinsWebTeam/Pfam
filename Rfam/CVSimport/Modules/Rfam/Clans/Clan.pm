package Rfam::Clans::Clan; 

#modules for dealing with Rfam clans

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

#Using dev for testing:
my @rfamDatabase = ("dbi:mysql:rfam_10_0:$Rfam::rdbHostDev:$Rfam::rdbPortDev", $Rfam::rdbUserDev, $Rfam::rdbPassDev);

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

#check_into_DB: Load the data contained in desc into the database:
sub check_into_DB {
    my $desc = shift;
    
    my $rfdbh = DBI->connect(
	@rfamDatabase, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
    
    ########
    # Fill the clans table:
    my $ccLines = $desc->{CC};
    $ccLines =~ s/\nCC\s{3}/\n/g;
    my $fillClansQuery = qq(
insert into clans (clan_acc, clan_id, clan_description, clan_author, clan_comment) VALUES ('$desc->{AC}','$desc->{ID}','$desc->{DE}','$desc->{AU}','$ccLines')
   );
    
    printf "Checking acc=[%s] id=[%s] de=[%s] into DB\n", $desc->{AC},$desc->{ID},$desc->{DE};
    
    my $sth = $rfdbh->prepare($fillClansQuery);
    $sth->execute();
    my $autoClan = $sth->{mysql_insertid};
    ########
    
    #Find the auto_rfam number for each member:
    my $autoRfQuery = qq(
           select auto_rfam, rfam_id from rfam where rfam_acc=?;
   );
    
    my $sthRfQuery = $rfdbh->prepare($autoRfQuery);
    my %mb2auto_rfam;
    foreach my $mb ( @{$desc->{MB}} ){
	
	$sthRfQuery->execute($mb);
	
	my $result = $sthRfQuery->fetchall_arrayref;
	my $rfamId='';
	foreach my $row (@$result){
	    $mb2auto_rfam{$mb} .= $row->[0] if defined $row->[0];
	    $rfamId .= $row->[1] if defined $row->[1];
	}
    }
    
    ##########
    # Query to fill the clan_membership table:
    my $fillClanMembershipQuery = qq(
insert into clan_membership (auto_clan, auto_rfam) VALUES ($autoClan, ?);
   );
    
    $sth = $rfdbh->prepare($fillClanMembershipQuery);
    
    foreach my $mb ( @{$desc->{MB}} ){
	$sth->execute($mb2auto_rfam{$mb}) if defined $mb2auto_rfam{$mb};
	warn "WARNING: failed to find an auto_rfam id for $mb!" if not defined $mb2auto_rfam{$mb};
    }
    ##########
    # Query to fill the database links table:
    
    if (defined $desc->{DR} and $desc->{DR} =~ /\S+/){
	my $fillDatabaseLinksQuery = qq(
insert into clan_database_links (auto_clan, db_id, comment, DB_link, other_params) VALUES ($autoClan, ?, '', ?, '');
   );
	
	$sth = $rfdbh->prepare($fillDatabaseLinksQuery);
	my $dbLinks = $desc->{DR};
	my @dbLinks = split(/\nDR\s+/, $dbLinks);
	foreach my $dbL (@dbLinks){
	    if($dbL =~ /(\S+);\s+(\S+)/){
		$sth->execute($1,$2);
	    }
	}
	
    }
    ##########
    # Query to fill the literature references table:
    #Check if they aleady exist in the DB
    
    if ($desc->{RN}){
	
	#
	my $orderAdded=1;
	foreach my $ref (@{$desc->{RN}}){
	    
	    #1. Check if it's already in the literature references table:
	    my $existingAutoLit = checkForExistingLitRefEntry($ref->{RM}, $rfdbh);
	    
	    my $fillClanLiteratureReferencesQuery = qq(
	        insert into clan_literature_references (auto_clan, auto_lit, comment, order_added) values ($autoClan, ?, ?, ?);
               );
	    
	    if (defined $existingAutoLit && RfamUtils::isInteger($existingAutoLit) && $existingAutoLit > 0){#literature_references entry already exists:
		$sth = $rfdbh->prepare($fillClanLiteratureReferencesQuery);
		$sth->execute($existingAutoLit,'',$orderAdded);
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
		
		$sth = $rfdbh->prepare($fillLiteratureReferencesQuery);
		$sth->execute($ref->{RM},$refTitle,$refAuthor,$refJournal);
		
		$existingAutoLit = $sth->{mysql_insertid};
		$sth = $rfdbh->prepare($fillClanLiteratureReferencesQuery);
		$sth->execute($existingAutoLit,'',$orderAdded);
		
	    }
	    
	    $orderAdded++;
	}
	
	
    }
#my $result = ($success ? $dbh->commit : $dbh->rollback);
 

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
    
    foreach my $row (@$result){
	$existingAutoLit .= $row->[0] if defined $row->[0];
    }
    return $existingAutoLit;
}

######################################################################

sub id_exists {
    my $id = shift;
    
    my $rfdbh = DBI->connect(
	@rfamDatabase, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
    
    # Query to search for the description of embl entries with the embl id
    my $query = qq(
           select clan_acc from clans where clan_id=?;         
   );
    
    my $sth = $rfdbh->prepare($query);
    $sth->execute($id);
    
    my $acc=0;
    my $result = $sth->fetchall_arrayref;
    foreach my $row (@$result){
	$acc .= $row->[0] if defined $row->[0];
    }
    $sth = $rfdbh->disconnect or warn $rfdbh->errstr;
    
    return $acc; # id exists
    
}

######################################################################
#sub check_out_family {
#sub check_in_family {
#sub allocate_new_accession {
sub allocate_new_clan_accession {
    my $id = shift;

     if( not defined $id && length($id)>0) {
 	warn("Cannot allocate a new family without a name");
 	return undef;
     }

#     my $db = Rfam::default_db();
#     if( my $name = $db->_get_lock() ) {
#         # locks the SDMB file
#         die "Unable to get the lock for the SDBM_file - $name has it\n";
#     }
    
    my $clan_acclog_file   = "$Rfam::clan_acclog_file";
    open(ACCLOG,"< $clan_acclog_file") || die "Could not open $clan_acclog_file ($!) A v. bad error\n[$!]";
    open(ACCTEMP,">$clan_acclog_file.$$") || die "Could not open $clan_acclog_file.$$ ($!) A v. bad error\n[$!]";
    
#RF00001 [5S_rRNA] [sgj] [Thu Aug  8 12:57:12 2002]
    my $line = "CL00000 [dummy] [pg5] [Tue Oct 20 15:59:05 2009]\n"; #Initialise with a dummy CL00000 entry 
    while( <ACCLOG> ) {
 	print ACCTEMP;
 	$line = $_;
    }
    
    ($line =~ /^(\S{2}\d+)/) || die "Last line of acclog is not an accession ($_). Big trouble!";
    
    my $acc = $1;
    $acc++;
    
    my $me = `whoami`;
    chomp $me;
    my $date = gmtime();

    print ACCTEMP "$acc [$id] [$me] [$date]\n";
    print         "$acc [$id] [$me] [$date]\n";
    
    close(ACCLOG);
    close(ACCTEMP);
    
    rename("$clan_acclog_file.$$",$clan_acclog_file) or die "FATAL: failed to rename [$clan_acclog_file.$$] as [$clan_acclog_file]\n[$!]";
    
#     # update the accmap
#     $db->_add_accession( $acc, $family );
#     $db->_unlock();
    
    return "$acc";
}

#Makes the clan directory and writes the CLANDESC file:
sub make_new_clan_files {
    my $desc = shift;
    
    die "FATAL: AC is not defined or is poorly formatted in the desc object! [" . $desc->{'AC'} . "]"  if not defined $desc->{'AC'} or $desc->{'AC'} !~ /^CL/;
    my $newfamily = $desc->{'AC'};
    my $dir = "$Rfam::clan_dir/$newfamily";
    
    die "FATAL: the directory [$dir] already exists!" if -d $dir;
    mkdir("$dir", 0775 ) or die "FATAL: (Clan.pm) Could not make a new clan directory for [$newfamily]!\n[$!]";
    
    print "Writing CLANDESC file: [$dir/CLANDESC]\n";
    open(DE, "> $dir/CLANDESC");
    Rfam::Clans::Clan::writeClanDesc($desc, \*DE);
    close(DE);
    system("chmod -R a+rxw $dir") and die "FATAL: failed to run chmod -R a+rxw $dir\n[$!]";
    return 1;
}

#sub is_in_clan { #returns a clan acc
#sub clanacc2clanid {
#sub clanid2clanacc {

#sub slurpClanDesc {
#sub generateClanDesc {

######################################################################

=head1 AUTHOR

Paul Gardner, C<pg5@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2009: Genome Research Ltd.

Authors: Paul Gardner (pg5@sanger.ac.uk)

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





