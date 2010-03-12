#!/software/bin/perl


#t1:task 1-simply update rfam_box info

=head1 NAME

rfambot_updatebox.pl

=head1 DESCRIPTION

rfambot code- this updates the rfam_box on the wikipages.
It gets the list of pages from the rdb (release version on dev). 
Gets the page content and does a simple reg-ex to find the rfam_box.
Updates the data in the rfambox with the new release data.

needs to be run with the -edit option in order to update. without it will just report changes.

Finally reports the pages that have problems etc that need attention (ie no box or box no acc etc).
 Note all of the original rfam box data was based on Rfam-acc.
 This has changes for some later families this still to be resolved.
If the box has already been updated for this release it wont re-edit it.

bot approval -
http://en.wikipedia.org/wiki/Wikipedia:BRfA
http://en.wikipedia.org/wiki/User:Rfambot

=head1 AUTHOR

original outline from mm6@sanger.ac.uk
jd7@sanger.ac.uk

=cut



use strict ;
use warnings ;
use Getopt::Long;
use LWP::UserAgent ;
use URI::Escape ;
use DBI;         # DB access

my ( $help, $edit, $release, $dbname);
&GetOptions( 'h|help' =>\$help,
             'edit' => \$edit,
             'release=s' => \$release,
	     'dbname=s'  => \$dbname  );
	     
if( $help) {
    &help();
    exit(1);
}

my $botname='Rfambot';
my $password='mafr123';
my $summary='Rfambot updates release 10.0';

#changes with release
$release="10.0";
$dbname="rfam_10_0";

if (!defined $botname){
    die "Botname must be specified$!";
}

if (!defined $password){
    die "Password must be specified$!";
}

if ( !defined $edit){
    warn "In test mode$!";
}

if (!defined $summary){
    die "Summary must be provided\n";
}

if (!defined $release){
    die "Rfam release version must be provided\n";
}

if (!defined $dbname){
    die "Rfam database must be provided\n";
}


#---------------------------------------------
# Read current article
my $ua = LWP::UserAgent->new ;

# set up the proxy info-didnt get this from magnus
my $PROXY_URL = 'http://wwwcache.sanger.ac.uk:3128';
$ua->proxy( [ 'http' ], $PROXY_URL );

$ua->agent ( "rfam-updater/0.1 " ) ;
$ua->cookie_jar ( {} ) ;

#--------------------------------------------
# set up the DB connection and statement handles

# DB connection parameters
my( $dbHost, $dbPort, $dbName, $dbUser );
$dbHost = 'pfamdb2a';
$dbPort = '3301'; # this will be different for the new different dbs..
$dbName = $dbname;
$dbUser = 'pfamro';

my $dsn    = "dbi:mysql:$dbName:$dbHost:$dbPort";
my $dbAttr = { RaiseError => 1,
                           PrintError => 1 };

# connect
my $dbh = DBI->connect( $dsn, $dbUser, '', $dbAttr )
  or die "(EE) ERROR: couldn't connect to database: $!";

# prepare all the queries that we'll need

# titles list
my $bsth = $dbh->prepare( 'SELECT distinct(title) FROM wikitext order by auto_wiki' )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;

my $asth = $dbh->prepare( 'SELECT r.description, r.rfam_id, r.type,  r.seed_source, r.structure_source, at.average_length, at.percent_id  FROM rfam as r, alignments_and_trees as at where r.auto_rfam=at.auto_rfam and r.rfam_acc=? and at.type="full"' )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;

#-------------------------------------------------

my @noBox=qw( tmRNA RyhB_RNA SnoRNA Hfq_binding_sRNA Sxy_5\'_UTR_element Ribosomal_RNA MicroRNA Hairpin_ribozyme transfer_RNA Group_II_intron Magnesium_Sensor CRISPR Transfer_RNA SnoRNA);
my %noBoxHash=  map { $_,1} @noBox;

#-------------------------------------------------------------------------------
#Main#

#get the titles to check:
my @pageNotFound=("(1) Pages Not Found");
my @noBoxFound=("(2) Pages no rfam_box found");
my @redirect=("(3) Pages with a redirect");
my @noAccFound= ("(4) Pages with box but no rfam_acc found");
my @pageUpdated= ("(5) Pages already updated");
my @updated= ("(6) Pages updated this parse"); 
# get the list of Rfams to check
my $titles = getTitles();

my $counter=0;

TITLE:foreach my $t (@$titles){
    sleep 2;
    #if ($counter==10) { last;}
    my $getpage = 0;
    my $res;
    while ( $getpage < 5 ) {
        my $req = HTTP::Request->new ( GET => "http://en.wikipedia.org/w/index.php?action=raw&title=$t" ) ;
        print "(ii) Requesting articule http://en.wikipedia.org/w/index.php?action=raw&title=$t\n";
        $res = $ua->request ( $req ) ;
        last if $res->is_success;
        sleep 10;
        $getpage++;
    }
    if ( $getpage > 0 ) {
        print STDERR "tried $getpage times to retrieve article\n";
    }

    unless ( $res->is_success ) {
        print  "failed to retrieve content $t after 5 tries: " . $res->status_line . "\n";
	push (@pageNotFound, $t);
        next TITLE;
    }
    #die $res->status_line . "\n" unless $res->is_success ;
    
    my $old_text = $res->content ;
    if ($old_text=~/\#REDIRECT/i){
	print STDERR "(ii) Page now has redirect-fix the title in RDB for $t\n";
	push (@redirect, $t);
	next TITLE;
   }
    my @old = split "\n" , $res->content ;
    
    #get rfambox - really actually only splits data
    my $box=getBox(@old);
    #print $box;
    my $boxstring;
    #if no rfam_box skip
    if (!defined $box->{'rfam_box'}){
        push( @noBoxFound, $t) unless (defined ($noBoxHash{$t}));
        print "(ii) No rfam_box found so skipping title $t\n";
        next TITLE;
        
    }else { 
        $boxstring=$box->{'rfam_box'};
    }
   
   if ($boxstring=~m/release\=$release/){
        print "Skip $t as edited already\n";
	push( @pageUpdated, $t);
        next TITLE;
    }    

     
    #parse the box and get rfam_acc
    my $rfam_acc;
    unless ( $rfam_acc=&parseBox( $box->{'rfam_box'}) ) {
        warn "(EE) Cant get the rfam accession from the rfam_box for :", $t," \n";
	push( @noAccFound, $t);
        next TITLE;
    }

    #get all the data to update with:
    print STDERR "(ii)Getting data from RDB\n";
    my $rdbdata;
    unless ( $rdbdata=&getRDBdata($rfam_acc) ) {
        print "(EE) Can't obtain data from RDB for: ", $rfam_acc,"\n";
        next TITLE;
    }
    
    my ($de, $id, $tp, $se, $ss, $le, $pi) =@$rdbdata;
    
#parse the pmids
    $ss=~s/PMID\:(\d+)/\{\{PMID\|$1\}\}/g;
    $se=~s/PMID\:(\d+)/\{\{PMID\|$1\}\}/g;
    print $boxstring, "\n";
  
      
    my $newbox="{{Rfam_box|acc=$rfam_acc| description=$de |abbreviation=$id |avg_length=$le |avg_identity=$pi |type=$tp |se=$se |ss=$ss |release=$release}}\n";
    print $newbox, "\n";

    my $newtext='';
    $newtext.= join("\n", @{$box->{'pre_rfam_box'}})."\n" if defined $box->{'pre_rfam_box'};
    $newtext.=$newbox. "\n";
    $newtext.= join("\n", @{$box->{'post_rfam_box'}})."\n" if defined $box->{'post_rfam_box'};
    $newtext=~ s/\n\n\n/\n\n/g;
  
   if ( ! $edit){
       # print $newtext;
       push (@updated, $t);
        ++$counter;
	next TITLE;
	
    } else { #edit

#Login

	$res = $ua->post ( "http://en.wikipedia.org/w/api.php" , [
								  'action' => 'login',
								  'lgname' => $botname,
								  'lgpassword' => $password
								  ] ) ;
	die $res->status_line . "\n" unless $res->is_success ;
	print "Logged in as $botname.\n" ;
	
#Get edit token
  my $tries = 0;
  while ( $tries < 5 ) {
      $res = $ua->get ( "http://en.wikipedia.org/w/api.php?format=xml&action=query&prop=info|revisions&intoken=edit&titles=$t" ) ;
      last if $res->is_success;
      sleep 10;
      $tries++;
  }
  if ( $tries > 0 ) {
      print STDERR "tried $tries times to retrieve content\n";
  }

  unless ( $res->is_success ) {
      die "failed to retrieve content after 5 tries: " . $res->status_line . "\n";
  }

#	die $res->status_line . "\n" unless $res->is_success ;
	
	$res->content =~ / timestamp="([^\"]+)"/ ;
	my $ts = $1 ;
	
	$res->content =~ / edittoken="([^\"]+)"/ ;
	my $token = $1 ;
	
	
	# # Write back to article
        $res = $ua->post ( "http://en.wikipedia.org/w/api.php" , [
								  'action' => 'edit',
								  'title' => $t ,
								  'summary' => $summary ,
								  'text' => $newtext ,
								  'basetimestamp' => $ts ,
								  'token' => $token ,
								  'bot' => 1
								  ] ) ;
	
	
	die $res->status_line . "\n" unless $res->is_success ;
	die "An error occured.\n" unless $res->content =~ / result\=\&quot\;Success\&quot\; / ;
	print "Wrote article $t  successfully.\n" ;
	++$counter;
	push (@updated, $t);
#Logout
	$res = $ua->get ( "http://en.wikipedia.org/w/api.php?action=logout" ) ;
	print "Logged out.\n" ;
    }#end of edit option
       
}# end of titles
print STDERR "Pages not found= ", scalar(@pageNotFound)-1,"\n";
print STDERR "Pages no rfam_box= ", scalar(@noBoxFound)-1,"\n";
print STDERR "Pages with redirect= ", scalar(@redirect)-1,"\n";
print STDERR "Pages with box but no rfam_acc= ", scalar(@noAccFound)-1,"\n";
print STDERR "Pages already updated= ", scalar(@pageUpdated)-1,"\n";
print STDERR "Pages updating this time= ", scalar(@updated)-1,"\n\n";

    &write(\@pageNotFound, \@noBoxFound, \@redirect, \@noAccFound, \@pageUpdated, \@updated );

#SUBROUTINES----------------

sub write {
    my @list=@_;
    foreach my $l (@list){
	print STDERR join("\n", @$l), "\n\n";	
    }
}

sub getTitles {
  # retrieve auto_rfam and description from the rfam table
  $bsth->execute;

  my @titles;
  while( my $row = $bsth->fetchrow ) {
        push @titles, $row;
  }
  die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
        if $DBI::err;

  return \@titles;
}

sub getBox {
   my @data=@_;
   my %ret;
   my $mode='pre_rfam_box';
   foreach my $l ( @data ) {
       $l =~ s/\s+$// ;
       if ($mode eq 'pre_rfam_box'){
	   if ( $l =~m/^\{\{rfam[_]box\|.*\}\}$/i ) {
	       $ret{'rfam_box'}=$l;
	       $mode='post_rfam_box';
	   }else{
	       push @{$ret{$mode}}, $l;
	   }
       }else{
	   push @{$ret{$mode}}, $l 
       }
    }
   return \%ret;
}

sub getRDBdata {
    my $acc=shift;
    $asth->execute($acc);
    my $row;
    $row = $asth->fetchrow_arrayref();
    die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n" if $DBI::err;
    $asth->finish;
    return $row;
}


sub parseBox{
    my $data=shift;
    my $RF;
   if ($data=~m/acc\=(RF\d+)\|\s?de/) {
	$RF=$1;
    }
    return $RF;
   
}





# # Write back to article
# $res = $ua->post ( "http://en.wikipedia.org/w/api.php" , [
# 	'action' => 'edit',
# 	'title' => $article_title ,
# 	'summary' => $summary ,
# 	'text' => $text ,
# 	'basetimestamp' => $ts ,
# 	'token' => $token ,
# 	'bot' => 1
# ] ) ;
# die $res->status_line . "\n" unless $res->is_success ;
# die "An error occured.\n" unless $res->content =~ / result\=\&quot\;Success\&quot\; / ;
# print "Wrote article successfully.\n" ;


# # Logout
# $res = $ua->get ( "http://en.wikipedia.org/w/api.php?action=logout" ) ;
# print "Logged out.\n" ;



###############################
#SUBROUTINES

sub help {
    print STDERR <<EOF;

This is the first incarnation of the Rfam bot code-written to simply update the rfam boxes. If the page does not have an Rfam box or has laready been updated for this release it will skip the page.

Usage:  ./rfam_update_9.1.t1.pl <options>
Options:       -h                  show this help
               -edit               if provided updates the page on wikipedia-otherwise the default leaves the page unedited but reports the new box string.
               -release            MUST be provided e.g. '9.1'

The correct format for the rfam box is now this:
Any existing Rfam box will get overwritten and replaced with the format in this=>

{{Rfam_box|acc=RF00423| description=Small Cajal body specific RNA 4 |abbreviation=SCARNA4 |avg_length=128.00 |avg_identity=82.00 |type=Gene;snRNA;snoRNA;scaRNA; |se=Moxon SJ, INFERNAL |ss=Predicted; RNAFOLD; Moxon SJ |release=9.1}}


EOF

}

