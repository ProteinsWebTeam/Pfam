#!/software/bin/perl


=head1 NAME

rfambot_imageupdate.pl

=head1 DESCRIPTION

CARE:currently hardcoded-release, dbname and bot password (2 places)
CARE:The description string submitted with an updated image should be checked 

-gets a list of pages from the release database
-check for the rfam_box
-gets the image name (RFXXX.jpg)
-gets the image from RCS -CURRENT
-checks wikicommons if this picture updated since last release and who user was.
-uploads the new image with a comment. 

=head1 AUTHOR

original code outline from mm6@sanger.ac.uk
jd7@sanger.ac.uk

=cut

use strict ;
use warnings ;
use Getopt::Long;
use LWP::UserAgent ;
use URI::Escape ;
use DBI;         # DB access
use Cwd;
use Rfam;
use DateTime; 
use LWP::Simple;
use LWP::UserAgent;
use HTTP::Request;
use HTTP::Response;
use HTTP::Cookies;
use Encode qw(encode);
use utf8;
use JSON; 


my ( $help, $edit, $release);
&GetOptions( 'h|help' =>\$help,
             'edit' => \$edit,
             'release=s' => $release);
	     
if( $help) {
    &help();
    exit(1);
}

my $botname='Rfambot';
my $password='mafr123';
my $pics=$Rfam::current_dir;
my $current=getcwd;

#changes with release
$release="10.0";
my $dbname='rfam_10_0';
#last release date

my $dt = DateTime->new( year => '2009', month=> '01', day => '30');
my $lastrelease=$dt->ymd;

my $now= DateTime->now;
my $nowtime= $now->iso8601(); 
$nowtime="$nowtime".'Z';

print STDERR "last update date=", $lastrelease, "\n";;


if (!defined $botname){
    die "Botname must be specified$!";
}

if (!defined $password){
    die "Password must be specified$!";
}

if ( !defined $edit){
    warn "In test mode$!";
}

if (!defined $release){
    die "Rfam release version must be provided\n";
}

#---------------------------------------------
# Read current article
# my $ua = LWP::UserAgent->new ;
#  my @ns_headers = (
#    'User-Agent' => 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7) Gecko/20041107 Firefox/1.0',
#    'Accept' => 'image/gif, image/x-xbitmap, image/jpeg,
#         image/pjpeg, image/png, */*',
#    'Accept-Charset' => 'iso-8859-1,*,utf-8',
#    'Accept-Language' => 'en-US',
#   );
# # set up the proxy info-didnt get this from magnus
# my $PROXY_URL = 'http://wwwcache.sanger.ac.uk:3128';
# $ua->proxy( [ 'http' ], $PROXY_URL );

# $ua->agent ( "rfam-updater/0.1 " ) ;
# $ua->cookie_jar ( {} ) ;

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
my $bsth = $dbh->prepare( 'SELECT distinct(title) FROM wikitext' )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;


#-------------------------------------------------------------------------------
#Main#
#get the titles to check:

my @noRfambox;
my @oldRelease;
my @noRfamacc;
my @badImageId;
my @noImage;
my @editCheck;

# get the list of Rfams to check
my $titles = getTitles();
#my $titles = ['Ribosomal_protein_L19_leader'];
my $counter=0;

TITLE:foreach my $t (@$titles){
    sleep 5;
    # Read current article
    my $ua = LWP::UserAgent->new ;
    my @ns_headers = (
                      'User-Agent' => 'Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7) Gecko/20041107 Firefox/1.0',
                      'Accept' => 'image/gif, image/x-xbitmap, image/jpeg,
        image/pjpeg, image/png, */*',
                      'Accept-Charset' => 'iso-8859-1,*,utf-8',
                      'Accept-Language' => 'en-US',
                      );
# set up the proxy info-didnt get this from magnus
    my $PROXY_URL = 'http://wwwcache.sanger.ac.uk:3128';
    $ua->proxy( [ 'http' ], $PROXY_URL );
    
    $ua->agent ( "rfam-updater/0.1 " ) ;
    $ua->cookie_jar ( {} ) ;

    #----------------------------------
    #if ($counter==1) {die "Done all the allowed edits for now =$counter;";}
    my $getpage = 0;
    my $res;
    while ( $getpage < 5 ) {
        my $req = HTTP::Request->new ( GET => "http://en.wikipedia.org/w/index.php?action=raw&title=$t" ) ;
        print "(ii)Requesting articule http://en.wikipedia.org/w/index.php?action=raw&title=$t\n";
        $res = $ua->request ( $req ) ;
        last if $res->is_success;
        sleep 10;
        $getpage++;
    }
    if ( $getpage > 0 ) {
        print "Tried $getpage times to retrieve article\n";
    }

    unless ( $res->is_success ) {
        print  "Failed to retrieve content $t after 5 tries: " . $res->status_line . "\n";
        next TITLE;
    }
    #die $res->status_line . "\n" unless $res->is_success ;
    
    my $old_text = $res->content ;
    my @old = split "\n" , $res->content ;
    
    #get rfambox - really actually only splits data
    my $box=getBox(@old);
    #print $box;
    my $boxstring;
    #if no rfam_box skip
    print "(ii)Checking for Rfam_box\n";
    if (!defined $box->{'rfam_box'}){
        push( @noRfambox, $t);
        print "(ii) No rfam_box found so skipping title $t\n";
        next TITLE;
        
    }else { 
        $boxstring=$box->{'rfam_box'};
    }

   #if box not updated to correct version
   print "(ii)Checking for release version in current box\n"; 
   if ($boxstring !~ m/release\=$release/){
        print "$t Box doesnt have the correct release vesion\n";
        push(@oldRelease, $t);
        next TITLE;
    }    
    
    #parse the box and get rfam_acc
    my $imageId;
    print "(ii)Getting the image id from the Rfambox\n";
    unless ( $imageId=&getImageId( $box->{'rfam_box'}) ) {
        warn "(EE) Can't get the image_id from the rfam_box for :", $t," \n";
        print $box->{'rfam_box'};
        push(@noRfamacc, $t);
        next TITLE;
    }

    print "(ii)Checking format of image id $imageId\n"; 
    if ($imageId !~/^RF\d{5}$/){
        warn "(EE) Image file doesnt have a rfacc.jpg as name:", $imageId," \n";
        push(@badImageId, $imageId);
         next TITLE;
    }

    my $rfamacc=$imageId;
   

    #check if the image file exists for this family in local files
    print "(ii)Checking  new image for this family exists in CURRENT dir\n"; 
    if (! -e "$pics/$rfamacc/rna_cons.ps"){
        warn "(EE) Image file rna_cons for $imageId doesn't exist in $pics/$rfamacc \n";
        push(@noImage, $imageId);
        next TITLE; 
    }

          
    #get the ps dile for the current release and convert to local jpg
    print "(ii) Copying and converting the rna_cons.ps file from CURRENT to local as jpg\n";
    system( "convert $pics/$rfamacc/rna_cons.ps $current/$imageId.jpg");
    
    #check if the image needs updating
    my $url='http://commons.wikimedia.org/w/api.php?action=query&format=json&titles=File:'.$imageId.'.jpg&prop=imageinfo&iilimit=50&iistart='.$nowtime.'&iiend='.$lastrelease.'T00:01:00Z';
    #   my $url='http://commons.wikimedia.org/w/api.php?action=query&format=json&titles=File:'.$imageId.'.jpg&prop=imageinfo&iilimit=50';
    my $newreq = HTTP::Request->new( GET => $url );
    my $r = $ua->request( $newreq );
    
    if ($r->is_success){
        print "(ii)Got the history for this image $imageId\n"; 
        #print $r->content;
        my $result= from_json( $r->content );
        #print $result, "\n";;
        
        my @ids= keys %{$result->{query}->{pages}};
        my $pageid = $ids[0];
        #print $pageid, "\n";
        my @keys=keys %{$result->{query}->{pages}->{$pageid}};
        #print join(",", @keys), "\n";
        my $rv = $result->{query}->{pages}->{$pageid}->{imageinfo}; 
        #print join(@$rv); exit;
        if (! $rv){
            print "(ii) Good-no changes since the last release\n";
        }else{
            
            foreach my $a (@{$rv}){
                my $user=  $a->{user};
                #check for the users
                if ($user eq "Jennifer Rfm" || $user eq "rfambot"){
                    print "(ee) Jennifer/Rfambot has edited this page since the last release-check it\n";
                    push(@editCheck, $imageId);
                    next TITLE;
                    
                }
            }
        } #is success and changes not jen or bot
               
        
        #update the image file
        #login#
        if ($edit){
            my $ignore_login_error=0;
            my $username = "rfambot";
            my $password = "mafr123";
            print "(ii)Trying to loggin in as $username\n";
            my $local_file="$current/$imageId.jpg";
            my $new_file="$imageId.jpg";
            my $description="Image update for $imageId taken from [[:w:en:Rfam|Rfam]] database release $release. Nucleotide colouring indicates sequence conservation between the members of this family";
            my $response=$ua->post("http://commons.wikimedia.org/w/index.php?title=Special:Userlogin&action=submitlogin",
                                   @ns_headers, Content=>[wpName=>$username,wpPassword=>$password,wpRemember=>"1",wpLoginAttempt=>"Log in"]);
            if($response->code!=302 && !$ignore_login_error) {
                print  "We weren't able to login as $username\n";
                print "*****response to second login***", $response->as_string; exit;
            }else{
                print "(ii)Logged in and uploading the new image for $imageId\n";
                #now do the upload
                my $response2=$ua->post("http://commons.wikimedia.org/wiki/Special:Upload",
                                        @ns_headers,Content_Type=>'form-data',Content=>
                                        [
                                         wpUploadFile=>[$local_file],
                                         wpDestFile => $new_file,
                                         wpUploadDescription=>$description,
                                         #wpLicense => $license,
                                         wpForReUpload => "1", # I added this for an update..
                                         wpUploadAffirm=>"1",
                                         wpUpload=>"Upload file",
                                         wpIgnoreWarning=>"1"
                                         ]);
                print STDERR $response2->as_string, "\n";
                ++$counter;
                if ($response2->code==200 || $response2->code==302){
                    #open(DEBUG,">$dir/debug.txt") or die "Could not write file.\n";
                    #print  "*****response to second login***", $response2->as_string; exit;
                }else{
                    #++$counter;
                    print "Everything seems to be OK and new image uploaded for $imageId \n"; 
                }
                print STDERR "Done with $t and image $imageId\n";
                #Logout
                $response2 = $ua->get ( "http://commons.wikimedia.org/w/api.php?action=logout") ;
                

            } #upload  
            #my $response3 = $ua->get ( "http://commons.wikimedia.org/w/api.php?action=logout") ;
        }# end of edit;
        
        
    }#wiki page got       
#$ua='';    
# exit(1);
    
}# end of titles


#report on images that were skiiped
print  "(ee) Pages with no Rfambox=", join(",", @noRfambox), "\n";
print  "(ee) Boxes with old release information=", join(",", @oldRelease), "\n";
print  "(ee) Boxes with no Rfamacc", join(",", @noRfamacc), "\n";
print  "(ee) Boxes with odd imageid", join(",", @badImageId), "\n";
print  "(ee) No image available in CURRENT", join(",", @noImage), "\n";
print  "(ee) Images updated since last release and need checked", join(",", @noRfambox), "\n";
   
#SUBROUTINES----------------
    

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


sub getImageId{
    my $data=shift;
    my $RF;
   if ($data=~m/^\{\{Rfam_box\|acc\=(RF\d+)\|/) {
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



EOF

}

