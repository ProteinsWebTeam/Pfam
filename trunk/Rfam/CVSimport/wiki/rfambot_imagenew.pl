#!/software/bin/perl


=head1 NAME

rfambot_imagenew.pl

=head1 DESCRIPTION

this was the second incarnation of the imagebot code-it was set to run on a single 
image at time-but easy enough to hack that.
-needs a family acc and the title for the image-I took the description lines-or at least
a truncated version of the de line with _secondary structure added on.

very  simply gets the image from the RCS CURRENT dir, uploads it with the correct name and 
license info.

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

#-------------------------------------------------------------------------------
#Main#
#get the titles to check:

#foreach image recreate the User agent
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
#essential information for an image update

my $rfamacc="RF01051"; #rfam_acc
my $id="Cyclic_di-GMP_riboswitch"; #rfam_id

#---------------------------------
my $imageId=$id."_secondary_structure";

#check if the image file exists for this family
print "(ii)Checking  new image for this family exists in CURRENT dir\n"; 
if (! -e "$pics/$rfamacc/rna_cons.ps"){
    warn "(EE) Image file rna_cons for $imageId doesn't exist in $pics/$rfamacc \n";
    next TITLE; 
}


#get the post script file for the current release and convert to local jpg
print "(ii) Copying and converting the rna_cons.ps file from CURRENT to local as jpg\n";
system( "convert $pics/$rfamacc/rna_cons.ps $current/$imageId.jpg");


#generate the information template

my $ignore_login_error=0;
print "(ii)Trying to loggin in as $botname\n";
my $local_file="$current/$imageId.jpg";
my $new_file="$imageId.jpg";
my $description=  "{{Information |description=Secondary structure image for $id non coding RNA ($rfamacc). Nucleotide colouring indicates sequence conservation between the members of this family. 
|source= Rfam database release 9.1
|date= 	January 2009
|author= Rfam database
|permission=This image is taken from the [http://rfam.sanger.ac.uk/ Rfam database], which is completely in the [ftp://ftp.sanger.ac.uk/pub/databases/Rfam/9.1/COPYING public domain]
|other_versions=
|other_fields=[[Category:Non-coding RNA]] 
}}" ;
my $license="PD-because|this image is taken from the [http://rfam.sanger.ac.uk/ Rfam database], which is completely in the [ftp://ftp.sanger.ac.uk/pub/databases/Rfam/9.1/COPYING public domain]";

#connect to wikicom
my $response=$ua->post("http://commons.wikimedia.org/w/index.php?title=Special:Userlogin&action=submitlogin",
                       @ns_headers, Content=>[wpName=>$botname,wpPassword=>$password,wpRemember=>"1",wpLoginAttempt=>"Log in"]);
if($response->code!=302 && !$ignore_login_error) {
    print  "We weren't able to login as $botname\n";
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
                             wpLicense => $license,
                             #wpForReUpload => "1", # I added this for an update..
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
    print STDERR "Done with image $imageId\n";
    #Logout
    $response2 = $ua->get ( "http://commons.wikimedia.org/w/api.php?action=logout") ;
    
    
} #upload  
#my $response3 = $ua->get ( "http://commons.wikimedia.org/w/api.php?action=logout") ;

   
exit;
