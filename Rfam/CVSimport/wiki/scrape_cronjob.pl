#!/software/bin/perl

# a first attempt at a script to screen scrape Rfam wikipedia articles
# and store them in a DB table.
# jt6 20070524 WTSI.

# to fix  a specific page run this:
# ~jd7/WIKI/Code/scrape_cronjob_all.pl -pages list -acc RF00173  -update  -fill

# to fix more than one  page just specify multiple -acc
# ~jd7/WIKI/Code/scrape_cronjob_all.pl -pages list -acc RF00173 -acc RF00018 -update  -fill

# to update all pages use this:
# ~jd7/WIKI/Code/scrape_cronjob_all.pl -pages all  -update  -fill

# *** the scrape code updates the RDB on dev- you then need to run this to copy the table over to live.******
# ~jd7/WIKI/Code/updateLive.sh
# *
# nb
# *the -update option has to be used otherwise it only reports changes that have been made and not actually update the RDB
# *the -fill option downloads the latest version of the page no matter when it was last edited.
# * to update only pages that have been edited in N days use -changes N instead of -fill

# e.g
# ./scrape_cronjob_all.pl -pages list -acc RF00173  -update  -changes 5
# will update this page if edited in the last 5 days.
# or
# /scrape_cronjob_all.pl -pages all  -update  -changes 5
#will update any of our pages if edited in the last 5 days. 


use strict;
use warnings;

# some of these modules are only installed in our custom perl
# installation... For production we'll need either to install them
# again somewhere more general, or get ISG to install them in the core
# perl installation

use DBI;         # DB access
use LWP;         # for making web requests
use JSON;        # NOT CORE: parsing the responses from the wp api
use URI::Escape; # NOT CORE: tidying up the URIs that we send to wp
use DateTime;    #for getting time/formatting for cron job 
use Getopt::Long;
use HTML::Parser;

$| = 1;



my ($pages, @rfacc, $title,$fill, $changes, $help, $update);
&GetOptions( "h|help" => \$help,
             "pages=s" => \$pages,
             "acc=s@" => \@rfacc,
	     "title=s"  => \$title,
             "fill" => \$fill,
             "changes=s"  => \$changes,
             "update" => \$update);

if( $help) {
    &help();
    exit(1);
}

#-------------------------------------------------------------------------------
# configuration

# the root for all wikipedia URLs
my $WP_ROOT = 'http://en.wikipedia.org';

# the root for the Rfam family entries in WP
my $WP = "$WP_ROOT/wiki/";

# the root for the wikipedia API
my $WP_API = 'http://en.wikipedia.org/w/api.php';

# DB connection parameters
my( $dbHost, $dbPort, $dbName, $dbUser, $dbPass );
$dbHost = 'pfamdb2a';
$dbPort = '3301'; # this will be different for the new different dbs..
$dbName = 'rfam_9_1';
$dbUser = 'pfam';
$dbPass = 'mafp1';

# the web proxy URL
my $PROXY_URL = 'http://wwwcache.sanger.ac.uk:3128';

# #set datetime for cron job
# my $dt = DateTime->now;
# $dt->set_time_zone('UTC');
# $dt->subtract (days => 1);
# my $lastday=$dt->ymd;

# counter for changed pages;
my $page_count=0;
my $unchanged=0;
my $checked=0;
my $updated=0;
#-------------------------------------------------------------------------------
# set up the HTML parser. Most of the code that uses the parser is
# taken straight from the examples in the module source package. And
# it's a bit hairy.

# set up to filter the tags and attributes

# attributes to remove
my @ignore_attr =
    qw(bgcolor background color face link alink vlink text
       onblur onchange onclick ondblclick onfocus onkeydown onkeyup onload
       onmousedown onmousemove onmouseout onmouseover onmouseup
       onreset onselect onunload
      );

# make it easier to look up attributes
my %ignore_attr = map { $_ => 1} @ignore_attr;

# tags to remove
my @ignore_tags = qw(font big small i);

# elements to remove... the distinction between these and tags being
# lost on me entirely
my @ignore_elements = qw( script style );

# this is a string that we'll populate with the HTML that drops
# through the first set of filters
my $OUTPUT  = '';

# build a new parser, set up the handlers and the lists of
# tags/elements to ignore
my $p = HTML::Parser->new(
						  start_h         => [ \&editTag, 'tokenpos, text, tag, attr' ],
						  default_h       => [ sub{ $OUTPUT .= shift }, 'text' ],
						  process_h       => [ '', '' ],
						  comment_h       => [ '', '' ],
						  ignore_tags     => \@ignore_tags,
						  ignore_elements => \@ignore_elements,
						 );

#----------------------------------------

# construct another parser that we'll use to fix up relative URLs

# first, build a list of HTML tags that might have an href
# attribute. This is directly from the example in the HTML::Parser
# distribution
my %link_attr;
{
  no warnings;

  # To simplify things, reformat the %HTML::Tagset::linkElements
  # hash so that it is always a hash of hashes.
  require HTML::Tagset;
  while (my($k,$v) = each %HTML::Tagset::linkElements) {
	if (ref($v)) {
	  $v = { map {$_ => 1} @$v };
	}
	else {
	  $v = { $v => 1};
	}
	$link_attr{$k} = $v;
  }
}

# somewhere to dump the output of the second filter
my $OUTPUT2 = '';

# a second parser...
my $a = HTML::Parser->new(
						  default_h => [ sub { foreach ( @_ ) { $OUTPUT2 .= $_ } }, 'text' ],
						  start_h   => [ \&editLink, 'tagname, tokenpos, text' ]
						 );

#-------------------------------------------------------------------------------
# set up the user agent

# create a user agent
my $ua = LWP::UserAgent->new;

# tweak the agent identity to make it look like a nice, friendly
# Mozilla browser
$ua->agent( 'Mozilla/5.001 (windows; U; NT4.0; en-us) Gecko/25250101' );

# set up the proxy
$ua->proxy( [ 'http' ], $PROXY_URL );

#-------------------------------------------------------------------------------
# set up the DB connection and statement handles

my $dsn    = "dbi:mysql:$dbName:$dbHost:$dbPort";
my $dbAttr = { RaiseError => 1,
			   PrintError => 1 };

# connect
my $dbh = DBI->connect( $dsn, $dbUser, $dbPass, $dbAttr )
  or die "(EE) ERROR: couldn't connect to database: $!";

# prepare all the queries that we'll need

# query for all page titles
my $qsth = $dbh->prepare( 'SELECT auto_wiki, title FROM wikitext' )
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;

# query for singe rfamacc
my $psth = $dbh->prepare( 'select wt.auto_wiki, wt.title from wikitext as wt, rfam as r where wt.auto_wiki=r.auto_wiki and r.rfam_acc=?')
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;

# query for single page title
my $rsth = $dbh->prepare( 'select auto_wiki, title from wikitext where title=?')
  or die '(EE) ERROR: couldn\'t prepare query to retrieve Rfam IDs: ' . $dbh->errstr;

# insert HTML
my $hsth = $dbh->prepare( 'UPDATE wikitext SET text=? where auto_wiki=?' )
  or die '(EE) ERROR: couldn\'t prepare query to insert HTML: ' . $dbh->errstr;


#-------------------------------------------------------------------------------
#sanity checks on pages to check..
if (! $pages ){
    die "You must specify option '-pages' to  update  should be  'all' or a 'list' of families\n";
}

if ($pages eq 'list' &&  ( (! @rfacc) && (!$title)) ){
    die "You have spcified single family update but not specified which family (rfacc) to update\n";
}
if ($pages eq 'all' &&  ( (@rfacc) || ($title)) ) {
    die "You have spcified to update all familes but also provided a list of family ids??\n";
}
#format of family id correct.
if (@rfacc){
    foreach my $f (@rfacc){
        if ($f!~/RF\d{5}/) {
            die "The family accession specified isnt in the correct format\n"; 
        }
    }
}
#format of title correct.
if ($title){
    if ($title=~/\s/) {
	die "The page title specified has spaces-this isnt right-check it\n"; 
    }
}

if (! $update){
    warn  "\nNo update specified.\nYou will get a report but RDB won't be updated unless you specify the -update option\n\n"; 
}

#update single dat
if ( ! $fill && ! $changes){
    die "You need to specify a full update or number of days to update changes from??\n";  
}

# set the length of time to checkdate to check last 24 hours or last 100 days
my $lastday;
if ($changes){
   if ($changes !~/\d+/){
       die "You need to specify a number of days to check changes from??\n";
   }else{
       my $dt = DateTime->now;
       $dt->set_time_zone('UTC');
       $dt->subtract (days => $changes);
       #$dt->subtract (year => $changes);
       $lastday=$dt->ymd;
       print STDERR "Updating ", join(",",@rfacc), " pages with edits since $lastday ($changes days)\n";
   }
}

#get the rfamacc for this title



# get the list of Rfams to update
my $rfams;
if ($pages eq 'list' && @rfacc){
    $rfams=getOneRfam(\@rfacc);
    print STDERR "Specified only wikipages for these families: ", join(",", @rfacc), " to be looked at\n\n";
}elsif($pages eq 'list' && $title ){
    print STDERR "Specified title \'$title\' to be looked at\n\n";
    unless ($rfams=getOneTitle($title)){
	print STDERR "This title '$title' does not appear to exist in the current RDB\n. Check the title\n\n"; exit(0);
    }
}elsif ($pages eq 'all'){
    $rfams = getAllRfams();
    print STDERR "Requesting pages for all families in the RDB are looked at\n\n";
}else{
    die "You need to specify pages as 'all' or  'list'\n";
}



#----------------------------------------------------------------------------
# main

#list of one or many pages:
#keys ($ar)= auto_wiki AND $desc=page title
print STDERR "**********\n";
TITLE:foreach my $ar ( keys %$rfams ) {
  my $desc = $rfams->{$ar};
  my $rdbtitle=$desc;
  print STDERR "\nChecking title $rdbtitle for auto_wiki $ar\n";   
  sleep 2;
  ++$checked;
  # work around slashes in the title...
  my $wpid = '';
  foreach ( split "/", $rdbtitle ) {
	$wpid .= uri_escape( $_, q|^A-Za-z_-| ) . '/';
  }
  # bin the 
  chop $wpid;
  # test for the existence of a page
  print STDERR "(ii) checking for existence of a WP article for title  \"$wpid\"\n";
  unless( entryFound( $wpid ) ) {
	print STDERR "(EE) ERROR: no such entry for \"$desc\" (wp ID \"$wpid\")\n\n";
	   if ($title){
	       exit(0);
	   }else{ next;}
  }
 
  my $revisions;
  if ($changes){   
      # check if page has changed in last '$update" days
      print STDERR "(ii) check if page changed in last $changes days\n";
      #my $revisions;
      unless( $revisions = getChanges( $wpid ) ) {
          ++$unchanged;
          #print STDERR "(WW) WARNING: couldn't retrieve revisions for \"$title\" ( (wp ID \"$wpid\")\n\\n";
          next;
      }

      #report on these revisions
      #may be multiple revisions per page since last check.
      #ONLY do the rest if the page has changed;
      unless (! $revisions ){
          #++$page_count;
          foreach my $a (@{$revisions}){
              if (!$a->{comment}) {$a->{comment}='No comment';}
              print "http://en.wikipedia.org/wiki/", $title, "\t" , join("\t", $a->{user},$a->{timestamp}, $a->{comment}),"\n";
          }
       }   
  }#end of $changes checking if page has changed and getting update

  #unless specified to update the RDB, skip to the next page.
  unless ($update){
      ++$page_count;
      next TITLE;
  }
  #if changed or want a full update
      if ($revisions || $fill){
          ++$page_count;
          # scrape the HTML for this entry
          print STDERR "(ii) getting annotations\n";
          my $content;
          unless( $content = getContent( $wpid ) ) {
              print STDERR "(WW) WARNING: couldn't retrieve content for \"$desc\"\n";
              next;
          }

          # edit the HTML
          my $editedContent;
          unless( $editedContent = editContent( $content ) ) {
              print STDERR "(WW) WARNING: couldn't edit HTML for \"$desc\"\n";
              next;
          }
          
          #update the wikitext table
          print STDERR "(ii) updating the text in wikitext table for auto_wiki \"$ar\", \"$title\" \n\n";
          unless( storeContent( $editedContent, $ar ) ) {
              print STDERR "(WW) WARNING: couldn't update text in  auto_wiki for \"$ar\", \"$title\"\n";
              next;
          }
          
      } #end of if revisions and or full update

}#end of each rfam

  if ($changes){
      if ($page_count > 0) {
          print "\n$page_count changed : $unchanged unchanged : $checked checked\n";
          print "\n";
      }else{
          print "\nNo pages changed : $checked checked\n"
          } 
  }else{
      print "\n$page_count pages checked/updated\n";
  }

print STDERR "**You should now update the live RDB with the new data. run the following command\n\n\/software/rfam/bin/mysqldump -h pfamdb2a -u pfam -pmafp1 -P 3301 rfam_9_1 wikitext     | /software/rfam/bin/mysql -h pfamdb1 -u pfamwebadmin -pmafpwa rfam_9_1\\n\n";


exit;

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------
# get the changes for this page

# # use the wp API to check if a given entry changed

sub getChanges {
     my $desc = shift;
  
     #last day includes willow 
     my $url=  $WP_API . '?action=query&format=json&prop=revisions&format=json&titles='.$desc.'&rvdir=newer&rvstart='.$lastday.'T03:22:00Z&rvprop=timestamp|user|comment&rvlimit=500';
 
    my $req = HTTP::Request->new( GET => $url );
    my $res = $ua->request( $req );

    my $rv;
    if( $res->is_success ) {

        #API returns something like this for revision
	# {
	#   "query":{
	#        "pages":{
	#           "11243763":{
	#               "pageid":11243763,
	#               "ns":0,
	#               "title":"Y RNA",
	#               "revisions":[
	#                             {
	#                      "user":"Alexbateman",
	#                      "timestamp":"2007-05-29T12:24:42Z",
	#                      "comment":"Split entry into sections"
        #                             }
	#                           ]
        #                       }
        #               }
        #          }
        # }

       	# parse it and get the resulting data structure using the JSON module
	my $result = from_json( $res->content );

	# get the list of internal wp page IDs from the JSON
	my @ids= keys %{$result->{query}->{pages}};
	
       	# we're expecting only a single page, so check that we can get the
	# first key from the hash containing the pages. If the page
	# doesn't exist we will get a different data structure for which
	# this process won't work

	my $pageid = $ids[0];
	$rv = $result->{query}->{pages}->{$pageid}->{revisions}; #array of hashes
		
		
  } else {
	print  STDERR "(WW) WARNING: couldn't retrieve changes for \"$desc\":" . $res->status_line;;
  }
  #this should be a hash.
  return $rv;

}


#-------------------------------------------------------------------------------
# store the HTML content

sub storeContent {
  my( $content, $autowiki ) = @_;

  # store the auto_rfam number and HTML content in the wikihtml table
  $hsth->execute(  $content, $autowiki );

  print STDERR "(WW) WARNING: error whilst inserting HTML into wikitext table: "
               . $dbh->errstr . "\n"
	if $DBI::err;

  return defined $DBI::err ? 0 : 1;
}

#-------------------------------------------------------------------------------
# retrieve a list of Rfams to check

sub getAllRfams {

  $qsth->execute;

  my %rfams;
  while( my $row = $qsth->fetchrow_arrayref ) {
	$rfams{ $row->[0] } = $row->[1];
  }
  die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
	if $DBI::err;

  return \%rfams;
}

sub getOneTitle {
    my $title=shift;
    my %rfams;
    $rsth->execute($title); 
    my $row = $rsth->fetch() ;
	die '(EE) ERROR: error whilst retrieving rfam acc: ' . $dbh->errstr . "\n"
            if $DBI::err;
    
    if ($row){
	$rfams{ $row->[0]} = $row->[1];
	return \%rfams;
    }else{
	return;
    }

}

sub getOneRfam {
    my $rfams=shift;
    my %rfams;
    foreach my $rf (@$rfams){
        $psth->execute($rf); 
        while( my $row = $psth->fetchrow_arrayref) {
            $rfams{ $row->[0] } = $row->[1]; #auto_wiki # title
        }
        die '(EE) ERROR: error whilst retrieving Rfam IDs: ' . $dbh->errstr . "\n"
            if $DBI::err;
     }   
    return \%rfams;
}


#-------------------------------------------------------------------------------
# retrieve the HTML for a given entry

sub getContent {
  my $wpid = shift;

  # build the URL
  my $url = $WP . $wpid;

  # build a new request object and retrieve a response from it
  my $req = HTTP::Request->new( GET => $url );
  my $res = $ua->request( $req );

  # see if the request worked...
  my $content;
  if( $res->is_success ) {
    ( $content ) = $res->content =~ m/\<\!\-\- start content \-\-\>(.*)\<\!\-\- end content \-\-\>/s;
  } else {
	print STDERR "(WW) WARNING: couldn't retrieve content for \"$wpid\": "
	             . $res->status_line . "\n";
  }

  return $content;
}

#-------------------------------------------------------------------------------
# use the wp API to check if a given entry actually exists in wp

sub entryFound {
  my $desc = shift;

  my $url = $WP_API . "?action=query&prop=info&format=json&titles=$desc";

  my $req = HTTP::Request->new( GET => $url );
  my $res = $ua->request( $req );

  my $rv = 0;
  if( $res->is_success ) {

	# the API is returning the result of our query as a snippet of
	# JSON, looking something like this:
	#
	# {
	#   "query": {
	#              "normalized": [
	#                              {
	#                                "from": "RNase_MRP",
	#                                "to":   "RNase MRP"
	#                              }
	#                            ],
	#              "pages":      {
	#                              "11243833": {
	#                                            "pageid":    11243833,
	#                                            "ns":        0,
	#                                            "title":     "RNase MRP",
	#                                            "touched":   "2007-05-25T04:27:55Z",
	#                                            "lastrevid": 131269941,
	#                                            "counter":   0,
	#                                            "length":    1730
	#                                          }
	#                            }
	#            }
	# }
	#
	# parse it and get the resulting data structure using the JSON module
	my $result = from_json( $res->content );

	# get the list of internal wp page IDs from the JSON
	my @ids = keys %{ $result->{query}->{pages} };

	# we're expecting only a single page, so check that we can get the
	# first key from the hash containing the pages. If the page
	# doesn't exist we will get a different data structure for which
	# this process won't work
	$rv = shift @ids > 0 || 0;

  } else {
	print STDERR "(WW) WARNING: couldn't retrieve content for \"$desc\": "
                 . $res->status_line . "\n";
  }

  return $rv;
}


#-------------------------------------------------------------------------------
# Filter and edit the retrieved WP content

sub editContent {
  my $content = shift;

  # empty the output string and parse the retrieved content
  $OUTPUT = '';

  # the first pass filters out unwanted tags and attributes
  $p->parse( $content );

  # reset the parser so that we can use it again
  $p->eof;

  $OUTPUT2 = '';

  # the second pass fixes relative WP URLs and makes them absolute
  $a->parse( $OUTPUT );
  $a->eof;

  return $OUTPUT2;
}

#-------------------------------------------------------------------------------
# a handler for the HTML::Parser module. This is where we have logic
# to remove various bits and pieces of the HTML

sub editTag {
  my( $pos, $text, $tag, $attr ) = @_;

  # flag to show whether we're printing this tag or not
  my $printing = 1;

  # first, tidy the attributes
  if( @$pos >= 4 ) {

	my( $k_offset, $k_len, $v_offset, $v_len ) = @{$pos}[-4 .. -1];
	my $next_attr = $v_offset ? $v_offset + $v_len : $k_offset + $k_len;

	my $edited;
	while( @$pos >= 4 ) {

	  ( $k_offset, $k_len, $v_offset, $v_len ) = splice @$pos, -4;

	  if( $ignore_attr{ lc substr( $text, $k_offset, $k_len ) } ) {
		substr( $text, $k_offset, $next_attr - $k_offset ) = '';
		$edited++;
	  }

	  $next_attr = $k_offset;
	}

	# if we killed all attributes, kill any extra whitespace too
	$text =~ s/^(<\w+)\s+>$/$1>/ if $edited;
  }

  # print the line ?
  $OUTPUT .= $text if $printing;
	
}

#-------------------------------------------------------------------------------
# edit any tag that could have an attribute "href" and substitute a
# leading slash for the root of the WP URLs. Again, straight from the
# HTML::Parser example

sub editLink {
  my($tagname, $pos, $text) = @_;
  if (my $link_attr = $link_attr{$tagname}) {
	while (4 <= @$pos) {
	  # use attribute sets from right to left
	  # to avoid invalidating the offsets
	  # when replacing the values
	  my($k_offset, $k_len, $v_offset, $v_len) =
		splice(@$pos, -4);
	  my $attrname = lc(substr($text, $k_offset, $k_len));
	  next unless $link_attr->{$attrname};
	  next unless $v_offset; # 0 v_offset means no value
	  my $v = substr($text, $v_offset, $v_len);
	  $v =~ s/^([\'\"])(.*)\1$/$2/;
	  my $new_v = _edit($v, $attrname, $tagname);
	  next if $new_v eq $v;
	  $new_v =~ s/\"/&quot;/g;  # since we quote with ""
	  substr($text, $v_offset, $v_len) = qq("$new_v");
	}
  }
  $OUTPUT2 .= $text;
}

#-------------------------------------------------------------------------------
# a method to make the substitution in the attribute value

sub _edit {
  local $_ = shift;
  my( $attr, $tag ) = @_; 
  no strict;
  s|^(/.*)|$WP_ROOT$1|g;
  $_;
}

sub help {
    print STDERR <<EOF;

Code currently being on the way to being production-doing everything we need it to.
Currently can specify all families or a list.
Specify to check for changes OR just do an fill.
Can specify how far back (how many days..) to check for edits.
Facility to check for changes & report but not update the RDB-default is report only 

**need to add facily to 
**note revisions wont report anything > 500 edits. 

Usage:  ./scrape_cronjob_all.pl <options>
Options:       -h               show this help
		-pages		can be 'all' or 'list'
		-acc		must be a family acc e. RF00648. can specify multiple.
		-fill           for updating pages changed or not
		-changes	update if changed in n (integer) days
		-update		unless this option is specified the RDB is not updated.
  
e.g. check for changes in all pages in last 20 days update rdb
 scrape_cronjob_all.pl -pages all -changes 20 -update 

e.g. changes in pages for RF00648 and RF00122 in last 5 days
 scrape_cronjob_all.pl -pages list -acc RF00648 -acc RF00122 -changes 5 -update 

e.g.update the page for RF00648
 scrape_cronjob_all.pl -pages list -acc RF00648 -fill

EOF

}


#-------------------------------------------------------------------------------
