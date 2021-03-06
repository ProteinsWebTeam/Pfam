#!/software/bin/perl
#
# update_features_sources.pl
# jt6 & pg6 20060428 WTSI
#
# Update the list of DAS sources in the database. This is intended to
# be run as a cron job, as often as required to keep the database
# table up to date.
#
# The script populates a table that's created as follows:
#   CREATE TABLE feature_das_sources (
#     server_id VARCHAR(40) NOT NULL,
#     system VARCHAR(200) NOT NULL,
#     sequence_type VARCHAR(200) NOT NULL,
#     name VARCHAR(100) NOT NULL,
#     url VARCHAR(200) NOT NULL,
#     helper_url VARCHAR(200) NULL,
#     default_server TINYINT(1) NOT NULL,
#     PRIMARY KEY(server_id, system, sequence_type)
#   );
#
# $Id: update_feature_sources.pl,v 1.5 2009-09-25 08:48:04 pg6 Exp $
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk), Prasad Gunasekaran (pg6@sanger.ac.uk),
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

use warnings;
use strict;

use Bio::Das::Lite;
use Data::Validate::URI qw( is_uri );
use DBI;
use Time::Local;
use Config::General qw(ParseConfig);
#use Getopt::Std;
use Getopt::Long;
use Data::Dump qw(dump);
use Mail::Mailer;
my %options;
my $file;
my $force = '';
GetOptions(
  "force!"   =>  \$force,
  "config=s"  =>  \$file
) or usage();

unless( defined $file ) {
  print STDERR  "error: must specify a configuration file\n";
  exit 1;
}


#read the apache style configuration file and parse it.
my ( %config,$mail);

eval{
  %config = ParseConfig( $file );
  $mail->{from} = $config{das}->{mailFrom};
  $mail->{to} = $config{das}->{mailTo};  
};
if($@){
  my $error_message = "error: there was a problem parsing the configuration file\n$@";
  send_mail($error_message,$mail);
}

# Bio::Das::Lite setup
my $DAS_DSN   = $config{das}->{dasDsn};
my $DAS_PROXY = $config{das}->{dasProxy};
my $DAS_TO    = $config{das}->{dasTo};

# db setup
my $DB_NAME   = $config{Model}->{WebUser}->{name};
my $DB_HOST   = $config{Model}->{WebUser}->{host};
my $DB_PORT   = $config{Model}->{WebUser}->{port};
my $DB_USER   = $config{Model}->{WebUser}->{admin_user};
my $DB_PASS   = $config{Model}->{WebUser}->{admin_pass};

my $DB_DSN    = "dbi:mysql:database=$DB_NAME;host=$DB_HOST;port=$DB_PORT";

# todays date, in seconds since the epoch
my $cd = time;

# get the lists of default servers and those to ignore entirely
my %defaultServers = map { $_ => 1 } @{ $config{das}->{defaultServer} };
my %ignoreServers  = map { $_ => 1 } @{ $config{das}->{ignoreServer} };

# main

# get a Bio::Das::Lite object that's connected to the specified registry
my $das = Bio::Das::Lite->new( { dsn     => $DAS_DSN,
                                 timeout => $DAS_TO,
                                 proxy   => $DAS_PROXY } );

# get a database handle
my $dbh = DBI->connect( $DB_DSN,
                        $DB_USER,
                        $DB_PASS,
                        { RaiseError => 1,
                          PrintError => 0,
                          AutoCommit => 0 } );

# create a temporary table and populate the results there;
eval{
  $dbh->do( "create temporary table temp like feature_das_sources" );
};
if( $@ ){
  my $error_message = "error: Temporary table creation failed:$@";
  send_mail($error_message,$mail);
}

#get the total number of features sources from the database;
my $feature_sources = $dbh->selectrow_arrayref("select count(*) from feature_das_sources");
my $total_features = $feature_sources->[0];
#print STDERR "The total number of features sources available in database is $total_features\n";


# prepare the queries
my $insertSth = $dbh->prepare( "INSERT INTO temp ( server_id, name, url, system, sequence_type, helper_url, default_server ) VALUES( ?, ?, ?, ?, ?, ?, ? )" );

# get the list of sources with features capability.
my $sourcesList = $das->registry_sources({capability  =>  'features'});
#print STDERR "(ii) retrieved " . scalar @$sourcesList . " sources from the registry\n"; 

# decide which sources we want to use
my $chosenList = [ ]; my $sources_to_populate = 0;
foreach my $source ( @$sourcesList ) {
  
  # check the lease date
  #$source->{leaseDate} =~ m/^(\d{4})\-(\d{2})\-(\d{2})T(\d{2}):(\d{2}):(\d{2}).(\d{3})Z$/i;
  $source->{leaseDate} =~ m/^(\d{4})\-(\d{2})\-(\d{2})$/i; 

  # convert the lease date into seconds since the epoch. Note that we need 
  # to subtract 1 from the month, since we need it zero-based but the 
  # registry date comes with it as and month-of-the-year, i.e. 1-based
  #my $ld = timelocal( $6, $5, $4, $3, $2 - 1, $1 );
 
  my $ld = timelocal( 0,0,0,$3, $2 - 1, $1 );
 
 # delta, in seconds
  my $dd = $cd - $ld;

  # don't add the source if the lease is older than two days
  if( $dd > $config{das}->{ActiveTime}) {
    #print STDERR  "(ww) skipping \"$source->{nickname}\" ($source->{id}); down for ".int($dd/86400)." days\n";
    next;
  }

  # don't add the source if it's in the "ignore" list
  if( $ignoreServers{ $source->{id} } ) {
    #print STDERR  "(ww) ignoring \"$source->{nickname}\" ($source->{id})\n";
    next;
  }

  my $entry = {};
  $entry->{id}        = $source->{id};
  $entry->{name}      = $source->{nickname};
  $entry->{url}       = $source->{url};
  $entry->{helperurl} = $source->{helperurl};

  # trim any trailing slashes off the URLs
  {
    $/ = "/";
    chomp $entry->{url}       if defined $entry->{url};
    chomp $entry->{helperurl} if defined $entry->{helperurl};
  };

  # validate the URLs
  unless( is_uri( $entry->{url} ) ) {
    #print STDERR  "(ww) skipping \"$source->{nickname}\" ($source->{id}); invalid URL ($source->{url})\n";
    next;
  }

  if( $entry->{helperurl} and not is_uri( $entry->{helperurl} ) ) {
    #print STDERR  "(ww) skipping \"$source->{nickname}\" ($source->{id}); invalid help URL ($entry->{helperurl})\n";
    next;
  }

#  # we're only interested in features; its not necessary as we already filter the sources 
#  unless( grep /features/, @{ $source->{capabilities} } ) {
#    print STDERR  "(ww) skipping \"$source->{nickname}\" ($source->{id}); no features\n";
#    next;
#  }

  foreach my $coords ( @{ $source->{coordinateSystem} } ) {
    if( defined $entry->{coords}{$coords->{uniqueId}} ) {
      #print STDERR  "(ww) ignoring duplicate co-ordinate system for $source->{id}\n";
      next;
    }
    $entry->{coords}{ $coords->{uniqueId} } = { system => $coords->{name}, 
                                                type   => $coords->{category} };
    $sources_to_populate++;                                                
  }
  
  if( defined $entry->{coords} ) {
    push @$chosenList, $entry;
  } else {
    #print STDERR  "(ww) no coordinates found for $source->{id}\n";
  }
}


#make sure we get certain number of sources, if not exit with sending mail 

unless( $force ){  
  my $message;
  if( $total_features ){
    #print " enters into the total_features blcok with value| $total_features|\n";
    print "the total number of sources to populate is ".$sources_to_populate."\n";
    my $percentage = ( $total_features - $sources_to_populate ) / $total_features;
    print " the total loss of percentage is $percentage\n";
    if( $percentage > $config{das}->{threshold} ){
      $message = "   
                  The total number of features sources present in Database is $total_features\n
                  The total number of features sources retrieved from das is ".scalar(@$sourcesList)."\n
                  the total sources which succeeds validation is ".scalar( @$chosenList ).",\n
                  Due to multiple coordinate system for single source, the source to be popualted is $sources_to_populate\n                                    
                  More than 10% of active das sources are lost......\n
                  Hence skipping the update\n";
                         
      send_mail($message,$mail);
    }  
  }else{
    #print " enters into the else total_features blcok with value| $total_features|\n";
    $message = "
                There is no feature sources present in database.\n
                Henceforth gain or loss percentage could not be calcualted.\n
                Hence skipping the process;\nIts advisory to use the script with option -force.\n  
                ";
    send_mail($message,$mail);
  }
}


# update the database
#print STDERR  "(ii) opening transaction...\n";

# start a transaction...
eval {
  
  foreach my $entry( sort { $a->{id} cmp $b->{id} } @$chosenList ) {
    
    foreach my $coord (values %{ $entry->{coords} } ) {
      #print "the coord is ",dump($coord),"\n";
      # print STDERR "(ii) inserting $entry->{name} [$coord->{system},$coord->{type}]... \n";
       $insertSth->execute( $entry->{id},
                           $entry->{name},
                           $entry->{url},
                           $coord->{system},
                           $coord->{type},
                           $entry->{helperurl},
                           exists $defaultServers{ $entry->{id} } ? 1 : 0
                         );  
    }
  
  }
  #print STDERR  "(ii) committed changes in table\n";
  
  $dbh->commit;
}; # end of "eval"


# before copying the contents of temporary table; look at the diff of both;

# source in temp table but not in database table;
my $src_in_temp_notin_das = "Select server_id, system,name from temp where server_ID not in ( select server_id from feature_das_sources )";
my $diff1 = $dbh->prepare( $src_in_temp_notin_das );

# source in das table but lost in temp table;
my $src_in_das_notin_temp = "Select server_id, system, name from feature_das_sources where server_ID not in ( select server_id from temp )";
my $diff2 = $dbh->prepare( $src_in_das_notin_temp );

eval{
  $diff1->execute();
  $diff2->execute();    
};


my ($gain, $loss );

while( my ( $id, $system,$name ) = $diff1->fetchrow_array ){
  $gain .= "$id\t$system\t$name\n";
}
while( my ( $id, $system,$name ) = $diff2->fetchrow_array ){
  $loss .= "$id\t$system\t$name\n";
}

my $success_message;

eval{
  $dbh->do( "delete from feature_das_sources ");
  $dbh->do( "insert into feature_das_sources ( select * from temp )");
  $dbh->commit;
};
if( $@ ){
  my $error_message = "ERROR in Deleting the contents of table and populating it : $@\n";
  send_mail($error_message,$mail);
}else{
  #print "eval success\n";
  my $new_feature_sources = $dbh->selectrow_arrayref("select count(*) from feature_das_sources");
  my $new_total_features = $new_feature_sources->[0];
  $success_message = "The total number of features sources present in Database before update is : $total_features
                         \n\nThe total number of features sources after update is :$new_total_features\n
                         \nHenceforth Update successful.\n";
  if( defined $gain ){
    $success_message .= "Gained sources:\n$gain\n";
  }elsif( defined $loss ){
    $success_message .= "Lost sources:\n$loss\n";
  }
  send_mail($success_message,$mail); 
}

exit;

#-----------------------------------------------------------------------------------------------------

#subroutine send_mail - sending mail to the maintainer

sub send_mail {  
  my ( $message, $mail ) = @_;
  my $mailer = Mail::Mailer->new();
  my $header = {
      To      => $mail->{to},
      from    => $mail->{from},
      Subject => "Cron job- For updating features table"
     };
    $mailer->open($header);
    print $mailer $message,"\n";
    $mailer->close;
    exit;        
}

#-----------------------------------------------------------------------------------------------------

#subroutine usage - Explains the usage of the script

sub usage {
  print <<EOF_help;
usage: $0 [-h] [-force] -config config_file 

 -config config_file : the Apache-style configuration file
 -force : Dont check the contents of database
 -h             : prints this message
 
EOF_help

  exit;
}

