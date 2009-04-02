#!software/bin/perl
#
# update_feature_sources.pl
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
# $Id: update_feature_sources.pl,v 1.1 2009-04-02 10:07:02 jt6 Exp $
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk), 
#          Prasad Gunasekaran (pg6@sanger.ac.uk)
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
use Config::General;
use Getopt::Std;
use Mail::Mailer;

my %options;
getopt( 'f', \%options ) or usage();

unless( defined $options{f} and -f $options{f} ) {
  print STDERR  "error: must specify a configuration file\n";
  exit 1;
}

#read the apache style configuration file and parse it.
my ( $conf, %config, $mail );
eval{
  $conf = new Config::General( $options{f} );
  %config = $conf->getall;
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

#get the total number of features sources from the database;
my $feature_sources = $dbh->selectrow_arrayref("select count(*) from feature_das_sources");
my $total_features = $feature_sources->[0];
#print STDERR "The total number of features sources available in database is $total_features\n";


# prepare the queries
my $insertSth = $dbh->prepare( "INSERT INTO feature_das_sources ( server_id, name, url, system, sequence_type, helper_url, default_server ) VALUES( ?, ?, ?, ?, ?, ?, ? )" );

# get the list of sources with features capability.
my $sourcesList = $das->registry_sources({capability  =>  'features'});
#print STDERR "(ii) retrieved " . scalar @$sourcesList . " sources from the registry\n"; 

# decide which sources we want to use
my $chosenList = [ ];
foreach my $source ( @$sourcesList ) {
  
  # check the lease date
  $source->{leaseDate} =~ m/^(\d{4})\-(\d{2})\-(\d{2})T(\d{2}):(\d{2}):(\d{2}).(\d{3})Z$/i;

  # convert the lease date into seconds since the epoch. Note that we need 
  # to subtract 1 from the month, since we need it zero-based but the 
  # registry date comes with it as and month-of-the-year, i.e. 1-based
  my $ld = timelocal( $6, $5, $4, $3, $2 - 1, $1 );

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
  }
  
  if( defined $entry->{coords} ) {
    push @$chosenList, $entry;
  } else {
    #print STDERR  "(ww) no coordinates found for $source->{id}\n";
  }
}


#make sure we get certain number of sources, if not exit with sending mail 

my $percentage = ( $total_features - scalar( @$chosenList ) ) / $total_features;
if( $percentage > $config{das}->{threshold} ){
  my $message = "
                 The total number of features sources present in Database is $total_features\n
                 The total number of features sources retrieved from das is ".scalar(@$chosenList)."\n
                 More than 10% of das sources are lost......\n
                 Das registry may be down......\nHence skipping the update\n";
  send_mail($message,$mail);                   
}


# update the database
#print STDERR  "(ii) opening transaction...\n";

# start a transaction...
eval {
  
  print STDERR "\n\nDeleting the contents of the feature_das_source table \n" if($dbh->do( "DELETE FROM feature_das_sources" ));
  foreach my $entry( sort { $a->{id} cmp $b->{id} } @$chosenList ) {

    foreach my $coord (values %{ $entry->{coords} } ) {
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

# check for errors in the transaction and roll back if we found any

if( $@ ) {
  #print STDERR "\n(EE) ERROR: transaction error: $@; rolling back\n";
  
  eval { $dbh->rollback; };
  if($@){
    my $error_message = "ERROR in transaction, rolling back\nHowever roll back failed : $@\n";
    send_mail($error_message,$mail);
  }  
  
} else {
   #print STDERR "(ii) transaction successful\n";
   my $new_feature_sources = $dbh->selectrow_arrayref("select count(*) from feature_das_sources");
   my $new_total_features = $new_feature_sources->[0];
   my $success_message = "The total number of features sources present in Database before update is : $total_features
                         \n\nThe total number of features sources after update is :$new_total_features\n
                         \nHenceforth Update successful.\n";
   send_mail($success_message,$mail);                         
}

#script wont come here but to exit explicitly
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
usage: $0 [-h] -f config_file

 -f config_file : the Apache-style configuration file
 -h             : prints this message
 
EOF_help

  exit;
}

