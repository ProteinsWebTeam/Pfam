#!/usr/bin/env perl

#Script to make database files for website 
#Script makes a dir in cwd called database_files
#which contains all the database files

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Log::Log4perl qw(:easy);
use File::Copy;

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my ($host, $user, $password, $port, $database) = ($pfamDB->{host}, $pfamDB->{user}, $pfamDB->{password}, $pfamDB->{port}, $pfamDB->{database});

#Make a database_files dir in cwd
my $dir = "database_files";
unless(-d $dir) {
  mkdir($dir, 0755) or $logger->logdie("Could not make direcotry $dir, $!");
}

#Make .sql, .innodb.sql and .txt database files for each table
$logger->info("Making database files for $database");
open(LIST, "mysql -h $host -u $user -p$password -P $port --skip-column-names $database -e \"show tables\" |");
while(<LIST>) {
  chomp $_;
  my $table = $_;
  next if($table =~ /^_/);  #Ignore tables that start with '_'

  $logger->info("Doing $table");

  #Innodb tables
  system("mysqldump -h $host -u $user -p$password -P $port -d $database $table > $dir/$table.innodb.sql") and $logger->logdie("Couldn't get innodb table for  $table, $!");

  #MyISAM tables (replace InnoDB with MyISAM in innodb.sql files to make .sql files)
  copy("$dir/$table.innodb.sql", "$dir/$table.sql") or $logger->logdie("Couldn't cp $dir/$table.innodb.sql to $dir/$table.sql, $!");
  system("sed -i 's/InnoDB/MyISAM/g' $dir/$table.sql") and $logger->logdie("Couldn't run sed command on $dir/$table.sql, $!");

  #Data files
  system("mysql -h $host -u $user -p$password -P $port --skip-column-names --max_allowed_packet=1024M --quick $database -e \"select * from $table\" > $dir/$table.txt") and $logger->logdie("Couldn't get data from $table, $!");

  #Gzip the files
  foreach my $file ("$table.innodb.sql", "$table.sql", "$table.txt") {
    if(-s "$dir/$file") {
      system("gzip $dir/$file");
    }
    elsif($file eq "$table.txt" and -e "$dir/$file") {
      $logger->die("$table.txt is empty");
    }
    else {
      $logger->logdie("Failed to make $dir/$file");
    }
  }
}
close LIST; 



