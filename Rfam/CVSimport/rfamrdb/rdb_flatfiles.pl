#!/usr/local/bin/perl 

## Written by Mhairi Marshall & Kevin Howe

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'PFAM_MODULES_DIR'})
            ?$ENV{'PFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;


use strict;
use Getopt::Long;
use FileHandle;



use RfamRCS;
use UpdateRDB;
use Rfam::DB::DB_RDB;
use Rfam;
#require "/nfs/team71/pfam/mm1/rfam_cvs/scripts/Modules/Rfam.pm";


my ( $dir, $dump_to_file, $load_from_file, $database, $part_copy);

&GetOptions( 
	    'dir=s' => \$dir,
	    'dump_to_file' => \$dump_to_file,
	    'load_from_file' => \$load_from_file,
	    'database=s' => \$database,
	    'part_copy=s' => \$part_copy



);

if ( (!$dump_to_file) && (!$load_from_file) ) {
  print "need to say whether to load db from files : -- load_from_file or dump db to files : --dump_to_file \n";
  exit(1);
}

if (!$database) {
  print "need database : --database \n";
  exit(1);
}


#die "Need the database --database \n" if (!$database);
die "Need the dir for the files --dir \n" if (!$dir);

my $rdb;
my $rdb_query;


################# TEMP ########################


my $db_driver_tgt = $Bio::Rfam::rdb_driver;
my $db_user_tgt = $Bio::Rfam::rdb_user;
my $db_host_tgt = $Bio::Rfam::rdb_host;
my $db_pass_tgt = $Bio::Rfam::rdb_pass;

#print "$db_driver_tgt, $db_user_tgt, $db_host_tgt, $db_pass_tgt
#  $rdb = UpdateRDB->new('-db_name' => $tgt_rdb_name,
#  $rdb = Bio::Rfam::DB_RDB->new('-db_name' =>  $tgt_rdb_name,
 
#eval {
#  $rdb = UpdateRDB->new('-db_name' => $database,
#			'-db_host' => $db_host_tgt, 
#			'-db_user' =>  $db_user_tgt,
#			'-db_driver' => $db_driver_tgt,
#			'-db_password' => $db_pass_tgt
#		       );
#};

#if ($@) {
#  die "Could not connect to rdb $database [$@]";
#}
$rdb = update_database($database);
eval {
#  $rdb_query = Rfam::DB::DB_RDB->new('-db_name' =>  $database,
#				'-db_host' => $db_host_tgt, 
#				'-db_user' =>  $db_user_tgt,
#				'-db_driver' => $db_driver_tgt,
#				'-db_password' => $db_pass_tgt
#			       );

  $rdb_query = query_database($database);
};

if ($@) {
  die "Could not connect to rdb $database [$@]";
}


my %static_tables;
if ($part_copy) {
  my @rfam = qw( chromosome_build genome_entry dead_families rfam rfam_database_links rfam_literature_references rfam_reg_full rfam_reg_seed  rfamseq  );
  my @mirna = qw  (  mirna mirna_chromosome_build mirna_families mirna_gene mirna_gene_alignment mirna_gene_contigs mirna_gene_database_links mirna_gene_homologue mirna_literature_references mirna_mature mirna_pre_mature mirna_species );
  if ($part_copy =~ /mirna/i) {
    foreach (@rfam) {
      $static_tables{$_} = $_;
    }

  } elsif ($part_copy =~ /rfam/i) {

    foreach (@mirna) {
      $static_tables{$_} = $_;
    }
  }
}

#print "\nHERE \n";
print "QUERY: $rdb_query, database: $database \n";
my (@temp1) = $rdb_query->query("show tables");
#print "\nWOW \n";

my ($schema_file);
$schema_file = $dir . "/database_schema";
if ($dump_to_file) {
  open(_FILE, ">$schema_file");
  close(_FILE);
} elsif ($load_from_file) {


}

foreach my $temp_table (@temp1) {

  my ($table) = @{$temp_table};

  next if (defined($static_tables{$table}));
  next if ($table eq "mirna_gene_alignment");

  print "Doing table: $table\n";
  my $file_name = "rdb_" . $table . ".dat";

  my $zipped_file = $file_name . ".gz";

  if ($dump_to_file) {
    my (@create) = $rdb_query->query("show create table $table");

    open(_FILE, ">>$schema_file");
    print _FILE "drop table $table; \n";
    foreach (@create) {   
      my @rows;
    
      push @rows, @{$_};
      my $first = 1;
      foreach my $row (@rows) {
	$row =~ s/ TYPE\=MyISAM/\;\n/;
	print _FILE "$row \n" if (!$first);
	$first = 0;
      }
      
    
    }
    

    my $outfile = $dir . "/" . $file_name;
    my (@temp) = $rdb_query->query("select * from $table into outfile '$outfile'");
    open(_FILE, "$outfile")  or print "\ncant open $dir/$file_name which was downloaded from database: $database \n\n";
    my $line_count = 0;
    while(<_FILE>) {
      $line_count++;

    }
   
   
    my (@tmp_table_count) = $rdb_query->query("select count(*) from $table");

    my ($table_count) = @{$tmp_table_count[0]};

   die "table: $table problem with file downloaded to $outfile , may be corrupted \n" if ( ($line_count ne $table_count) && ($table ne "interpro_and_go") );
    system("gzip $outfile");

  } elsif ($load_from_file) {
  
 
    if ( (-e "$dir/$file_name") || (-e "$dir/$zipped_file") ) {
      
      if ( -e "$dir/$zipped_file") {
	
	system("gunzip $dir/$zipped_file");

      }

	my $line_count = 0;
	open(_FILE, "$dir/$file_name") or print "\ncant open $dir/$file_name to upload to database: $database \n\n";
	while(<_FILE>) {
	  $line_count++;
	}

	close(_FILE);

	$rdb->load_generic_file_to_rdb($dir . "/" . $file_name, $table, 1);

	my (@tmp_table_count) = $rdb_query->query("select count(*) from $table");

	my ($table_count) = @{$tmp_table_count[0]};

	die "table: $table problem with file to be uploaded: $dir/$file_name , may be corrupted \n" if ( ($line_count ne $table_count) && ($table ne "interpro_and_go") );
	
	system("gzip $dir/$file_name");
   

    } else {

      print STDERR "$table cant be uploaded to as no file in $dir - may be a static table\n" if(undef($static_tables{$table}));
    }


  }
#  exit(0);
}

sub query_database {

  my($rdb) = shift;

  my $dbh;

  if ($rdb eq "rfam") {
    $dbh =  Rfam::external_rdb();
  } elsif ($rdb eq "rfam2") {
     $dbh =  Rfam::switchover_rdb();

  } elsif ($rdb eq "rfamlive") {
     $dbh = Rfam::live_rdb()
    
  } elsif ($rdb eq "rfam_temp") {
    $dbh =  Rfam::temp_rdb()
  }

  return $dbh;

}

sub update_database {

  my($rdb) = shift;

  my $dbh;

  if ($rdb eq "rfam") {
    $dbh =  Rfam::external_rdb_update();
  } elsif ($rdb eq "rfam2") {
     $dbh =  Rfam::switchover_rdb_update();

  } elsif ($rdb eq "rfamlive") {
     $dbh = Rfam::live_rdb_update()
    
  } elsif ($rdb eq "rfam_temp") {
    $dbh =  Rfam::temp_rdb_update()
  }

  return $dbh;

}
