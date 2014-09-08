#!/usr/bin/env perl

use strict;
use warnings;
use LWP::Simple qw( $ua );
use File::Temp qw(tempfile);
use File::Touch;
use XML::XPath;
use XML::XPath::XMLParser;
use IO::Uncompress::Gunzip qw(gunzip $GunzipError) ;
use Cwd;
use Log::Log4perl qw(:easy);
use File::Path qw(make_path);
use Parallel::ForkManager;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my($statusdir, $threads);
$threads = 8;
&GetOptions( "statusdir=s"   => \$statusdir,
             "threads=i" ) or die "Invalid option!\n";




#---------------------------------------------------------------------------------
#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

unless(defined($statusdir) ){
  $logger->logdie("The status directory was not defined as an option.");    
}
unless(-d $statusdir){
  $logger->logdie("The status directory, $statusdir, does not exist.");  
}

#Database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#---------------------------------------------------------------------------------
#Main

my($pdbs);
if(-e "$statusdir/got_pdb_info"){
  $logger->info("Already got the PDB entry information");
  my $pdb_st = $dbh->prepare("select pdb_id from pdb");
  $pdb_st->execute;
  %{$pdbs} = map {$_->[0] => 1 }  @{$pdb_st->fetchall_arrayref}; 
}else{
  $pdbs = getPdbInfo($logger, $config, $dbh);
  touch("$statusdir/got_pdb_info");
}

$logger->info("Going to get mapping");
if(-e "$statusdir/got_pdb_mapping"){
  $logger->info("Already got the PDB mapping information");
}else{
  $logger->info("Getting UniProt mapping");
  getUniProtMapping($logger, $config, $pdbs, $dbh, $statusdir, $pfamDB, $threads );
  touch("$statusdir/got_pdb_mapping");
}



#---------------------------------------------------------------------------------

sub getPdbInfo {
  my($logger, $config, $dbh) = @_;
  
  #Set the proxy on the user agent if we have one
  #$ua->proxy( http => $config->proxy ) if($config->proxy);
    
  $logger->info("Fetching PDB information.");
  #Build the XML to get all of the current holdings in PDB.
  my $XML_query = qq(<?xml version="1.0" encoding="UTF-8"?>
  <orgPdbQuery>
    <version>head</version>
    <queryType>org.pdb.query.simple.HoldingsQuery</queryType>
    <description>Holdings : All Structures</description>
    <experimentalMethod>ignore</experimentalMethod>
    <moleculeType>ignore</moleculeType>
  </orgPdbQuery>);

  # Create a request                                                                                  
  my $request = HTTP::Request->new( POST => 'http://www.rcsb.org/pdb/rest/search/');
  $request->content_type( 'application/x-www-form-urlencoded' );
  $request->content( $XML_query );
  my $response = $ua->request( $request );

  # Check to see if there is an error
  unless( $response->is_success ) {
      $logger->logdie("An error occurred querying the PDB: ", 
                          $response->status_line);
  }
  $logger->info("Got list of PDB IDs.");
    
  my $localdbs = $config->localDbsLoc;
  $localdbs .= "/PDB";
  open(PDB, ">$localdbs/pdb.dat") or $logger->logdie("Could not open $localdbs/pdb.dat");


  #Now for each pdb id, get the data we want from the RCSB REST interface (batches of 1000 ids).
  #We get a CSV file that we process 
  
  my %currentPdbs;
  my @pdbIds = split(/\n/, $response->content);
  while(@pdbIds){
    my @top300 = splice(@pdbIds, 0, 300);
    foreach (@top300){
      $currentPdbs{$_}++;
    }
    my $list = join(",", @top300);
    my $request2 = HTTP::Request->new( GET => 'http://www.rcsb.org/pdb/rest/customReport?pdbids='.$list.
                                            '&customReportColumns=classification,structureTitle,releaseDate,'.
                                            'resolution,experimentalTechnique,structureAuthor&service=wsdisplay&format=csv');
    my $response2 = $ua->request($request2);
    if($response2->is_success){
      #split on the br, and skip the first "header" line
      #print STDERR $response2->content;
      #my $xp = XML::XPath->new(xml => $response2->content);
      #my $records = $xp->find('/dataset/record');
      my @data = split(/<br \/>/, $response2->content);
      #Skip first element as it has the header!
      for(my $i=1; $i<=$#data; $i++){
        $data[$i] =~ s/""/"\\N"/g; #Change "" for nulls.
        $data[$i] =~ s/","/\t/g;
        $data[$i] =~ s/"//g;
        print PDB $data[$i]."\n";
      }
#      my $seen;
#      foreach my $record ($records->get_nodelist){
#        if(!$xp->find('structureId', $record)->shift){
#          $logger->logdie("Failed to get structureId for ".XML::XPath::XMLParser::as_string($recode));
#        }
#        my $thisPdb = $xp->find('structureId', $record)->shift->string_value;
#        next if($seen->{$thisPdb});
#        $seen->{$thisPdb}++;
#        print PDB "$thisPdb\t";
#        foreach my $element (qw(classification structureTitle releaseDate resolution experimentalTechnique structureAuthor)){
#          my $d = $xp->find($element, $record)->shift;
#          print PDB (defined($d) ? ( $d->string_value eq 'null' ? '\N' : $d->string_value ) : '\N');
#          if($element eq 'structureAuthor'){
#            print PDB "\n";
#          }else{
#            print PDB "\t";
#          }
#        }
#      }
    }else{
      $logger->logdie("Failed to get info for $list:".$response2->status_line);
    }
  } 
  close(PDB);
  $logger->info("Got titles etc. for PDB IDs.");
   
   
  foreach my $table (qw(pdb_residue_data pdb_image pdb_pfamA_reg pdb_pfamB_reg pdb)){
    $dbh->do("truncate $table")
    or $logger->logdie("Failed to delete contents from $table:".$dbh->errstr);
  }
  $logger->info("Deleted content from PDB tables");

  #Now load into the table.
  $logger->info("Loading PDB table information.");
  my $sth = $dbh->prepare("INSERT INTO pdb (pdb_id, keywords, title, date, resolution, method, author) values (?, ?, ?, ?, ?, ?,?)"); 
  _loadTable($dbh, "$localdbs/pdb.dat", $sth, 7); 
  return (\%currentPdbs);
}


sub _loadTable {
  my ( $dbh, $file, $sth, $cols ) = @_;

  my $batchsize = 5000;
  my $report    = 100000;
  my $reportNo  = 1000000;
  my $count     = 0;


  $dbh->begin_work;    # start a transaction

  open( my $input, '<', $file ) or die "Could not open $file:[$!]";

  while ( my $record = <$input> ) {
    chomp $record;
    my @values = split( /\t/, $record );
    for ( my $i = 0 ; $i < $cols ; $i++ ) {
      $values[$i] = undef if ( !defined($values[$i]) or $values[$i] eq '\N');
    }
    $sth->execute(@values);

    $count += 1;
    if ( $count % $batchsize == 0 ) {
      $dbh->commit;    # doublecheck the commit statement too
      if ( $count % $report == 0 ) {
        if ( $count % $reportNo == 0 ) {
          print STDERR "$count";
        }
        else {
          print STDERR ".";
        }
      }

      $dbh->begin_work;

    }
  }
  $dbh->commit;
}

sub getPfamseqAccs {
  my($logger, $dbh) = @_;
  
  #In teory could have prepared this earlier! However, the script has checkpoint files
  #so can recover.
  my $st = $dbh->prepare("select pfamseq_acc from pfamseq"); #mySQL statement updated for new schema
  $st->execute() or $logger->logdie("Couldn't select pfamseq_acc from pfamseq table ".$st->errstr."\n");
  my $data = $st->fetchall_arrayref;
  my %pfamseq;
  foreach (@$data){
    $pfamseq{$_->[0]} = 1; #updated as the mySQL statement above only searches for pfamseq_acc now so can't populate hash with pfamseq_acc keys and auto_pfamseq values
  }

  return \%pfamseq;
}


sub mirrorSifts {
  my ($logger, $url, $mirrorDir) = @_;
  
  $logger->info("Updating sifts mirror directory");
  
  #Chdir
  my $pwd = getcwd;
  chdir($mirrorDir) 
    or die "Could not chdir to ".$mirrorDir."\n";
  
  
  my $cmd = 'wget --no-proxy -mq '.$url.' '.$mirrorDir;
  print "\n\n$cmd\n\n";
  #This uses wget to mirror the directory.
  system( $cmd ) and 
         $logger->logdie("Failed to wget sifts [ $cmd ] : [$?]");
  
  chdir($pwd);
}


sub parseUpdatedSifts{
  my($logger, $dbh, $pfamdb, $dir, $statusdir, $threads) = @_;

  #Get a list of all PDBs
  my @rows = $pfamdb->getSchema->resultset('Pdb')->search;
  my $pm = new Parallel::ForkManager($threads);
  my $update = 0;
  foreach my $row (@rows){
    my $pdb_id = lc($row->pdb_id);
    my $bit = substr($pdb_id, 1, 2);
    
    make_path("$statusdir/sifts/$bit");
    next if( -e "$statusdir/sifts/$bit/$pdb_id");
    my $f = $dir.'/'.$bit.'/'.$pdb_id.'.xml.gz';
    next unless( -e $f);
    $pm->start and next;
     my $pfamDBThread = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
     my $dbhThread = $pfamDBThread->getSchema->storage->dbh;
     extractAndParse($f, $dbhThread);
     touch("$statusdir/sifts/$bit/$pdb_id");
     $pm->finish;
  }
  $pm->wait_all_children;
}


sub getUniProtMapping {
  my ($logger, $config, $pdbs, $dbh, $statusdir, $pfamdb, $threads ) = @_;
  $logger->debug("In getUniProtMapping");
  my $destserv='ftp://ftp.ebi.ac.uk'; 
  my $destpath = '/pub/databases/msd/sifts/split_xml/';
  my $store = $config->localDbsLoc."/sifts";
  if(-e "$statusdir/mirroredSifts"){
    $logger->info("Already mirrored sifts\n");
  }else{
    mirrorSifts( $logger, $destserv.$destpath, $store);
    touch("$statusdir/mirroredSifts");
  }
  
  if(-e "$statusdir/updateSifts"){
    $logger->info("Already updated sifts\n");
  }else{
    parseUpdatedSifts($logger, $dbh, $pfamdb, $store."/ftp.ebi.ac.uk".$destpath, $statusdir, $threads );
    touch("$statusdir/updatedSifts");
  }
}

sub extractAndParse {
  my ($file, $dbh) = @_;
  
  my $sth = $dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc = ?"); #mySQL statement updated for new db schema - I assume this is needed as a check the acc exists?
  my($accs);
  
  my ($fh, $filename) = tempfile();
  my ($output) = $file =~ /(.*\.xml)/;
  gunzip $file => $output or die "gunzip failed: $GunzipError\n";
  my $xp = XML::XPath->new(filename => $output);
  my $chains = $xp->find('/entry/entity[@type="protein"]'); # find all protein chains
  foreach my $chain ($chains->get_nodelist) {
    my $segments = $xp->find('segment[listResidue]', $chain);
    foreach my $segment ($segments->get_nodelist){
      my $residues = $xp->find('listResidue/residue', $segment );
      foreach my $residue ($residues->get_nodelist){
        my $serial = $residue->getAttribute('dbResNum');
        my $rname  = $residue->getAttribute('dbResName');
        my $pdb = $xp->find('crossRefDb[@dbSource="PDB"]', $residue)->shift;
        #Next unless
        my $uni = $xp->find('crossRefDb[@dbSource="UniProt"]', $residue)->shift;
        my $ss  = $xp->find('residueDetail[@property="codeSecondaryStructure"]', $residue)->shift;
        my $obs = $xp->find('residueDetail[@property="Annotation"]', $residue)->shift;
       
        #Split off the insert code if we find one.
        my($resNum, $insertCode);
        if($pdb->getAttribute("dbResNum") =~ /(\d+)([A-Za-z])/){
          $resNum     = $1;
          $insertCode = $2;
        }else{
          $resNum = $pdb->getAttribute("dbResNum");
          $insertCode = '\N';
        }
        #Get the mapping to the auto number.
        if($uni){
          unless($accs->{$uni->getAttribute('dbAccessionId')}){
            $sth->execute($uni->getAttribute('dbAccessionId'));
            my $row = $sth->fetchrow_arrayref;
            $accs->{$row->[0]} = 1; #mySQL query that is updated above due to schema change only gives one row of results, so can no longer have key as pfamseq_acc and value as auto_pfamseq so changed value to 1.
          }
        } 

        #Print out the tab delimited file!
        print $fh
              uc($pdb->getAttribute("dbAccessionId"))."\t". #pdb_id
              $pdb->getAttribute("dbChainId")."\t". #pdb chain
              $serial."\t". #msd serial count
              $pdb->getAttribute("dbResName")."\t". #pdb residue
              $resNum."\t".$insertCode."\t". # pdb res number and insert code
              ( (defined($obs) and $obs->string_value =~ /Not_Observed/) ? "0" : '1' )."\t". #observed
              ( defined($ss) ?  $ss->string_value : '\N' )."\t"; #dssp_code
        if($uni and $accs->{$uni->getAttribute('dbAccessionId')}){
          print $fh $uni->getAttribute('dbAccessionId')."\t". #pfamseq_acc
              #  $accs->{$uni->getAttribute('dbAccessionId')}."\t". #auto_pfamseq  #now hashed out as mySQL query for new db schema no longer contains auto_pfamseq
                $uni->getAttribute('dbResName')."\t". #pfamseq residue
                $uni->getAttribute('dbResNum')."\n"; # resiude number
        }else{
          print $fh '\N'."\t".'\N'."\t".'\N'."\n";  #one fewer column in file due to changed mySQL statement / db schema
        }
      }
    }
  }
  close($fh);
  #Remove the xml files (unzipped) 
  unlink($output);
  
  my $resSth = $dbh->prepare("INSERT INTO pdb_residue_data (pdb_id, 
                                                            chain, 
                                                            serial, 
                                                            pdb_res, 
                                                            pdb_seq_number, 
                                                            pdb_insert_code, 
                                                            observed, 
                                                            dssp_code, 
                                                            pfamseq_acc, 
                                                            pfamseq_res, 
                                                            pfamseq_seq_number) 
                                       VALUES (?,?,?,?,?,?,?,?,?,?,?)" ); #mySQL statement updated to remove auto_pfamseq field and corresponsing ? due to db schema changes
  _loadTable($dbh, $filename, $resSth, 11); #changed as only 11 columns populated with new mySQL statement / new db schema
  
}
