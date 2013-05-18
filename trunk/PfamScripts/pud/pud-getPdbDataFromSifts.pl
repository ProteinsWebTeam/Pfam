#!/usr/bin/env perl

use strict;
use warnings;
use LWP::Simple qw( $ua );
use Net::FTP;
use XML::XPath;
use XML::XPath::XMLParser;
use IO::Uncompress::Gunzip qw(gunzip $GunzipError) ;
use Net::SCP;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my($statusdir);
&GetOptions( "statusdir=s"   => \$statusdir ) or die "Invalid option!\n";




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

#SCP
my $scp = Net::SCP->new( { "host"=> $config->pfamliveAdmin->{host} } );

#---------------------------------------------------------------------------------
#Main

my($pdbs);
if(-e "$statusdir/got_pdb_info"){
  $logger->info("Already got the PDB entry information");
  my $pdb_st = $dbh->prepare("select pdb_id from pdb");
  $pdb_st->execute;
  %{$pdbs} = map {$_->[0] => 1 }  @{$pdb_st->fetchall_arrayref}; 
}else{
  $pdbs = getPdbInfo($logger, $config, $dbh, $scp);
  system("touch $statusdir/got_pdb_info");
}

$logger->info("Going to get mapping, $pdbs");
if(-e "$statusdir/got_pdb_mapping"){
  $logger->info("Already got the PDB mapping information");
}else{
  $logger->info("Getting UniProt mapping");
  getUniProtMapping($logger, $config, $scp, $pdbs, $dbh, $statusdir );
  system("touch $statusdir/got_pdb_mapping");
}



#---------------------------------------------------------------------------------

sub getPdbInfo {
  my($logger, $config, $dbh, $scp) = @_;
  
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
    my @top1000 = splice(@pdbIds, 0, 1000);
    foreach (@top1000){
      $currentPdbs{$_}++;
    }
    my $list = join(",", @top1000);
    my $request2 = HTTP::Request->new( GET => 'http://www.rcsb.org/pdb/rest/customReport?pdbids='.$list.
                                            '&customReportColumns=classification,structureTitle,releaseDate,'.
                                            'resolution,experimentalTechnique,structureAuthor&service=wsdisplay&format=xml');
    my $response2 = $ua->request($request2);
    if($response2->is_success){
      #split on the br, and skip the first "header" line
      #print STDERR $response2->content;
      my $xp = XML::XPath->new(xml => $response2->content);
      my $records = $xp->find('/dataset/record');
      #my @data = split(/<br \/>/, $response2->content);
      #Skip first element as it has the header!
      #for(my $i=1; $i<=$#data; $i++){
      #  $data[$i] =~ s/""/"\\N"/g; #Change "" for nulls.
      #  print PDB $data[$i]."\n";
      #}
      my $seen;
      foreach my $record ($records->get_nodelist){
        my $thisPdb = $xp->find('structureId', $record)->shift->string_value;
        next if($seen->{$thisPdb});
        $seen->{$thisPdb}++;
        print PDB "$thisPdb\t";
        foreach my $element (qw(classification structureTitle releaseDate resolution experimentalTechnique structureAuthor)){
          my $d = $xp->find($element, $record)->shift;
          print PDB (defined($d) ? ( $d->string_value eq 'null' ? '\N' : $d->string_value ) : '\N');
          if($element eq 'structureAuthor'){
            print PDB "\n";
          }else{
            print PDB "\t";
          }
        }
      }
    }else{
      $logger->logdie("Failed to get info for $list:".$response2->status_line);
    }
  } 
  close(PDB);
  $logger->info("Got titles etc. for PDB IDs.");
    
  #Copy the file across and upload
  scpFile($logger, $scp, $localdbs, "pdb.dat");
  foreach my $table (qw(pdb_residue_data pdb_author pdb_image pdb_pfamA_reg pdb_pfamB_reg pdb)){
    $dbh->do("truncate $table")
    or $logger->logdie("Failed to delete contents from $table:".$dbh->errstr);
  }
  $logger->info("Deleted content from PDB tables");

  
  $logger->info("Loading PDB table information.");
  $dbh->do('load data infile "/tmp/pdb.dat" into table pdb;') #FIELDS TERMINATED BY \',\' ENCLOSED BY \'"\' LINES TERMINATED BY \'\n\';')
    or $logger->logdie("Failed to upload pdb file:".$dbh->errstr );
  return (\%currentPdbs);
}

sub scpFile {
  my($logger, $scp, $path, $file) = @_;
  
  #Copy the file to tmp file
  $scp->put($path."/".$file, "/tmp/".$file) 
    or $logger->logdie("Could not scp $file to  database host:".$scp->{errstr});
}

sub getPfamseqAccs {
  my($logger, $dbh) = @_;
  
  #In teory could have prepared this earlier! However, the script has checkpoint files
  #so can recover.
  my $st = $dbh->prepare("select auto_pfamseq, pfamseq_acc from pfamseq");
  $st->execute() or $logger->logdie("Couldn't select pfamseq_acc from pfamseq table ".$st->errstr."\n");
  my $data = $st->fetchall_arrayref;
  my %pfamseq;
  foreach (@$data){
    $pfamseq{$_->[1]} = $_->[0];
  }

  return \%pfamseq;
}

sub getUniProtMapping {
  my ($logger, $config, $scp, $pdbs, $dbh, $statusdir ) = @_;
   $logger->debug("In getUniProtMapping");
   my $destserv='ftp.ebi.ac.uk'; 
  my $destpath = '/pub/databases/msd/sifts/xml-split/';
  my $store = $config->localDbsLoc."/PDB";

  my $ftp = Net::FTP->new($destserv) or $logger->logdie("error connecting");
  $ftp->login();
  $ftp->binary();
  my %bits;
 

  my $st = $dbh->prepare("select pfamseq_acc, auto_pfamseq from pfamseq where pfamseq_acc = ?");
  
  open(my $fh, ">>$store/pdb_residue_data.dat") 
    or die "Could not open pdb_residue_data.dat\n";
      
   $logger->debug("Starting ftp");
  foreach my $dir ( $ftp->ls($destpath) ) {
    next unless $dir =~ m/.*\/(\S{2})$/;
    #next unless $dir =~ m/.*\/(ab)$/;
    my $bit = $1;
    $logger->info("Working on $bit");
    next if(-e "$statusdir/pdb_mapping_$bit");
    open(my $fh2, ">$store/$bit") or $logger->logdie("Could not open $store/$bit for writing:$!");
    foreach my $pdb ( $ftp->ls($dir) ){
      my $id = uc(substr($pdb,38,4));
      $logger->info("Working on $id");
      next unless(exists($pdbs->{$id}));
      $logger->info("Working on $id");
      if( $pdb =~ m/.*\/(\S{4}\.xml\.gz)$/){
        $ftp->get($pdb, $store."/$1");
        $logger->debug("Extracting $1");
        extractAndParse($store."/$1", $fh2, $st);
      }
    }
    close($fh2);
    open(F, "$store/$bit") or $logger->logdie("Could not open $store/$bit:$!");
    while(<F>){
      print $fh $_;
    }
    close($fh2);
    unlink("$store/$bit");
    system("touch $statusdir/pdb_mapping_$bit");
  }
  close($fh);
  scpFile($logger, $scp, $store, 'pdb_residue_data.dat');
  $dbh->do('load data infile "/tmp/pdb_residue_data.dat" into table pdb_residue_data')
    or $logger->logdie("Failed to upload pdb file:".$dbh->errstr );
}

sub extractAndParse {
  my ($file, $fh, $st) = @_;
  my($accs);
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
        if($pdb->getAttribute("dbResNum") =~ /(\d+)([A-Z])/){
          $resNum     = $1;
          $insertCode = $2;
        }else{
          $resNum = $pdb->getAttribute("dbResNum");
          $insertCode = '\N';
        }
        if($uni){
          unless($accs->{$uni->getAttribute('dbAccessionId')}){
            $st->execute($uni->getAttribute('dbAccessionId'));
            my $row = $st->fetchrow_arrayref;
            $accs->{$row->[0]} = $row->[1];
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
                $accs->{$uni->getAttribute('dbAccessionId')}."\t". #auto_pfamseq  
                $uni->getAttribute('dbResName')."\t". #pfamseq residue
                $uni->getAttribute('dbResNum')."\n"; # resiude number
        }else{
          print $fh '\N'."\t".'\N'."\t".'\N'."\t".'\N'."\n";
        }
      }
    }
  }
  #Remove the xml files (zipped and unzipped) 
  unlink($output);
  unlink($file);
}
