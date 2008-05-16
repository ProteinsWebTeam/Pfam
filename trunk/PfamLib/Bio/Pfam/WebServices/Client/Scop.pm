=head1 NAME

Bio::Pfam::::WebServices::Client::Scop

=head1 SYNOPSIS

    use Bio::Pfam::Webservices::Client::Scop;

    $scop = new Bio::Pfam::Webservices:Client::Scop;


=head1 DESCRIPTION

Some description goes in here.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...

package Bio::Pfam::WebServices::Client::Scop;
use vars qw($AUTOLOAD @ISA);

use strict;
use warnings;
use SOAP::Lite;
use XML::LibXML;
use XML::LibXML::XPathContext;
use Bio::Pfam::Root;
use Bio::Pfam::WebServices::Client;
@ISA = qw(Bio::Pfam::Root Bio::Pfam::WebServices::Client);


sub new {
  my ($class, %params) = @_;

  my $self = bless {}, ref($class) || $class;

  my $proxy    = $params{'-proxy'};
  my $pdbId    = $params{'-pdbId'};

 #Quick assess, miss out get/sets
  eval{
    $self->{'proxy'} = $proxy;
    $self->{'pdbId'} = $pdbId;
  };
  # set a default proxy, if it's not specified
  $self->{'proxy'} ||= "http://wwwcache.sanger.ac.uk:3128/";

  return $self;
}




sub queryService {
  my $self = shift;
  my $soap = SOAP::Lite
    -> uri('/SCOP_XML')
    #-> outputxml(1)
      -> proxy('http://131.111.89.223:4242/soap.cgi',
			   proxy => ['http' => $self->proxy ]);

  my $result = $soap->domains('pdbid' => [$self->pdbId ], 'fields' => ['sid', 'family.name']);

  die "No result from SCOP SOAP query" unless $result;

  if( $result->result ){

    my $parser = XML::LibXML->new();

    #This is some gnarly code to get round the incorrect response string....
    #rdf 22/08/2006
    my @results = split(/\>\n/, $result->result);
    my $resultsString = join(">\n", @results[2..$#results]);
    $resultsString .= ">";

    eval{
      $parser->parse_string($resultsString);
    };
    if( $@ ) {
	  die "Couldn't parse SCOP SOAP response: $@; raw response: ", $result->result;
	}

	$self->_response($parser->parse_string($resultsString));

  } else {

    my $fcode = $result->faultcode;
    my $fstr  = $result->faultstring;
	die "Received a SOAP fault from SCOP SOAP service: $fcode, $fstr";

  }
}


sub pdbId {
  my ($self,$pdbId) = @_;
  # set a pdbId, if defined
  $self->{pdbId} = $pdbId if defined $pdbId;
  return $self->{pdbId};
}

# sub convertResultsToAnnotatedSeq{
#   my $self = shift;
#   my $rdbName = shift;

#   my $rdb = Bio::Pfam::which_select_rdb($rdbname);
#   die "Did not get a connection to the database\n" unless ($rdb);

#   #Step 1 Get a sequence for each chain.
#   my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
#   my @seqs = $rdb->query("select
#   my $annSeq = $fac->createAnnotatedSequence();


#   my $ns = "http://scop.mrc-lmb.cam.ac.uk/test1/";
#   my $xc = XML::LibXML::XPathContext->new($self->_response);
#   $xc->registerNs( "scop" => $ns );

#   foreach my $pdbIdNode ($xc->findnodes("scop:document/scop:list-of-scop-domains/scop:pdbEntry")){
#     if($pdbIdNode->hasAttribute("pdbid")){
#       my $pdbId = $pdbIdNode->getAttribute("pdbid");
#       $xc = XML::LibXML::XPathContext->new($pdbIdNode);
#       $xc->registerNs( "scop" => $ns );
#       foreach my $domainNode ($xc->findnodes("scop:domain")){
# 	print "Got Domain node\n";
# 	$xc = XML::LibXML::XPathContext->new($domainNode);
# 	$xc->registerNs( "scop" => $ns );
# 	my $nameNode = $xc->findnodes("scop:attribute/scop:node")->shift;
# 	print $nameNode->getAttribute("sunid")."\n";
# 	print $nameNode->textContent."\n";
#       }
#     }
#   }
# }








# if ($sdb eq 'SCOP'){
# 			#get structural domains by querying the SCOP domain SOAP server
# 			my $tmp_chain;
# 			if (!$chain){
# 				$tmp_chain = "_";
# 				}
# 			else{
# 				$tmp_chain = $chain;
# 				}
# 			my $tmp_pdb = uc $pdb;
# 			my $read = 0;
# 			my $get_region =0;
# 			my ($sunid, $domain_name);
# 			#keys will be sunids, valuses will be start~end or 0; 
# 			my %scop_domains;
# 			my $soap = SOAP::Lite                                             
# 			         -> uri('/SCOP_XML')
# 			          -> proxy('http://131.111.89.223:4242/soap.cgi',
# 				  proxy => ['http' =>'http://wwwcache.sanger.ac.uk:3128/']	
# 				  );
# 			my $result = $soap -> domains('pdbid' => [$pdb],
# 										  'fields' => ['sid', 'family.name']);
# 			unless ($result->fault)
# 				{
# 			 	foreach(split /\n/, $result->result()){
# 					if(/<pdbEntry pdbid=\"$tmp_pdb\">/){
# 					$read++;
# 					}
# 				elsif($read && (/<domain sunid=\"(\d+)\"/)){
# 					$sunid = $1;
# 					}
# 				elsif($read && (/<region chain=\"$tmp_chain\"/)){
# 					my $line = $_;
# 					if($line =~ /start=\"(\d+)\" stop=\"(\d+)\"/){
# 						$scop_domains{$sunid}{'region'}= "$1~$2";
# 						$scop_domains{$sunid}{'name'}= $domain_name;
# 						}
# 					else{
# 						$scop_domains{$sunid}{'region'} = 0;
# 						$scop_domains{$sunid}{'name'}= $domain_name;
# 						$get_region++;
# 						}
# 					}
# 				elsif($read && (/<node sunid=\"\d+\">(.*)<\/node>/)){
# 					$domain_name = $1;
# 					}
# 				elsif($read && (/<\/domain>/)){
# 					$sunid=0;
# 					$domain_name=0;
# 					}
# 				}

# 				}	
# 			else
# 				{
# 				 my $fcode = $result->faultcode;
# 				 my $fstr  = $result->faultstring;
#  				print STDERR "FAULT: $fcode, $fstr\n";
# 				}
	
# 		#get disordered structural regions
# 		my $st = "select distinct pfamseq_seq_number, pdb_seq_number from msd_data join pdb, pfamseq where pdb.auto_pdb=msd_data.auto_pdb and pfamseq.auto_pfamseq=msd_data.auto_pfamseq and pdb_id=\'$pdb\' and chain=\'$chain\' and pfamseq_id=\'$in_id\'"; 
# 		my $ordered_region = $dbh->prepare($st);
# 		$ordered_region->execute();
# 		#fetchrow
# 		my @ordered;
# 		while( (my @ary = $ordered_region->fetchrow) ){
# 			push(@ordered, \@ary);
# 			}
# 		$ordered_region->finish;
# 		#look for any discontinous regions of structure (continuous region is a an array of 0s).

# 		my @disordered;
# 		for(my $n = 0; $n < $the_length; $n++){
# 			$disordered[$n]=1;
# 			}
# 		foreach(sort{$$a[0] <=> $$b[0]} @ordered){
# 			$disordered[$$_[0] - 1]=0;
# 			}
			
# 		my ($ds, $de);
# 		for(my $m = 0; $m < $the_length; $m++){
# 			if(($ds and $de) && ($disordered[$m] == 1)){	
# 				$de=$m + 1;
# 				}
# 			elsif(($ds and $de) && ($disordered[$m] == 0)){
# 				#new disorder region, add to the annotated sequence object.
# 				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 																		 '-FROM' => $ds,
# 																		 '-TO' => $de,
# 																		 '-TYPE' => "disordered",
# 																		 '-SOURCE' => "structure"
# 																		 )); 
# 				$de = $ds = 0;
# 				}
# 			elsif($disordered[$m] == 1){
# 				#start new disordered region
# 				$ds=$de=($m + 1); 
# 				}
# 			}
			
# 			if($de and $ds){
# 				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 																		 '-FROM' => $ds,
# 																		 '-TO' => $de,
# 																		 '-TYPE' => "disordered",
# 																		 '-SOURCE' => "structure"
# 																		 )); 
# 				}
		
# 		#now add the scop domains to the annotated sequnce object.
# 		if($get_region){
# 			#get the ordered region, scop domain is the whole chain
# 			@ordered = sort{$$a[0] <=> $$b[0]} @ordered;

# 			foreach my $id (keys %scop_domains){
# 				my $name  = $scop_domains{$id}{'name'} if ($scop_domains{$id}{'name'});	
# 				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 																	 	 '-FROM' => ${$ordered[0]}[0],
# 																         '-TO' => ${$ordered[$#ordered]}[0],
# 																	 	 '-TYPE' => "SCOP:$name",
# 																	     '-SOURCE' => "$id"
# 																		 )); 
# 				}
# 			}
# 		else{
# 			#use the scop domain  boundaries, need to be converted from pdb to sequence coos
# 			foreach my $id (keys %scop_domains){
# 				#get start/end seqpos
# 				my ($start, $end);
# 				my @se = split /~/, $scop_domains{$id}{'region'};
# 				foreach(@ordered){
# 					if($se[0] eq $$_[1]){
# 						$start=$$_[0]
# 						}
# 					if($se[1] eq $$_[1]){
# 						$end=$$_[0]
# 						}
# 					}
# 				my $name = $scop_domains{$id}{'name'};	
# 				#having got the sequence start end add to the structure
# 				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 																	 	 '-FROM' => $start,
# 																         '-TO' => $end,
# 																	 	 '-TYPE' => "SCOP:$name",
# 																	     '-SOURCE' => "$id"
# 																		 )); 

# 				}
# 			}
		
# 			}
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
