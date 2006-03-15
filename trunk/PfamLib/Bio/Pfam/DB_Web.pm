
#
# BioPerl module for Bio::Pfam::DB_Web
#
# Cared for by Mhairi Marshall <mm1@sanger.ac.uk>
#
# Copyright Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::DB_Web - The Web version of Pfam::DB

=head1 SYNOPSIS    A pfamseq identifier (ref to a list of)
    A list of region types needed
 Notes   : 
    1. The AnnotatedSequence returned has no enclosed sequence,
    just annotation. If the sequence is required, use 
    get_Seq_pfamseq to get a Bio::Pfam::SeqPfam and insert it into
    the AnnotatedSequence method using 
    AnnotatedSequence->sequence().

    2. Region-type items can be:
        'seed'
        'full'
        'pfamb'
        'other'
    When the list is empty, all regions are returned. If not, then 
    if the specified region types are availalble they will be 
    returned, but the types specified will not be returned


This is a concrete implementation of the abstract database object
found in Bio::Pfam::DB

=head1 DESCRIPTION

Describe the object here

=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...

##########
# use libs in modules are bad but we have to do this to add PFAM_MODULES_DIR 
# to @INC after BIOPERL_DIR, and so inherit from the correct Root object

# hopefully don't need this anymore
#BEGIN {
#    $pfam_mod_dir = 
#        (defined $ENV{'PFAM_MODULES_DIR'})
#            ?$ENV{'PFAM_MODULES_DIR'}:"/nfs/disk100/pubseq/Pfam/scripts/Modules";
#    $bioperl_dir =
#        (defined $ENV{'BIOPERL_DIR'})
#            ?$ENV{'BIOPERL_DIR'}:"/nfs/disk100/pubseq/Pfam/bioperl";
#}

#use lib $bioperl_dir;
#use lib $pfam_mod_dir;
#
##########
BEGIN {
    # SOAP isn't always available
    # if it isn't then we don't want to bomb
     eval "use SOAP::Lite"; 
}

package Bio::Pfam::DB_Web;

use vars qw(@ISA);
use strict;

use Bio::Pfam::SeqPfam;
use FileHandle;

use Bio::Pfam::DB;
use Bio::Pfam::EntryA_Web;
use Bio::Pfam::EntryB_Impl;
use Bio::Pfam::PfamAnnSeqFactory;

use Bio::Pfam::HMMOtherRegion;
use Bio::Pfam::ContextPfamRegion;

use DBI;
use LWP;
use Bio::Pfam::PfamRDB;
use Bio::Pfam::Stockholm;
use Bio::SeqIO;
#require '/nfs/WWWdev/TEST_docs/cgi-bin/Pfam/Bio/Pfam/Stockholm.pm';
@ISA = qw(Bio::Pfam::DB);

my $logger;
BEGIN {
    # SOAP isn't always available
    # if it isn't then we don't want to bomb
    if($ENV{'HTTP_X_FORWARDED_HOST'}){
	require XML::LibXML;
	require SOAP::Lite;
    }
    eval "use Log::Log4perl 'get_logger'";
    if ($@)
    {
        warn "Log::Log4perl not found. DB_Web WILL break...\n";
    }
    else
    {
        $logger = get_logger(__PACKAGE__);
    }
}


sub new {
  my($class, %params) = @_;
  my $self = $class->SUPER::new(%params);

  my( $datadir, $srspfamseq, $srspfam, $srspfamb, $srsswisspfam, $getz, $nordb) = 
      (($params{'-datadir'} || $params{'-DATADIR'}),
       ($params{'-srsdb'}||$params{'-SRSDB'}),
       ($params{'-srspfam'}||$params{'-SRSPFAM'}) ,
       ($params{'-srspfamb'} || $params{'-SRSPFAMB'}),
       ($params{'-srsswisspfam'} || $params{'-SRSSWISSPFAM'}),
       ($params{'-getz'} || $params{'-GETZ'}),
       ($params{'-no_rdb'} || $params{'-NO_RDB'})
       );

  ## Add parameters for the web site !
  $self->_the_RDB( Bio::Pfam::PfamRDB->new( %params ));

  if( ! -d $datadir ) {
      $self->throw("$datadir is not a directory for web pfam access");
  }
  
  $self->{'dbweb_srspfamseq'} = $srspfamseq;
  $self->{'dbweb_srspfam'} = $srspfam;
  $self->{'dbweb_srspfamb'} = $srspfamb;
  $self->{'dbweb_srsswisspfam'} = $srsswisspfam;
  $self->{'dbweb_getz'} = $getz;
  $self->{'dbweb_datadir'} = $datadir;
  $self->{'no_rdb'} = $nordb;

  return $self;
}



=head2 id2acc

 Title   : id2acc
 Usage   : $acc = $db->id2acc("pkinase"); 
 Function: converts an id to an accession. Only works for
	   current ids - does not work for previous ids
 Returns : string of the accession, or undef if no accession
 Throws  : Exceptions thrown if this $id does not exist or due to RCS misconfiguration
 Args    :

=cut

sub id2acc{
   my ($self,$id) = @_;
   my ($dbh, $st, $acc, $len);

   eval {
       if (not $self->{'no_rdb'}) {
	   $dbh = $self->_the_RDB->connect();
	   
	   if(defined($dbh)) {
	       $st = $dbh->prepare("select pfamA_acc, model_length from pfamA where pfamA_id = '$id'");
	       $st->execute();
	       ($acc, $len) = $st->fetchrow;
	       $st->finish();
	       $self->_the_RDB->disconnect;
	   }
       }
   };
   
   if ($self->{'no_rdb'} or $@) {
       $acc =  $self->_file_id2acc($id);
   } 
   else {
       if (not defined $acc) {
	   $self->throw("DB_RDB::id2acc - $id is not a valid id for this pfam database");
       }
       elsif (not defined $len) {
	   $self->throw("DB_RDB::id2acc - $id is the id of a dead family");
       }
       
   }  
   return $acc;
}


=head2 _file_id2acc

 Title   : _file_id2acc
 Usage   : $acc = $db->id2acc("pkinase"); 
 Function: converts an id to an accession. Only works for
	   current ids - does not work for previous ids
 Returns : string of the accession, or undef if no accession
 Throws  : Exceptions thrown if this $id does not exist or due to RCS misconfiguration
 Args    :
 Notes   : 

=cut

sub _file_id2acc{
   my ($self,$id) = @_;
   my ($srspfam, $fh, $acc, $temp);

   my $srsdb = $self->{'dbweb_srspfam'};
   $fh = new FileHandle;

   my $getz = $self->{'dbweb_getz'};

   if( $fh->open("$getz -f 'acc' '[$srsdb-id:$id]' |") == 0) {
       $self->throw("Cannot fork getz process off");
   }
   $temp = <$fh>;

   if( $temp =~ /^AC\s+(\S+)/ ) {
       $acc = $1;
   }
   else {
       $self->throw("No Pfam family with that id $id");
   }
   if( $fh->close() == 0 ) {
       $self->throw("Getz pipe failed to close");
   }

   return $acc;
}


sub pfamB_acc_to_id {
   my ($self,$acc) = @_;
   my ($dbh, $st, $id);
   eval {
       if (not $self->{'no_rdb'}) {
	   $dbh = $self->_the_RDB->connect();

	   if(defined($dbh)) {
	       $dbh = $self->_the_RDB->connect();
	       $st = $dbh->prepare("select pfamB_id from pfamB where pfamB_acc = '$acc'");
	       $st->execute;
	       ($id) = $st->fetchrow;
	       $st->finish;
	       $self->_the_RDB->disconnect;
	   }
       }
   };

   if($self->{'no_rdb'} or $@) {
   #    $id =  $self->_file_acc2id($acc);
   }
   else {
       if (not defined $id) {
	   $self->throw("DB_RDB::acc2id - $acc is not a valid id for this pfam database");
       }

   }
 

   return $id;


}


=head2 acc2id

 Title   : acc2id
 Usage   : $id = $db->acc2id("PF00001");
 Function: converts an accession to an id
 Returns : the id, throws an exception if no accession, or dead accession
 Args    :

=cut

sub acc2id{
   my ($self,$acc) = @_;
   my ($dbh, $st, $id, $len);
   
   eval {
       if (not $self->{'no_rdb'}) {
	   $dbh = $self->_the_RDB->connect();
	   
	   if(defined($dbh)) {
	       $dbh = $self->_the_RDB->connect();
	       $st = $dbh->prepare("select pfamA_id, model_length from pfamA where pfamA_acc = '$acc'");
	       $st->execute;
	       ($id, $len) = $st->fetchrow;
	       $st->finish;
	       $self->_the_RDB->disconnect;
	   }
       }
   };
   
   if($self->{'no_rdb'} or $@) {
       $id =  $self->_file_acc2id($acc);
   }
   else {
       if (not defined $id) {
	   $self->throw("DB_RDB::acc2id - $acc is not a valid id for this pfam database");
       }
       elsif (not defined $len) {
	   $self->throw("DB_RDB::acc2id - $acc is the id of a dead family");
       }
       
   }
 
   return $id;
}


=head2 _file_acc2id

 Title   : _file_acc2id
 Usage   : $id = $db->acc2id("PF00001");
 Function: converts an accession to an id
 Returns : the id, or undef if no id for that accession
 Args    :
 Notes   : 

=cut

sub _file_acc2id{
   my ($self,$acc) = @_;
   my ($srspfam, $fh, $id, $temp);

   $srspfam = $self->{'dbweb_srspfam'};

  my $getz = $self->{'dbweb_getz'};

   $fh = new FileHandle;

   if( $fh->open("$getz -f 'id' '[$srspfam-acc:$acc]' |") == 0) {
       $self->throw("Cannot fork getz process off");
   }
   $temp = <$fh>;

   if( $temp =~ /^ID\s+(\S+)/ ) {
       $id = $1;
   }
   else {
       $self->throw("No Pfam family with accession $acc");
   }
   if( $fh->close() == 0 ) {
       $self->throw("Getz pipe failed to close");
   }

   return $id;


}


=head2 get_allacc

 Title   : get_allacc
 Usage   : @list = $db->getallacc('ALPHA');
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_allacc{
   my ($self,$type) = @_;
   my ($dbh, $st, $acc, $id, @acclist, %map);

   eval {
       if (not $self->{'no_rdb'}) {
	   $dbh = $self->_the_RDB->connect();
	   $dbh->prepare("select pfamA_acc, pfamA_id from pfamA where model_length is not null");
	   $st = $dbh->execute();
	   while ( ($acc, $id) = $st->fetchrow) {
	       $map{ $acc } = $id;
	   }
	   $st->finish;
	   $self->_the_RDB->disconnect;
       }
   };
   
   if ($self->{'no_rdb'} or $@) {
       @acclist = $self->_file_get_allacc($type);
   }
   else {  
       if ($type =~ /ALPHA/) {
	   @acclist = sort { lc($map{$a}) cmp lc($map{$b}) } keys %map;
       }
       elsif ($type =~ /DATE/) {
	   @acclist = sort {my ($aa,$bb); $a =~ /^PF+0+(\d+)$/; $aa = $1; $b =~ /^PF+0+(\d+)$/; $bb = $1; return $aa <=> $bb; } keys %map;
       }
       else {
	   $self->throw("Bio::Pfam::DB_RDB - get_allaccs - cannot sort into $type order");
       }
   }

   return @acclist;
}



=head2 _file_get_allacc

 Title   : _file_get_allacc
 Usage   : @list = $db->getallacc('ALPHA');
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.
=cut

sub _file_get_allacc{
    my ($self, @args) = @_;
    my ($dir,@files,@out,$acc);

    $dir = $self->{'dbweb_datadir'};
    opendir(_DIR,"$dir/desc") || $self->throw("Could not open data directory $dir/desc $!");
    @files = readdir(_DIR);
    closedir(_DIR);

    foreach $acc ( @files ) {
	$acc =~ /^PF/ || next;
	$acc =~ s/\..*$//g;
	push(@out,$acc);
    }

    return @out;

}





=head2 get_AnnotSeqs

 Title   : get_AnnotSeqs
 Usage   : ($annot) = $db->get_annotSeqs( $id );
 Function: gets an Annotated sequence from underlying swisspfam db
 Returns : Bio::Pfam::AnnotatedSequence 
 Args    : A ref to a list of swisspfam (pfamseq) identifier
 Notes   : 
    1. The AnnotatedSequence returned has no enclosed sequence,
    just annotation. If the sequence is required, use 
    get_Seq_pfamseq to get a Bio::Pfam::SeqPfam and insert it into
    the AnnotatedSequence method using 
    AnnotatedSequence->sequence().

    2. Region-type items can be:
        'seed'
        'full'
        'pfamb'
        'other'
    When the list is empty, all regions are returned. If not, then 
    if the specified region types are availalble they will be 
    returned, but the types specified will not be returned

  3. This sub acesses the RDB, if it cant get a connection then it gets the sequences from the flat file version.

=cut


sub get_AnnotSeqs {
   my ($self, $id_list, $type_list) = @_;
   my ($dbh, $st_pfamA_reg_seed, $st_pfamA_reg_full,  $st_pfamB_reg, $st_other_reg,$st_hmm_other_region,$st_context_region, $st_length, $length,
       $seq_id, $st, $en, $score, $mo_st, $mo_en, $bits, $ev, $acc, $id, $desc, $orien,
       $annot, $sou, $all_regs, $mod_len, $tree_order,
       @annseqlist,$domain_score,
       %temp);

 
   my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
   eval {
       $dbh = $self->_the_RDB->connect();
   };
   if ($@ or not defined( $dbh ) or $self->{'no_rdb'}) { 
       ## Connection to the database failed - get pfama & pfamb from the flatfile
       my @type_list = ("full", "pfamb");
       
       @annseqlist =  $self->_file_get_AnnotSeqs( $id_list, \@type_list);
       
   }
   else {
       if (not $type_list) {
	   $all_regs = "true";	 
       }
       else {
	   foreach my $info_type (@{$type_list}) {
	       if ($info_type =~ /seed/i) {
		   $temp{ "seed" } = 1;
	       }
	       elsif ($info_type =~ /full/i) {
		   $temp{ "full" } = 1;
	       }
	       elsif ($info_type =~ /pfamb/i) {
		   $temp{ "pfamb" } = 1;
	       }
	       elsif ($info_type =~ /other/i) {
		   $temp{ "other" } = 1;
		       
	       }
	       else {
		   $self->throw("Bio::Pfam::DB_RDB->get_AnnotSeqs does not supprto region type $info_type");
	       }
	   }
       }
       
       
       if ($temp{"full"} or $all_regs) {

#	   my $stat = "select seq_start, seq_end, model_start, model_end, ";
#	   $stat .= "bits_score, evalue_score, pfamA_reg_full.pfamA_acc, pfamA_id, description ";
#	   $stat .= "from pfamA_reg_full natural left join pfamA  where pfamseq_id = ?";
	
	  my $stat = "select seq_start, seq_end, model_start, model_end, ";
	  $stat .= "domain_bits_score, domain_evalue_score, pfamA.pfamA_acc, pfamA_id, pfamA.description , pfamA_reg_full.tree_order ";
	  $stat .= "from pfamseq, pfamA, pfamA_reg_full ";
	  $stat .= "where pfamseq.pfamseq_id =  ? and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq and pfamA_reg_full.auto_pfamA = pfamA.auto_pfamA  and in_full = '1'";
	  ##and significant = '1'  #### BUG FIX !! 


	   $st_pfamA_reg_full = $dbh->prepare($stat);
	   
	   
	   my $q = "select length ";     
	   $q .= "from pfamseq  where pfamseq_id = ?";
	   
	   
	   $st_length = $dbh->prepare($q);
       }
	   
       ### STILL TO BE FIXED
       if ($temp{"seed"}) {
#	   my $stat = "select seq_start, seq_end, ";
#	   $stat .= "pfamA_reg_seed.pfamA_acc, pfamA_id, description ";
#	   $stat .= "from pfamA_reg_seed natural left join pfamA where pfamseq_id = ?";
	   
	 my $stat = "select seq_start, seq_end, ";
           $stat .= "pfamA.pfamA_acc, pfamA_id, pfamA.description ";
           $stat .= "from pfamA_reg_seed, pfamA, pfamseq  where pfamseq.pfamseq_id  = ? and pfamseq.auto_pfamseq = pfamA_reg_seed.auto_pfamseq and pfamA_reg_seed.auto_pfamA = pfamA.auto_pfamA";



	   $st_pfamA_reg_seed = $dbh->prepare($stat);
       }

        ### STILL TO BE FIXED
       if ($temp{"pfamb"} or $all_regs) {
#	   my $stat = "select seq_start, seq_end, pfamB_reg.pfamB_acc, pfamB_id ";
#	   $stat .= "from pfamB_reg natural left join pfamB where pfamseq_id = ?";

	  my $stat = "select distinct seq_start, seq_end, pfamB.pfamB_acc, pfamB_id ";
	#  my $stat = "select distinct * ";
	  $stat .= "from pfamB_reg, pfamB, pfamseq ";
	  $stat .= "where pfamseq_id = ? and pfamB_reg.auto_pfamseq = pfamseq.auto_pfamseq and pfamB_reg.auto_pfamB = pfamB.auto_pfamB;";

	


	   $st_pfamB_reg = $dbh->prepare($stat);
       }

        ### STILL TO BE FIXED
       if ($temp{"other"} or $all_regs) {

#	   my $stat = "select seq_start, seq_end, type_id, source_id, score, orientation ";
#	   $stat .= "from other_reg, pfamseq ";
#	   $stat .= "where pfamseq_id = ? and other_reg.auto_pfamseq = pfamseq.auto_pfamseq";

	   my $stat = "select seq_start, seq_end, type_id, source_id, score, orientation ";
           $stat .= "from other_reg, pfamseq ";
           $stat .= "where pfamseq.pfamseq_id = ? and other_reg.auto_pfamseq = pfamseq.auto_pfamseq";

	   $st_other_reg = $dbh->prepare($stat);
       }


       
       if ($temp{"context"} or $all_regs) {
	 # 
	 my $stat = "select seq_start, seq_end, domain_score, pfamA.pfamA_acc, pfamA_id,  pfamA.description ";
	 $stat .= "from context_pfam_regions, pfamseq, pfamA ";
	 $stat .= "where pfamseq.pfamseq_id = ? and context_pfam_regions.auto_pfamseq = pfamseq.auto_pfamseq ";
	 $stat .= " and pfamA.auto_pfamA = context_pfam_regions.auto_pfamA ";

	 $st_context_region = $dbh->prepare($stat);
	 #	     print "STAT: $stat <P>";
       }
       

       if ($temp{"hmmother_region"} or $all_regs) {
	 #	     print "Content-type: text/html\n\n";
	 #	   my $stat = "select seq_start, seq_end, type_id, source_id, score, orientation ";
	 #	   $stat .= "from other_reg, pfamseq ";
	 #	   $stat .= "where pfamseq_id = ? and other_reg.auto_pfamseq = pfamseq.auto_pfamseq";
	 
	 my $stat = "select seq_start, seq_end, smart_id ";
	 $stat .= "from smart_regions, pfamseq, smart ";
	 $stat .= "where pfamseq.pfamseq_id = ? and smart_regions.auto_pfamseq = pfamseq.auto_pfamseq ";
	 $stat .= " and smart.auto_smart = smart_regions.auto_smart ";
	 $st_hmm_other_region = $dbh->prepare($stat);
	 #$stat = "select seq_start, seq_end, smart_id from smart_regions, pfamseq, smart where pfamseq.pfamseq_id = 'vav_human'  and smart_regions.auto_pfamseq =  pfamseq.auto_pfamseq and smart.auto_smart = smart_regions.auto_smart" ;
	 #  print "STAT: $stat <P>";
       }
       
       foreach my $in_id (@{$id_list}) {
	   my $annSeq = $fac->createAnnotatedSequence();
	   $annSeq->id( $in_id );
	 
	   if (defined $st_pfamA_reg_full) {
	       $st_pfamA_reg_full->execute($in_id);

	       while ( ($st, $en, $mo_st, $mo_en, $bits, $ev, $acc, $id, $desc, $tree_order)
		       = $st_pfamA_reg_full->fetchrow) {

		   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
									   '-PFAM_ID' => $id,
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en,
									   '-MODEL_FROM' => $mo_st,
									   '-MODEL_TO' => $mo_en,
									   '-BITS' => $bits,
									   '-EVALUE' => $ev,
									   '-ANNOTATION' => $desc));
		   
	       }
	       
	       
	       $st_pfamA_reg_full->finish;
	   }
	   
	   
	   
	   if (defined $st_pfamA_reg_seed) {
	       $st_pfamA_reg_seed->execute($in_id);

	       while ( ($st, $en, $acc, $id, $desc)
		       = $st_pfamA_reg_seed->fetchrow) {
	
		   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
									   '-PFAM_ID' => $id,
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en,
									   '-ANNOTATION' => $desc));
	       }
	       $st_pfamA_reg_seed->finish;
	   }
	   
	   if (defined $st_pfamB_reg) {
	       $st_pfamB_reg->execute($in_id);
	       
	       while ( ($st, $en, $acc, $id) = $st_pfamB_reg->fetchrow) {
		   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
									   '-PFAM_ID' => $id,
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en));
	       }
	       $st_pfamB_reg->finish;
	   }
	   
	   
	   ## Get the length and add to the annotated sequence object
	   my($the_length);
	   if (defined $st_length) {
	       $st_length->execute($in_id);
	       while ( ($length) = $st_length->fetchrow) {
		   
		   $the_length = $length if ($length =~ /[0-9]/);
		   
	       }
	       
	       $annSeq->length( $the_length);
	       $st_length->finish;
	       
	   }

	   
	   if (defined $st_other_reg) {
	       $st_other_reg->execute($in_id);
	       while ( ($st, $en, $id, $sou,$score, $orien ) = $st_other_reg->fetchrow) {
		   $annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
									    '-FROM' => $st,
									    '-TO' => $en,
									    '-TYPE' => $id,
									    '-SOURCE' => $sou,
									    '-SCORE' => $score,
									    '-ORIENTATION' => $orien
									    )); 
		   
	       }
	       $st_other_reg->finish;
	   }

	   if (defined $st_context_region) {
	     
	     $st_context_region->execute($in_id);
	     while ( ( $st, $en, $domain_score, $acc, $id,  $desc) = $st_context_region->fetchrow) {
	       
	       
	       $annSeq->addAnnotatedRegion( Bio::Pfam::ContextPfamRegion->new(
									      '-PFAM_ACCESSION' => $acc,
									      '-PFAM_ID' => $id,
									      '-SEQ_ID' => $in_id,
									      '-FROM' => $st,
									      '-TO' => $en,
									      '-ANNOTATION' => $desc,
									      '-DOMAIN_SCORE' => $domain_score
									     )); 
	       
	     }
	     $st_other_reg->finish;
	   }

	   if (defined $st_hmm_other_region) {
	     
	     $st_hmm_other_region->execute($in_id);
	     #seq_start, seq_end, sentence_score, contribution_to_sentence_score, based_pfam_thres, based_prob_thres, pfamA.pfamA_acc, pfamA_id,  pfamA.description
	     while ( ( $st, $en, $id) = $st_hmm_other_region->fetchrow) {
	       

	       
	       $annSeq->addAnnotatedRegion( Bio::Pfam::HMMOtherRegion->new(
									   '-SEQ_ID' => $in_id,
									   '-FROM' => $st,
									   '-TO' => $en,
									   '-DOMAIN' => $id,
									   '-HMM_DB', => "smart"
									  ));
	     }
	     $st_other_reg->finish;
	   }
	   
	   push @annseqlist, $annSeq;
       }
       
       $self->_the_RDB->disconnect;
   }
   
   return @annseqlist;
}




=head2 _file_get_AnnotSeqs

 Title   : _file_get_AnnotSeqs
 Usage   : ($annot1, $annot2) = $db->get_AnnotSeqs( [$id1, $id2], $type1, $type2 );
 Function: gets Annotated sequencea from underlying swisspfam database
 Returns : Bio::Pfam::AnnotatedSequence (list of) 
 Args    : 
    A pfamseq identifier (ref to a list of)
    A list of region types needed
 Notes   : 
    1. The AnnotatedSequence returned has no enclosed sequence,
    just annotation. If the sequence is required, use 
    get_Seq_pfamseq to get a Bio::Pfam::SeqPfam and insert it into
    the AnnotatedSequence method using 
    AnnotatedSequence->sequence().

    2. Region-type items can be:
        'full'
        'pfamb'
    When the list is empty, all regions are returned. If not, then 
    if the specified region types are availalble they will be 
    returned, but the types specified will not be returned

=cut

sub _file_get_AnnotSeqs{
   my ($self, $input_ids, $type_list) = @_;

   my ($fh, $annotseq, $fac, $swisspfamdb, @retlist, $getz);
   $swisspfamdb = $self->{'dbweb_srsswisspfam'};
   $getz = $self->{'dbweb_getz'};

   $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
 
   $annotseq = $fac->createAnnotatedSequence();

   foreach my $in_id (@{$input_ids}) {
     
       $fh = new FileHandle;
       
       if( $fh->open("$getz -e '[$swisspfamdb-id:$in_id]' |") == 0) {
	   $self->throw("Cannot fork getz process off");
       }

       
       $fac->addSwissPfamToAnnSeq( $annotseq, $fh, $type_list);

       if( $fh->close() == 0 ) {
	   $self->throw("Getz pipe failed to close");
       }
     
   }
   push @retlist, $annotseq;
   return @retlist;
}



=head2 get_Seq_pfamseq

 Title   : get_Seq_pfamseq($id)
 Usage   : $seq = $db->get_Seq_pfamseq($id)
 Function: gets a Bio::Pfam::SeqPfam object from the underlying database
 Returns : a newly made Bio::Pfam::SeqPfam object
 Args    :
    1. name of sequence
    2. Whether the name is an accession number or id ('id' or 'acc')
    3. Whether the actual sequence itself is required
 Notes   :
    Arg 3 is ignored in this database implementation, since the sequences themselves
    are not strored in the pfam relational database

=cut

sub get_Seq_pfamseq{
   my ($self,$input_id, $lookup, $seqrequired) = @_;
   my ($dbh, $st, $id, $acc, $desc, $len, $org, $out, $seq);
   #$seqrequired = 1;
   eval {
       if (not $self->{'no_rdb'}) {
	   $dbh = $self->_the_RDB->connect();
	   if(defined($dbh)) {
	       #   if ($lookup =! /acc/i) {
	     #print "LOOKUP: $lookup , IN: $input_id <P>";
	     if ($lookup =~  /acc/i) {
	       $st = "select pfamseq_id, pfamseq_acc, description, length, species, sequence from pfamseq where pfamseq_acc = '$input_id'";
	     }
	     elsif ($lookup =~  /id/i) {
	       $st = "select pfamseq_id, pfamseq_acc, description, length, species, sequence  from pfamseq where pfamseq_id = '$input_id'";
	     } elsif ($lookup =~  /sec/i) {
	        $st = "select pfamseq_id, pfamseq_acc, description, length, species, sequence  from pfamseq, secondary_pfamseq_acc where secondary_acc = '$input_id' and secondary_pfamseq_acc.auto_pfamseq = pfamseq.auto_pfamseq"
	       
	     }
	       my($stat) = $dbh->prepare($st);
	       $stat->execute();
	       ($id, $acc, $desc, $len, $org, $seq) = $stat->fetchrow;
	       $stat->finish;
	       $self->_the_RDB->disconnect;
	   }
       }
   };
   if ($@ or $self->{'no_rdb'}) {
       $out = $self->_file_get_Seq_pfamseq($input_id, $lookup, $seqrequired);   
   }
   else {
       if ($seqrequired) {
	   #my $srspfamseq = $self->{'dbweb_srspfamseq'};
#	   my $fh = new FileHandle;
#   	  my $getz = $self->{'dbweb_getz'};
#	   if( $fh->open("$getz -f seq -sf fasta '[$srspfamseq-$lookup:$input_id]' |") == 0) {
#	       $self->throw("Cannot fork getz process off (DB_Web::get_filehandle_pfamseq)");
#	   }

#	   $seq = "";
#	   my $id = undef;
#	   my $readingseq = 0;
	   
#	   while (<$fh>) {
#	       if ($readingseq) {
#		   /^>/ && $self->throw("Request $id gave multiple sequences in db. Problem!");	   
#		   s/\W//g;
#		   $seq .= $_;
#	       }

#	       elsif ( /^>(\S+)\s/ ) {
#		   $readingseq = 1;
#	       }
#	   }
#	   if( $fh->close() == 0 ) {
#	       $self->throw("Getz pipe failed to close");
#	   }
	return $seq;
       }

       $org =~ s/\.$//; $org = lc $org;
       $desc =~ s/\.$//; $desc = lc $desc;
       
       ($acc and $id and $desc and $org and $len) or
	   $self->throw("DB_RDB::get_Seq_pfamseq - Information missing"); 
       #print "EEP <P>";
       #print "seq: $seq <P>";
      # $seq = "aaaaaaaaaaaaaaaa";
       $out = Bio::Pfam::SeqPfam->new('-seq' => $seq,
				      '-start' => '1',
				      '-end' => $len,
				      '-id'=> $id,
				      '-acc' => $acc,
				      '-organism' => $org,
				      '-desc' => $desc);
   } 

   return $out;
}




=head2 _file_get_Seq_pfamseq

 Title   : _file_get_Seq_pfamseq($id)
 Usage   : $seq = $db->get_Seq_pfamseq($id)
 Function: gets a Bio::Pfam::SeqPfam object from the underlying database
 Returns : a newly made Bio::Pfam::SeqPfam object
 Args    :


=cut

sub _file_get_Seq_pfamseq{
   my ($self,$input_id, $lookup, $seqrequired) = @_;
   my ($temp,$seq,$fh,$out, $srspfamseq, $id, $acc, $readingseq, $desc, $org, $tax, $seqcmd);
   if (not defined $lookup) { 
       $lookup = 'id';
   }
   $seqcmd = ($seqrequired)?"-f 'seq' -sf fasta":"";
   $srspfamseq = $self->{'dbweb_srspfamseq'};

   $fh = new FileHandle;
   	  my $getz = $self->{'dbweb_getz'};
   if( $fh->open("$getz -f 'id' -f 'acc' -f 'des' -f 'org' -f 'tax' $seqcmd '[$srspfamseq-$lookup:$input_id]' |") == 0) {
       $self->throw("Cannot fork getz process off (DB_Web::get_filehandle_pfamseq)");
   }

   $seq = "";
   $desc = "";
   $id = undef;
   $readingseq = 0;

   while (<$fh>) {
       /^\/\// and last;

       if ($readingseq) {
	   /^>/ && $self->throw("Request $id gave multiple sequences in db. Problem!");	   
	   s/\W//g;
	   $seq .= $_;
       }
       elsif (/^AC\s+(\S+);/) {
	   $acc = $1;
       }
       elsif (/^ID\s+(\S+)/) {
	   $id = $1;
       }
       elsif (/^DE\s+(.*)/) {
	   if ($desc) {
	       $desc .= " $1";
	   }
	   else {
	       $desc = $1;
	   }
       }
       elsif (/^OS\s+(.*)/) {
	   if ($org) {
	       $org .= " $1";
	   }
	   else {
	       $org = $1;
	   }
       }
       elsif (/^OC\s+(.*)/) {
	   if ($tax) {
	       $tax .= " $1";
	   }
	   else {
	       $tax = $1;
	   }
       }
       elsif ( /^>(\S+)\s/ ) {
	   $readingseq = 1;
       }
   }

   if( $fh->close() == 0 ) {
       $self->throw("Getz pipe failed to close");
   }

   $org =~ s/\.$//; $org = lc $org;
   $tax = lc $tax;
   $desc =~ s/\.$//; $desc = lc $desc;
   ($acc and $id and $desc and $org and $tax) or
       $self->throw("DB_RCS::get_Seq_pfamseq - Information missing");

   $out = Bio::Pfam::SeqPfam->new('-seq' => $seq,
				  '-start' => '1',
				  '-end' => length($seq),
				  '-id'=>$id,
				  '-acc' => $acc, 
				  '-organism' => $org,
				  '-taxonomy' => $tax,
				  '-desc' => $desc);

   return $out;
}



=head2 get_EntryA_by_id

 Title   : get_EntryA_by_id
 Usage   : $en = $db->get_EntryA_by_id('pkinase');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family


=cut

sub get_EntryA_by_id{
   my ($self,$id) = @_;
   my ($dir,$out,$acc);
   
  
   $acc = $self->id2acc($id);

   $dir = $self->{'dbweb_datadir'};
   open(_DESC,"$dir/desc/$acc.desc") || $self->throw("Could not open [$dir/desc/$acc.desc] as a web site description");
   $out = Bio::Pfam::EntryA_Web->new();
   $out->_datadir($dir);
   $out->acc($acc);
   close(DESC);
   return $out;
}



#=head2 get_EntryB_by_id

# Title   : get_EntryB_by_id
# Usage   : $en = $db->get_EntryB_by_id('Pfam-B_101');
# Function: get a Bio::Pfam::Entry object by id
# Returns : Bio::Pfam::Entry object
# Args    : string of the id of the family


#=cut

#sub get_EntryB_by_id{
#    my ($self,$id) = @_;
#    my ($out, $pfambdb);
    
#    $pfambdb = $self->{'dbweb_srspfamb'};
    
#   my $getz = $self->{'dbweb_getz'};
#    open TEST, "$getz -e '[$pfambdb-id:$id]' |";
#    while (<TEST>) {
#	if (/^AC\s+(\S+)/) {
#	    $out = Bio::Pfam::EntryB_Impl->new();
#	    $out->_datasource("$getz -e '[$pfambdb-id:$id]' |");
#	    $out->id($id);
#	    $out->acc($1);
#	    last;
#	}
#    }
#    close TEST;

#    if (not defined $out) {
#	$self->throw("There is no entry for id $id");
#    }
#    else {
#	return $out;
#    }
#}

sub get_EntryB_by_id{
  my ($self,$id) = @_;
  my ($out, $pfambdb);
  
print "Content_type: text/html\n\n";
#print "Here <P>";
  
#  print " THE ID: $id <BR>";
  #my $Index_File_Name = "/nfs/disk100/pubseq/Pfam/RELEASES/6.4/bio_Pfam-B.index";
 # my $Index_File_Name = $self->{'bioindex_pfamB'};  ## NEW NEED IMPL
   #my $Index_File_Name = "/nfs/disk100/pubseq/Pfam/temp/index/new_bio_Pfam-B.index";
my $Index_File_Name = "/nfs/disk100/pubseq/Pfam/temp/index/11.0_Pfam-B.index";
  my $inx = Bio::Pfam::Stockholm->new($Index_File_Name);

  my $bio_index = $inx->fetch($id);
  my $file = $$ . "_index"; 
  open(_BIO, ">$PfamWWWConfig::tempdir/$file");
  
  while(<$bio_index>) {
    /^\/\// && last;
    print _BIO $_;
  }
  close(_BIO);
  
  if (defined($bio_index)) {
    
    #     print "EEP HAS A VALUE <BR>";
  } else {
    #     print "NO VALUE FOR EEP <BR>";
  }
  
  #    $pfambdb = $self->{'dbweb_srspfamb'};
  
  #   my $getz = $self->{'dbweb_getz'};
  
  
  #open TEST, "$getz -e '[$pfambdb-id:$id]' |";
  
  open(_BIO, "$PfamWWWConfig::tempdir/$file");
  
  while (<_BIO>) {
#    print "ALL: $_ <BR>";
    #    while (<TEST>) {
    if (/^AC\s+(\S+)/) {
#      print "ACC: $1 <BR>";
      $out = Bio::Pfam::EntryB_Impl->new();
      $out->_datasource("$PfamWWWConfig::tempdir/$file");
      
      #	    $out = Bio::Pfam::EntryB_Impl->new();
      #	    $out->_datasource("$getz -e '[$pfambdb-id:$id]' |");
      $out->id($id);
      $out->acc($1);
      last;
    }
  }
  #    close TEST;
  close(_BIO);
  
  if (not defined $out) {
  #  print STDERR "BLEE CANT FIND $id \n";
    $self->throw("There is no entry for id $id");
  }
  else {
    return $out;
  }
}


sub test_nested_regions {
  my ($self, $acc1, $acc2) = @_;

  my($dbh, $nesting_allowed);
#  print "ACC: $acc1 :: 2: $acc2 <P>";
  eval {
    $dbh = $self->_the_RDB->connect();
    if(defined($dbh)) {
    
      my $st = "select auto_pfamA from pfamA where pfamA_acc = '$acc1'";
      my($stat) = $dbh->prepare($st);
      $stat->execute();
      my ($auto_pfamA) = $stat->fetchrow;
      $stat->finish;

      my $st_1 = "select pfamA_acc from nested_domains, pfamA where nested_domains.auto_pfamA = '$auto_pfamA' and nested_domains.nests_auto_pfamA = pfamA.auto_pfamA ";
     # print "ST: $st <P>";
      my($stat_a) = $dbh->prepare($st_1);
      $stat_a->execute();
      my $temp_acc;
      while ( ($temp_acc) = $stat_a->fetchrow) {
#	print "TEMP: $temp_acc <P>";
	$nesting_allowed = 1 if ($temp_acc eq $acc2);
      }
      
      $stat->finish;
      
      $self->_the_RDB->disconnect;
    }
  };
  #print "ALLOWED: $nesting_allowed <P>";
  return $nesting_allowed;
  
}


sub search_pfam {

  my ($self, $terms) = @_;
  my @results;

  my ($pfamA_acc, $pfamA_id, $description, $dbh, $st, %store_acc);

  eval {
       $dbh = $self->_the_RDB->connect();
   };
  if ($@ or not defined( $dbh ) or $self->{'no_rdb'}) { 
    print "<b>Sorry, we are experiencing problems at the moment and are unable to complete the query</B><P>";
    return; 
    
  } 

  ##  #### GET FAMILIES FROM pfamA table


  my $st_a = "select pfamA_acc, pfamA_id, description from pfamA where pfamA_acc = '$terms' or pfamA_id = '$terms'";

    eval {
    #  $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	
	my($stat) = $dbh->prepare($st_a);
	$stat->execute();
	while ( ($pfamA_acc, $pfamA_id, $description) = $stat->fetchrow) {
	  push @results , "$pfamA_acc~$pfamA_id~$description";
	  $store_acc{$pfamA_acc} = $pfamA_acc;
	}
	
	$stat->finish;
	
#	$self->_the_RDB->disconnect;
      }
    };


  $st = " select pfamA_acc, pfamA_id, description from pfamA where ( ";
  my (@fields) = ("pfamA_acc", "pfamA_id", "description", "type", "comment", "author", "previous_id");
  my $num_fields = @fields;
  
  my (@terms);
  if ($terms !~ /\&/) {
    push @terms, $terms;
  } else {
    (@terms) = split(/\&/, $terms);
  }
  
  my $num_terms = @terms;
  
  my $count_terms = 0;
  my $count_fields = 0;
  
  foreach my $term (@terms) {
    $count_terms++;
  next if ($term !~ /[A-Z]|[a-z]|[0-9]/);
    $count_fields = 0;
    foreach my $field (@fields) {
      $count_fields++;
      $st = $st . " $field like '%$term%' ";
      $st = $st . " or " if ($count_fields ne $num_fields);
    }
    $st = $st . " ) ";
    $st = $st . "  and (" if ($count_terms ne $num_terms);
  }
  
  
  
  
  eval {
    #$dbh = $self->_the_RDB->connect();
    if(defined($dbh)) {
      
      my($stat) = $dbh->prepare($st);
      $stat->execute();
      while ( ($pfamA_acc, $pfamA_id, $description) = $stat->fetchrow) {
	if (defined($store_acc{$pfamA_acc})) {
	    #push @results , "$pfamA_acc~$pfamA_id~$description";
	  } else {
	    push @results , "$pfamA_acc~$pfamA_id~$description";
	    $store_acc{$pfamA_acc} = $pfamA_acc;
	  }
	$store_acc{$pfamA_acc} = $pfamA_acc;
      }
      
      $stat->finish;
      
     # $self->_the_RDB->disconnect;
    } else {
      print "BOMBED <P>";
    }
  };
  



  #### SEARCH TERMS IN INTERPRO & GO TABLE 

  my (@fields_a) = ("interpro_abstract", "interpro_id", "go_function",  "go_component", "go_process");

  
  foreach my $field (@fields_a) {
    my $temp_go;
    $temp_go = " , $field " if ($field =~ /^go/i);
    $st = " select pfamA_acc, pfamA_id, description, interpro_id  $temp_go from interpro_and_go, pfam_to_interpro, pfamA where ( ";
      
    my $temp_field;
    my $inter_id;
    
    my (@terms);
    if ($terms !~ /\&/) {
      push @terms, $terms;
    } else {
      (@terms) = split(/\&/, $terms);
    }
    
    my $num_terms = @terms;
    
    my $count_terms = 0;
    
    foreach my $term (@terms) {
      $count_terms++;
  next if ($term !~ /[A-Z]|[a-z]|[0-9]/);
      $st = $st . " $field like '%$term%' ";
      $st = $st . " ) ";
      $st = $st . "  and (" if ($count_terms ne $num_terms);
    }
    $st = $st . " and interpro_and_go.auto_interpro = pfam_to_interpro.auto_interpro and pfam_to_interpro.auto_pfamA = pfamA.auto_pfamA";
    
  
    eval {
      #$dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	my($stat) = $dbh->prepare($st);
	$stat->execute();
	while ( ($pfamA_acc, $pfamA_id, $description,$inter_id, $temp_field) = $stat->fetchrow) {
	  if (defined($store_acc{$pfamA_acc})) {
	   # push @results , "$pfamA_acc~$pfamA_id~$description";
	  } else {
	    my $field_type = " *INTERPRO*$inter_id* ";
	    $field_type = " *GO*$temp_field*$temp_go* " if ($field =~ /go/i);
	    push @results , "$pfamA_acc~$pfamA_id~$description ";
	    $store_acc{$pfamA_acc} = $pfamA_acc;
	  }
	  
	}
	
	$stat->finish;
	
#	$self->_the_RDB->disconnect;
      }
      };
    
    if ($@) {
      
      print "PROBLEM WITH QUERY AS $@ <P>";
    }

  }

 


  #### SEARCH TERMS IN PDB

#  my (@fields) = ("pdb_id", "header", "title");
#  my $pdb_id;
# # print "TERMS: $terms <BR>";
#  foreach my $field (@fields) {
#    $st = " select distinct pfamA_acc, pfamA_id, description, pdb_id from pdb, pdbmap, pfamA where ( ";
      
    
#    my (@terms);
#    if ($terms !~ /\&/) {
#      push @terms, $terms;
#    } else {
#      (@terms) = split(/\&/, $terms);
#    }
    
#    my $num_terms = @terms;
    
#    my $count_terms = 0;
    
#    foreach my $term (@terms) {
#      $count_terms++;
#      next if ($term !~ /[A-Z]|[a-z]|[0-9]/);
#      $st = $st . " $field like '%$term%' ";
#      $st = $st . " ) ";
#      $st = $st . "  and (" if ($count_terms ne $num_terms);
#    }
#    $st = $st . " and pdb.auto_pdb = pdbmap.auto_pdb and pdbmap.auto_pfam = pfamA.auto_pfamA and pfam_region = '1'";
    
    
#    eval {
#      $dbh = $self->_the_RDB->connect();
#      if(defined($dbh)) {
##	print "ST: $st <BR>";
#	my($stat) = $dbh->prepare($st);
#	$stat->execute();
#	while ( ($pfamA_acc, $pfamA_id, $description, $pdb_id) = $stat->fetchrow) {
#	  #	  print "EEP: $pfamA_acc, $pfamA_id, $description <BR>";
#	  if (defined($store_acc{$pfamA_acc})) {
#	    push @results , "$pfamA_acc~$pfamA_id~$description";
#	  } else {
#	    my $field_type = " *PDB: $pdb_id* ";
#	    #$field_type = " *GO* " if ($field =~ /go/i);
#	    push @results , "$pfamA_acc~$pfamA_id~$description $field_type";
#	    #  print "NEW pfamA_id $pfamA_acc <BR>";
#	  }
	  
#	}
	
#	$stat->finish;
	
#	$self->_the_RDB->disconnect;
#      }
#      };
    
#    if ($@) {
      
#      print "PROBLEM WITH QUERY AS $@ <P>";
#    }

#  }

  return @results;

}



=head2 rdb_get_EntryA_by_acc

 Title   : get_EntryA_by_acc
 Usage   : $en = $db->get_EntryA_by_acc('PF00076');
 Function: get a Bio::Pfam::Entry object by acc
 Returns : Bio::Pfam::Entry object
 Args    : string of the accession number of the family


=cut

#sub _rdb_get_EntryA_by_acc{
  sub get_EntryA_by_acc{
    my ($self,$acc) = @_;
    my ($dir, $out , $dbh, $st);

    ##### PARAMS FOR pfamA table ########
    my ($auto_pfamA , $pfamA_acc , $pfamA_id , $description , $model_length , $author  , $seed_source , $alignment_method , $type   , $ls_sequence_GA , $ls_domain_GA , $fs_sequence_GA , $fs_domain_GA , $ls_sequence_TC , $ls_domain_TC , $fs_sequence_TC , $fs_domain_TC , $ls_sequence_NC , $ls_domain_NC , $fs_sequence_NC , $fs_domain_NC , $ls_mu , $ls_kappa , $fs_mu , $fs_kappa , $comment , $previous_id, $hmmbuild_ls, $hmmcalibrate_ls, $hmmbuild_fs, $hmmcalibrate_fs, $num_seed, $num_full, $created, $updated, $tmp_auto_pfamA, $average_length , $percentage_id , $average_coverage , $status);

    if (not $self->{'no_rdb'}) {
      
      
      eval {
	# if (not $self->{'no_rdb'}) {
	$dbh = $self->_the_RDB->connect();
	if(defined($dbh)) {
	  $st = "select * from pfamA, pfamA_web where pfamA_acc = '$acc' and pfamA.auto_pfamA = pfamA_web.auto_pfamA";

	  my($stat) = $dbh->prepare($st);
	  $stat->execute();
	  ($auto_pfamA , $pfamA_acc , $pfamA_id , $description , $model_length , $author  , $seed_source , $alignment_method , $type   , $ls_sequence_GA , $ls_domain_GA , $fs_sequence_GA , $fs_domain_GA , $ls_sequence_TC , $ls_domain_TC , $fs_sequence_TC , $fs_domain_TC , $ls_sequence_NC , $ls_domain_NC , $fs_sequence_NC , $fs_domain_NC , $ls_mu , $ls_kappa , $fs_mu , $fs_kappa , $comment , $previous_id, $hmmbuild_ls, $hmmcalibrate_ls, $hmmbuild_fs, $hmmcalibrate_fs, $num_seed, $num_full, $created, $updated, $tmp_auto_pfamA, $average_length , $percentage_id , $average_coverage , $status) = $stat->fetchrow;
	  $stat->finish;
	  
	#  $self->_the_RDB->disconnect;
	}
	# }
      };
    
      if ($@ or not defined( $dbh ) or $self->{'no_rdb'} or (!$auto_pfamA)) { 
	## Connection to the database failed - get pfama from the flatfile

	$out = $self->_file_get_EntryA_by_acc($acc);
	
	
      } else {

	
	$out = Bio::Pfam::EntryA_Web->new();
	$out->from_rdb(1);
	$out->acc($acc);
	
	
	$out->entry_type($type );
	$out->id($pfamA_id );
	$out->model_length($model_length);
	$out->author($author );
	$out->source($seed_source );
	$out->alignmethod($alignment_method );
	
	$out->ls_sequence_gathering_cutoff($ls_sequence_GA );
	$out->ls_domain_gathering_cutoff($ls_domain_GA );
	$out->fs_sequence_gathering_cutoff( $fs_sequence_GA );
	$out->fs_domain_gathering_cutoff($fs_domain_GA );
	
	$out->ls_sequence_trusted_cutoff($ls_sequence_TC );
	$out->ls_domain_trusted_cutoff($ls_domain_TC );
	$out->fs_sequence_trusted_cutoff($fs_sequence_TC );
	$out->fs_domain_trusted_cutoff( $fs_domain_TC);
	
	$out->ls_sequence_noise_cutoff($ls_sequence_NC );
	$out->ls_domain_noise_cutoff($ls_domain_NC );
	$out->fs_sequence_noise_cutoff($fs_sequence_NC );
	$out->fs_domain_noise_cutoff($fs_domain_NC );
	
	$out->previous_ids($previous_id );
	
	$out->auto_pfamA($auto_pfamA );
	
	$out->ls_kappa($ls_kappa );
	$out->ls_mu($ls_mu );
	$out->fs_kappa($fs_kappa );
	$out->fs_mu($fs_mu );
	
	$out->comment($comment );
	$out->description( $description);
	
	### KNOWS WHAT THE DATA DIR IS !
	my $dir = $self->{'dbweb_datadir'};
	$out->_datadir($dir);
	
	$out->num_seqs_in_full($num_full);
	$out->num_seqs_in_seed($num_seed);
	$out->average_length($average_length);
	$out->percentage_id($percentage_id);
	$out->average_coverage($average_coverage);

	$out->add_build_line($hmmbuild_ls);
	$out->add_build_line($hmmcalibrate_ls);
	$out->add_build_line($hmmbuild_fs);
	$out->add_build_line($hmmcalibrate_fs);
	
	
	############# FILL UP THE ANNOTATION OBJECT FROM THE RDB ################
	# GET PDB, database_links & literature_refs and add then to the object!
	################################################################################
	
	my @refs;
	
	##### GET LITERATURE REFERENCES
	#### PARAMS FOR LITERATURE REFS ########
	my($lit_comment, $order_added, $medline, $title, $author , $journal);
	
	eval {
	  if (not $self->{'no_rdb'}) {
	    $dbh = $self->_the_RDB->connect();
	    if(defined($dbh)) {
	      $st = "select comment, order_added, medline, title, author , journal from pfamA_literature_references, literature_references where auto_pfamA = '$auto_pfamA' and pfamA_literature_references.auto_lit = literature_references.auto_lit";
	      
	      my($stat) = $dbh->prepare($st);
	      $stat->execute();
	      #  while ( ($st, $en, $acc, $id) = $st_pfamB_reg->fetchrow) {
	      
	      while ( ($lit_comment, $order_added, $medline, $title, $author , $journal) = $stat->fetchrow) {
		push @refs, "LIT:$lit_comment~$order_added~$medline~$title~$author~$journal";
	
	      }
	      $stat->finish;
	      
	#      $self->_the_RDB->disconnect;
	    }
	  }
	};
	
	
	
	#### PARAMS FOR DATABASE LINKS ########
	my($db_id, $db_comment, $db_link, $other_params);
	
	eval {
	  if (not $self->{'no_rdb'}) {
	    $dbh = $self->_the_RDB->connect();
	    if(defined($dbh)) {
	      $st = "select db_id, comment , db_link, other_params from pfamA_database_links where auto_pfamA = '$auto_pfamA'";
	      
	      my($stat) = $dbh->prepare($st);
	      $stat->execute();
	     
	      my %db_links;
	      while ( ($db_id, $db_comment, $db_link, $other_params) = $stat->fetchrow) { #### ADD OTHER STUFF
		
		if ( ($db_comment || $other_params)  ||  ($db_id !~ /HOMSTRAD/i) && ($db_id !~ /PFAMB/i) && ($db_id !~ /COGS/i) ) {
		  push @refs, "DATA:$db_id~$db_comment~$db_link~$other_params";
		  
		} else {
		  $db_links{$db_id} = $db_links{$db_id} . " " . $db_link;

		} 


		
	      }

	      foreach my $db_id (sort keys %db_links) {

		push @refs, "DATA:" . $db_id . "~ ~" . $db_links{$db_id} ."~ ";
		
	      }

#	      my $pfam_db_link;
#	      while ( ($db_id, $db_comment, $db_link, $other_params) = $stat->fetchrow) { #### ADD OTHER STUFF
		
#		if ($db_id !~ /PFAMB/i) {
#		  push @refs, "DATA:$db_id~$db_comment~$db_link~$other_params";
#		} else {
#		  $pfam_db_link = $pfam_db_link . " " . $db_link;	
#		}
		
#	      }
	      
#	      push @refs, "DATA:PFAMB~ ~$pfam_db_link~ " if ($pfam_db_link);
	     
	      
	      $stat->finish;
	      
	#      $self->_the_RDB->disconnect;
	    }
	  }
	};
	
	
	#### ADD PDBMAP !!!! ##########
	my($tmp_pdb_id, $tmp_chain, $start, $end);

	eval {
	  if (not $self->{'no_rdb'}) {
	    $dbh = $self->_the_RDB->connect();
	    if(defined($dbh)) {
	      $st = "select pdb_id, chain, pdb_start_res, pdb_end_res from pdb, pdbmap, pfamA where pfamA_acc = '$acc' and pfamA.auto_pfamA = pdbmap.auto_pfam and pfam_region = '1' and pdbmap.auto_pdb = pdb.auto_pdb";
	      
	      my($stat) = $dbh->prepare($st);
	      $stat->execute();
	     
	      while ( ($tmp_pdb_id, $tmp_chain, $start, $end) = $stat->fetchrow) { #### ADD OTHER STUFF
	
		push @refs, "DATA:PDB~~$tmp_pdb_id $tmp_chain~$start;$end";
	
	      }
	      $stat->finish;
	      
	#      $self->_the_RDB->disconnect;
	    }
	  }
	};


 #### ADD ROSETTA VALUES !!!! ##########

        my($pdb_id, $structure, $model_num, $chain, $mammoth_Z_score, $length_ratio, $group_confidence , $scop_confidence, $auto_pdb);

        eval {
          if (not $self->{'no_rdb'}) {
            $dbh = $self->_the_RDB->connect();
            if(defined($dbh)) {
              $st = "select  group_confidence, scop_confidence, structure, model_num, chain, mammoth_Z_score, length_ratio, auto_pdb  from rosetta_pfam_structures, rosetta_pfam_values, pfamA where pfamA_acc = '$acc' and pfamA.auto_pfamA = rosetta_pfam_structures.auto_pfamA and  rosetta_pfam_values.auto_pfamA = pfamA.auto_pfamA  ";
         # and ( rosetta_pfam_structures.auto_pdb = pdb.auto_pdb  or auto_pdb = '0')

              my($stat) = $dbh->prepare($st);
              $stat->execute();
              my $auto_pdb;
              while ( ($group_confidence, $scop_confidence, $structure, $model_num,  $chain, $mammoth_Z_score, $length_ratio, $auto_pdb  ) = $stat->fetchrow) { #### ADD OTHER STUFF

                my $st = $dbh->prepare("select pdb_id from pdb where auto_pdb = '$auto_pdb'");
                $st->execute();
                $pdb_id = $st->fetchrow;
                $st->finish();

        #       print "$group_confidence, $scop_confidence, $structure, $model_num, $pdb_id, $chain,$mammoth_Z_score, $length_ratio <BR>";
                push @refs, "DATA:ROSETTA~~$structure~$group_confidence;$scop_confidence;$model_num;$pdb_id;$chain;$mammoth_Z_score;$length_ratio";

              }
              $stat->finish;

  #            $self->_the_RDB->disconnect;
            }
          }
        };



	$out->_load_refs_from_rdb($comment, $description, @refs);
	$self->_the_RDB->disconnect;

      } ### end if RDB CONNECTION FAILED
      
      
    } else {
      
      $out = $self->_file_get_EntryA_by_acc($acc);
      
      
    } # END IF SWITCHED OFF RDB CONNECTION WITH NORDB FILE
    
    return $out;
    
}



sub _file_get_EntryA_by_acc{
   my ($self,$acc) = @_;
   my ($dir,$out);
   $dir = $self->{'dbweb_datadir'};
  
   open(_DESC,"$dir/desc/$acc.desc") || &PfamWWWConfig::user_error("This is not a valid Pfam accession number", 1);

   $out = Bio::Pfam::EntryA_Web->new();
   $out->from_rdb(0);
   $out->_datadir($dir);
   $out->acc($acc);
   close(DESC);
   return $out;

}



sub get_pfamseq_ids_for_pfam_regions {
  
  my($self, $aln_type, $acc) = @_;
  
  my(@ids, $dbh, $st, $stat, %pfamseq, @store, @final_seqs);
  
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	if ($aln_type =~ /FULL/i) {
	  
	  #### UPDATE SQL - IN FOR TESTING 
	  $st = "select  pfamseq_id, tree_order from pfamA_reg_full, pfamseq, pfamA where pfamA_acc = '$acc' and pfamA.auto_pfamA = pfamA_reg_full.auto_pfamA and pfamA_reg_full.auto_pfamseq = pfamseq.auto_pfamseq and in_full = '1' ";

	} else {
	  $st = "select distinct pfamseq_id from pfamA_reg_seed, pfamseq, pfamA where pfamA_acc = '$acc' and pfamA.auto_pfamA = pfamA_reg_seed.auto_pfamA and pfamA_reg_seed.auto_pfamseq = pfamseq.auto_pfamseq ";
	}
	my($stat) = $dbh->prepare($st);
	$stat->execute();
	my ($pfamseq_id, $tree_order);
	while (($pfamseq_id, $tree_order) = $stat->fetchrow) {
	  if (defined($pfamseq{$pfamseq_id})) {
	    if ($tree_order < $pfamseq{$pfamseq_id}) {
	      $pfamseq{$pfamseq_id} = $tree_order if ($tree_order ne 0);
	    }
	  } else {
	    $pfamseq{$pfamseq_id} = $tree_order if ($tree_order ne 0);
	  }
		      
	  ###push @ids, $pfamseq_id;
	}
	      
	foreach my $pfam_id (sort keys %pfamseq) {
	  my %temp = ('id' => $pfam_id,
		      'count' => $pfamseq{$pfam_id}
		      );
	  push @store, \%temp;

	}
	      
	 @store = sort { $a->{'count'} <=> $b->{'count'} } @store;
	
	my($outer_loop) = 0;
	while ($outer_loop <= $#store ) {
	  

	  push @final_seqs, $store[$outer_loop]->{'id'};
	  $outer_loop++;
	}

 
      
	      
	$stat->finish;
	      
	      


	$self->_the_RDB->disconnect;
      }
    }
  };

  return @final_seqs;
}



sub get_EntryB_by_acc{
   my ($self,$acc, $get_alignment) = @_;
   my ($out, $pfambdb);


#print "Content_type: text/html\n\n";
#print "Here <P>";

#   print " THE ACC: $acc <BR>";
#   my $Index_File_Name = $self->{'bioindex_pfamB'};  ## NEW
  # my $Index_File_Name = "/nfs/disk100/pubseq/Pfam/temp/index/new_bio_Pfam-B.index";
my $Index_File_Name = "/nfs/disk100/pubseq/Pfam/temp/index/11.0_Pfam-B.index";
   #print "IN: $Index_File_Name <P>";
   my $inx;
   #eval {
   #exit(0);
     $inx = Bio::Pfam::Stockholm->new('-filename' => $Index_File_Name);
    # print "THE INX: $inx <P>";
     #$inx = Bio::Pfam::Stockholm->new($Index_File_Name);
   #};
   #if ($@) {
   #  print "BOMBED AS $@ <P>";
   #}
#print "INX: $inx <P>";
#print "Content_type: text/html\n\n";
#print "Here  HERE <P>";

   my $bio_index = $inx->fetch($acc);
   my $file = $acc. "_index" ;
#print "FILE: $PfamWWWConfig::tempdir/$file <P>";
   if (! -e "$PfamWWWConfig::tempdir/$file") {
#	print "DOSENT EXIST: $PfamWWWConfig::tempdir/$file <P>";
     open(_BIO, ">$PfamWWWConfig::tempdir/$file");
     
     while(<$bio_index>) {
       /^\/\// && last;
#	print "$_ <BR>";
       print _BIO $_;
     }
     close(_BIO);
     
   }



   if ($get_alignment) {
     
    
     my $write = "$PfamWWWConfig::tempdir/$acc.shtml";
     if  (! -e "$write") {
     
       
       my $jtml_file = "$PfamWWWConfig::tempdir/$acc" . "_jtml";
       my $outfile = "$PfamWWWConfig::tempdir/$acc" . ".tmp";
      
       my $got_GS = 0;
       open(_JTML, ">$jtml_file");
       open(_INDEX, "$PfamWWWConfig::tempdir/$file");

       while(<_INDEX>) {
	 if ($_ =~ /^\#=GS/) {
	   
	   $got_GS = 1;
	 }
	 
	 if ( ($got_GS) ) {
	   
	   print _JTML $_;
	 }
	 
	 
       }
                     
       close(_JTML);
       close(_INDEX);
   
       my $command = "/nfs/disk100/pubseq/Pfam/bin/pfam_jtml $jtml_file 80 > $outfile";
     

       system($command);

       unlink $jtml_file;

       ##### ADD THE HTML !
        
       open(_READ, "$outfile");
       open(_JTML, ">$write");

       while(<_READ>) {
	 my $line = $_;

	 if ($_ =~ /(HREF=\"Jtml\.css\")/) {
	#   my $jtml_tag;
	   my $tag = $1;	 
	   my $jtml_tag = "HREF=\"$PfamWWWConfig::WWW_root/Jtml.css\" ";
	   $_ =~ s/$tag/$jtml_tag/;
	   
	 }


	 my ($id, $junk) = split(/&/, $_);
	 if ($id =~ /<b>(.*)/) {
	   my $all = $1;
	   
	   my $swiss;
	   my $residues;
	   
	   if ($all =~ /\//) {
	     ($swiss, $residues) = split(/\//, $all);
	   } else {
	     $swiss = $all;
	   }
	   
	   my $new_string = "<NOBR><A HREF=/cgi-bin/Pfam/swisspfamget.pl?name=$swiss>$all</A></NOBR>";
	   $line =~ s/$all/$new_string/;
	   print _JTML $line;
	   
	 } else {
	   print _JTML $_;
	 }
	 
       }

       close(_JTML);
       close(_READ);
   
       system("rm -f $jtml_file");
       system("rm -f $outfile");

     } 


   } #/ end if get coloured alignment



#   open(_BLEE, $inx->fetch($acc));
#   while(<_BLEE>) {
#     print "BLEE: $_ <BR>";
     
#   }

#   close(_BLEE);

#   print "THE EEP : $bio_index <BR>";

   if (defined($bio_index)) {

#     print "EEP HAS A VALUE <BR>";
   } else {
#     print "NO VALUE FOR EEP <BR>";
   }

#   open (TEST, $bio_index) 
#       or print " DOSENT WORK !!! $! <P>";

 #  print "FILE: $file <BR>";
#   open(_BIO, "$PfamWWWConfig::tempdir/$file");
#   while(<_BIO>) {
#     /^\/\// && last;
#  #   print " ALL: $_ <BR>";
#   }
#   close(_BIO);





#my $id;
#   while(<$bio_index> ) {
#     /^\/\// && last;
#      print "DOL: $_ <BR>"; 
#     #  if (/^>(\S+).*\s(\d+)\sa\.a\./) {
#     if (/^#=GF\s+AC\s+(\S+)/) {
#	 print "ID: $1 \n";
#	 # we have found the id line.
#	 if (not defined($id)) {
#	   $id = $1;
	   
#	 }
#	 else {
#	   last;
#	 }
	 
#       }      else {
#	 # we must have a domain entry line
#	 if (/^(\S+)\s+\d+.*\(\d+\)\s+(\w+\d+)\s+?(.*?)(?=\s+\d+-\d+)/) {
#	   my ($domainName, $acc, $annotation, $interval, $remainder, $id, %type_hash
#	      );
	   
#	   $domainName = $1;
#	   $acc = $2;
#	   $annotation = $3;
#	   $remainder = $';
#	   print "$domainName, $acc, $annotation, $interval, $remainder \n";
#	 }
	 
#       }
     
     
# #    print "BEFORE $_<BR>";
#  #   $_ =~ s/#=GF\s+//;
##     print "AFTER $_<BR>";


#   }
#   print "<P><P> EEP : $bio_index <P>";




#   $pfambdb = $self->{'dbweb_srspfamb'};

#   my $getz = $self->{'dbweb_getz'};
#   open (TEST, "$getz -e '[$pfambdb-acc:$acc]' |") 
#       or $self->throw("Could not open the getz test command for $acc");



  # while (<TEST>) {
 open(_BIO, "$PfamWWWConfig::tempdir/$file");
   while (<_BIO>) {
     #print "ALL: $_ <BR>";
       if (/^ID\s+(\S+)/) {
#	 print " THE GROOVE: $1 <P>";
	    $out = Bio::Pfam::EntryB_Impl->new();
#	    $out->_datasource("$getz -e '[$pfambdb-acc:$acc]' |"); 
	    $out->_datasource("$PfamWWWConfig::tempdir/$file");
#	      $out->test_indexing($bio_index);
#	    $out->_datasource("$acc");
#	    print " GOT IT ! <BR>";
	  #  $out->_datasource("$inx->fetch($acc) |");
	  #  $out->_datasource($bio_index);
#	    print "ACC: $acc :: ID: $1 <P>";
	    $out->acc($acc);
	    $out->id($1);
	    last;
       }
   }
#   close TEST;
close(_BIO);

 #  unlink("$PfamWWWConfig::tempdir/$file");

   if (not defined $out) {
     print STDERR "BLEE FOUND FIND $acc \n";
       $self->throw("There is no entry for acc $acc");
   }
   else {
       return $out;
   }
}


#=head2 get_EntryB_by_acc

#Title   : get_EntryB_by_acc
# Usage   : $en = $db->get_EntryB_by_acc('PB000076');
# Function: get a Bio::Pfam::Entry object by acc
# Returns : Bio::Pfam::Entry object
# Args    : string of the accession number of the family


#=cut

#sub get_EntryB_by_acc{
#   my ($self,$acc) = @_;
#   my ($out, $pfambdb);

#   $pfambdb = $self->{'dbweb_srspfamb'};

#   my $getz = $self->{'dbweb_getz'};
#   open (TEST, "$getz -e '[$pfambdb-acc:$acc]' |") 
#       or $self->throw("Could not open the getz test command for $acc");

#   while (<TEST>) {
#       if (/^ID\s+(\S+)/) {
#	    $out = Bio::Pfam::EntryB_Impl->new();
#	    $out->_datasource("$getz -e '[$pfambdb-acc:$acc]' |");
#	    $out->acc($acc);
#	    $out->id($1);
#	    last;
#       }
#   }
#   close TEST;

#   if (not defined $out) {
#     &PfamWWWConfig::user_error("This is not a valid Pfam accession number", 1);
#       $self->throw("There is no entry for acc $acc");
#   }
#   else {
#       return $out;
#   }
#}


######################################################################################################
#  MODEL LENGTH
######################################################################################################


=head2 _model_length

 Title   : _model_length
 Usage   : $id = $db->acc2id("PF00001");
 Function: converts an accession to an id
 Returns : the id, throws an exception if no accession, or dead accession
 Args    :

=cut

sub _model_length {
   my ($self,$acc) = @_;
   my ($dbh, $st, $len, $id);
   eval {
       if (not $self->{'no_rdb'}) {
           $dbh = $self->_the_RDB->connect();

           if(defined($dbh)) {
               $dbh = $self->_the_RDB->connect();
               $st = $dbh->prepare("select pfamA_id, model_length from pfamA where pfamA_acc = '$acc'");
               $st->execute;
               ($id, $len) = $st->fetchrow;
               $st->finish;
               $self->_the_RDB->disconnect;
           }
       }
   };

   if($self->{'no_rdb'} or $@) {
  
   }
   else {
       if (not defined $id) {
           $self->throw("DB_RDB::model_length - $acc is not a valid id for this pfam database");
       }
       elsif (not defined $len) {
           $self->throw("DB_RDB::model_length - $acc is the id of a dead family");
       }

   }

   return $len;
}

######################################################################################################
## FAMILY TYPE
######################################################################################################

sub get_family_type {

  my($self, $acc) = @_;

  my($dbh, $st, $type);

  eval {
       if (not $self->{'no_rdb'}) {
           $dbh = $self->_the_RDB->connect();

           if(defined($dbh)) {
               $dbh = $self->_the_RDB->connect();
               $st = $dbh->prepare("select type from pfamA where pfamA_acc = '$acc'");
               $st->execute;
               ($type) = $st->fetchrow;
               $st->finish;
               $self->_the_RDB->disconnect;
           }
       }
   };

  if($self->{'no_rdb'} or $@) {
	$type = "family";
    # $self->throw("Error getting type from RDB \n");
   }
  $type = lc($type);
     return $type;
}




######################################################################################################
# COMPLEXES
######################################################################################################

sub interaction_overview {
  my ($self,$acc, $pdb) = @_;
  my($dbh, $st, %return);
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	
	
#	### UPDATE FOR PFAMA & B
	$st = "select pfamA_int_pfamA.auto_partner_pfamA, pfamA_int_pfamA.auto_dom_int from pfamA,pfamA_int_pfamA where pfamA_acc = '$acc' and pfamA.auto_pfamA =  pfamA_int_pfamA.auto_pfamA";
	#print "st: $st <P>";
	my($int_stat) = $dbh->prepare($st);
	$int_stat->execute();
	my ($auto_partner_pfamA, $auto_dom_int);
	
	while ( ($auto_partner_pfamA, $auto_dom_int ) = $int_stat->fetchrow) {
	  my $first = 1;
#	  print "<P>$auto_partner_pfamA, $auto_dom_int  <BR>";
	  my $pdb_sql;

	  if ($pdb) {
	    $pdb_sql = " and pdb_id = '$pdb'";
	  }

	  #my $new_st = "select distinct pfamA_id, pfamA_acc, pdb_id from pfamA, pdb, pdb_domain_egs where auto_pfamA = '$auto_partner_pfamA' and pdb_domain_egs.auto_dom_int = '$auto_dom_int' and pdb_domain_egs.auto_pdb = pdb.auto_pdb $pdb_sql";
	  #print "st: $new_st <P>";
	  #my($val_stat) = $dbh->prepare($new_st);
	  #$val_stat->execute();
	  #my ($pfamA_id, $pfamA_acc);
	  #my(@values);
	  #while ( my($tmp_pfamA_id, $tmp_pfamA_acc, $pdb_id) = $val_stat->fetchrow) {
	    #print "$pfamA_id, $pfamA_acc, $pdb_id <BR>";
	   # my $tilda = "";
	    #$tilda = "~" if (!$first);
	     #$return{$tmp_pfamA_id . "~" . $tmp_pfamA_acc} .=  $tilda . $pdb_id;
	     ##print "$tmp_pfamA_id $tmp_pfamA_acc $pdb_id <BR>";
	    #$first = 0;
	  #}#
	  
	 # print "id: $pfamA_id, acc: $pfamA_acc, vals: ~values <P>";
	  
	 # $count++;
	}
	
	
      
	
      }
      
    }
  
    
    
  };
  


  
  return \%return;
  
}

sub interaction_seq {
 my ($self, $pdb) = @_;
  my($dbh, $st, %return, %results);

  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	
	
#	### UPDATE FOR PFAMA & B
	$st = " select pfamseq_id, pfamseq_seq_number from msd_data, pfamseq, pdb where pdb_id = '$pdb' and pdb.auto_pdb = msd_data.auto_pdb and msd_data.auto_pfamseq = pfamseq.auto_pfamseq";

	#print "st: $st <P>";
	my($int_stat) = $dbh->prepare($st);
	$int_stat->execute();
	
	while ( my($pfamseq_id, $pfamseq_seq_number ) = $int_stat->fetchrow) {
	  $results{$pfamseq_id} .= "~" . $pfamseq_seq_number;
	 # print "$pfamseq_id, $pfamseq_seq_number <BR>";
	}

      }
    }
  };

 foreach my $key (sort keys %results) {
   my $min = "99999999999999999"; my $max = 0;
   my (@temp) = split(/~/, $results{$key});
   foreach (@temp) {
    #
#	my ($auto_partner_pfamA, $auto_dom_int); print "$_ <BR>";
     next if ($_ !~ /[0-9]/);
     if ($_ > $max) {
       $max = $_;
     }
     if ($_ < $min) {
       $min = $_;
     }

   }
   $return{$key} = $min . "~" . $max;

   #print "key: $_ , val: " .$results{$_} . " <BR>";
 }

 return %return;

}



 


sub get_complexes{




  my ($self,$acc, $complex_exists) = @_;
  my ($dbh, $st, $id,  $desc, $len, $org, $out, $seq);

  my %arr;
  my $complex = 0;
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	
	my $pfamA = 0;

	if ($acc =~ /^PF/i) {
	  
	   ### UPDATE FOR PFAMA & B
	  $st = "select distinct auto_pdb from pdbmap,  pfamA where pfamA_acc = '$acc' and pfamA.auto_pfamA = pdbmap.auto_pfam and pfam_region = '1' and complex = '1'";

	  $pfamA = 1;
	} else {
	  $st = "select distinct auto_pdb from pdbmap,  pfamB where pfamB_acc = '$acc' and pfamB.auto_pfamB = pdbmap.auto_pfam and pfam_region = '0' and complex = '1'";

	}

        my($stat) = $dbh->prepare($st);
        $stat->execute();
        my( $auto_pdb, $pdb, $domain, $pfamseq);

	
	my @the_pfam_type = ("pfamA", "pfamB");
        while ( ($auto_pdb  ) = $stat->fetchrow) {
	if ($complex_exists) {
	   $complex = 1;
	   return;
	 }
	  my @all;

	  
	  foreach my $the_pfam_type (@the_pfam_type) {
	    
	    $complex = 1;
	    
	    ### UPDATE FOR PFAMA & B
	    my $sql;
	    my $id = "pfamB_id";
	    my $the_auto = "auto_pfamB";
	    my $the_pfamregion = 0;
	    if ($the_pfam_type =~ /pfamA/i) {
	      $id = "pfamA_id";
	      $the_auto = "auto_pfamA";
	      $the_pfamregion = 1;
	    }

	
	      $sql =  "select pdb_id, chain,pdb_start_res, pdb_end_res, " .$id . " , pfam_region, pfamseq_id, pfam_start_res , pfam_end_res , complex  from pdb, pdbmap, $the_pfam_type, pfamseq where pdb.auto_pdb = '$auto_pdb' and pdb.auto_pdb = pdbmap.auto_pdb and pfam_region = " . $the_pfamregion. "  and pdbmap.auto_pfam = $the_pfam_type." . $the_auto . " and pdbmap.auto_pfamseq = pfamseq.auto_pfamseq order by " . $id ." ";
	      

	    
	    my($int_stat) = $dbh->prepare($sql);
	    $int_stat->execute();
	    my ($int_pdb, $int_chain, $pdb_start_atom, $pdb_end_atom,  $int_domain, $int_pfam_type,  $int_pfamseq, $int_pfam_start, $int_pfam_end,  $int_complex);
	    
	    
	    while ( ($int_pdb, $int_chain, $pdb_start_atom, $pdb_end_atom,  $int_domain, $int_pfam_type,  $int_pfamseq, $int_pfam_start, $int_pfam_end,  $int_complex  ) = $int_stat->fetchrow) {
	      
	      
	      $pdb = $int_pdb;
	      push @all, "$int_pdb~$int_chain~$pdb_start_atom~$pdb_end_atom~$int_domain~$int_pfamseq~$int_pfam_start~$int_pfam_end~$int_complex";
	    }

	    
          }			#/ end WHILE
	  


	  my @refs;
	  
	  my %store_ids;

	  foreach (@all) {

	    my($int_pdb, $int_chain, $pdb_start_atom, $pdb_end_atom,  $int_domain,  $int_pfamseq, $int_pfam_start, $int_pfam_end,  $int_complex) = split(/~/, $_);
	   
	    my $id = "$int_domain~$int_pfamseq";
	    
	    $store_ids{$id} = $store_ids{$id} . "*" . "$int_pfam_start~$int_pfam_end~$int_chain"; 
	    push @refs, $id;
	  }
	  
	  my ($temp_name, $temp_value);
          $temp_value = $pdb. ";";
	  
	  
	  my $first = 1;
	  
	  my %check_ids;
	  my @new_refs;
	  foreach (@refs) {
	    if(defined($check_ids{$_})) {

	    } else {
	      push @new_refs, $_;
	      $check_ids{$_} = $_;
	    }
	  }
	  
	  foreach my $key (@new_refs) {
	    my $min = "999999999999999";
	    my $max = 0;
	    my(@vals) = split(/\*/, $store_ids{$key} );
	    my %chains;
	    foreach my $val  (@vals) {
	      if ($val =~ /[A-Z]|[0-9]/i) {
		my($pfam_start, $pfam_end, $chain) = split(/~/, $val);
		$min = $pfam_start if ($pfam_start < $min);
		$max = $pfam_end if ($pfam_end > $max);
		$chains{$chain} = $chain;
	      }
	    }
	    
	    my ($the_domain, $the_pfamseq) = split(/~/, $key);
	    
	    if ($first) {
	      $temp_name = $the_domain;
	      $first = 0;
	    } else {
	      
	      
	      $temp_name = $temp_name . "~" . $the_domain;
	      
	    }
	    
	    $temp_value = $temp_value . $the_domain . "~" . $min . "~" . $max . "~" . $the_pfamseq . ":";
	    
	  }
	  



          if(defined($arr{$temp_name})) {
		my(@existing) = split(/\*/, $arr{$temp_name});
		my $not_equals = 1;
	
		foreach (@existing) {
	          $not_equals = 0 if ($temp_value eq $_);				
		}

	    $arr{$temp_name} = $arr{$temp_name} . "*" . $temp_value if($not_equals);
          } else {
            $arr{$temp_name} = $temp_value;
          }


        }

      }


    }


	
  };
  if ($complex_exists) {
    $complex
  } else {
    return %arr;
    
 }
}

# New Interaction Methods (rdf 01/2004)

=head2 

 Title   : get_interacting_domains
 Usage   : $db->get_interacting_domains($acc);
 Function: Takes a pfam accession and gets all domains that interact with it.
 Returns : Hash of accession as key with the value an array ref of structures contain such
		 : a domain interaction
 Args    : Pfam acc

=cut

sub get_interacting_domains {
	my ($self, $acc) = @_;
	my ($dbh, $query, %ints);

	eval{
		if (not $self->{'no_rdb'}){
			$dbh = $self->_the_RDB->connect();
			if(defined($dbh)){
				$query = "select distinct pfamA_id_B, pfamA_acc_B, pdb_id from interaction join pfamA, pdb where pfamA_acc=\"$acc\" and auto_pfamA=auto_pfamA_A and pdb.auto_pdb=interaction.auto_pdb";
			}
			my $st = $dbh->prepare($query);
			$st->execute();
			while(my @r = $st->fetchrow_array){
				push(@{$ints{"$r[0]~$r[1]"}}, $r[2]);

			}
			$st->finish;
			$self->_the_RDB->disconnect;
		}
	};
	return %ints;	
}


######################################################################################################
# CRC 64
######################################################################################################


sub _crc64_to_protein {

  my($self, $crc64, $pfam_type) = @_;
  my($dbh, $st);
  my($pfam_id, %all_pfam_ids,  $seq_start, $seq_end, $pfamseq_id, $pfamB_acc, $pfamB_id);


	if (!$pfam_type) {
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {

     
	$st = "select distinct pfamA_id from pfamA_reg_full, pfamA, pfamseq  where crc64 = '$crc64' and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq and pfamA_reg_full.auto_pfamA = pfamA.auto_pfamA ";

        my($stat) = $dbh->prepare($st);
        $stat->execute();
        
	my($pfam_id);

        while ( ($pfam_id) = $stat->fetchrow) {

	$all_pfam_ids{$pfam_id} = $pfam_id;
	}
      }


    }

  };
	} else {
	   eval {
      if (not $self->{'no_rdb'}) {
	$dbh = $self->_the_RDB->connect();
	if(defined($dbh)) {
	  
	  
	  $st = "select  pfamseq_id, seq_start, seq_end, pfamB_acc, pfamB_id from pfamB_reg, pfamseq, pfamB  where crc64 = '$crc64' and pfamseq.auto_pfamseq = pfamB_reg.auto_pfamseq and pfamB_reg.auto_pfamB = pfamB.auto_pfamB";

	  my($stat) = $dbh->prepare($st);
	  $stat->execute();
	  
	  my($pfam_id);
	  
	  while ( ($pfamseq_id, $seq_start, $seq_end, $pfamB_acc, $pfamB_id) = $stat->fetchrow) {
	 
	    my $fasta_search = $pfamseq_id . "~" . $seq_start . "~" . $seq_end . "~" .  $pfamB_acc . "~" . $pfamB_id;

	    $all_pfam_ids{$seq_start . "~" . $seq_end} = $fasta_search;
	  }
	}
	
	
      }
      
    };


	}

  return %all_pfam_ids;
  



}


######################################################################################################
# INTERPRO & QuickGO information
######################################################################################################


sub get_interpro_information {

  my($self, $pfamA_acc) = @_;
   my($dbh, $st, $interpro_id, $interpro_abstract, $process, $component, $function, %interpro_info);
 
  eval {

      $dbh = $self->_the_RDB->connect();
      if ( (defined($dbh))  or (not $self->{'no_rdb'})   ) {


	$st = "select interpro_and_go.interpro_id, interpro_abstract, go_function , go_component, go_process, pfamA.auto_pfamA from pfamA, interpro_and_go, pfam_to_interpro where pfamA_acc = '$pfamA_acc' and pfamA.auto_pfamA = pfam_to_interpro.auto_pfamA and pfam_to_interpro.auto_interpro = interpro_and_go.auto_interpro ";


        my($stat) = $dbh->prepare($st);

        $stat->execute();
        
	my($pfam_id);
	
        my ($interpro_id, $interpro_abstract, $go_function , $go_component, $go_process, $auto_pfamA) = $stat->fetchrow;

	
	if (!$auto_pfamA) {

	} else {
	  $interpro_info{'INTERPRO'} = $interpro_id ;
	  $interpro_info{'ABSTRACT'} = $interpro_abstract ;
	  $interpro_info{'FUNCTION'} = $go_function ;
	  $interpro_info{'PROCESS'} = $go_process ;
	  $interpro_info{'COMPONENT'} = $go_component ;
	  
	}

      } else {
	
	%interpro_info = $self->_file_get_interpro_information($pfamA_acc);
	
      }
      

      
      
  };
  

  return %interpro_info;
  

}


#### CANT GET INTERPRO FROM RDB - USE FILE INSTEAD !

sub _file_get_interpro_information {
  
  my($self, $acc) = @_;
  
  my($interpro_id, $interpro_abstract, $function, $process, $component, %interpro_info);
  my $interpro_file = $PfamWWWConfig::data_root . "/interpro/" . $acc . ".interpro";
 
  open(_INTERPRO, $interpro_file) or die "canna open $interpro_file $! <P>";;
  while(<_INTERPRO>) {

    if ($_ =~ /^ABSTRACT\s+(.*)/) {
      $interpro_abstract .= $1;
    } elsif ($_ =~  /^INTERPRO\s+(\S+)/){
      $interpro_id = $1;
     
    } elsif ($_ =~  /^PROCESS\s+(.*)/) {
      $process = $1;
    } elsif ($_ =~  /^COMPONENT\s+(.*)/){
      $component = $1;
    } elsif ($_ =~  /^FUNCTION\s+(.*)/) {
      $function = $1;
    } else {
      $interpro_abstract .= $_;
    }

  }
  $interpro_info{'INTERPRO'} = $interpro_id ;
  $interpro_info{'ABSTRACT'} = $interpro_abstract ;
  $interpro_info{'FUNCTION'} = $function ;
  $interpro_info{'PROCESS'} = $process ;
  $interpro_info{'COMPONENT'} = $component ;
  
close(_INTERPRO);

  return %interpro_info;

}


sub dql_query {
  my($self, $temp_and_arr, $temp_not_arr, $temp_or_arr) = @_;
  my (@and_arr, @not_arr, @or_arr);

  (@and_arr) = @{$temp_and_arr};
  (@or_arr) = @{$temp_or_arr};
  (@not_arr) = @{$temp_not_arr};

  my($dbh);
  
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      
    }
  };
  
  my (%pfamseq_seqs);
  my $count = 0;

  ########### AND #####################
  foreach my $pfam (@and_arr) {
    my (%tmp);
   # print "$_ <BR>";
    $count++;
    my $sub = "_id";
    $sub = "_acc" if ($pfam =~ /^PF\d\d\d\d\d/);
    my $st = "select pfamseq_acc from pfamA_reg_full, pfamseq, pfamA where pfamA" . $sub . " = '$pfam'  and pfamA.auto_pfamA = pfamA_reg_full.auto_pfamA and in_full = '1' and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq";
   # print "st: $st <P>";
    my($stat) = $dbh->prepare($st);
    $stat->execute();
    
    #print "STAT: $stat <P>";
    while ( my($tmp_acc) = $stat->fetchrow) {
      if ($count == 1) {

	$pfamseq_seqs{$tmp_acc} .= "~$count";

      } else {
#	print "HERE $count<P>";
	if (defined($pfamseq_seqs{$tmp_acc})) {
#	  print "ADD TO TMP: $tmp_acc <P>";
	  $tmp{$tmp_acc} = $count;
	}
      }

    #  print "$tmp_acc <BR>";
    }
    
    %pfamseq_seqs = %tmp if ($count > 1);
  }

  foreach (sort keys %pfamseq_seqs) {
  #  print "$_: " . $pfamseq_seqs{$_}. " <BR>";
  }


  ########### NOT ######################
#  $count > 0;
    foreach my $pfam (@not_arr) {
      my (%tmp);
   # print "$_ <BR>";
      $count++;
      my $sub = "_id";
      $sub = "_acc" if ($pfam =~ /^PF\d\d\d\d\d/);
    my $st = "select pfamseq_acc from pfamA_reg_full, pfamseq, pfamA where pfamA" . $sub . " = '$pfam'  and pfamA.auto_pfamA = pfamA_reg_full.auto_pfamA and in_full = '1' and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq";
   # print "st: $st <P>";
    my($stat) = $dbh->prepare($st);
    $stat->execute();
    
    #print "STAT: $stat <P>";
    while ( my($tmp_acc) = $stat->fetchrow) {
#      if ($count == 1) {

#	$pfamseq_seqs{$tmp_acc} .= "~$count";

#      } else {
#	print "HERE $count<P>";
	if (defined($pfamseq_seqs{$tmp_acc})) {
	  delete($pfamseq_seqs{$tmp_acc});
#	  print "ADD TO TMP: $tmp_acc <P>";
	 # $tmp{$tmp_acc} = $count;
	} else {
	#  print "ELSE $tmp_acc <P>";
	#   $tmp{$tmp_acc} = $count;
	}

      }

    #  print "$tmp_acc <BR>";
#    }
    
    #%pfamseq_seqs = %tmp;
  }
    #foreach (sort keys %pfamseq_seqs) {
   #print "$_: " . $pfamseq_seqs{$_}. " <BR>";
  #}

  return %pfamseq_seqs;
}


sub genome_get_doms {
  my($self, $ncbi) = @_;
  my($dbh, $stat, $st, %results);
  #print "CAT: $cat <P>";

  my(%sort_regs );

  $st = "select genome_seqs.auto_pfamA, pfamA_acc, pfamA_id, description, sum(count) from genome_seqs, pfamA where ncbi_code = '$ncbi' and genome_seqs.auto_pfamA = pfamA.auto_pfamA group by genome_seqs.auto_pfamA;
";
  #print "ST: $st <P>";
my(@domains);
  my (@all_regions, @order);
  eval {
    $dbh = $self->_the_RDB->connect();
    if(defined($dbh)) {
      $stat = $dbh->prepare($st);
      $stat->execute();
      while ( my($auto_pfamA, $pfamA_acc, $pfamA_id, $description, $count) = $stat->fetchrow) {
	my $seq_st = $dbh->prepare("select distinct auto_pfamseq from genome_seqs where auto_pfamA = '$auto_pfamA' and ncbi_code = '$ncbi'");
	$seq_st->execute();
	my $seq_count = 0;
	while( my($auto_pfamseq) = $seq_st->fetchrow) {
	 # print "$auto_pfamseq\n";
	  $seq_count++;
	}
	

	my $tmp =  $pfamA_acc . "~" . $pfamA_id . "~" . $description . "~" . $seq_count;
#	print "acc: $auto_pfamA , ncbi: $ncbi <BR>";
	my %temp = ('id' => $tmp,
		'count' => $count
		);

	push @domains, \%temp;

      }
    }


  };

  
  @domains = sort { $a->{'count'} <=> $b->{'count'} } @domains;

  foreach (@domains) {
    push @all_regions, $_->{'id'} . "~" .  $_->{'count'};
  }

  my @tmp = reverse @all_regions;
  return @tmp;

}



sub genome_build_cat {
  my($self, $cat, $regions) = @_;
  my($dbh, $stat, $st, @results);
  #print "CAT: $cat <P>";
#  my $st;
  if ($regions) {
    $st = "select auto_genome, description, taxonomy from genome_entry where auto_genome = '$cat' order by description";
  } else {
    $st = "select ncbi_code, species,  num_distinct_regions, num_total_regions, num_proteins , sequence_coverage , residue_coverage from genome_species where grouping like '%$cat%' order by species";
  }
   # my 
## print "ST: $st <P>";
  eval {
    $dbh = $self->_the_RDB->connect();
    if(defined($dbh)) {
      
     # print "st: $st <P>";
      $stat = $dbh->prepare($st);
      $stat->execute();
      while ( my($ncbi_code, $species, $num_distinct_regions, $num_total_regions, $num_proteins , $sequence_coverage , $residue_coverage) = $stat->fetchrow) {
	#print "$ncbi_code, $species, $num_distinct_regions, $num_total_regions, $num_proteins , $sequence_coverage , $residue_coverage <BR>";
	#print "$ncbi_code, $species , $num_distinct_regions, $num_total_regions<P>";
        #print "<P>gen: $auto_genome, $description, $taxonomy <BR>";
        
       # my $new_st = "select species, num_distinct_regions, num_total_regions from genome_species where ncbi_code = '$ncbi_code \n"";
#       print "$new_st <P>";
        #my $new_stat = $dbh->prepare($new_st);
        #$new_stat->execute();
        #my (@sub_results);
        #while ( my($rfam_id, $rfam_acc, $count) = $new_stat->fetchrow) {
#         print "$rfam_id, $rfam_acc, $count <BR>";
        #  push @sub_results, $rfam_id . "~" . $rfam_acc . "~" . $count
        #}
        
        #print $description . "~" . $taxonomy . "<BR>";
#	my $st_prot = "select count(*) from genome_sequences where ncbi_code = '$ncbi_code' and pfam_doms = '1'";
#	my $prot_stat = $dbh->prepare($st_prot);
#	$prot_stat->execute();	
#	my $prot_count = $prot_stat->fetchrow();


#	my $st_prot_no = "select count(*) from genome_sequences where ncbi_code = '$ncbi_code' and pfam_doms = '0'";
#	my $prot_stat_no = $dbh->prepare($st_prot_no);
#	$prot_stat_no->execute();	
#	my $prot_count_no = $prot_stat_no->fetchrow();

        push @results, $ncbi_code  . "~" .  $species . "~". $num_distinct_regions . "~". $num_total_regions . "~" . $num_proteins ."~". $sequence_coverage ."~". $residue_coverage ;

	#print $species . "~". $num_distinct_regions . "~". $num_total_regions . "<BR>";
      }
      
      $stat->finish;
      
      $self->_the_RDB->disconnect;
    }
  };
  

  return @results;

}


sub genome_get_species {
  my($self, $ncbi) = @_;
  my ($dbh, $st, $species);

   # eval {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	$st = "select species from genome_species where ncbi_code = '$ncbi'";
	my $stat = $dbh->prepare($st);
	$stat->execute();
	$species   =  $stat->fetchrow;
	$stat->finish;
      }
	
     
	
      $self->_the_RDB->disconnect;

  return $species
}
sub genome_get_regions {
  my($self, $cat, %regions) = @_;
  my($dbh, $stat, $st, @results, $sql);
  #print "Content-type: text/html\n\n";
  #print "WOW <P>";
  #my $valid_seqs = ">";
  #$valid_seqs = "=" if (!$valid);
  #print "CAT: $cat <BR>";
  #foreach (sort keys %regions) {
  #  print "REG: $_ <BR>";
   # $sql .= " rfam_id = \"$_\" or";
  #}
 my(%genome_store_pfamseq);
  my (@store_sql);

  if (%regions) {
    foreach (sort keys %regions) {
    #print "REG: $_ <BR>";
      push @store_sql, "select pfamseq.pfamseq_id from pfamseq, genome_seqs, pfamA where ncbi_code = '$cat' and  genome_seqs.auto_pfamseq = pfamseq.auto_pfamseq and genome_seqs.auto_pfamA = pfamA.auto_pfamA and pfamA_acc = '$_'";
  }
    

  } else {

    push @store_sql, "select pfamseq.pfamseq_id from pfamseq, genome_seqs where ncbi_code = '$cat' and  genome_seqs.auto_pfamseq = pfamseq.auto_pfamseq";
  }

 
  
  foreach my $st (@store_sql) {
    #print "ST: $st <P>";
    eval {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	
	# print "st: $st <P>";
	$stat = $dbh->prepare($st);
	$stat->execute();
	while ( my($pfamseq_id) = $stat->fetchrow) {
	  $genome_store_pfamseq{$pfamseq_id} = $pfamseq_id;
	  # push @results, $pfamseq_id;
#	  print "PFAMSEQ: $pfamseq_id <P>";
	}
	
	$stat->finish;
	
	$self->_the_RDB->disconnect;
      }
    };
  }
#  print "WW{P <P>";
#foreach (sort keys %genome_store_pfamseq) {
#    print "WOW THE KEY: $_ <BR>";
#  }
  
  return %genome_store_pfamseq;
 # return @results;

}

######################################################################################################
# PDB DETAILS 
######################################################################################################


=head2 get_pdb_details

 Title   : get_pdb_details
 Usage   : 
 Function:
 Returns : 
 Args    : 


=cut

sub get_pdb_details {

  my($self, $pdb_id) = @_;
  my($dbh, $st);
  my(  $header, $title);
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {

        $st = "select header, title from pdb where pdb_id = '$pdb_id' ";
        my($stat) = $dbh->prepare($st);
        $stat->execute();
        
	my($tmp_header, $tmp_title);

        while ( ($tmp_header, $tmp_title) = $stat->fetchrow) {
	 $header = $tmp_header;
	 $title = $tmp_title;
	}
      }


    }

  };

  return $header , $title;
  


}

sub get_jpeg_key_info {

  my($self, $pdb_id) = @_;
  my($dbh, $st);
  my(  $header, $title, @return_vals);
  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {

        $st = "select chain, pdb_start_res, pdb_end_res, pfamA_acc, pfamA_id, hex_colour from pdb, pdbmap, pfamA where  pdb_id = '$pdb_id' and pdb.auto_pdb = pdbmap.auto_pdb and pdbmap.auto_pfam = pfamA.auto_pfamA and pfam_region = '1' order by chain ";
        my($stat) = $dbh->prepare($st);
        $stat->execute();
        
	my($tmp_header, $tmp_title);

        while ( my($chain, $pdb_start_res, $pdb_end_res, $pfamA_acc, $pfamA_id, $hex_colour) = $stat->fetchrow) {
	  push @return_vals, $chain ."~". $pdb_start_res."~". $pdb_end_res ."~".$pfamA_acc ."~".$pfamA_id ."~".$hex_colour;
	}
      }


    }

  };

  return @return_vals;
  


}



=head2 get_Structural_AnnotSeqs

 Title   : get_Structural_AnnotSeqs
 Usage   : ($annot) = $db->get_annotSeqs( $pdb, $chain, $id );
 Function: gets an structural domain for sequence from underlying db
 Returns : Bio::Pfam::AnnotatedSequence 
 Args    : A ref to a list of swisspfam (pfamseq) identifier

=cut


sub get_Structural_AnnotSeqs {
   my ($self, $pdb, $chain, $sdb, $id_list) = @_;
   my ($dbh);
   my $fac = Bio::Pfam::PfamAnnSeqFactory->instance();
   eval {
       $dbh = $self->_the_RDB->connect();
   };
   	if ($@ or not defined( $dbh ) or $self->{'no_rdb'}) { 
       	## Connection to the database failed - get pfama & pfamb from the flatfile   
		}
	else {
		foreach my $in_id (@{$id_list}) {
			#create annotated sequnece object
	   		my $annSeq = $fac->createAnnotatedSequence();
	   		#set the sequence id for the annotated sequnece object
			$annSeq->id( $in_id );
			
			my $q = "select length from pfamseq  where pfamseq_id = ?";
			my $st_length = $dbh->prepare($q);

		   	## Get the length and add to the annotated sequence object
			my($the_length, $length);
			if (defined $st_length) {
			$st_length->execute($in_id);
			while ( ($length) = $st_length->fetchrow) {
			   	$the_length = $length if ($length =~ /[0-9]/);
	       		}
	       	$annSeq->length( $the_length);
	   	   	$st_length->finish;    
		   	}
			
			if ($sdb eq 'SCOP'){
			#get structural domains by querying the SCOP domain SOAP server
			my $tmp_chain;
			if (!$chain){
				$tmp_chain = "_";
				}
			else{
				$tmp_chain = $chain;
				}
			my $tmp_pdb = uc $pdb;
			my $read = 0;
			my $get_region =0;
			my ($sunid, $domain_name);
			#keys will be sunids, valuses will be start~end or 0; 
			my %scop_domains;
			my $soap = SOAP::Lite                                             
			         -> uri('/SCOP_XML')
			          -> proxy('http://131.111.89.223:4242/soap.cgi',
				  proxy => ['http' =>'http://wwwcache.sanger.ac.uk:3128/']	
				  );
			my $result = $soap -> domains('pdbid' => [$pdb],
										  'fields' => ['sid', 'family.name']);
			unless ($result->fault)
				{
			 	foreach(split /\n/, $result->result()){
					if(/<pdbEntry pdbid=\"$tmp_pdb\">/){
					$read++;
					}
				elsif($read && (/<domain sunid=\"(\d+)\"/)){
					$sunid = $1;
					}
				elsif($read && (/<region chain=\"$tmp_chain\"/)){
					my $line = $_;
					if($line =~ /start=\"(\d+)\" stop=\"(\d+)\"/){
						$scop_domains{$sunid}{'region'}= "$1~$2";
						$scop_domains{$sunid}{'name'}= $domain_name;
						}
					else{
						$scop_domains{$sunid}{'region'} = 0;
						$scop_domains{$sunid}{'name'}= $domain_name;
						$get_region++;
						}
					}
				elsif($read && (/<node sunid=\"\d+\">(.*)<\/node>/)){
					$domain_name = $1;
					}
				elsif($read && (/<\/domain>/)){
					$sunid=0;
					$domain_name=0;
					}
				}

				}	
			else
				{
				 my $fcode = $result->faultcode;
				 my $fstr  = $result->faultstring;
 				print STDERR "FAULT: $fcode, $fstr\n";
				}
	
		#get disordered structural regions
		my $st = "select distinct pfamseq_seq_number, pdb_seq_number from msd_data join pdb, pfamseq where pdb.auto_pdb=msd_data.auto_pdb and pfamseq.auto_pfamseq=msd_data.auto_pfamseq and pdb_id=\'$pdb\' and chain=\'$chain\' and pfamseq_id=\'$in_id\'"; 
		my $ordered_region = $dbh->prepare($st);
		$ordered_region->execute();
		#fetchrow
		my @ordered;
		while( (my @ary = $ordered_region->fetchrow) ){
			push(@ordered, \@ary);
			}
		$ordered_region->finish;
		#look for any discontinous regions of structure (continuous region is a an array of 0s).

		my @disordered;
		for(my $n = 0; $n < $the_length; $n++){
			$disordered[$n]=1;
			}
		foreach(sort{$$a[0] <=> $$b[0]} @ordered){
			$disordered[$$_[0] - 1]=0;
			}
			
		my ($ds, $de);
		for(my $m = 0; $m < $the_length; $m++){
			if(($ds and $de) && ($disordered[$m] == 1)){	
				$de=$m + 1;
				}
			elsif(($ds and $de) && ($disordered[$m] == 0)){
				#new disorder region, add to the annotated sequence object.
				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																		 '-FROM' => $ds,
																		 '-TO' => $de,
																		 '-TYPE' => "disordered",
																		 '-SOURCE' => "structure"
																		 )); 
				$de = $ds = 0;
				}
			elsif($disordered[$m] == 1){
				#start new disordered region
				$ds=$de=($m + 1); 
				}
			}
			
			if($de and $ds){
				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																		 '-FROM' => $ds,
																		 '-TO' => $de,
																		 '-TYPE' => "disordered",
																		 '-SOURCE' => "structure"
																		 )); 
				}
		
		#now add the scop domains to the annotated sequnce object.
		if($get_region){
			#get the ordered region, scop domain is the whole chain
			@ordered = sort{$$a[0] <=> $$b[0]} @ordered;

			foreach my $id (keys %scop_domains){
				my $name  = $scop_domains{$id}{'name'} if ($scop_domains{$id}{'name'});	
				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																	 	 '-FROM' => ${$ordered[0]}[0],
																         '-TO' => ${$ordered[$#ordered]}[0],
																	 	 '-TYPE' => "SCOP:$name",
																	     '-SOURCE' => "$id"
																		 )); 
				}
			}
		else{
			#use the scop domain  boundaries, need to be converted from pdb to sequence coos
			foreach my $id (keys %scop_domains){
				#get start/end seqpos
				my ($start, $end);
				my @se = split /~/, $scop_domains{$id}{'region'};
				foreach(@ordered){
					if($se[0] eq $$_[1]){
						$start=$$_[0]
						}
					if($se[1] eq $$_[1]){
						$end=$$_[0]
						}
					}
				my $name = $scop_domains{$id}{'name'};	
				#having got the sequence start end add to the structure
				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																	 	 '-FROM' => $start,
																         '-TO' => $end,
																	 	 '-TYPE' => "SCOP:$name",
																	     '-SOURCE' => "$id"
																		 )); 

				}
			}
		
			}
			elsif($sdb eq 'CATH'){
				#get structural domains by querying the CATH domain cgi server
        		my $ua = new LWP::UserAgent;
        		$ua->agent("Pfam2CATH domainer/1.1");
				$ua->timeout(5);
        		$ua->proxy(http => 'http://wwwcache.sanger.ac.uk:3128'); 
        		my $newreq = new HTTP::Request GET => "http://www.biochem.ucl.ac.uk/cgi-bin/cath/SearchPdb.pl?type=PDB&xml=1&query=$pdb$chain";

        		my $newres = $ua->request($newreq);
				my $cath_xml;
        		my $read = 0;
				my $seg = 0;
				my ($cath_domain, %cath_domains);
        		if($newres->is_success){
					foreach (split /\n/, $newres->content()){
						#Need to parse a load of xml here
						if (/<cath_domain(\s+)domain_id=\"(\S+)\"/){ 
							$read = 1;
							$cath_domain = $2;
							}
						elsif((/>([\d+\.]*)<\/cath_code>/)&& ($read == 1)) {
							$cath_domains{$cath_domain}{'code'}=$1;
							}
						elsif(/seg_num=\"(\d+)\"/ && $read == 1){
							$seg = $1;
							}
						elsif(/pdb_start=\"(\d+)\"/ && $seg){
							$cath_domains{$cath_domain}{'segs'}{$seg}{'start'}=$1;
							}
						elsif(/pdb_stop=\"(\d+)\"/ && $seg){
							$cath_domains{$cath_domain}{'segs'}{$seg}{'stop'}=$1;
							}
						elsif(/<\/cath_domain>/){
							$read = 0;
							$seg = 0;
							$cath_domain = 0;
							}
						}
					}
				else{
					print "CATH domains not retrieved\n";
					}

				#get disordered structural regions
				my $st = "select distinct pfamseq_seq_number, pdb_seq_number from msd_data join pdb, pfamseq where pdb.auto_pdb=msd_data.auto_pdb and pfamseq.auto_pfamseq=msd_data.auto_pfamseq and pdb_id=\'$pdb\' and chain=\'$chain\' and pfamseq_id=\'$in_id\' and pfamseq_seq_number"; 
				my $ordered_region = $dbh->prepare($st);
				$ordered_region->execute();
				#fetchrow
				my @ordered;
				while( (my @ary = $ordered_region->fetchrow) ){
					push(@ordered, \@ary);
					}
				$ordered_region->finish;
				#look for any discontinous regions of structure (continuous region is a an array of 0s).

		my @disordered;
		for(my $n = 0; $n < $the_length; $n++){
			$disordered[$n]=1;
			}
		foreach(@ordered){
			$disordered[$$_[0] - 1]=0;
			}
			
		my ($ds, $de);
		for(my $m = 0; $m < $the_length; $m++){
			if(($ds and $de) && ($disordered[$m] == 1)){	
				$de=$m + 1;
				}
			elsif(($ds and $de) && ($disordered[$m] == 0)){
				#new disorder region, add to the annotated sequence object.
				$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																		 '-FROM' => $ds,
																		 '-TO' => $de,
																		 '-TYPE' => "disordered",
																		 '-SOURCE' => "structure"
																		 )); 
				$de = $ds = 0;
				}
			elsif($disordered[$m] == 1){
				#start new disordered region
				$ds=$de=($m + 1); 
				}
			}
		#end for loop	
		if($de and $ds){
			$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																	 '-FROM' => $ds,
																	 '-TO' => $de,
																	 '-TYPE' => "disordered",
																	 '-SOURCE' => "structure"
																	 )); 
			}
		
		#now add the scop domains to the annotated sequnce object.
		foreach my $id (keys %cath_domains){
				my $name = $cath_domains{$id}{'code'};
				foreach my $seg (keys %{$cath_domains{$id}{'segs'}}){
					my ($start, $end);
					foreach(@ordered){
						if( $cath_domains{$id}{'segs'}{$seg}{'start'} eq $$_[1]){
							$start=$$_[0]
							}
						if(  $cath_domains{$id}{'segs'}{$seg}{'stop'} eq $$_[1]){
							$end=$$_[0]
							}
						}
					#having got the sequence start end add to the structure
					$annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
																		 	 '-FROM' => $start,
																	         '-TO' => $end,
																		 	 '-TYPE' => "CATH:$name",
																		     '-SOURCE' => "$id"
																			 )); 

					}
				}
		
			}
		return ($annSeq);
		}
	}
}



=head2 transform_seq_pos2pdb_pos

=cut

sub transform_seq_pos2pdb_pos{
	my ($self, $pdb, $chain, %seq_pos) = @_;
	my $dbh;
	eval {
       $dbh = $self->_the_RDB->connect();
   		};
   	if ($@ or not defined( $dbh ) or $self->{'no_rdb'}) { 
       	## Connection to the database failed   
		}
	else {
		my @seq_pos = keys(%seq_pos);
		my $query_string = join(" or pfamseq_seq_number=", @seq_pos); 
		my $st = "select distinct pfamseq_seq_number, pdb_seq_number from msd_data join pdb where pdb.auto_pdb=msd_data.auto_pdb and pdb_id=\'$pdb\' and msd_data.chain=\'$chain\' and (pfamseq_seq_number=$query_string)";  
		my $pdb_pos = $dbh->prepare($st);
		$pdb_pos->execute();
		while( (my @ary = $pdb_pos->fetchrow) ){
			$seq_pos{$ary[0]}=$ary[1];
			}
		$pdb_pos->finish;
		return (%seq_pos);
		}
	}




sub species_distribution_for_family {

  my ($self, $family_id) = @_;

  my($dbh, $st);

  my (@list);

  eval {
    if (not $self->{'no_rdb'}) {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {

   	if ($family_id =~ /^Pfam-B_/) {

	  $st = "select distinct pfamseq_id, species, taxonomy from pfamB, pfamB_reg, pfamseq where pfamB_id = '$family_id' and pfamB.auto_pfamB = pfamB_reg.auto_pfamB  and pfamB_reg.auto_pfamseq = pfamseq.auto_pfamseq";
	  
	} else {
	  $st = "select distinct pfamseq_id, species, taxonomy from pfamA, pfamA_reg_full, pfamseq where pfamA_id = '$family_id' and pfamA.auto_pfamA = pfamA_reg_full.auto_pfamA and in_full = '1' and pfamA_reg_full.auto_pfamseq = pfamseq.auto_pfamseq";
	  
	}

        my($stat) = $dbh->prepare($st);
        $stat->execute();
        
	my ($tmp_seq, $tmp_species, $tmp_taxonomy);

        while ( ( $tmp_seq, $tmp_species, $tmp_taxonomy) = $stat->fetchrow) {
	  push @list, "$tmp_seq~$tmp_species~$tmp_taxonomy";

	}
      }


    } else {
      return 0;
    }

  };

 
  return @list;






}


sub query {

    my ($self, $query) = @_;
    my ($dbh, $st, @retlist);

 if ( ($query !~ /^select/i) && ($query !~ /^show/i) &&  ($query !~ /^describe/i) ) {

        $self->throw("DB_RDB::query - only 'select' queries are allowed");
        }

    eval {
        $dbh = $self->_the_RDB->connect();
        $st = $dbh->prepare($query);
        $st->execute();
        while( my @list = $st->fetchrow() ) {
            # print STDERR "$query: $list[0]\n";
            push @retlist, \@list;
        }
        $st->finish;
        
        $self->_the_RDB->disconnect;
    };   
    $@ and $self->throw("DB_RDB::query - error with query '$query' [$@]"); 

    return @retlist;
}




########################################################################
#
#   CONTEXT REGIONS
#
########################################################################



sub context_regions {
  
  my ($self, $acc, $count) = @_;
  my ( $tmp_count);
  
  my ( $dbh, $st);
  
  if ($count) {
    $st = " select count(*) from pfamA, context_pfam_regions where pfamA_acc = '$acc' and context_pfam_regions.auto_pfamA = pfamA.auto_pfamA ";
    
    
    eval {
      $dbh = $self->_the_RDB->connect();
      if(defined($dbh)) {
	
	
	my($stat) = $dbh->prepare($st);
	$stat->execute();
	while ( ($tmp_count) = $stat->fetchrow) {
	  $count = $tmp_count;
	  
	}
	
	$stat->finish;
	
	$self->_the_RDB->disconnect;
      }
    };
    
    
    return $count;
    
  }				# end if count
  
  my (@domains, %pfamseq);
  eval {
    if (not $self->{'no_rdb'}) {
      
      $dbh = $self->_the_RDB->connect();
      
      if(defined($dbh)) {
	
	$st = "select pfamseq_id from  pfamseq, pfamA, context_pfam_regions where pfamA_acc = '$acc' and pfamA.auto_pfamA = context_pfam_regions.auto_pfamA and context_pfam_regions.auto_pfamseq = pfamseq.auto_pfamseq  ";
	
	my($stat) = $dbh->prepare($st);
	$stat->execute();
	my ($pfamseq_id);
	while (($pfamseq_id) = $stat->fetchrow) {
	  
	  if (defined($pfamseq{$pfamseq_id})) {
	    
	  } else {
	    $pfamseq{$pfamseq_id} = $pfamseq_id;
	    push @domains, $pfamseq_id;
	  }
	  
	}
	
	
      }
    } else {
      
      
    }
  };
  
  return @domains;

} #/ end CONTEXT REGIONS

######################################################################################################
############################ Internal methods
######################################################################################################


=head2 _the_RDB

 Title   : _the_RDB
 Usage   : $rdb = $self->_the_RDB();
 Function:
    Gets/sets the underlying database object for this query layer
 Returns : A object that provides basic connection facilities to the RDB
 Args    : A object that provides basic connection facilities to the RDB (optional)

=cut

sub _the_RDB {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'_the_rdb'} = $value;
   }
   return $self->{'_the_rdb'};
}

