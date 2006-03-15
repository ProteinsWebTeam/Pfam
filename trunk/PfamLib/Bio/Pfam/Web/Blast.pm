package QueueStub::Blast;
#########
# Author: rmp
#
use Website::QueueStub;

use strict;
use vars qw(@ISA);
@ISA = qw(Website::QueueStub);

sub preinit {
  my ($self) = @_;
   push @{$self->{'_fields'}}, (qw(omnitickets database repeatmask querynumber blastexe blastopts hashpath blastdb nqueries));
 # push @{$self->{'_fields'}}, (qw(omnitickets database repeatmask blastexe blastopts hashpath blastdb));
}

sub postinit {
  my $self    = shift;
  my $hp      = $self->hashpath() || "";
  my $id      = $self->id()       || "";
  my $tracker = $self->tracker()  || "";
  my $lwd;

  #########
  # Hashpath but no id
  #
  if($hp && !$id) {
    my ($lwd, $id) = $hp =~ m|^(.*)/([^/]+)$|;
    $self->id($id);
    $self->lwd($lwd);
  }

  #########
  # tracker but no id
  #
  if($tracker && !$id) {
    my ($lsfid, $blastid) = split(':', $tracker);
    $self->id($blastid);
  }

  #########
  # figure out database given an id
  #
}

sub presubmit {
  my $self = shift;
  my @jobs = @{$self->omnitickets()||[]};
  my $rm   = $self->repeatmask()  || "";
  my $bo   = $self->blastopts()   || "";
  my $nq   = $self->nqueries()    || "";
  my $id   = $self->id();
 # $self->twd("/data/jobs");
  if($rm && $rm ne "n" && $rm ne "no" && $rm ne "off") {
#    my $rmid = Website::Utilities::IdGenerator->get_unique_id();
#    print STDERR "SUBMIT THE  COMMAND: qq(/data/bin/hmmpfam -E 1.0 -Z 7677 /data/blastdb/Pfam/data/Pfam_ls.bin $id.tmp ), lwd: " .$self->lwd() . " , ID: $id.tmp  RMID: $rmid <P>\n";
#    $self->debug() and print STDERR qq(Submitting repeatmasker job $rmid\n);
#    my $rmqs = Website::QueueStub->new({
#					'id'        => $rmid,
##					'command'   => qq(rm -rf $id.tmp.* ; /usr/bin/perl /data/bin/RepeatMasker $id.tmp; mv $id.tmp.masked $id.tmp),
##					'command'   => qq(rm -rf $id.tmp.* ; RepeatMasker $id.tmp; mv $id.tmp.masked $id.tmp; rm -f $id.tmp.*;),
### rm -rf $id.tmp.* ; RepeatMasker $id.tmp
#					'command'   => qq(/data/bin/hmmpfam -E 1.0 -Z 7677 /data/blastdb/Pfam/data/Pfam_fs.bin $id.tmp ),
#					'lwd'       => $self->lwd(),
#					'pushfiles' => ["$id.tmp"],
##					'pullfiles' => ["$id.tmp"],
#					'priority'  => 'high',
#				       });
#    my $tracker = $rmqs->submit();
#    $self->debug() and print STDERR qq(Repeatmasker job tracker is '$tracker'\n);

#    $self->wait("ended($rmid)");

  } else {
    #########
    # If we used repeatmasker then the query sequence is already on the target host
    # if not then we still need to push it
    #
    $self->pushfiles(["$id.tmp"]);  
     my $nqueries = $self->nqueries();
    if($nqueries) {
      $self->pushfiles([map { "$id.$_.tmp" } (1..$nqueries)]);
    }
  }

  #########
  # general setup
  #
  $ENV{'BLASTDB'} = $self->blastdb() || "/data/blastdb";
  $ENV{'COILSDIR'} = "/data/bin/coils";
 
  if(@jobs) {
   # print STDERR "GOT JOBS " . $self->lwd() . " , " . $self->twd() . " <P>";
   my $i = 0;
   my @pullfiles = ();
   $self->array([map {
     $i++;
      my $qn   = $_->{'querynumber'} || "";
     push @pullfiles, "$id.$i";
    # push @pullfiles, "$id.$i";
     
     my $be   = $_->{'blasttype'}   || "";
     my $db   = $_->{'database'}    || "";
     my $crc = $_->{'crc64'}    || "";
     my $mode = $_->{'mode'}    || "";
     my $pfamBcrc = $_->{'pfamBcrc'}    || "";
     my $dnasearch  = $_->{'dnasearch'}    || "";
     my $rfam  = $_->{'rfam'}    || "";
     my $hmmfetch;

     my $push_dir = $_->{'dir'};
    
     if ($crc) {
       $db = "$id" . "hmmsub" . "_" . $mode;
       my(@pfamA_ids) = split(/~/, $crc);
      # my(%all_pfam_ids) = &PfamWWWConfig::get_protein_by_crc64($crc);
       foreach (@pfamA_ids) { #(sort keys %all_pfam_ids) {
	 $hmmfetch .= "/data/bin/hmmfetch /data/blastdb/Pfam/data/Pfam_" . $mode . " $_ >> $db;";
       }

       push @pullfiles, $db;
     }
     my $params = " --cpu 1  -E 1.0 -Z 7677 ";
     $params = " " if ($db =~ /Pfam\-B\.fasta/);
     my $command = "$hmmfetch $be  $params $db  $id.tmp > $id.$i";
     if ($be =~ /seg/) {
       $command = "$be $id.tmp -l > $id.$i ";
     }
     if ($be =~ /ncoils/) {
       $command = "$be  -f < $id.tmp > $id.$i";
     }
     if ($pfamBcrc) {
       $command = "printf $pfamBcrc > $id.$i";
     }
     #print "Content-type: text/html\n\n";
     if ( ($dnasearch) || ($rfam) ) {
       $command = "$be $id.tmp > $push_dir" . "$id.$i";
     #  print "COMMAND: $command <P>";
     }
   #  print "EEP $command /usr/bin/scp $id.$i XXX_subhost_XXX:XXX_lwd_XXX  <P>";
   #  print STDERR "COMMAND $command; /usr/bin/scp $id.$i XXX_subhost_XXX:XXX_lwd_XXX<P>";
   #  print STDERR "COMMAND: $command  <P>";# if ($command =~ /ncoils/);
   #  print STDERR "COMMAND: $be  -E 1.0 -Z 7677 $db $id.tmp $bo > $id.$i; lsrcp $id.$i XXX_subhost_XXX:XXX_lwd_XXX<P>";
     my $output = $push_dir . "$id.$i";
     {
       'command' => "$command; /usr/bin/scp $output XXX_subhost_XXX:XXX_lwd_XXX",

     };
   } @jobs]);

##  'command' => "$be  -E 1.0 -Z 7677 $db $id.tmp $bo > $id.$i; lsrcp $id.$i XXX_subhost_XXX:XXX_lwd_XXX",

#   $self->stdout("$id.%I");
   $self->debug() and print STDERR qq(Submitting array of @{[scalar @{$self->array()}]} jobs\n);
   $self->pullfiles([@pullfiles]);

  } else {
    my $be   = $self->blastexe()    || "";
    my $db   = $self->database()    || "";
    my $cmd  = "";

    $cmd .= "$be $db $id.tmp $bo";
    $self->command($cmd);
  }
}

## /usr/local/lsf/bin/bsub -I -q offline /data/bin/wublastp /nfs/disk100/pubseq/blastdb/Pfam/data/Pfam-B.fasta /nfs/WWW/SANGER_docs/htdocs/tmp/blast/116/HC/00/116HC00A97m39T698305Q.tmp 

1;

