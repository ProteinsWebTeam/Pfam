
#
# Perl Module for RfamWWWConfig
#
# Written by Ewan Birney <birney@sanger.ac.uk>
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package RfamWWWConfig;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use FileHandle;
use drawing;

#use Bio::Rfam::SeqRfam;
use Rfam::DB::DB_Web;

no strict qw( vars );

$is_internal_site = 0;

$no_lsf = '/nfs/WWW/NOLSF';
$no_rdb = '/nfs/WWW/htdocs/Software/Rfam/NORDB';

# formatting config

$rfamcolour = '#EFC779';

# For the middleware layer configuration :

$rfam_root_dir = '/nfs/disk100/pubseq/Rfam';
$rfam_current_dir = "$rfam_root_dir/CURRENT";
$getz = '/usr/local/pubseq/bin/getz';
$bsub = '/usr/local/lsf/bin/bsub';

$rdb_name = 'rfam';
$rdb_driver = 'mysql';
$rdb_host = 'pfam';
$rdb_user = 'rfam';
$rdb_password = 'mafp1';

$file_root = '/nfs/WWW/htdocs/Software/Rfam';
$data_root = "/nfs/WWW/htdocs/Software/Rfam/data";

$site       = 'http://www.sanger.ac.uk';
$WWW_root   = "$site/Software/Rfam";
$cgibin     = "/cgi-bin/Rfam";
$local_root = "/Software/Rfam";
$srsserver  = "http://srs.sanger.ac.uk/srsbin/cgi-bin/wgetz?-noSession+-e+[EMBLRELEASE-ACC:THEACC]";

#image stuff

$image_dir  = "$file_root/gifs";
$image_link = "$WWW_root/gifs";
$image_temp = "$WWW_root/temp";
$tempdir  = "$file_root/temp";

# Extra images

$space   = "$image_link/space.gif";
$overlap = "$image_link/overlap2.gif";
$blank   = "$image_link/blank2.gif";






# stuff for database searches




# script names

$getalignment		= "$cgibin/getalignment.pl";
$membersequences	= "$cgibin/membersequences.pl";
$mir_submit		= "$cgibin/mir_submit.pl";
$queryrfam		= "$cgibin/queryrfam.pl";
$seqget			= "$cgibin/seqget.pl";
$speciesdist		= "$cgibin/speciesdist.pl";
$speciesview		= "$cgibin/speciesview.pl";
$getacc         	= "$cgibin/getacc";

# contact point 

$contact   =  '<a href="mailto:rfam@sanger.ac.uk">Rfam</a>';
$email     =  'rfam@sanger.ac.uk';
$adminname =  'Rfam';

#headers and footers

$header_file = "$file_root/rfam_cgi_external.header";
$java_header_file = "$file_root/" . "javascript_" . "rfam_cgi_external.header";
$footer_file = "$file_root/rfam.footer";



# Methods....



=head2 link_mapper

 Title   : link_mapper
 Usage   : 
   $ref = $RfamWWWConfig->link_mapper;
 Function: 
   Returns a reference to a hash of database links. The hash is 
   indexed by database name, and each entry is a list reference,
   each element of which is a hash reference with three fields:
     home : url for the database home page
     link : urls to link to entries, with '$id' in 
            the place of the identifier
     id   : an id for the database
 Notes:
   The first element in each list is the primary link (the one to give 
   when only one is needed. When the lists contains more than one element,
   they each need to have an identifier (corresponding to author or
   location)

=cut

sub link_mapper {
    my $mapper = {
        'SWISSPROT' => 
	    [{'home' => 'http://www.expasy.ch/swissprot',
	      'link' => 'http://www.expasy.ch/cgi-bin/niceprot.pl?$acc',
	      'id'   => 'SWISS-PROT',
		'INTERPRO-LINK' => 'http://srs6.ebi.ac.uk/srs6bin/cgi-bin/wgetz?-newId+-e+[SWall-ACC:$id]',
		}
	    ],
        'PROSITE' => 
	    [{'home' => 'http://www.expasy.ch/prosite',
	      'link' => 'http://www.expasy.ch/cgi-bin/nicedoc.pl?$id',
	      'id'   => 'Expasy'},
	     {'link' => 'http://www.sanger.ac.uk/srs5bin/cgi-bin/wgetz?-e+[PROSITEDOC-ID:$id]',
	      'id'   => 'SRS-UK'},
	     {'link' => 'http://iubio.bio.indiana.edu:81/srs//srsc?[PROSITEDOC-id:$id]',
	      'id'   => 'SRS-USA'}
	    ],
        'PROSITEDOC' => 
	    [{'home' => 'http://www.expasy.ch/prosite',
	      'link' => 'http://www.expasy.ch/cgi-bin/nicedoc.pl?$id',
	      'id'   => 'Expasy',
	      'INTERPRO-LINK' => 'http://www.expasy.ch/cgi-bin/nicedoc.pl?$id',
		},
	     {'link' => 'http://www.sanger.ac.uk/srs5bin/cgi-bin/wgetz?-e+[PROSITEDOC-ID:$id]',
	      'id'   => 'SRS-UK'},
	     {'link' => 'http://iubio.bio.indiana.edu:81/srs//srsc?[PROSITEDOC-id:$id]',
	      'id'   => 'SRS-USA'}
	],
	'EC' =>
	    [{'home' => 'http://www.expasy.ch/enzyme',
	      'link' => 'http://www.expasy.ch/cgi-bin/nicezyme.pl?$id',
	      'id'   => 'ENZYME',
		'INTERPRO-LINK' => 'http://srs6.ebi.ac.uk/srs6bin/cgi-bin/wgetz?-e+[enzyme-ECNumber:$id]'}
	    ],
	'RFAM' =>
	    [ { 'INTERPRO-LINK' => '/cgi-bin/Rfam/getacc?$id'
		}
	 ],
	'MEDLINE' =>
	    [ { 'INTERPRO-LINK' => 'http://www.ncbi.nlm.nih.gov/htbin-post/Entrez/query?uid=$id&form=6&db=m&Dopt=b' 
		}
	 ],
	'NCBITAXON' =>
	    [ { 'INTERPRO-LINK' => 'http://www.ncbi.nlm.nih.gov/htbin-post/Taxonomy/wgetorg?lvl=0&id=$id' 
		}
	 ],
	'SCOP' =>
	    [{'home' => 'http://www.biochem.ucl.ac.uk/bsm/pdbsum',
	      'link' => 'http://www.biochem.ucl.ac.uk/bsm/pdbsum/PDBID/main.html',
	      'id'   => 'CATH-PDBSUM'},
	     {'home' => 'http://scop.mrc-lmb.cam.ac.uk/scop',
	      'link' => 'http://scop.mrc-lmb.cam.ac.uk/scop/search.cgi?pdb=PDBID&tlev=$add',
	      'id'   => 'SCOP-UK'},
	     {'home' => 'http://scop.berkeley.edu',
	      'link' => 'http://scop.berkeley.edu/search.cgi?pdb=PDBID&tlev=$add',
	      'id'   => 'SCOP-USA'},
	     {'home' => 'msd.ebi.ac.uk',
	      'link' => 'http://oca.ebi.ac.uk/oca-bin/ocaids?id=PDBID',
	      'id'   => 'MSD'}
	    ],
	'PRODOM' =>
	    [{'home' => 'http://protein.toulouse.inra.fr/prodom.html',
	      'link' => 'http://www.toulouse.inra.fr/prodom/cgi-bin/ReqProdomII.pl?id_dom1=$id',
	      'id'   => 'INRA'},
	     {'link' => 'http://www.sanger.ac.uk/srs5bin/cgi-bin/wgetz?-e+[PRODOM-PAC:$id]',
	      'id'   => 'SRS-UK'}
	    ]
    };

    return $mapper;
}





=head2 get_database

 Title   : get_database
 Usage   : $db = $RfamWWWConfig::get_database;
 Function: 
    All access to the underlying database is via this object.
    The method is called mainly by other methods in this module
    but now and again by other modules
 Returns : Bio::Rfam::DB
 Args    : None


=cut

sub get_database {

    if (not defined $RfamWWWConfig::the_database) {
       my $loc_no_rdb = (-e $no_rdb)?1:0;
        $RfamWWWConfig::the_database = 
	    Rfam::DB::DB_Web->new('-current' => $rfam_current_dir,
				  '-attic' => $rfam_attic_dir,
			          '-index' => $rfam_index_file,
                                  '-lock_file' => $rfam_lock_file,
                                  '-datadir' => $data_root,
                                  '-srsdb' => $srs_db,
                                  '-srsrfam' => $srs_rfam,
				  '-srsrfamb' => $srs_rfamB,  
				  '-srsswissrfam' => $srs_swissrfam,
				  '-getz' => $getz,
				  '-db_name' => $rdb_name,
				  '-db_host' => $rdb_host,
				  '-db_driver' => $rdb_driver,
				  '-db_user' => $rdb_user,
				  '-db_password' => $rdb_password,
				  '-no_rdb' => $loc_no_rdb);

   }

   return $RfamWWWConfig::the_database;

}


	

=head2 get_alignment

 Title   : get_alignment
 Usage   : $aln = $RfamWWWConfig::get_alignment('PF00003', 'full');
 Function: 
   Returns a SimpleAlign object for the given Rfam accession number
 Returns : Bio::SimpleAlign
 Args    : 
   1. Accession number
   2. Codeword for alignment needed:
       'seed': seed alignment
       'full': full alignment


=cut




sub get_alignment {
    my $acc = shift;
    my $type = shift; # either 'full' or 'seed'
    my $is_unheaded =shift;
    my $get_rfamB_coloured = shift;
    my $ali;

    my $db = &get_database();


    eval {    
     
      $entry = $db->get_Entry_by_acc( $acc ); 
      $ali = ($type =~ /full/i)?$entry->full():$entry->seed();	
    };

    return $ali;

}

sub get_sequence {
  my ($key, $type) = @_;
  my $db = &get_database();
  my $seq;
  ($seq) = $db->get_Seq( [$key], $type  );

  return $seq;


}


sub user_error {
    my $mess = shift;
    my $noheader = shift;

    if( defined $noheader ) {
        print "Content-type: text/html\n\n";
    }
    print &RfamWWWConfig::header("Error");
    print "<center><h3>$mess</h3></center><p>";

    print &RfamWWWConfig::footer();
    exit(0);
}

sub species_for_rfamseq {
  my ($acc) = @_;
  my ($species);
  my $db = &get_database();
  eval {
    $species = $db->rfamseq_species( $acc );
  };
  
  return $species;

}

sub new_rfam_search {
  my ($search, @terms) = @_;

  my $db = &get_database();
  my (@results);
  eval {
    (@results) = $db->search_rfam( $search, @terms );
  };
  
  return @results;

}

sub array {
  my (%arr) = @_;
  foreach (sort keys %arr) {

    print "KEY: $_ , VAL: " . $arr{$_} . "<BR>";

  }


}

sub domain_species {
        
  my($id) = @_;
  my @domain_species;

    my $db = &get_database();
    eval {
       @domain_species = $db->species_distribution_for_family( $id );
    };




    return @domain_species;
}

sub species_get_sequence {
  my ($seq_id, $acc) = @_;
  my $db = &get_database();
  my $seq;
  ($seq) = $db->species_get_seq( $seq_id, $acc );

  return $seq;


}



=head2 header

 Title   : header
 Usage   : $RfamWWWConfig::header("The title of the page");
 Function: 
   Prints the website page header followed by the given title for
   the page
 Returns :
 Args    : The title for the page


=cut


sub header {
  my($title, $rfam_id , $rfam_acc_num, $url_atributes) = @_;
  
  my ($varhash,$outstr);
  
  $varhash{'title'} = $title;
  $varhash{'root'}  = $local_root;
	$varhash{'cgiroot'}  = $cgibin;
  $varhash{'site'}  = $site;
  $varhash{'email'} = $email;
  $varhash{'admin'} = $adminname;
  $varhash{'rfam_acc_num'} = $rfam_acc_num;
  
  $varhash{'gif2'} = "gifs/blank.gif";
  $varhash{'gif'} = "gifs/blank.gif";
  $varhash{'taxonomy'} = "tql_small_tab.gif";
  $varhash{'tsearch'} = "tsearch_small_tab.gif";
  $varhash{'genomes'} = "genomes_small_tab.gif";	
  $varhash{'search'} = "search_small_tab.gif";
  if($title =~ /alignment/i) {
    $varhash{'rfamcss'} = "rfam_align.css";
  } else {
    $varhash{'rfamcss'} = "rfam.css";
  }
  
  &create_family_header_tag($rfam_acc_num, $rfam_id) if ($rfam_acc_num);
  
  
  if ($title =~ /Taxonomy/i) {
    $varhash{'taxonomy'} = "tql_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";
    
  } elsif  ( ( ($title =~ /sequence/i) && ($title =~ /results/i) )  || ( $title =~ /Embl entry/i) )  {
    
    $varhash{'search'} = "search_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";
    
  } elsif ( ($title =~ /query/i) || ($title =~ /search/i) ) {
    $varhash{'tsearch'} = "tsearch_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";
    
    
  } elsif ($title =~ /embl entry for/i) {
    
  } elsif ($title =~ /web-log/i) {
    
  } elsif ($title =~ /error/i) {
    
  } elsif ($title =~ /schema/i) {
    
  }  elsif ($title =~ /genome/i) {  
    $varhash{'genomes'} = "genomes_large_tab.gif";
    
  
    
  }  elsif ( ($title !~ /struct/i) && ($title !~ /SwissRfam/i) && ($title !~ /mistake/i) )  {
    
    $varhash{'url'} = "getacc?$rfam_acc_num";
    $varhash{'gif'} = "temp/new_gifs/". $rfam_acc_num . "_large.gif";
    
    
  } elsif ($title =~ /swissrfam/i) {
    if ($rfam_acc_num) {
      $varhash{'url'} = "getacc?$rfam_acc_num";
      $varhash{'gif'} = "temp/new_gifs/". $rfam_acc_num . "_small.gif";
      
      $varhash{'url2'} = "swissrfamget.pl?$url_atributes";
      $varhash{'gif2'} = "gifs/page_header/swissrfam_large_tab.gif";
      
    } else {
      $varhash{'url'} = "swissrfamget.pl?$url_atributes";
      $varhash{'gif'} = "gifs/page_header/swissrfam_large_tab.gif";
    }
    
    
  }  elsif ($title =~ /structural/i) {
    
    $varhash{'url2'} = "structural_view.pl?$url_atributes";
    $varhash{'gif2'} = "gifs/page_header/structure_large_tab.gif";
    
    if ($rfam_acc_num) {
      $varhash{'url'} = "getacc?$rfam_acc_num";
      $varhash{'gif'} = "temp/new_gifs/". $rfam_acc_num . "_small.gif";
      
    }
    
  } 
  
  $header_file = $header_file if ($javascript);
  
  open(HEAD,$header_file) || die "Could not open $header_file $!";
  while(<HEAD>) {
    # print "<pre>$_ </pre><BR>";
    while( $_ =~ /\$(\w+)/g ) {
      
      ## TAKEN OUT FOR NOW !
      $var = $1;
      #   print STDERR "Got $var as a subs [$varhash{$var}]\n";
      $temp = $varhash{$var};
      # print "VAR: $var <BR>";
      if (defined($varhash{$var})) {
	#    print "TEMP: $temp :: var: $var<P>";
	s/\$$var/$temp/;	# here it is...
      }
      
    }
    $outstr .= $_;
  }
  close(HEAD);
  
  
  return $outstr;
}


sub get_member_seqs {
    my ($acc, $type) = @_;
  my $db = &get_database();
  my $seq;
  ($seqs) = $db->get_sequences_for_acc( $acc, $type  );
#    print "RETURN $seqs <P>";
  return $seqs;


}


sub search_rfam {
  
  my $terms = shift;
  
  

   my (@rfam_accs);
   $db = &get_database();
   (@rfam_accs) = $db->search_rfam( $terms);

   return @rfam_accs;

}


sub genome_species_name {
  my($auto_genome) = @_;
  my $db = &get_database();
   my($genome_species_name) = $db->get_genome_name($auto_genome );

   return $genome_species_name;

}

sub genome_category {
  my($cat, $regions) = @_;
  my $db = &get_database();
   my(%genome_cat_results) = $db->genome_build_cat($cat, $regions );

   return %genome_cat_results;

}

sub genome_regions {
  my($cat, %regions) = @_;
  my $db = &get_database();
  my(@genome_regions) = $db->genome_get_regions($cat, %regions );
  
   return @genome_regions;

}

sub seq_from_lines {
    my ($lines) = @_;

    my ($line, $name, $seqc);

    # find the first blank line. Probably a neater way to do this in perl
    for($line = shift @$lines; defined $line ;$line = shift @$lines ) {
        if( $line =~ /\S/ ) {
            last;
        }
    }
    if( !defined $line ) {
        return undef;
    }



    if( ($line =~ /^\s*\>\s*(\S*)/) ) {
        $name = $1;
        if( $name =~ /^\s*$/ ) {
            $name = "UserSeq";
        }
        $seqc = "";
    } else {
        $name = "UserSeq";
        $seqc = $line;
    }

    $seqc .= join("", @$lines);


    $seqc =~ tr/[a-z]/[A-Z]/;
    $seqc =~ s/\s//g;
    $seqc =~ s/\d//g;

    # Now we should contain protein chars and nothing else

   

    if ($seqc =~ /\>/) {
      &user_error("There seems to be more than one sequence submmitted. <P>Please only submit <font color=#ff0000>ONE</font> at a time");
    }
    if ($seqc =~ /[^ACGTUMRWSYKVHDBXN]/) {
	&user_error("The supplied sequence contains some non-protein symbols");
    }
        
    my $max_length = 2000;
    if (length($seqc) > $max_length) {
      &user_error("Your sequence is to long. <BR> <font size=-1>You can search large sequences against Rfam by mailing them to:</font> <a href=\"mailto:rfam-admin\@sanger.ac.uk\"><font color=\"000070\" size=2><B>rfam-admin\@sanger.ac.uk</B></font></a>");
    }




    return $seqc;
}

=head2 footer

 Title   : footer
 Usage   : $RfamWWWConfig::footer("The title of the page");
 Function: 
   Prints the website page footer
 Returns :
 Args    : The title for the page


=cut

sub footer {
  my $title = shift;

  # ignore the title!
  my ($varhash,$outstr);
  
  #pick up the header file, set up the variables
  
    
  open(HEAD,$footer_file) || die "Could not open $footer_file $!";
  while(<HEAD>) {
    while( $_ =~ /\$(\w+)/g ) {
    }
    $outstr .= $_;
  }
  close(HEAD);
  
  
  return $outstr;
  
  #  return "<hr>If you think there is anything wrong with this script, please contact $contact\n";
}



sub logs {

  my ($message, $seq) = @_;
  
  
  open(_LOG, ">>$tempdir/stats/weblog") or print "TEMPDIR CANNA OPEN $tempdir/stats/web log $!\n";
  
  my($sec, $min, $hour, $mday, $mon, $year) = (localtime) [0, 1, 2, 3, 4, 5];
  $mon++;
  print _LOG "$mday/$mon/2003 $hour:$min:$sec ".  $ENV{'REMOTE_HOST'} ." " . $ENV{'REMOTE_ADDR'} . " " . $ENV{'USER_AGENT'} . " $message \n";
  
  close(_LOG);

  if ($seq) {
    
    open(_LOG, ">>$tempdir/stats/sequences") or print "TEMPDIR CANNA OPEN $tempdir/stats/sequences $!\n";
    
    print _LOG ">$mday/$mon/2003 $hour:$min:$sec ".  $ENV{'REMOTE_HOST'} ." " . $ENV{'REMOTE_ADDR'} . " " . $ENV{'USER_AGENT'} . "\n";
    $seq=~ s/(.{1,60})/$1\n/g;
    print _LOG "$seq";
    close(_LOG);
  }

}





1;  # says use was ok

__END__
