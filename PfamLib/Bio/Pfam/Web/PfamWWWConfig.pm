#########
# Author: mm1
#
# Perl Module for Bio::Pfam::Web::PfamWWWConfig
#
# Written by Ewan Birney <birney@sanger.ac.uk>
#
# Cared for by Mhairi Marshall <mm1@sanger.ac.uk>
#
# Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package Bio::Pfam::Web::PfamWWWConfig;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use FileHandle;

use Bio::Pfam::SeqPfam;
use Bio::Pfam::DB_Web;
use Bio::SeqIO;

no strict qw( vars );

$is_internal_site = 0;



# formatting config

$pfamcolour = '#FFFFCC';

# For the middleware layer configuration :

$srs_db = 'pfamseq';
$srs_pfam = 'pfam';
$srs_pfamB = 'pfamb';
$srs_swisspfam = 'swisspfam';

$rdb_name = 'pfam_19_0_web';
$rdb_driver = 'mysql';
$rdb_host = 'pfam';
$rdb_user = 'pfam';
$rdb_password = 'password';

($doc_root) = $ENV{'DOCUMENT_ROOT'} =~ m|([a-z0-9_/\.]+)|i;
$file_root = "$doc_root/Software/Pfam";
$data_root = "$file_root/data";
$jtml_root = "$data_root/jtml/";
$no_rdb = '$file_root//nfs/WWW/htdocs/Software/Pfam/NORDB';

$site       = "http://$ENV{'HTTP_X_FORWARDED_HOST'}/";
$WWW_root   = "$site"."Software/Pfam";
$cgibin     = "/cgi-bin/Pfam";
$local_root = "/Software/Pfam";
$jtml_www = "$WWW_root/data/jtml/";



$align_help    = "$WWW_root/help/alignments.shtml";
$stable_link  = "$WWW_root/help/othersite.shtml";
$hall_of_fame  = "$WWW_root/help/hall_of_fame.shtml";
$region_help = "$WWW_root/help/region_help.shtml";

#image stuff

$image_dir  = "$file_root/gifs";
$image_link = "$WWW_root/gifs";
$image_temp = "$WWW_root/temp";
$image_ext = '.png';
$domain_gfx = "/temp/domain_gfx";

# Extra images

$space   = "$image_link/space.gif";
$overlap = "$image_link/overlap2.gif";
$blank   = "$image_link/blank2.gif";


%image_names = ( 'overlap' => "$image_link/overlap2.gif",
		 'sig_p' =>	"$image_link/sig_p.gif",
		 'transmembrane' =>	"$image_link/transmembrane.gif",
		 'low_complexity' =>	"$image_link/low_complexity.gif",
		 'coiled_coil' =>	"$image_link/coiled_coil.gif"
		);




# stuff for database searches

$tempdir  = "$file_root/temp";
$tempwww = "$WWW_root/temp";
$decypher = "$tempdir/dycpher";
$batch_search_dir = "$tempdir/batch";

$hmmlibs_path = '/data/blastdb/Pfam/data';
$treemaker = 'bsub -I -q fast clustalw /kimura';

# stuff for taxonomy query
$pnh = "$data_root/domain.pnh";

# stuff for jalview, nifas and rasmol script

$nifasjar = "$WWW_root/nifas.jar";
$jaljar    = "$WWW_root/jalview.jar";
$mailserver = "www.sanger.ac.uk";
$srsserver  = "www.sanger.ac.uk/srs5bin/cgi-bin/";
$basedb   = "pfamseq";
$pdbmap = "$data_root/pdbmap";
$featurefile = "$data_root/featurefile";
$structure_mapping = "$data_root/astral_pdb_map.dat";


# script names

$getacc		= "$site" . "$cgibin/getacc";
$entry2PSIMI = "$cgibin/entry2PSIMI.pl";
$swisspfam	= "$cgibin/swisspfamget.pl";
$getalign	= "$cgibin/getalignment.pl";
$extraalign     = "$cgibin/getalnform.pl";
$getpfamb	= "$site" . "$cgibin/pfambget.pl";
$search		= "$cgibin/nph-search.cgi";
$taxonomy	= "$cgibin/tql.pl";
$text_s		= "$cgibin/qquerypfam.pl";
$getall		= "$cgibin/getallproteins.pl";
$annotateFam	= "$cgibin/annotateFamily.pl";
$align2seed	= "$cgibin/align2seed.pl";
$addcomments    = "$cgibin/comments.pl";
$nifasgifs      = "$cgibin/nifas_images.pl";
$gettree        = "$cgibin/gettree";
$getstructure	= "$cgibin/pdb2rasmol.pl";
$pfamstructure	= "$cgibin/structural_view.pl";
$scop_cath_pfam = "$cgibin/structural_domains2pfam.pl";
$getspecies     = "$cgibin/speciesdist.pl";
$viewspecies    = "$cgibin/speciesview.pl";
$pnhsearch      = "$cgibin/pnh-search.pl";

# contact point 

$contact   =  '<a href="mailto:pfam@sanger.ac.uk">Pfam</a>';
$email     =  'pfam@sanger.ac.uk';
$adminname =  'Pfam';

#headers and footers

$header_file = "$file_root/pfam_cgi_external.header";
$java_header_file = "$file_root/" . "javascript_" . "pfam_cgi_external.header";
$footer_file = "$file_root/pfam.footer";


# structural scop link
$scop_structure_link = "http://scop.mrc-lmb.cam.ac.uk/scop/search.cgi";

# Methods....



=head2 link_mapper

 Title   : link_mapper
 Usage   : 
   $ref = $Bio::Pfam::Web::PfamWWWConfig->link_mapper;
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
	      'link' => 'http://www.ebi.uniprot.org/entry/$acc',
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
#	     {'link' => 'http://iubio.bio.indiana.edu:81/srs//srsc?[PROSITEDOC-id:$id]',
#	      'id'   => 'SRS-USA'}
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
	      'INTERPRO-LINK' => 'http://www.ebi.ac.uk/intenz/query?cmd=SearchEC&ec=$id'}
	    ],
# these were reported as broken by a user
# JT 20060106 WTSI
# 	'TC' =>
# 	    [{'home' => 'http://saier-144-164.ucsd.edu/',
# 	      'link' => 'http://saier-144-164.ucsd.edu/tcdb/index.php?tc=$id',
# 	      'id'   => 'TRANSPORT',
# 	      'INTERPRO-LINK' => 'http://tcdb.ucsd.edu/tcdb/subfamily.php?tc=$id'}
# 	    ],
	'TC' =>
	    [{'home' => 'http://www.tcdb.org/tcdb/index.php',
	      'link' => 'http://www.tcdb.org/tcdb/index.php?tc=$id',
	      'id'   => 'TRANSPORT',
	      'INTERPRO-LINK' => 'http://www.tcdb.org/tcsb/subfamily.php?tc=$id'}
	    ],
	'PIRSF' =>
	    [{'home' => 'http://pir.georgetown.edu/pirwww/aboutpir/aboutpir.html',
	      'link' => 'http://pir.georgetown.edu/cgi-bin/ipcSF?id=PIRSF000478',
	      'id'   => 'PIR',
	      'INTERPRO-LINK' => 'http://pir.georgetown.edu/cgi-bin/ipcSF?id=PIRSF000478'}
	    ],
	'PFAM' =>
	    [ { 'INTERPRO-LINK' => '/cgi-bin/Pfam/getacc?$id'
		}
	 ],
	'MEDLINE' =>
	    [ { 'INTERPRO-LINK' => 'http://www.ncbi.nlm.nih.gov:80/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Abstract&list_uids=' 
		}
	 ],
	'NCBITAXON' =>
	    [ { 'INTERPRO-LINK' => 'http://www.ncbi.nlm.nih.gov/htbin-post/Taxonomy/wgetorg?lvl=0&id=$id' 
		}
	 ],
	'MIM' => 
	    [{'home' => 'http://www.ncbi.nlm.nih.gov/omim',
	      'link' => 'http://www.ncbi.nlm.nih.gov/htbin-post/Omim/dispmim?$id',
	      'id'   => 'NCBI'},
	     {'link' => 'http://www.sanger.ac.uk/srs5bin/cgi-bin/wgetz?-e+[OMIM-ID:$id]',
	      'id'   => 'SRS-UK'}
	    ],
	'PROSITE_PROFILE' => 
	    [{'home' => 'http://www.isrec.isb-sib.ch/software/PFSCAN_form.html',
	      'link' => 'http://www.isrec.isb-sib.ch/cgi-bin/get_pstprf?$id',
	      'id'   => 'ISREC'}
	    ],
	'SCOP' =>
	    [{'home' => 'http://www.biochem.ucl.ac.uk/bsm/pdbsum',
	      'link' => 'http://www.ebi.ac.uk/thornton-srv/databases/cgi-bin/pdbsum/GetPage.pl?pdbcode=PDBID',
	      'id'   => 'PDBSUM'},
	     {'home' => 'http://scop.mrc-lmb.cam.ac.uk/scop',
	      'link' => 'http://scop.mrc-lmb.cam.ac.uk/scop/search.cgi?pdb=PDBID&tlev=$add',
	      'id'   => 'SCOP-UK'},
	     {'home' => 'msd.ebi.ac.uk',
	      'link' => 'http://www.ebi.ac.uk/msd-srv/atlas?id=PDBID',
	      'id'   => 'MSD'}
	    ],
	'SMART' =>
	    [{'home' => 'http://smart.embl-heidelberg.de',
	      'link' => 'http://smart.embl-heidelberg.de/smart/do_annotation.pl?DOMAIN=$id&BLAST=DUMMY',
	      'id'   => 'SMART'}
	    ],
	'PRINTS' => 
	    [{'home' => 'http://www.biochem.ucl.ac.uk/bsm/dbbrowser/PRINTS/PRINTS.html',
	      'link' => 'http://umber.sbs.man.ac.uk/cgi-bin/dbbrowser/sprint/searchprintss.cgi?print
s_accn=$id&display_opts=Prints&category=None&queryform=false',
	      'id'   => 'MANCHESTER',
	      'INTERPRO-LINK' => 'http://methionine.sbc.man.ac.uk/cgi-bin/dbbrowser/sprint/searchprintss.cgi?prints_accn=$id&display_opts=Prints&category=None&queryform=false',
		}
	    ],
	'PRODOM' =>
	    [{'home' => 'http://protein.toulouse.inra.fr/prodom.html',
	      'link' => 'http://www.toulouse.inra.fr/prodom/cgi-bin/ReqProdomII.pl?id_dom1=$id',
	      'id'   => 'INRA'},
	     {'link' => 'http://www.sanger.ac.uk/srs5bin/cgi-bin/wgetz?-e+[PRODOM-PAC:$id]',
	      'id'   => 'SRS-UK'}
	    ],
	'MEROPS' => 
	    [{'home' => 'http://merops.iapc.bbsrc.ac.uk',
	      'link' => 'http://merops.sanger.ac.uk/cgi-bin/merops.cgi?id=$id',
	      'id'   => 'MEROPS'}
	    ],
	'CAZY' =>
 	[{'home' => 'http://afmb.cnrs-mrs.fr/~cazy/CAZY',	
	'link' => 'http://afmb.cnrs-mrs.fr/CAZY/$id.html',
	      'id'   => 'CAZY',
	'INTERPRO-LINK' => 'http://afmb.cnrs-mrs.fr/CAZY/$id.html'}
	    ],
'PANDIT' =>
            [{'home' => 'http://http://www.ebi.ac.uk/goldman-srv/pandit/',
              'link' => 'http://www.ebi.ac.uk/goldman-srv/pandit/pandit.cgi?action=browse&fam=THE_ID',
              'id'   => 'PANDIT'}
            ],
'FUNSHIFT' =>
            [{'home' => 'http://funshift.cgb.ki.se/index.html',
              'link' => 'http://funshift.cgb.ki.se/cgi-bin/stksub.cgi?domain=THE_ID',
              'id'   => 'FUNSHIFT'}
            ],

        'SYSTERS' =>
            [{'home' => 'http://systers.molgen.mpg.de',
              'link' => 'http://systers.molgen.mpg.de/cgi-bin/nph-fetchcluster.pl?PFAM=THE_ID',
              'id'   => 'SYSTERS'}
            ],
 'ROSETTA' =>
            [{'home' => 'http://yeastrc.org',
              'link' => 'http://www.yeastrc.org',
              'id'   => 'ROSETTA'}
            ],
    'COGS' =>
            [{'home' => 'http://www.ncbi.nlm.nih.gov/COG/',
#              'link' => 'http://www.ncbi.nlm.nih.gov/COG/new/release/cow.cgi?cog=$id',
              'link' => 'http://www.ncbi.nlm.nih.gov/COG/old/palox.cgi?$id',
              'id'   => 'COGS',
	'INTERPRO-LINK' => 'http://www.ncbi.nlm.nih.gov/COG/old/palox.cgi?$id'}
            ],
	'HOMSTRAD' =>
            [{'home' => 'http://www-cryst.bioc.cam.ac.uk/homstrad/',
              'link' => 'http://www-cryst.bioc.cam.ac.uk/cgi-bin/homstrad.cgi?family=$id',
              'id'   => 'HOMSTRAD',
	'INTERPRO-LINK' => 'http://www-cryst.bioc.cam.ac.uk/cgi-bin/homstrad.cgi?family=$id'}
        ],
        'GO' =>
            [{'home' => 'http://www.ebi.ac.uk/ego/',
              'link' => 'http://www.ebi.ac.uk/ego/DisplayGoTerm?id=$id&format=normal',
              'id'   => 'GO'}
            ],
	'INTERPRO' => 
	    [{'home' => 'http://www.ebi.ac.uk/interpro',
	      'link' => 'http://www.ebi.ac.uk/interpro/IEntry?ac=$id',
	      'id'   => 'INTERPRO',
	      'INTERPRO-LINK' => 'http://www.ebi.ac.uk/interpro/IEntry?ac=$id',

		}
	    ]
    };

    return $mapper;
}





=head2 get_database

 Title   : get_database
 Usage   : $db = $Bio::Pfam::Web::PfamWWWConfig::get_database;
 Function: 
    All access to the underlying database is via this object.
    The method is called mainly by other methods in this module
    but now and again by other modules
 Returns : Bio::Pfam::DB
 Args    : None


=cut

sub get_database {

    if (not defined $Bio::Pfam::Web::PfamWWWConfig::the_database) {
       my $loc_no_rdb = (-e $no_rdb)?1:0;
        $Bio::Pfam::Web::PfamWWWConfig::the_database = 
	    Bio::Pfam::DB_Web->new('-current' => $pfam_current_dir,
				  '-attic' => $pfam_attic_dir,
			          '-index' => $pfam_index_file,
                                  '-lock_file' => $pfam_lock_file,
                                  '-datadir' => $data_root,
                                  '-srsdb' => $srs_db,
                                  '-srspfam' => $srs_pfam,
				  '-srspfamb' => $srs_pfamB,  
				  '-srsswisspfam' => $srs_swisspfam,
				  '-getz' => $getz,
				  '-db_name' => $rdb_name,
				  '-db_host' => $rdb_host,
				  '-db_driver' => $rdb_driver,
				  '-db_user' => $rdb_user,
				  '-db_password' => $rdb_password,
				  '-no_rdb' => $loc_no_rdb);

    }

    return $Bio::Pfam::Web::PfamWWWConfig::the_database;

}


	

=head2 get_alignment

 Title   : get_alignment
 Usage   : $aln = $Bio::Pfam::Web::PfamWWWConfig::get_alignment('PF00003', 'full');
 Function: 
   Returns a SimpleAlign object for the given Pfam accession number
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
    my $get_pfamB_coloured = shift;
    my $ali;

    my $db = &get_database();
    if ($acc =~ /^PF/) {
        eval {    
	     $entry = $db->get_EntryA_by_acc( $acc ); 
	     $ali = ($type =~ /full/i)?$entry->full():$entry->seed();	
        };
        $@ && &Bio::Pfam::Web::PfamWWWConfig::exit_html("$@", $is_unheaded );	
    }
    else {
        # must be a Pfam-B entry needed
        eval { 
	     $entry = $db->get_EntryB_by_acc( $acc, $get_pfamB_coloured );
	     $ali = $entry->full();
        }; 
        $@ && &Bio::Pfam::Web::PfamWWWConfig::exit_html("$@", $is_unheaded);
    }

    return $ali;
}




=head2 annotated_sequence_by_name

 Title   : annotated_sequence_by_name
 Usage   : $annseq = $Bio::Pfam::Web::PfamWWWConfig::annotated_sequence_by_name('VAV_HUMAN');
 Function: 
   Returns an AnnotatedSequence for the given pfamseq identifier
 Returns : Bio::Pfam::AnnotatedSequence
 Args    : 
   A pfamseq identifier

=cut

sub annotated_sequence_by_name {
    my $name = shift;
    my $start_end = shift;		
    my ($db, $annotseq);

    $db = &get_database();
    ($annotseq) = $db->get_AnnotSeqs( [$name] );

    return $annotseq;
}


sub seq2pdb {
    my ($pdb, $chain, %seq_loc) = @_;
    my $db  = &get_database();
    my %pdb_loc = $db->transform_seq_pos2pdb_pos($pdb, $chain, %seq_loc);
    return %pdb_loc;
} 

sub model_length_by_acc {
    my $acc = shift;
    my ($db, $model_length);
    $db = &get_database();
    ($model_length) = $db->_model_length( $acc );

    return $model_length;
}


sub get_protein_by_crc64  {
    my $crc64 = shift;
    my $pfam_type = shift;	
    my ($db, %all_pfam_ids);
    # my ($db, $auto_pfamseq, $pfamseq_id);
    $db = &get_database();
    (%all_pfam_ids) = $db->_crc64_to_protein( $crc64,$pfam_type );
	
    return %all_pfam_ids;
}

=head2 protein_sequence

 Title   : protein_sequence
 Usage   : $seq = $Bio::Pfam::Web::PfamWWWConfig::protein_sequence_by_name('VAV_HUMAN');
 Function: 
   Returns a filehandle to a pfamseq entry for the given identifier
 Returns : filehandle
 Args    : 
   A pfamseq identifier or accession number
 Notes   :
   Returns undef if no seq can be found with the given id or accession

=cut

sub protein_sequence {
    my $name = shift;
    my $db = shift;
    my $seq_factory = Bio::Pfam::AnnSeqFactory->new;
    my @seq;

    
    @seq = $db->get_Seq_pfamseq( $name, "id", $seq_factory );

    if ($@) {
       # try again with accession this time
       eval {
           @seq = $db->get_Seq_pfamseq( $name, "acc", $seq_factory );
       };
       
       if ($@) {
	   # try again with secondary-accession this time
	   eval {
		@seq = $db->get_Seq_pfamseq( $name, "sec", $seq_factory);
		}; 
	   if ($@) {
	      @seq = undef;
	   }
       }
    }
    
    return @seq;
}



sub interpro_info {
	
  my($acc) = @_;
  my %interpro_info;

    my $db = &get_database();
    eval {
       %interpro_info = $db->get_interpro_information( $acc );
    };




    return %interpro_info;
}

sub clans_info {

  my($acc) = @_;
  my %clans_info;
    my $db = &get_database();
    eval {
       %clans_info = $db->get_clan_information( $acc );
    };




    return %clans_info;
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




=head2 exit_html

 Title   : exit_html
 Usage   : $Bio::Pfam::Web::PfamWWWConfig::exit_html("Good night, sweet prince");
 Function: 
   A function that exits with an error message to the browser
   (To be called if something in the script went unrecoverably wrong).
 Returns :
 Args    :
   1. An error message 
   2. A boolean (true if an http header is required);


=cut

sub exit_html {
    my $mess = shift;
    my $noheader = shift;

    if( defined $noheader ) {
	print "Content-type: text/html\n\n";
    }

    print "<p>Sorry - the script failed due to errors. [$mess]. Please contact $contact if you feel there is a problem<p>\n";
    exit(0);
}


=head2 apology

 Title   : apology
 Usage   : $Bio::Pfam::Web::PfamWWWConfig::apology("Good night, sweet prince (see you in the morning)");
 Function: 
   A function that exits with an error message to the browser
   (Pretty output; to be called is a service is unavailable)
 Returns :
 Args    : 
   1. An error message 
   2. A boolean (true if an http header is required);

=cut

sub apology {
    my $mess = shift;
    my $noheader = shift;

    if( defined $noheader ) {
        print "Content-type: text/html\n\n";
    }

    print &Bio::Pfam::Web::PfamWWWConfig::header("Sorry");
    print "<center><h3>$mess</h3></center><p>";
    print "<center><h3>We regret any inconvenience that this may cause</h3></center><p>";    

    print &Bio::Pfam::Web::PfamWWWConfig::footer();
    exit(0);
}




=head2 user_error

 Title   : user_error
 Usage   : $Bio::Pfam::Web::PfamWWWConfig::exit_html("Good night, sweet prince (see you in the morning)");
 Function: 
   A function that exits with an error message to the browser
   (Pretty output; to be called if the user made an error).
 Returns :
 Args    : 
   1. An error message 
   2. A boolean (true if an http header is required);

=cut

sub user_error {
    my $mess = shift;
    my $noheader = shift;

    if( defined $noheader ) {
        print "Content-type: text/html\n\n";
    }

    print &Bio::Pfam::Web::PfamWWWConfig::header("Error");	   
    print "<center><h3>$mess</h3></center><p>";

    print &Bio::Pfam::Web::PfamWWWConfig::footer();
    exit(0);
}



=head2 header

 Title   : header
 Usage   : $Bio::Pfam::Web::PfamWWWConfig::header("The title of the page");
 Function: 
   Prints the website page header followed by the given title for
   the page
 Returns :
 Args    : The title for the page


=cut


sub header {
my($title, $pfam_id , $pfam_acc_num, $url_atributes) = @_;

  my ($varhash,$outstr);
    
  #pick up the header file, set up the variables
    
    
  $varhash{'title'} = $title;
  $varhash{'root'}  = $local_root;
  $varhash{'site'}  = $site;
  $varhash{'email'} = $email;
  $varhash{'admin'} = $adminname;
  $varhash{'pfam_acc_num'} = $pfam_acc_num;

  $varhash{'gif2'} = "gifs/blank.gif";
  $varhash{'gif'} = "gifs/blank.gif";
  $varhash{'taxonomy'} = "tql_small_tab.gif";
  $varhash{'dnasearch'} = "dnasearch_small_tab.gif";
  $varhash{'tsearch'} = "tsearch_small_tab.gif";
  $varhash{'genomes'} = "genomes_small_tab.gif";
  $varhash{'search'} = "search_small_tab.gif";
  if($title =~ /alignment/i) {
   $varhash{'pfamcss'} = "pfam.css";
  } else {
   $varhash{'pfamcss'} = "pfam.css";
  }
  $varhash{'pfamjs'} = "pfam.js";
  $varhash{'pfammenujs'} = "pfam_menu.js";
  $varhash{'udmjs'} = "udm-custom.css";

  #&create_family_header_tag($pfam_acc_num, $pfam_id, $Bio::Pfam::Web::PfamWWWConfig::image_ext) if ($pfam_acc_num);


  if ($title =~ /Taxonomy/i) {
    $varhash{'taxonomy'} = "tql_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";
} elsif ($title =~ /Genome/i) {
    $varhash{'genomes'} = "genomes_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";


} elsif ($title =~ /Domain/i) {



  } elsif ( ($title =~ /query/i) || ($title =~ /search/i) ) {
    $varhash{'tsearch'} = "tsearch_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";
} elsif ($title =~ /DNA/i) {
   $varhash{'dnasearch'} = "dnasearch_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";

} elsif  ( ($title =~ /alignment/i) || ($title =~ /results/i) )  {

    $varhash{'search'} = "search_large_tab.gif";
    $varhash{'gif'} = "gifs/blank.gif";


} elsif ($title =~ /web-log/i) {

} elsif ($title =~ /error/i) {

} elsif ($title =~ /sorry/i) {


} elsif ($title =~ /schema/i) {


  
  }  elsif ( ($title !~ /struct/i) && ($title !~ /SwissPfam/i) && ($title !~ /mistake/i) )  {

    $varhash{'url'} = "getacc?$pfam_acc_num";
    $varhash{'gif'} = "temp/new_gifs/". $pfam_acc_num . "_large" . $image_ext;
  
  } elsif ($title =~ /swisspfam/i) {
    if ($pfam_acc_num) {
      $varhash{'url'} = "getacc?$pfam_acc_num";
      $varhash{'gif'} = "temp/new_gifs/". $pfam_acc_num .  "_small" . $image_ext;

      $varhash{'url2'} = "swisspfamget.pl?$url_atributes";
      $varhash{'gif2'} = "gifs/page_header/swisspfam_large_tab.gif";
      
    } else {
      $varhash{'url'} = "swisspfamget.pl?$url_atributes";
      $varhash{'gif'} = "gifs/page_header/swisspfam_large_tab.gif";
    }

  }  elsif ($title =~ /structural/i) {

    $varhash{'url2'} = "structural_view.pl?$url_atributes";
    $varhash{'gif2'} = "gifs/page_header/structure_large_tab.gif";

    if ($pfam_acc_num) {
      $varhash{'url'} = "getacc?$pfam_acc_num";
     $varhash{'gif'} = "temp/new_gifs/". $pfam_acc_num . "_small" . $image_ext;
     
    }

  } 

	## TEMP REMOVAL OF JAVASCRIPT
	#$header_file = $java_header_file if ($javascript);
	#$header_file = $header_file if ($javascript);
    open(HEAD,$header_file) || die "Could not open $header_file $!";
    while(<HEAD>) {
	while( $_ =~ /\$(\w+)/g ) {
	    
	   my $var = $1;
	    # print STDERR "Got $var as a subs [$varhash{$var}]\n";
	   my $temp = $varhash{$var};
	    s/\$$var/$temp/; # here it is...
	}
	$outstr .= $_;
    }
    close(HEAD);


    return $outstr;
}



=head2 footer

 Title   : footer
 Usage   : $Bio::Pfam::Web::PfamWWWConfig::footer("The title of the page");
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
    
    
#    $varhash{'title'} = $title;
#    $varhash{'root'}  = $local_root;
#    $varhash{'site'}  = $site;
#    $varhash{'email'} = $email;
#    $varhash{'admin'} = $adminname;
#  $varhash{'pfam_acc_num'} = $pfam_acc_num;
  

	## TEMP REMOVAL OF JAVASCRIPT
	#$header_file = $java_header_file if ($javascript);
#	$header_file = $header_file if ($javascript);
    open(HEAD,$footer_file) || die "Could not open $footer_file $!";
    while(<HEAD>) {
	while( $_ =~ /\$(\w+)/g ) {
	    
	  #  $var = $1;
	    # print STDERR "Got $var as a subs [$varhash{$var}]\n";
	 #   $temp = $varhash{$var};
	  #  s/\$$var/$temp/; # here it is...
	}
	$outstr .= $_;
    }
    close(HEAD);


    return $outstr;

  #  return "<hr>If you think there is anything wrong with this script, please contact $contact\n";
}




=head2 seq_from_lines

 Title   : footer
 Usage   : $seq = $Bio::Pfam::Web::PfamWWWConfig::seq_from_lines(\@lines, $start, $end);
 Function: 
    Constructs a Bio::Pfam::SeqPfam object from the given ref. to array of lines.
    The start and end are optional.
 Returns :
 Args    : 
   1. A ref to an array of lines

=cut

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

    if ($seqc =~ /[^ABCDEFGHIKLMNPQRSTVWXYZ]/) {
#	&user_error("The supplied sequence contains some non-protein symbols");
    }

    my $seq = Bio::Pfam::SeqPfam->new( '-id' => $name,
				      '-seq' => $seqc );
    $seq->start('1');
    my $end = length($seqc);
    $end--;
    $seq->end( $end );

    $seq = Bio::Pfam::SeqPfam->new( '-id' => $seq->id(),
				    '-seq' => $seq->seq(),
				    '-start' => $start,
				    '-end' => $end   );

    return $seq;
}



sub logs {

        my ($message) = @_;
	if($message){
	    open(_LOG, ">>$tempdir/stats/weblog") or print STDERR "TEMPDIR CANNA OPEN $tempdir/stats/web log $!\n";
	    my($sec, $min, $hour, $mday, $mon, $year) = (localtime) [0, 1, 2, 3, 4, 5];
	    $mon++;
	    print _LOG "$mday/$mon/2005 $hour:$min:$sec ".  $ENV{'REMOTE_HOST'} ." " . $ENV{'REMOTE_ADDR'} . " " . $ENV{'USER_AGENT'} . " $message \n";
	    
	    close(_LOG);
	}

}

sub crc64_logs {

        my ($crc_key) = @_;


        open(_LOG, ">>$tempdir/stats/crc64") or print "CRC CANNA OPEN $tempdir/stats/crc log $!\n";

        my($sec, $min, $hour, $mday, $mon, $year) = (localtime) [0, 1, 2, 3, 4, 5];
        $mon++;
        print _LOG "$mday/$mon/2002 $hour:$min:$sec  ~" . "$crc_key \n";

        close(_LOG);



}




=head2  get_pdb_title

 Title   : get_pdb_title
 Usage   :
 Function:
   Gets the pdb header and title from a file
 Example :
 Returns :
 Args    :

=cut



sub get_pdb_title {

   my $tmp_pid = shift;



my ($title, $header);	
  $db = &get_database();
    ($header, $title) = $db->get_pdb_details( $tmp_pid);

    return $header, $title;

}

sub  jpeg_key_info{

  my $pid = shift;
  
  
  
  my (@jpeg_info);	
  $db = &get_database();
  (@jpeg_info) = $db->get_jpeg_key_info( $pid);
  
  return @jpeg_info;
  
}

=head2  pdb_gif_shuffle

 Title   :  pdb_gif_shuffle
 Usage   :
 Function:
   Takes the array with the pdb structures to be displayed and randomly mixes them
 Example :
 Returns :
 Args    :

=cut

sub  pdb_gif_shuffle {

  my $array = shift;
    my $i;
  for ($i = @$array; --$i;) {
    my $j = int rand ($i + 1);
    next if $i == $j;
    @$array[$i, $j] = @$array[$j, $i];


  }

 return $array;



}


########### Check & delete dir's to be deleted
# currently used by nph-search & swisspfamget
#
#

sub check_dir_sizes {

  ### Check temp & new_gifs dir and delete if FLAG file > half a day old

  my $dir = $Bio::Pfam::Web::PfamWWWConfig::tempdir;

  ### Get the time for the FLAG FILE
  my $flag_file_time =  -C "$dir/FLAG";
  ### If the flag file is older that half a day then delete the dir and new_gifs dir
  if ($flag_file_time > 0.5) {
    opendir(_EX_DIR, "$dir") || die("Could not open $dir $!");

    ## delete all the files in temp dir
    foreach $temp_file ( readdir(_EX_DIR) ) {
      $temp_file =~ /^\.+$/ && next;
        my $file = $1 if ($temp_file =~ /^([-\@_\w.]+)$/);
        
      unlink("$dir/$file");

    }

    ## delete all the new_gifs dir
    opendir(_GIF_DIR, "$dir/new_gifs") || die("Could not open $dir $!");
    foreach $temp_file ( readdir(_GIF_DIR) ) {
      $temp_file =~ /^\.+$/ && next;
        my $file = $1 if ($temp_file =~ /^([-\@_~\w.]+)$/);
      unlink("$dir/new_gifs/$file");
    }

    ## Reset the FLAG file
    open(_FILE, ">$Bio::Pfam::Web::PfamWWWConfig::tempdir/FLAG");
    print _FILE "1";
    close(_FILE);

  }


}

sub drawing_generate_gif {

	my ($width, $colours, $region, $return_gif, $width_offset, $beg_shape , $end_shape) = @_;	

 my $new_image =  &generate_domain_gif( $width, $colours, $region, $return_gif, $width_offset, $beg_shape, $end_shape);

	return $new_image;

}

sub download_generate_all_download_gif {

  my ($image_coords, $image_name) = @_;
  my $new_image =  &generate_all_download_gif($image_coords, $image_name );
  return $new_image;
}

sub genome_category {
  my($cat, $regions) = @_;
  my $db = &get_database();
   my(@genome_cat_results) = $db->genome_build_cat($cat, $regions );

   return @genome_cat_results;

}

sub genome_doms {
  my($cat) = @_;
  my $db = &get_database();
   my(@genome_ncbi_results) = $db->genome_get_doms($cat);

   return @genome_ncbi_results;

}

sub get_species {
  my($ncbi) = @_;
   
  my $db = &get_database();
   my($species) = $db->genome_get_species($ncbi);

   return $species;

}

sub genome_regions {
  my($cat,  %regions) = @_;
  my $db = &get_database();
  my(%genome_regions) = $db->genome_get_regions($cat,  %regions );
 # foreach (sort keys %genome_regions) {
 #   print "THE KEY: $_ <BR>";
 # }
   return %genome_regions;

}

sub get_hex_colour {

  my ($number) = @_;
  open(_HEX, "$Bio::Pfam::Web::PfamWWWConfig::data_root/hex_colours");
  my $hex;

  while(<_HEX>) {
    if ($_ =~ /(\d+)~(\w+)~/) {

      if ($1 eq $number) {
	$hex = $2;
	last;
      }
    }
  }

  close(_HEX);
  return $hex;

}

sub get_rgb_colour {
  my ($number) = @_;
  open(_RGB, "$Bio::Pfam::Web::PfamWWWConfig::data_root/hex_colours");
  my $rgb;
  while(<_RGB>){
    if ($_ =~ /(\d+)~(\w+)~(\S+)/){
      if ($1 eq $number) {
	$rgb = $3;
	last;
      }
    }
  }
  close(_RGB);
  return $rgb;
}

# stub to point to a method in DB_Web.pm. It's beyond me why we need
# this indirection, but hey...
# JT 20051118 WTSI
sub lookUp {
  my $term = shift;
  my $db = &get_database();
  return $db->lookUp( $term );
}


sub search_pfam {

   my $terms = shift;



   my (@pfam_accs);
   $db = &get_database();
   (@pfam_accs) = $db->search_pfam( $terms);

   return @pfam_accs;

}


sub context_regions {

   my ($acc, $count)  = @_;
   $db = &get_database();
   if ($count) {
     my $ret_count;
     ($ret_count) = $db->context_regions( $acc, $count);
     
     return $ret_count;
   }


}


sub test_nested_regions {
  #print "ALL: @_ <P>";
  #print "1: " .$_[0] . " 2: " .$_[1] . " <P>";
  my ($acc1, $acc2)  = @_;
 # my $acc1 = $_[0];
 # my $acc2 = $_[1];
#print "$acc1 :BLEE: $acc2 <P>";
  $db = &get_database();
  my $allow_nesting;
  ($allow_nesting) = $db->test_nested_regions( $acc1, $acc2);
 #  print "ALLO: $allow_nesting <P>";
  return $allow_nesting;
   
  

}



1;  # says use was ok

__END__
