-- MySQL dump 10.9
--
-- Host: pfam    Database: web_user
-- ------------------------------------------------------
-- Server version	4.1.10-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `web_user`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `web_user` /*!40100 DEFAULT CHARACTER SET latin1 */;

USE `web_user`;

--
-- Table structure for table `alignment_das_sources`
--

CREATE TABLE `alignment_das_sources` (
  `from_system` varchar(200) NOT NULL default '',
  `from_type` varchar(200) NOT NULL default '',
  `to_system` varchar(200) NOT NULL default '',
  `to_type` varchar(200) NOT NULL default '',
  `server_id` varchar(40) NOT NULL default '',
  `name` varchar(100) NOT NULL default '',
  `url` varchar(200) NOT NULL default '',
  `helper_url` varchar(200) default NULL,
  PRIMARY KEY  (`from_system`,`from_type`,`to_system`,`to_type`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `alignment_das_sources`
--

INSERT INTO `alignment_das_sources` VALUES ('Ensembl','Protein Sequence','UniProt','Protein Sequence','DS_192','alig_uniprot_ensp','http://das.sanger.ac.uk/das/aliguniprotensp',NULL);
INSERT INTO `alignment_das_sources` VALUES ('UniProt','Protein Sequence','Ensembl','Protein Sequence','DS_192','alig_uniprot_ensp','http://das.sanger.ac.uk/das/aliguniprotensp',NULL);
INSERT INTO `alignment_das_sources` VALUES ('UniProt','Protein Sequence','PDBresnum','Protein Structure','DS_113','alig_pdb_sp','http://das.sanger.ac.uk/das/msdpdbsp',NULL);
INSERT INTO `alignment_das_sources` VALUES ('PDBresnum','Protein Structure','UniProt','Protein Sequence','DS_113','alig_pdb_sp','http://das.sanger.ac.uk/das/msdpdbsp',NULL);

--
-- Table structure for table `das_sources`
--

CREATE TABLE `das_sources` (
  `server_id` varchar(40) NOT NULL default '',
  `name` varchar(100) NOT NULL default '',
  `url` varchar(200) NOT NULL default '',
  `system` varchar(200) NOT NULL default '',
  `helper_url` varchar(200) default NULL,
  `default_server` tinyint(1) default NULL,
  PRIMARY KEY  (`server_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `das_sources`
--

INSERT INTO `das_sources` VALUES ('DS_109','uniprot','http://www.ebi.ac.uk/das-srv/uniprot/das/aristotle','UniProt','http://www.ebi.ac.uk/uniprot-das',1);
INSERT INTO `das_sources` VALUES ('DS_110','dssp','http://cmbi4.cmbi.ru.nl/das/dssp','PDBresnum','',0);
INSERT INTO `das_sources` VALUES ('DS_111','cath','http://cathwww.biochem.ucl.ac.uk:9000/das/cath_pdb','PDBresnum','http://www.biochem.ucl.ac.uk/bsm/cath',0);
INSERT INTO `das_sources` VALUES ('DS_114','signalp','http://genome.cbs.dtu.dk:9000/das/signalp','UniProt','http://www.cbs.dtu.dk/services/SignalP',0);
INSERT INTO `das_sources` VALUES ('DS_115','netphos','http://genome.cbs.dtu.dk:9000/das/netphos','UniProt','http://www.cbs.dtu.dk/services/NetPhos',0);
INSERT INTO `das_sources` VALUES ('DS_116','netoglyc','http://genome.cbs.dtu.dk:9000/das/netoglyc','UniProt','http://www.cbs.dtu.dk/services/NetOGlyc',0);
INSERT INTO `das_sources` VALUES ('DS_117','tmhmm','http://genome.cbs.dtu.dk:9000/das/tmhmm','UniProt','http://www.cbs.dtu.dk/services/TMHMM',0);
INSERT INTO `das_sources` VALUES ('DS_118','prop','http://genome.cbs.dtu.dk:9000/das/prop','UniProt','http://www.cbs.dtu.dk/services/ProP',0);
INSERT INTO `das_sources` VALUES ('DS_119','transmem_pred','http://pongo.biocomp.unibo.it/das/dasdb','UniProt','http://www.cbs.dtu.dk/services/SecretomeP',0);
INSERT INTO `das_sources` VALUES ('DS_120','superfamily','http://supfam.org/SUPERFAMILY/cgi-bin/das/up','UniProt','http://supfam.mrc-lmb.cam.ac.uk/SUPERFAMILY',1);
INSERT INTO `das_sources` VALUES ('DS_121','secretomep','http://genome.cbs.dtu.dk:9000/das/secretomep','UniProt','http://www.cbs.dtu.dk/services/SecretomeP',0);
INSERT INTO `das_sources` VALUES ('DS_124','uniprot_exon_snp','http://das.sanger.ac.uk/das/uniprotensemblpepfeatures','UniProt','',0);
INSERT INTO `das_sources` VALUES ('DS_129','superfam','http://supfam.mrc-lmb.cam.ac.uk/SUPERFAMILY/cgi-bin/das/to','Ensembl','http://supfam.mrc-lmb.cam.ac.uk/SUPERFAMILY',0);
INSERT INTO `das_sources` VALUES ('DS_132','cbs_sort','http://genome.cbs.dtu.dk:9000/das/cbs_sort','UniProt','http://www.cbs.dtu.dk/services',0);
INSERT INTO `das_sources` VALUES ('DS_133','cbs_ptm','http://genome.cbs.dtu.dk:9000/das/cbs_ptm','UniProt','http://www.cbs.dtu.dk/services',0);
INSERT INTO `das_sources` VALUES ('DS_134','cbs_func','http://genome.cbs.dtu.dk:9000/das/cbs_func','UniProt','http://www.cbs.dtu.dk/services',0);
INSERT INTO `das_sources` VALUES ('DS_146','ensp_pdb_mapping','http://das.sanger.ac.uk/das/ensppdbmapping','Ensembl','http://www.efamily.org.uk/software/dasclients/spice/ensembl.shtml',0);
INSERT INTO `das_sources` VALUES ('DS_150','s3dm','http://www.ebi.ac.uk/msd-srv/msdmotif/das/s3dm','UniProt','http://www.ebi.ac.uk/msd-srv/msdmotif',0);
INSERT INTO `das_sources` VALUES ('DS_168','gtd','http://bioinf.cs.ucl.ac.uk:8000/servlet/pdas.pdasServlet2/das/GTD','UniProt','http://bioinf.cs.ucl.ac.uk/GTD',0);
INSERT INTO `das_sources` VALUES ('DS_170','MisPred','http://mispred.enzim.hu/das/mispred','Ensembl','http://mispred.enzim.hu',0);
INSERT INTO `das_sources` VALUES ('DS_171','PRIDE-DAS','http://www.ebi.ac.uk/das-srv/pride/das/PRIDE','UniProt','http://www.ebi.ac.uk/das-srv/pride/das',0);
INSERT INTO `das_sources` VALUES ('DS_184','netnglyc','http://genome.cbs.dtu.dk:9000/das/netnglyc','UniProt','http://www.cbs.dtu.dk/services/NetNGlyc',0);
INSERT INTO `das_sources` VALUES ('DS_185','netacet','http://genome.cbs.dtu.dk:9000/das/netacet','UniProt','http://www.cbs.dtu.dk/services/NetAcet',0);
INSERT INTO `das_sources` VALUES ('DS_186','targetp','http://genome.cbs.dtu.dk:9000/das/targetp','UniProt','http://www.cbs.dtu.dk/services/TargetP',0);
INSERT INTO `das_sources` VALUES ('DS_187','lipop','http://genome.cbs.dtu.dk:9000/das/lipop','UniProt','http://www.cbs.dtu.dk/services/LipoP',0);
INSERT INTO `das_sources` VALUES ('DS_188','netnes','http://genome.cbs.dtu.dk:9000/das/netnes','UniProt','http://www.cbs.dtu.dk/services/NetNES',0);
INSERT INTO `das_sources` VALUES ('DS_189','cbs_total','http://genome.cbs.dtu.dk:9000/das/cbs_total','UniProt','http://www.cbs.dtu.dk/services',0);
INSERT INTO `das_sources` VALUES ('DS_191','hsa35pep','http://das.sanger.ac.uk/das/hsa35pep','Ensembl','',0);
INSERT INTO `das_sources` VALUES ('DS_210','SMART','http://smart.embl.de/smart/das/smart','UniProt','http://smart.embl.de/help/smart_about.shtml',1);
INSERT INTO `das_sources` VALUES ('DS_212','FUNCut','http://pdg.cnb.uam.es/das/funcut','UniProt','http://www.pdg.cnb.uam.es/funcut.html',0);
INSERT INTO `das_sources` VALUES ('DS_216','PDBsum_protprot','http://www.ebi.ac.uk/das-srv/proteindas/das/sasprot','UniProt','',0);
INSERT INTO `das_sources` VALUES ('DS_217','PDBsum_ligands','http://www.ebi.ac.uk/das-srv/proteindas/das/saslig','UniProt','',0);
INSERT INTO `das_sources` VALUES ('DS_218','PDBsum_DNAbinding','http://www.ebi.ac.uk/das-srv/proteindas/das/sasdna','UniProt','',0);
INSERT INTO `das_sources` VALUES ('DS_233','cath_uniprot_mapping','http://cathwww.biochem.ucl.ac.uk:9000/das/cath_sptr','UniProt','http://www.biochem.ucl.ac.uk/bsm/cath',0);
INSERT INTO `das_sources` VALUES ('DS_311','Pfam Other Features','http://das.sanger.ac.uk/das/pfamOtherFeatures','UniProt','http://www.sanger.ac.uk/Software/Pfam',1);
INSERT INTO `das_sources` VALUES ('DS_327','interpro','http://das.ensembl.org/das/interpro','UniProt','http://www.ebi.ac.uk/interpro',1);
INSERT INTO `das_sources` VALUES ('DS_340','Tango aggregation','http://switpc7.vub.ac.be:9000/das/tango','Ensembl','http://tango.embl.de',0);
INSERT INTO `das_sources` VALUES ('DS_341','everest','http://www.protonet.cs.huji.ac.il/das/everest','UniProt','http://www.protonet.cs.huji.ac.il',0);
INSERT INTO `das_sources` VALUES ('DS_342','protonet','http://www.protonet.cs.huji.ac.il/das/protonet','UniProt','http://www.protonet.cs.huji.ac.il',0);
INSERT INTO `das_sources` VALUES ('DS_999','phobius','http://obelix:9000','UniProt','http://',1);

--
-- Table structure for table `feature_das_sources`
--

CREATE TABLE `feature_das_sources` (
  `server_id` varchar(40) NOT NULL default '',
  `system` varchar(200) NOT NULL default '',
  `sequence_type` varchar(200) NOT NULL default '',
  `name` varchar(100) NOT NULL default '',
  `url` varchar(200) NOT NULL default '',
  `helper_url` varchar(200) default NULL,
  `default_server` tinyint(1) NOT NULL default '0',
  PRIMARY KEY  (`server_id`,`system`,`sequence_type`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `feature_das_sources`
--

INSERT INTO `feature_das_sources` VALUES ('DS_109','UniProt','Protein Sequence','uniprot','http://www.ebi.ac.uk/das-srv/uniprot/das/aristotle','http://www.ebi.ac.uk/uniprot-das',1);
INSERT INTO `feature_das_sources` VALUES ('DS_110','PDBresnum','Protein Structure','dssp','http://cmbi4.cmbi.ru.nl/das/dssp','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_111','PDBresnum','Protein Structure','cath','http://cathwww.biochem.ucl.ac.uk:9000/das/cath_pdb','http://www.biochem.ucl.ac.uk/bsm/cath',0);
INSERT INTO `feature_das_sources` VALUES ('DS_114','UniProt','Protein Sequence','signalp','http://genome.cbs.dtu.dk:9000/das/signalp','http://www.cbs.dtu.dk/services/SignalP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_115','UniProt','Protein Sequence','netphos','http://genome.cbs.dtu.dk:9000/das/netphos','http://www.cbs.dtu.dk/services/NetPhos',0);
INSERT INTO `feature_das_sources` VALUES ('DS_116','UniProt','Protein Sequence','netoglyc','http://genome.cbs.dtu.dk:9000/das/netoglyc','http://www.cbs.dtu.dk/services/NetOGlyc',0);
INSERT INTO `feature_das_sources` VALUES ('DS_117','UniProt','Protein Sequence','tmhmm','http://genome.cbs.dtu.dk:9000/das/tmhmm','http://www.cbs.dtu.dk/services/TMHMM',0);
INSERT INTO `feature_das_sources` VALUES ('DS_118','UniProt','Protein Sequence','prop','http://genome.cbs.dtu.dk:9000/das/prop','http://www.cbs.dtu.dk/services/ProP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_119','UniProt','Protein Sequence','transmem_pred','http://pongo.biocomp.unibo.it/das/dasdb','http://www.cbs.dtu.dk/services/SecretomeP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_120','UniProt','Protein Sequence','superfamily','http://supfam.org/SUPERFAMILY/cgi-bin/das/up','http://supfam.mrc-lmb.cam.ac.uk/SUPERFAMILY',1);
INSERT INTO `feature_das_sources` VALUES ('DS_121','UniProt','Protein Sequence','secretomep','http://genome.cbs.dtu.dk:9000/das/secretomep','http://www.cbs.dtu.dk/services/SecretomeP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_124','UniProt','Protein Sequence','uniprot_exon_snp','http://das.sanger.ac.uk/das/uniprotensemblpepfeatures','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_126','Ensembl','Gene_ID','phenotypes','http://www.ebi.ac.uk/das-srv/genedas/das/phenotypes','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_129','Ensembl','Protein Sequence','superfam','http://supfam.mrc-lmb.cam.ac.uk/SUPERFAMILY/cgi-bin/das/to','http://supfam.mrc-lmb.cam.ac.uk/SUPERFAMILY',0);
INSERT INTO `feature_das_sources` VALUES ('DS_130','SGD','Chromosome','ygpm','http://das.ensembl.org/das/ens_sc1_ygpm','http://www.cbs.dtu.dk/services/ProP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_132','UniProt','Protein Sequence','cbs_sort','http://genome.cbs.dtu.dk:9000/das/cbs_sort','http://www.cbs.dtu.dk/services',0);
INSERT INTO `feature_das_sources` VALUES ('DS_133','UniProt','Protein Sequence','cbs_ptm','http://genome.cbs.dtu.dk:9000/das/cbs_ptm','http://www.cbs.dtu.dk/services',0);
INSERT INTO `feature_das_sources` VALUES ('DS_134','UniProt','Protein Sequence','cbs_func','http://genome.cbs.dtu.dk:9000/das/cbs_func','http://www.cbs.dtu.dk/services',0);
INSERT INTO `feature_das_sources` VALUES ('DS_135','RGSC','Chromosome','das_ncbi','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_ncbi','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_136','RGSC','Chromosome','gnomon','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_gnomon','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_137','RGSC','Chromosome','fgenes','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_fgenes','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_138','RGSC','Chromosome','rattwscan','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_twinscan','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_139','RGSC','Chromosome','ratrgd','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_rgdgene','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_140','RGSC','Chromosome','ratrefseq','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_refseq','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_141','RGSC','Chromosome','ratbacend','http://das1.sanger.ac.uk:7070/perl/das/ens_rgsc_31_bacend','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_142','MGI','Gene_ID','emma','http://andy.emma.cnr.it:9000/das/emma','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_143','EMBL','Clone','das_ens_cds','http://das.ensembl.org/das/ensembl_Mus_musculus_core_28_33d_cds','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_144','EMBL','Clone','das_ens_trans','http://das.ensembl.org/das/ensembl_Mus_musculus_core_28_33d_trans','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_146','Ensembl','Protein Sequence','ensp_pdb_mapping','http://das.sanger.ac.uk/das/ensppdbmapping','http://www.efamily.org.uk/software/dasclients/spice/ensembl.shtml',0);
INSERT INTO `feature_das_sources` VALUES ('DS_150','PDBresnum','Protein Structure','s3dm','http://www.ebi.ac.uk/msd-srv/msdmotif/das/s3dm','http://www.ebi.ac.uk/msd-srv/msdmotif',0);
INSERT INTO `feature_das_sources` VALUES ('DS_150','UniProt','Protein Sequence','s3dm','http://www.ebi.ac.uk/msd-srv/msdmotif/das/s3dm','http://www.ebi.ac.uk/msd-srv/msdmotif',0);
INSERT INTO `feature_das_sources` VALUES ('DS_154','NCBI','Chromosome','nscsnp deleterious','http://www.brightstudy.ac.uk/cgi-bin4/das/snp_das','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_168','UniProt','Protein Sequence','gtd','http://bioinf.cs.ucl.ac.uk:8000/servlet/pdas.pdasServlet2/das/GTD','http://bioinf.cs.ucl.ac.uk/GTD',0);
INSERT INTO `feature_das_sources` VALUES ('DS_170','Ensembl','Protein Sequence','MisPred','http://mispred.enzim.hu/das/mispred','http://mispred.enzim.hu',0);
INSERT INTO `feature_das_sources` VALUES ('DS_171','UniProt','Protein Sequence','PRIDE-DAS','http://www.ebi.ac.uk/das-srv/pride/das/PRIDE','http://www.ebi.ac.uk/das-srv/pride/das',0);
INSERT INTO `feature_das_sources` VALUES ('DS_175','EMBL','Clone','vega_human_cds','http://servlet.sanger.ac.uk:8080/das/vega_Homo_sapiens_core_40_1006_cds','http://vega.sanger.ac.uk/Homo_sapiens',0);
INSERT INTO `feature_das_sources` VALUES ('DS_176','EMBL','Clone','vega_mouse_trans','http://servlet.sanger.ac.uk:8080/das/vega_Mus_musculus_core_39_0706_trans','http://vega.sanger.ac.uk/Mus_musculus',0);
INSERT INTO `feature_das_sources` VALUES ('DS_177','EMBL','Clone','vega_mouse_cds','http://servlet.sanger.ac.uk:8080/das/vega_Mus_musculus_core_39_0706_cds','http://vega.sanger.ac.uk/Mus_musculus',0);
INSERT INTO `feature_das_sources` VALUES ('DS_178','EMBL','Clone','vega_human_trans','http://servlet.sanger.ac.uk:8080/das/vega_Homo_sapiens_core_40_1006_trans','http://vega.sanger.ac.uk/Homo_sapiens',0);
INSERT INTO `feature_das_sources` VALUES ('DS_180','NCBI m','Contig','IMGT Genes','http://imgt3d.igh.cnrs.fr:9000/das/IMGT_Genes','http://imgt.cines.fr',0);
INSERT INTO `feature_das_sources` VALUES ('DS_180','NCBI','Contig','IMGT Genes','http://imgt3d.igh.cnrs.fr:9000/das/IMGT_Genes','http://imgt.cines.fr',0);
INSERT INTO `feature_das_sources` VALUES ('DS_182','ZFISH','Chromosome','ZFMODELS-microarrays','http://das.ensembl.org/das/ens_zfish6_array','http://www.sanger.ac.uk/cgi-bin/Projects/D_rerio/zfmodels/arraymap.pl',0);
INSERT INTO `feature_das_sources` VALUES ('DS_183','ZFISH','Chromosome','ZFMODELS-enhancers','http://das.ensembl.org/das/ens_zfish6_enhancer','http://www.sanger.ac.uk/cgi-bin/Projects/D_rerio/zfmodels/ins_mut_map.pl',0);
INSERT INTO `feature_das_sources` VALUES ('DS_184','UniProt','Protein Sequence','netnglyc','http://genome.cbs.dtu.dk:9000/das/netnglyc','http://www.cbs.dtu.dk/services/NetNGlyc',0);
INSERT INTO `feature_das_sources` VALUES ('DS_185','UniProt','Protein Sequence','netacet','http://genome.cbs.dtu.dk:9000/das/netacet','http://www.cbs.dtu.dk/services/NetAcet',0);
INSERT INTO `feature_das_sources` VALUES ('DS_186','UniProt','Protein Sequence','targetp','http://genome.cbs.dtu.dk:9000/das/targetp','http://www.cbs.dtu.dk/services/TargetP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_187','UniProt','Protein Sequence','lipop','http://genome.cbs.dtu.dk:9000/das/lipop','http://www.cbs.dtu.dk/services/LipoP',0);
INSERT INTO `feature_das_sources` VALUES ('DS_188','UniProt','Protein Sequence','netnes','http://genome.cbs.dtu.dk:9000/das/netnes','http://www.cbs.dtu.dk/services/NetNES',0);
INSERT INTO `feature_das_sources` VALUES ('DS_189','UniProt','Protein Sequence','cbs_total','http://genome.cbs.dtu.dk:9000/das/cbs_total','http://www.cbs.dtu.dk/services',0);
INSERT INTO `feature_das_sources` VALUES ('DS_190','ZFISH','Chromosome','ZFMODELS-tilling','http://das.ensembl.org/das/ens_zfish6_tilling','http://www.sanger.ac.uk/Projects/D_rerio/tilling',0);
INSERT INTO `feature_das_sources` VALUES ('DS_191','Ensembl','Protein Sequence','hsa35pep','http://das.sanger.ac.uk/das/hsa35pep','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_193','NCBI','Chromosome','segdup_washu','http://das.ensembl.org/das/ens_35_segdup_washu','http://das.ensembl.org/das/ens_35_segdup_washu',0);
INSERT INTO `feature_das_sources` VALUES ('DS_194','NCBI','Chromosome','segdup_toronto','http://das.ensembl.org/das/ens_35_segdup_toronto','http://das.ensembl.org/das/ens_35_segdup_toronto',0);
INSERT INTO `feature_das_sources` VALUES ('DS_195','NCBI','Chromosome','segdup_wssd','http://das.ensembl.org/das/ens_35_segdup_wssd','http://das.ensembl.org/das/ens_35_segdup_wssd',0);
INSERT INTO `feature_das_sources` VALUES ('DS_196','NCBI','Chromosome','segdup_sanger','http://das.ensembl.org/das/ens_35_segdup_sanger','http://das.ensembl.org/das/ens_35_segdup_sanger',0);
INSERT INTO `feature_das_sources` VALUES ('DS_197','NCBI','Chromosome','segdup_washufilt','http://das.ensembl.org/das/ens_35_segdup_washufilt','http://das.ensembl.org/das/ens_35_segdup_washufilt',0);
INSERT INTO `feature_das_sources` VALUES ('DS_198','Zv','Chromosome','indel_Zv5','http://das.ensembl.org/das/indel_Zv5','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_198','Zv','Scaffold','indel_Zv5','http://das.ensembl.org/das/indel_Zv5','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_201','Zv','Chromosome','snp_Zv5','http://das.ensembl.org/das/snp_Zv5','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_201','Zv','Scaffold','snp_Zv5','http://das.ensembl.org/das/snp_Zv5','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_202','NCBI m','Chromosome','genetarget_m34','http://das.ensembl.org/das/genetarget_m34','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_204','Zv','Scaffold','e32_danio_trans','http://servlet.sanger.ac.uk:8080/das/ensembl_Danio_rerio_core_32_5_trans','http://www.ensembl.org/Danio_rerio',0);
INSERT INTO `feature_das_sources` VALUES ('DS_205','Zv','Scaffold','e32_danio_cds','http://servlet.sanger.ac.uk:8080/das/ensembl_Danio_rerio_core_32_5_cds','http://www.ensembl.org/Danio_rerio',0);
INSERT INTO `feature_das_sources` VALUES ('DS_208','EMBL','Clone','e32_hs_trans','http://servlet.sanger.ac.uk:8080/das/ensembl_Homo_sapiens_core_32_35e_trans','http://www.ensembl.org/Homo_sapiens',0);
INSERT INTO `feature_das_sources` VALUES ('DS_209','EMBL','Clone','e32_hs_cds','http://servlet.sanger.ac.uk:8080/das/ensembl_Homo_sapiens_core_32_35e_cds','http://www.ensembl.org/Homo_sapiens',0);
INSERT INTO `feature_das_sources` VALUES ('DS_210','UniProt','Protein Sequence','SMART','http://smart.embl.de/smart/das/smart','http://smart.embl.de/help/smart_about.shtml',1);
INSERT INTO `feature_das_sources` VALUES ('DS_211','NCBI','Chromosome','RZPD verif. cDNA.','http://das.rzpd.de:9010/das/rzpd-c-human-verified','http://www.rzpd.de/products/sets_libraries',0);
INSERT INTO `feature_das_sources` VALUES ('DS_212','UniProt','Protein Sequence','FUNCut','http://pdg.cnb.uam.es/das/funcut','http://www.pdg.cnb.uam.es/funcut.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_213','MGI','Gene_ID','EURExpress','http://www.eurexpress.org/das/tadb','http://www.eurexpress.org',0);
INSERT INTO `feature_das_sources` VALUES ('DS_213','Ensembl','Gene_ID','EURExpress','http://www.eurexpress.org/das/tadb','http://www.eurexpress.org',0);
INSERT INTO `feature_das_sources` VALUES ('DS_214','NCBI','Chromosome','RZPD ProtExp.','http://das.rzpd.de:9010/das/rzpd-c-human-expression','http://www.rzpd.de/applications/protexpress',0);
INSERT INTO `feature_das_sources` VALUES ('DS_215','NCBI','Chromosome','RZPD esiRNA.','http://das.rzpd.de:9010/das/rzpd-c-human-esirna','http://www.rzpd.de/products/esirna',0);
INSERT INTO `feature_das_sources` VALUES ('DS_216','UniProt','Protein Sequence','PDBsum_protprot','http://www.ebi.ac.uk/das-srv/proteindas/das/sasprot','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_217','UniProt','Protein Sequence','PDBsum_ligands','http://www.ebi.ac.uk/das-srv/proteindas/das/saslig','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_218','UniProt','Protein Sequence','PDBsum_DNAbinding','http://www.ebi.ac.uk/das-srv/proteindas/das/sasdna','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_220','Ensembl','Gene_ID','RZPD Prot Exp','http://das.rzpd.de:9010/das/rzpd-g-human-expression','http://www.rzpd.de/applications/protexpress',0);
INSERT INTO `feature_das_sources` VALUES ('DS_221','Ensembl','Gene_ID','RZPD esiRNA','http://das.rzpd.de:9010/das/rzpd-g-human-esirna','http://www.rzpd.de/products/esirna',0);
INSERT INTO `feature_das_sources` VALUES ('DS_222','NCBI','Chromosome','omim','http://das.ensembl.org/das/ens_ncbi_35_omimg','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_223','NCBI','Chromosome','decipher','http://das.ensembl.org/das/decipher','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_224','NCBI','Chromosome','toronto poly','http://das.ensembl.org/das/toronto_poly','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_225','IPI','Protein Sequence','LIFEdb localisation','http://www.dkfz.de/LIFEdb/das/localization','http://www.lifedb.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_228','GENCODE','Protein Sequence','GENCODE_pfam','http://www.ebi.ac.uk/das-srv/proteindas/das/gencodepfam','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_229','GENCODE','Protein Sequence','GENCODE_pdbsum','http://www.ebi.ac.uk/das-srv/proteindas/das/gencodepdbsum','http://www.ebi.ac.uk/thornton-srv/ENCODE/pdbsum.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_230','NCBI','Chromosome','L1Base NCBI35 FLI','http://l1das.molgen.mpg.de:8080/das/hsflil1_2635','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_231','NCBI','Chromosome','L1Base NCBI35 FLnI','http://l1das.molgen.mpg.de:8080/das/hsflnil1_2635','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_232','NCBI','Chromosome','L1Base NCBI35 ORF2','http://l1das.molgen.mpg.de:8080/das/hsorf2_2635','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_233','UniProt','Protein Sequence','cath_uniprot_mapping','http://cathwww.biochem.ucl.ac.uk:9000/das/cath_sptr','http://www.biochem.ucl.ac.uk/bsm/cath',0);
INSERT INTO `feature_das_sources` VALUES ('DS_234','NCBI','Chromosome','COSMIC','http://das.ensembl.org/das/cosmic_ncbi_35','http://www.sanger.ac.uk/cosmic',0);
INSERT INTO `feature_das_sources` VALUES ('DS_236','NCBI','Chromosome','SNPbox test','http://www.molgenbioinfo.ua.ac.be/cgi-bin/SNPbox/das/SNPbox_human_37_35j','http://www.snpbox.org',0);
INSERT INTO `feature_das_sources` VALUES ('DS_238','EMDB','Volume Map','3D-EM_EMDB','http://biocomp.cnb.uam.es:9000/das/3DEM_EMDB','http://biocomp.cnb.uam.es/das',0);
INSERT INTO `feature_das_sources` VALUES ('DS_244','EMBL','Clone','vega_danio_cds','http://servlet.sanger.ac.uk:8080/das/vega_Danio_rerio_core_38_0606_cds','http://vega.sanger.ac.uk/Danio_rerio',0);
INSERT INTO `feature_das_sources` VALUES ('DS_245','NCBI','Contig','SayaMatcher DAS','http://genome.saitama-med.ac.jp/das/das/profit_human','http://sayamatcher.sourceforge.jp',0);
INSERT INTO `feature_das_sources` VALUES ('DS_246','NCBI m','Contig','SayaMatcher DAS m','http://genome.saitama-med.ac.jp/das/das/profit_mouse','http://sayamatcher.sourceforge.jp',0);
INSERT INTO `feature_das_sources` VALUES ('DS_247','EMBL','Clone','vega_danio_trans','http://servlet.sanger.ac.uk:8080/das/vega_Danio_rerio_core_38_0606_trans','http://vega.sanger.ac.uk/Danio_rerio',0);
INSERT INTO `feature_das_sources` VALUES ('DS_248','Entrez','Gene_ID','HGNC','http://onyx.gene.ucl.ac.uk:9000/das/HGNC','http://www.gene.ucl.ac.uk/nomenclature',0);
INSERT INTO `feature_das_sources` VALUES ('DS_251','Ensembl','Gene_ID','Ensembl Human Genes','http://www.ebi.ac.uk/das-srv/ensembl/das/human_genes','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_311','UniProt','Protein Sequence','Pfam Other Features','http://das.sanger.ac.uk/das/pfamOtherFeatures','http://www.sanger.ac.uk/Software/Pfam',1);
INSERT INTO `feature_das_sources` VALUES ('DS_312','ZFISH','Scaffold','ZFMODELS-Baxendale-ESTs','http://das.ensembl.org/das/ens_zfish6_baxendale_est','http://www.sanger.ac.uk/cgi-bin/Projects/D_rerio/ESTs/sanger_zfish_est_db_search',0);
INSERT INTO `feature_das_sources` VALUES ('DS_313','ZFISH','Scaffold','Carp-ESTs','http://das.ensembl.org/das/ens_zfish6_carp_est','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_314','ZFISH','Chromosome','Morpholinos','http://das.ensembl.org/das/ens_zfish6_morpholino','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_315','ZFISH','Scaffold','vega_zfish_genes','http://das.ensembl.org/das/ens_zfish6_vega_genes','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_316','Ensembl','Gene_ID','arrayexpress','http://www.ebi.ac.uk/das-srv/genedas/das/arrayexpress','http://www.ebi.ac.uk/arrayexpress',0);
INSERT INTO `feature_das_sources` VALUES ('DS_318','TETRAODON','Chromosome','features_tetraodon','http://genome.imim.es:8080/das/features_tetraodon','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_319','WASHUC','Chromosome','features_chicken','http://genome.imim.es:8080/das/features_chicken','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_320','WASHUC','Chromosome','features_SGPchickenxhuman','http://genome.imim.es:8080/das/features_SGPchickenxhuman','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_321','NCBI','Chromosome','features_SGPhumanxmouse','http://genome.imim.es:8080/das/features_SGPhumanxmouse','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_322','NCBI m','Chromosome','features_SGPmousexhuman','http://genome.imim.es:8080/das/features_SGPmousexhuman','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_323','RGSC','Chromosome','features_SGPratxhuman','http://genome.imim.es:8080/das/features_SGPratxhuman','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_324','RGSC','Chromosome','features_rat','http://genome.imim.es:8080/das/features_rat','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_325','NCBI m','Chromosome','features_mouse','http://genome.imim.es:8080/das/features_mouse','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_326','NCBI','Chromosome','features_human','http://genome.imim.es:8080/das/features_human','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_327','UniProt','Protein Sequence','interpro','http://das.ensembl.org/das/interpro','http://www.ebi.ac.uk/interpro',1);
INSERT INTO `feature_das_sources` VALUES ('DS_328','RGSC','Chromosome','rnorf2_233c','http://l1das.molgen.mpg.de:8080/das/rnorf2_233c','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_329','RGSC','Chromosome','rnflnil1_233c','http://l1das.molgen.mpg.de:8080/das/rnflnil1_233c','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_330','RGSC','Chromosome','rnflil1_233c','http://l1das.molgen.mpg.de:8080/das/rnflil1_233c','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_331','RGSC','Chromosome','mmorf2_2433','http://l1das.molgen.mpg.de:8080/das/mmorf2_2433','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_332','RGSC','Chromosome','mmflnil1_2433','http://l1das.molgen.mpg.de:8080/das/mmflnil1_2433','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_333','RGSC','Chromosome','mmflil1_2433','http://l1das.molgen.mpg.de:8080/das/mmflil1_2433','http://line1.molgen.mpg.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_334','NCBI','Chromosome','GSC_mRNA','http://www.genoscope.cns.fr/cgi-bin/ggb/BioSapiens/das/GSC_mRNA','http://www.genoscope.cns.fr/cgi-bin/ggb/BioSapiens/gbrowse/GSC_mRNA?help=citations).',0);
INSERT INTO `feature_das_sources` VALUES ('DS_335','NCBI','Chromosome','ExoFish','http://www.genoscope.cns.fr/cgi-bin/ggb/BioSapiens/das/ExoFish','ttp://www.genoscope.cns.fr/externe/tetraodon/what_exofish.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_336','NCBI','Chromosome','NCBI35cis','http://sysdb.cs.helsinki.fi/das/NCBI35cis','',0);
INSERT INTO `feature_das_sources` VALUES ('DS_337','HIV','Chromosome','viraldas_hiv1_hxb2','http://viraldas.bioinf.mpi-sb.mpg.de/cgi-bin/das/hiv1_hxb2','http://viraldas.bioinf.mpi-sb.mpg.de/cgi-bin/gbrowse/hiv1_hxb2',0);
INSERT INTO `feature_das_sources` VALUES ('DS_338','CEL','Chromosome','features_celegans','http://genome.imim.es:8080/das/features_celegans','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_339','CHIMP','Chromosome','features_chimp','http://genome.imim.es:8080/das/features_chimp','http://genome.imim.es/genepredictions/index.html',0);
INSERT INTO `feature_das_sources` VALUES ('DS_340','Ensembl','Protein Sequence','Tango aggregation','http://switpc7.vub.ac.be:9000/das/tango','http://tango.embl.de',0);
INSERT INTO `feature_das_sources` VALUES ('DS_341','UniProt','Protein Sequence','everest','http://www.protonet.cs.huji.ac.il/das/everest','http://www.protonet.cs.huji.ac.il',0);
INSERT INTO `feature_das_sources` VALUES ('DS_342','UniProt','Protein Sequence','protonet','http://www.protonet.cs.huji.ac.il/das/protonet','http://www.protonet.cs.huji.ac.il',0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_ARMA','ARMA','Chromosome','Dasypus_novemcinctus.ARMA.reference','http://www.ensembl.org/das/Dasypus_novemcinctus.ARMA.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_AaegL1','AaegL','Chromosome','Aedes_aegypti.AaegL1.reference','http://www.ensembl.org/das/Aedes_aegypti.AaegL1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_AgamP3','AgamP','Chromosome','Anopheles_gambiae.AgamP3.reference','http://www.ensembl.org/das/Anopheles_gambiae.AgamP3.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_BDGP4.3','BDGP','Chromosome','Drosophila_melanogaster.BDGP4.3.reference','http://www.ensembl.org/das/Drosophila_melanogaster.BDGP4.3.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_BROADD1','BROADD','Chromosome','Canis_familiaris.BROADD1.reference','http://www.ensembl.org/das/Canis_familiaris.BROADD1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_BROADD2','BROADD','Chromosome','Canis_familiaris.BROADD2.reference','http://www.ensembl.org/das/Canis_familiaris.BROADD2.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_BROADE1','BROADE','Chromosome','Loxodonta_africana.BROADE1.reference','http://www.ensembl.org/das/Loxodonta_africana.BROADE1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_BROADO3','BROADO','Chromosome','Monodelphis_domestica.BROADO3.reference','http://www.ensembl.org/das/Monodelphis_domestica.BROADO3.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_BROADS1','BROADS','Chromosome','Gasterosteus_aculeatus.BROADS1.reference','http://www.ensembl.org/das/Gasterosteus_aculeatus.BROADS1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_Btau_2.0','Btau_','Chromosome','Bos_taurus.Btau_2.0.reference','http://www.ensembl.org/das/Bos_taurus.Btau_2.0.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_CEL160','CEL','Chromosome','Caenorhabditis_elegans.CEL160.reference','http://www.ensembl.org/das/Caenorhabditis_elegans.CEL160.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_CHIMP2.1','CHIMP','Chromosome','Pan_troglodytes.CHIMP2.1.reference','http://www.ensembl.org/das/Pan_troglodytes.CHIMP2.1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_CSAV2.0','CSAV','Chromosome','Ciona_savignyi.CSAV2.0.reference','http://www.ensembl.org/das/Ciona_savignyi.CSAV2.0.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_FUGU4','FUGU','Chromosome','Takifugu_rubripes.FUGU4.reference','http://www.ensembl.org/das/Takifugu_rubripes.FUGU4.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_JGI2','JGI','Chromosome','Ciona_intestinalis.JGI2.reference','http://www.ensembl.org/das/Ciona_intestinalis.JGI2.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_JGI4.1','JGI','Chromosome','Xenopus_tropicalis.JGI4.1.reference','http://www.ensembl.org/das/Xenopus_tropicalis.JGI4.1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_MEDAKA1','MEDAKA','Chromosome','Oryzias_latipes.MEDAKA1.reference','http://www.ensembl.org/das/Oryzias_latipes.MEDAKA1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_MMUL_1','MMUL_','Chromosome','Macaca_mulatta.MMUL_1.reference','http://www.ensembl.org/das/Macaca_mulatta.MMUL_1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_NCBI36','NCBI','Chromosome','Homo_sapiens.NCBI36.reference','http://www.ensembl.org/das/Homo_sapiens.NCBI36.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_NCBIM36','NCBIM','Chromosome','Mus_musculus.NCBIM36.reference','http://www.ensembl.org/das/Mus_musculus.NCBIM36.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_OANA5','OANA','Chromosome','Ornithorhynchus_anatinus.OANA5.reference','http://www.ensembl.org/das/Ornithorhynchus_anatinus.OANA5.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_RABBIT','RABBIT','Chromosome','Oryctolagus_cuniculus.RABBIT.reference','http://www.ensembl.org/das/Oryctolagus_cuniculus.RABBIT.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_RGSC3.4','RGSC','Chromosome','Rattus_norvegicus.RGSC3.4.reference','http://www.ensembl.org/das/Rattus_norvegicus.RGSC3.4.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_SGD1','SGD','Chromosome','Saccharomyces_cerevisiae.SGD1.reference','http://www.ensembl.org/das/Saccharomyces_cerevisiae.SGD1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_SGD1.01','SGD','Chromosome','Saccharomyces_cerevisiae.SGD1.01.reference','http://www.ensembl.org/das/Saccharomyces_cerevisiae.SGD1.01.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_TENREC','TENREC','Chromosome','Echinops_telfairi.TENREC.reference','http://www.ensembl.org/das/Echinops_telfairi.TENREC.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_TETRAODON7','TETRAODON','Chromosome','Tetraodon_nigroviridis.TETRAODON7.reference','http://www.ensembl.org/das/Tetraodon_nigroviridis.TETRAODON7.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_WASHUC1','WASHUC','Chromosome','Gallus_gallus.WASHUC1.reference','http://www.ensembl.org/das/Gallus_gallus.WASHUC1.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_WASHUC2','WASHUC','Chromosome','Gallus_gallus.WASHUC2.reference','http://www.ensembl.org/das/Gallus_gallus.WASHUC2.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_1_ZFISH6','ZFISH','Chromosome','Danio_rerio.ZFISH6.reference','http://www.ensembl.org/das/Danio_rerio.ZFISH6.reference',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_3_BROADD2','BROADD','Chromosome','Canis_familiaris.BROADD2.transcripts','http://www.ensembl.org/das/Canis_familiaris.BROADD2.transcripts',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_3_OANA5','OANA','Chromosome','Ornithorhynchus_anatinus.OANA5.transcripts','http://www.ensembl.org/das/Ornithorhynchus_anatinus.OANA5.transcripts',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_3_SGD1','SGD','Chromosome','Saccharomyces_cerevisiae.SGD1.transcripts','http://www.ensembl.org/das/Saccharomyces_cerevisiae.SGD1.transcripts',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_3_SGD1.01','SGD','Chromosome','Saccharomyces_cerevisiae.SGD1.01.transcripts','http://www.ensembl.org/das/Saccharomyces_cerevisiae.SGD1.01.transcripts',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_3_WASHUC1','WASHUC','Chromosome','Gallus_gallus.WASHUC1.transcripts','http://www.ensembl.org/das/Gallus_gallus.WASHUC1.transcripts',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_3_WASHUC2','WASHUC','Chromosome','Gallus_gallus.WASHUC2.transcripts','http://www.ensembl.org/das/Gallus_gallus.WASHUC2.transcripts',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_4_NCBI36','NCBI','Chromosome','Homo_sapiens.NCBI36.ditags','http://www.ensembl.org/das/Homo_sapiens.NCBI36.ditags',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('ENSEMBL_5_NCBIM36','NCBIM','Chromosome','Mus_musculus.NCBIM36.cagetags','http://www.ensembl.org/das/Mus_musculus.NCBIM36.cagetags',NULL,0);
INSERT INTO `feature_das_sources` VALUES ('DS_985','UniProt','Protein Sequence','Phobius','http://das.sbc.su.se:9000/das/phobius','',1);

--
-- Table structure for table `error_log`
--

DROP TABLE IF EXISTS `error_log`;
CREATE TABLE `error_log` (
  `auto_error` int(11) NOT NULL auto_increment,
  `message` longtext NOT NULL,
  `num` int(11) default '0',
  `first` timestamp NOT NULL default '0000-00-00 00:00:00',
  `last` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  PRIMARY KEY  (`auto_error`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `family_count`
--

DROP TABLE IF EXISTS `family_count`;
CREATE TABLE `family_count` (
  `auto_pfamA` int(10) NOT NULL default '0',
  `pfamA_acc` varchar(7) NOT NULL default '',
  `pfamA_id` varchar(40) NOT NULL default '',
  `view_count` int(10) default '0',
  PRIMARY KEY  (`auto_pfamA`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `job_data`
--

DROP TABLE IF EXISTS `job_data`;
CREATE TABLE `job_data` (
  `job_id` int(10) unsigned NOT NULL auto_increment,
  `data_id` tinytext NOT NULL,
  `data_string` longblob NOT NULL,
  KEY `job_data_job_id_idx` (`job_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `job_results`
--

DROP TABLE IF EXISTS `job_results`;
CREATE TABLE `job_results` (
  `job_id` int(10) unsigned NOT NULL auto_increment,
  `result_xml` longblob NOT NULL,
  KEY `job_results_job_id` (`job_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `job_status`
--

DROP TABLE IF EXISTS `job_status`;
CREATE TABLE `job_status` (
  `job_id` int(10) unsigned NOT NULL auto_increment,
  `part` int(10) unsigned NOT NULL default '0',
  `part_name` tinytext,
  `job_status` tinytext NOT NULL,
  `internal_id` int(10) unsigned default NULL,
  KEY `job_status_job_id_idx` (`job_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `job_submission`
--

DROP TABLE IF EXISTS `job_submission`;
CREATE TABLE `job_submission` (
  `job_id` int(10) unsigned NOT NULL auto_increment,
  `job_unique_id` tinytext,
  `referrer` tinytext,
  `web_service_call` tinytext,
  `parameters` tinytext,
  `date` timestamp NOT NULL default CURRENT_TIMESTAMP,
  PRIMARY KEY  (`job_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Table structure for table `news`
--

DROP TABLE IF EXISTS `news`;
CREATE TABLE `news` (
  `auto_news` int(11) NOT NULL auto_increment,
  `author` varchar(50) NOT NULL default '',
  `pubDate` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `title` text NOT NULL,
  `summary` text NOT NULL,
  `description` text NOT NULL,
  `live` tinyint(1) default '0',
  PRIMARY KEY  (`auto_news`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `news`
--

LOCK TABLES `news` WRITE;
/*!40000 ALTER TABLE `news` DISABLE KEYS */;
INSERT INTO `news` VALUES (7,'jt6@sanger.ac.uk','2006-12-11 15:58:02','Pfam 21.0 released','Pfam 21.0 isnow available from the Wellcome Trust Sanger Institute','The latest release contains 8957 protein domainfamilies, matching 74% of sequences from UniProt 8.0. This release includes 669 new families and 8 families have been deleted since the last release.',1),(8,'jt6@sanger.ac.uk','2006-12-10 16:16:20','New Pfam website enters beta test','The entirely re-developed Pfam website is now available for internal testing.','The Pfam website has been re-written from the ground up.',1),(10,'jt6@sanger.ac.uk','2007-02-06 16:55:00','Pfam website installed in Stockholm','The new Pfam website was successfully installed in the Sonnhammer lab, Stockholm.','The Pfam website is intended to replace the various different Pfam &quot;mirror&quot;sites with a single site, sporting a unified look-and-feel and providing a consistent set of tools and features across all sites. The first step towards this was the successful installation of the Pfam site inthe lab of Pfam member Eric Sonnhammer.',1);
/*!40000 ALTER TABLE `news` ENABLE KEYS */;
UNLOCK TABLES;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

