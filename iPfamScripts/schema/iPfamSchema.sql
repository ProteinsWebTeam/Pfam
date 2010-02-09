-- MySQL dump 10.11
--
-- Host: 127.0.0.1    Database: iPfam
-- ------------------------------------------------------
-- Server version	5.0.41

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `ddi`
--

DROP TABLE IF EXISTS `ddi`;
CREATE TABLE `ddi` (
  `region_id_A` int(10) unsigned NOT NULL,
  `region_id_B` int(10) unsigned NOT NULL,
  `residue_A` int(11) NOT NULL,
  `residue_B` int(11) NOT NULL,
  `intrachain` tinyint(4) NOT NULL,
  `quality_control` int(10) unsigned NOT NULL,
  KEY `ddi_quality_control_Idx` (`quality_control`),
  KEY `ddi_region_id_A_region_id_B_Idx` (`region_id_A`,`region_id_B`),
  KEY `ddi_residue_A_residue_B_Idx` (`residue_A`,`residue_B`),
  KEY `fk_ddi_region_B` (`region_id_B`),
  CONSTRAINT `fk_ddi_region_B` FOREIGN KEY (`region_id_B`) REFERENCES `domain` (`region_id`),
  CONSTRAINT `fk_ddi_region_A` FOREIGN KEY (`region_id_A`) REFERENCES `domain` (`region_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='domain domain interactions';

--
-- Dumping data for table `ddi`
--

LOCK TABLES `ddi` WRITE;
/*!40000 ALTER TABLE `ddi` DISABLE KEYS */;
/*!40000 ALTER TABLE `ddi` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dli`
--

DROP TABLE IF EXISTS `dli`;
CREATE TABLE `dli` (
  `region_id` int(10) unsigned NOT NULL,
  `internal_ligand_id` varchar(10) NOT NULL,
  `region_residue` int(11) NOT NULL,
  `ligand_residue` int(11) NOT NULL,
  `quality_control` int(10) unsigned NOT NULL,
  KEY `dli_internal_ligand_id_Idx` (`internal_ligand_id`),
  KEY `dli_ligand_residue_Idx` (`ligand_residue`),
  KEY `dli_quality_control_Idx` (`quality_control`),
  KEY `dli_region_id_internal_ligand_id_Idx` (`region_id`,`internal_ligand_id`),
  KEY `dli_region_residue_Idx` (`region_residue`),
  CONSTRAINT `fk_dli_region` FOREIGN KEY (`region_id`) REFERENCES `domain` (`region_id`),
  CONSTRAINT `fk_dli_ligands` FOREIGN KEY (`internal_ligand_id`) REFERENCES `ligands` (`internal_ligand_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='domain ligand interactions';

--
-- Dumping data for table `dli`
--

LOCK TABLES `dli` WRITE;
/*!40000 ALTER TABLE `dli` DISABLE KEYS */;
/*!40000 ALTER TABLE `dli` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `domain`
--

DROP TABLE IF EXISTS `domain`;
CREATE TABLE `domain` (
  `pfam_acc` varchar(8) NOT NULL,
  `protein_accession` varchar(6) NOT NULL,
  `start` int(11) NOT NULL,
  `end` int(11) NOT NULL,
  `region_id` int(10) unsigned NOT NULL auto_increment,
  `region_source_db` varchar(12) NOT NULL,
  `protein_id` varchar(12) NOT NULL,
  PRIMARY KEY  (`region_id`),
  KEY `region_pfam_acc_Idx` (`pfam_acc`),
  KEY `region_protein_accession_start_end_Idx` (`protein_accession`,`start`,`end`),
  KEY `fk_region_proteins` (`protein_accession`,`protein_id`),
  CONSTRAINT `fk_region_proteins` FOREIGN KEY (`protein_accession`, `protein_id`) REFERENCES `protein` (`accession`, `id`),
  CONSTRAINT `fk_region_pfamA` FOREIGN KEY (`pfam_acc`) REFERENCES `pfama` (`pfamA_acc`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `domain`
--

LOCK TABLES `domain` WRITE;
/*!40000 ALTER TABLE `domain` DISABLE KEYS */;
/*!40000 ALTER TABLE `domain` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `interface`
--

DROP TABLE IF EXISTS `interface`;
CREATE TABLE `interface` (
  `structure_accession` int(10) unsigned NOT NULL,
  `chain_A` char(1) NOT NULL,
  `chain_B` char(1) NOT NULL,
  `residue_A` int(11) NOT NULL,
  `residue_B` int(11) NOT NULL,
  `bond` varchar(6) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `interface`
--

LOCK TABLES `interface` WRITE;
/*!40000 ALTER TABLE `interface` DISABLE KEYS */;
/*!40000 ALTER TABLE `interface` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ligand_chemistry`
--

DROP TABLE IF EXISTS `ligand_chemistry`;
CREATE TABLE `ligand_chemistry` (
  `ligand_id` int(11) NOT NULL,
  `lig_code` varchar(10) NOT NULL,
  `three_letter_code` char(3) NOT NULL,
  `one_letter_code` varchar(5) NOT NULL,
  `name` text NOT NULL,
  `systematic_name` text NOT NULL,
  `num_all_atoms` int(10) unsigned NOT NULL,
  `num_atoms_no_H` int(10) unsigned NOT NULL,
  `stereo_smiles` text NOT NULL,
  `non_stereo_smiles` bigint(20) NOT NULL,
  `charge` int(11) NOT NULL,
  `category` text NOT NULL,
  `formula` text NOT NULL,
  `molecular_weight` float NOT NULL,
  UNIQUE KEY `ligand_chemistry_code_Idx` (`lig_code`),
  UNIQUE KEY `ligand_chemistry_ligand_id_Idx` (`ligand_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `ligand_chemistry`
--

LOCK TABLES `ligand_chemistry` WRITE;
/*!40000 ALTER TABLE `ligand_chemistry` DISABLE KEYS */;
/*!40000 ALTER TABLE `ligand_chemistry` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ligand_int_atoms`
--

DROP TABLE IF EXISTS `ligand_int_atoms`;
CREATE TABLE `ligand_int_atoms` (
  `internal_ligand_id` varchar(10) NOT NULL,
  `atom_number` int(11) NOT NULL,
  `residue` bigint(20) NOT NULL,
  `atom_acc` int(10) unsigned NOT NULL auto_increment,
  `atom` bigint(20) NOT NULL,
  PRIMARY KEY  (`atom_acc`),
  KEY `ligand_int_atoms_atom_acc_Idx` (`atom_acc`),
  KEY `ligand_int_atoms_internal_ligand_id_Idx` (`internal_ligand_id`),
  CONSTRAINT `fk_ligand_atoms_ligands` FOREIGN KEY (`internal_ligand_id`) REFERENCES `ligands` (`internal_ligand_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `ligand_int_atoms`
--

LOCK TABLES `ligand_int_atoms` WRITE;
/*!40000 ALTER TABLE `ligand_int_atoms` DISABLE KEYS */;
/*!40000 ALTER TABLE `ligand_int_atoms` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ligand_synonyms`
--

DROP TABLE IF EXISTS `ligand_synonyms`;
CREATE TABLE `ligand_synonyms` (
  `ligand_id` int(11) NOT NULL,
  `alt_name` text NOT NULL,
  KEY `ligand_synonyms_ligand_id_Idx` (`ligand_id`),
  CONSTRAINT `fk_ligand_synonyms_ligand_chemistry` FOREIGN KEY (`ligand_id`) REFERENCES `ligand_chemistry` (`ligand_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `ligand_synonyms`
--

LOCK TABLES `ligand_synonyms` WRITE;
/*!40000 ALTER TABLE `ligand_synonyms` DISABLE KEYS */;
/*!40000 ALTER TABLE `ligand_synonyms` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ligands`
--

DROP TABLE IF EXISTS `ligands`;
CREATE TABLE `ligands` (
  `accession` varchar(5) NOT NULL,
  `ligand_id` int(11) NOT NULL,
  `ligand_number` int(11) NOT NULL,
  `chain` varchar(4) NOT NULL,
  `atom_start` int(11) NOT NULL,
  `atom_end` int(11) NOT NULL,
  `internal_ligand_id` varchar(10) NOT NULL,
  PRIMARY KEY  (`internal_ligand_id`),
  KEY `ligands_accession_Idx` (`accession`),
  KEY `ligands_internal_ligand_id_Idx` (`internal_ligand_id`),
  KEY `ligands_ligand_id_Idx` (`ligand_id`),
  KEY `ligands_ligand_number_Idx` (`ligand_number`),
  CONSTRAINT `fk_structure2ligand_structure` FOREIGN KEY (`accession`) REFERENCES `pdb` (`pdb_id`),
  CONSTRAINT `fk_ligands_ligand_chemistry` FOREIGN KEY (`ligand_id`) REFERENCES `ligand_chemistry` (`ligand_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `ligands`
--

LOCK TABLES `ligands` WRITE;
/*!40000 ALTER TABLE `ligands` DISABLE KEYS */;
/*!40000 ALTER TABLE `ligands` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nadi`
--

DROP TABLE IF EXISTS `nadi`;
CREATE TABLE `nadi` (
  `nucleic_acid_id` varchar(12) NOT NULL,
  `base` int(11) NOT NULL,
  `base_type` varchar(5) NOT NULL,
  `region_id` int(10) unsigned NOT NULL,
  `region_residue` int(11) NOT NULL,
  `residue_type` bigint(20) NOT NULL,
  `quality_control` int(10) unsigned NOT NULL,
  KEY `fk_nadi_domain` (`region_id`),
  CONSTRAINT `fk_nadi_domain` FOREIGN KEY (`region_id`) REFERENCES `domain` (`region_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `nadi`
--

LOCK TABLES `nadi` WRITE;
/*!40000 ALTER TABLE `nadi` DISABLE KEYS */;
/*!40000 ALTER TABLE `nadi` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `napi`
--

DROP TABLE IF EXISTS `napi`;
CREATE TABLE `napi` (
  `nucleic_acid_id` varchar(12) NOT NULL,
  `protein_id` varchar(12) NOT NULL,
  `quality_control` int(10) unsigned NOT NULL,
  KEY `napi_nucleic_acid_id_protein_id_Idx` (`nucleic_acid_id`,`protein_id`),
  KEY `napi_protein_id_Idx` (`protein_id`),
  KEY `napi_quality_control_Idx` (`quality_control`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `napi`
--

LOCK TABLES `napi` WRITE;
/*!40000 ALTER TABLE `napi` DISABLE KEYS */;
/*!40000 ALTER TABLE `napi` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nardi`
--

DROP TABLE IF EXISTS `nardi`;
CREATE TABLE `nardi` (
  `protein_region_id` int(10) unsigned NOT NULL,
  `nucleic_region_id` int(10) unsigned NOT NULL,
  `protein_residue` int(11) NOT NULL,
  `nucleic_base` bigint(20) NOT NULL,
  `quality_control` int(10) unsigned NOT NULL,
  KEY `fk_nardi_domain` (`protein_region_id`),
  KEY `fk_nardi_nucleic_acid_region` (`nucleic_region_id`),
  CONSTRAINT `fk_nardi_nucleic_acid_region` FOREIGN KEY (`nucleic_region_id`) REFERENCES `nucleic_acid_region` (`region_id`),
  CONSTRAINT `fk_nardi_domain` FOREIGN KEY (`protein_region_id`) REFERENCES `domain` (`region_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `nardi`
--

LOCK TABLES `nardi` WRITE;
/*!40000 ALTER TABLE `nardi` DISABLE KEYS */;
/*!40000 ALTER TABLE `nardi` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nucleic_acid`
--

DROP TABLE IF EXISTS `nucleic_acid`;
CREATE TABLE `nucleic_acid` (
  `accession` varchar(6) NOT NULL,
  `id` varchar(12) NOT NULL,
  `seq_version` bigint(20) NOT NULL,
  `type` varchar(3) NOT NULL,
  `source_db` int(11) NOT NULL,
  `length` int(11) NOT NULL,
  `ncbi_code` int(11) NOT NULL,
  `sequence` blob NOT NULL,
  KEY `fk_nucleic_acid_pdb` (`accession`),
  CONSTRAINT `fk_nucleic_acid_pdb` FOREIGN KEY (`accession`) REFERENCES `pdb` (`pdb_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `nucleic_acid`
--

LOCK TABLES `nucleic_acid` WRITE;
/*!40000 ALTER TABLE `nucleic_acid` DISABLE KEYS */;
/*!40000 ALTER TABLE `nucleic_acid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nucleic_acid_int_atoms`
--

DROP TABLE IF EXISTS `nucleic_acid_int_atoms`;
CREATE TABLE `nucleic_acid_int_atoms` (
  `nucleic_acid_acc` varchar(6) NOT NULL,
  `atom_number` int(11) NOT NULL,
  `nucleic_acid_id` varchar(12) NOT NULL,
  `base` int(11) NOT NULL,
  `base_name` varchar(5) NOT NULL,
  `atom` varchar(3) NOT NULL,
  `atom_acc` int(11) NOT NULL,
  KEY `nucleic_acid_int_atoms_atom_acc_Idx` (`atom_acc`),
  KEY `nucleic_acid_int_atoms_nucleic_acid_acc_Idx` (`nucleic_acid_acc`),
  KEY `nucleic_acid_int_atoms_nucleic_acid_id_Idx` (`nucleic_acid_id`),
  CONSTRAINT `fk_nucleic_acid_int_atoms_nucleic_acid` FOREIGN KEY (`nucleic_acid_acc`) REFERENCES `nucleic_acid` (`accession`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `nucleic_acid_int_atoms`
--

LOCK TABLES `nucleic_acid_int_atoms` WRITE;
/*!40000 ALTER TABLE `nucleic_acid_int_atoms` DISABLE KEYS */;
/*!40000 ALTER TABLE `nucleic_acid_int_atoms` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `nucleic_acid_region`
--

DROP TABLE IF EXISTS `nucleic_acid_region`;
CREATE TABLE `nucleic_acid_region` (
  `rfam_acc` varchar(8) NOT NULL,
  `accession` varchar(6) default NULL,
  `start` int(11) NOT NULL,
  `end` int(11) NOT NULL,
  `region_id` int(10) unsigned NOT NULL,
  `region_source_db` varchar(12) NOT NULL,
  `nucleic_acid_id` varchar(12) NOT NULL,
  PRIMARY KEY  (`region_id`),
  KEY `nucleic_acid_region_accession_Idx` (`accession`),
  KEY `nucleic_acid_region_rfam_acc_Idx` (`rfam_acc`),
  CONSTRAINT `fk_nucleic_acid_region_rfam` FOREIGN KEY (`rfam_acc`) REFERENCES `rfam` (`rfam_acc`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `nucleic_acid_region`
--

LOCK TABLES `nucleic_acid_region` WRITE;
/*!40000 ALTER TABLE `nucleic_acid_region` DISABLE KEYS */;
/*!40000 ALTER TABLE `nucleic_acid_region` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `pdb`
--

DROP TABLE IF EXISTS `pdb`;
CREATE TABLE `pdb` (
  `accession` int(10) unsigned NOT NULL auto_increment,
  `pdb_id` varchar(5) NOT NULL default 'NULL',
  `header` text,
  `title` text,
  `date` date NOT NULL,
  `resolution` decimal(5,2) unsigned NOT NULL,
  `experiment_short` text NOT NULL,
  `experiment_long` text NOT NULL,
  `pubmed_id` int(11) NOT NULL,
  `biological_Unit` tinyint(4) NOT NULL,
  PRIMARY KEY  (`accession`),
  UNIQUE KEY `pdb_accession_Idx` (`accession`),
  UNIQUE KEY `pdb_pdb_id_Idx` (`pdb_id`),
  KEY `pdb_biological_Unit_Idx` (`biological_Unit`),
  CONSTRAINT `fk_structure_protein` FOREIGN KEY (`pdb_id`) REFERENCES `protein` (`accession`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `pdb`
--

LOCK TABLES `pdb` WRITE;
/*!40000 ALTER TABLE `pdb` DISABLE KEYS */;
/*!40000 ALTER TABLE `pdb` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `pdb_author`
--

DROP TABLE IF EXISTS `pdb_author`;
CREATE TABLE `pdb_author` (
  `accession` int(10) unsigned NOT NULL,
  `author_order` int(11) NOT NULL,
  `last_name` text NOT NULL,
  `name_initials` text NOT NULL,
  KEY `pdb_author_accession_Idx` (`accession`),
  CONSTRAINT `fk_pdb_author_pdb` FOREIGN KEY (`accession`) REFERENCES `pdb` (`accession`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `pdb_author`
--

LOCK TABLES `pdb_author` WRITE;
/*!40000 ALTER TABLE `pdb_author` DISABLE KEYS */;
/*!40000 ALTER TABLE `pdb_author` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `pdb_residue_data`
--

DROP TABLE IF EXISTS `pdb_residue_data`;
CREATE TABLE `pdb_residue_data` (
  `pdb_id` varchar(5) NOT NULL,
  `chain` char(1) NOT NULL,
  `uniprot_acc` varchar(6) NOT NULL,
  `pdb_coordinate` int(11) NOT NULL,
  `uniprot_coordinate` int(11) NOT NULL,
  KEY `pdb_residue_data_pdb_id_Idx` (`pdb_id`),
  KEY `pdb_residue_data_uniprot_acc_Idx` (`uniprot_acc`),
  CONSTRAINT `fk_MSD_data_protein_uniprot` FOREIGN KEY (`uniprot_acc`) REFERENCES `protein` (`accession`),
  CONSTRAINT `fk_MSD_data_protein_pdb` FOREIGN KEY (`pdb_id`) REFERENCES `protein` (`accession`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `pdb_residue_data`
--

LOCK TABLES `pdb_residue_data` WRITE;
/*!40000 ALTER TABLE `pdb_residue_data` DISABLE KEYS */;
/*!40000 ALTER TABLE `pdb_residue_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `pfama`
--

DROP TABLE IF EXISTS `pfama`;
CREATE TABLE `pfama` (
  `pfamA_acc` varchar(8) NOT NULL,
  `pfamA_id` varchar(20) NOT NULL,
  `NumberInAlign` int(11) NOT NULL,
  `description` text NOT NULL,
  PRIMARY KEY  (`pfamA_acc`),
  KEY `pfamA_pfamA_acc_Idx` (`pfamA_acc`),
  KEY `pfamA_pfamA_id_Idx` (`pfamA_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `pfama`
--

LOCK TABLES `pfama` WRITE;
/*!40000 ALTER TABLE `pfama` DISABLE KEYS */;
/*!40000 ALTER TABLE `pfama` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `pli`
--

DROP TABLE IF EXISTS `pli`;
CREATE TABLE `pli` (
  `protein_id` varchar(12) NOT NULL,
  `internal_ligand_id` varchar(10) NOT NULL,
  KEY `pli_internal_ligand_id_Idx` (`internal_ligand_id`),
  KEY `pli_protein_id_internal_ligand_id_Idx` (`protein_id`,`internal_ligand_id`),
  CONSTRAINT `fk_pli_protein` FOREIGN KEY (`protein_id`) REFERENCES `protein` (`accession`),
  CONSTRAINT `fk_pli_ligands` FOREIGN KEY (`internal_ligand_id`) REFERENCES `ligands` (`internal_ligand_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `pli`
--

LOCK TABLES `pli` WRITE;
/*!40000 ALTER TABLE `pli` DISABLE KEYS */;
/*!40000 ALTER TABLE `pli` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ppi`
--

DROP TABLE IF EXISTS `ppi`;
CREATE TABLE `ppi` (
  `accession_A` varchar(6) NOT NULL,
  `accession_B` varchar(6) NOT NULL,
  `quality_control` int(10) unsigned NOT NULL,
  KEY `ppi_accession_A_accession_B_Idx` (`accession_A`,`accession_B`),
  KEY `ppi_accession_B_Idx` (`accession_B`),
  KEY `ppi_quality_control_Idx` (`quality_control`),
  CONSTRAINT `fk_ppi_proteins_B` FOREIGN KEY (`accession_B`) REFERENCES `protein` (`accession`),
  CONSTRAINT `fk_ppi_proteins_A` FOREIGN KEY (`accession_A`) REFERENCES `protein` (`accession`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='protein protein interactions';

--
-- Dumping data for table `ppi`
--

LOCK TABLES `ppi` WRITE;
/*!40000 ALTER TABLE `ppi` DISABLE KEYS */;
/*!40000 ALTER TABLE `ppi` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `protein`
--

DROP TABLE IF EXISTS `protein`;
CREATE TABLE `protein` (
  `accession` varchar(6) NOT NULL COMMENT 'The accession field can also contain chain information',
  `id` varchar(12) default 'NULL',
  `seq_version` int(10) unsigned default NULL,
  `md5` varchar(32) NOT NULL,
  `source_db` int(10) unsigned NOT NULL,
  `length` int(10) unsigned NOT NULL,
  `ncbi_code` int(11) default NULL,
  `sequence` blob NOT NULL,
  UNIQUE KEY `proteins_accession_id_Idx` (`accession`,`id`),
  KEY `proteins_md5_Idx` (`md5`),
  KEY `proteins_ncbi_code_Idx` (`ncbi_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `protein`
--

LOCK TABLES `protein` WRITE;
/*!40000 ALTER TABLE `protein` DISABLE KEYS */;
/*!40000 ALTER TABLE `protein` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `protein_int_atoms`
--

DROP TABLE IF EXISTS `protein_int_atoms`;
CREATE TABLE `protein_int_atoms` (
  `protein_acc` varchar(6) NOT NULL,
  `atom_number` int(10) unsigned NOT NULL,
  `protein_id` varchar(12) NOT NULL,
  `residue` int(11) NOT NULL,
  `atom_acc` int(10) unsigned NOT NULL auto_increment,
  `atom` varchar(3) NOT NULL,
  PRIMARY KEY  (`atom_acc`),
  KEY `protein_int_atoms_atom_acc_Idx` (`atom_acc`),
  KEY `protein_int_atoms_protein_acc_Idx` (`protein_acc`),
  KEY `protein_int_atoms_protein_id_residue_Idx` (`protein_id`,`residue`),
  KEY `fk_atoms_protein` (`protein_acc`,`protein_id`),
  CONSTRAINT `fk_atoms_protein` FOREIGN KEY (`protein_acc`, `protein_id`) REFERENCES `protein` (`accession`, `id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `protein_int_atoms`
--

LOCK TABLES `protein_int_atoms` WRITE;
/*!40000 ALTER TABLE `protein_int_atoms` DISABLE KEYS */;
/*!40000 ALTER TABLE `protein_int_atoms` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `protein_ligand_bonds`
--

DROP TABLE IF EXISTS `protein_ligand_bonds`;
CREATE TABLE `protein_ligand_bonds` (
  `protein_atom` int(10) unsigned NOT NULL,
  `ligand_atom` int(10) unsigned NOT NULL,
  `bond_type` varchar(12) NOT NULL,
  `distance` float NOT NULL,
  KEY `protein_ligand_bonds_bond_type_Idx` (`bond_type`),
  KEY `protein_ligand_bonds_ligand_atom_Idx` (`ligand_atom`),
  KEY `protein_ligand_bonds_protein_atom_ligand_atom_Idx` (`protein_atom`,`ligand_atom`),
  CONSTRAINT `fk_protein_ligand_atoms_ligand_int_atoms` FOREIGN KEY (`ligand_atom`) REFERENCES `ligand_int_atoms` (`atom_acc`),
  CONSTRAINT `fk_protein_ligand_atoms_protein_int_atoms` FOREIGN KEY (`protein_atom`) REFERENCES `protein_int_atoms` (`atom_acc`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `protein_ligand_bonds`
--

LOCK TABLES `protein_ligand_bonds` WRITE;
/*!40000 ALTER TABLE `protein_ligand_bonds` DISABLE KEYS */;
/*!40000 ALTER TABLE `protein_ligand_bonds` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `protein_nucleic_acid_bonds`
--

DROP TABLE IF EXISTS `protein_nucleic_acid_bonds`;
CREATE TABLE `protein_nucleic_acid_bonds` (
  `nucleic_acid_atom` int(11) NOT NULL,
  `protein_atom` int(10) unsigned NOT NULL,
  `bond_type` varchar(12) NOT NULL,
  `distance` float NOT NULL,
  KEY `protein_nucleic_acid_bonds_bond_type_Idx` (`bond_type`),
  KEY `protein_nucleic_acid_bonds_nucleic_acid_atom_protein_atom_Idx` (`nucleic_acid_atom`,`protein_atom`),
  KEY `protein_nucleic_acid_bonds_protein_atom_Idx` (`protein_atom`),
  CONSTRAINT `fk_protein_nucleic_acid_atoms_protein_int_atoms` FOREIGN KEY (`protein_atom`) REFERENCES `protein_int_atoms` (`atom_acc`),
  CONSTRAINT `fk_protein_nucleic_acid_atoms_nucleic_acid_int_atoms` FOREIGN KEY (`nucleic_acid_atom`) REFERENCES `nucleic_acid_int_atoms` (`atom_acc`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `protein_nucleic_acid_bonds`
--

LOCK TABLES `protein_nucleic_acid_bonds` WRITE;
/*!40000 ALTER TABLE `protein_nucleic_acid_bonds` DISABLE KEYS */;
/*!40000 ALTER TABLE `protein_nucleic_acid_bonds` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `protein_protein_bonds`
--

DROP TABLE IF EXISTS `protein_protein_bonds`;
CREATE TABLE `protein_protein_bonds` (
  `atom_A` int(10) unsigned NOT NULL,
  `atom_B` int(10) unsigned NOT NULL,
  `bond_type` varchar(12) NOT NULL,
  `distance` float unsigned NOT NULL,
  `intrachain` tinyint(4) NOT NULL,
  KEY `protein_protein_bonds_atom_A_atom_B_Idx` (`atom_A`,`atom_B`),
  KEY `protein_protein_bonds_atom_B_Idx` (`atom_B`),
  KEY `protein_protein_bonds_bond_type_Idx` (`bond_type`),
  CONSTRAINT `fk_bond_atoms_B` FOREIGN KEY (`atom_B`) REFERENCES `protein_int_atoms` (`atom_acc`),
  CONSTRAINT `fk_bond_atoms_A` FOREIGN KEY (`atom_A`) REFERENCES `protein_int_atoms` (`atom_acc`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `protein_protein_bonds`
--

LOCK TABLES `protein_protein_bonds` WRITE;
/*!40000 ALTER TABLE `protein_protein_bonds` DISABLE KEYS */;
/*!40000 ALTER TABLE `protein_protein_bonds` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quality_control`
--

DROP TABLE IF EXISTS `quality_control`;
CREATE TABLE `quality_control` (
  `quality_control` int(10) unsigned NOT NULL auto_increment,
  `method` varchar(10) NOT NULL,
  `score` float NOT NULL,
  `comment` text NOT NULL,
  PRIMARY KEY  (`quality_control`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `quality_control`
--

LOCK TABLES `quality_control` WRITE;
/*!40000 ALTER TABLE `quality_control` DISABLE KEYS */;
/*!40000 ALTER TABLE `quality_control` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `rfam`
--

DROP TABLE IF EXISTS `rfam`;
CREATE TABLE `rfam` (
  `rfam_acc` varchar(8) NOT NULL,
  `rfam_id` varchar(20) NOT NULL,
  `decsription` text NOT NULL,
  PRIMARY KEY  (`rfam_acc`),
  KEY `rfam_rfam_acc_Idx` (`rfam_acc`),
  KEY `rfam_rfam_id_Idx` (`rfam_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `rfam`
--

LOCK TABLES `rfam` WRITE;
/*!40000 ALTER TABLE `rfam` DISABLE KEYS */;
/*!40000 ALTER TABLE `rfam` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2007-10-25  9:37:29
