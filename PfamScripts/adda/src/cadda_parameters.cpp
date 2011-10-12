//--------------------------------------------------------------------------------
// Project adda
//
// Copyright (C) 2003 Andreas Heger All rights reserved
//
// Author: Andreas Heger <heger@ebi.ac.uk>
//
// $Id: adda.cpp,v 1.3 2006/06/15 09:19:32 aheger Exp $
//--------------------------------------------------------------------------------    

#include <math.h>

#include <iostream>
#include <fstream>
#include <iomanip>
#include <iterator>

#include <cstdio>

#include <string>
#include <map>
#include <vector>
#include <list>
#include <set>

#include "adda.h"

// #define DEBUG

using namespace std;

//--------------global parameters ------------------------------------------------------------------
unsigned int param_loglevel = 1;

/* file names */
std::string param_file_name_trees = "adda.segments";
std::string param_file_name_graph = "adda.graph";
std::string param_file_name_index = "adda.graph.idx";
std::string param_file_name_nids = "adda.nids";
std::string param_file_name_transfers = "adda.transfers";
std::string param_file_name_domains = "adda.domains";
std::string param_file_name_domain_graph = "adda.domain_graph";
std::string param_file_name_mst = "adda.mst";

/* various options */
bool param_disallow_shortening = false;
bool param_descend = false;
int param_max_iterations = 10;
bool param_use_file_nids = false;

double param_resolution = 1.0;

/* function parameters for sigmoid */
double param_real_k = 100;	// smooth sigmoid (has to incorporate the resolution (real k = resolution * k)
double param_real_c = 10;	// average domain size of 100
double param_real_max = 1.0;
double param_real_min = 0.0;

bool param_relative_overhang = false;
double param_real_e = 0.05; // Probability of alignment ending directly in alignment is 5% 
double param_real_f = 0.0;

// safety threshold for small overlaps (should be in the range of the resolution);
int param_threshold_overlap = (int)(12.0 / param_resolution);

int param_report_step = 10000;

bool param_only_query = false;	// if true, calculate only score for query

// for convert
int param_min_residues_overlap = 10;
double param_min_coverage = 0.2;
double param_min_relative_overlap = 0.2;
int param_mode = 1;

double param_evalue_threshold_trusted_links = -12;

// minimum domain size (used for merging links)
int param_min_domain_size = 30;

// for mst
size_t param_initial_graph_size;

// parameter setters

void cadda_setFilenameSegments( const char * f) { param_file_name_trees = f; }
void cadda_setFilenameGraph( const char * f) { param_file_name_graph = f; }
void cadda_setFilenameIndex( const char * f) { param_file_name_index = f; }
void cadda_setFilenameNids( const char * f) { param_file_name_nids = f; }
void cadda_setFilenameDomains( const char * f) { param_file_name_domains = f; }
void cadda_setFilenameDomainGraph( const char * f) { param_file_name_domain_graph = f; }
void cadda_setFilenameTransfers( const char * f) { param_file_name_transfers = f; }
void cadda_setFilenameMst( const char * f) { param_file_name_mst = f; }

void cadda_setDisallowShortening( int f) { param_disallow_shortening = f; }
void cadda_setDescend( int f) { param_descend = f; }
void cadda_setUseFileNids( int f) { param_use_file_nids = f; }
void cadda_setOnlyQuery( int f) { param_only_query = f; }
void cadda_setRelativeOverhang( int f) { param_relative_overhang = f; }

void cadda_setMaxIterations( int f) { param_max_iterations = f; }

void cadda_setResolution( double f) { param_resolution = f; }
void cadda_setK( double f) { param_real_k = f; }
void cadda_setC( double f) { param_real_c = f; }
void cadda_setMax( double f) { param_real_max = f; }
void cadda_setMin( double f) { param_real_min = f; }
void cadda_setE( double f) { param_real_e = f; }
void cadda_setF( double f) { param_real_f = f; }
void cadda_setLogLevel( int f) { param_loglevel = f; }
void cadda_setReportStep( int f) { param_report_step = f; }
void cadda_setInitialGraphSize( size_t s) { param_initial_graph_size = s; }

void cadda_setEvalueThresholdTrustedLinks( double f) { param_evalue_threshold_trusted_links = f; }

//------------------------------------------------------------
void cadda_dump_parameters()
{
	std::cout << "####################################################" << std::endl;
	std::cout << "# general options:" << std::endl;
	std::cout << "# loglevel			  			  : " << param_loglevel << std::endl;
	std::cout << "# report step		  			  : " << param_report_step << std::endl;
	std::cout << "####################################################" << std::endl;
	std::cout << "# filenames:" << std::endl;
	std::cout << "# file with domains		  		  : " << param_file_name_domains << std::endl;
	std::cout << "# file with trees			  	  : " << param_file_name_trees << std::endl;
	std::cout << "# file with graph			  	  : " << param_file_name_graph << std::endl;
	std::cout << "# file with index for graph		  : " << param_file_name_index << std::endl;
	std::cout << "# file with mst				  	  : " << param_file_name_mst << std::endl;
	std::cout << "# file with transfer			  : " << param_file_name_transfers << std::endl;
	std::cout << "# file with domain graph	 	  : " << param_file_name_domain_graph << std::endl;
	std::cout << "####################################################" << std::endl;
	std::cout << "# options for computation of minimum spanning tree:" << std::endl;
	std::cout << "# minimum overlap (residues)	  : " << param_min_residues_overlap << std::endl;
	std::cout << "# minimum overlap (relative)	  : " << param_min_relative_overlap << std::endl;
	std::cout << "# minimum coverage (relative)	  : " << param_min_coverage << std::endl;
	std::cout << "# log evalue for trusted links  : " << param_evalue_threshold_trusted_links << std::endl;
	std::cout << "# mode				  : " << param_mode << std::endl;  
	std::cout << "#			0: all possible pairings" << std::endl;
	std::cout << "#			1: maximum overlapping pairings" << std::endl;
	std::cout << "#			2: check pairings" << std::endl;
	std::cout << "#			3: prune alignments" << std::endl;
	std::cout << "####################################################" << std::endl;
	std::cout << "# options for optimisation:" << std::endl;
	std::cout << "# use_file_nids					  : " << param_use_file_nids << std::endl;
	std::cout << "# descend						  : " << param_descend << std::endl;  
	std::cout << "# disallow_shortening			  : " << param_disallow_shortening << std::endl;
	std::cout << "# max_iterations			  	  : " << param_max_iterations << std::endl;
	std::cout << "# resolution			  	  	  : " << param_resolution << std::endl;  
	std::cout << "# sigmoid k	  					  : " << param_real_k << std::endl;  
	std::cout << "# sigmoid c	  					  : " << param_real_c << std::endl;    
	std::cout << "# sigmoid min	  				  : " << param_real_min << std::endl;  
	std::cout << "# sigmoid max  					  : " << param_real_max << std::endl; 
	std::cout << "# exponential e	  				  : " << param_real_e << std::endl;    
	std::cout << "# exponential f	  				  : " << param_real_f << std::endl;      
	std::cout << "####################################################" << std::endl;
}



























