//--------------------------------------------------------------------------------
// Project adda
//
// Copyright (C) 2000 Andreas Heger All rights reserved
//
// Author: Andreas Heger <heger@ebi.ac.uk>
//
// $Id: adda_index.cpp,v 1.3 2006/06/15 09:19:32 aheger Exp $
//--------------------------------------------------------------------------------    

#include "adda.h"

#include <math.h>

#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <cstring>
#include <cstdlib>
#include <fcntl.h>
#include <cstdio>

using namespace std;

extern std::string param_file_name_graph;
extern std::string param_file_name_index;

extern unsigned int param_report_step;
extern unsigned int param_loglevel;

//--------------------------------------------------------------------------------
int cadda_build_index()
{

  FILE * infile = openFileForRead( param_file_name_graph );
  FILE * outfile = openFileForWrite( param_file_name_index );
  	
  char buffer[MAX_LINE_LENGTH+1];
  unsigned int iteration = 0;
  Nid nid;
  FileIndex index;

  Nid last_nid = 0;

  std::cout << "# size of index=" << sizeof(FileIndex) << " bytes" << std::endl;


  
  while(!feof(infile)) 
  { 
      ++iteration;
      
      if (param_loglevel >= 2) 
      {
    	  if (!(iteration % param_report_step)) 
    	  {
	    std::cout << "# line=" << iteration << " last_nid=" << last_nid << std::endl;
    	  }
      }

      fgetpos( infile, &index );
      {
	char * x = fgets( buffer, MAX_LINE_LENGTH, infile );
	if (x == NULL) return 0;
      }

      // skip comments or header (starting with query_nid)
      if (buffer[0] == '#' || strncmp(buffer, "query_nid", 9) == 0 ) continue;

      {
	int r = sscanf(buffer, "%ld", &nid);
	if ( r != 1 ) 
	  {
	    std::cerr << "# ERROR: could not parse nid from line: " << buffer << std::endl;
	    return 0;
	  }
      }
					      
      if (last_nid != nid) 
      {
    	  {
	    int r = fwrite(&nid,sizeof(Nid),1,outfile);
	    if ( r != 1)
	      {
		std::cerr << "# ERROR: index for nid " << nid << " could not be written" << std::endl;
		return 0;
	      }
    	  }
    	  {
	    int r = fwrite(&index,sizeof(FileIndex),1,outfile);
	    if ( r != 1)
	      {
		std::cerr << "# ERROR: index for nid " << nid << " could not be written" << std::endl;
		return 0;
	      }
    	  }

    	  if (nid < last_nid) 
	    {
	      std::cerr << "# ERROR: nid " << nid << " is smaller than last nid " << last_nid << std::endl;
	      return 0;
	    }
    	  last_nid = nid;
      }

    }
 
  fclose(infile);  
  fclose(outfile);  
  return iteration;
}

//--------------------------------------------------------------------------------
int cadda_check_index()
{

  FILE * infile = openFileForRead( param_file_name_graph );
  FILE * outfile = openFileForRead( param_file_name_index );

  char buffer[MAX_LINE_LENGTH+1];

  unsigned int iteration = 0;
  Nid nid;
  FileIndex index;

  if (param_loglevel >= 1) 
  {
      std::cout << "# checking index " << param_file_name_index << std::endl;
      std::cout << "# against neighbours in " << param_file_name_graph << std::endl;
  }

  while(!feof(outfile)) 
  { 
      ++iteration;
    
      {
    	  int x = fread(&nid,sizeof(Nid), 1, outfile);
	  if (x != 1) return 0;
      }
      if (feof(outfile)) break;
      {
    	  int x = fread(&index, sizeof(FileIndex), 1, outfile);
	  if (x != 1) return 0;
      }

      if (param_loglevel >= 2) 
      {
    	  if (!(iteration % param_report_step)) 
    	  {
    		  std::cout << "# line=" << iteration << " nid=" << nid << std::endl;
    	  }
      }

      Nid check_nid;
      fsetpos( infile, &index );
      {
	char * x = fgets( buffer, MAX_LINE_LENGTH, infile );
	if (x == NULL)
	  {
	    std::cerr << "error for nid " << nid << ": could not position"
		      << std::endl;
	    return 1;
	  }
      }

      if (sscanf(buffer, "%ld", &check_nid) != 1) 
      {
	std::cerr << "error for nid " << nid << ": could not parse from line:" << buffer
		  << std::endl;

	return 1;
      }
      
      if (nid != check_nid)
	{
    	  std::cerr << "error for nid " << nid << ": incorrect file position gives " 
		  	<< check_nid << " at line :" << buffer << std::endl;
    	  return 1;
	}
  }
  
  fclose(infile);  
  fclose(outfile);  
  return 0;
}








