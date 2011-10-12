//--------------------------------------------------------------------------------
// Project adda
//
// Copyright (C) 2003 Andreas Heger All rights reserved
//
// Author: Andreas Heger <heger@ebi.ac.uk>
//
// $Id: adda.h,v 1.3 2006/06/15 09:19:32 aheger Exp $
//--------------------------------------------------------------------------------    

#ifndef ADDA_H
#define ADDA_H 1

// enable large file system support
/*
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#endif

#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif

#define _FILE_OFFSET_BITS 64
*/

#include <map>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include "gzstream.h"

//------------------------------------------------------------------------
typedef long Nid;
typedef fpos_t FileIndex;
typedef int Index;
typedef int Residue;
typedef double Score;

// types for storing alignments in Adda graph
// length of an alignment string
typedef short unsigned int Length;
typedef short unsigned int uResidue;

typedef std::map< Nid, Index > MapNid2Index;

#define MAX_LINE_LENGTH 10000000
#define SEPARATOR '\t'
#define SEPARATOR_RANGE '_'
// end of file token
#define TOKEN "#//\n"

// minimum probability to avoid taking the log of 0
#define SMALL_PROBABILITY 1e-20

//------------------------------------------------------------------------
typedef std::vector<FileIndex> FileIndexMap;
typedef std::vector<Nid> NidMap;

// streams for non-parallel output
typedef ogzstream OutStream;
typedef igzstream InStream;

//------------------------------------------------------------------------
FILE * openFileForRead( const std::string & filename );
FILE * openFileForWrite( const std::string & filename );
int toCompressedFile( unsigned char *, size_t, FILE *);
int fromCompressedFile( unsigned char *, size_t, FILE *);
void fillFileIndexMap( FileIndexMap & map_nid2fileindex, std::string & file_name_index);

//------------------------------------------------------------------------
template< class Array >
void fillParameterArrays( std::ifstream & infile,
			  Array & array)
{
  
  unsigned int x;
  double y;

  while (!infile.eof()) 
  {
    // skip comments
    char c = infile.peek();
    if (c == '#')
      {
	infile.ignore(10000, '\n');
	continue;
      }
    
    infile >> x >> y;
    infile.ignore(10000, '\n');
    if (infile.eof()) break;
    if (array.size() <= x) array.resize( x + 1, 0);
    array[x] = y;
  }
}

//------------------------------------------------------------------------
/** fill values from 0 to first value with first value.
    fill values in between with linear interpolation of adjacent values.
 */
template< class Array >
void interpolateParameterArrays( Array & array) 
{
  
  // set left side
  unsigned int x=0;
  while (x < array.size() && array[x] == 0) { ++x; };

  for (unsigned int y = 0; y < x; ++y) { array[y] = array[x]; };

  double last_value = array[x];

  for ( x+=1 ; x < array.size() - 1; ++x) {
    if (array[x] == 0) {
      unsigned int y = x + 1;
      while (y < array.size() && array[y] == 0) ++y;
      array[x] = last_value + (array[y] - last_value) / (double)(y - x);
    }
    last_value = array[x];
  }
}

struct IndexedNeighbour 
{
  Nid sbjct_nid;
  float evalue;
  uResidue query_start;
  uResidue query_end;
  uResidue sbjct_start;
  uResidue sbjct_end;
  Length query_alen;
  Length sbjct_alen;
  char * query_ali;
  char * sbjct_ali;
};

struct Link
{
Link( Index xquery_nid,
      Residue xquery_from,
      Residue xquery_to,
      Index xsbjct_nid,
      Residue xsbjct_from,
      Residue xsbjct_to,
      float score) :
  query_nid(xquery_nid),
  query_from(xquery_from),
    query_to(xquery_to),
    sbjct_nid(xsbjct_nid),
    sbjct_from(xsbjct_from),
    sbjct_to(xsbjct_to),
    score(score)
  {};
  Nid query_nid;
  Residue query_from;
  Residue query_to;
  Nid sbjct_nid;
  Residue sbjct_from;
  Residue sbjct_to;
  Score score;
};

typedef std::vector< Link > LinkList;
typedef std::vector< LinkList > Links;
  
/** fill links from infile.

    The infile is a compressed ADDA graph. It is assumed to
    be correctly positioned.
*/
template< class OutputIter >
void fillLinks( FILE * infile,
		const Nid & nid,
		OutputIter it)
{

  size_t nneighbours;
  Nid query_nid;
	
  int n = 0;
  
  n = fread( &query_nid, sizeof(Nid), 1, infile );
  n += fread( &nneighbours, sizeof(size_t), 1, infile );
  
  if (n != 2 || ferror( infile ))
    {
      std::cerr << "error while reading neighbours: can not read query_nid" << std::endl;
      exit(EXIT_FAILURE);
    }

  if (nid != query_nid and query_nid != 0)
    {
      std::cerr << "positioning error for nid " << nid << " got: " << query_nid << std::endl;
      exit(EXIT_FAILURE);
    }

  if (query_nid == 0) return;
  
  
#define MAX_BUFFER_SIZE 100000000
  unsigned char * buffer = (unsigned char *)calloc( MAX_BUFFER_SIZE, sizeof(unsigned char) );
  int retval = fromCompressedFile( buffer, MAX_BUFFER_SIZE, infile );
  if (retval != 0)
    {
      std::cerr << "error during uncompression for nid " << query_nid << ":" << retval << std::endl;
      free(buffer);
      exit(EXIT_FAILURE);
    }
	  
  IndexedNeighbour * nei;
  
  unsigned char * p = buffer;
  
  // see also cadda.py: fromBuffer()
  for (size_t x = 0; x < nneighbours; ++x)
    {
      nei = (IndexedNeighbour*)p;
      
      p += sizeof( IndexedNeighbour) - 2 * sizeof(char *);
      p += sizeof( char ) * (nei->query_alen + 1);
      p += sizeof( char ) * (nei->sbjct_alen + 1);

      if (query_nid == nei->sbjct_nid)
	continue;

      *it = Link(nid,
		 nei->query_start,
		 nei->query_end,
		 nei->sbjct_nid,
		 nei->sbjct_start,
		 nei->sbjct_end,
		 nei->evalue);
      ++it;
    }
  
  // reset stream, move away from eof.
  if (feof(infile))
    rewind(infile);

  free(buffer);
}

#endif

