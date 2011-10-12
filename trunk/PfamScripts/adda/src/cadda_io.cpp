//--------------------------------------------------------------------------------------------
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <zlib.h>
#include "adda.h"
#include <cassert>

//--------------------------------------------------------------------------------------------
bool fileExists (const std::string & filename)
{
	FILE * infile = fopen(filename.c_str(), "r");
	if (infile != NULL)
	{
		fclose(infile);
		return true;
	}
	else
	{
		return false;
	}
}

//--------------------------------------------------------------------------------------------
FILE * openFileForWrite( const std::string & filename )
{
	if (fileExists(filename))
	{
		std::cerr << "# file " << filename << " already exists, aborting." << std::endl;
		exit(EXIT_FAILURE);
	}

	FILE * file = fopen( filename.c_str(), "w");

	if (file == NULL)
	{
		std::cerr << "# error while opening " << filename << " for writing." << std::endl;
		exit(EXIT_FAILURE);
	}

	return file;
}

//--------------------------------------------------------------------------------------------
FILE * openFileForRead( const std::string & filename )
{
	FILE * file = fopen( filename.c_str(), "r");
	if (file == NULL)
	{
		std::cerr << "# file " << filename << " could not be opened for reading." << std::endl;
		exit(EXIT_FAILURE);
	}

	return file;
}

// save buffer into file with zlib compression
int toCompressedFile( unsigned char * buffer, size_t uncompressed_size, FILE * output_f )
{

  uLongf compressed_size = uncompressed_size * 2;
  Bytef * compressed = (Bytef *)calloc( compressed_size, sizeof(Bytef) );
  int level = 9;
  int zok = compress2(compressed, &compressed_size, buffer, uncompressed_size, level);

  if ( zok != Z_OK || fwrite( &compressed_size, sizeof(uLongf), 1, output_f ) != 1 || ferror( output_f ))
    {
      free( compressed );
      return zok;
    }
        
  if ( fwrite(compressed, 1, compressed_size, output_f) != compressed_size || ferror(output_f))
    {
      free( compressed );
      return -10;
    }

  free( compressed );
  return zok;
}

// save compressed data into buffer. Buffer has to be large enough.
int fromCompressedFile( unsigned char * buffer, size_t uncompressed_size, FILE * input_f )
{
  uLongf compressed_size;
  
  if ( fread( &compressed_size, sizeof(uLongf), 1, input_f) != 1 || ferror(input_f))
    {
      return Z_ERRNO;
    }
  
  Bytef * compressed = (Bytef *)calloc( compressed_size, sizeof(Bytef) );

  if ( ( fread(compressed, 1, compressed_size, input_f) != compressed_size) || ferror(input_f) )
    {
      free( compressed );
      return Z_ERRNO;
    }

  int zok = uncompress (buffer, &uncompressed_size, compressed, compressed_size);

  free(compressed);
  return zok;
}
  
//--------------------------------------------------------------------------------
void fillFileIndexMap( FileIndexMap & map_nid2fileindex, std::string & file_name_index)
{
  FILE * file = fopen(file_name_index.c_str(), "r");

  if (file == NULL)
    {
      std::cerr << "could not open filename with indices: " << file_name_index << std::endl;
      exit(EXIT_FAILURE);
    }

  Nid nnids = 0;

  if (fread( &nnids, sizeof(Nid), 1, file ) != 1 or ferror( file) )
    {
      std::cerr << "could not read index from " << file_name_index << std::endl;
      exit(EXIT_FAILURE);
    }

  FileIndex * index = (FileIndex *)new FileIndex[nnids];

  if (index == NULL)
    {
      std::cerr << "out of memory when allocating index for %i nids" << nnids << std::endl;
      exit(EXIT_FAILURE);
    }

  if (fread( index, sizeof(FileIndex), nnids, file ) != (size_t)nnids or ferror(file))
    {
      free( index );
      std::cerr << "failure while reading index for %i nids" << nnids << std::endl;
      exit(EXIT_FAILURE);
    }
  
  fclose( file );

  map_nid2fileindex.resize( nnids );
  
  for (Nid x = 1; x < nnids; ++x)
    {
      map_nid2fileindex[x] = index[x];
    }
  delete [] index;
}
