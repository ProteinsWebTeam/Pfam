/* Function declaration for Pyrex with C linkage 
 * 
 * These have equivalents in adda.h where they are
 * declared as 'extern "C"'
 * 
 * */

#include <stdio.h>
#include <stdlib.h>

// parameter setters for optimization
void cadda_setFilenameSegments( const char * f);
void cadda_setFilenameGraph( const char * f);
void cadda_setFilenameIndex( const char * f);
void cadda_setFilenameNids( const char * f);
void cadda_setFilenameDomains( const char * f);
void cadda_setFilenameTransfers( const char * f);
void cadda_setFilenameDomainGraph( const char * f);
void cadda_setFilenameMst( const char * f);

void cadda_setDisallowShortening( int f);
void cadda_setDescend( int f);
void cadda_setUseFileNids( int f);
void cadda_setOnlyQuery( int f);
void cadda_setRelativeOverhang( int f);

void cadda_setMaxIterations( int f);

void cadda_setResolution( double f);
void cadda_setK( double f) ;
void cadda_setC( double f) ;
void cadda_setMax( double f);
void cadda_setMin( double f);
void cadda_setE( double f) ;
void cadda_setF( double f) ;
void cadda_setLogLevel( int f);
void cadda_setReportStep( int f);

void cadda_setEvalueThresholdTrustedLinks( double f);

// call optimizer
int cadda_optimise_initialise();
double cadda_optimise_iteration();
int cadda_optimise_destroy();
int cadda_optimise_save_partitions( const char * f);
int cadda_optimise_load_partitions( const char * f);
long cadda_optimise_get_num_partitions();


// build index
int cadda_build_index();

// build index
int cadda_check_index();

// convert sequence to domain graph
int cadda_convert( const char * f);

// construct mst
int cadda_build_mst( const char * out,
		     const char * in);


// dump parameters
void cadda_dump_parameters();

// write buffer to file with compression
int toCompressedFile( unsigned char *, size_t, FILE *);

// read buffer to file with compression
int fromCompressedFile( unsigned char *, size_t, FILE *);
