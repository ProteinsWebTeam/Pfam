#include "easel.h"
#include "esl_sqio.h"
#include "esl_ssi.h"

/* Macros for converting C structs to perl, and back again)
 * from: http://www.mail-archive.com/inline@perl.org/msg03389.html
 * note the typedef in ~/perl/tw_modules/typedef
 */
#define perl_obj(pointer,class) ({                      \
      SV* ref=newSViv(0); SV* obj=newSVrv(ref, class);  \
      sv_setiv(obj, (IV) pointer); SvREADONLY_on(obj);  \
      ref;                                              \
    })

#define c_obj(sv,type) (                                                \
                        (sv_isobject(sv) && sv_derived_from(sv, #type)) \
                        ? ((type*)SvIV(SvRV(sv)))                       \
                        : NULL                                          \
                                                   )

/* Function:  _c_open_sqfile()
 * Incept:    EPN, Mon Mar  4 13:27:43 2013
 * Synopsis:  Open a sequence file and point a pointer at it.
 * Returns:   eslOK on success, some other status upon failure.
 */

SV *_c_open_sqfile (char *seqfile)
{
  int           status;      /* Easel status code */
  ESL_SQFILE   *sqfp = NULL; /* open input alignment file */

  /* open input file */
  status = esl_sqfile_Open(seqfile, eslSQFILE_UNKNOWN, NULL, &sqfp);

  if(sqfp != NULL) { 
    fprintf(stderr, "_c_open_sqfile, sqfp != NULL\n");
  }

  return perl_obj(sqfp, "ESL_SQFILE");
}    

/* Function:  _c_open_ssi()
 * Incept:    EPN, Mon Mar  4 13:58:29 2013
 * Synopsis:  Open an SSI file for an open sequence file.
 * Returns:   eslOK on success, some other status upon failure.
 */

int _c_open_ssi (ESL_SQFILE *sqfp)
{
  int           status;     /* Easel status code */

  fprintf(stderr, "in _c_open_ssi, kachow0\n"); 

  if(sqfp) { 
    fprintf(stderr, "in _c_open_ssi, sqfp != NULL\n");
  }
  else { 
    fprintf(stderr, "in _c_open_ssi, sqfp == NULL\n");
  }
  fprintf(stderr, "in _c_open_ssi, format is %d\n", sqfp->format);

  /* Open the SSI index for retrieval */
  //if (sqfp->data.ascii.do_gzip)           return eslENOFORMAT; /* caller will know what this means */
  if (esl_sqio_IsAlignment(sqfp->format)) return eslETYPE; /* caller will know what this means */

  fprintf(stderr, "in _c_open_ssi, kachow1\n"); 
  status = esl_sqfile_OpenSSI(sqfp, NULL);
  fprintf(stderr, "in _c_open_ssi, kachow2\n"); 
  /* this looks silly, since we always just return status, but we list
   * them so caller knows which different error status should be 
   * handled differently upon return.
   */
  if      (status == eslEFORMAT)   return status; 
  else if (status == eslERANGE)    return status;
  else if (status == eslENOTFOUND) return status;
  else if (status != eslOK)        return status;

  return status; /* status will be eslOK if we get here */
}    

/* Function:  _c_fetch_seq()
 * Incept:    EPN, Mon Mar  4 15:03:11 2013
 * Synopsis:  Fetch a sequence from an open sequence file.
 * Returns:   eslOK on success, some other status upon failure.

 * TODO: have this take an open file pointer, so it can output to an already open file 
 */

int _c_fetch_seq (ESL_SQFILE *sqfp, char *outfile, char *key)
{
  int  status;     /* Easel status code */
  FILE *ofp = NULL; /* output file */
  ESL_SQ *sq = esl_sq_Create();

  /* TEMP: open the output file for writing */
  if ((ofp = fopen(outfile, "w")) == NULL) return eslEWRITE;

  if (sqfp->data.ascii.ssi == NULL) { 
    return eslEINVAL; /* caller will know what this means */
  }

  /* from esl-sfetch.c's onefetch(), caller will output informative error message based on status returned here */
  status = esl_sqfile_PositionByKey(sqfp, key);
  if      (status == eslENOTFOUND) return eslENOTFOUND;
  else if (status == eslEFORMAT)   return eslERANGE; /* note we change status here */
  else if (status != eslOK)        return eslFAIL;
    
  status = esl_sqio_Read(sqfp, sq);
  if      (status == eslEFORMAT) return eslEFORMAT;
  else if (status == eslEOF)     return eslEOF;
  else if (status != eslOK)      return eslETYPE;

  if (strcmp(key, sq->name) != 0 && strcmp(key, sq->acc) != 0) return eslECORRUPT;
    
  if (esl_sqio_Echo(sqfp, sq, ofp) != eslOK) esl_fatal("Echo failed: %s\n", esl_sqfile_GetErrorBuf(sqfp));

  esl_sq_Destroy(sq);
  fclose(ofp);

  return eslOK;
}    
