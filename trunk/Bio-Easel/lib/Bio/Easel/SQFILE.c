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

  /* Open the SSI index for retrieval */
  if (sqfp->data.ascii.do_gzip)           return eslENOFORMAT; /* caller will know what this means */
  if (esl_sqio_IsAlignment(sqfp->format)) return eslETYPE;     /* caller will know what this means */

  status = esl_sqfile_OpenSSI(sqfp, NULL);
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

/* Function:  _c_fetch_seq_to_fasta_string()
 * Incept:    EPN, Mon Mar  4 15:03:11 2013
 * Synopsis:  Fetch a sequence from an open sequence file and return it as a string.
 * Returns:   A pointer to a string that is the sequence in FASTA format.
 */

char *_c_fetch_seq_to_fasta_string (ESL_SQFILE *sqfp, char *key)
{
  int     status;                /* Easel status code */
  ESL_SQ *sq = esl_sq_Create();  /* the sequence */
  char   *seqstring = NULL;      /* the sequence string */
  int     n  = 0;                /* position in string */
  int     n2 = 0;                /* position in string */

  /* make sure we're not in digital mode */
  if(sq->dsq)                       goto ERROR; 
  if (sqfp->data.ascii.ssi == NULL) goto ERROR;

  /* from esl-sfetch.c's onefetch(), caller will output informative error message based on status returned here */
  status = esl_sqfile_PositionByKey(sqfp, key);
  if      (status == eslENOTFOUND) goto ERROR;
  else if (status == eslEFORMAT)   goto ERROR;
  else if (status != eslOK)        goto ERROR;
    
  status = esl_sqio_Read(sqfp, sq);
  if      (status == eslEFORMAT) goto ERROR;
  else if (status == eslEOF)     goto ERROR;
  else if (status != eslOK)      goto ERROR;

  if (strcmp(key, sq->name) != 0 && strcmp(key, sq->acc) != 0) goto ERROR;
    
  /* create seqstring  */
  n = strlen(sq->name);
  if (esl_strdup(sq->name, n, &seqstring) != eslOK) goto ERROR;
  if (sq->acc) { 
    n2 = strlen(sq->acc);
    if (esl_strcat(&seqstring, n, sq->acc, n2) != eslOK) goto ERROR;
    n += n2;
  }
  if (sq->desc) { 
    n2 = strlen(sq->desc);
    if (esl_strcat(&seqstring, n, sq->desc, n2) != eslOK) goto ERROR;
    n += n2;
  }
  /* add newline */
  if (esl_strcat(&seqstring, n, "\n", 1) != eslOK) goto ERROR;
  n++;
  if (esl_strcat(&seqstring, n, sq->seq, sq->n) != eslOK) goto ERROR;

  esl_sq_Destroy(sq);

  return seqstring;

 ERROR: 
  return NULL;
}    
