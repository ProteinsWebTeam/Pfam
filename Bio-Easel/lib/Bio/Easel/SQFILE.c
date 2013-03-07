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
 * Synopsis:  Fetch a sequence from an open sequence file and return it as a FASTA
 *            formatted string.
 * Args:      sqfp  - open ESL_SQFILE to fetch seq from
 *            key   - name or accession of sequence to fetch
 *            textw - width for each sequence of FASTA record, -1 for unlimited.
 * Returns:   A pointer to a string that is the sequence in FASTA format.
 */

char *_c_fetch_seq_to_fasta_string (ESL_SQFILE *sqfp, char *key, int textw)
{

  int     status;                /* Easel status code */
  ESL_SQ *sq = esl_sq_Create();  /* the sequence */
  char   *seqstring = NULL;      /* the sequence string */
  int     n   = 0;               /* position in string */
  int     n2  = 0;               /* position in string */
  int     pos = 0;               /* position in sq->seq */

  /* make sure textw makes sense */
  if(textw < 0 && textw != -1) croak("invalid value for textw\n"); 
  /* make sure we're not in digital mode, and SSI is valid */
  if (sq->dsq)                      croak("sequence file is unexpectedly digitized\n");
  if (sqfp->data.ascii.ssi == NULL) croak("sequence file has no SSI information\n"); 

  /* from esl-sfetch.c's onefetch(), caller will output informative error message based on status returned here */
  status = esl_sqfile_PositionByKey(sqfp, key);
  if      (status == eslENOTFOUND) croak("seq %s not found in SSI index for file %s\n", key, sqfp->filename); 
  else if (status == eslEFORMAT)   croak("Failed to parse SSI index for %s\n", sqfp->filename);
  else if (status != eslOK)        croak("Failed to look up location of seq %s in SSI index of file %s\n", key, sqfp->filename);
    
  status = esl_sqio_Read(sqfp, sq);
  if      (status == eslEFORMAT) croak("Parse failed (sequence file %s):\n%s\n",  sqfp->filename, esl_sqfile_GetErrorBuf(sqfp));
  else if (status == eslEOF)     croak("Unexpected EOF reading sequence file %s\n", sqfp->filename);
  else if (status != eslOK)      croak("Unexpected error %d reading sequence file %s\n", status, sqfp->filename);

  if (strcmp(key, sq->name) != 0 && strcmp(key, sq->acc) != 0) 
    croak("whoa, internal error; found the wrong sequence %s, not %s\n", sq->name, key);
    
  /* create seqstring  */
  /* '>' character */
  n = 0;
  if (esl_strdup(">", 1, &seqstring)              != eslOK) croak("out of memory while fetching sequence %s\n", key);
  n++;
  /* name */
  n2 = strlen(sq->name);
  if (esl_strcat(&seqstring, n, sq->name, n2)     != eslOK) croak("out of memory while fetching sequence %s\n", key);
  n += n2;
  /* accession */
  if (sq->acc[0] != 0) { 
    if (esl_strcat(&seqstring, n, " ", 1)         != eslOK) croak("out of memory while fetching sequence %s\n", key);
    n++;
    n2 = strlen(sq->acc);
    if (esl_strcat(&seqstring, n, sq->acc, n2)    != eslOK) croak("out of memory while fetching sequence %s\n", key);
    n += n2;
  }
  /* desc */
  if (sq->desc[0] != 0) { 
    if (esl_strcat(&seqstring, n, " ", 1)         != eslOK) croak("out of memory while fetching sequence %s\n", key);
    n++;
    n2 = strlen(sq->desc);
    if (esl_strcat(&seqstring, n, sq->desc, n2)   != eslOK) croak("out of memory while fetching sequence %s\n", key);
    n += n2;
  }
  /* newline */
  if (esl_strcat(&seqstring, n, "\n", 1)          != eslOK) croak("out of memory while fetching sequence %s\n", key);
  n++;
  /* sequence */
  if (textw == -1) { /* unlimited line length */
    if (esl_strcat(&seqstring, n, sq->seq, sq->n) != eslOK) croak("out of memory while fetching sequence %s\n", key);
    n += sq->n;
    /* newline */
    if (esl_strcat(&seqstring, n, "\n", 1)        != eslOK) croak("out of memory while fetching sequence %s\n", key);
    n++;
  }
  else { /* textw != -1, limit line length to textw */
    for (pos = 0; pos < sq->n; pos += textw) { 
      n2 = ESL_MIN(textw, sq->n - pos);
      if (esl_strcat(&seqstring, n, sq->seq+pos, n2) != eslOK) croak("out of memory while fetching sequence %s\n", key);
      n += n2;
      /* newline */
      if (esl_strcat(&seqstring, n, "\n", 1) != eslOK) croak("out of memory while fetching sequence %s\n", key);
      n++;
    }
  }
  /* fprintf(stderr, "in C, seqstring: %s\n", seqstring); */
  esl_sq_Destroy(sq);

  return seqstring;
}    
