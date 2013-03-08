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
  if      (status == eslENOTFOUND) croak("Sequence file %s not found.\n",     seqfile);
  else if (status == eslEFORMAT)   croak("Format of file %s unrecognized.\n", seqfile);
  else if (status == eslEINVAL)    croak("Can't autodetect stdin or .gz for sequence file %s\n", seqfile);
  else if (status != eslOK)        croak("Open of sequence file %s failed, code %d.\n", seqfile, status);

  return perl_obj(sqfp, "ESL_SQFILE");
}    

/* Function:  _c_open_ssi_index()
 * Incept:    EPN, Mon Mar  4 13:58:29 2013
 * Synopsis:  Open an SSI file for an open sequence file.
 * Returns:   eslOK on success, eslENOTFOUND if SSI file does not exist
 *            dies via croak with informative error message upon an error
 */

int _c_open_ssi_index (ESL_SQFILE *sqfp)
{
  int           status;     /* Easel status code */

  /* Open the SSI index for retrieval */
  if (sqfp->data.ascii.do_gzip)           croak("can't use SSI index for file %s because it is gzipped", sqfp->filename);
  if (esl_sqio_IsAlignment(sqfp->format)) croak("can't use SSI index for file %s because it is an alignment", sqfp->filename);
  
  status = esl_sqfile_OpenSSI(sqfp, NULL);
  
  if      (status == eslEFORMAT)   croak("SSI index for file %s is in incorrect format\n", sqfp->filename);
  else if (status == eslERANGE)    croak("SSI index for file %s is in 64-bit format and we can't read it\n", sqfp->filename);
  else if (status == eslENOTFOUND) return status; /* this is okay, caller may try to deal by creating a new SSI file */
  else if (status != eslOK)        croak("Failed to open SSI index for file %s\n", sqfp->filename);

  return eslOK;
}    

/* Function:  _c_create_ssi_index()
 * Incept:    EPN, Fri Mar  8 09:46:52 2013
 * Synopsis:  Create an SSI index file for an existing sequence file.
 *            Based on and nearly identical to easel's miniapps/esl-sfetch.c::create_ssi_index.
 * Returns:   eslOK on success, eslENOTFOUND if SSI file does not exist
 *            dies via croak with informative error message upon an error
 */

void _c_create_ssi_index (ESL_SQFILE *sqfp)
{
  ESL_NEWSSI *ns      = NULL;
  ESL_SQ     *sq      = esl_sq_Create();
  int         nseq    = 0;
  char       *ssifile = NULL;
  uint16_t    fh;
  int         status;

  esl_strdup(sqfp->filename, -1, &ssifile);
  esl_strcat(&ssifile, -1, ".ssi", 4);
  status = esl_newssi_Open(ssifile, TRUE, &ns); /* TRUE is for allowing overwrite. */
  if      (status == eslENOTFOUND)   croak("failed to open SSI index %s", ssifile);
  else if (status == eslEOVERWRITE)  croak("SSI index %s already exists; delete or rename it", ssifile); /* won't happen, see TRUE above... */
  else if (status != eslOK)          croak("failed to create a new SSI index");

  if (esl_newssi_AddFile(ns, sqfp->filename, sqfp->format, &fh) != eslOK)
    croak("Failed to add sequence file %s to new SSI index\n", sqfp->filename);

  while ((status = esl_sqio_ReadInfo(sqfp, sq)) == eslOK)
    {
      nseq++;
      if (sq->name == NULL) croak("Every sequence must have a name to be indexed. Failed to find name of seq #%d\n", nseq);

      if (esl_newssi_AddKey(ns, sq->name, fh, sq->roff, sq->doff, sq->L) != eslOK)
	croak("Failed to add key %s to SSI index", sq->name);

      if (sq->acc[0] != '\0') {
	if (esl_newssi_AddAlias(ns, sq->acc, sq->name) != eslOK)
	  croak("Failed to add secondary key %s to SSI index", sq->acc);
      }
      esl_sq_Reuse(sq);
    }
  if      (status == eslEFORMAT) croak("Parse failed (sequence file %s):\n%s\n",
					   sqfp->filename, esl_sqfile_GetErrorBuf(sqfp));
  else if (status != eslEOF)     croak("Unexpected error %d reading sequence file %s",
					    status, sqfp->filename);

  /* Determine if the file was suitable for fast subseq lookup. */
  if (sqfp->data.ascii.bpl > 0 && sqfp->data.ascii.rpl > 0) {
    if ((status = esl_newssi_SetSubseq(ns, fh, sqfp->data.ascii.bpl, sqfp->data.ascii.rpl)) != eslOK) 
      croak("Failed to set %s for fast subseq lookup.");
  }

  /* Save the SSI file to disk */
  if (esl_newssi_Write(ns) != eslOK)  croak("Failed to write keys to ssi file %s\n", ssifile);

  /* done */
  free(ssifile);
  esl_sq_Destroy(sq);
  esl_newssi_Close(ns);
  return;
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
  if(key != NULL) { 
    status = esl_sqfile_PositionByKey(sqfp, key);
    if      (status == eslENOTFOUND) croak("seq %s not found in SSI index for file %s\n", key, sqfp->filename); 
    else if (status == eslEFORMAT)   croak("Failed to parse SSI index for %s\n", sqfp->filename);
    else if (status != eslOK)        croak("Failed to look up location of seq %s in SSI index of file %s\n", key, sqfp->filename);
  }

  status = esl_sqio_Read(sqfp, sq);
  if      (status == eslEFORMAT) croak("Parse failed (sequence file %s):\n%s\n",  sqfp->filename, esl_sqfile_GetErrorBuf(sqfp));
  else if (status == eslEOF)     croak("Unexpected EOF reading sequence file %s\n", sqfp->filename);
  else if (status != eslOK)      croak("Unexpected error %d reading sequence file %s\n", status, sqfp->filename);

  if (key != NULL && strcmp(key, sq->name) != 0 && strcmp(key, sq->acc) != 0) 
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
  esl_sq_Destroy(sq);

  return seqstring;
}    

/* Function:  _c_fetch_next_seq_to_fasta_string()
 * Incept:    EPN, Fri Mar  8 05:51:49 2013
 * Synopsis:  Fetch the next sequence from an open sequence file and return it as a 
 *            FASTA formatted string. This is a wrapper for _c_fetch_seq_to_fasta_string
 *            that passes NULL for <key>. This function is only necessary because
 *            I don't know how to pass a NULL value in for a char * to an inline C 
 *            function from Perl (as far as I can tell, it can't be done).
 * Args:      sqfp  - open ESL_SQFILE to fetch seq from
 *            textw - width for each sequence of FASTA record, -1 for unlimited.
 * Returns:   A pointer to a string that is the sequence in FASTA format.
 */

char *_c_fetch_next_seq_to_fasta_string (ESL_SQFILE *sqfp, int textw)
{
  return _c_fetch_seq_to_fasta_string(sqfp, NULL, textw);
}
