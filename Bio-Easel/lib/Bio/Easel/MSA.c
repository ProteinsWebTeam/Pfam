#include "easel.h"
#include "esl_alphabet.h"
#include "esl_distance.h"
#include "esl_msa.h"
#include "esl_msafile.h"
#include "esl_sq.h"
#include "esl_sqio.h"
#include "esl_vectorops.h"
#include "esl_wuss.h"
#include "esl_msaweight.h"

/* Macros for converting C structs to perl, and back again)
* from: http://www.mail-archive.com/inline@perl.org/msg03389.html
* note the typedef in ~/perl/tw_modules/typedef
*/
#define perl_obj(pointer,class) ({                 \
  SV* ref=newSViv(0); SV* obj=newSVrv(ref, class); \
  sv_setiv(obj, (IV) pointer); SvREADONLY_on(obj); \
  ref;                                             \
})

#define c_obj(sv,type) (                           \
  (sv_isobject(sv) && sv_derived_from(sv, #type))  \
    ? ((type*)SvIV(SvRV(sv)))                      \
    : NULL                                         \
  )

/* Function:  _c_int_copy_array_perl_to_c()
 * Incept:    EPN, Thu Nov 21 09:07:47 2013
 * Synopsis:  Copy a perl array of ints into a C array of ints
 *            <cA> must already be allocated to proper length (<len>).
 * Returns:   void
 *
 */

void _c_int_copy_array_perl_to_c (AV *perlAR, int *cA, int len)
{
  int i;
  SV **value; /* this will hold the value we extract */

  for(i = 0; i < len; i++) { 
    /* look up the i'th element, which is an SV */
    value = av_fetch(perlAR, i, 0);
    /* a couple of sanity checks */
    if (value == NULL)  croak( "_c_int_copy_array_perl_to_c, failed array lookup for element %d", i);
    if (!SvIOK(*value)) croak( "_c_int_copy_array_perl_to_c, array element %d is not an integer", i);
    cA[i] = SvIV(*value);
  }

  return;
}

/* Function:  _c_read_msa()
 * Incept:    EPN, Sat Feb  2 14:14:20 2013
 * Synopsis:  Open a alignment file, read an msa, and close the file.
 * Args:      infile:     name of file to read MSA from (one alignment per
 *                        file required currently, should probably fix this one day.)
 *            reqdFormat: required format, "unknown" for no specific format required
 *            digitize:   '1' to read alignment in digital mode, '0' to read in text mode
 *                        digital mode is faster, safer, text preserves case, exact characters in input msa
 * Returns:   an ESL_MSA and a string describing it's format
 */

void _c_read_msa (char *infile, char *reqdFormat, int digitize)
{
  Inline_Stack_Vars;

  int           status;     /* Easel status code */
  ESLX_MSAFILE *afp;        /* open input alignment file */
  ESL_MSA      *msa;        /* an alignment */
  ESL_ALPHABET *abc = NULL; /* alphabet for MSA, by passing this to 
                             * eslx_msafile_Open(), we force digital MSA mode */
  int           fmt;        /* int code for format string */
  char         *actual_format = NULL; /* string describing format of file, e.g. "Stockholm" */
                             
  /* decode reqdFormat string */
  fmt = eslx_msafile_EncodeFormat(reqdFormat);

  /* open input file, either in text or digital mode */
  if ((status = eslx_msafile_Open((digitize) ? &abc : NULL, /* digitize or text mode */
                                  infile, NULL, fmt, NULL, &afp)) != eslOK) { 
    croak("Error reading alignment file %s: %s\n", infile, afp->errmsg);
  }
  
  /* read_msa */
  status = eslx_msafile_Read(afp, &msa);
  if(status != eslOK) croak("Alignment file %s read failed with error code %d\n", infile, status);

  /* convert actual alignment file format to a string */
  actual_format = eslx_msafile_DecodeFormat(afp->format);
  
  Inline_Stack_Reset;
  Inline_Stack_Push(perl_obj(msa, "ESL_MSA"));
  Inline_Stack_Push(newSVpvn(actual_format, strlen(actual_format)));
  Inline_Stack_Done;
  Inline_Stack_Return(2);

  /* close msa file */
  free(actual_format);
  if (afp) eslx_msafile_Close(afp);
  
  return;
}    

/* Function:  _c_write_msa()
 * Incept:    EPN, Sat Feb  2 14:23:28 2013
 * Synopsis:  Open an output file, write an msa, and close the file.
 *            If the outfile string is STDOUT than output the alignment
 *            to stdout, not to a file.
 * Returns:   eslOK on success; eslEINVAL if format is invalid;
 *            eslEINVAL if format invalid
 *            eslFAIL if unable to open file for writing.
 */
int _c_write_msa (ESL_MSA *msa, char *outfile, char *format) 
{
  FILE  *ofp;      /* open output alignment file */
  int   fmt;       /* alignment output format */       
  int   do_stdout; /* TRUE to output to stdout instead of a file */
  do_stdout = (strcmp(outfile, "STDOUT") == 0) ? 1 : 0;
    
  if(do_stdout) { 
    ofp = stdout;
  }
  else { 
    if((ofp  = fopen(outfile, "w"))  == NULL) { 
      return eslFAIL;
    }
  }
  if((fmt = eslx_msafile_EncodeFormat(format)) == eslMSAFILE_UNKNOWN) { 
    return eslEINVAL;
  }
  eslx_msafile_Write(ofp, msa, fmt);

  if(! do_stdout) {
    fclose(ofp);
  }

  return eslOK;
}

/* Function:  _c_write_msa_unaligned_fasta()
 * Incept:    EPN, Thu Oct 31 11:03:29 2013
 * Synopsis:  Open an output file, write individual seqs in an msa as unaligned
 *            FASTA, and close the file.
 *            If the outfile string is STDOUT than output the alignment
 *            to stdout, not to a file.
 * Returns:   eslOK on success; 
 *            eslFAIL if unable to open file for writing.
 *            eslEMEM if out of memory
 */
int _c_write_msa_unaligned_fasta (ESL_MSA *msa, char *outfile)
{
  FILE   *ofp; /* open output alignment file */
  ESL_SQ *sq = NULL;
  int     i;
  int     status;
  int   do_stdout; /* TRUE to output to stdout instead of a file */
  do_stdout = (strcmp(outfile, "STDOUT") == 0) ? 1 : 0;

  if(do_stdout) { 
    ofp = stdout;
  }
  else { 
    if((ofp  = fopen(outfile, "w"))  == NULL) { 
      return eslFAIL;
    }
  }

  for(i = 0; i < msa->nseq; i++) { 
    status = esl_sq_FetchFromMSA(msa, i, &sq);
    if(status != eslOK) { fclose(ofp); return status; }
    esl_sqio_Write(ofp, sq, eslSQFILE_FASTA, FALSE);
    esl_sq_Destroy(sq); /* note: this is inefficient, FetchFromMSA allocates a new seq each time */
  }    

  if(! do_stdout) {
    fclose(ofp);
  }
  return eslOK;
}

/* Function:  _c_write_single_unaligned_seq
 * Incept:    EPN, Mon Nov  4 09:57:43 2013
 * Synopsis:  Open an output file, and write a single unaligned sequence to it, in
 *            FASTA format, then close the file.
 * Returns:   eslOK on success; 
 *            eslFAIL if unable to open file for writing.
 *            eslEINVAL if idx is out of bounds < 0 || >= msa->nseq
 *            eslEMEM if out of memory
 */
int _c_write_single_unaligned_seq(ESL_MSA *msa, int idx, char *outfile)
{
  FILE   *ofp; /* open output alignment file */
  ESL_SQ *sq = NULL;
  int     status;

  if(idx < 0 || idx >= msa->nseq) { 
    return eslEINVAL;
  }
  status = esl_sq_FetchFromMSA(msa, idx, &sq);
  if(status != eslOK) return status;

  if((ofp  = fopen(outfile, "w"))  == NULL) { 
    return eslFAIL;
  }
  esl_sqio_Write(ofp, sq, eslSQFILE_FASTA, FALSE);
  esl_sq_Destroy(sq); /* note: this is inefficient, FetchFromMSA allocates a new seq each time */
  fclose(ofp);

  return eslOK;
}

/* Function:  _c_free_msa()
 * Incept:    EPN, Sat Feb  2 14:33:15 2013
 * Synopsis:  Free an MSA.
 * Returns:   void
 */
void _c_free_msa (ESL_MSA *msa)
{
  esl_msa_Destroy(msa);
  return;
}

/* Function:  _c_destroy()
 * Incept:    EPN, Sat Feb  2 14:33:15 2013
 * Synopsis:  Free an MSA and associated data structures.
 * Returns:   void
 */
void _c_destroy (ESL_MSA *msa)
{
  _c_free_msa(msa);
  return;
}

/* Function:  _c_nseq()
 * Incept:    EPN, Sat Feb  2 14:34:34 2013
 * Synopsis:  Returns nseq
 * Returns:   number of sequences in <msa>
 */
I32 _c_nseq (ESL_MSA *msa)
{
  return msa->nseq;
}   

/* Function:  _c_alen()
 * Incept:    EPN, Sat Feb  2 14:34:50 2013
 * Synopsis:  Returns alen
 * Returns:   number alignment length in columns
 */
I32 _c_alen (ESL_MSA *msa)
{
  return msa->alen;
}

/* Function:  _c_checksum()
 * Incept:    EPN, Tue Mar  4 09:53:19 2014
 * Synopsis:  Returns checksum. Caution: the same 
 *            MSA will return a different checksum
 *            depending on whether it was read in 
 *            digital or text mode.
 * Returns:   checksum for the MSA
 * Dies:      if checksum cannot be determined
 */
U32 _c_checksum (ESL_MSA *msa)
{
  int status;
  uint32_t checksum;

  if((status = esl_msa_Checksum(msa, &checksum)) != eslOK) { 
    croak("_c_checksum() problem calculating checksum"); 
  }

  return checksum;
}   

/* Function:  _c_is_digitized()
 * Incept:    EPN, Wed Mar  5 09:16:47 2014
 * Synopsis:  Returns '1' if MSA is digitized, else '0' (if MSA is in text mode).
 * Returns:   '1' if MSA is digitized, else '0'
 */
int _c_is_digitized (ESL_MSA *msa)
{
  return (msa->flags & eslMSA_DIGITAL) ? 1 : 0;
}   

/* Function:  _c_has_rf()
 * Incept:    EPN, Tue Apr  2 19:43:06 2013
 * Synopsis:  Returns TRUE if msa->rf is valid
 * Returns:   Returns '1' if msa->rf is non-NULL, else returns 0
 */
int _c_has_rf (ESL_MSA *msa)
{
  if(msa->rf) return 1;
  else        return 0;
}

/* Function:  _c_has_ss_cons()
 * Incept:    EPN, Fri May 24 09:57:56 2013
 * Synopsis:  Returns TRUE if msa->ss_cons is valid
 * Returns:   Returns '1' if msa->ss_cons is non-NULL, else returns 0
 */
int _c_has_ss_cons (ESL_MSA *msa)
{
  if(msa->ss_cons) return 1;
  else             return 0;
}

/* Function:  _c_get_rf()
 * Incept:    EPN, Thu Nov 21 10:10:51 2013
 * Synopsis:  Returns msa->rf if non-NULL, else dies.
 *            Caller should have used _c_has_rf to verify it exists.
 * Returns:   msa->rf()
 */
char *_c_get_rf (ESL_MSA *msa)
{
  if(msa->rf == NULL) croak("_c_get_rf, but RF is NULL");
  return msa->rf;
}

/* Function:  _c_set_rf()
 * Incept:    EPN, Tue Feb 18 09:56:35 2014
 * Synopsis:  Sets msa->rf
 * Returns:   void
 */
void _c_set_rf (ESL_MSA *msa, char *rfstr)
{
  int rflen = strlen(rfstr);
  if(rflen != msa->alen) croak("_c_set_rf() trying to set RF with string of incorrect length");
  if(msa->rf) free(msa->rf);

  esl_strdup(rfstr, rflen, &(msa->rf));
  return;
}   

/* Function:  _c_get_ss_cons()
 * Incept:    EPN, Fri May 24 09:58:32 2013
 * Synopsis:  Returns msa->ss_cons if non-NULL, else dies.
 *            Caller should have used _c_has_ss_cons to verify it exists.
 * Returns:   msa->ss_cons()
 */
char *_c_get_ss_cons (ESL_MSA *msa)
{
  if(msa->ss_cons == NULL) croak("_c_get_ss_cons, but SS_cons is NULL");
  return msa->ss_cons;
}

/* Function:  _c_set_ss_cons()
 * Incept:    EPN, Tue Feb 18 10:18:29 2014
 * Synopsis:  Sets msa->ss_cons
 * Returns:   void
 */
void _c_set_ss_cons (ESL_MSA *msa, char *ss_cons_str, int do_full_wuss)
{
  int status;
  int ss_cons_len = strlen(ss_cons_str);
  if(ss_cons_len != msa->alen) croak("_c_set_ss_cons() trying to set SS_cons with string of incorrect length");
  if(msa->ss_cons) free(msa->ss_cons);

  esl_strdup(ss_cons_str, ss_cons_len, &(msa->ss_cons));
  if(do_full_wuss) { 
    if((status = esl_wuss_full(msa->ss_cons, msa->ss_cons)) != eslOK) { 
      croak("_c_set_ss_cons() problem converting SS_cons to full WUSS notation"); 
    }
  }
  return;
}   

/* Function:  _c_set_blank_ss_cons()
 * Incept:    EPN, Tue Oct 22 10:39:59 2013
 * Synopsis:  Sets msa->ss_cons as all '.' (no basepairs).
 * Returns:   void
 */
void _c_set_blank_ss_cons (ESL_MSA *msa)
{
  int status;
  int i;

  if(msa->ss_cons == NULL) { 
    ESL_ALLOC(msa->ss_cons, sizeof(char) * (msa->alen+1)); 
  }
  for(i = 0; i < msa->alen; i++) { 
    msa->ss_cons[i] = '.';
  }
  msa->ss_cons[msa->alen] = '\0';

  return;

 ERROR: 
  croak("out of memory");
  return; /* NEVER REACHED */
}

/* Function:  _c_get_ss_cons_ct()
 * Incept:    EPN, Mon Jul  7 09:34:01 2014
 * Synopsis:  Return a 'CT' array describing the consensus secondary structure
 *            for the alignment.
 * Args:      msa: the alignment
 * Returns:   a ct[] array, [1..alen]:
 *            ct[i] is the position that position 'i' pairs to
 *                  or '0' if position 'i' is unpaired.
 * Dies:      with croak, if we can't make a CT array for some reason
 */
void _c_get_ss_cons_ct (ESL_MSA *msa)
{
  Inline_Stack_Vars;

  int status;
  int *ct;
  int apos;

  if(msa->ss_cons == NULL) { 
    croak("ERROR in _c_get_ss_cons_ct(), msa has no SS_cons annotation");
  }

  /* allocate 'ct' */
  ESL_ALLOC(ct, sizeof(int) * (msa->alen+1));

  if((status = esl_wuss2ct(msa->ss_cons, msa->alen, ct)) != eslOK) { 
    croak("ERROR in _c_get_ss_cons_ct(), problem converting SS_cons to CT array"); 
  }

  /* calculate sequence conservation, and fill return array */
  Inline_Stack_Reset;
  for(apos = 0; apos <= msa->alen; apos++) { 
    Inline_Stack_Push(newSViv(ct[apos])); 
  }
  Inline_Stack_Done;
  Inline_Stack_Return(msa->alen+1);

  if(ct) free(ct);
  return;

 ERROR: 
  croak("out of memory");
  return; /* NEVER REACHED */
}

/* Function:  _c_get_accession()
 * Incept:    EPN, Sat Feb  2 14:35:18 2013
 * Synopsis:  Returns msa->acc.
 * Returns:   MSA's accession or 'none' if none set.
 */
char *_c_get_accession (ESL_MSA *msa)
{
  if(msa->acc) return msa->acc;
  else         return "none";
}

/* Function:  _c_get_name()
 * Incept:    EPN, Mon Jul  8 10:01:47 2013
 * Synopsis:  Returns msa->name.
 * Returns:   MSA's name or 'none' if none set.
 */
char *_c_get_name (ESL_MSA *msa)
{
  if(msa->name) return msa->name;
  else          return "none";
}

/* Function:  _c_set_accession()
 * Incept:    EPN, Sat Feb  2 14:36:27 2013
 * Synopsis:  Sets msa->acc to newacc
 * Returns:   eslOK on success.
 */
int _c_set_accession (ESL_MSA *msa, char *newacc)
{
  int status;
  status = esl_msa_SetAccession(msa, newacc, -1);
  return status;
}

/* Function:  _c_set_name()
 * Incept:    EPN, Mon Jul  8 10:02:36 2013
 * Synopsis:  Sets msa->name to newname
 * Returns:   eslOK on success.
 */
int _c_set_name (ESL_MSA *msa, char *newname)
{
  int status;
  status = esl_msa_SetName(msa, newname, -1);
  return status;
}

/* Function:  _c_get_sqname()
 * Incept:    EPN, Sat Feb  2 14:37:09 2013
 * Synopsis:  Returns msa->sqname[idx]
 * Returns:   msa->sqname[idx]
 */
char *_c_get_sqname (ESL_MSA *msa, I32 idx)
{
  return msa->sqname[idx];
}

/* Function:  _c_get_sqidx()
 * Incept:    EPN, Mon Feb  3 14:22:06 2014
 * Synopsis:  Returns sequence index of seq named <sqname>
 */
int _c_get_sqidx (ESL_MSA *msa, char *sqname)
{
  int idx, status;
  if(msa->index == NULL) croak ("ERROR, msa->index is NULL in _c_get_sqidx");
  if(esl_keyhash_GetNumber(msa->index) == 0) croak ("ERROR, msa->index has no keys in _c_get_sqidx");
  status = esl_keyhash_Lookup(msa->index, sqname, -1, &idx);
  if(status == eslENOTFOUND) return -1;
  else if(status == eslOK)   return idx;
  else                       croak ("ERROR, unexpected error in _c_get_sqidx");
}

/* Function:  _c_set_sqname()
 * Incept:    EPN, Sat Feb  2 14:37:34 2013
 * Synopsis:  Sets msa->sqname[idx]
 * Returns:   void
 */
void _c_set_sqname (ESL_MSA *msa, I32 idx, char *newname)
{
    if(msa->sqname[idx]) free(msa->sqname[idx]);
    esl_strdup(newname, -1, &(msa->sqname[idx]));
    return;
}   

/* Function:  _c_has_sqwgts()
 * Incept:    EPN, Tue Apr  1 13:49:31 2014
 * Synopsis:  Returns '1' if msa has weights, as indicated by
 *            eslMSA_HASWGTS flag.
 * Returns:   '1' if eslMSA_HASWGTS flag is up, else returns 0.
 */
int _c_has_sqwgts (ESL_MSA *msa)
{
  return (msa->flags & eslMSA_HASWGTS) ? 1 : 0;
}

/* Function:  _c_get_sqwgt()
 * Incept:    EPN, Fri May 24 10:48:17 2013
 * Synopsis:  Returns msa->wgt[idx]
 * Returns:   msa->wgt[idx]
 */
double _c_get_sqwgt (ESL_MSA *msa, I32 idx)
{
  return msa->wgt[idx];
}

/* Function:  _c_remove_sqwgts()
 * Incept:    EPN, Thu Feb 20 14:34:20 2014
 * Synopsis:  Removes sequence weights from an MSA.
 * Returns:   void
 */
void _c_remove_sqwgts(ESL_MSA *msa)
{
  int i;
  for(i = 0; i < msa->nseq; i++) { 
    msa->wgt[i] = 1.0;
  }
  msa->flags &= ~eslMSA_HASWGTS; /* drop the flag */

  return;
}

/* Function:  _c_any_allgap_columns()
 * Incept:    EPN, Sat Feb  2 14:38:18 2013
 * Synopsis:  Checks for any all gap columns.
 * Args:      msa: the alignment
 *            gapstr: string of gaps (e.g. "-_.~"), can be NULL if msa is digitized.
 * Returns:   TRUE if any all gap columns exist, else FALSE.
 */
int _c_any_allgap_columns (ESL_MSA *msa, char *gapstr) 
{
  int apos, idx; 
  
  /***************** digital mode **************************/
  if(msa->flags & eslMSA_DIGITAL) { 
    for (apos = 1; apos <= msa->alen; apos++) {
      for (idx = 0; idx < msa->nseq; idx++) {
        if (! esl_abc_XIsGap(msa->abc, msa->ax[idx][apos]) &&
            ! esl_abc_XIsMissing(msa->abc, msa->ax[idx][apos])) { 
          break;
        }
      }
      if(idx == msa->nseq) { /* apos is an all gap column */
        return TRUE; 
      }
    }
  }
  /***************** text mode **************************/
  else { 
    for (apos = 0; apos < msa->alen; apos++) {
      for (idx = 0; idx < msa->nseq; idx++) {
	{ /* check all seqs to see if this column is all gaps */
          if (strchr(gapstr, msa->aseq[idx][apos]) == NULL) { 
            break;
          }
        }
      }
      if(idx == msa->nseq) { /* apos is an all gap column */
        return TRUE; 
      }
    }
  }
  /****************************************************/
  return FALSE;
}   

/* Function:  _c_average_id()
 * Incept:    EPN, Sat Feb  2 14:38:18 2013
 * Purpose:   Calculate and return average fractional identity of 
 *            an alignment. If more than max_nseq sequences exist
 *            take a sample of (max_nseq)^2 pairs and return the 
 *            average fractional identity of those.
 * Returns:   Average fractional identity.
 */
float _c_average_id(ESL_MSA *msa, int max_nseq) 
{
  double avgid;
  
  if(msa->flags & eslMSA_DIGITAL) { 
    esl_dst_XAverageId(msa->abc, msa->ax, msa->nseq, (max_nseq * max_nseq), &avgid);
  }
  else { 
    esl_dst_CAverageId(msa->aseq, msa->nseq, (max_nseq * max_nseq), &avgid);
  }
  return (float) avgid;
}

/* Function:  _c_get_sqstring_aligned()
 * Incept:    EPN, Fri May 24 11:03:49 2013
 * Purpose:   Return aligned sequence <seqidx>.
 * Returns:   Aligned sequence <seqidx>.
 */
SV *_c_get_sqstring_aligned(ESL_MSA *msa, int seqidx)
{
  int status;
  SV *seqstringSV;  /* SV version of msa->ax[->seq */
  char *seqstring;

  ESL_ALLOC(seqstring, sizeof(char) * (msa->alen + 1));
  if(msa->flags & eslMSA_DIGITAL) { 
    if((status = esl_abc_Textize(msa->abc, msa->ax[seqidx], msa->alen, seqstring)) != eslOK) croak("failed to textize digitized aligned sequence");
  }
  else { /* text mode */
    if((status = esl_strdup(msa->aseq[seqidx], msa->alen, &seqstring)) != eslOK) croak("failed to duplicate text aligned sequence");
  }    

  seqstringSV = newSVpv(seqstring, msa->alen);
  free(seqstring);

  return seqstringSV;

 ERROR: 
  croak("out of memory");
  return NULL;
}

/* Function:  _c_get_ppstring_aligned()
 * Incept:    EPN, Mon Jul  7 09:20:03 2014
 * Purpose:   Return aligned PP annotation for sequence <seqidx>.
 * Returns:   PP annotation for aligned sequence <seqidx>.
 */
SV *_c_get_ppstring_aligned(ESL_MSA *msa, int seqidx)
{
  int status;
  SV *ppstringSV;  /* SV version of msa->ax[->seq */
  char *ppstring;

  if(_c_check_ppidx(msa, seqidx) == 0) { croak("no PP annotation for sequence"); }

  ESL_ALLOC(ppstring, sizeof(char) * (msa->alen + 1));
  if((status = esl_strdup(msa->pp[seqidx], msa->alen, &ppstring)) != eslOK) croak("failed to duplicate text aligned posterior probability annotation");

  ppstringSV = newSVpv(ppstring, msa->alen);
  free(ppstring);

  return ppstringSV;

 ERROR: 
  croak("out of memory");
  return NULL;
}

/* Function:  _c_get_sqstring_unaligned()
 * Incept:    EPN, Fri May 24 13:08:17 2013
 * Purpose:   Return unaligned sequence <seqidx>.
 * Returns:   Unaligned sequence <seqidx>.
 */
SV *_c_get_sqstring_unaligned(ESL_MSA *msa, int seqidx)
{
  int status;
  ESL_SQ *sq = NULL;    /* the sequence, fetched from the msa */
  SV     *seqstringSV;  /* SV version of sq->seq */
  
  status = esl_sq_FetchFromMSA(msa, seqidx, &sq);
  if(status != eslOK) croak("failed to fetch seq %d from msa\n", seqidx);

  if(msa->flags & eslMSA_DIGITAL) { 
    /* convert digital mode to text mode */
    if(sq->dsq == NULL) croak("fetched seq %d from digitized msa, and it's unexpectedly NOT digitized", seqidx);
    if((status = esl_sq_Textize(sq)) != eslOK) croak("failed to textize fetched seq from MSA");
  }
  else { /*text mode, no need to digitize, but verify */
    if(sq->seq == NULL) croak("fetched seq %d from textized msa, and it's unexpectedly not in text mode", seqidx);
  }

  seqstringSV = newSVpv(sq->seq, sq->n);
  esl_sq_Destroy(sq);

  return seqstringSV;
}
 
/* Function:  _c_get_sqlen()
 * Incept:    EPN, Sat Feb  2 14:38:18 2013
 * Purpose:   Return unaligned sequence length of sequence <seqidx>.
 * Returns:   Sequence length of sequence <seqidx>.
 */
int _c_get_sqlen(ESL_MSA *msa, int seqidx)
{
  int apos;
  int len = 0;

  if(msa->flags & eslMSA_DIGITAL) { 
    return (int) esl_abc_dsqrlen(msa->abc, msa->ax[seqidx]);
  }
  else { 
    for (apos=0; apos < msa->alen; apos++) { 
      if (strchr("-_.~", msa->aseq[seqidx][apos]) == NULL) len++; 
    }
    return len;
  }
}

/* Function:  _c_get_column()
 * Incept:    EPN, Tue Feb 18 09:25:07 2014
 * Purpose:   Return alignment column <apos> (1..alen)
 * Returns:   Alignment column <apos> as a PERLized string
 */
SV *_c_get_column(ESL_MSA *msa, int apos)
{
  int status;
  SV *columnSV;  /* SV version of column */
  char *column;
  int i;

  ESL_ALLOC(column, sizeof(char) * (msa->nseq + 1));
  column[msa->nseq] = '\0';
  if(msa->flags & eslMSA_DIGITAL) { /* digital mode */
    for(i = 0; i < msa->nseq; i++) { 
      column[i] = msa->abc->sym[msa->ax[i][apos]];
    }
  }
  else { /* text mode */
    for(i = 0; i < msa->nseq; i++) { 
      column[i] = msa->aseq[i][apos-1];
    }
  }

  columnSV = newSVpv(column, msa->nseq);
  free(column);

  return columnSV;

 ERROR: 
  croak("out of memory");
  return NULL;
}

/* Function:  _c_count_residues()
 * Incept:    March 5, 2013
 * Purpose:   Count residues in all sequences;
 * Returns:   Total residues.
 */
float _c_count_residues(ESL_MSA *msa)
{
  int i;
  float len = 0.;
  for(i = 0; i < msa->nseq; i++) { 
    len += _c_get_sqlen(msa, i);
  }
  
  return len;
}

/* Function:  _c_average_sqlen()
 * Incept:    EPN, Sat Feb  2 14:43:18 2013
 * Purpose:   Calculate and return average unaligned sequence length.
 * Returns:   Average unaligned sequence length.
 */
float _c_average_sqlen(ESL_MSA *msa)
{ 
  return (_c_count_residues(msa) / msa->nseq);
}


/* Function:  _c_addGF()
 * Incept:    EPN, Sat Feb  2 14:48:47 2013
 * Purpose:   Add GF annotation to MSA.
 * Returns:   eslOK on success, ! eslOK on failure.
 */
int _c_addGF(ESL_MSA *msa, char *tag, char *value)
{
  int    status;
  status = esl_msa_AddGF(msa, tag, -1, value, -1);
  return status;
}

/* Function:  _c_addGC()
 * Incept:    EPN, Wed Feb  4 11:00:17 2015
 * Purpose:   Add GC annotation to MSA.
 * Returns:   eslOK on success, ! eslOK on failure.
 */
int _c_addGC(ESL_MSA *msa, char *tag, char *value)
{
  int    status;
  status = esl_msa_AppendGC(msa, tag, value);
  return status;
}

/* Function:  _c_addGS()
 * Incept:    EPN, Sat Feb  2 14:48:47 2013
 * Purpose:   Add GS annotation to a sequence in a MSA.
 * Returns:   eslOK on success, ! eslOK on failure.
 */
int _c_addGS(ESL_MSA *msa, int sqidx, char *tag, char *value)
{
  int    status;
  status = esl_msa_AddGS(msa, tag, -1, sqidx, value, -1);
  return status;
}

/* Function:  _c_addGC_identity()
 * Incept:    EPN, Fri Nov  8 09:36:24 2013
 * Purpose:   Determine and add GC ID annotation to a MSA.
 *            all columns that are 100% identical will 
 *            be indicated with either the residue that
 *            occurs in all seqs (if $use_res) or a 
 *            '*' (if ! $use_res). Non-identical columns
 *            are annotated as '.'.
 *
 *            Gaps are considered residues. That is,
 *            a column in which nseq-1 sequences are
 *            an 'A', but 1 sequence is a gap is NOT
 *            100% identical.
 * 
 * Returns:   eslOK on success, ! eslOK on failure.
 */
int _c_addGC_identity(ESL_MSA *msa, int use_res) 
{
  int     status;
  int     apos, idx;
  ESL_DSQ dres;
  char    cres, cres2;
  char    *id = NULL;

  ESL_ALLOC(id, sizeof(char) * (msa->alen + 1));
  id[msa->alen] = '\0';

  /* first create the annotation */
  /********************** digital mode ****************************/
  if(msa->flags & eslMSA_DIGITAL) { 
    for (apos = 1; apos <= msa->alen; apos++) {
      dres = msa->ax[0][apos];
      idx = 1;
      if(esl_abc_XIsResidue(msa->abc, dres)) { 
        for (; idx < msa->nseq; idx++) {
          if(msa->ax[idx][apos] != dres) break;
        }
      }
      if(idx == msa->nseq) { /* column is same dresidue in all seqs */
          id[apos-1] = (use_res) ? msa->abc->sym[dres] : '*';
      }
      else { /* column has at least 2 different residues */
        id[apos-1] = '.';
      }
    }
    /********************** text mode ****************************/
  }
  else { 
    for (apos = 0; apos < msa->alen; apos++) {
      cres = msa->aseq[0][apos];
      idx = 1;
      if (isalpha(cres)) { 
        if (islower(cres)) cres = toupper(cres);
        for (; idx < msa->nseq; idx++) {
          cres2 = msa->aseq[idx][apos];
          if (islower(cres2)) cres2 = toupper(cres2);
          if(cres2 != cres) break;
        }
      }
      if(idx == msa->nseq) { /* column is same residue in all seqs */
        id[apos] = (use_res) ? cres : '*';
      }
      else { /* column has at least 2 different residues */
        id[apos] = '.';
      }
    }
  }

  status = esl_msa_AppendGC(msa, "ID", id);
  free(id);

  return status;

 ERROR: 
  if(id) free(id);
  return status; 
}

/* Function:  _c_hasGC
 * Incept:    EPN, Wed Feb  4 15:07:26 2015
 * Synopsis:  Returns '1' if a msa has GC annotation with the tag <tag>,
 *            else returns '0'.
 */
int _c_hasGC (ESL_MSA *msa, char *tag)
{
  int status;
  int tagidx;

  if(strcmp(tag, "SS_cons") == 0) return _c_has_ss_cons(msa);
  if(strcmp(tag, "RF")      == 0) return _c_has_rf(msa);

  if(strcmp(tag, "SA_cons") == 0) { return (msa->sa_cons == NULL) ? 0 : 1; }
  if(strcmp(tag, "PP_cons") == 0) { return (msa->pp_cons == NULL) ? 0 : 1; }
  if(strcmp(tag, "MM")      == 0) { return (msa->mm      == NULL) ? 0 : 1; }
  /* not a parsed tag, search for it */
  /* get tagidx for this GC tag */
  if(msa->ngc > 0) { 
    status = esl_keyhash_Lookup(msa->gc_idx, tag, -1, &tagidx);
    return (status == eslOK) ? 1 : 0;
  }
  else { 
    return 0;
  }   
}

/* Function:  _c_getGC_given_tag
 * Incept:    EPN, Fri May 24 09:58:32 2013
 * Synopsis:  Returns the GC annotation pertaining to tag <tag>.
 * Returns:   the GC annotation with tag <tag> as a string
 */
char *_c_getGC_given_tag (ESL_MSA *msa, char *tag)
{
  int status;
  int tagidx;

  if(! (_c_hasGC(msa, tag))) croak("_c_getGC, no such annotation exists");
  if(strcmp(tag, "SS_cons") == 0) return _c_get_ss_cons(msa);
  if(strcmp(tag, "RF")      == 0) return _c_get_rf(msa);
  /* we've already verified it exists with the _c_hasGC call, hence
   * the lack of checks for NULL below */
  if(strcmp(tag, "SA_cons") == 0) return msa->sa_cons;
  if(strcmp(tag, "PP_cons") == 0) return msa->pp_cons;
  if(strcmp(tag, "MM")      == 0) return msa->mm;

  /* not a parsed tag, search for it */
  /* get tagidx for this GC tag. existing tag: <ngc; new: == ngc. */
  status = esl_keyhash_Lookup(msa->gc_idx, tag, -1, &tagidx);
  if (status != eslOK) croak("_c_getGC unexpected error, tag %s seems to exist but it does not", tag);
  return msa->gc[tagidx];
}

/* Function:  _c_getGC_given_idx
 * Incept:    EPN, Wed Feb  4 20:51:34 2015
 * Synopsis:  Returns the GC annotation of idx <tagidx>.
 * Returns:   the GC annotation of idx <tagidx>
 */
char *_c_getGC_given_idx (ESL_MSA *msa, int tagidx)
{
  if(tagidx >= msa->ngc) croak("_c_getGC_given_idx, no such tagidx exists");
  return msa->gc[tagidx];
}

/* Function:  _c_getGC_number
 * Incept:    EPN, Wed Feb  4 17:40:34 2015
 * Synopsis:  Return number of GC tag annotations in msa (msa->ngc).
 * Returns:   msa->ngc
 */
int _c_getGC_number (ESL_MSA *msa)
{
  return msa->ngc;
}

/* Function:  _c_getGC_tag
 * Incept:    EPN, Wed Feb  4 17:41:30 2015
 * Synopsis:  Return tag number <idx> of GC annotation, or die if it
 *            doesn't exist.
 * Returns:   msa->gc_tag[idx];
 */
char *_c_getGC_tag (ESL_MSA *msa, int tagidx)
{
  if(tagidx >= msa->ngc) croak("_c_getGC_tag, no such tagidx exists");
  return(msa->gc_tag[tagidx]);
}

/* Function:  _c_getGC_tagidx
 * Incept:    EPN, Wed Feb  4 21:06:24 2015
 * Synopsis:  Return tag idx of GC annotation with tag <tag>
 * Returns:   idx of GC annotation with tag <tag>
 * Dies:      if the idx does not exist
 */
int _c_getGC_tagidx (ESL_MSA *msa, char *tag)
{
  int status;
  int tagidx;

  if(msa->ngc > 0) { 
    status = esl_keyhash_Lookup(msa->gc_idx, tag, -1, &tagidx);
    if(status == eslOK) return tagidx;
  }
  
  croak("_c_getGC_tagidx, no such tag exists");
  return -1; /* never reached */
}   

/* Function:  _c_weight_GSC()
 * Incept:    EPN, Fri May 24 10:40:00 2013
 * Purpose:   Calculate sequence weights using the GSC (Gerstein/Sonnhammer/Chotia) 
 *            algorithm.
 * Returns:   eslOK on success, ! eslOK on failure.
 */
int _c_weight_GSC(ESL_MSA *msa) 
{
  int    status;
  status = esl_msaweight_GSC(msa);
  return status;
}

/* Function:  _c_msaweight_IDFilter()
 * Incept:    March 1, 2013
 * Purpose:   Calculate and output msa after %id weight filtering
 * Returns:   weighted msa object on success
 *            NULL on failure
 */

SV *_c_msaweight_IDFilter(ESL_MSA *msa_in, double maxid)
{
  int status;
  ESL_MSA      *msa_out;        /* an alignment */
  
  status = esl_msaweight_IDFilter(msa_in, maxid, &msa_out);
  if(status != eslOK)
  {
    fprintf(stderr, "Failure code %d when attempting to call esl_msaweight_IDFilter", status);
    return NULL;
  } 
  
  return perl_obj(msa_out, "ESL_MSA");
}

/* Function:  _c_percent_coverage()
 * Incept:    March 4, 2013
 * Purpose:   Calculate and output sequence coverage ratios for each alignment position in an msa
 * Returns:   array of size 0 to msa->alen, represents position in alignemnt coverage ratio
 *            Nothing on failure
 */

void _c_percent_coverage(ESL_MSA *msa)
{
  Inline_Stack_Vars;
  
  int status;
  int apos, i;
  double **abc_ct = NULL;
  double ret = 0.0;
  
  //don't let user divide by 0
  if(msa->nseq <= 0)
  {
    fprintf(stderr, "invalid number of sequences in msa: %d", msa->nseq);
    return;// NULL;
  }
  
  //first allocate abc_ct matrix
  ESL_ALLOC(abc_ct, sizeof(double *) * msa->alen); 
  for(apos = 0; apos < msa->alen; apos++) 
  { 
    ESL_ALLOC(abc_ct[apos], sizeof(double) * (msa->abc->K+1));
    esl_vec_DSet(abc_ct[apos], (msa->abc->K+1), 0.);
  }
  
  //populate abc_ct
  for(i = 0; i < msa->nseq; i++) 
  { 
    for(apos = 0; apos < msa->alen; apos++) 
    { /* update appropriate abc count, careful, ax ranges from 1..msa->alen (but abc_ct is 0..msa->alen-1) */
      if(! esl_abc_XIsDegenerate(msa->abc, msa->ax[i][apos+1])) 
      {
        if((status = esl_abc_DCount(msa->abc, abc_ct[apos], msa->ax[i][apos+1], 1.0)) != eslOK)
        {
          fprintf(stderr, "problem counting residue %d of seq %d", apos, i);
          return;
        }
      }
    }
  }
  
  Inline_Stack_Reset;
  
  //determine coverage ratio for each position, push it onto the perl return stack
  for(apos = 0; apos < msa->alen; apos++)
  {
    ret = esl_vec_DSum(abc_ct[apos], msa->abc->K);
    Inline_Stack_Push(sv_2mortal(newSVnv(ret / (double) msa->nseq)));
  } 
  
  Inline_Stack_Done;
  Inline_Stack_Return(msa->alen);
  
  ERROR:
    fprintf(stderr, "Memory allocation in _c_percent_coverage failed");
    return;
}

/* Function: _c_bp_dist
 * Incept:   EPN, Thu Jul 11 10:50:25 2013
 * Purpose:  Helper function for _c_rfam_bp_stats()
 *           Given two base pairs, reprensented by ints (a1:b1 and a2:b2)
 *           where a1,b1,a2,b2 are all in the range 0..msa->abc->Kp-1 (RNA's Kp-1),
 *           return:
 *             0 if  a1==a2 && b1==b2, 
 *             1 if (a1!=a2 && b1==b2) || (a1==a2 && b1!=b2),
 *             2 if (a1!=a2 && b1!=b2)
 */
int 
_c_bp_dist(int a1, int b1, int a2, int b2) 
{
  if     (a1 == a2 && b1 == b2) return 0;
  else if(a1 != a2 && b1 != b2) return 2;
  else                          return 1;
}

/* Function: _c_bp_is_canonical
 * Incept:   EPN, Thu Jul 11 10:44:04 2013
 * Purpose:  Helper function for _c_rfam_bp_stats()
 *           Determine if two indices represent two residues 
 *           that form a canonical base pair or not.
 *
 * Returns:  TRUE if:
 *            ldsq   rdsq
 *           -----  ------
 *           0 (A)  3 (U)
 *           3 (U)  0 (A)
 *           1 (C)  2 (G)
 *           2 (G)  1 (C)
 *           2 (G)  3 (U)
 *           3 (U)  2 (G)
 *
 * (below are ambiguous, included because they were included in Paul's
 * original code rqc-ss-cons.pl)
 *
 *           5 (R)  6 (Y)
 *           6 (Y)  5 (R)
 *           7 (M)  8 (K)
 *           8 (K)  7 (M)
 *           9 (S)  9 (S)
 *          10 (W) 10 (W)
 *           Else, return FALSE.
 */
int 
_c_bp_is_canonical(int a, int b)
{
  switch (a) { 
  case 0:
    switch (b) {
    case 3: return TRUE; break;
    default: break;
    }
    break;
  case 1:
    switch (b) { 
    case 2: return TRUE; break;
    default: break;
    }
    break;
  case 2:
    switch (b) { 
    case 1: return TRUE; break;
    case 3: return TRUE; break;
    default: break;
    }
    break;
  case 3:
    switch (b) { 
    case 0: return TRUE; break;
    case 2: return TRUE; break;
    default: break;
    }
    break;
  case 5:
    switch (b) { 
    case 6: return TRUE; break;
    default: break;
    }
    break;
  case 6:
    switch (b) { 
    case 5: return TRUE; break;
    default: break;
    }
    break;
  case 7:
    switch (b) { 
    case 8: return TRUE; break;
    default: break;
    }
    break;
  case 8:
    switch (b) { 
    case 7: return TRUE; break;
    default: break;
    }
    break;
  case 9:
    switch (b) { 
    case 9: return TRUE; break;
    default: break;
    }
    break;
  case 10:
    switch (b) { 
    case 10: return TRUE; break;
    default: break;
    }
    break;
  default: break;
  }
  
  return FALSE;
}

/* Function: _c_max_rna_two_letter_ambiguity
 * Incept:   EPN, Mon Jul 15 13:24:33 2013
 * Purpose:  Helper function for _c_rfam_qc_stats().
 *           Given A, C, G, and U counts, determine the two-letter 
 *           IUPAC ambiguity code that is most common and the
 *           fraction of counts it represents.
 *
 *           M == A or C
 *           R == A or G
 *           W == A or U
 *           S == C or G
 *           Y == C or U
 *           K == G or U
 *
 * Returns:  Max 2 letter ambiguity as a character
 *           in *ret_max_2l and the fraction of
 *           total counts (i.e. total weighted length)
 *           that the maximum character represents.
 */
void 
_c_max_rna_two_letter_ambiguity(double act, double cct, double gct, double uct, char *ret_max_2l, double *ret_max_2l_frac)
{
  double sum;          /* act + cct + gct + uct */
  char   max_2l;       /* max 2 letter ambiguity */
  double max_2l_frac;  /* fraction of sum represented by max_2l */

  sum = act + cct + gct + uct;
  max_2l = 'M'; /* A or C */
  max_2l_frac = (act + cct) / sum;
  if(((act + gct) / sum) > max_2l_frac) { 
    max_2l = 'R'; /* A or G */
    max_2l_frac = (act + cct) / sum;
  }
  if(((act + uct) / sum) > max_2l_frac) { 
    max_2l = 'W'; /* A or U */
    max_2l_frac = (act + uct) / sum;
  }
  if(((cct + gct) / sum) > max_2l_frac) { 
    max_2l = 'S'; /* C or G */
    max_2l_frac = (cct + gct) / sum;
  }
  if(((cct + uct) / sum) > max_2l_frac) { 
    max_2l = 'Y'; /* C or U */
    max_2l_frac = (cct + uct) / sum;
  }
  if(((gct + uct) / sum) > max_2l_frac) { 
    max_2l = 'K'; /* G or U */
    max_2l_frac = (gct + uct) / sum;
  }

  *ret_max_2l = max_2l;
  *ret_max_2l_frac = max_2l_frac;

  return;
}

/* Function: _c_rfam_comp_and_len_stats
 * Incept:   EPN, Tue Jul 16 09:06:31 2013
 * Purpose:  Helper function for _c_rfam_qc_stats().  Determine the
 *           per-sequence and total sequence counts of an
 *           MSA as well as unaligned lengths of all seqs and total
 *           summed length.
 * 
 *           MSA must be digitized because we count up number of
 *           each residue.
 *
 * Returns:  Allocated and returned:
 *         
 *           ret_abcAA:    [0..i..msa->nseq-1][0..a..msa->abc->K]: weighted counts of nt 'a' in sequence 'i',  a==abc->K are gaps, missing residues or nonresidues 
 *           ret_abc_totA: [0..a..msa->abc->K]:                    weighted counts of nt 'a' in all sequences, a==abc->K are gaps, missing residues or nonresidues 
 *           ret_lenA:     [0..i..msa->nseq-1]:                    nongap len of sequence i 
 *           ret_len_tot:  total length of all sequences
 *           ret_len_min:  minimum sequence length
 *           ret_len_max:  maximum sequence length
 *
 *           eslOK if successful
 *           eslEMEM if we run out of memory
 */
int
_c_rfam_comp_and_len_stats(ESL_MSA *msa, double ***ret_abcAA, double **ret_abc_totA, int **ret_lenA, int *ret_len_tot, int *ret_len_min, int *ret_len_max)
{
  int        status;           /* Easel status */
  int        i;                /* counter over sequences */
  int        apos;             /* alignment position counter */
  double   **abcAA    = NULL;  /* [0..i..msa->nseq-1][0..a..abc->K]: count of nt 'a' in sequence 'i', a==abc->K are gaps, missing residues or nonresidues */
  double    *abc_totA = NULL;  /* [0..a..abc->K]: count of nt 'a' in all sequences, a==abc->K are gaps, missing residues or nonresidues */
  int       *lenA     = NULL;  /* [0..i..msa->nseq-1]: nongap length of sequence i */
  int        len_tot  = 0;     /* total length of all seqs */ 
  int        len_min  = 0;     /* minimum seq length */
  int        len_max  = 0;     /* maximum seq length */
  double     seqwt;            /* sequence weight */
  int        have_weights;     /* set to '1' if MSA has valid weights, else it does not and we'll use 1.0 as the weight for all sequences */
  
  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_rfam_comp_stats() contract violation, MSA is not digitized");
  have_weights = (msa->flags & eslMSA_HASWGTS) ? 1 : 0;

  /* allocate and initialize */
  ESL_ALLOC(abcAA,       sizeof(double *)  * msa->nseq); 
  ESL_ALLOC(abc_totA,    sizeof(double) * (msa->abc->K+1)); 
  esl_vec_DSet(abc_totA, msa->abc->K+1, 0.);
  for(i = 0; i < msa->nseq; i++) { 
    ESL_ALLOC(abcAA[i], sizeof(double) * (msa->abc->K+1));
    esl_vec_DSet(abcAA[i], (msa->abc->K+1), 0.);
  }

  ESL_ALLOC(lenA, sizeof(int) * msa->nseq); 
  esl_vec_ISet(lenA, msa->nseq, 0);

  /* add counts and compute lengths */
  for(i = 0; i < msa->nseq; i++) { 
    seqwt = (have_weights) ? msa->wgt[i] : 1.0;
    for(apos = 0; apos < msa->alen; apos++) { 
      if(esl_abc_XIsResidue(msa->abc, msa->ax[i][apos+1])) lenA[i]++; 
      if((status = esl_abc_DCount(msa->abc, abcAA[i], msa->ax[i][apos+1], seqwt)) != eslOK) croak("problem counting residue %d of seq %d", apos, i);
    }
    esl_vec_DAdd(abc_totA, abcAA[i], msa->abc->K+1); /* add this seqs count to the abc_totA array */
    len_tot += lenA[i];
    len_min = (i == 0) ? lenA[i] : ESL_MIN(len_min, lenA[i]);
    len_max = (i == 0) ? lenA[i] : ESL_MAX(len_max, lenA[i]);
  }

  /* note: we do NOT normalize counts, this is impt for _c_rfam_qc_stats() */

  *ret_abcAA    = abcAA;
  *ret_abc_totA = abc_totA;
  *ret_lenA     = lenA;
  *ret_len_tot  = len_tot;
  *ret_len_min  = len_min;
  *ret_len_max  = len_max;

  return eslOK;

 ERROR: 
  if(abcAA) { 
    for(i = 0; i < msa->nseq; i++) { 
      if(abcAA[i]) free(abcAA[i]);
    }
    free(abcAA);
  }
  if(abc_totA) free(abc_totA);
  if(lenA)     free(lenA);

  croak("out of memory");
  return eslEMEM; /* NOTREACHED */
}

/* Function: _c_rfam_bp_stats
 * Incept:   EPN, Mon Jul 15 14:20:49 2013
 * Purpose:  Helper function for _c_rfam_qc_stats().  Determine the
 *           basepairs in the consensus structure of the MSA, after
 *           potentially removing pseudoknots. Then, calculate the
 *           fraction of canonical basepairs as well as a covariation
 *           statistic and return the information _c_rfam_qc_stats()
 *           will need to output.
 *
 *           See comments in the code for details on the 'covariation
 *           statistic'.
 *
 * Returns:  Allocates and returns:
 *
 *           ret_nbp:      number of basepairs in (potentially deknotted) consensus secondary structure 
 *           ret_rposA:    [0..i..msa->alen-1]: right position for basepair with left half position of 'i', else -1 if 'i' is not left half of a pair (i always < j) 
 *           ret_seq_canA  [0..i..msa->nseq-1]: number of canonical basepairs in sequence i 
 *           ret_pos_canA  [0..i..msa->alen-1]: number of canonical basepairs with left half position of 'i' 
 *           ret_covA      [0..i..msa->alen-1]: 'covariation statistic' for basepair 'i'
 *           ret_mean_cov: total covariation sum of ret_covA, divided by 'tau' (see code)
 *
 *           eslOK if successful
 *           eslEMEM if out of memory
 */
int
_c_rfam_bp_stats(ESL_MSA *msa, int *ret_nbp, int **ret_rposA, int **ret_seq_canA, int **ret_pos_canA, double **ret_covA, double *ret_mean_cov)
{
  int        status;               /* Easel status */
  int       *ct = NULL;            /* 0..alen-1 base pair partners array for current sequence */
  char      *ss_nopseudo = NULL;   /* no-pseudoknot version of structure */
  int        have_weights;         /* set to '1' if MSA has valid weights, else it does not and we'll use 1.0 as the weight for all sequences */
  double     seqwt1, seqwt2;       /* weight of current sequences */
  int        nbp = 0;              /* number of canonical basepairs in the (possibly deknotted) consensus secondary structure */
  int       *seq_canA = NULL;      /* [0..i..msa->nseq-1]: number of canonical basepairs in sequence i */
  int       *rposA    = NULL;      /* [0..apos..msa->alen-1]: right position for basepair with left half position of 'apos', else -1 if 'apos' is not left half of a pair (apos always < rpos) */
  int       *pos_canA = NULL;      /* [0..apos..msa->alen-1]: number of canonical basepairs with left half position of 'apos' */
  double    *covA     = NULL;      /* [0..apos..msa->alen-1]: covariation per basepair */
  double    *cov_cntA = NULL;      /* [0..apos..msa->alen-1]: weighted count of basepair covariation per basepair */
  int        apos, rpos;           /* counters over alignment positions */
  int        i, j;                 /* counters over sequences */

  /* variables used when calculating covariation statistic */
  int a1, b1;            /* int index of left, right half of basepair 1 */
  int a2, b2;            /* int index of left, right half of basepair 1 */
  int d;                 /* distance between a1:b1 and a2:b2 (number of differences) */
  int iscanonical1;      /* is a1:b1 a canonical pair? (by Paul's definition, see _c_bp_is_canonical() */
  int iscanonical2;      /* is a2:b2 a canonical pair? (by Paul's definition, see _c_bp_is_canonical() */
  double contrib = 0.;   /* contribution of current a1:b1 compared to a2:b2 */
  double mean_cov = 0.;  /* mean covariation statistic */

  have_weights = (msa->flags & eslMSA_HASWGTS) ? 1 : 0;

  /* get ct array which defines the consensus base pairs */
  ESL_ALLOC(ct,  sizeof(int)  * (msa->alen+1));
  ESL_ALLOC(ss_nopseudo, sizeof(char) * (msa->alen+1));
  esl_wuss_nopseudo(msa->ss_cons, ss_nopseudo);
  if ((status = esl_wuss2ct(ss_nopseudo, msa->alen, ct)) != eslOK) croak("Consensus structure string is inconsistent.");

  /* allocate and initialize */
  ESL_ALLOC(rposA,    sizeof(int)       * msa->alen); 
  ESL_ALLOC(pos_canA, sizeof(int)       * msa->alen); 
  ESL_ALLOC(seq_canA, sizeof(int)       * msa->nseq); 
  esl_vec_ISet(rposA,    msa->alen, -1);
  esl_vec_ISet(pos_canA, msa->alen, 0);
  esl_vec_ISet(seq_canA, msa->nseq, 0);

  /* determine location of basepairs and count them */
  for(apos = 0; apos < msa->alen; apos++) { 
    /* careful ct is indexed 1..alen, not 0..alen-1 */
    if(ct[(apos+1)] > (apos+1)) { /* apos+1 is an 'i' in an i:j pair, where i < j */
      rposA[apos] = ct[(apos+1)]-1; /* rposA is indexed 0..msa->alen-1 */
      nbp++;
    }
  }

  /* Calculate covariation statistic. 
   *
   * This is a reimplementation of Paul's covariation statistic from
   * rqc-ss-cons.pl which he said when asked (via email 07.11.13) was
   * the "RNAalifold covariation statistic" with the reference being:
   * (Lindgreen, Stinus, Paul P. Gardner, and Anders Krogh. "Measuring
   * covariation in RNA alignments: physical realism improves
   * information measures."  Bioinformatics 22.24 (2006): 2988-2995) .
   *
   * I actually haven't looked at that paper but simply reimplemented
   * what Paul had, so the new function exactly reproduced the
   * original script. Best documentation is probably the code below,
   * unfortunately.
   *
   * Note this is O(N^2) for N sequences because we have to look at
   * each pair of sequences. I was fairly certain a O(N) algorithm
   * existed, but I had trouble getting it to work properly and gave
   * up lest I waste too much time trying to fix it. This O(N^2) 
   * approach is from Paul's rqc-ss-cons.pl.
   */
  ESL_ALLOC(covA,     sizeof(double) * msa->alen);
  ESL_ALLOC(cov_cntA, sizeof(double) * msa->alen);
  esl_vec_DSet(covA,     msa->alen, 0.);
  esl_vec_DSet(cov_cntA, msa->alen, 0.);
  for(i = 0; i < msa->nseq; i++) { 
    seqwt1 = (have_weights) ? msa->wgt[i] : 1.0;
    for(apos = 0; apos < msa->alen; apos++) { 
      if(rposA[apos] != -1) { 
        rpos = rposA[apos]; 
        a1 = msa->ax[i][apos+1];
        b1 = msa->ax[i][rpos+1];
        if(a1 != msa->abc->K || b1 != msa->abc->K) { 
          iscanonical1 = _c_bp_is_canonical(a1, b1);
          if(iscanonical1) { 
            seq_canA[i]++;
            pos_canA[apos]++;
          }
          /* for every other sequence, add contribution of covariation */
          for(j = i+1; j < msa->nseq; j++) { 
            seqwt2 = (have_weights) ? msa->wgt[j] : 1.0;
            a2 = msa->ax[j][apos+1];
            b2 = msa->ax[j][rpos+1];
            iscanonical2 = _c_bp_is_canonical(a2, b2);
            d = _c_bp_dist(a1, b1, a2, b2);
            if(iscanonical1 && iscanonical2) { 
              contrib = d * (seqwt1 + seqwt2);
            }
            else { 
              contrib = -1 * d * (seqwt1 + seqwt2);
            }
            covA[apos]     += contrib;
            cov_cntA[apos] += (seqwt1 + seqwt2);
          }
        }
      }
    }
  }

  /* calculate mean covariation statistic */
  mean_cov = (nbp == 0) ? 0. : esl_vec_DSum(covA, msa->alen) / esl_vec_DSum(cov_cntA, msa->alen);

  /* divide covA values so their per-basepair-count, we make sure we do this after calc'ing the mean above */
  for(apos = 0; apos < msa->alen; apos++) { 
    if(rposA[apos] != -1) { 
      if(fabs(cov_cntA[apos]) > 1E-10) { /* don't divide by zero */
        covA[apos] /= cov_cntA[apos];
      }
    }
  }

  /* clean up, and return */
  if(cov_cntA) free(cov_cntA);
  if(ret_nbp      != NULL) { *ret_nbp      = nbp;      }
  if(ret_rposA    != NULL) { *ret_rposA    = rposA;    } else { free(rposA); }
  if(ret_seq_canA != NULL) { *ret_seq_canA = seq_canA; } else { free(seq_canA); }
  if(ret_pos_canA != NULL) { *ret_pos_canA = pos_canA; } else { free(pos_canA); }
  if(ret_covA     != NULL) { *ret_covA     = covA;     } else { free(covA); }
  if(ret_mean_cov != NULL) { *ret_mean_cov = mean_cov; } 

  return eslOK;

 ERROR:
  /* clean up, and return */
  if(cov_cntA) free(cov_cntA);
  if(rposA)    free(rposA);
  if(seq_canA) free(seq_canA);
  if(pos_canA) free(pos_canA);
  if(covA)     free(covA);

  croak("out of memory");

  return eslEMEM;
}

/* Function: _c_rfam_pid_stats
 * Incept:   EPN, Tue Jul 16 08:47:20 2013
 * Purpose:  Helper function for _c_rfam_qc_stats().  Determine the
 *           average, maximum and minimum percent identity between
 *           all pairs of sequences.
 *
 *           Note: this is slow for very large alignments, but the
 *           largest seed in Rfam 11.0 is 1020 (glnA) so * it's
 *           probably safe at least for seeds, which is what it's
 *           designed for.
 *
 * Returns:  ret_pid_mean:  mean    pairwise identity between all pairs of seqs 
 *           ret_pid_min:   minimum pairwise identity between all pairs of seqs 
 *           ret_pid_max:   maximum pairwise identity between all pairs of seqs 
 *
 *           eslOK if successful
 */
int
_c_rfam_pid_stats(ESL_MSA *msa, double *ret_pid_mean, double *ret_pid_min, double *ret_pid_max)
{
  int    status;         /* Easel status */
  int    i, j;           /* sequence index counters */
  double pid_mean = 0.;  /* mean    pairwise id between all pairs of seqs */
  double pid_min  = 1.;  /* minimum pairwise id between all pairs of seqs */
  double pid_max  = 0.;  /* maximum pairwise id between all pairs of seqs */
  double pid;            /* current pairwise id */

  for (i = 0; i < msa->nseq; i++) { 
    for (j = i+1; j < msa->nseq; j++) { 
      if(msa->flags & eslMSA_DIGITAL) { 
        if ((status = esl_dst_XPairId(msa->abc, msa->ax[i], msa->ax[j], &pid, NULL, NULL)) != eslOK) return status;
      }
      else { /* text mode */
        if ((status = esl_dst_CPairId(msa->aseq[i], msa->aseq[j], &pid, NULL, NULL)) != eslOK) return status;
      }
      pid_min   = ESL_MIN(pid_min, pid);
      pid_max   = ESL_MAX(pid_max, pid);
      pid_mean += pid;
    }
  }
  pid_mean /= (double) (msa->nseq * (msa->nseq-1) / 2);

  *ret_pid_mean = pid_mean;
  *ret_pid_min  = pid_min;
  *ret_pid_max  = pid_max;

  return eslOK;
}

/* Function:  _c_rfam_qc_stats()
 * Incept:    EPN, Mon Jul 15 09:01:25 2013
 * Purpose:   A very specialized function. Calculate and output
 *            several statistics used for quality-control (qc) for
 *            Rfam seed alignments. Specifically the following stats are
 *            calculated and output
 *
 *            Per-family stats, output to 'fam_outfile':
 *            fractional canonical basepairs
 *            mean 'covariation' per basepair
 *            number seqs
 *            alignment length
 *            number of consensus basepairs
 *            total number of nucleotides (nongaps)
 *            average/max/min pairwise percentage identity 
 *            average/max/min sequence length
 *            fraction of nongaps
 *            fraction of A/C/G/U
 *            most common 'dinucleotide' (two letter IUPAC ambiguity code)
 *            fraction of 'CG'
 *             
 *            Per-sequence stats, output to 'seq_outfile':
 *            fractional canonical basepairs
 *            sequence length (ungapped)
 *            fraction of A/C/G/U
 *            most common 'dinucleotide' (two letter IUPAC ambiguity code)
 *            fraction of 'CG'
 *
 *            Per-basepair stats, output to 'bp_outfile':
 *            fraction canonical basepairs
 *            'covariation' statistic
 *
 * Helper functions do all the dirty work for this function:
 * _c_rfam_comp_and_len_stats(): sequence length and composition stats
 * _c_rfam_bp_stats():           all basepair-related stats
 * _c_rfam_pid_stats():          percent identity stats
 *
 * This function reproduces all functionality in Paul Gardner's
 * rqc-ss-cons.pl script, last used in Rfam 10.0 and deprecated during
 * Sanger->EBI transition code overhaul.
 * 
 * Returns:   eslOK on success.
 */

int _c_rfam_qc_stats(ESL_MSA *msa, char *fam_outfile, char *seq_outfile, char *bp_outfile)
{
  FILE  *ffp;          /* open output per-family   stats output file */
  FILE  *sfp;          /* open output per-sequence stats output file */
  FILE  *bfp;          /* open output per-basepair stats output file */
  int    i;            /* sequence index */
  int    apos;         /* alignment position */
  double seqwt;        /* sequence weight */
  int    have_weights; /* set to '1' if MSA has valid weights, else it does not and we'll use 1.0 as the weight for all sequences */

  /* variables related to seq composition statistics, mainly
   * used by _c_rfam_comp_stats() 
   */
  double   **abcAA    = NULL;  /* [0..i..msa->nseq-1][0..a..abc->K]: count of nt 'a' in sequence 'i', a==abc->K are gaps, missing residues or nonresidues */
  double    *abc_totA = NULL;  /* [0..a..abc->K]: count of nt 'a' in all sequences, a==abc->K are gaps, missing residues or nonresidues */
  int       *lenA     = NULL;  /* [0..i..msa->nseq-1]: nongap length of sequence i */
  int        len_tot;          /* total (summed) sequence length */
  int        len_min;          /* minimum sequence length */
  int        len_max;          /* maximum sequence length */

  /* variables related to sequence pairwise identities, 
   * mainly used by _c_rfam_pid_stats()
   */
  double pid_mean;  /* mean    pairwise id between all pairs of seqs */
  double pid_min;   /* minimum pairwise id between all pairs of seqs */
  double pid_max;   /* maximum pairwise id between all pairs of seqs */

  /* variables related to basepair statistics, mainly used
   * by _c_rfam_bp_stats() helper function.
   */
  int       *rposA    = NULL;  /* [0..apos..msa->alen-1]: right position for basepair with left half position of 'i', else -1 if 'i' is not left half of a pair (i always < j) */
  int       *seq_canA = NULL;  /* [0..i..msa->nseq-1]: number of canonical basepairs in sequence i */
  int       *pos_canA = NULL;  /* [0..apos..msa->alen-1]: number of canonical basepairs with left half position of 'i' */
  double    *covA     = NULL;  /* [0..apos..msa->alen-1]: covariation per basepair */
  int        nbp = 0;          /* number of canonical basepairs in the (possibly deknotted) consensus secondary structure */
  double     mean_cov;         /* mean covariation */  

  /* variables related to the most common 2-letter ambiguity,
   * what Paul called a 'dinucleotide' in rqc-ss-cons.pl */
  char       max_2l;           /* most common 2-letter ambiguity */
  double     max_2l_frac;      /* fraction of residues represented by most common 2-letter ambiguity */

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_rfam_qc_stats() contract violation, MSA is not digitized");
  have_weights = (msa->flags & eslMSA_HASWGTS) ? 1 : 0;

  /* open output files */
  if((ffp = fopen(fam_outfile, "w"))  == NULL) { croak("unable to open %s for writing", fam_outfile); }
  if((sfp = fopen(seq_outfile, "w"))  == NULL) { croak("unable to open %s for writing", seq_outfile); }
  if((bfp = fopen(bp_outfile,  "w"))  == NULL) { croak("unable to open %s for writing", bp_outfile); }

  _c_rfam_comp_and_len_stats(msa, &abcAA, &abc_totA, &lenA, &len_tot, &len_min, &len_max);
  _c_rfam_pid_stats         (msa, &pid_mean, &pid_min, &pid_max);
  _c_rfam_bp_stats          (msa, &nbp, &rposA, &seq_canA, &pos_canA, &covA, &mean_cov);

  /* calc most common 2-letter ambiguity for full alignment */
  _c_max_rna_two_letter_ambiguity(abc_totA[0], abc_totA[1], abc_totA[2], abc_totA[3], &max_2l, &max_2l_frac);

  /* print 'ss-stats-per-family' */
  fprintf(ffp, "%-20s  %25s  %11s  %7s  %10s  %6s  %7s  %8s  %7s  %7s  %8s  %7s  %7s  %11s  %6s  %6s  %6s  %6s  %9s  %10s\n", 
         "FAMILY", "MEAN_FRACTN_CANONICAL_BPs", "COVARIATION", "NO_SEQs", "ALN_LENGTH", "NO_BPs", "NO_NUCs", "mean_PID", "max_PID", "min_PID", "mean_LEN", "max_LEN", "min_LEN", "FRACTN_NUCs", "FRAC_A", "FRAC_C", "FRAC_G", "FRAC_U", "MAX_DINUC", "CG_CONTENT");
  fprintf(ffp, "%-20s  %25.5f  %11.5f  %7d  %10" PRId64 "  %6d  %7d  %8.3f  %7.3f  %7.3f  %8.3f  %7d  %7d  %11.3f  %6.3f  %6.3f  %6.3f  %6.3f  %c:%-7.3f  %10.3f\n", 
          msa->name,                                           /* family name */
          (nbp == 0) ? 0. : ((double) esl_vec_ISum(seq_canA, msa->nseq)) / ((double) msa->nseq * nbp), /* fractional canonical basepairs */
          mean_cov,                                            /* the 'covariation' statistic, mean */
          msa->nseq,                                           /* number of sequences */
          msa->alen,                                           /* alignment length */
          nbp,                                                 /* number of basepairs in (possibly deknotted) consensus secondary structure */
          len_tot,                                             /* total number of non-gap/missing/nonresidues in alignment (non-weighted) */
          pid_mean,                                            /* average pairwise seq identity */
          pid_max,                                             /* max pairwise seq identity */
          pid_min,                                             /* min pairwise seq identity */
          (double) len_tot / msa->nseq,                        /* avg length */
          len_max,                                             /* max sequence length */
          len_min,                                             /* min sequence length */       
          (double) len_tot / ((double) (msa->alen*msa->nseq)), /* fraction nucleotides (nongaps) */
          abc_totA[0] / (double) len_tot,                      /* fraction of As */
          abc_totA[1] / (double) len_tot,                      /* fraction of Cs */
          abc_totA[2] / (double) len_tot,                      /* fraction of Gs */
          abc_totA[3] / (double) len_tot,                      /* fraction of U/Ts */
          max_2l,                                              /* identity of most common two-letter iupac code */
          max_2l_frac,                                         /* fraction of most common two-letter iupac code */
          (abc_totA[1] + abc_totA[2]) / (double) len_tot);     /* CG fraction */
  
  /* print ss-stats-persequence */
  fprintf(sfp, "%-20s  %-30s  %20s  %5s  %6s  %6s  %6s  %6s  %9s  %10s\n", 
          "FAMILY", "SEQID", "FRACTN_CANONICAL_BPs", "LEN", "FRAC_A", "FRAC_C", "FRAC_G", "FRAC_U", "MAX_DINUC", "CG_CONTENT");
  for(i = 0; i < msa->nseq; i++) { 
    seqwt = (have_weights) ? msa->wgt[i] : 1.0;
    /* get most common two-letter iupac ambiguity */
    _c_max_rna_two_letter_ambiguity(abcAA[i][0], abcAA[i][1], abcAA[i][2], abcAA[i][3], &max_2l, &max_2l_frac);
    fprintf(sfp, "%-20s  %-30s  %20.5f  %5d  %6.3f  %6.3f  %6.3f  %6.3f  %c:%-7.3f  %10.3f\n", 
            msa->name,                                         /* family name */
            msa->sqname[i],                                    /* seq name */
            (nbp == 0) ? 0. : (double) seq_canA[i] / (double) nbp, /* fraction of canonical bps */
            lenA[i],                                           /* seq length */
            abcAA[i][0] / (seqwt * lenA[i]),                   /* fraction of As */
            abcAA[i][1] / (seqwt * lenA[i]),                   /* fraction of Cs */
            abcAA[i][2] / (seqwt * lenA[i]),                   /* fraction of Gs */
            abcAA[i][3] / (seqwt * lenA[i]),                   /* fraction of Us */
            max_2l,                                            /* identity of most common dinuc */
            max_2l_frac,                                       /* fraction of most common dinuc */
            (abcAA[i][1] + abcAA[i][2]) / (seqwt * lenA[i]));  /* CG fraction */
  }

  /* print ss-stats-perbasepair */
  fprintf(bfp, "%-20s  %11s  %20s  %11s\n", 
         "FAMILY", "BP_COORDS", "FRACTN_CANONICAL_BPs", "COVARIATION");
  for(apos = 0; apos < msa->alen; apos++) { 
    if(rposA[apos] != -1) { 
      fprintf(bfp, "%-20s  %5d:%-5d  %20.4f  %11.4f\n", 
              msa->name,                                     /* family name */
              (apos+1), (rposA[apos]+1),                     /* left and right position of bp, note off-by-one b/c apos is 0..alen-1 */
              (double) pos_canA[apos] / (double) msa->nseq,  /* fraction of this bp that are canonical */
              covA[apos]);                                   /* 'covariation statistic' for this bp */
    }
  }

  /* close output files */
  fclose(ffp);
  fclose(sfp);
  fclose(bfp);

  /* cleanup and exit */
  if(abcAA) { 
    for(i = 0; i < msa->nseq; i++) { 
      if(abcAA[i]) free(abcAA[i]);
    }
    free(abcAA);
  }
  if(abc_totA) free(abc_totA);
  if(lenA)     free(lenA);
  if(rposA)    free(rposA);
  if(seq_canA) free(seq_canA);
  if(pos_canA) free(pos_canA);
  if(covA)     free(covA);
  
  return eslOK;
}

/* Function: _c_check_reqd_format
 * Incept:   EPN, Thu Jul 18 11:07:44 2013
 * Purpose:  Check if <format> string is a valid format,
 *           croak if it is not.
 *
 * Returns:  void
 */
void
_c_check_reqd_format(char *format)
{
  int fmt; /* int format code */

  fmt = eslx_msafile_EncodeFormat(format);

  if(fmt == eslMSAFILE_UNKNOWN) croak ("required format string %s, is not valid, choose from: \"stockholm\", \"pfam\", \"a2m\", \"phylip\", \"phylips\", \"psiblast\", \"selex\", \"afa\", \"clustal\", \"clustallike\"\n", format);

  return;
}

/* Function: _c_pairwise_identity
 * Incept:   EPN, Wed Aug 21 10:13:35 2013
 * Purpose:  Calculate pairwise identity [0..1] between two aligned
 *           sequences (i and j)
 *
 * Args:     msa: the alignment
 *           i:   idx of first seq 
 *           j:   idx of second seq
 * Returns:  fractional identity between seq i and j
 * Dies:     if msa is not digitized or seq i or j does not exist
 */
double
_c_pairwise_identity(ESL_MSA *msa, int i, int j)
{
  int status; 
  double pid;

  if(i < 0 || i >= msa->nseq)         croak("_c_pairwise_identity() contract violation, idx i (%d) out of bounds (nseq: %d)", i, msa->nseq);
  if(j < 0 || j >= msa->nseq)         croak("_c_pairwise_identity() contract violation, idx j (%d) out of bounds (nseq: %d)", j, msa->nseq);

  if(msa->flags & eslMSA_DIGITAL) { 
    status = esl_dst_XPairId(msa->abc, msa->ax[i], msa->ax[j], &pid, NULL, NULL);
  }
  else { 
    status = esl_dst_CPairId(msa->aseq[i], msa->aseq[j], &pid, NULL, NULL); 
  }
  if(status != eslOK) croak("_c_pairwise_identity() error, aligned seqs different lengths");
  return pid;
}

/* Function: _c_clone_msa
 * Incept:   EPN, Thu Nov 21 09:12:49 2013
 * Purpose:  Duplicates an MSA, and returns the newly created duplicate.
 *
 * Args:     msa:   the input alignment
 *
 * Returns:  upon success: a new msa, a duplicate of the input msa
 *           NULL on error
 */
SV *
_c_clone_msa(ESL_MSA *msa)
{
  ESL_MSA *new_msa = NULL;

  new_msa = esl_msa_Clone(msa);

  if(new_msa == NULL) return NULL;

  return perl_obj(new_msa, "ESL_MSA");
}

/* Function: _c_sequence_subset
 * Incept:   EPN, Thu Nov 14 10:41:06 2013
 * Purpose:  Create a new MSA and return it, with a 
 *           subset of the sequences in <msa>.
 *           Keep only those sequences i for which
 *           usemeAR[i] is TRUE, remove all others.
 *
 * From esl_msa_SequenceSubset() comments:
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 *            The smaller alignment might now contain columns
 *            consisting entirely of gaps or missing data, depending
 *            on what sequence subset was extracted. The caller may
 *            want to immediately call <esl_msa_MinimGaps()> on the
 *            new alignment to clean this up.
 *
 *            Unparsed GS and GR Stockholm annotation that is presumably still
 *            valid is transferred to the new alignment. Unparsed GC, GF, and
 *            comments that are potentially invalidated by taking the subset
 *            of sequences are not transferred to the new MSA.
 *            
 *            Weights are transferred exactly. If they need to be
 *            renormalized to some new total weight (such as the new,
 *            smaller total sequence number), the caller must do that.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 * Args:     msa:   the input alignment
 *           useme: [0..i..msa->nseq-1]: TRUE to keep seq i, FALSE to remove it
 *
 * Returns:  new subset msa upon success
 *           NULL on error
 */
SV *
_c_sequence_subset(ESL_MSA *msa, AV *usemeAR)
{
  int status;              /* status */
  ESL_MSA *new_msa = NULL; /* the new_msa we'll create and return */

  /* create C int array useme */
  int *useme = NULL;
  ESL_ALLOC(useme, sizeof(int) * msa->nseq);

  /* copy the perl array into the C one */
  _c_int_copy_array_perl_to_c(usemeAR, useme, msa->nseq);

  status = esl_msa_SequenceSubset(msa, useme, &new_msa);
  if     (status == eslEINVAL) croak("in _c_sequence_subset(), no sequences in input msa"); 
  else if(status == eslEMEM)   croak("in _c_sequence_subset(), out of memory");
  else if(status != eslOK)     croak("in _c_sequence_subset(), esl_msa_SequenceSubset() had a problem");

  free(useme);

  return perl_obj(new_msa, "ESL_MSA");

 ERROR:
  if(useme) free(useme);
  croak("in _c_sequence_subset(), out of memory");
  return NULL; /* NEVERREACHED */
}

/* Function: _c_remove_all_gap_columns
 * Incept:   EPN, Thu Nov 14 13:44:02 2013
 * Purpose:  Remove columns containing all gap symbols
 *           by calling esl_msa_MinimGaps().
 *
 * From comments in esl_msa_MinimGaps():
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 *            If <consider_rf> is TRUE, only columns that are gaps
 *            in all sequences of <msa> and a gap in the RF annotation 
 *            of the alignment (<msa->rf>) will be removed. It is 
 *            okay if <consider_rf> is TRUE and <msa->rf> is NULL
 *            (no error is thrown), the function will behave as if 
 *            <consider_rf> is FALSE.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 * 
 * Args:     msa:         the input alignment
 *           consider_rf: TRUE to not delete any nongap RF column
 * 
 * Returns:  void
 * Dies:     with croak upon an erro
 *           NULL on error
 */
void
_c_remove_all_gap_columns(ESL_MSA *msa, int consider_rf)
{
  int  status;              /* status */
  char errbuf[eslERRBUFSIZE];

  if (msa->flags & eslMSA_DIGITAL) { /* digital mode, pass in NULL for gap string */
    status = esl_msa_MinimGaps(msa, errbuf, NULL, consider_rf); 
  }
  else { /* text mode */
    status = esl_msa_MinimGapsText(msa, errbuf, "-_.~", consider_rf, 1); /* TRUE says: fix broken basepairs */
  }

  if(status != eslOK) croak ("ERROR, _c_remove_all_gap_columns: %s\n", errbuf);
  
  return;
}

/* Function: _c_column_subset
 * Incept:   EPN, Thu Nov 21 09:01:02 2013
 * Purpose:  Remove columns from an MSA based on
 *           usemeAR. If $usemeAR->[$i] is '1' then
 *           keep column $i, else remove it.
 * 
 *           Real work is done by esl_msa_ColumnSubset().
 *
 * Args:     msa:         the input alignment
 *           useme: [0..apos..msa->alen-1]: TRUE to keep col i, FALSE to remove it
 * 
 * Returns:  void
 * Dies:     with croak upon an erro
 *           NULL on error
 */
void
_c_column_subset(ESL_MSA *msa, AV *usemeAR)
{
  int  status;              /* status */
  char errbuf[eslERRBUFSIZE];
  /* for manipulating the perl usemeAR */

  /* create C int array useme */
  int *useme = NULL;
  ESL_ALLOC(useme, sizeof(int) * msa->alen);

  /* copy the perl array into the C one */
  _c_int_copy_array_perl_to_c(usemeAR, useme, msa->alen);

  /* remove the columns in place */
  status = esl_msa_ColumnSubset(msa, errbuf, useme);
  if(status != eslOK) croak ("ERROR, _c_column_subset: %s\n", errbuf);
  
  return;

 ERROR:
  if(useme) free(useme);
  croak("in _c_column_subset(), out of memory");
  return; /* NEVERREACHED */
}

/* Function: _c_create_from_string
 * Incept:   EPN, Fri Nov 29 08:30:42 2013
 * Purpose:  Create a new ESL_MSA object from an
 *           input string in format <fmt_str>
 *           and return it. If <fmt_str> is 
 *           not recognized we'll try to parse
 *           the alignment as an unknown format.
 *           If <do_digitize> we also digitize 
 *           the alignment, since most of the 
 *           BioEasel MSA code requires a digitized
 *           MSA.
 *
 * Args:     msa_str: the alignment string
 *           fmt_str: the format string
 *           abc_str: string describing alphabet, 
 *                    either "amino", "rna", "dna", "coins", "dice", "custom";
 *                    irrelevant unless 'do_digitize' is TRUE
 *           do_digitize: '1' to digitize ESL_MSA before returning, else do not
 * Returns:  ESL_MSA created here, from <msa_str>
 * Dies:     with croak upon an error
 */

SV *
_c_create_from_string(char *msa_str, char *fmt_str, char *abc_str, int do_digitize)
{
  int  status;
  int  fmt;   
  int  abc_type; 
  ESL_MSA *ret_msa = NULL;
  ESL_ALPHABET *abc = NULL; 
  char errbuf[eslERRBUFSIZE];

  fmt = eslx_msafile_EncodeFormat(fmt_str);

  ret_msa = esl_msa_CreateFromString(msa_str, fmt);
  if(ret_msa == NULL) croak("ERROR, problem creating MSA from string");

  if(do_digitize) { 
    abc_type = esl_abc_EncodeType(abc_str);
    if(abc_type == eslUNKNOWN) croak ("ERROR, unable to create alphabet of type %s", abc_str);

    abc = esl_alphabet_Create(abc_type);
    if(abc == NULL) croak ("ERROR, problem creating alphabet of type code %d", abc_type);
    
    status = esl_msa_Digitize(abc, ret_msa, errbuf);
    if(status != eslOK) croak ("ERROR, digitizing alignment: %s", errbuf);
  }

  return perl_obj(ret_msa, "ESL_MSA");
}

/* Function: _c_is_residue
 * Incept:   EPN, Fri Nov 29 17:11:15 2013
 * Purpose:  Return TRUE if msa->ax[sqidx][apos]
 *           is a residue, else return 0.
 *
 * Args:     sqidx: sequence index
 *           apos:  alignment position [1..alen] (NOT 0..alen-1)
 * 
 * Returns:  TRUE if digitized and msa->ax[sqidx][apos] is a residue, or 
 *                if text      and msa->aseq[sqidx][apos-1] is a alphabetic character, else FALSE
 * Dies:     with croak upon an error
 */

int
_c_is_residue(ESL_MSA *msa, int sqidx, int apos)
{

  if(msa->flags & eslMSA_DIGITAL) { 
    return esl_abc_XIsResidue(msa->abc, msa->ax[sqidx][apos]);
  }
  else { 
    return (isalpha(msa->aseq[sqidx][apos-1])) ? 1 : 0;
  }
}

/* Function: _c_reorder
 * Incept:   EPN, Mon Feb  3 14:43:36 2014
 * Purpose:  Reorder sequences in an MSA by swapping pointers.
 *           Copied and slightly modified from esl-alimanip.c's
 *           reorder_msa().
 *
 * Args:     msa:     the alignment
 *           orderAR: int array specifying new order (orderAR[2] = x ==> x becomes 3rd sequence)
 * 
 * Returns:  void
 * Dies:     with croak upon an error
 */
void
_c_reorder(ESL_MSA *msa, AV *orderAR)
{

  int status;
  char **tmp; 
  int i, a;
  int *order = NULL;
  ESL_ALLOC(tmp, sizeof(char *) * msa->nseq);

  /* create C int array useme */
  ESL_ALLOC(order, sizeof(int) * msa->nseq);
  /* copy the perl array into the C one */
  _c_int_copy_array_perl_to_c(orderAR, order, msa->nseq);

  /* contract check */
  /* 'order' must be have nseq elements, elements must be in range [0..nseq-1], no duplicates  */
  int *covered;
  ESL_ALLOC(covered, sizeof(int) * msa->nseq);
  esl_vec_ISet(covered, msa->nseq, 0);
  for(i = 0; i < msa->nseq; i++) { 
    if(covered[order[i]]) croak("_c_reorder() order array has duplicate entries for i: %d\n", i);
    covered[order[i]] = 1;
  }
  free(covered);

  /* swap aseq or ax (one or the other must be non-NULL) */
  if(msa->flags & eslMSA_DIGITAL) { /* digital MSA */
    ESL_DSQ **tmp_dsq; 
    ESL_ALLOC(tmp_dsq, sizeof(ESL_DSQ *) * msa->nseq);
    for(i = 0; i < msa->nseq; i++) tmp_dsq[i] = msa->ax[i];
    for(i = 0; i < msa->nseq; i++) msa->ax[i] = tmp_dsq[order[i]];
    free(tmp_dsq);
  }
  else { /* text MSA */
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->aseq[i];
    for(i = 0; i < msa->nseq; i++) msa->aseq[i] = tmp[order[i]];
  }

  /* swap sqnames (mandatory) */
  for(i = 0; i < msa->nseq; i++) tmp[i] = msa->sqname[i];
  for(i = 0; i < msa->nseq; i++) msa->sqname[i] = tmp[order[i]];

  /* swap sqacc, if they exist */
  if(msa->sqacc != NULL) { 
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->sqacc[i];
    for(i = 0; i < msa->nseq; i++) msa->sqacc[i] = tmp[order[i]];
  }

  /* swap sqdesc, if they exist */
  if(msa->sqdesc != NULL) { 
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->sqdesc[i];
    for(i = 0; i < msa->nseq; i++) msa->sqdesc[i] = tmp[order[i]];
  }

  /* swap ss, if they exist */
  if(msa->ss != NULL) { 
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->ss[i];
    for(i = 0; i < msa->nseq; i++) msa->ss[i] = tmp[order[i]];
  }

  /* swap sa, if they exist */
  if(msa->sa != NULL) { 
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->sa[i];
    for(i = 0; i < msa->nseq; i++) msa->sa[i] = tmp[order[i]];
  }

  /* swap pp, if they exist */
  if(msa->pp != NULL) { 
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->pp[i];
    for(i = 0; i < msa->nseq; i++) msa->pp[i] = tmp[order[i]];
  }

  /* swap gs annotation, if it exists */
  for(a = 0; a < msa->ngs; a++) {
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->gs[a][i];
    for(i = 0; i < msa->nseq; i++) msa->gs[a][i] = tmp[order[i]];
  }

  /* swap gr annotation, if it exists */
  for(a = 0; a < msa->ngr; a++) {
    for(i = 0; i < msa->nseq; i++) tmp[i] = msa->gr[a][i];
    for(i = 0; i < msa->nseq; i++) msa->gr[a][i] = tmp[order[i]];
  }
  free(tmp);

  free(order);
  return;

 ERROR:
  croak("_c_reorder() out of memory");
}

/* Function:  _c_check_index()
 * Incept:    EPN, Mon Feb  3 15:18:15 2014
 * Synopsis:  Check if an MSA has a valid index and if not, create it.
 * Dies:      If unable to create an index.
 */
void _c_check_index (ESL_MSA *msa)
{
  int status;

  /* create the index if it doesn't exist or it seems incorrect (num keys != num seqs) */
  if(msa->index == NULL || (esl_keyhash_GetNumber(msa->index) != msa->nseq)) { 
    status = esl_msa_Hash(msa);
    if(status == eslEDUP)      { croak ("ERROR, _c_check_index() MSA has duplicated names in it"); }
    else if(status == eslEMEM) { croak ("ERROR, _c_check_index() out of memory"); }
    else if(status != eslOK)   { croak ("ERROR, _c_check_index() unexpected error"); }
  }

  return;
}

/* Function:  _c_capitalize_based_on_rf()
 * Incept:    EPN, Tue Feb 18 10:56:26 2014
 * Synposis:  Set all residues in nongap RF columns as uppercase,
              and all gap characters ('.-_') to '-'. Set all residues
              in gap RF columns to lowercase and all gap characters
              '.-_' to '.'.
 * Dies:      If msa is not in text mode, or does not have RF annotation.
 */
void _c_capitalize_based_on_rf(ESL_MSA *msa)
{
  int i, apos;

  if(msa->rf == NULL)             croak("ERROR, _c_capitalize_based_on_rf() RF annotation does not exist"); 
  if(msa->flags & eslMSA_DIGITAL) croak("ERROR, _c_capitalize_based_on_rf() MSA is not in text mode"); 

  for (apos = 0; apos < msa->alen; apos++) {
    if(strchr("-_.~", msa->rf[apos]) == NULL) { /* nongap RF character */
      for(i = 0; i < msa->nseq; i++) { 
        if(isalpha(msa->aseq[i][apos])) { /* residue: convert to uppercase */
          msa->aseq[i][apos] = toupper(msa->aseq[i][apos]);
        }
        else if(strchr("._", msa->aseq[i][apos]) != NULL) { /* gap: convert to '-' */
          msa->aseq[i][apos] = '-';
        }
      }
    }
    else { /* gap RF character */
      for(i = 0; i < msa->nseq; i++) { 
        if(isalpha(msa->aseq[i][apos])) { /* residue: convert to lowercase */
          msa->aseq[i][apos] = tolower(msa->aseq[i][apos]);
        }
        else if(strchr("-_", msa->aseq[i][apos]) != NULL) { /* gap: convert to '.' */
          msa->aseq[i][apos] = '.';
        }
      }
    }
  }

  return;
}

/* Function:  _c_get_gf_num()
 * Incept:    EPN, Thu May  8 13:44:36 2014
 * Synposis:  Return the number of GF annotations (msa->ngf).
 */
int _c_get_gf_num(ESL_MSA *msa)
{
  return msa->ngf;
}

/* Function:  _c_get_gf_tag()
 * Incept:    EPN, Thu May  8 13:45:23 2014
 * Synposis:  Return the tag for GF annotation num <idx>
 * Dies:      If GF annotation num <idx> does not exist.
 */
char *_c_get_gf_tag(ESL_MSA *msa, int idx)
{
  if(idx >= msa->ngf) croak("ERROR, _c_get_gf_tag(), asking for tag %d, but only %d exist.", idx+1, msa->ngf); 
  return msa->gf_tag[idx];
}

/* Function:  _c_get_gf()
 * Incept:    EPN, Thu May  8 13:45:23 2014
 * Synposis:  Return GF annotation num <idx>
 * Dies:      If GF annotation num <idx> does not exist.
 */
char *_c_get_gf(ESL_MSA *msa, int idx)
{
  if(idx >= msa->ngf) croak("ERROR, _c_get_gf(), asking for GF annotation %d, but only %d exist.", idx+1, msa->ngf); 
  return msa->gf[idx];
}

/* Function:  _c_check_ppidx()
 * Incept:    EPN, Mon Jul  7 09:15:48 2014
 * Synposis:  Return '1' if sequence <idx> has PP annotation in the MSA, else return '0';
 */
int _c_check_ppidx(ESL_MSA *msa, int idx)
{
  if(msa->pp      == NULL)        return 0;
  if(idx < 0 || idx >= msa->nseq) return 0;
  if(msa->pp[idx] == NULL)        return 0;
  return 1;
}

/* Function:  _c_most_informative_sequence()
 * Incept:    EPN, Thu May 15 13:21:13 2014
 * Synposis:  Calculate and return the 'most informative sequence'
 *            (Freyhult, Moulton and Gardner, 2005)
 *            'Any residue that has a higher frequency than than the background frequency is projected
 *            into the IUPAC redundancy codes.' The purpose
 *            is to match the sequence created on the Rfam
 *            pre-12.0 website under the 'secondary structure'
 *            tab. Length of the returned sequence is msa->alen.
 *            (If you only want nongap RF positions, remove all
 *             gap RF columns with column_subset() first.)
 * Args:      msa: the alignment
 *            gapthresh: only positions with >= gapthresh nongaps will become a nongap residue in the MIS
 *            use_weights: '1' to use weights, '0' not to
 * Returns:   the 'most informative sequence' calculated here.
 * Dies:      if MSA is NOT digitized, or if use_weights is '1' and weights are invalid
 */
char *_c_most_informative_sequence(ESL_MSA *msa, float gapthresh, int use_weights)
{
  int        status;           /* Easel status */
  int        i;                /* counter over sequences */
  int        a, a2;            /* counters over alphabet indices (residues) */
  int        apos;             /* alignment position counter [0..alen-1] and [0..rflen-1], respectively */
  double   **abcAA    = NULL;  /* [0..apos..msa->alen-1][0..a..abc->K]: count of nt 'a' in column 'apos', a==abc->K are gaps, missing residues or nonresidues */
  double    *abc_totA = NULL;  /* [0..a..abc->K]: count of nt 'a' in all sequences, a==abc->K are gaps, missing residues or nonresidues */
  double     sum;              /* sum of a vector, used in several contexts */
  char      *mis = NULL;       /* the most informative sequence, allocated below */
  int       *above_bgA = NULL; /* [0..a..abc->K-1] above_bg[a] is '1' if freq of nt 'a' is above background in current column */
  char       gapchar = '-';    /* gap character */
  int        amatch;           /* degenerate residue index that matches current column */
  int        found_mismatch;   /* set to '1' when we found a mismatch for current column and candidate degenerate residue */
  float      seqwt = 0.;       /* weight of current sequence, always 1.0 if use_weights == FALSE */
  float      tol = 0.0001;     /* tolerance, amount of leeway we allow for defining if a nt in a column is above background, 
                                * we add this to the observed frequency. This reproduces how Paul Gardner implemented the MSI 
                                * calculation originally for Rfam 11.0 and earlier, possibly because it guarantees at least one nt is 
                                * always above background.
                                */

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_most_informative_sequence() contract violation, MSA is not digitized");
  if((! (msa->flags & eslMSA_HASWGTS)) && (use_weights)) croak("_c_most_informative_sequence() trying to use weights, but they're not valid in the msa");
    
  /* allocate and initialize */
  ESL_ALLOC(abcAA,       sizeof(double *)  * msa->alen); 
  ESL_ALLOC(abc_totA,    sizeof(double) * (msa->abc->K+1)); 
  esl_vec_DSet(abc_totA, msa->abc->K+1, 0.);

  for(apos = 0; apos < msa->alen; apos++) { 
    ESL_ALLOC(abcAA[apos], sizeof(double) * (msa->abc->K+1));
    esl_vec_DSet(abcAA[apos], (msa->abc->K+1), 0.);
  }
  ESL_ALLOC(mis, sizeof(char) * (msa->alen+1)); 
  mis[msa->alen] = '\0';
  ESL_ALLOC(above_bgA, sizeof(int) * msa->abc->K);
  
  /* compile counts */
  for(i = 0; i < msa->nseq; i++) { 
    seqwt = (use_weights) ? msa->wgt[i] : 1.0;
    for(apos = 0; apos < msa->alen; apos++) { 
      if((status = esl_abc_DCount(msa->abc, abcAA[apos], msa->ax[i][apos+1], seqwt)) != eslOK) croak("problem counting residue %d of seq %d", apos, i);
      esl_vec_DAdd(abc_totA, abcAA[apos], msa->abc->K+1); /* add this seqs count to the abc_totA array */
    }
  }

  /* normalize the nongap chars in the abc_totA vector */
  esl_vec_DNorm(abc_totA, msa->abc->K); /* only normalize the first K values (omit gaps) */

  /* determine the most informative (possibly degenerate) residue at each position */
  for(apos = 0; apos < msa->alen; apos++) { 
    esl_vec_ISet(above_bgA, msa->abc->K, 0);  /* initialize above_bgA */
    sum = esl_vec_DSum(abcAA[apos], msa->abc->K); /* only sum first K values (omit nonresidues) */
    amatch = -1; /* this is set to a valid a [0..msa->abc->Kp-3] when we find it, and acts as a flag if we don't find one (which is an error) */
    /* fprintf(stderr, "apos: %4d sum/nseq: %.3f gapthresh: %.3f\n", apos, sum/msa->nseq, gapthresh); */
    if((sum / msa->nseq) < gapthresh) { /* most seqs are gaps at this posn, set mis residue as a gap */
      amatch = msa->abc->K; /* gap character */
    }
    else { /* not mostly gaps, calculate most informative residue */
      esl_vec_ISet(above_bgA, msa->abc->K, 0);
      for(a = 0; a < msa->abc->K; a++) { 
        abcAA[apos][a] = (abcAA[apos][a] / sum) + tol; /* normalize, then add  a 'tolerance' of 0.0001 (which guarantees at least one nt is above bg) */
        /*fprintf(stderr, "apos: %3d a: %d freq: %.3f bg: %.3f\n", apos, a, abcAA[apos][a], abc_totA[a]);*/
        if(abcAA[apos][a] > abc_totA[a]) above_bgA[a] = 1;
      }
      for(a = 0; a <= msa->abc->Kp-3; a++) { /* for each nucleotide, including degenerate ones */
        found_mismatch = 0;
        for(a2 = 0; a2 < msa->abc->K; a2++) {
          if(msa->abc->degen[a][a2] != above_bgA[a2]) { /* a non-match */
            found_mismatch = 1;
          }
        }
        if(! found_mismatch) { 
          amatch = a;
          a = msa->abc->Kp; /* breaks us out of 'for(a = 0)' loop */
        }
      }
    }
    if(amatch == -1) { croak("unable to find a matching degenerate residue for position %d\n", apos+1); }
    mis[apos] = msa->abc->sym[amatch];
  }

  /* clean up and return */
  if(abcAA) {
    for(apos = 0; apos < msa->alen; apos++) { 
      if(abcAA[apos]) { 
        free(abcAA[apos]);
      }
    }
  }
  free(abcAA);
  if(above_bgA) free(above_bgA);
  if(abc_totA) free(abc_totA);

  return mis;

 ERROR:
  if(abcAA) {
    for(apos = 0; apos < msa->alen; apos++) { 
      if(abcAA[apos]) { 
        free(abcAA[apos]);
      }
    }
  }
  free(abcAA);
  if(above_bgA) free(above_bgA);
  if(abc_totA) free(abc_totA);
  croak("out of memory in _c_most_informative_sequence()");
  return NULL; /* not reached */
}

/* Function:  _c_map_rfpos_to_apos()
 * Incept:    EPN, Mon May 19 11:00:49 2014
 * Synopsis:  Given an MSA, determine the alignment position of each nongap RF position
 *            and return it. The abc is only used to define gap chars.
 *            Stolen and slightly modified from esl-alimanip.c [EPN, Mon May 19 11:02:20 2014]
 * Args:      msa:          the alignment
 *            abc:          the alphabet, used only to define gaps
 *            ret_i_am_rf:  RETURN: [0..apos..msa->alen-1]: '1' if apos is a nongap RF char, else '0'
 *            ret_rf2a_map: RETURN: [0..rfpos..rflen-1]:    'x': rf position 'rfpos' maps to aln posn 'x'
 *            ret_a2rf_map: RETURN: [0..apos..alen-1]:      'y': ali position 'apos' maps to nongap RF posn 'y', -1 if is not a nongap RF posn
 *            ret_rflen:    RETURN: nongap RF length, and size of ret_rf2a_map
 * Returns:   an ESL_MSA and a string describing it's format
 */
int _c_map_rfpos_to_apos(ESL_MSA *msa, ESL_ALPHABET *abc, int **ret_i_am_rf, int **ret_rf2a_map, int **ret_a2rf_map, int *ret_rflen)
{
  int status;
  int rflen = 0;
  int *rf2a_map = NULL;
  int *a2rf_map = NULL;
  int *i_am_rf = NULL;
  int rfpos = 0;
  int apos = 0;

  /* contract check */
  if(msa->rf == NULL) croak("_c_map_rfpos_to_apos(), trying to map RF positions to alignment positions, but msa->rf is NULL.");
  ESL_ALLOC(a2rf_map, sizeof(int) * (msa->alen));
  esl_vec_ISet(a2rf_map, msa->alen, -1); /* init to -1 */


  /* count non-gap RF columns */
  for(apos = 0; apos < msa->alen; apos++) { 
    if((! esl_abc_CIsGap(abc, msa->rf[apos])) && 
       (! esl_abc_CIsMissing(abc, msa->rf[apos])) && 
       (! esl_abc_CIsNonresidue(abc, msa->rf[apos])))
      { 
	rflen++;
	/* I don't use esl_abc_CIsResidue() b/c that would return FALSE for 'x' with RNA and DNA */
      }
  }
  /* build map */
  ESL_ALLOC(i_am_rf, sizeof(int) * msa->alen);
  ESL_ALLOC(rf2a_map, sizeof(int) * rflen);
  for(apos = 0; apos < msa->alen; apos++) {
    if((! esl_abc_CIsGap(abc, msa->rf[apos])) && 
       (! esl_abc_CIsMissing(abc, msa->rf[apos])) && 
       (! esl_abc_CIsNonresidue(abc, msa->rf[apos]))) { 
      i_am_rf[apos] = TRUE;
      rf2a_map[rfpos] = apos;
      a2rf_map[apos]  = rfpos;
      rfpos++;
    }
    else { 
      i_am_rf[apos] = FALSE;
    }
  }
  if(ret_i_am_rf != NULL)  { *ret_i_am_rf  = i_am_rf; }
  else                     { free(i_am_rf); }
  if(ret_rf2a_map != NULL) { *ret_rf2a_map = rf2a_map; }
  else                     { free(rf2a_map); };
  if(ret_a2rf_map != NULL) { *ret_a2rf_map = a2rf_map; }
  else                     { free(a2rf_map); };
  if(ret_rflen != NULL)    { *ret_rflen = rflen; }
  return eslOK;

 ERROR:
  if(i_am_rf  != NULL) free(i_am_rf);
  if(rf2a_map != NULL) free(rf2a_map);
  if(a2rf_map != NULL) free(a2rf_map);
  croak("_c_map_rfpos_to_apos(), out of memory");
  return eslEMEM; /* NEVER REACHED */
}

/* Function:  _c_pos_fcbp()
 * Incept:    EPN, Mon May 19 13:39:53 2014
 * Synopsis:  Calculate and return fraction of canonical basepairs at each alignment position.
 * Args:      msa: the alignment
 * Returns:   the fraction of canonical basepairs at each aln position (as an array in Perl's return stack) 
 */
void _c_pos_fcbp(ESL_MSA *msa)
{
  Inline_Stack_Vars;

  int     status;           /* error status */
  int     apos;             /* counter over alignment positions */
  int    *rposA    = NULL;  /* [0..apos..msa->alen-1]: right position for basepair with left half position of 'i', else -1 if 'i' is not left half of a pair (i always < j) */
  int    *pos_canA = NULL;  /* [0..apos..msa->alen-1]: number of canonical basepairs with left half position of 'i' */
  double *fcbpA    = NULL;  /* [0..apos..msa->alen-1]: fraction of canonical basepairs at alignment position apos */

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_pos_fcbp() contract violation, MSA is not digitized");

  /* calculate fraction of canonicals per alignment position */
  _c_rfam_bp_stats (msa, NULL, &rposA, NULL, &pos_canA, NULL, NULL);

  /* fill fcbpA with fraction of canoncial bps per position */
  ESL_ALLOC(fcbpA, sizeof(double) * msa->alen); 
  esl_vec_DSet(fcbpA, msa->alen, 0.); /* init to 0. */
  for(apos = 0; apos < msa->alen; apos++) { 
    if(rposA[apos] != -1) { 
     fcbpA[apos]        = (double) pos_canA[apos] / (double) msa->nseq;
     fcbpA[rposA[apos]] = (double) pos_canA[apos] / (double) msa->nseq;
    }
  }

  /* fill Perl's return stack */
  Inline_Stack_Reset;
  for(apos = 0; apos < msa->alen; apos++) { 
    /* fprintf(stderr, "C: apos: %3d fcbp: %.3f\n", apos, fcbpA[apos]); */
    Inline_Stack_Push(newSVnv(fcbpA[apos])); 
    /* is the following line the proper way? both this and the above line seem to work fine...: 
     * Inline_Stack_Push(sv_2mortal(newSVnv(fcbpA[apos]))); 
     */
  }
  Inline_Stack_Done;
  Inline_Stack_Return(msa->alen);
  
  /* clean up and return */
  if(fcbpA)    free(fcbpA);
  if(rposA)    free(rposA);
  if(pos_canA) free(pos_canA);
  return;

 ERROR:
  if(fcbpA)    free(fcbpA);
  if(rposA)    free(rposA);
  if(pos_canA) free(pos_canA);
  croak("ERROR: _c_pos_fcbp(), out of memory");
  return;
}

/* Function:  _c_pos_covariation()
 * Incept:    EPN, Tue May 20 09:26:03 2014
 * Synopsis:  Calculate and return the covariation statistic (Lindgreen, Gardner, Krogh, 2006) at each alignment position.
 * Args:      msa: the alignment
 * Returns:   the covariation statistics at each aln position (as an array in Perl's return stack) 
 */
void _c_pos_covariation(ESL_MSA *msa)
{
  Inline_Stack_Vars;

  int     apos;             /* counter over alignment positions */
  int    *rposA   = NULL;   /* [0..apos..msa->alen-1]: right position for basepair with left half position of 'i', else -1 if 'i' is not left half of a pair (i always < j) */
  double *covA    = NULL;   /* [0..apos..msa->alen-1]: covariation per basepair */

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_pos_covariation() contract violation, MSA is not digitized");

  /* calculate covariation statistic */
  _c_rfam_bp_stats (msa, NULL, &rposA, NULL, NULL, &covA, NULL);
  
  /* _c_rfam_bp_stats() only sets covA to a non-zero value for the left half posns of basepairs, 
   * copy those values to the corresponding right half posns, and while you're at it, fill the
   * return array. 
   */
  Inline_Stack_Reset;
  for(apos = 0; apos < msa->alen; apos++) { 
    if(rposA[apos] != -1) { 
      covA[rposA[apos]] = covA[apos]; /* set covA value for right position equal to left position */
    }
    Inline_Stack_Push(newSVnv(covA[apos])); 
  }
  Inline_Stack_Done;
  Inline_Stack_Return(msa->alen);
  
  /* clean up and return */
  if(rposA) free(rposA);
  if(covA)  free(covA);
  return;

 ERROR:
  if(rposA) free(rposA);
  if(covA)  free(covA);
  croak("ERROR: _c_pos_covariation(), out of memory");
  return;
}

/* Function:  _c_pos_entropy()
 * Incept:    EPN, Tue May 20 10:45:41 2014
 * Synopsis:  Calculate and return the entropy at each alignment position.
 * Args:      msa: the alignment
 * Returns:   the entropy at each aln position (as an array in Perl's return stack) 
 */
void _c_pos_entropy(ESL_MSA *msa, int use_weights)
{
  Inline_Stack_Vars;

  int        status;           /* error status */
  int        apos;             /* counter over alignment positions */
  int        i;                /* counter over sequences */
  int        a;                /* counter over residues in an alphabet */
  float      seqwt = 0.;       /* weight of current sequence, always 1.0 if use_weights == FALSE */
  double   **abcAA    = NULL;  /* [0..apos..msa->alen-1][0..a..abc->K]: count of nt 'a' in column 'apos', a==abc->K are gaps, missing residues or nonresidues */
  double    *entA     = NULL;  /* [0..apos..msa->alen-1] entropy of column apos */

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_pos_entropy() contract violation, MSA is not digitized");
  if((! (msa->flags & eslMSA_HASWGTS)) && (use_weights)) croak("_c_pos_entropy() trying to use weights, but they're not valid in the msa");

  /* entropy (eqn 11.8, BSA book; Durbin, Eddy, Krogh, Mitchison 1998) 
   * - sum_i P(x_i) log P(x_i)
   * where i ranges over each residue in the alphabet (e.g. nucleotides) 
   */

  /* allocate and initialize */
  ESL_ALLOC(abcAA, sizeof(double *)  * msa->alen); 
  for(apos = 0; apos < msa->alen; apos++) { 
    ESL_ALLOC(abcAA[apos], sizeof(double) * (msa->abc->K+1));
    esl_vec_DSet(abcAA[apos], (msa->abc->K+1), 0.);
  }
  ESL_ALLOC(entA, sizeof(double) * msa->alen);
  esl_vec_DSet(entA, msa->alen, 0.);

  /* compile counts */
  for(i = 0; i < msa->nseq; i++) { 
    seqwt = (use_weights) ? msa->wgt[i] : 1.0;
    for(apos = 0; apos < msa->alen; apos++) { 
      if((status = esl_abc_DCount(msa->abc, abcAA[apos], msa->ax[i][apos+1], seqwt)) != eslOK) croak("problem counting residue %d of seq %d", apos, i);
    }
  }

  /* calculate entropy, and fill return array */
  Inline_Stack_Reset;
  for(apos = 0; apos < msa->alen; apos++) { 
    esl_vec_DNorm(abcAA[apos], msa->abc->K); /* note: only normalize the first msa->abc->K values, this will ignore gaps and normalize nongap counts */
    for(a = 0; a < msa->abc->K; a++) { 
      if(abcAA[apos][a] > eslSMALLX1) { /* above zero */
        entA[apos] += abcAA[apos][a] * (log(abcAA[apos][a]) / log(2)); /* convert natural log to log base 2 */
        /*fprintf(stderr, "apos %3d a %3d added %5.3f to entA[apos], new value: %5.3f\n", apos, a, abcAA[apos][a] * (log(abcAA[apos][a]) / log(2)), entA[apos]);*/
      }
    }
    entA[apos] *= -1.; /* convert to a negative number */
    /*fprintf(stderr, "entA[%2d]: %.3f\n", apos, entA[apos]);*/
    Inline_Stack_Push(newSVnv(entA[apos])); 
  }
  Inline_Stack_Done;
  Inline_Stack_Return(msa->alen);

  /* clean up and return */
  if(abcAA) { 
    for(i = 0; i < msa->nseq; i++) { if(abcAA[i]) free(abcAA[i]); }
    free(abcAA);
  }
  if(entA)  free(entA);
  return;

 ERROR:
  if(abcAA) { 
    for(i = 0; i < msa->nseq; i++) { if(abcAA[i]) free(abcAA[i]); }
    free(abcAA);
  }
  if(entA)  free(entA);
  croak("ERROR: _c_pos_entropy(), out of memory");
  return;
}


/* Function:  _c_pos_conservation()
 * Incept:    EPN, Tue May 20 15:04:29 2014
 * Synopsis:  Calculate and return the 'sequence conservation' at each alignment position.
 *            'sequence conservation' of a position is the maximum frequency of any
 *            residue in a column, where frequency is number of occurences divided by
 *            number of sequences (so no column with >=1 gap can have a conservation of 1.0).
 *            And all gap columns have a conservation of 0.0.
 * Args:      msa: the alignment
 * Returns:   the sequence conservation at each aln position (as an array in Perl's return stack) 
 */
void _c_pos_conservation(ESL_MSA *msa, int use_weights)
{
  Inline_Stack_Vars;

  int        status;           /* error status */
  int        apos;             /* counter over alignment positions */
  int        i;                /* counter over sequences */
  int        a;                /* counter over residues in an alphabet */
  float      seqwt = 0.;       /* weight of current sequence, always 1.0 if use_weights == FALSE */
  double   **abcAA    = NULL;  /* [0..apos..msa->alen-1][0..a..abc->K]: count of nt 'a' in column 'apos', a==abc->K are gaps, missing residues or nonresidues */
  double    *consA    = NULL;  /* [0..apos..msa->alen-1] sequence conservation of column apos */

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_pos_conservation() contract violation, MSA is not digitized");
  if((! (msa->flags & eslMSA_HASWGTS)) && (use_weights)) croak("_c_pos_conservation() trying to use weights, but they're not valid in the msa");

  /* allocate and initialize */
  ESL_ALLOC(abcAA, sizeof(double *)  * msa->alen); 
  for(apos = 0; apos < msa->alen; apos++) { 
    ESL_ALLOC(abcAA[apos], sizeof(double) * (msa->abc->K+1));
    esl_vec_DSet(abcAA[apos], (msa->abc->K+1), 0.);
  }
  ESL_ALLOC(consA, sizeof(double) * msa->alen);
  esl_vec_DSet(consA, msa->alen, 0.);

  /* compile counts */
  for(i = 0; i < msa->nseq; i++) { 
    seqwt = (use_weights) ? msa->wgt[i] : 1.0;
    for(apos = 0; apos < msa->alen; apos++) { 
      if((status = esl_abc_DCount(msa->abc, abcAA[apos], msa->ax[i][apos+1], seqwt)) != eslOK) croak("problem counting residue %d of seq %d", apos, i);
    }
  }

  /* calculate sequence conservation, and fill return array */
  Inline_Stack_Reset;
  for(apos = 0; apos < msa->alen; apos++) { 
    esl_vec_DNorm(abcAA[apos], msa->abc->K+1); /* note: normalize all msa->abc->K+1 values (including gaps) this differs from what we did in _c_pos_entropy() */
    for(a = 0; a < msa->abc->K; a++) { /* look at all non-gaps for most common one */
      if(abcAA[apos][a] > consA[apos]) { 
        consA[apos] = abcAA[apos][a];
      }
    }
    /*fprintf(stderr, "consA[%2d]: %.3f\n", apos, consA[apos]);*/
    Inline_Stack_Push(newSVnv(consA[apos])); 
  }
  Inline_Stack_Done;
  Inline_Stack_Return(msa->alen);

  /* clean up and return */
  if(abcAA) { 
    for(i = 0; i < msa->nseq; i++) { if(abcAA[i]) free(abcAA[i]); }
    free(abcAA);
  }
  if(consA)  free(consA);
  return;

 ERROR:
  if(abcAA) { 
    for(i = 0; i < msa->nseq; i++) { if(abcAA[i]) free(abcAA[i]); }
    free(abcAA);
  }
  if(consA)  free(consA);
  croak("ERROR: _c_pos_conservation(), out of memory");
  return;
}
    
/* Function:  _c_remove_gap_rf_basepairs()
 * Incept:    EPN, Fri Jan 29 16:27:32 2016
 * Synposis:  Remove any basepair (i,j) in msa->ss_cons for which 
 *            one or both of i and j are gaps in msa->rf.
 *            Works with pseudoknots. Convert SS_cons to 
 *            full WUSS format if <do_wussify> is 1
 * Dies:      If msa does not have RF or SS_cons annotation or msa->ss_cons is not consistent.
 */
void _c_remove_gap_rf_basepairs(ESL_MSA *msa, int do_wussify)
{
  int   status;
  int   apos, opos;               /* alignment positions */
  int  *i_am_rf          = NULL;  /* [1..msa->alen] i_am_rf[apos] = 1 if position apos is not a gap, else it's 0 */
  int  *msa_ct           = NULL;  /* [1..msa->alen] msa_ct[apos] = x; x==0 if apos is unpaired, x==opos if apos is paired to opos in msa->ss_cons */

  if(msa->rf      == NULL) croak("ERROR, _c_remove_gap_rf_basepairs() RF annotation does not exist"); 
  if(msa->ss_cons == NULL) croak("ERROR, _c_remove_gap_rf_basepairs() SS_cons annotation does not exist"); 

  ESL_ALLOC(i_am_rf, sizeof(int) * (msa->alen+1));
  esl_vec_ISet(i_am_rf, (msa->alen+1), 0);

  /* allocated msa_ct */
  ESL_ALLOC(msa_ct, sizeof(int) * (msa->alen+1));

  for (apos = 0; apos < msa->alen; apos++) {
    i_am_rf[(apos+1)] = (strchr("-_.~", msa->rf[apos]) == NULL) ? 1 : 0;
  }

  /* get CT array that describes all basepairs (including pknots) in the ss_cons (ct array is 1..alen, not 0..alen-1 */
  status = esl_wuss2ct(msa->ss_cons, msa->alen, msa_ct);
  if(status == eslESYNTAX) croak("in _c_remove_gap_rf_basepairs msa->ss_cons is not valid (structure not consistent)");
  if(status == eslEMEM)    croak("Out of memory");
  for(apos = 1; apos <= msa->alen; apos++) { 
    if(msa_ct[apos] != 0) { 
      opos = msa_ct[apos];
      if((! i_am_rf[apos]) || (! i_am_rf[opos])) { 
        /* one or both apos and opos are gaps in RF, remove this pair */
        msa->ss_cons[apos-1] = '.';
        msa->ss_cons[opos-1] = '.';
      }
    }
  }
  
  if(do_wussify) { 
    esl_wuss_full(msa->ss_cons, msa->ss_cons);
    /* and for each gap RF column, make it a '.' in the SS_cons */
    for(apos = 1; apos <= msa->alen; apos++) { 
      if(! i_am_rf[apos]) { 
        msa->ss_cons[(apos-1)] = '.';
      }
    }
  }

  if(i_am_rf != NULL) free(i_am_rf); 
  if(msa_ct  != NULL) free(msa_ct);
  return;

 ERROR:
  if(i_am_rf != NULL) free(i_am_rf); 
  if(msa_ct  != NULL) free(msa_ct);
  croak("Out of memory");

  return;
}
    
