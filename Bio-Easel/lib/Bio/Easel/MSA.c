#include "easel.h"
#include "esl_alphabet.h"
#include "esl_distance.h"
#include "esl_msa.h"
#include "esl_msafile.h"
#include "esl_sq.h"
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

/* Function:  _c_read_msa()
 * Incept:    EPN, Sat Feb  2 14:14:20 2013
 * Synopsis:  Open a alignment file, read an msa, and close the file.
 * Returns:   an ESL_MSA and a string describing it's format
 */

void _c_read_msa (char *infile, char *reqdFormat)
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

  /* open input file */
  if ((status = eslx_msafile_Open(&abc, infile, NULL, fmt, NULL, &afp)) != eslOK)
    croak("Error reading alignment file %s: %s\n", infile, afp->errmsg);
  
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
 * Returns:   eslOK on success; eslEINVAL if format is invalid
 */
int _c_write_msa (ESL_MSA *msa, char *outfile, char *format) 
{
  FILE  *ofp; /* open output alignment file */
  int   fmt; /* alignment output format */       
  
  if((ofp  = fopen(outfile, "w"))  == NULL) { 
    return eslFAIL;
  }
  if((fmt = eslx_msafile_EncodeFormat(format)) == eslMSAFILE_UNKNOWN) { 
    return eslEINVAL;
  }
  eslx_msafile_Write(ofp, msa, fmt);
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

/* Function:  _c_get_ss_cons()
 * Incept:    EPN, Fri May 24 09:58:32 2013
 * Synopsis:  Returns msa->ss_cons if non-NULL, else dies.
 *            Caller should have used _c_has_ss_cons to verify it exists.
 * Returns:   msa->ss_cons()
 */
char *_c_get_ss_cons (ESL_MSA *msa)
{
  if(msa->ss_cons == NULL) esl_fatal("c_get_ss_cons, but SS_cons is NULL");
  return msa->ss_cons;
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

/* Function:  _c_get_sqwgt()
 * Incept:    EPN, Fri May 24 10:48:17 2013
 * Synopsis:  Returns msa->sqwgt[idx]
 * Returns:   msa->sqwft[idx]
 */
double _c_get_sqwgt (ESL_MSA *msa, I32 idx)
{
    return msa->wgt[idx];
}

/* Function:  _c_any_allgap_columns()
 * Incept:    EPN, Sat Feb  2 14:38:18 2013
 * Synopsis:  Checks for any all gap columns.
 * Returns:   TRUE if any all gap columns exist, else FALSE.
 */
int _c_any_allgap_columns (ESL_MSA *msa) 
{
  int apos, idx; 
  
  for (apos = 1; apos <= msa->alen; apos++) {
    for (idx = 0; idx < msa->nseq; idx++) {
      if (! esl_abc_XIsGap(msa->abc, msa->ax[idx][apos]) &&
	  ! esl_abc_XIsMissing(msa->abc, msa->ax[idx][apos])) { 
	break;
      }
    }
    if(idx == msa->nseq) { 
      return TRUE; 
    }
  }
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
  
  esl_dst_XAverageId(msa->abc, msa->ax, msa->nseq, (max_nseq * max_nseq), &avgid);
  
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
  if((status = esl_abc_Textize(msa->abc, msa->ax[seqidx], msa->alen, seqstring)) != eslOK) croak("failed to textize digitized aligned sequence");

  seqstringSV = newSVpv(seqstring, msa->alen);
  free(seqstring);

  return seqstringSV;

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
  /* convert digital mode to text mode */
  if(sq->dsq == NULL) croak("fetched seq %d from msa, and it's unexpectedly NOT digitized", seqidx);
  if((status = esl_sq_Textize(sq)) != eslOK) croak("failed to textize fetched seq from MSA");
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
  return (int) esl_abc_dsqrlen(msa->abc, msa->ax[seqidx]);
}

/* Function:  _c_average_sqlen()
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
  
  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_rfam_comp_stats() contract violation, MSA is not digitized");

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
    seqwt = msa->wgt[i];
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
  return eslEMEM; /* NEVERREACHED */
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
 *           ret_mean_cov:  total covariation sum of ret_covA, divided by 'tau' (see code)
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
  int a, b;              /* indices */
  int a1, b1;            /* int index of left, right half of basepair 1 */
  int a2, b2;            /* int index of left, right half of basepair 1 */
  int d;                 /* distance between a1:b1 and a2:b2 (number of differences) */
  int iscanonical1;      /* is a1:b1 a canonical pair? (by Paul's definition, see _c_bp_is_canonical() */
  int iscanonical2;      /* is a2:b2 a canonical pair? (by Paul's definition, see _c_bp_is_canonical() */
  double contrib = 0.;   /* contribution of current a1:b1 compared to a2:b2 */
  double mean_cov = 0.;  /* mean covariation statistic */

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
    seqwt1 = msa->wgt[i];
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
            seqwt2 = msa->wgt[j];
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
  mean_cov = esl_vec_DSum(covA, msa->alen) / esl_vec_DSum(cov_cntA, msa->alen);

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

  *ret_nbp      = nbp;
  *ret_rposA    = rposA;
  *ret_seq_canA = seq_canA;
  *ret_pos_canA = pos_canA;
  *ret_covA     = covA;
  *ret_mean_cov = mean_cov;

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

  if(! (msa->flags & eslMSA_DIGITAL)) croak("_c_rfam_pid_stats() contract violation, MSA is not digitized");

  for (i = 0; i < msa->nseq; i++) { 
    for (j = i+1; j < msa->nseq; j++) { 
      if ((status = esl_dst_XPairId(msa->abc, msa->ax[i], msa->ax[j], &pid, NULL, NULL)) != eslOK) return status;
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
  int status;
  FILE  *ffp;   /* open output per-family   stats output file */
  FILE  *sfp;   /* open output per-sequence stats output file */
  FILE  *bfp;   /* open output per-basepair stats output file */
  int i;        /* sequence index */
  int apos;     /* alignment position */
  double seqwt; /* sequence weight */

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
  fprintf(ffp, "%-20s  %25.5f  %11.5f  %7d  %10lld  %6d  %7d  %8.3f  %7.3f  %7.3f  %8.3f  %7d  %7d  %11.3f  %6.3f  %6.3f  %6.3f  %6.3f  %c:%-7.3f  %10.3f\n", 
         msa->name,                                           /* family name */
         ((double) esl_vec_ISum(seq_canA, msa->nseq)) / ((double) msa->nseq * nbp), /* fractional canonical basepairs */
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
    seqwt = msa->wgt[i];
    /* get most common two-letter iupac ambiguity */
    _c_max_rna_two_letter_ambiguity(abcAA[i][0], abcAA[i][1], abcAA[i][2], abcAA[i][3], &max_2l, &max_2l_frac);
    fprintf(sfp, "%-20s  %-30s  %20.5f  %5d  %6.3f  %6.3f  %6.3f  %6.3f  %c:%-7.3f  %10.3f\n", 
           msa->name,                                         /* family name */
           msa->sqname[i],                                    /* seq name */
           (double) seq_canA[i] / (double) nbp,               /* fraction of canonical bps */
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
