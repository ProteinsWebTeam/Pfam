#include "easel.h"
#include "esl_distance.h"
#include "esl_msa.h"
#include "esl_msafile.h"
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
 * Returns:   an ESL_MSA
 */

SV *_c_read_msa (char *infile)
{
    int           status;     /* Easel status code */
    ESLX_MSAFILE *afp;        /* open input alignment file */
    ESL_MSA      *msa;        /* an alignment */
    ESL_ALPHABET *abc = NULL; /* alphabet for MSA, by passing this to 
			       * eslx_msafile_Open(), we force digital MSA mode 
			       */

    /* open input file */
    if ((status = eslx_msafile_Open(&abc, infile, NULL, eslMSAFILE_STOCKHOLM, NULL, &afp)) != eslOK)
      eslx_msafile_OpenFailure(afp, status);

    /* read_msa */
    status = eslx_msafile_Read(afp, &msa);
    if(status != eslOK) esl_fatal("Alignment file %s read failed with error code %d\n", infile, status);

    /* close msa file */
    if (afp) eslx_msafile_Close(afp);

    return perl_obj(msa, "ESL_MSA");
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

/* Function:  _c_count_msa()
 * Incept:    EPN, Sat Feb  2 14:43:51 2013
 * Purpose:   Calculate and return average unaligned sequence length.

 * Purpose:   Given an msa, count residues and base pairs and store them in 
 *            <ret_abc_ct> and <ret_bp_ct>.
 *  
 *            <ret_abc_ct> [0..apos..alen-1][0..abc->K]:
 *            - per position count of each symbol in alphabet over all seqs.
 * 
 *            <ret_bp_ct>  [0..apos..alen-1][0..abc->Kp-1][0..abc->Kp-1]
 *            - per (non-pknotted) consensus basepair count of each
 *              possible basepair over all seqs basepairs are indexed
 *              by 'i' the minimum of 'i:j' for a pair between i and
 *              j, where i < j. Note that non-canonicals and gaps and
 *              the like are all stored independently.
 *
 *            A 'gap' has a looser definition than in esl_abc here,
 *            esl_abc's gap, missing residues and nonresidues are
 *            all considered 'gaps' here.
 *
 *            If we encounter an error, we return non-eslOK status
 *            and fill errbuf with error message.
 * 
 *            This function was stolen from
 *            easel/miniapps/esl-alistat.c::count_msa() vi1.1rc2 and
 *            simplified to not include PP counting.
 *
 * Returns:   eslOK on success.
 */
int _c_count_msa(ESL_MSA *msa, char *errbuf, int no_ambig, int use_weights, double ***ret_abc_ct, double ****ret_bp_ct)
{
  int status;
  double  **abc_ct = NULL;
  double ***bp_ct = NULL;
  int       apos, rpos, i, x;
  /* variables related to getting bp counts */
  int      *ct = NULL;            /* 0..alen-1 base pair partners array for current sequence */
  char     *ss_nopseudo = NULL;   /* no-pseudoknot version of structure */
  double    seqwt;  /* weight of current sequence, always 1.0 if !use_weights */

  if(! (msa->flags & eslMSA_DIGITAL)) ESL_FAIL(eslEINVAL, errbuf, "count_msa() contract violation, MSA is not digitized");
  if(use_weights && msa->wgt == NULL) ESL_FAIL(eslEINCOMPAT, errbuf, "count_msa(): use_weights==TRUE but msa->wgt == NULL");

  /* allocate and initialize bp_ct, if nec */
  ESL_ALLOC(bp_ct,  sizeof(double **) * msa->alen); 
  /* get ct array which defines the consensus base pairs */
  ESL_ALLOC(ct,  sizeof(int)  * (msa->alen+1));
  ESL_ALLOC(ss_nopseudo, sizeof(char) * (msa->alen+1));
  esl_wuss_nopseudo(msa->ss_cons, ss_nopseudo);

  if ((status = esl_wuss2ct(ss_nopseudo, msa->alen, ct)) != eslOK) ESL_FAIL(status, errbuf, "Consensus structure string is inconsistent.");
  for(apos = 0; apos < msa->alen; apos++) { 
    /* careful ct is indexed 1..alen, not 0..alen-1 */
    if(ct[(apos+1)] > (apos+1)) { /* apos+1 is an 'i' in an i:j pair, where i < j */
      ESL_ALLOC(bp_ct[apos], sizeof(double *) * (msa->abc->Kp));
      for(x = 0; x < msa->abc->Kp; x++) { 
	ESL_ALLOC(bp_ct[apos][x], sizeof(double) * (msa->abc->Kp));
	esl_vec_DSet(bp_ct[apos][x], msa->abc->Kp, 0.);
      }
    }
    else { /* apos+1 is not an 'i' in an i:j pair, where i < j, set to NULL */
      bp_ct[apos] = NULL;
    }
  }

  ESL_ALLOC(abc_ct, sizeof(double *) * msa->alen); 
  for(apos = 0; apos < msa->alen; apos++) { 
    ESL_ALLOC(abc_ct[apos], sizeof(double) * (msa->abc->K+1));
    esl_vec_DSet(abc_ct[apos], (msa->abc->K+1), 0.);
  }

  for(i = 0; i < msa->nseq; i++) { 
    seqwt = use_weights ? msa->wgt[i] : 1.0;
    
    for(apos = 0; apos < msa->alen; apos++) { /* update appropriate abc count, careful, ax ranges from 1..msa->alen (but abc_ct is 0..msa->alen-1) */
      if((! no_ambig) || (! esl_abc_XIsDegenerate(msa->abc, msa->ax[i][apos+1]))) { /* skip ambiguities (degenerate residues) if no_ambig is TRUE */
	if((status = esl_abc_DCount(msa->abc, abc_ct[apos], msa->ax[i][apos+1], seqwt)) != eslOK) ESL_FAIL(status, errbuf, "problem counting residue %d of seq %d", apos, i);
      }
    }
    
    /* get bp counts */
    for(apos = 0; apos < msa->alen; apos++) { /* update appropriate abc count, careful, ax ranges from 1..msa->alen (but abc_ct is 0..msa->alen-1) */
      if(bp_ct[apos] != NULL) { /* our flag for whether position (apos+1) is an 'i' in an i:j pair where i < j */
	rpos = ct[apos+1] - 1; /* ct is indexed 1..alen */
	bp_ct[apos][msa->ax[i][apos+1]][msa->ax[i][rpos+1]] += seqwt;
      }
    }
  }

  *ret_abc_ct = abc_ct;
  *ret_bp_ct  = bp_ct;

  if(ss_nopseudo != NULL) free(ss_nopseudo);
  if(ct != NULL) free(ct);

  return eslOK;

 ERROR:
  if(abc_ct != NULL)  esl_Free2D((void **) abc_ct, msa->alen);
  if(bp_ct != NULL)   esl_Free3D((void ***) bp_ct, msa->alen, msa->abc->Kp);
  ESL_FAIL(status, errbuf, "Error, out of memory while counting important values in the msa.");
  return status; /* NEVERREACHED */
}

/* Function: _c_bp_is_canonical
 * Incept:   EPN, Wed Oct 14 06:17:27 2009
 * Purpose:  Determine if two residues form a canonical base pair or not.
 *           Works for RNA or DNA (because for some reason cmsearch allows
 *           the user to format output as DNA (with --dna)).
 *           [derived from Infernal 1.1rc2's display.c::bp_is_canonical]
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
 *           Else, return FALSE.
 */
int 
_c_bp_is_canonical(ESL_DSQ ldsq, ESL_DSQ rdsq)
{
  switch (ldsq) { 
  case 0:
    switch (rdsq) {
    case 3: return TRUE; break;
    default: break;
    }
    break;
  case 1:
    switch (rdsq) { 
    case 2: return TRUE; break;
    default: break;
    }
    break;
  case 2:
    switch (rdsq) { 
    case 1: return TRUE; break;
    case 3: return TRUE; break;
    default: break;
    }
    break;
  case 3:
    switch (rdsq) { 
    case 0: return TRUE; break;
    case 2: return TRUE; break;
    default: break;
    }
    break;
  default: break;
  }
  
  return FALSE;
}

/* Function:  _c_calc_and_write_bp_stats()
 * Incept:    EPN, Sat Feb  2 14:47:15 2013
 * Purpose:   Calculate and output bp stats, including
 *            fraction of canonical bps and covariation for 
 *            each pair in msa->ss_cons.
 * Returns:   eslOK on success
 *            ! eslOK on failure
 */
int _c_calc_and_write_bp_stats(ESL_MSA *msa, char *outfile)
{
  int   status;
  FILE *ofp;               /* open output alignment file */
  int   apos;              /* counter over alignment positions */
  int   a,b;               /* counters */
  double    sum = 0.;      /* sum of all bp counts for a basepair */
  double    c_sum = 0.;    /* sum of canonical bp counts for a basepair */
  double  **abc_ct = NULL; /* [0..msa->alen-1][0..abc->K] number of each residue at each position (abc->K is gap) */
  double ***bp_ct  = NULL; /* [0..msa->alen-1][0..abc->Kp-1][0..abc->Kp-1] per (non-pknotted) consensus basepair *
			    * count of each possible basepair over all seqs basepairs are indexed by 'i' the minimum *
			    * of 'i:j' for a pair between i and j, where i < j. */
  char errbuf[eslERRBUFSIZE];
  int      *ct = NULL;            /* ct array of basepairs for SS_cons */
  char     *ss_nopseudo = NULL;   /* no-pseudoknot version of structure */

  /* TODO: return errbuf, but I can't get that to work, left this block here as starting point for revisiting that:
     if(msa->abc->type != eslRNA)              ESL_FAIL(eslEINVAL, errbuf, "msa type not eslRNA");
     if(msa->ss_cons   != NULL)                ESL_FAIL(eslEINVAL, errbuf, "ss_cons is NULL");
     if((ofp  = fopen(outfile, "w"))  == NULL) ESL_FAIL(eslEINVAL, errbuf, "unable to open %s for writing", outfile);
  */

  /* Do required preliminary steps, if any fail, return FALSE, to tell caller we failed */
  if(msa->abc->type != eslRNA)             return FALSE;
  if(msa->ss_cons   == NULL)               return FALSE;
  if((ofp = fopen(outfile, "w"))  == NULL) return FALSE;

  /* get counts */
  if((status = _c_count_msa(msa, errbuf, 
			    FALSE, /* don't ignore ambiguous residues */
			    FALSE, /* don't use msa->wgt sequence weights */
			    &abc_ct, &bp_ct))
     != eslOK) {
    return status;
  }

  /* output data */
  fprintf(ofp, "%-7s  %-11s  %-18s  %-7s\n", "# acc", "bp_coords", "canonical_fraction", "covariation");

  /* get ct array which defines the consensus base pairs */
  ESL_ALLOC(ct,  sizeof(int) * (msa->alen+1));
  ESL_ALLOC(ss_nopseudo, sizeof(char) * (msa->alen+1));
  esl_wuss_nopseudo(msa->ss_cons, ss_nopseudo);
  if ((status = esl_wuss2ct(ss_nopseudo, msa->alen, ct)) != eslOK) ESL_FAIL(status, errbuf, "Consensus structure string is inconsistent.");

  for(apos = 0; apos < msa->alen; apos++) { 
    if(ct[(apos+1)] > (apos+1)) { /* apos+1 is an 'i' in an i:j pair, where i < j */
      c_sum = sum = 0.;
      for(a = 0; a < msa->abc->K; a++) { 
	for(b = 0; b < msa->abc->K; b++) { 
	  sum += bp_ct[apos][a][b];
	  if(_c_bp_is_canonical(a, b)) { 
	    c_sum += bp_ct[apos][a][b];
	  }
	}
      }
      fprintf(ofp,"%-7s  %5d:%-5d  %6.4f\n", msa->acc, apos+1, ct[apos+1], c_sum/sum);
    }  
  }

  fclose(ofp);
  if(abc_ct != NULL) { esl_Free2D((void **) abc_ct, msa->alen);               abc_ct   = NULL; }
  if(bp_ct != NULL)  { esl_Free3D((void ***) bp_ct, msa->alen, msa->abc->Kp); bp_ct = NULL; }
  if(ss_nopseudo != NULL) free(ss_nopseudo);
  if(ct != NULL) free(ct);

  return eslOK;

 ERROR:
  fclose(ofp);
  if(abc_ct != NULL) { esl_Free2D((void **) abc_ct, msa->alen);               abc_ct   = NULL; }
  if(bp_ct != NULL)  { esl_Free3D((void ***) bp_ct, msa->alen, msa->abc->Kp); bp_ct = NULL; }
  if(ss_nopseudo != NULL) free(ss_nopseudo);
  if(ct != NULL) free(ct);
  return status; /* failure */
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

