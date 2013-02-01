#include "easel.h"
#include "esl_distance.h"
#include "esl_msa.h"
#include "esl_msafile.h"

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

SV *_c_read_msa (char *infile, SV *perl_abc) 
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
    /* convert C abc object to perl */
    perl_abc = perl_obj(abc, "ESL_ALPHABET");

    return perl_obj(msa, "ESL_MSA");
}    

void _c_write_msa (ESL_MSA *msa, char *outfile) 
{
    FILE         *ofp;        /* open output alignment file */

    if ((ofp  = fopen(outfile, "w"))  == NULL) esl_fatal("Failed to open output file %s\n", outfile);
    eslx_msafile_Write(ofp, msa, eslMSAFILE_STOCKHOLM);
    fclose(ofp);

    return;
}

void _c_free_msa (ESL_MSA *msa)
{
  esl_msa_Destroy(msa);
  return;
}

void _c_destroy (ESL_MSA *msa, ESL_ALPHABET *abc)
{
  _c_free_msa(msa);
  if(abc) esl_alphabet_Destroy(abc);
  return;
}

I32 _c_nseq (ESL_MSA *msa)
{
  return msa->nseq;
}   

I32 _c_alen (ESL_MSA *msa)
{
  return msa->alen;
}

char *_c_get_sqname (ESL_MSA *msa, I32 idx)
{
    /* should this check if idx is valid? perl func that calls it already does... is that proper? */
    return msa->sqname[idx];
}

void _c_set_sqname (ESL_MSA *msa, I32 idx, char *newname)
{

    /* should this check if idx is valid? perl func that calls it already does... is that proper? */
    if(msa->sqname[idx]) free(msa->sqname[idx]);
    esl_strdup(newname, -1, &(msa->sqname[idx]));

    return;
}   

int _c_any_allgap_columns (ESL_MSA *msa) 
{
  /* determine if there's any all gap columns */
  printf("in _c_any_allgap_columns()\n");
  printf("msa->abc->type is %d\n", msa->abc->type);

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

float _c_average_pid(ESL_MSA *msa, int max_nseq) 
{
  double avgid;
  
  esl_dst_XAverageId(msa->abc, msa->ax, msa->nseq, (max_nseq * max_nseq), &avgid);
  
  return (float) avgid;
}
