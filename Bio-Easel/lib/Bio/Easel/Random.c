#include "easel.h"
#include "esl_random.h"

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


/* Function:  _c_create_randomness()
 * Incept:    EPN, Tue Apr  9 09:28:24 2013
 * Synopsis:  Create an ESL_RANDOMNESS object, given a seed.
 * Returns:   eslOK on success, dies with 'croak' upon an error.
 */

SV *_c_create_randomness(U32 seed)
{
  int           status;      /* Easel status code */
  ESL_RANDOMNESS *r = NULL;  /* the RNG */

  r = esl_randomness_Create(seed);
  if(r == NULL) croak("unable to create ESL_RANDOMNESS object, probably out of memory"); 

  return perl_obj(r, "ESL_RANDOMNESS");
}    

/* Function:  _c_roll()
 * Incept:    EPN, Tue Apr  9 09:32:00 2013
 * Synopsis:  Return a uniformly distributed integer in the range 0..a-1.
 * Note:      Based on easel's esl_roll() macro.
 * Args:      r:     ESL_RANDOMNESS
 *            range: randomly chosen int between 0 and range-1 will be returned.
 * Returns:   randomly chosen integer
 */

unsigned long _c_roll (ESL_RANDOMNESS *r, unsigned long range)
{
  return (unsigned long) (esl_random(r) * (range));
}

/* Function:  _c_get_seed()
 * Incept:    EPN, Tue Apr  9 09:50:21 2013
 * Synopsis:  Return seed of a ESL_RANDOMNESS object.
 * Returns:   r->seed.
 */

U32 _c_get_seed(ESL_RANDOMNESS *r)
{
  if(r == NULL) { croak("_c_get_seed, r is NULL"); }
  return r->seed;
}

/* Function:  _c_destroy()
 * Incept:    EPN, Tue Apr  9 09:34:37 2013
 * Synopsis:  Destroy an ESL_RANDOMNESS object.
 * Returns:   void
 */

void _c_destroy(ESL_RANDOMNESS *r)
{
  if(r) esl_randomness_Destroy(r);
  r = NULL;
  return;
}
