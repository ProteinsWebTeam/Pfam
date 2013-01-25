#include "easel.h"
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


SV *c_read_msa (char *infile) 
{
    int           status;     /* Easel status code */
    ESLX_MSAFILE *afp;        /* open input alignment file */
    ESL_MSA      *msa;        /* an alignment */

    /* open input file */
    if ((status = eslx_msafile_Open(NULL, infile, NULL, eslMSAFILE_UNKNOWN, NULL, &afp)) != eslOK)
      eslx_msafile_OpenFailure(afp, status);

    /* read_msa */
    status = eslx_msafile_Read(afp, &msa);
    if(status != eslOK) esl_fatal("Alignment file %s read failed with error code %d\n", infile, status);

    printf("read %d seqs\n", msa->nseq);
    
    return perl_obj(msa, "ESL_MSA");
}    

void c_write_msa (ESL_MSA *msa, char *outfile) 
{
    FILE         *ofp;        /* open output alignment file */

    if ((ofp  = fopen(outfile, "w"))  == NULL) esl_fatal("Failed to open output file %s\n", outfile);
    eslx_msafile_Write(ofp, msa, eslMSAFILE_STOCKHOLM);

    return;
}

I32 c_nseq (ESL_MSA *msa)
{
    return msa->nseq;
}   

char *c_get_sqname_idx (ESL_MSA *msa, I32 idx)
{
    /* should this check if idx is valid? perl func that calls it already does... is that proper? */
    return msa->sqname[idx];
}

void c_set_sqname_idx (ESL_MSA *msa, I32 idx, char *newname)
{

    /* should this check if idx is valid? perl func that calls it already does... is that proper? */
    if(msa->sqname[idx]) free(msa->sqname[idx]);
    esl_strdup(newname, -1, &(msa->sqname[idx]));

    return;
}   




int is_nonempty_arrayref (SV* the_arg) {

        /* Make sure we have an array ref with values */
        if (
        (!SvROK(the_arg))  // arg is not a reference
        || (SvTYPE(SvRV(the_arg)) != SVt_PVAV)  // arg is a reference, but it's not an array ref
        || ( av_len((AV *)SvRV(the_arg)) < 0)) { // arg is a ref to an empty array
                return 0;
        } else {
                return 1;
        }

}


/*
*  #################################
*  ###### c_convert_2D_array #######
*  #################################
* Accept
*  * a pointer to an 2-D array from perl
*  * a char indicating which type of variables reside in the array ('i'=int, 'd'=double - for now)
*  * a pointer to a (not yet malloced) 2-D array of the appropriate type.
*  * a pointer to an I32(essentially an int), which stores the # rows
*  * a pointer to an array of I32s, which stores the # cols in each row
*
* Action
*  * Allocate the array of the appropriate type
*  * Assign values to the array.
*  * Assign values to the row_cnt and col_cnts variables
*  * Return 1 if successful, 0 otherwise
*
* Receiving function is responsible for freeing memory
*/
int c_convert_2D_array (SV* my_arg, char type, void*** arr, int* row_cnt, int** col_cnts) {
	int i,j;

        AV* twoD_array;
        int** i_arr = NULL;
        double** d_arr = NULL;
        SV* row_ref;
        AV* row_array;

	if ( ! (type=='i' || type=='d') )
		return 0;

	if (is_nonempty_arrayref(my_arg)==0)
		return 0;

	twoD_array = (AV *)SvRV(my_arg);
	*row_cnt =  av_len(twoD_array) + 1;

	if (type == 'i') {
		i_arr = (int**) malloc(*row_cnt * sizeof(int*));
		*arr = (void**)i_arr;
	} else {// 	if (type == 'd') {
		d_arr = (double**) malloc(*row_cnt * sizeof(double*));
		*arr = (void**)d_arr;
	}
	*col_cnts = (int*) malloc(*row_cnt * sizeof(int));

	for (i = 0; i < *row_cnt ; i++) {
		row_ref = (* av_fetch(twoD_array, i, 0) );
		if (is_nonempty_arrayref(row_ref)==0)
			return 0;

		row_array = (AV *)SvRV(row_ref);
		(*col_cnts)[i] = av_len(row_array) + 1;

		if (type == 'i') {
			i_arr[i] = (int*) malloc((*col_cnts)[i] * sizeof(int));
		} else { // 	if (type == 'd') {
			d_arr[i] = (double*) malloc((*col_cnts)[i] * sizeof(double));
		}

		for (j = 0; j < (*col_cnts)[i] ; j++) {
			if (type == 'i') {
				i_arr[i][j] = SvIV(* av_fetch(row_array, j, 0)); //SvIV is for doubles
			} else { // if (type == 'd') {
				d_arr[i][j] = SvNV(* av_fetch(row_array, j, 0)); //SvNV is for doubles
			}
		}
	}

	return 1;
}


/*
*  #################################
*  ###### c_convert_1D_array #######
*  #################################
* Accept
*  * a pointer to an 1-D array from perl
*  * a char indicating which type of variables reside in the array ('i'=int, 'd'=double - for now)
*  * a pointer to a (not yet malloced) 1-D array of the appropriate type.
*  * a pointer to an I32(essentially an int), which stores the # elements
*
* Action
*  * Allocate the array of the appropriate type
*  * Assign values to the array.
*  * Assign values to the row_cnt and col_cnts variables
*  * Return 1 if successful, 0 otherwise
*
* Receiving function is responsible for freeing memory
*/
int c_convert_1D_array (SV* my_arg, char type, void** arr, int* count) {
	int i;
        AV* array; 
        int* i_arr = NULL;
        double* d_arr = NULL;

	if ( ! (type=='i' || type=='d') )
		return 0;

	if (is_nonempty_arrayref(my_arg)==0)
		return 0;

	array = (AV *)SvRV(my_arg);
	*count =  av_len(array) + 1;

	if (type == 'i') {
		i_arr = (int*) malloc(*count * sizeof(int));
		*arr = (void*)i_arr;
	} else { // 	if (type == 'd') {
		d_arr = (double*) malloc(*count * sizeof(double));
		*arr = (void*)d_arr;
        }

	for (i = 0; i < *count ; i++) {
		if (type == 'i') {
			i_arr[i] = SvIV(* av_fetch(array, i, 0)); //SvIV is for doubles
		} else { // if (type == 'd') {
			d_arr[i] = SvNV(* av_fetch(array, i, 0)); //SvNV is for doubles
		}
	}


	return 1;
}





// ========================================================================================
// ============  convenience functions   ==================================================
// ========================================================================================



/*
*  #################################
*  ##### c_convert_1D_double #######
*  #################################
* Accept
*  * a pointer to a 1-D array from perl
*  * a pointer to a (not yet malloced) 1-D array of doubles.
*  * a pointer to an I32(essentially an int), which stores the # rows
*
* Action
*  * call c_convert_1D_array, let it do the job
*  * Return 1 if successful, 0 otherwise
*
* Receiving function is responsible for freeing memory
*/
int c_convert_1D_double (SV* my_arg, double** d_array, int* count) {
	return  c_convert_1D_array (my_arg, 'd', (void**)d_array, count);
}



/*
*  #################################
*  ###### c_convert_1D_int #########
*  #################################
* Accept
*  * a pointer to a 1-D array from perl
*  * a pointer to a (not yet malloced) 1-D array of ints.
*  * a pointer to an I32(essentially an int), which stores the # rows
*
* Action
*  * call c_convert_1D_array, let it do the job
*  * Return 1 if successful, 0 otherwise
*
* Receiving function is responsible for freeing memory
*/
int c_convert_1D_int (SV* my_arg, int** i_array, int* count) {
	return  c_convert_1D_array (my_arg, 'i', (void**)i_array, count);
}


/*
*  #################################
*  ##### c_convert_2D_double #######
*  #################################
* Accept
*  * a pointer to a 2-D array from perl
*  * a pointer to a (not yet malloced) 2-D array of doubles.
*  * a pointer to an I32(essentially an int), which stores the # rows
*  * a pointer to an array of I32s, which stores the # cols in each row
*
* Action
*  * call c_convert_2D_array, let it do the job
*  * Return 1 if successful, 0 otherwise
*
* Receiving function is responsible for freeing memory
*/
int c_convert_2D_double (SV* my_arg, double*** d_array, int* row_cnt, int** col_cnts) {
	return  c_convert_2D_array (my_arg, 'd', (void***)d_array, row_cnt, col_cnts);
}


/*
*  #################################
*  ####### c_convert_2D_int ########
*  #################################
* Accept
*  * a pointer to an 2-D array from perl
*  * a pointer to a (not yet malloced) 2-D array of ints.
*  * a pointer to an I32(essentially an int), which stores the # rows
*  * a pointer to an array of I32s, which stores the # cols in each row
*
* Action
*  * call c_convert_2D_array, let it do the job
*  * Return 1 if successful, 0 otherwise
*
* Receiving function is responsible for freeing memory
*/
int c_convert_2D_int (SV* my_arg, int*** i_array , int* row_cnt, int** col_cnts) {
	return  c_convert_2D_array (my_arg, 'i', (void***)i_array, row_cnt, col_cnts);
}




