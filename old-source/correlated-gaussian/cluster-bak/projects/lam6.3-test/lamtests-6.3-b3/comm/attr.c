/****************************************************************************

 MESSAGE PASSING INTERFACE TEST CASE SUITE

 Copyright IBM Corp. 1995

 IBM Corp. hereby grants a non-exclusive license to use, copy, modify, and
 distribute this software for any purpose and without fee provided that the
 above copyright notice and the following paragraphs appear in all copies.

 IBM Corp. makes no representation that the test cases comprising this
 suite are correct or are an accurate representation of any standard.

 In no event shall IBM be liable to any party for direct, indirect, special
 incidental, or consequential damage arising out of the use of this software
 even if IBM Corp. has been advised of the possibility of such damage.

 IBM CORP. SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT LIMITED
 TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS AND IBM
 CORP. HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 ENHANCEMENTS, OR MODIFICATIONS.

****************************************************************************

 These test cases reflect an interpretation of the MPI Standard.  They are
 are, in most cases, unit tests of specific MPI behaviors.  If a user of any
 test case from this set believes that the MPI Standard requires behavior
 different than that implied by the test case we would appreciate feedback.

 Comments may be sent to:
    Richard Treumann
    treumann@kgn.ibm.com

****************************************************************************
*/
#include <stdio.h>
#include "mpi.h"

int 
get_int_value(void* val)
{
  int i;
  void *v1;
  int *i1, *i2;

  v1 = (void*) 1;
  i1 = (int*) &v1;
  i2 = (int*) val;
  for (i = 0; i < (sizeof(void*) / sizeof(int)); i++) {
    printf("Checking for value in position %d -- value is %d\n", i, i2[i]);
    if (i1[i] == 1)
      return i2[i];
  }
  
  return 0;
}

int
main(argc,argv)
int argc;
char**argv;
{
   int class,rc,me,tasks,flag,key,temp;
   MPI_Comm comm;
   void *val;
   int intval;

#define TAG_UB (1<<15)-1 

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);

   MPI_Attr_get(MPI_COMM_WORLD,MPI_TAG_UB,&val,&flag);
   if(!flag) printf("ERROR in MPI_Attr_get: no val for MPI_TAG_UB\n");
   if (TAG_UB < 32767) {
      printf("ERROR: TAG_UB %d < 32767\n", TAG_UB);
   }
   /* Have to do some mucking around if sizeof(void*) may be > sizeof(int) */
   intval = get_int_value(val);
   if (intval != TAG_UB)
     printf("ERROR: tag_ub is %d, expected %d\n", intval, TAG_UB);
   MPI_Attr_get(MPI_COMM_WORLD,MPI_HOST,&val,&flag);
   if(!flag) printf("ERROR in MPI_Attr_get: no val for MPI_HOST\n");
   intval = get_int_value(val);
   if ((intval != MPI_PROC_NULL) && ((intval < 0) || (intval >= tasks)))
      printf("ERROR in MPI_Attr_get: host = %d\n",intval);

   MPI_Attr_get(MPI_COMM_WORLD,MPI_IO,&val,&flag);
   if(!flag) printf("ERROR in MPI_Attr_get: no val for MPI_IO\n");
   intval = get_int_value(val);
   if ((intval != MPI_ANY_SOURCE) && (intval != MPI_PROC_NULL) &&
				((intval < 0) || (intval >= tasks)))
      printf("ERROR in MPI_Attr_get: io = %d\n",intval);

   MPI_Keyval_create(MPI_NULL_COPY_FN,MPI_NULL_DELETE_FN,&key,0);

   MPI_Comm_dup(MPI_COMM_WORLD,&comm);

/*
 * MPI does not require attributes be copied on MPI_Comm_dup().
 */
   MPI_Attr_get(comm,MPI_TAG_UB,&val,&flag);
   if(flag) {
       intval = get_int_value(val);
       if(intval != TAG_UB)
	   printf("ERROR in MPI_Attr_get: tag_ub = %d, should be %d\n",
		  intval,TAG_UB);
   }

   intval = 12345;
   MPI_Attr_put(comm,key,&intval);
   MPI_Attr_get(comm,key,&val,&flag);
   if(flag == 0)
       printf("ERROR in MPI_Attr_get: flag is false\n");
   intval = *((int*) val);
   if(intval != 12345)
       printf("ERROR in MPI_Attr_get: val = %d, should be %d\n",
	      intval,12345);
   MPI_Errhandler_set(comm,MPI_ERRORS_RETURN);

   temp = key;
   MPI_Keyval_free(&key);
   if(key != MPI_KEYVAL_INVALID)
      printf("ERROR in MPI_Keyval_free: key not set to INVALID\n");

   /* Note that this is erroneous use of a keyval; the standard 
      does not specify any particular behavior */
   key = temp;
   rc = MPI_Attr_get(comm,key,&val,&flag);
   MPI_Error_class(rc,&class);
   if(class != MPI_ERR_OTHER) {
       if (rc != MPI_SUCCESS) {
	   printf("WARNING in MPI_Keyval_free: key not freed\n");
	   printf("error returned was %d(%d)\n", rc, class );
	   }
       else {
/*
 * MPI does not require this user error to be detected.
 */
       }
  }

   rc = MPI_Attr_delete(comm,MPI_TAG_UB);
   if(rc == MPI_SUCCESS)
      printf("ERROR in MPI_Attr_delete, no error detected\n"); 

   rc = MPI_Attr_delete(comm,MPI_HOST);
   if(rc == MPI_SUCCESS)
      printf("ERROR in MPI_Attr_delete, no error detected\n"); 

   rc = MPI_Attr_delete(comm,MPI_IO);
   if(rc == MPI_SUCCESS)
      printf("ERROR in MPI_Attr_delete, no error detected\n"); 

   MPI_Barrier(comm);
   MPI_Comm_free( &comm );
   MPI_Finalize();
return 0;
}
