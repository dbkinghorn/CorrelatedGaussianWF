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
#define MAXDIMS 10
 
int main(argc,argv)
int argc;
char**argv;
{
   int rc,class,dims[MAXDIMS],tasks,me,ndims;
 
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);

   tasks = 6; 
   ndims = 2;
   dims[0] = 0;  dims[1] = 0;
   MPI_Dims_create(tasks,ndims,dims);
   if(dims[0] != 3 || dims[1] != 2)
      printf("ERROR in MPI_Dims_create, dims = %d,%d, should be %d %d\n",dims[0],dims[1],3,2);
 
   dims[0] = 2;  dims[1] = 0;
   ndims = 2;
   MPI_Dims_create(tasks,ndims,dims);
   if(dims[0] != 2 || dims[1] != 3)
      printf("ERROR in MPI_Dims_create, dims = %d,%d, should be %d %d\n",dims[0],dims[1],2,3);

   MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);
   dims[0] = 0;  dims[1] = 5;
   ndims = 2;
   rc = MPI_Dims_create(tasks,ndims,dims);
   MPI_Error_class(rc,&class);
   if(class != MPI_ERR_DIMS) {
      printf("ERROR in MPI_Dims_create, invalid dims not detected\n");
      printf( "Error values are %d(%d)\n", rc, class );
      }

   dims[0] = 0;  dims[1] = 0;  dims[2] = 0;
   ndims = 3;
   rc = MPI_Dims_create(tasks,ndims,dims);
   if(dims[0] != 3 || dims[1] != 2 || dims[2] != 1)
      printf("ERROR in MPI_Dims_create, dims = %d,%d,%d, should be %d %d %d\n",dims[0],dims[1],dims[2],3,2,1);

   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
return 0;
}
