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
#include <stdlib.h>
#include "mpi.h"

int main(argc,argv)
int argc;
char**argv;
{
   int myself;
   MPI_Status status;
 
   int ii;
   int len;
   int numtasks,me;
   MPI_Aint disp;
   MPI_Datatype type,newtype;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&myself);

    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &me);

    if ((numtasks < 2)) {
      printf("this testcase requires 2 tasks.\n");
      MPI_Abort(MPI_COMM_WORLD, me);
    }
    
    if ((numtasks > 2) && (me > 1)) { 
#ifdef DB_TALK
      printf("Testcase uses two tasks, extraneous task #%d exited.\n",me);
#endif
      MPI_Finalize();
      exit(1);
    }
   len = 1;
   MPI_Address(&ii,&disp);
   type = MPI_INT;
   MPI_Type_struct(1,&len,&disp,&type,&newtype);
   MPI_Type_commit(&newtype);

   if(myself == 0)  {
      ii = 2;
      MPI_Send(MPI_BOTTOM,1,newtype,1,0,MPI_COMM_WORLD);
   } else if(myself == 1) {
      ii = 0;
      MPI_Recv(MPI_BOTTOM,1,newtype,0,0,MPI_COMM_WORLD,&status);
      if(ii != 2) printf("MPI_BOTTOM test FAILED.\n"); 
   } 
   MPI_Type_free( &newtype );
   MPI_Finalize();
return 0; 
}
