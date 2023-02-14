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

int main(argc,argv)
int argc;
char**argv;
{
   int in,out,done;
   int myself,tasks;
   MPI_Request req1,req2;
   MPI_Status status;
 
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&myself);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);

   in = -1;
   out = 1;

   if(myself < 2)  {
      if(myself == 0)  {
         MPI_Isend(&out,1,MPI_INT,1,1,MPI_COMM_WORLD,&req1);
         MPI_Irecv(&in,1,MPI_INT,1,2,MPI_COMM_WORLD,&req2);
         for(;;) { MPI_Test(&req1,&done,&status); if(done) break; }
         for(;;) { MPI_Test(&req2,&done,&status); if(done) break; }
      } else if(myself == 1) { 
         MPI_Send(&out,1,MPI_INT,0,2,MPI_COMM_WORLD);
         MPI_Recv(&in,1,MPI_INT,0,1,MPI_COMM_WORLD,&status);
      }
      if(in != 1) printf("ERROR IN TASK %d (%d)\n",myself,in);
   }
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
return 0;
}
