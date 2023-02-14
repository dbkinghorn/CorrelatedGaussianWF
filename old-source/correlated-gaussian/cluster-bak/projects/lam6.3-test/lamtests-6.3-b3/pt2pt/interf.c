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
   int me,tasks,val1,val2;
   MPI_Comm my_comm;
   MPI_Status status;
   MPI_Request request1,request2;

   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_dup(MPI_COMM_WORLD,&my_comm);
   if(me==0)  {
      val1 = 1;
      MPI_Send(&val1,1,MPI_INT,1,1,MPI_COMM_WORLD);
      val2 = 2;
      MPI_Send(&val2,1,MPI_INT,1,1,my_comm);
   } else if(me == 1)  {
      MPI_Irecv(&val1,1,MPI_INT,0,1,my_comm,&request1);
      MPI_Irecv(&val2,1,MPI_INT,0,1,MPI_COMM_WORLD,&request2);
      MPI_Wait(&request1,&status);
      MPI_Wait(&request2,&status);
      if(val1 != 2 || val2 != 1)  printf("ERROR, messages were exchanged between different communicators\n");
   }
   MPI_Barrier(my_comm);
   MPI_Comm_free( &my_comm );
   MPI_Finalize();
   return 0;
}
