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

int main(ac, av)
int ac;
char **av;
{
   int outmsg,inmsg; 
   int me,flag=0;
   MPI_Status status;
   MPI_Request msgid;

   MPI_Init(&ac,&av);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
 
   if(me == 1) { 
      outmsg = 5;
      MPI_Send(&outmsg,1,MPI_INT,0,1,MPI_COMM_WORLD);
   }
   if(me == 0) {
      MPI_Irecv(&inmsg,1,MPI_INT,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&msgid);
      while(flag == 0)
         MPI_Test(&msgid,&flag,&status);
      if(!flag || inmsg != 5 || status.MPI_SOURCE != 1 || status.MPI_TAG != 1)
         printf("ERROR flag, inmsg, src, tag = %d %d %d %d, should be %d %d %d %d\n",flag,inmsg,status.MPI_SOURCE,status.MPI_TAG,1,5,1,1);
   }
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
return 0;
}
