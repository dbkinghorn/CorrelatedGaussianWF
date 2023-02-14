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
   int me,cnt,data,src,tag,flag;
   MPI_Comm comm;
   MPI_Status status;

   MPI_Init(&argc,&argv);
   comm = MPI_COMM_WORLD;
   MPI_Comm_rank(comm,&me);

   if(me == 0) {
      data = 7;
      MPI_Send(&data,1,MPI_INT,1,1,comm);
   } else if(me == 1)  {
      for(;;)  {
         MPI_Iprobe(0,1,comm,&flag,&status);
         if(flag) break;
      }
      src = status.MPI_SOURCE;
      if(src != 0)
         printf("ERROR in MPI_Probe: src = %d, should be %d\n",src,0);
      tag = status.MPI_TAG;
      if(tag != 1)
         printf("ERROR in MPI_Probe: tag = %d, should be %d\n",tag,1);
      MPI_Get_count(&status,MPI_INT,&cnt);
      if(cnt != 1) printf("ERROR in MPI_Probe: cnt = %d, should be %d\n",cnt,1);
      MPI_Recv(&data,cnt,MPI_INT,src,tag,comm,&status);
      if(data != 7) printf("ERROR in MPI_Recv, data = %d, should be %d\n",data,7);
   }
   MPI_Barrier(comm);
   MPI_Finalize();
return 0;
}
