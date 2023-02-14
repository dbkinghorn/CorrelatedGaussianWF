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

#define ITER 2 

int main(argc,argv)
int argc;
char**argv;
{
   int i,me,datain[2000],dataout[2000];
   int buf[1000000];
   MPI_Status status;
   MPI_Request request;
   MPI_Comm comm;

   MPI_Init(&argc,&argv);
   comm = MPI_COMM_WORLD;
   MPI_Comm_rank(comm,&me);

   /* Clear the buffer to prevent unitialized reads */

   for (i = 0; i < 2000; i++)
     dataout[i] = i;

   MPI_Buffer_attach(buf,sizeof(buf));
   if(me == 0)  {
      for(i=0;i<ITER;i++)  {
         dataout[i] = i;
         MPI_Ibsend(&dataout[i],sizeof(dataout) - 10,MPI_BYTE,1,0,comm,
		    &request);
         MPI_Request_free(&request); 
      }
      MPI_Barrier(comm);
   } else if(me == 1) {
      MPI_Barrier(comm);
      for(i=0;i<ITER;i++)  {
         MPI_Recv(datain,sizeof(datain) - 10,MPI_BYTE,0,0,comm,&status);
         if(datain[0] != i)  printf("ERROR %d %d\n",i,datain[0]);
      }
   }
   MPI_Barrier(comm);
   MPI_Finalize();
   return 0;
}
