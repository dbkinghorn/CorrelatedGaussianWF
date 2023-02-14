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

#define MAXLEN 10000
#define BUFLEN 1000000
 
int main(argc,argv)
int argc;
char**argv;
{
   int out[BUFLEN],in[BUFLEN],i,j,k;
   int myself,tasks,recvcounts[128];
 
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&myself);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);

   for (i = 0; i < BUFLEN; i++)
     out[i] = 1;

   for(j=1;j<=MAXLEN*tasks;j*=10)  {
      for(i=0;i<tasks;i++)  recvcounts[i] = j;
      for(i=0;i<j*tasks;i++)  out[i] = i;

      MPI_Reduce_scatter(out,in,recvcounts,MPI_INT,MPI_SUM,MPI_COMM_WORLD);

      for(k=0;k<j;k++) {
         if(in[k] != tasks*(myself*j+k)) {  printf("bad answer (%d) at index %d of %d (should be %d)\n",in[k],k,j,tasks*(myself*j+k)); break; }
      }
   }
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
   return 0;
}
