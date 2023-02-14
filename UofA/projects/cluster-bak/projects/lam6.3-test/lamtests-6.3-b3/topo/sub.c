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
   int me,tasks,i,dims[10],periods[10],remain[10];
   int size,rank;
   MPI_Comm comm,subcomm;
   MPI_Comm comms[20];
   int      cnt=0;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);

   if(tasks != 6)  {
      printf("MUST RUN WITH 6 TASKS\n");
      MPI_Finalize();
      exit(0);
   }

   dims[0] = 2;  dims[1] = 3;
   periods[0] = 0;
   periods[1] = 0;
   MPI_Cart_create(MPI_COMM_WORLD,2,dims,periods,0,&comm);
   comms[cnt++] = comm;

   remain[0] = 0;  remain[1] = 1;
   MPI_Cart_sub(comm,remain,&subcomm);
   comms[cnt++] = subcomm;
   MPI_Comm_size(subcomm,&size);
   if(size != 3)
      printf("ERROR in MPI_Cart_sub: size = %d, should be 3\n",size);
   MPI_Comm_rank(subcomm,&rank);
   if(rank != me%3)
      printf("ERROR in MPI_Cart_sub: rank = %d, should be %d\n",rank,me);

   remain[0] = 0;  remain[1] = 0;
   MPI_Cart_sub(comm,remain,&subcomm);
   comms[cnt++] = subcomm;
   MPI_Comm_size(subcomm,&size);
   if(size != 1)
      printf("ERROR in MPI_Cart_sub: size = %d, should be 1\n",size);
   MPI_Comm_rank(subcomm,&rank);
   if(rank != 0)
      printf("ERROR in MPI_Cart_sub: rank = %d, should be %d\n",rank,0);

   remain[0] = 1;  remain[1] = 1;
   MPI_Cart_sub(comm,remain,&subcomm);
   comms[cnt++] = subcomm;
   MPI_Comm_size(subcomm,&size);
   if(size != tasks)
      printf("ERROR in MPI_Cart_sub: size = %d, should be %d\n",size,tasks);
   MPI_Comm_rank(subcomm,&rank);
   if(rank != me)
      printf("ERROR in MPI_Cart_sub: rank = %d, should be %d\n",rank,me);

   remain[0] = 1;  remain[1] = 0;
   MPI_Cart_sub(comm,remain,&subcomm);
   comms[cnt++] = subcomm;
   MPI_Comm_size(subcomm,&size);
   if(size != 2)
      printf("ERROR in MPI_Cart_sub: size = %d, should be 2\n",size);
   MPI_Comm_rank(subcomm,&rank);
   if(rank != me/3)
      printf("ERROR in MPI_Cart_sub: rank = %d, should be %d\n",rank,me/3);

   MPI_Barrier(MPI_COMM_WORLD);
   for (i=0; i<cnt; i++) 
       MPI_Comm_free( &comms[i] );
   MPI_Finalize();
return 0;
}
