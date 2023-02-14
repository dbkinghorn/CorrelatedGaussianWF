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
#include "mpi.h"

int
main(argc,argv)
int argc;
char**argv;
{
   int i,myself;
   MPI_Comm comm,newcomm;

#define ITER 10 

   MPI_Comm savecomm[2*ITER];
   int      cnt = 0;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&myself);
   
   for(i=0;i<ITER;i++) {
      MPI_Comm_dup(MPI_COMM_WORLD,&comm);
      savecomm[cnt++] = comm;
   }

   comm = MPI_COMM_WORLD;
   for(i=0;i<ITER;i++) { 
      MPI_Comm_dup(comm,&newcomm);
      comm = newcomm;
      savecomm[cnt++] = newcomm;
   }
   MPI_Barrier(MPI_COMM_WORLD);
   for (i=0; i<cnt; i++) 
       MPI_Comm_free( &savecomm[i] );

   MPI_Finalize();
   return 0;
}
