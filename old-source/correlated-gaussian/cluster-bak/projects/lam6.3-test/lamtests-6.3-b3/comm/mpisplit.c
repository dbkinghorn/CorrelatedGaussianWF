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

int
main(argc,argv)
int argc;
char**argv;
{
   int i,me,tasks,color,key,ranks[128];
   MPI_Comm newcomm;

   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);
   if(tasks%2) {
       printf("MUST HAVE EVEN NUMBER OF TASKS\n");
       MPI_Finalize();
       exit(0);
   }

   MPI_Comm_rank(MPI_COMM_WORLD,&me);

   color = me%2;
   key = me;
   MPI_Comm_split(MPI_COMM_WORLD,color,key,&newcomm);
   MPI_Allgather(&me,1,MPI_INT,ranks,1,MPI_INT,newcomm);
   if(me%2 == 0) {
      for(i=0;i<tasks/2;i++)
         if(ranks[i] != 2*i)  printf("ERROR in MPI_Comm_split: wrong tasks\n");
   }
   if(me%2 != 0) {
      for(i=0;i<tasks/2;i++)
         if(ranks[i] != 2*i+1)  printf("ERROR in MPI_Comm_split: wrong tasks\n");
   }
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Comm_free( &newcomm );
   MPI_Finalize();
return 0;
}
