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
#include <malloc.h>
#include "mpi.h"
 
int
main(argc,argv)
int argc;
char**argv;
{
   int gsize,sendarray[100][150],*sptr;
   int root,*rbuf = 0,myrank,blocklen[2];
   MPI_Datatype stype,type[2];
   int displs[128],i,j,t,rcounts[128],num;
   MPI_Aint disp[2];
 
   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&gsize);
   MPI_Comm_rank(MPI_COMM_WORLD,&myrank);
 
   num = myrank+1;
   for(i=0;i<100;i++)
      for(j=0;j<150;j++)
         sendarray[i][j] = -1;
   for(i=0;i<num;i++)
      sendarray[i][myrank] = myrank;
 
   root = 0;
   MPI_Gather(&num,1,MPI_INT,rcounts,1,MPI_INT,root,MPI_COMM_WORLD);
   if(myrank == root) {
      displs[0] = 0;
      for(i=1;i<gsize;i++)  displs[i] = displs[i-1] + rcounts[i-1];
      rbuf = (int *) malloc(sizeof(int)*gsize*(gsize+1)/2);
   }
   disp[0] = 0;  disp[1] = 150*sizeof(int);
   type[0] = MPI_INT;  type[1] = MPI_UB;
   blocklen[0] = 1;  blocklen[1] = 1;
   MPI_Type_struct(2,blocklen,disp,type,&stype);
   MPI_Type_commit(&stype);
   sptr = &sendarray[0][myrank];
   MPI_Gatherv(sptr,num,stype,rbuf,rcounts,displs,MPI_INT,root,MPI_COMM_WORLD);
   if(myrank == 0)  {
      for(t = i=0;i<gsize;i++)
         for(j=0;j<i+1;j++,t++) {
            if(rbuf[t] != i) {
		printf("ERROR: rbuf[%d] = %d instead of %d\n", t, rbuf[t], i);
            }
         }
   }
   if (myrank == root)
     free(rbuf);
   MPI_Type_free( &stype );
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
   return 0;
}
