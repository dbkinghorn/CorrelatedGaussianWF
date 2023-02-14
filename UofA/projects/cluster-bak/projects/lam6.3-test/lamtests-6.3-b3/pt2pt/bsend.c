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
    /* Note that the buffer sizes must include the BSEND_OVERHEAD;
       these values are probably sizeof(int) too large */
   int *oldbuf,me,i,size,rc;
   int data[100000+MPI_BSEND_OVERHEAD],
       buf1[1000+MPI_BSEND_OVERHEAD],
       buf2[100000+MPI_BSEND_OVERHEAD];
   MPI_Status status;
 
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);
 
   if(me==0) {
      MPI_Buffer_attach(buf1,sizeof(buf1));

      /* Need to be purify clean */

      for (i = 0; i < 100000; i++)
	data[i] = 0;

      MPI_Bsend(data,1000,MPI_INT,1,1,MPI_COMM_WORLD);
 
      MPI_Buffer_detach(&oldbuf,&size);
      if(oldbuf != buf1)
         printf("ERROR in MPI_Buffer_detach, incorrect buffer returned\n");
      if(size != sizeof(buf1))
         printf("ERROR in MPI_Buffer_detach, incorrect size returned\n");
 
      MPI_Buffer_attach(buf2,sizeof(buf2));
      MPI_Bsend(data,1001,MPI_INT,1,1,MPI_COMM_WORLD);
 
      MPI_Barrier(MPI_COMM_WORLD);
 
/* test to see if large array is REALLY being buffered */
      for(i=0;i<100000;i++)  data[i] = 1;
      MPI_Bsend(data,100000,MPI_INT,1,1,MPI_COMM_WORLD);
      MPI_Recv(data,100000,MPI_INT,1,2,MPI_COMM_WORLD,&status);
      for(i=0;i<100000;i++)
         if(data[i] != 2)  printf("ERROR, incorrect data value, task 0\n");
 
      rc = MPI_Bsend(data,100000+1+MPI_BSEND_OVERHEAD,
		     MPI_INT,1,1,MPI_COMM_WORLD);
      if(rc == MPI_SUCCESS)  printf("ERROR: Buffer overflow not detected\n");
   } else if(me == 1) {
      MPI_Recv(data,1000,MPI_INT,0,1,MPI_COMM_WORLD,&status);
      MPI_Recv(data,1001,MPI_INT,0,1,MPI_COMM_WORLD,&status);
 
      MPI_Barrier(MPI_COMM_WORLD);
      MPI_Buffer_attach(buf2,sizeof(buf2));
 
/* test to see if large array is REALLY being buffered */
      for(i=0;i<100000;i++)  data[i] = 2;
      MPI_Bsend(data,100000,MPI_INT,0,2,MPI_COMM_WORLD);
      MPI_Recv(data,100000,MPI_INT,0,1,MPI_COMM_WORLD,&status);
      for(i=0;i<100000;i++)
         if(data[i] != 1)  printf("ERROR, incorrect data value, task 1\n");
   }
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
return 0;
}
