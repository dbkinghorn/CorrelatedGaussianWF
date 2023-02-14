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
   int i,me,datain,dataout,rc,class;
   MPI_Status status;
   MPI_Request request;
   MPI_Comm comm;

#define ITER 100

   MPI_Init(&argc,&argv);
   comm = MPI_COMM_WORLD;
   MPI_Comm_rank(comm,&me);

   if(me < 2)  {
   if(me == 0)  {
      for(i=0;i<ITER;i++)  {
         dataout = i;
         MPI_Isend(&dataout,1,MPI_INT,1,0,comm,&request);
         MPI_Request_free(&request);
         if(request != MPI_REQUEST_NULL) 
            printf("ERROR in MPI_Request_free, request not set to NULL\n");
         MPI_Recv(&datain,1,MPI_INT,1,0,comm,&status);
         if(datain != i)  
            printf("ERROR in MPI_Recv, datain = %d, should be %d\n",datain,i); 
      }
   } else if(me == 1) {
      MPI_Recv(&datain,1,MPI_INT,0,0,comm,&status);
      for(i=0;i<ITER-1;i++)  {
         dataout = i;
         MPI_Isend(&dataout,1,MPI_INT,0,0,comm,&request);
         MPI_Request_free(&request);
         if(request != MPI_REQUEST_NULL) 
            printf("ERROR in MPI_Request_free, request not set to NULL\n");
         MPI_Recv(&datain,1,MPI_INT,0,0,comm,&status);
      }
      dataout = ITER-1;
      MPI_Send(&dataout,1,MPI_INT,0,0,comm);
   }

   MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);

   /* The standard allows MPI_Request_free; in fact, this test MUST 
      be successful (see the discussion of MPI_Request_free in the standard)
      In fact, the example makes the use that is tested here. */
   MPI_Irecv(&datain,1,MPI_INT,me,0,MPI_COMM_WORLD,&request);
   rc = MPI_Request_free(&request);

   MPI_Error_class(rc,&class);
   if(class != MPI_SUCCESS)  
       printf("ERROR: Attempt to free receive not allowed\n");

   MPI_Recv_init(&dataout,1,MPI_INT,me,1,MPI_COMM_WORLD,&request);
   rc = MPI_Request_free(&request);
   if(rc != MPI_SUCCESS)  
       printf("ERROR: not able to free inactive receive request\n");

   MPI_Recv_init(&dataout,1,MPI_INT,me,2,MPI_COMM_WORLD,&request);
   MPI_Start(&request);
   rc = MPI_Request_free(&request);
   MPI_Error_class(rc,&class);
   if(class != MPI_SUCCESS)  
       printf("ERROR: Attempt to free active persistent receive not allowed\n");

   }

   MPI_Send(&dataout,1,MPI_INT,me,0,MPI_COMM_WORLD);
   MPI_Send(&dataout,1,MPI_INT,me,2,MPI_COMM_WORLD);

   MPI_Barrier(comm);
   MPI_Finalize();
return 0;
}
