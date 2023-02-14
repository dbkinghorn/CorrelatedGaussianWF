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
 
int me,tasks,bytes,i,data[1000],buf[10000];
MPI_Request req[1000];
MPI_Status stats[1000];
 
void wstart()
{
   for(i=0;i<tasks;i++)  data[i] = -1;
 
   MPI_Startall(2*tasks,req);
   MPI_Waitall(2*tasks,req,stats);
 
   for(i=0;i<tasks;i++)
      if(data[i] != i)
         printf("ERROR in Startall: data is %d, should be %d\n",data[i],i);
   for(i=1;i<2*tasks;i+=2) {
      MPI_Get_count(&stats[i],MPI_BYTE,&bytes);
      if(bytes != 4)
         printf("ERROR in Waitall: bytes = %d, should be 4\n",bytes);
   }
   for (i=0; i<2*tasks; i++)
       MPI_Request_free( &req[i] );
}
 
int main(argc,argv)
int argc;
char**argv;
{
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);
   MPI_Buffer_attach(buf,sizeof(buf));
 
   for(i=0;i<tasks;i++)  {
      MPI_Send_init(&me,1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i]);
      MPI_Recv_init(&data[i],1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i+1]);
   }
   wstart();

   for(i=0;i<tasks;i++)  {
      MPI_Ssend_init(&me,1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i]);
      MPI_Recv_init(&data[i],1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i+1]);
   }
   wstart();

/*
   for(i=0;i<tasks;i++)  {
      MPI_Rsend_init(&me,1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i]);
      MPI_Recv_init(&data[i],1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i+1]);
   }
   wstart();

*/
   for(i=0;i<tasks;i++)  {
      MPI_Bsend_init(&me,1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i]);
      MPI_Recv_init(&data[i],1,MPI_INT,i,1,MPI_COMM_WORLD,&req[2*i+1]);
   }
   wstart();
 
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
return 0;
}
