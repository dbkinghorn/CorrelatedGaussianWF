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

void Errors_warn( comm, code )
MPI_Comm *comm;
int      *code;
{
  char buf[MPI_MAX_ERROR_STRING];
  int  myid, result_len; 
  
  MPI_Comm_rank( MPI_COMM_WORLD, &myid );
  MPI_Error_string( *code, buf, &result_len );
  fprintf( stderr, "%d : %s\n", myid, buf );
}
 
int main(argc,argv)
int argc;
char**argv;
{
   int *oldbuf,me,i,size,flag;
   int data[100000],buf1[10000+MPI_BSEND_OVERHEAD],buf2[100000];
   MPI_Status status;
   MPI_Request request;
   MPI_Errhandler warn;
 
   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Errhandler_create((MPI_Handler_function*) Errors_warn,&warn);
   MPI_Errhandler_set(MPI_COMM_WORLD,warn);
 
   if(me==0) {
      MPI_Buffer_detach(&oldbuf,&size);
      MPI_Buffer_attach(buf2,sizeof(buf2));
      MPI_Buffer_detach(&oldbuf,&size);

      MPI_Buffer_attach(buf1,sizeof(buf1));
      for(i=0;i<10000;i++)  data[i] = i;
      MPI_Ibsend(data,10000,MPI_INT,1,1,MPI_COMM_WORLD,&request);
 
      MPI_Buffer_detach(&oldbuf,&size);
      for(i=0;i<10000;i++)  buf1[i] = 0;
      MPI_Test(&request,&flag,&status);
      if(!flag) printf("ERROR\n");
   } else if(me == 1) {
      for(i=0;i<4000000;i++);
      MPI_Recv(data,10000,MPI_INT,0,1,MPI_COMM_WORLD,&status);
      for(i=0;i<10000;i++)
         if(data[i] != i) { printf("ERROR\n"); break; }
   }
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Errhandler_free(&warn);
   MPI_Finalize();
return 0;
}
