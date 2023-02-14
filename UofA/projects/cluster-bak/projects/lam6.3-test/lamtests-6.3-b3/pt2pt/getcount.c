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
   int me,data[100],count;
   MPI_Status status;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   
   if(me == 0)  {
      MPI_Send(data,5,MPI_BYTE,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_CHAR,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_INT,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_FLOAT,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_DOUBLE,1,1,MPI_COMM_WORLD);
/*      MPI_Send(data,5,MPI_LONG_DOUBLE,1,1,MPI_COMM_WORLD);*/
      MPI_Send(data,5,MPI_SHORT,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_LONG,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_PACKED,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_UNSIGNED_CHAR,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_UNSIGNED_SHORT,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_UNSIGNED,1,1,MPI_COMM_WORLD);
      MPI_Send(data,5,MPI_UNSIGNED_LONG,1,1,MPI_COMM_WORLD);
   } else if(me == 1)  {
      MPI_Recv(data,5,MPI_BYTE,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_BYTE,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_CHAR,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_CHAR,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_INT,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_INT,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_FLOAT,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_FLOAT,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_DOUBLE,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_DOUBLE,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
#ifdef FOO
      MPI_Recv(data,5,MPI_LONG_DOUBLE,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_LONG_DOUBLE,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
#endif
      MPI_Recv(data,5,MPI_SHORT,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_SHORT,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_LONG,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_LONG,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_PACKED,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_PACKED,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_UNSIGNED_CHAR,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_UNSIGNED_CHAR,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_UNSIGNED_SHORT,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_UNSIGNED_SHORT,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_UNSIGNED,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_UNSIGNED,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
      MPI_Recv(data,5,MPI_UNSIGNED_LONG,0,1,MPI_COMM_WORLD,&status);
      MPI_Get_count(&status,MPI_UNSIGNED_LONG,&count);
      if(count != 5) printf("ERROR in MPI_Get_count, count = %d, should be %d\n",count,5);
   }
   MPI_Barrier(MPI_COMM_WORLD);
MPI_Finalize();
return 0;
}  
