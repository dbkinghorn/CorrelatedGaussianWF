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
   int me,i,cnt,data,src,tag,tasks;
   MPI_Comm comm;
   MPI_Status status;
 
   MPI_Init(&argc,&argv);
   comm = MPI_COMM_WORLD;
   MPI_Comm_rank(comm,&me);
   MPI_Comm_size(comm,&tasks);

/* probe for specific source, tag */
   if(me) {
      data = me;
      MPI_Send(&data,1,MPI_INT,0,me,comm);
   } else  {
      for(i=1;i<tasks;i++)  {
         MPI_Probe(i,i,comm,&status);
         src = status.MPI_SOURCE;
         if(src != i)
            printf("ERROR in MPI_Probe(1): src = %d, should be %d\n",src,i);
         tag = status.MPI_TAG;
         if(tag != i)
            printf("ERROR in MPI_Probe(1): tag = %d, should be %d\n",tag,i);
         MPI_Get_count(&status,MPI_INT,&cnt);
         if(cnt != 1) printf("ERROR in MPI_Probe(1): cnt = %d, should be %d\n",cnt,1);
         MPI_Recv(&data,cnt,MPI_INT,src,tag,comm,&status);
         if(data != i) printf("ERROR in MPI_Recv(1), data = %d, should be %d\n",data,i);
      }
   }
 
/* probe for specific source, tag = MPI_ANY_TAG */
   if(me) {
      data = me;
      MPI_Send(&data,1,MPI_INT,0,me,comm);
   } else  {
      for(i=1;i<tasks;i++)  {
         MPI_Probe(i,MPI_ANY_TAG,comm,&status);
         src = status.MPI_SOURCE;
         if(src != i)
            printf("ERROR in MPI_Probe(2): src = %d, should be %d\n",src,i);
         tag = status.MPI_TAG;
         if(tag != i)
            printf("ERROR in MPI_Probe(2): tag = %d, should be %d\n",tag,i);
         MPI_Get_count(&status,MPI_INT,&cnt);
         if(cnt != 1) printf("ERROR in MPI_Probe(2): cnt = %d, should be %d\n",cnt,1);
         MPI_Recv(&data,cnt,MPI_INT,src,tag,comm,&status);
         if(data != i) printf("ERROR in MPI_Recv(2), data = %d, should be %d\n",data,i);
      }
   }
 
/* probe for specific tag, source = MPI_ANY_SOURCE */
   if(me) {
      data = me;
      MPI_Send(&data,1,MPI_INT,0,me,comm);
   } else  {
      for(i=1;i<tasks;i++)  {
         MPI_Probe(MPI_ANY_SOURCE,i,comm,&status);
         src = status.MPI_SOURCE;
         if(src != i)
            printf("ERROR in MPI_Probe(3): src = %d, should be %d\n",src,i);
         tag = status.MPI_TAG;
         if(tag != i)
            printf("ERROR in MPI_Probe(3): tag = %d, should be %d\n",tag,i);
         MPI_Get_count(&status,MPI_INT,&cnt);
         if(cnt != 1) printf("ERROR in MPI_Probe(3): cnt = %d, should be %d\n",cnt,1);
         MPI_Recv(&data,cnt,MPI_INT,src,tag,comm,&status);
         if(data != i) printf("ERROR in MPI_Recv(3), data = %d, should be %d\n",data,i);
      }
   }
 
/* probe for source = MPI_ANY_SOURCE, tag = MPI_ANY_TAG */
   if(me) {
      data = me;
      MPI_Send(&data,1,MPI_INT,0,me,comm);
   } else  {
      for(i=1;i<tasks;i++)  {
         MPI_Probe(MPI_ANY_SOURCE,MPI_ANY_TAG,comm,&status);
         src = status.MPI_SOURCE;
         tag = status.MPI_TAG;
         if(src != tag)
            printf("ERROR in MPI_Probe(4): tag = %d, should be %d\n",tag,src);
         MPI_Get_count(&status,MPI_INT,&cnt);
         if(cnt != 1) printf("ERROR in MPI_Probe(4): cnt = %d, should be %d\n",cnt,1);
         MPI_Recv(&data,cnt,MPI_INT,src,tag,comm,&status);
         if(data != src)
            printf("ERROR in MPI_Recv(4), data = %d, should be %d\n",data,src);
      }
   }
 
   MPI_Barrier(comm);
   MPI_Finalize();
return 0;
}
