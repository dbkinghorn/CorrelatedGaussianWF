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
 
int newsize,flag,sum,me,size,color,key,local_lead,remote_lead,newme,newsum,othersum;
MPI_Comm comm,intercomm,mergecomm;
MPI_Status status;
MPI_Group newgid;
 
void inter_tests()
{
   MPI_Comm_test_inter(intercomm,&flag);
   if(flag != 1)
      printf("ERROR in MPI_Comm_test_inter: flag = %d, should be %d\n",flag,1);
 
   MPI_Comm_remote_size(intercomm,&newsize);
   if(newsize != size/2)
      printf("ERROR in MPI_Comm_remote_size: size = %d, should be %d\n",newsize,size/2);
 
   MPI_Comm_remote_group(intercomm,&newgid);
   MPI_Group_size(newgid,&newsize);
   if(newsize != size/2)
      printf("ERROR in MPI_Comm_remote_group: size = %d, should be %d\n",newsize,size/2);
 
   newsum = sum;
   MPI_Sendrecv_replace(&newsum,1,MPI_INT,newme,7,newme,7,intercomm,&status);
   othersum = size/2*(size/2-1);
   if(me%2 == 0)  othersum += size/2;
   if(othersum != newsum)
      printf("ERROR in Intercomm_create, sum = %d, should be %d\n",othersum,newsum);
 
   MPI_Intercomm_merge(intercomm,color,&mergecomm);
   MPI_Allreduce(&me,&newsum,1,MPI_INT,MPI_SUM,mergecomm);
   if(newsum != size*(size-1)/2)
      printf("ERROR in MPI_Intercomm_merge: sum = %d, should be %d\n",newsum,size*(size-1)/2);
   MPI_Comm_free( &mergecomm );
   MPI_Group_free( &newgid );
}
 
int
main(argc,argv)
int argc;
char**argv;
{
   MPI_Comm comm1, comm2;
   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&size);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
 
   if(size%2) {
       printf("MUST RUN WITH EVEN NUMBER OF TASKS\n");
       MPI_Finalize();
       exit(0);
   }
 
   key = me;
   color = me%2;
   MPI_Comm_split(MPI_COMM_WORLD,color,key,&comm);
   comm1 = comm;
   MPI_Comm_test_inter(comm,&flag);
   if(flag != 0)
      printf("ERROR in MPI_Comm_test_inter: flag = %d, should be %d\n",flag,0);
   MPI_Comm_rank(comm,&newme);

   MPI_Allreduce(&me,&sum,1,MPI_INT,MPI_SUM,comm);
 
   local_lead = 0;
   remote_lead = color ? 0 : 1;
   MPI_Intercomm_create(comm,local_lead,MPI_COMM_WORLD,remote_lead,5,&intercomm);
   comm2 = intercomm;
   inter_tests();
 
   MPI_Comm_dup(intercomm,&comm);
   intercomm = comm;
   inter_tests();
 
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Comm_free( &comm );
   MPI_Comm_free( &comm1 );
   MPI_Comm_free( &comm2 );
   MPI_Finalize();
return 0;
}
