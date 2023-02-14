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
 
int main(argc,argv)
int argc;
char**argv;
{
   int i,size,myself,ranks1[16],ranks2[16];
   int ranges[10][3];
   MPI_Group group,newgroup, groups[20];
   int cnt = 0;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&myself);
   MPI_Comm_group(MPI_COMM_WORLD,&group);
   groups[cnt++] = group;
   MPI_Group_size(group,&size);
   if(size != 8) {
       if (myself == 0) printf("MUST RUN WITH 8 TASKS\n");
       MPI_Finalize();
       exit(0);
   }
 
   ranges[0][0] = 1; ranges[0][1] = 4; ranges[0][2] = 1;
   ranges[1][0] = 5; ranges[1][1] = 8; ranges[1][2] = 2;
   MPI_Group_range_incl(group,2,ranges,&newgroup);
   groups[cnt++] = newgroup;
   MPI_Group_size(newgroup,&size);
   if(size != 6)  printf("ERROR: Size = %d, should be 6\n",size);
   for(i=0;i<6;i++)  ranks1[i] = i;
   MPI_Group_translate_ranks(newgroup,6,ranks1,group,ranks2);
   if(ranks2[0] != 1 || ranks2[1] != 2 || ranks2[2] != 3 || ranks2[3] != 4
      || ranks2[4] != 5 || ranks2[5] != 7)
      printf("ERROR: Wrong ranks %d %d %d %d %d %d\n",ranks2[0],ranks2[1],
              ranks2[2],ranks2[3],ranks2[4],ranks2[5]);

   MPI_Group_range_excl(group,2,ranges,&newgroup);
   groups[cnt++] = newgroup;
   MPI_Group_size(newgroup,&size);
   if(size != 2)  printf("ERROR: Size = %d, should be 2\n",size);
   MPI_Group_translate_ranks(newgroup,2,ranks1,group,ranks2);
   if(ranks2[0] != 0 || ranks2[1] != 6)
      printf("ERROR: Wrong ranks %d %d\n",ranks2[0],ranks2[1]);

   ranges[0][0] = 6; ranges[0][1] = 0; ranges[0][2] = -3;
   MPI_Group_range_incl(group,1,ranges,&newgroup);
   groups[cnt++] = newgroup;
   MPI_Group_size(newgroup,&size);
   if(size != 3)  printf("ERROR: Size = %d, should be 3\n",size);
   for(i=0;i<3;i++)  ranks1[i] = i;
   MPI_Group_translate_ranks(newgroup,3,ranks1,group,ranks2);
   if(ranks2[0] != 6 || ranks2[1] != 3 || ranks2[2] != 0)
      printf("ERROR: Wrong ranks %d %d %d\n",ranks2[0],ranks2[1],ranks2[2]);

   MPI_Group_range_excl(group,1,ranges,&newgroup);
   groups[cnt++] = newgroup;
   MPI_Group_size(newgroup,&size);
   if(size != 5)  printf("ERROR: Size = %d, should be 5\n",size);
   MPI_Group_translate_ranks(newgroup,5,ranks1,group,ranks2);
   if(ranks2[0] != 1 || ranks2[1] != 2 || ranks2[2] != 4 || ranks2[3] != 5 || ranks2[4] != 7)
      printf("ERROR: Wrong ranks %d %d %d %d %d\n",ranks2[0],ranks2[1],ranks2[2],ranks2[3],ranks2[4]);

   MPI_Barrier(MPI_COMM_WORLD);
   for (i=0; i<cnt; i++) 
       MPI_Group_free( &groups[i] );
   MPI_Finalize();
return 0;
}
