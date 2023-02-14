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
   int tasks,me,size,rank,i,result,rc,class;
   int ranks1[128],ranks2[128];
   MPI_Group group1,group2,group3,newgroup;
   MPI_Group groups[20];
   int       cnt = 0;
   MPI_Comm newcomm;
 
   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   if(tasks < 2) {
       printf("MUST HAVE AT LEAST 2 TASKS\n");
       MPI_Finalize();
       exit(0);
   }
 
   MPI_Comm_group(MPI_COMM_WORLD,&group1);
   groups[cnt++] = group1;
   MPI_Group_size(group1,&size);
   if(size != tasks)  printf("ERROR in MPI_Group_size, size = %d, should be %d\n",size,tasks);
   MPI_Group_rank(group1,&rank);
   if(rank != me)  printf("ERROR in MPI_Group_rank, rank = %d, should be %d\n",rank,me);
   for(i=0;i<tasks/2;i++)  ranks1[i] = i;
   MPI_Group_incl(group1,tasks/2,ranks1,&newgroup);
   /* newgroup freed below */
   MPI_Group_size(newgroup,&size);
   if(size != tasks/2)  printf("ERROR in MPI_Group_size, size = %d, should be %d\n",size,tasks/2);
   MPI_Group_compare(newgroup,newgroup,&result);
   if(result != MPI_IDENT)  printf("ERROR in MPI_Group_compare (1), result = %d, should be %d\n", result, (int) MPI_IDENT);
   MPI_Group_compare(newgroup,group1,&result);
   if(result != MPI_UNEQUAL)  printf("ERROR in MPI_Group_compare (2), result = %d, should be %d\n", result, (int) MPI_UNEQUAL);
   MPI_Group_union(group1,newgroup,&group2);
   groups[cnt++] = group2;
   MPI_Group_compare(group1,group2,&result);
   if(result != MPI_IDENT)  printf("ERROR in MPI_Group_compare (3), result = %d, should be %d\n", result, (int) MPI_IDENT);
   MPI_Group_intersection(newgroup,group1,&group2);
   groups[cnt++] = group2;
   MPI_Group_compare(group2,newgroup,&result);
   if(result != MPI_IDENT)  printf("ERROR in MPI_Group_compare (4), result = %d, should be %d\n", result, (int) MPI_IDENT);
   MPI_Group_difference(group1,newgroup,&group2);
   groups[cnt++] = group2;
   MPI_Group_size(group2,&size);
   if(size != tasks/2)  printf("ERROR in MPI_Group_size, size = %d, should be %d\n",size,tasks/2);
   for(i=0;i<size;i++)  ranks1[i] = i;
   MPI_Group_translate_ranks(group2,size,ranks1,group1,ranks2);
   for(i=0;i<size;i++) {
      if(ranks2[i] != tasks/2 + i)  printf("ERROR in MPI_Group_translate_ranks\n");
   }
   MPI_Comm_create(MPI_COMM_WORLD,newgroup,&newcomm);
   if(newcomm != MPI_COMM_NULL)  {
      MPI_Comm_group(newcomm,&group3);
      groups[cnt++] = group3;
      if(group3 != newgroup) 
#if 0
	/* Bonk -- unfortunately, this is not portable to
	   architectures/OS's where sizeof(int) < sizeof(void*) */
	printf("ERROR in MPI_Comm_group, group = %d, should be %d\n", (int) group2, (int) newgroup);
#else
	printf("ERROR in MPI_Comm_group, group is not what it should be\n");
#endif
   }
   MPI_Group_excl(group1,tasks/2,ranks1,&group3);
   groups[cnt++] = group3;
   MPI_Group_compare(group2,group3,&result);
   if(result != MPI_IDENT)  printf("ERROR in MPI_Group_compare (5) , result = %d, should be %d\n",result, (int) MPI_IDENT);
 
   for(i=0;i<tasks;i++)  ranks1[tasks-1-i] = i;
   MPI_Group_incl(group1,tasks,ranks1,&group3);
   groups[cnt++] = group3;

   MPI_Group_compare(group1,group3,&result);
   if(result != MPI_SIMILAR)  printf("ERROR in MPI_Group_compare (6), result = %d, should be %d\n",result, (int) MPI_SIMILAR);
 
   group3 = newgroup;
   MPI_Group_free(&newgroup);
   if(newgroup != MPI_GROUP_NULL)  
#if 0
     /* Bonk -- unfortunately, this is not portable to
        architectures/OS's where sizeof(int) < sizeof(void*) */
     printf("ERROR in MPI_Group_free, group = %d, should be %d\n", (int) newgroup, (int) MPI_GROUP_NULL);
#else
     printf("ERROR in MPI_Group_free, group is not MPI_GROUP_NULL\n");
#endif
   if(newcomm != MPI_COMM_NULL)  {
      MPI_Comm_free(&newcomm);
      if(newcomm != MPI_COMM_NULL)  
#if 0
     /* Bonk -- unfortunately, this is not portable to
        architectures/OS's where sizeof(int) < sizeof(void*) */
	printf("ERROR in MPI_Comm_free, comm = %d, should be %d\n", (int) newcomm, (int) MPI_COMM_NULL);
#else
	printf("ERROR in MPI_Comm_free, comm is not MPI_COMM_NULL\n");
#endif
   }
   MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);
   rc = MPI_Comm_create(MPI_COMM_WORLD,newgroup,&newcomm);  
   MPI_Error_class(rc,&class);
   if(class != MPI_ERR_GROUP)  
#if 0
     /* Bonk -- unfortunately, this is not portable to
        architectures/OS's where sizeof(int) < sizeof(void*) */
     printf("ERROR in MPI_Group_free, group %d not freed\n", (int) newgroup);
#else
     printf("ERROR in MPI_Group_free, group not freed\n");
#endif
   MPI_Barrier(MPI_COMM_WORLD);

   /* Note newcomm above is not actually created... */
   for (i=0; i<cnt; i++) 
       MPI_Group_free( &groups[i] );
   MPI_Finalize();
return 0;
}
