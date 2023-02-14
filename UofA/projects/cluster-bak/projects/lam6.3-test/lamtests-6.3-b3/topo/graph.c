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
   static int index[]={2,3,4,6},edges[]={1,3,0,3,0,2};
   int me,tasks,reorder;
   int i,num,start,type,nnodes,nedges,neighbors[4];
   MPI_Comm comm;
 

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);
   if(tasks != 4) {
       printf("MUST RUN WITH 4 TASKS\n");
       MPI_Finalize();
       exit(0);
   }

   reorder = 0;
   MPI_Graph_create(MPI_COMM_WORLD,4,index,edges,reorder,&comm);
   
   MPI_Topo_test(comm,&type);
   if(type != MPI_GRAPH)  printf("ERROR in MPI_Topo_test, type = %d, should be %d (GRAPH)\n",type,MPI_GRAPH);

   MPI_Graphdims_get(comm,&nnodes,&nedges);
   if(nnodes != 4 || nedges != 6)
      printf("ERROR in MPI_Graphdims_get, nnodes,nedges = %d %d, should be %d %d\n",nnodes,nedges,4,6);

   MPI_Graph_get(comm,4,6,index,edges);
   if(index[0] != 2 || index[1] != 3 || index[2] != 4 || index[3] != 6)
      printf("ERROR in MPI_Graph_get: index = %d %d %d %d, should be %d %d %d %d\n",index[0],index[1],index[2],index[3],2,3,4,6);
   if(edges[0] != 1 || edges[1] != 3 || edges[2] != 0 || edges[3] != 3 || edges[4] != 0 || edges[5] != 2) 
      printf("ERROR in MPI_Graph_get: edges = %d %d %d %d %d %d, should be %d %d %d %d %d %d\n",edges[0],edges[1],edges[2],edges[3],edges[4],edges[5],1,3,0,3,0,2);

   MPI_Graph_neighbors_count(comm,me,&nnodes);
   if(nnodes != 1 + (me==0 || me==3))
      printf("ERROR in MPI_Graph_neighbors_count: count = %d, should be %d\n",nnodes,1+((me==0)|(me==3)));

   MPI_Graph_neighbors(comm,me,4,neighbors);  
   start = (me==0) ? 0 : index[me-1];
   num = (me==0) ? index[0] : index[me] - start;
   for(i=0;i<num;i++)  {
      if(neighbors[i] != edges[start+i])
         printf("ERROR in MPI_Graph_neighbors: wrong neighbor on task %d index %d, neighbor = %d, should be %d\n",me,i,neighbors[i],edges[start+i]);
   }

   MPI_Barrier(comm);

   MPI_Comm_free( &comm );
   MPI_Finalize();
return 0;
}
