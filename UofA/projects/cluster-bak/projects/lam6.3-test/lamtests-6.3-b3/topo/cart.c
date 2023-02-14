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
#define MAXDIMS 10
 
int main(argc,argv)
int argc;
char**argv;
{
   int dims[MAXDIMS],tasks,me,periods[MAXDIMS],type,ndims,coords[MAXDIMS];
   int rank,src,dest;
   MPI_Group gid;
   MPI_Comm comm;
   MPI_Comm comms[20];
   int      cnt=0, i;
 
   MPI_Init(&argc,&argv);
   MPI_Comm_group(MPI_COMM_WORLD,&gid);
   MPI_Group_size(gid,&tasks);
   if(tasks != 6)  {
       printf("MUST RUN WITH 6 TASKS\n");
       MPI_Finalize();
       exit(0);
   }
 
/* test non-periodic topology */
 
   dims[0] = 0;  dims[1] = 0;
   MPI_Dims_create(tasks,2,dims);
   if(dims[0] != 3 || dims[1] != 2)
      printf("ERROR in MPI_Dims_create, dims = %d,%d, should be %d %d\n",dims[0],dims[1],3,2);
   periods[0] = 0;
   periods[1] = 0;
   MPI_Cart_create(MPI_COMM_WORLD,2,dims,periods,0,&comm);
   comms[cnt++] = comm;
   MPI_Comm_rank(comm,&me);
 
   MPI_Topo_test(comm,&type);
   if(type != MPI_CART)  printf("ERROR in MPI_Topo_test, type = %d, should be %d\n",type,MPI_CART);
 
   MPI_Cartdim_get(comm,&ndims);
   if(ndims != 2) printf("ERROR in MPI_Cartdim_get, ndims = %d, should be %d\n",ndims,2);
 
   MPI_Cart_get(comm,MAXDIMS,dims,periods,coords);
   if(dims[0] != 3||dims[1] != 2)
      printf("ERROR in MPI_Cart_get, dims = %d %d, should be %d %d\n",dims[0],dims[1],3,2);
   if(periods[0] != 0 || periods[1] != 0) printf("WRONG PERIODS!\n");
   if(coords[0] != me/2 || coords[1] != me%2)
      { printf("ERROR in MPI_Cart_get, coords = %d %d, should be %d %d\n",coords[0],coords[1],me/2,me%2); exit(0); }
 
   MPI_Cart_rank(comm,coords,&rank);
   if(rank != me)  printf("ERROR in MPI_Cart_rank, rank = %d, should be %d\n",rank,me);
 
   MPI_Cart_coords(comm,rank,MAXDIMS,coords);
   if(coords[0] != me/2 || coords[1] != me%2)
      { printf("ERROR in MPI_Cart_coords, coords = %d %d, should be %d %d\n",coords[0],coords[1],me/2,me%2); exit(0); }
 
   MPI_Cart_shift(comm,0,5,&src,&dest);
   if(src != MPI_PROC_NULL || dest != MPI_PROC_NULL)
      printf("ERROR in MPI_Cart_shift, src/dest = %d %d, should be %d %d\n",src,dest,MPI_PROC_NULL,MPI_PROC_NULL);
 
   MPI_Cart_shift(comm,0,1,&src,&dest);
   if(me/2<2 && dest != me+2)
      printf("ERROR in MPI_Cart_shift, dest = %d, should be %d\n",dest,me+2);
 
   if(me/2>0 && src != me-2)
      printf("ERROR in MPI_Cart_shift, src = %d, should be %d\n",src,me-2);
 
   MPI_Cart_shift(comm,1,-1,&src,&dest);
   if(me%2 && dest != me-1)
      printf("ERROR in MPI_Cart_shift, dest = %d, should be %d\n",dest,me-1);
   if(me%2 && src != MPI_PROC_NULL)
      printf("ERROR in MPI_Cart_shift, src = %d, should be %d\n",src,MPI_PROC_NULL);
   if(me%2==0 && src != me+1)
      printf("ERROR in MPI_Cart_shift, src = %d, should be %d\n",src,me+1);
   if(me%2==0 && dest != MPI_PROC_NULL)
      printf("ERROR in MPI_Cart_shift, dest = %d, should be %d\n",dest,MPI_PROC_NULL);
 
/* test periodic topology */
 
   dims[0] = 2;  dims[1] = 0;
   MPI_Dims_create(tasks,2,dims);
   if(dims[0] != 2 || dims[1] != 3)
      printf("ERROR in MPI_Dims_create, dims = %d,%d, should be %d %d\n",dims[0],dims[1],2,3);
 
   periods[0] = 1;
   periods[1] = 1;
   MPI_Cart_create(MPI_COMM_WORLD,2,dims,periods,0,&comm);
   comms[cnt++] = comm;
   MPI_Comm_rank(comm,&me);
   coords[0] = me/3; coords[1] = me%3;
   MPI_Cart_rank(comm,coords,&rank);
   if(rank != me)  printf("ERROR in MPI_Cart_rank, rank = %d, should be %d\n",rank,me);
 
   MPI_Cart_coords(comm,rank,MAXDIMS,coords);
   if(coords[0] != me/3 || coords[1] != me%3)
      printf("ERROR in MPI_Cart_coords, coords = %d %d, should be %d %d\n",coords[0],coords[1],me/3,me%3);
 
   MPI_Cart_shift(comm,0,5,&src,&dest);
   if(src != (me+3)%6 || dest != (me+3)%6)
      printf("ERROR in MPI_Cart_shift, src/dest = %d %d, should be %d %d\n",src,dest,me+3,me+3);
 
   MPI_Cart_shift(comm,1,-1,&src,&dest);
   if(dest != (me-1)+3*(me%3==0))
      printf("ERROR in MPI_Cart_shift, dest = %d, should be %d\n",dest,(me-1+3)%3);
   if(src != (me+1)-3*(me%3==2))
      printf("ERROR in MPI_Cart_shift, src = %d, should be %d\n",src,(me+1+3)%3);

   dims[0] = 1; 
   MPI_Cart_create(MPI_COMM_WORLD,2,dims,periods,0,&comm);
   comms[cnt++] = comm;

   MPI_Barrier(MPI_COMM_WORLD);

   for (i=0; i<cnt; i++)
       if (comms[i] != MPI_COMM_NULL) MPI_Comm_free( &comms[i] );
   MPI_Group_free( &gid );
   MPI_Finalize();
return 0;
}
