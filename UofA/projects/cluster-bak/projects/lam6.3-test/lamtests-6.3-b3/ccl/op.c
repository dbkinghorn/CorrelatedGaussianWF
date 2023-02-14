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

typedef struct {
   int data;
   int flag;
} mydt;

void less(in_p,inout_p,count,dt)
void *in_p, *inout_p;
int *count;
MPI_Datatype *dt;
{
   mydt *in = in_p;
   mydt *inout = inout_p;
   int i;

   if(*dt != MPI_2INT)  printf("ERROR in less: wrong data type\n");

   for(i=0;i<*count;i++)
      inout[i].flag = (in[i].data < inout[i].data) && in[i].flag && inout[i].flag;
}

int
main(argc,argv)
int argc;
char**argv;
{
   int me,tasks,root,commute,rc,class;
   mydt info,result;
   MPI_Op temp;
   MPI_Op op;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);

   commute = 0;
   MPI_Op_create(less,commute,&op);

   info.data = me;
   info.flag = 1;

   root = 0;
   MPI_Reduce(&info,&result,1,MPI_2INT,op,root,MPI_COMM_WORLD);
   if(me == root)
      if(result.flag != 1)
         printf("ERROR in MPI_Reduce(1): result = %d, should be 1\n",result.flag);

   root = tasks/2;
   MPI_Reduce(&info,&result,1,MPI_2INT,op,root,MPI_COMM_WORLD);
   if(me == root)
      if(result.flag != 1)
         printf("ERROR in MPI_Reduce(2): result = %d, should be 1\n",result.flag);

   info.data = tasks - me;
   root = tasks-1;
   MPI_Reduce(&info,&result,1,MPI_2INT,op,root,MPI_COMM_WORLD);
   if(me == root)
      if(result.flag != 0)
         printf("ERROR in MPI_Reduce(3): result = %d, should be 0\n",result.flag);

   info.data = me;
   if(me == 0)  info.data = 1;
   MPI_Reduce(&info,&result,1,MPI_2INT,op,root,MPI_COMM_WORLD);
   if(me == root)
      if(result.flag != 0)
         printf("ERROR in MPI_Reduce(4): result = %d, should be 0\n",result.flag);

   temp = op;
   MPI_Op_free(&op);
   if(op != MPI_OP_NULL)
      printf("ERROR in MPI_Op_free: op not set to NULL\n");

   MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);
   rc = MPI_Reduce(&info,&result,1,MPI_2INT,op,root,MPI_COMM_WORLD);
   MPI_Error_class(rc,&class);
   if(class != MPI_ERR_OP) {
      printf("WARNING in MPI_Op_free: error on NULL op not MPI_ERR_OP\n"); 
      printf("error returned was %d(%d)\n", rc, class );
   }
/*
   op = temp;
   rc = MPI_Reduce(&info,&result,1,MPI_2INT,op,root,MPI_COMM_WORLD);
   MPI_Error_class(rc,&class);
   if(class != MPI_ERR_OP) {
      printf("WARNING in MPI_Op_free: error on op not freed not MPI_ERR_OP\n"); 
      printf("error returned was %d(%d)\n", rc, class );
   }
 */

   op = MPI_OP_NULL;
   MPI_Op_create(less,commute,&op);
   /* This is NOT an error */
/*
    if(op != temp)
      printf("ERROR in MPI_Op_create: op not reused\n");
 */
   temp = op;
   op = MPI_SUM;
   rc = MPI_Op_free(&op);
   MPI_Error_class(rc,&class);
   if(class != MPI_ERR_OP) {
       char errmsg[MPI_MAX_ERROR_STRING];
       int  rlen;
       printf("WARNING in MPI_Op_free: error on free MPI_SUM not MPI_ERR_OP\n"); 
       MPI_Error_string( rc, errmsg, &rlen );
      printf("error returned was %s [%d(%d)]\n", errmsg, rc, class );
   }

   /* Tidy up */
   MPI_Op_free(&temp);

   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
   return 0;
}
