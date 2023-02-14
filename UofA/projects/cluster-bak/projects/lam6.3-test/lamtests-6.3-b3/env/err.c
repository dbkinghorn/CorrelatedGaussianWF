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

static int errcount1 = 0;
static int errcount2 = 0;


#ifdef __STDC__
void myhandler1(MPI_Comm *comm,int *code,...)
#else
void myhandler1( comm, code )
MPI_Comm *comm;
int      *code;
#endif
{
   int me;

   ++errcount1;

   MPI_Comm_rank(*comm,&me);
   if (*code != MPI_ERR_COUNT) {
      printf("ERROR: rank %d: expected errcode %d, got %d\n",
		me, MPI_ERR_COUNT, *code);
   }
}

#ifdef __STDC__
void myhandler2(MPI_Comm *comm,int *code,...)
#else
void myhandler2(comm,code)
MPI_Comm *comm;
int *code;
#endif
{
   int me;

   ++errcount2;

   MPI_Comm_rank(*comm,&me);
   if (*code != MPI_ERR_ROOT) {
      printf("ERROR: rank %d: expected errcode %d, got %d\n",
		me, MPI_ERR_ROOT, *code);
   }
}

int main(argc,argv)
int argc;
char**argv;
{
   int size,me,tasks,rc;
   MPI_Errhandler handler1,handler2,commhandler;
   MPI_Comm comm;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);
   MPI_Comm_size(MPI_COMM_WORLD,&tasks);

   MPI_Errhandler_create((MPI_Handler_function*) myhandler1, &handler1);
   MPI_Errhandler_create((MPI_Handler_function*) myhandler2, &handler2);
   
   MPI_Comm_dup(MPI_COMM_WORLD,&comm);
   
   MPI_Errhandler_set(comm,handler1);   
   MPI_Errhandler_get(comm,&commhandler);
   if(commhandler != handler1) 
#if 0
     /* Bonk -- unfortunately, this is not portable to
        architectures/OS's where sizeof(int) < sizeof(void*) */
     printf("ERROR in MPI_Errhandler_get, handler = %d, should be %d\n",
	    (int) commhandler, (int) handler1); 
#else
     printf("ERROR in MPI_Errhandler_get, handler is wrong value\n");
#endif
   MPI_Bcast(&size,-1,MPI_INT,0,comm); 
   
   MPI_Errhandler_set(comm,handler2);   
   MPI_Bcast(&size,1,MPI_INT,-1,comm); 

   MPI_Errhandler_free(&handler2);
   if(handler2 != MPI_ERRHANDLER_NULL)
     printf("ERROR in MPI_Errorhandler_free, handle not set to NULL\n");
   
   MPI_Errhandler_set(comm,commhandler);   
   
   MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);   
   rc = MPI_Errhandler_create(0,&handler2);
   if(rc == MPI_SUCCESS) printf("ERROR: NULL function not detected\n");
   
   if ((errcount1 != 1) || (errcount2 != 1)) {
     printf("ERROR: errcount1 & errcount2 should be 1, 1; they are %d, %d\n",
	    errcount1, errcount2);
   }

   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Errhandler_free( &handler1 );
   MPI_Errhandler_free( &commhandler );
   MPI_Comm_free( &comm );
   MPI_Finalize();
return 0;
}
