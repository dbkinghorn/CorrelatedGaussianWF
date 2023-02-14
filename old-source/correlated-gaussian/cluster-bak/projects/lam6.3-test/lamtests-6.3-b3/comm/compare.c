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

int
main(argc,argv)
int argc;
char**argv;
{
   MPI_Comm comm1,comm2, comm3;
   int me,result,color,key;

   MPI_Init(&argc,&argv);
   MPI_Comm_rank(MPI_COMM_WORLD,&me);

   MPI_Comm_dup(MPI_COMM_WORLD,&comm1);

   MPI_Comm_compare(comm1,comm1,&result);
   if(result != MPI_IDENT)
      printf("ERROR in MPI_Comm_compare, result = %d, should be %d (IDENT)\n",result,MPI_IDENT);

   MPI_Comm_compare(MPI_COMM_WORLD,comm1,&result);
   if(result != MPI_CONGRUENT)
      printf("ERROR in MPI_Comm_compare, result = %d, should be %d (CONGRUENT)\n",result,MPI_CONGRUENT);

   color = 1;
   key = -me;
   MPI_Comm_split(comm1,color,key,&comm2);
   comm3 = comm2;
   MPI_Comm_compare(comm1,comm2,&result);
   if(result != MPI_SIMILAR)
      printf("ERROR in MPI_Comm_compare, result = %d, should be %d (SIMILAR)\n",result,MPI_SIMILAR);

   color = me;
   MPI_Comm_split(comm1,color,key,&comm2);
   MPI_Comm_compare(comm1,comm2,&result);
   if(result != MPI_UNEQUAL)
      printf("ERROR in MPI_Comm_compare, result = %d, should be %d (UNEQUAL)\n",result,MPI_UNEQUAL);

   MPI_Comm_free( &comm1 );
   MPI_Comm_free( &comm2 );
   MPI_Comm_free( &comm3 );
   MPI_Finalize();
return 0;
}
