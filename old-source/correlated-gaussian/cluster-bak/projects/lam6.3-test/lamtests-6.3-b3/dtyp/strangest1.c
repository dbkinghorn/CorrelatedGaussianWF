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

#define MSZ 16

int main( argc, argv )
int argc;
char **argv;
{
    char imessage[MSZ], omessage[MSZ], xmessage[MSZ];
    int myrank;
    int numtasks,me;
    int i,error=0;
    int tmp,tmp2,tmp3;
    int pos;
    int           aob[5];
    MPI_Aint      aod[5];
    MPI_Aint      extent,lb,ub;
    MPI_Datatype  aot[5];

    MPI_Datatype newtype0;

    for (i=0;i<sizeof(imessage);i++) {
      omessage[i] = (char) (i % 255)+1;
      imessage[i] = (char) 0;
      xmessage[i] = (char) 0;
    }
    MPI_Init( &argc, &argv );

    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &me);

    if ((numtasks != 1) && (me!=0)) { 
#ifdef DB_TALK
      printf("Testcase %s uses one task, extraneous task #%d exited.\n",argv[0],me);
#endif
      MPI_Finalize();
      exit(1);
    }

    MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
    aob[0] = 1; aot[0] = MPI_LB;    aod[0] = 0;
    aob[1] = 1; aot[1] = MPI_SHORT; aod[1] = 0;
    aob[2] = 1; aot[2] = MPI_SHORT; aod[2] = 8;
    aob[3] = 1; aot[3] = MPI_UB;    aod[3] = 2; 
    MPI_Type_struct(4,aob,aod,aot,&newtype0);
    MPI_Type_commit(&newtype0);
    
    MPI_Type_extent(newtype0,&extent);
    MPI_Type_lb(newtype0,&lb);
    MPI_Type_ub(newtype0,&ub);
#ifdef DB_TALK
    printf("extent= %d, lb= %d, ub= %d.\n", extent, lb, ub);
#endif
    pos = 0;
    MPI_Pack(omessage,4,newtype0,imessage,sizeof(imessage),&pos,MPI_COMM_WORLD);
    pos = 0;
    MPI_Unpack(imessage,sizeof(imessage),&pos,xmessage,4,newtype0,MPI_COMM_WORLD); 
    for (i=0; i<MSZ; i++)
      if ( omessage[i] != xmessage[i])
        error++;
    if (error) {
      printf("FAIL: results below.\n");
      for (i=0; i<MSZ; i++) {
        tmp =  (int) omessage[i]; 
        tmp2 = (int) imessage[i];
        tmp3 = (int) xmessage[i];
        printf("idx %4d    original %4d    packval %4d    unpackval %4d\n",
                i, tmp, tmp2, tmp3);
      }
    }

    MPI_Type_free(&newtype0);
    if (newtype0 != MPI_DATATYPE_NULL) 
      printf("MPI_Type_free test FAILED.\n");
    MPI_Finalize();
return 0; 
}
