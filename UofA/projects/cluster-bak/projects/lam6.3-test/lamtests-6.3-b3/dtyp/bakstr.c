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
/* This bizarre datatype uses a negative stride to move backward through
   a buffer picking up every other halfword.  Despite the backwards 
   nature of the type, LB is < UB because the standard states this is 
   always so.                                                              

   WDG - The standard has changed since this was written.  ub and lb 
   are used to set the stride and no longer must have lb <= ub.
*/
 
#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#define MSZ 96
int main( argc, argv )
int argc;
char **argv;
{
    char imessage[MSZ], omessage[MSZ], xmessage[MSZ];
    int myrank;
    int i,error=0;
    int numtasks,me;
    int tmp,tmp2,tmp3,tmp4;
    char mark[3];
    int checkmask[MSZ];
    static int checkpack[] = {
  49,50, 45,46, 41,42, 37,38, 33,34, 29,30, 25,26, 21,22, 17,18, 13,14,  9,10,  5, 6,
  95,96, 91,92, 87,88, 83,84, 79,80, 75,76, 71,72, 67,68, 63,64, 59,60, 55,56, 51,52
    };
    int pos;
    MPI_Aint      extent,lb,ub;

    MPI_Datatype newtype0;

    for (i=0;i<MSZ;i++) {
      omessage[i] = (char) (i % 255)+1;
      imessage[i] = (char) 0;
      xmessage[i] = (char) 0;
    }
    checkmask[0] = 0;
    checkmask[1] = 0;

    for (i=2;i<=46; i+=4) {
	checkmask[i]   =0;
	checkmask[i+1] =0;
	checkmask[i+2] =1;
	checkmask[i+3] =1;
      } 
    for (i=48;i<=92; i+=4) {
	checkmask[i]   =0;
	checkmask[i+1] =0;
	checkmask[i+2] =1;
	checkmask[i+3] =1;
    } 
    checkmask[48] = 1;
    checkmask[49] = 1;

    MPI_Init( &argc, &argv );
    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &me);

    if ((numtasks != 1) && (me!=0)) { 
#ifdef DB_TALK
      printf("Testcase %s uses one task, extraneous task #%d exited.\n",argv[0],me);
#endif
      MPI_Finalize();
      exit(0);
    }
    MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
    MPI_Type_vector(12,1,-2,MPI_SHORT,&newtype0);
    MPI_Type_commit(&newtype0);
    
    MPI_Type_extent(newtype0,&extent);
    MPI_Type_lb(newtype0,&lb);
    MPI_Type_ub(newtype0,&ub);
    pos = 0;
#ifdef DB_TALK
    printf("extent= %d, lb= %d, ub= %d.\n", extent, lb, ub);
    printf("The first gather runs back from &buf+%2d and the second from &buf+%2d.\n",
           MSZ/2, MSZ/2+extent);
#endif
    MPI_Pack(omessage+MSZ/2,2,newtype0,imessage,sizeof(imessage),&pos,MPI_COMM_WORLD);
    pos = 0;
    MPI_Unpack(imessage,sizeof(imessage),&pos,xmessage+MSZ/2,2,newtype0,MPI_COMM_WORLD); 
    for (i=0; i<sizeof(checkpack)/sizeof(int); i++) {
      if (checkpack[i] != (int) imessage[i]) {
        error++;
      }
    }
    for (i=0; i<MSZ; i++) {
      if (checkmask[i]) {
        if (omessage[i] != xmessage[i]) {
          error++;
        }
      } else {
        if (xmessage[i] != 0)  {
          error++;
        }
      }
    }
    if (error) {     
	printf( "Found %d errors\n", error );
      for (i=0; i<MSZ; i++) {
        tmp =  (int) omessage[i]; 
        tmp2 = (int) imessage[i];
        tmp3 = (int) xmessage[i];
        if (i<48)
          tmp4 = checkpack[i];
        else
          tmp4 = 0;
        mark[2] = '\0';
        if (checkmask[i]) {
          mark[0] = '<';
          mark[1] = '-';
        } else {
          mark[0] = ' ';
          mark[1] = ' ';
        }
        printf(
         "idx %2d; original %2d; packval %2d (%2d); unpackval %2d %s\n",
	 i,           tmp,         tmp2, tmp4,            tmp3, mark);
	}
      }

    MPI_Type_free(&newtype0);
    if (newtype0 != MPI_DATATYPE_NULL) 
      printf("MPI_Type_free test FAILED.\n");
    MPI_Finalize();
return 0; 
}
