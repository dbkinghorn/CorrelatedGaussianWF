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

int main( argc, argv )
int argc;
char **argv;
{
    char imessage[2000], omessage[2000], xmessage[2000];
    int myrank;
    int me,numtasks;
    int i,error=0;
    int tmp,tmp3;
    int size;
    MPI_Aint extent;
    int checkmask[200];
    int pos;
    int           aob[5];
    MPI_Aint      aod[5];
    MPI_Datatype  aot[5], shortint;

    MPI_Datatype newtype1, newtype2;

    for (i=0;i<sizeof(imessage);i++) {
      omessage[i] = (char) (i % 255)+1;
      imessage[i] = (char) 0;
      xmessage[i] = (char) 0;
    }
    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &myrank );

    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &me);

    if ((numtasks != 1) && (me!=0)) { 
#ifdef DB_TALK
      printf("Testcase %s uses one task, extraneous task #%d exited.\n",argv[0],me);
#endif
      MPI_Finalize();
      exit(1);
    }

/* Typemap for MPI_SHORT_INT is: { {short,0) (int,4) } extent==8 */
    /* MPI_SHORT_INT may contain an explicit UB, which will mess these
       tests up */
    aob[0]=1; aod[0]=0;  aot[0]=MPI_SHORT;
    aob[1]=1; aod[1]=sizeof(int); aot[1]=MPI_INT;
    MPI_Type_struct( 2, aob, aod, aot, &shortint );

    MPI_Type_contiguous(2,shortint,&newtype1);

/* Typemap for newtype1 is: 
    { (short,0) (int,4) (short,8) (int,12) } extent 16           */

    MPI_Type_extent(newtype1,&extent);
    MPI_Type_size(newtype1,&size);
    if ((extent!=16) || (size!=12))
      printf("OOPS: newtype1, extent = %d, size = %d.\n",(int) extent, 
	     (int) size);

    aob[0]=2; aod[0]=0;  aot[0]=MPI_INT;
    aob[1]=1; aod[1]=16; aot[1]=newtype1;
    aob[2]=2; aod[2]=64; aot[2]=shortint;
    MPI_Type_struct(3,aob,aod,aot,&newtype2);
    MPI_Type_commit(&newtype2);
 
/* Typemap for newtype2 is:
    { (int,0) (int,4)
      (short,0+16) (int,4+16) (short,8+16) (int,12+16)
      (short,0+64) (int,4+64) (short,8+64) (int,12+64)  } 
      extent==80 
    gaps are at: 8-15, 18-19, 26-27, 32-63, 66-67 74-75        */                     
    for (i=0;i<200;i++) {
      checkmask[i] = 1;
      if ((i>= 8) && (i<=15)) checkmask[i] = 0;
      if ((i>=18) && (i<=19)) checkmask[i] = 0;
      if ((i>=26) && (i<=27)) checkmask[i] = 0;
      if ((i>=32) && (i<=63)) checkmask[i] = 0;
      if ((i>=66) && (i<=67)) checkmask[i] = 0;
      if ((i>=74) && (i<=75)) checkmask[i] = 0;
      if ((i>= 8+80) && (i<=15+80)) checkmask[i] = 0;
      if ((i>=18+80) && (i<=19+80)) checkmask[i] = 0;
      if ((i>=26+80) && (i<=27+80)) checkmask[i] = 0;
      if ((i>=32+80) && (i<=63+80)) checkmask[i] = 0;
      if ((i>=66+80) && (i<=67+80)) checkmask[i] = 0;
      if ((i>=74+80) && (i<=75+80)) checkmask[i] = 0;
    } 
    MPI_Type_extent(newtype2,&extent);
    MPI_Type_size(newtype2,&size);
    if ((extent!=80) || (size!=32))
      printf("OOPS: newtype2, extent = %d, size = %d.\n", (int) extent,
	     (int) size);
    for (i=0;i<sizeof(imessage);i++) {
      imessage[i] = (char) 0;
      xmessage[i] = (char) 0;
    }
    pos = 0;
    MPI_Pack(omessage,2,newtype2,imessage,sizeof(imessage),&pos,MPI_COMM_WORLD);
    pos = 0;
    MPI_Unpack(imessage,sizeof(imessage),&pos,xmessage,2,newtype2,MPI_COMM_WORLD);
    for (i=0; i<extent*2; i++) {
      tmp =  (int) omessage[i]; 
      tmp3 = (int) xmessage[i];
      if ((!checkmask[i]) && (tmp3!=0)) {
        error++;
        printf("Byte# %d should have remained 0 but was %d.\n",i,tmp3);
      }
      if ((checkmask[i]) && (tmp!=tmp3)) {
        error++;
        printf("Byte# %d should have been %d but was  %d.\n",i,tmp,tmp3);
      }
     /* Restore this printf to see the whole pattern if you need to debug .... 
      printf("idx %#4d    original %#4d    packval %#4d    unpackval %#4d\n",
              i, tmp, tmp2, tmp3);
    */
    }
    MPI_Type_free(&newtype1);
    if (newtype1 != MPI_DATATYPE_NULL) 
      printf("MPI_Type_free test FAILED.\n");
    MPI_Type_free(&newtype2);
    MPI_Type_free(&shortint);
    MPI_Finalize();
return 0; 
}
