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
    int imessage[10][10][10], omessage[10][10][10];
    int packbuf[1000];
    int i,j,k;
    int numtasks,me;
    MPI_Aint iaddr, iaddr1, iaddr2, iaddr3;
    MPI_Aint oaddr, oaddr1, oaddr2, oaddr3;
    int pos;
    int aob[10];
    MPI_Datatype aot[10];
    MPI_Aint aod[10];

    MPI_Datatype type_ox, type_oy, type_oz;
    MPI_Datatype type_ia, type_ib, type_ic;

    for (i=0;i<10;i++)
      for (j=0;j<10;j++)
        for (k=0;k<10;k++) {
          omessage[i][j][k] = i*100 + j*10 + k;
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

    /* leftmost stride== 400, middle stride==40, rightmost== 4 */ 
                        /* x  y  z  */ 
    MPI_Address(&(omessage[0][0][0]), &oaddr);
    MPI_Address(&(omessage[1][0][0]), &oaddr1);
    MPI_Address(&(omessage[0][1][0]), &oaddr2);
    MPI_Address(&(omessage[0][0][1]), &oaddr3);

    for (i=0; i<10; i++) { 
      aot[i] = MPI_INT; 
      aob[i] = 1;
      aod[i] = i*(oaddr3-oaddr);
    }
    MPI_Type_struct(10,aob,aod,aot,&type_oz); 

    for (i=0; i<10; i++) { 
      aot[i] = type_oz; 
      aob[i] = 1;
      aod[i] = i*(oaddr2-oaddr);
    }
    MPI_Type_struct(10,aob,aod,aot,&type_oy); 

    for (i=0; i<10; i++) { 
      aot[i] = type_oy; 
      aob[i] = 1;
      aod[i] = i*(oaddr1-oaddr);
    }
    MPI_Type_struct(10,aob,aod,aot,&type_ox); 
    MPI_Type_commit(&type_ox);

                        /* c  b  a  */
    MPI_Address(&(imessage[0][0][0]), &iaddr);
    MPI_Address(&(imessage[1][0][0]), &iaddr1);
    MPI_Address(&(imessage[0][1][0]), &iaddr2);
    MPI_Address(&(imessage[0][0][1]), &iaddr3);
 
    for (i=0; i<10; i++) { 
      aot[i] = MPI_INT; 
      aob[i] = 1;
      aod[i] = i*(iaddr1-iaddr);
    }
    MPI_Type_struct(10,aob,aod,aot,&type_ic); 

    for (i=0; i<10; i++) { 
      aot[i] = type_ic; 
      aob[i] = 1;
      aod[i] = i*(iaddr2-iaddr);
    }
    MPI_Type_struct(10,aob,aod,aot,&type_ib); 

    for (i=0; i<10; i++) { 
      aot[i] = type_ib; 
      aob[i] = 1;
      aod[i] = i*(iaddr3-iaddr);
    }
    MPI_Type_struct(10,aob,aod,aot,&type_ia); 
    MPI_Type_commit(&type_ia);

    pos = 0;
    MPI_Pack(omessage, 1, type_ox, packbuf, sizeof(packbuf), &pos, MPI_COMM_WORLD);
    pos = 0;
    MPI_Unpack(packbuf, sizeof(packbuf), &pos, imessage, 1, type_ia, MPI_COMM_WORLD);

    for (i=0;i<10;i++)
      for (j=0;j<10;j++)
        for (k=0;k<10;k++) 
          if (omessage[i][j][k] != imessage[k][j][i]) {
            printf("OOPS i=%d, j=%d, k=%d, omessage[...] = %d, imessage[...] = %d. \n",
                   i,j,k,omessage[i][j][k],imessage[k][j][i]);
          }
    MPI_Type_free( &type_oz );
    MPI_Type_free( &type_oy );
    MPI_Type_free( &type_ox );
    MPI_Type_free( &type_ic );
    MPI_Type_free( &type_ib );
    MPI_Type_free( &type_ia );

    MPI_Finalize();
return 0; 
}
