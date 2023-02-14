C  ag_ring_nblk.f -- ring allgather using nonblocking sends and receives
C 
C  Input: series of blocksizes for allgather, 0 to stop.
C  Output: Contents of gathered array on each process -- list of
C      process ranks, each rank appearing in a block of size blocksize.
C 
C  Note:  array sizes are hardwired in MAX and LOCAL_MAX.
C 
C  See Chap 13, pp. 298 & ff, in PPMPI.
C
C ******************************************************************  
      PROGRAM RingBlk
      INCLUDE 'mpif.h'
      INCLUDE 'ciof.h'
      integer       MAX 
      integer       LOCAL_MAX 
      parameter     (MAX = 1024, LOCAL_MAX = 1024)
      integer       p
      integer       my_rank
      real          x(0:1023)
      real          y(0:1023)
      integer       blocksize
      integer       io_comm
      integer       i
      integer       rtnval
      integer       ierr
      character *40 prompt,title
C
      call MPI_INIT( ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,  p, ierr )
      call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
      call MPI_COMM_DUP(MPI_COMM_WORLD, io_comm, ierr )
      rtnval = Cache_io_rank(MPI_COMM_WORLD, io_comm  )
C
      prompt = 'Enter the local array size (0 to quit)'
      rtnval = Cread(io_comm, 1, prompt, blocksize)
C
      do while(blocksize .GT. 0) 
          do 100 i = 0  , blocksize-1
              x(i) =  my_rank
 100      continue
          call Allgather_ring(x, blocksize, y, MPI_COMM_WORLD  )
          call Print_arrays(io_comm,  'GATHERed_arrays ', 
     +                      y, blocksize)
C  Enter 0 to stop.
          prompt =  'Enter the local array size (0 to quit)'   
          rtnval = Cread(io_comm, 1, prompt, blocksize) 
      end do
C
      call MPI_FINALIZE(ierr)
      end
C
C ******************************************************************  
      subroutine Print_arrays(io_comm, title, y, blocksize)
      integer         io_comm     
      character *17     title       
      real            y(128)         
      integer         blocksize   
C
      include     'mpif.h'
      include     'ciof.h'
      integer     ierr
      integer  i, j
      integer  p
      integer  retval
      integer  iitem
      integer  list(128)
      data     list /128*0/
C
      call MPI_COMM_SIZE(io_comm,  p, ierr)
      
      do 100 i = 1 , blocksize*p
          iitem = int(y(i))
          list(i) = iitem
 100  continue
      retval = Cprint(io_comm, blocksize*p, title,list)
      return
      end
C
C
C ******************************************************************  
      subroutine Allgather_ring(x, blocksize, y, ring_comm)
      real      x(0:blocksize)         
      integer   blocksize   
      real      y(0:1023)         
      integer   ring_comm
C
      INCLUDE   'mpif.h'
      integer   i, p, my_rank
      integer   successor, predecessor
      integer   send_offset, recv_offset
      integer   status(MPI_STATUS_SIZE)
      integer   send_request
      integer   recv_request
      integer   ierr
C
      call MPI_COMM_SIZE(ring_comm,  p, ierr)
      call MPI_COMM_RANK(ring_comm,  my_rank, ierr)
C
C  Copy x into correct location in y
      do 100 i = 0, blocksize -1
          y(i + my_rank*blocksize) = x(i)
 100   continue
 
      successor = MOD((my_rank + 1) , p )
      predecessor = MOD((my_rank - 1 + p), p)
C
      send_offset = my_rank*blocksize
      recv_offset = MOD((my_rank - 1 + p) , p)*blocksize
      do 200 i = 0 , p - 2  
          call MPI_ISEND(y(send_offset), blocksize, MPI_REAL ,
     +         successor, 0, ring_comm,  send_request, ierr)
          call MPI_IRECV(y(recv_offset), blocksize, MPI_REAL ,
     +         predecessor, 0, ring_comm,  recv_request,ierr )
C
          send_offset = MOD((my_rank - i - 1 + p) , p)*blocksize
          recv_offset = MOD((my_rank - i - 2 + p) , p)*blocksize
C
          call MPI_WAIT( send_request,  status, ierr)
          call MPI_WAIT( recv_request,  status, ierr)
 200  continue
      return
      end
C
 