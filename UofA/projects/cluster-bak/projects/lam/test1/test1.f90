program test1
  ! see if I can get mpi to work

  include 'mpif.h' ! would be nice to replace this with a module
  character*32 hostname
  integer  :: flag, ierr, rank, nproc, i, status(MPI_STATUS_SIZE)
  integer :: hostnm
  real    :: num

  call MPI_INIT( ierr )
  call MPI_COMM_RANK( MPI_COMM_WORLD, rank, ierr )
  call MPI_COMM_SIZE( MPI_COMM_WORLD, nproc, ierr )
write(6,*) rank
  if( rank == 0 ) then
     do i=0,nproc-1
        num = real(i)+1.0
        call MPI_SEND(num,1,MPI_REAL,i,100,MPI_COMM_WORLD,ierr)
     end do
  end if
  
  call MPI_RECV(num,1,MPI_REAL,0,100,MPI_COMM_WORLD,status,ierr)
        
  flag = hostnm(hostname)
  write(*,*) "Hello from ", hostname, "recieved", num
  call flush(6)
  call MPI_FINALIZE( ierr )

end program test1
  

    
