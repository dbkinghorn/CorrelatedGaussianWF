program test1
  ! see if I can get mpi to work

  include 'mpif.h' ! would be nice to replace this with a module
  character*32 hostname
  integer  :: flag, ierr, rank, nproc, i, status(MPI_STATUS_SIZE)
  integer :: hostnm
  integer :: n,stp
  real, dimension(8) :: nums
  real, dimension(:), allocatable :: lnums,sumv

  call MPI_INIT( ierr )
  call MPI_COMM_RANK( MPI_COMM_WORLD, rank, ierr )
  call MPI_COMM_SIZE( MPI_COMM_WORLD, nproc, ierr )

  if( rank == 0 ) then
     read(*,*) n    !read from stdin n, nums
     read(*,*) nums
     stp = n/nproc
     write(*,*) n, stp
     !do i=0,nproc-1
     !   call MPI_SEND(nums(i*stp+1),stp,MPI_REAL,i,100,MPI_COMM_WORLD,ierr)
     !end do
  end if

  call MPI_BCAST(stp,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)  
  allocate( lnums(stp), sumv(stp) )
  call MPI_SCATTER(nums,stp,MPI_REAL,lnums,stp,MPI_REAL,0, &
                   MPI_COMM_WORLD,ierr)
  lnums=lnums+1.0

!  call MPI_RECV(lnums,stp,MPI_REAL,0,100,MPI_COMM_WORLD,status,ierr)
        
  flag = hostnm(hostname)
  write(*,*) "Hello from ", hostname, "recieved\n ", lnums

  call MPI_REDUCE(lnums,sumv,stp,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
 
  
  if( rank == 0 ) then
     write(*,*) "sumv is: ", sumv
  end if

  call MPI_FINALIZE( ierr )

end program test1
