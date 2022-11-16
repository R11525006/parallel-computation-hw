program pingpong
implicit none
include 'mpif.h'
integer::rank,size,ierror   !處理器參數                 
integer::i,j,k              !loop index
real*8::latency,bandwidth


call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)


   


!!main program




call MPI_FINALIZE(ierror)
end program exercise11
