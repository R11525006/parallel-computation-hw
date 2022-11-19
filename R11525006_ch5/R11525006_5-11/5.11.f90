program exercise11
implicit none
include 'mpif.h'
integer::rank,size,ierror   !�B�z���Ѽ�
integer::n                          !numbers to calculation
integer::d                          !digits of precision
real*8::sum,gsum                    !local sum , global sum                     
integer::i                          !loop index
real*8::start,finish                !execution time

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !�}�l�ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then                           !rank 0 �ǰen.d����
   n=1000000
   d=100
end if

call MPI_Bcast(n,1,MPI_INT,0,MPI_COMM_WORLD,ierror)
call MPI_Bcast(d,1,MPI_INT,0,MPI_COMM_WORLD,ierror)

sum=0.d0
gsum=0.d0
do i=rank+1,n,size          !���u�B�z1/i
   sum=sum+1.0/real(i)
  ! if (rank==1)then 
  !    write(*,*)i
  !    write(*,*)sum
  ! end if 
end do

call MPI_REDUCE(sum,gsum,1,MPI_double,MPI_sum,0,MPI_COMM_WORLD,ierror)  !����sum

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime() !�����ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then

   write(*,*)"The value of S_n for n =",n,"and printed to",d,&
               & "digits of precision after the decimal point is:"
   write(*,100)gsum

   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !�`����ɶ�
end if

100 format(f105.100)

call MPI_FINALIZE(ierror)
end program exercise11
