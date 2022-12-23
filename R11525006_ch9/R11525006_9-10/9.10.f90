program chap9
implicit none
include 'mpif.h'
integer::rank,size,ierror   !處理器參數 
integer::status(MPI_STATUS_SIZE)
real*8::start,finish          !程式執行時間開始與結
integer::i,j,k  !loop index
integer::max    !max power
integer*8::stp    !2^n-1
integer*8::largest                    !sqrt(2^n-1)
integer,allocatable,dimension(:)::n   ! record 2^n-1 is prime (local)
integer,allocatable,dimension(:)::gn  ! record 2^n-1 is prime (global)
integer(kind=8),allocatable,dimension(:)::perfect_number
integer::count                        !numbers of perfect number
integer,allocatable,dimension(:)::rcounts
integer,allocatable,dimension(:)::disp  

call MPI_INIT(ierror)       !初始化mpi環境
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror) !size
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror) !rank

if (rank==0) then
   write(*,*)"If 2^(n)-1 is prime then (2^(n)-1)*(2^(n-1)) is perfect number."
   write(*,*)"the first eight n of perfect numbers are:"
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !開始時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

max=100000000                           !test n=2~max , 2^31-1 (limit)
allocate(n(1:max/size))               !給定n陣列大小
allocate(perfect_number(1:max/size))  !給定perfect_number陣列大小         

k=1                                   !loop index
outer: do i=rank+2,max,size           !平行切割

   stp=2**(i)-1                       !2^n-1
   largest=int(stp**0.5)             !sqrt
   inner: do j=3,largest,2            !inner迴圈判斷3~stp^0.5之間有沒有整數能夠整除
      if (mod(int(stp),j)==0) cycle outer !如果被整除回到outer迴圈執行下一個
   end do inner
     
   n(k)=i                                        !record n value when  2^n-1 is prime
   perfect_number(k)=(2**(i)-1)*(2**(i-1))       !calculate and record the perfect number
   k=k+1                                         !record local numbers of prefect number (k-1)
         
end do outer

!call MPI_allreduce(k-1,count,1,MPI_INT,MPI_SUM,MPI_COMM_WORLD,ierror) !numbers of prefect number (global)
!allocate(rcounts(0:size-1))
!allocate(disp(0:size-1))
!allocate(gn(0:count-1))            !給定gn陣列大小


!call MPI_gather(k-1,1,MPI_INT,rcounts,1,MPI_INT,0,MPI_COMM_WORLD,IERROR)
!if (rank==0) then 
  ! write(*,*)"rcounts=",rcounts
  ! disp(0)=0
  ! do i=1,size-1
    !  disp(i)=disp(i-1)+rcounts(i-1)
  ! end do
  ! write(*,*)"disp=",disp
!end if

!call MPI_gather(n,k-1,MPI_INT,gn,rcounts,disp,MPI_INT,0,MPI_COMM_WORLD,IERROR) !n -> gn in rank0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime()  !結束時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call MPI_Barrier(MPI_COMM_WORLD)

do i=1,k-1
   if (n(i)<=31) then
      write(*,*)n(i)
   end if
end do

call MPI_Barrier(MPI_COMM_WORLD)

if (rank==0) then
 !  write(*,*)"count=",count
 !  write(*,*)gn
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !總執行時間
end if

call MPI_FINALIZE(ierror)
end program chap9
