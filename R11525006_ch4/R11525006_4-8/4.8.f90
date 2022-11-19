program exercise8
implicit none
include 'mpif.h'
integer::rank,size,ierror
integer::a
integer::low_value,high_value,psize
integer::prime,first,count,gcount
integer,allocatable,dimension(:)::marked
integer::i,index
integer::cobp,gcobp  !consecutive odd integers are both prime
real*8::start,finish                !execution time

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !開始時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!write(*,*)"rank",rank
a=1000000

low_value = 2+rank*(a-1)/size      !每個處理器分到的最小值

if (rank>0) then
   low_value = low_value-2         !除了處理器0以外，最小值-2，目的是為了覆蓋上一個處理器
end if                             !所分到的最後兩個值，若此兩值中有質數則無須傳送訊息給下
                                   !一個處理器，節省溝通時間。
  
high_value = 1+(rank+1)*(a-1)/size !每個處理器分到的最大值

psize=high_value-low_value+1       !每個處理器所分到的值的數量

!write(*,*)"low",low_value,"high",high_value,"psize",psize,"rank:",rank


allocate(marked(0:psize-1))
do i=0,psize-1
   marked(i)=0 !處理數量大小的矩陣初始化
end do

if (rank == 0)then
   index=0
end if

prime=2 !質數從2開始

do while (prime*prime <= a)
   if (prime*prime > low_value) then
      first = prime*prime-low_value
      else if (mod(low_value,prime)==0) then
         first =0;
         else
            first=prime-mod(low_value,prime)
         end if
         
         do i=first,psize-1,prime
            marked(i)=1;
         end do
         
         if (rank ==0) then   !rank0尋找下一個prime
            do i=0,psize-1                  
               if (marked(i)==0) then   !找出未被標記的數字
                  if (i+2 > prime) then !該數字比目前的prime還大
                     prime=i+2          !從2開始                         
                     exit      !滿足條件跳出迴圈
                  end if
               end if
            end do
         end if
         !通知所有rank prime已更新
         call   MPI_BCAST(prime, 1, MPI_INT, 0, MPI_COMM_WORLD, IERROR)    
end do

!count =0
!do i=0,psize-1          !計算自己的範圍內有幾個prime
  ! if (marked(i)==0) then
  ! count = count +1
  ! end if
!end do

!全部有幾個prime
!call MPI_REDUCE(count,gcount,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierror) 

!if (rank==0) then
  ! write(*,*)"There are",gcount,"primes less than or equal to",a,"from rank",rank
!end if

cobp=0            
do i=2,psize-1          !計算自己的範圍內有幾個連續奇數對都是質數
   if((marked(i)==0).and.(marked(i-2)==0)) then
      cobp=cobp+1
   end if
end do
   

!全部有幾個cobp
call MPI_REDUCE(cobp,gcobp,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierror) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime() !結束時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then
   write(*,"(A80,I5,1X,A30,I8,1X,A10,I2)")"The number of times that two consecutive odd integers are both prime:"&
              & ,gcobp,"for all integers less than:",a,"from rank:",rank
   
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size   !總執行時間
end if


call MPI_FINALIZE(ierror)
end program exercise8
