program exercise11
implicit none
include 'mpif.h'
integer::rank,size,ierror   !處理器參數 
integer::m,n,p              !矩陣大小m*n ,p=遊戲次數                  
integer,allocatable,dimension(:,:)::x        !矩陣樣貌
integer,allocatable,dimension(:,:)::life     !細胞周圍8格內的活細胞
integer::iopen,iread,n_unit=9                !開啟檔案
integer::i,j,k                               !loop index
integer::root

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)
root=rank
!open data
open (Unit=n_unit,File='life.dat',Status='OLD',Action='read', IOSTAT=iopen)
! iopen=0 means open file successfully

if (iopen==0) then               

   read(n_unit,*,iostat=iread)m,n   !iread=0 means read successfully !data input
   write(*,*)
 !  write(*,*)"m:",m,"n:",n
   write(*,*)
   
   allocate(x(0:m+1,0:n+1))         !給定矩陣大小
   allocate(life(m,n))
   x=0
   life=0
   
   
 
  ! write(*,*)"state:",0
   do i=1,m                      !讀進矩陣資料                
      read(n_unit,*,iostat=iread)(x(i,j),j=1,n)   !iread=0 means read successfully !data input
      ! write(*,100)(x(i,j),j=1,n)
      if(iread < 0) exit
   end do
end if

p=2      !遊戲次數
do k=1,p

  write(*,*)"life state:",k
                      
  do j=rank+1,n,size   !行平行切割
     do i=1,m          !計算每顆細胞周圍有幾個活細胞
        life(i,j)=x(i-1,j-1)+x(i-1,j)+x(i-1,j+1)+x(i,j-1)+x(i,j+1)+x(i+1,j-1)+x(i+1,j)+x(i+1,j+1)
     end do
  end do

                     
  do j=rank+1,n,size         !行平行切割
     do i=1,m 
        if  (x(i,j)==0) then          !如果細胞是死的
           if(life(i,j)==3) then              !如果旁邊有3個活細胞就復活
              x(i,j)=1
           else
              x(i,j)=0                         !如果沒有維持死亡狀態
           end if
        else if(x(i,j)==1) then       !如果細胞是活的
           if((life(i,j)==2).or.(life(i,j)==3)) then  !如果周圍有2或3個活細胞維持活著
              x(i,j)=1
           else
              x(i,j)=0                                !如果周圍活細胞<2 或 >3 就死亡
           end if
        end if
     end do
  end do
   
 ! if (rank==0)then                                                              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   do i=1,size-1
   !     do j=rank+1+i,n,size
    !       call MPI_RECV(x(1:m,j),m,MPI_INT,i,0,MPI_COMM_WORLD,IERROR)
     !      write(*,*)"IERROR:",IERROR,"ranl:",rank
      !  end do
    ! end do                                                                       !!!!!!!!! 跑不出來!!!!!!!!!!!!
 ! end if

  !if(rank>0)then
   !  do j=rank+1,n,size
    !    call MPI_SEND(x(1:m,j),m,MPI_INT,0,0,MPI_COMM_WORLD,IERROR)
    ! end do
 ! end if                                                                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  
  
 ! do j=rank+1,n,size                                                           !!!!!!!!!!!!!!!!!!!!!!!!
    ! call MPI_BCAST(x(1:m,rank+1),m,MPI_INT,rank,MPI_COMM_WORLD,IERROR)        !!!訊息沒有被接收!!!!!!!!!
 ! end do                                                                       !!!!!!!!!!!!!!!!!!!!!!!!
 

  call MPI_BCAST(x(1:m,1),m,MPI_INT,0,MPI_COMM_WORLD,IERROR)
  call MPI_BCAST(x(1:m,2),m,MPI_INT,1,MPI_COMM_WORLD,IERROR)
  call MPI_BCAST(x(1:m,3),m,MPI_INT,2,MPI_COMM_WORLD,IERROR)
  call MPI_BCAST(x(1:m,4),m,MPI_INT,3,MPI_COMM_WORLD,IERROR)
  call MPI_BCAST(x(1:m,5),m,MPI_INT,0,MPI_COMM_WORLD,IERROR)

  if(rank==0) then
     write(*,*)"state:",k,"rank:",rank
     do i=1,m
        write(*,100)(x(i,j),j=1,n)
     end do
  end if

end do
















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!if (rank==0) then
 ! ! write(*,*)"rank0"
  ! do i=1,5
    !  do j=1,n
     !    x(i,j)=1
    !  end do
      !  write(*,100)(x(i,j),j=1,5)
  ! end do
!end if

   
!if (rank==1) then 
 ! ! write(*,*)"rank1"
  ! do i=2,2
   !   do j=1,n
    !     x(i,j)=2
     ! end do
     ! write(*,100)(x(i,j),j=1,n)
  ! end do
!end if

!std=0
!do i=rank+1,m,size
  ! root=rank
  ! root=i-std*size-1
  ! write(*,*)"i=",i,"root:",root,"rank:",rank
  ! call MPI_BCAST(x(i,1:n),n,MPI_INT,root,MPI_COMM_WORLD,IERROR)
  ! std=std+1
!end do

!do k=1,size-1
 !  if (rank==0)then
  !    do i=rank+1+k,m,size
   !      call MPI_RECV(x(i,1:n),n,MPI_INT,k,0,MPI_COMM_WORLD,IERROR)
    !     write(*,*)"IERROR:",IERROR,"ranl:",rank
     ! end do
  ! end  if
!end do

 !if(rank>0)then
  !  do i=rank+1,m,size
   !    call MPI_SEND(x(i,1:n),n,MPI_INT,0,0,MPI_COMM_WORLD,IERROR)
   ! end do
 !end if



!call MPI_BCAST(x(1,1:n),n,MPI_INT,0,MPI_COMM_WORLD,IERROR)
!call MPI_BCAST(x(5,1:n),n,MPI_INT,0,MPI_COMM_WORLD,IERROR)
!call MPI_BCAST(X(2,1:n),n,MPI_INT,1,MPI_COMM_WORLD,IERROR)

!if (rank==0)then
 !  write(*,*)"rank0"
  ! do i=1,5
   !   write(*,100)(x(i,j),j=1,5)
  ! end do
!end if

!do i=1,m
 !  call MPI_Bcast(x(i,1:n),n,MPI_INT,0,MPI_COMM_WORLD,IERROR)
!end do

!if (rank==3)then
 !  write(*,*)"rank3"
  ! do i=1,5
   !   write(*,100)(x(i,j),j=1,5)
  ! end do
!end if


100 format(5I2)


!call MPI_Bcast(n,1,MPI_INT,0,MPI_COMM_WORLD,ierror)
!call MPI_Bcast(d,1,MPI_INT,0,MPI_COMM_WORLD,ierror)




call MPI_FINALIZE(ierror)
end program exercise11
