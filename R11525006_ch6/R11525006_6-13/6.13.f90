program exercise13
implicit none
include 'mpif.h'
integer::rank,size,ierror   !處理器參數 
integer::status(MPI_STATUS_SIZE)
integer::m,n,p              !矩陣大小m*n ,p=遊戲次數                  
integer,allocatable,dimension(:,:)::x        !矩陣樣貌
integer,allocatable,dimension(:,:)::life     !細胞周圍8格內的活細胞
integer::iopen,iread,n_unit=9                !開啟檔案
integer::i,j,k                               !loop index
character(len=80)::form       !!! 輸出格式                
integer::num                  !!! 輸出格式
real*8::start,finish          !程式執行時間開始與結束

call MPI_INIT(ierror)       !初始化mpi環境
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror) !size
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror) !rank

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !開始時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!open data
open (Unit=n_unit,File='test.dat',Status='OLD',Action='read', IOSTAT=iopen) !開啟資料檔案
! iopen=0 means open file successfully

if (iopen==0) then               

   read(n_unit,*,iostat=iread)m,n   !iread=0 means read successfully !data input,讀進矩陣大小m*n
   
   allocate(x(0:m+1,0:n+1))         !給定x矩陣大小,上下左右各多一排值都固定為0,方便資料矩陣邊緣的運算
   allocate(life(m,n))              !給定life矩陣大小
   x=0                              
   life=0
   
   do i=1,m                      !讀進矩陣資料                
      read(n_unit,*,iostat=iread)(x(i,j),j=1,n)   !iread=0 means read successfully !data input
      if(iread < 0) exit
   end do
end if

num=n
write(form,*) "(",num,"I2)" !設定輸出格式

if (rank==0) then
   write(*,*)"state:",0,"rank:",rank             !rank0 顯示讀進來的初始資料
   do i=1,m
      write(*,form)(x(i,j),j=1,n)
   end do
end if

p=8      !遊戲次數
do k=1,p
                      
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
   
  if (rank==0)then                                                              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     do i=1,size-1                                                               !rank0接收不同rank所處理的行資料
        do j=rank+1+i,n,size
           call MPI_RECV(x(1:m,j),m,MPI_INT,i,0,MPI_COMM_WORLD,status,IERROR)
        end do
     end do                                                                       !!!!!!!!!MPI_RECV 需要 status!!!!!!
  end if

  if(rank>0)then
     do j=rank+1,n,size                                                          !rank0以外的rank將自己處理的行資料送給rank0
        call MPI_SEND(x(1:m,j),m,MPI_INT,0,0,MPI_COMM_WORLD,IERROR)
     end do
  end if                                                                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                                                          
  call MPI_BCAST(x,(m+2)*(n+2),MPI_INT,0,MPI_COMM_WORLD,IERROR)                  !統合所有資料後rank0廣播新的x矩陣
   
                                                                   
  if(rank==0) then
     write(*,*)"state:",k,"rank:",rank       !從rank0顯示第k次的資料型態
     do i=1,m
        write(*,form)(x(i,j),j=1,n)
     end do
  end if

end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime()  !結束時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !總執行時間
end if

call MPI_FINALIZE(ierror)
end program exercise13
