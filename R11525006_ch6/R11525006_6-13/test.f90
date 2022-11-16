program test
implicit none
integer::p
integer::m,n
integer,allocatable,dimension(:,:)::x
integer,allocatable,dimension(:,:)::life
integer::iopen,iread,n_unit=9
integer::i,j,k


!open data
open (Unit=n_unit,File='life.dat',Status='OLD',Action='read', IOSTAT=iopen)
! iopen=0 means open file successfully

if (iopen==0) then               

   read(n_unit,*,iostat=iread)m,n   !iread=0 means read successfully !data input
   write(*,*)
   write(*,*)"m:",m,"n:",n
   write(*,*)
   
   allocate(x(0:m+1,0:n+1))         !給定矩陣大小
   allocate(life(m,n))
   x=0
   life=0
   
   write(*,*)"state:",0
      do i=1,m                      !讀進矩陣資料                
          read(n_unit,*,iostat=iread)(x(i,j),j=1,n)   !iread=0 means read successfully !data input
          write(*,100)(x(i,j),j=1,n)
          if(iread < 0) exit
    end do
end if

p=10         !遊戲次數
do k=1,p

  !write(*,*)"life state:",k
   do i=1,m                     
      do j=1,n                         !計算每顆細胞周圍有幾個活細胞
         life(i,j)=x(i-1,j-1)+x(i-1,j)+x(i-1,j+1)+x(i,j-1)+x(i,j+1)+x(i+1,j-1)+x(i+1,j)+x(i+1,j+1)
      end do
    ! write(*,100)(life(i,j),j=1,n)
   end do

   do i=1,m                    
      do j=1,n         
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

   write(*,*)"state:",k
   do i=1,m
      write(*,100)(x(i,j),j=1,n)
   end do

end do
100 format(5I2)

end program test
