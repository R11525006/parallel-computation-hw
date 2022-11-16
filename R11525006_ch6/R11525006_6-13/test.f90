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
   
   allocate(x(0:m+1,0:n+1))         !���w�x�}�j�p
   allocate(life(m,n))
   x=0
   life=0
   
   write(*,*)"state:",0
      do i=1,m                      !Ū�i�x�}���                
          read(n_unit,*,iostat=iread)(x(i,j),j=1,n)   !iread=0 means read successfully !data input
          write(*,100)(x(i,j),j=1,n)
          if(iread < 0) exit
    end do
end if

p=10         !�C������
do k=1,p

  !write(*,*)"life state:",k
   do i=1,m                     
      do j=1,n                         !�p��C���ӭM�P�򦳴X�Ӭ��ӭM
         life(i,j)=x(i-1,j-1)+x(i-1,j)+x(i-1,j+1)+x(i,j-1)+x(i,j+1)+x(i+1,j-1)+x(i+1,j)+x(i+1,j+1)
      end do
    ! write(*,100)(life(i,j),j=1,n)
   end do

   do i=1,m                    
      do j=1,n         
         if  (x(i,j)==0) then          !�p�G�ӭM�O����
            if(life(i,j)==3) then              !�p�G���䦳3�Ӭ��ӭM�N�_��
               x(i,j)=1
            else
               x(i,j)=0                         !�p�G�S���������`���A
            end if
         else if(x(i,j)==1) then       !�p�G�ӭM�O����
            if((life(i,j)==2).or.(life(i,j)==3)) then  !�p�G�P��2��3�Ӭ��ӭM��������
               x(i,j)=1
            else
               x(i,j)=0                                !�p�G�P�򬡲ӭM<2 �� >3 �N���`
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
