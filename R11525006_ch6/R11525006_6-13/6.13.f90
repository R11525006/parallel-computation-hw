program exercise11
implicit none
include 'mpif.h'
integer::rank,size,ierror   !�B�z���Ѽ� 
integer::m,n,p              !�x�}�j�pm*n ,p=�C������                  
integer,allocatable,dimension(:,:)::x        !�x�}�˻�
integer,allocatable,dimension(:,:)::life     !�ӭM�P��8�椺�����ӭM
integer::iopen,iread,n_unit=9                !�}���ɮ�
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
   
   allocate(x(0:m+1,0:n+1))         !���w�x�}�j�p
   allocate(life(m,n))
   x=0
   life=0
   
   
 
  ! write(*,*)"state:",0
   do i=1,m                      !Ū�i�x�}���                
      read(n_unit,*,iostat=iread)(x(i,j),j=1,n)   !iread=0 means read successfully !data input
      ! write(*,100)(x(i,j),j=1,n)
      if(iread < 0) exit
   end do
end if

p=2      !�C������
do k=1,p

  write(*,*)"life state:",k
                      
  do j=rank+1,n,size   !�業�����
     do i=1,m          !�p��C���ӭM�P�򦳴X�Ӭ��ӭM
        life(i,j)=x(i-1,j-1)+x(i-1,j)+x(i-1,j+1)+x(i,j-1)+x(i,j+1)+x(i+1,j-1)+x(i+1,j)+x(i+1,j+1)
     end do
  end do

                     
  do j=rank+1,n,size         !�業�����
     do i=1,m 
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
   
 ! if (rank==0)then                                                              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !   do i=1,size-1
   !     do j=rank+1+i,n,size
    !       call MPI_RECV(x(1:m,j),m,MPI_INT,i,0,MPI_COMM_WORLD,IERROR)
     !      write(*,*)"IERROR:",IERROR,"ranl:",rank
      !  end do
    ! end do                                                                       !!!!!!!!! �]���X��!!!!!!!!!!!!
 ! end if

  !if(rank>0)then
   !  do j=rank+1,n,size
    !    call MPI_SEND(x(1:m,j),m,MPI_INT,0,0,MPI_COMM_WORLD,IERROR)
    ! end do
 ! end if                                                                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  
  
 ! do j=rank+1,n,size                                                           !!!!!!!!!!!!!!!!!!!!!!!!
    ! call MPI_BCAST(x(1:m,rank+1),m,MPI_INT,rank,MPI_COMM_WORLD,IERROR)        !!!�T���S���Q����!!!!!!!!!
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
