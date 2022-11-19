program exercise13
implicit none
include 'mpif.h'
integer::rank,size,ierror   !�B�z���Ѽ� 
integer::status(MPI_STATUS_SIZE)
integer::m,n,p              !�x�}�j�pm*n ,p=�C������                  
integer,allocatable,dimension(:,:)::x        !�x�}�˻�
integer,allocatable,dimension(:,:)::life     !�ӭM�P��8�椺�����ӭM
integer::iopen,iread,n_unit=9                !�}���ɮ�
integer::i,j,k                               !loop index
character(len=80)::form       !!! ��X�榡                
integer::num                  !!! ��X�榡
real*8::start,finish          !�{������ɶ��}�l�P����

call MPI_INIT(ierror)       !��l��mpi����
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror) !size
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror) !rank

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !�}�l�ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!open data
open (Unit=n_unit,File='test.dat',Status='OLD',Action='read', IOSTAT=iopen) !�}�Ҹ���ɮ�
! iopen=0 means open file successfully

if (iopen==0) then               

   read(n_unit,*,iostat=iread)m,n   !iread=0 means read successfully !data input,Ū�i�x�}�j�pm*n
   
   allocate(x(0:m+1,0:n+1))         !���wx�x�}�j�p,�W�U���k�U�h�@�ƭȳ��T�w��0,��K��Ưx�}��t���B��
   allocate(life(m,n))              !���wlife�x�}�j�p
   x=0                              
   life=0
   
   do i=1,m                      !Ū�i�x�}���                
      read(n_unit,*,iostat=iread)(x(i,j),j=1,n)   !iread=0 means read successfully !data input
      if(iread < 0) exit
   end do
end if

num=n
write(form,*) "(",num,"I2)" !�]�w��X�榡

if (rank==0) then
   write(*,*)"state:",0,"rank:",rank             !rank0 ���Ū�i�Ӫ���l���
   do i=1,m
      write(*,form)(x(i,j),j=1,n)
   end do
end if

p=8      !�C������
do k=1,p
                      
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
   
  if (rank==0)then                                                              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     do i=1,size-1                                                               !rank0�������Prank�ҳB�z������
        do j=rank+1+i,n,size
           call MPI_RECV(x(1:m,j),m,MPI_INT,i,0,MPI_COMM_WORLD,status,IERROR)
        end do
     end do                                                                       !!!!!!!!!MPI_RECV �ݭn status!!!!!!
  end if

  if(rank>0)then
     do j=rank+1,n,size                                                          !rank0�H�~��rank�N�ۤv�B�z�����ưe��rank0
        call MPI_SEND(x(1:m,j),m,MPI_INT,0,0,MPI_COMM_WORLD,IERROR)
     end do
  end if                                                                         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                                                          
  call MPI_BCAST(x,(m+2)*(n+2),MPI_INT,0,MPI_COMM_WORLD,IERROR)                  !�ΦX�Ҧ���ƫ�rank0�s���s��x�x�}
   
                                                                   
  if(rank==0) then
     write(*,*)"state:",k,"rank:",rank       !�qrank0��ܲ�k������ƫ��A
     do i=1,m
        write(*,form)(x(i,j),j=1,n)
     end do
  end if

end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime()  !�����ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !�`����ɶ�
end if

call MPI_FINALIZE(ierror)
end program exercise13
