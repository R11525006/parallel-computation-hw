program pingpong
implicit none
include 'mpif.h'
integer::rank,psize,ierror   !�B�z���Ѽ�     
integer::status(MPI_STATUS_SIZE)            
integer::i,j               !loop index  
integer::ping,pong,n        !�ǰe���|,n�Ӧ^�ǰe���� 
real*8::start1,finish1,start2,finish2  !�}�l�P����
real*8::time(2,1)          !���P����byte�Ҫ�O���ɶ�
integer(kind=4)::test1,test2(10000)  !4byte , 40000byte
real*8::x(2,1)=0.d0        !��latency,bandwidth
real*8::A(2,2)=1.d0        ![A]*{x}={Time}
real*8::InverseA(2,2)      !{x}=[A]^-1 * {Time}
real*8::detA               !A����C����
real*8::latency,bandwidth


call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, psize ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)
A(1,2)=4.d0           !  | 1    4   |   |   latency    |   |  Time1 |
A(2,2)=40000.d0       !  |          | * |              | = |        |
n=10000               !  | 1   40000|   | 1/bandwidth  |   |  Time2 |
if (rank==0)then

   start1=MPI_Wtime() !�Ĥ@���ǰetest1=4byte
   do i=1,n
      call MPI_SEND(test1,1,MPI_INT,1,ping,MPI_COMM_WORLD,IERROR)
      call MPI_RECV(test1,1,MPI_INT,1,pong,MPI_COMM_WORLD,status,IERROR) 
   end do
   finish1=MPI_Wtime()

   start2=MPI_Wtime() !�ĤG���ǰetest2=4byte*10000
   do i=1,n
      call MPI_SEND(test2,size(test2),MPI_int,1,ping,MPI_COMM_WORLD,IERROR)
      call MPI_RECV(test2,size(test2),MPI_int,1,pong,MPI_COMM_WORLD,status,IERROR) 
   end do
   finish2=MPI_Wtime()
   
   time(1,1)=(finish1-start1)/real(2*n)  !�Ĥ@���ǰe�`�@�ɶ�/�Ӧ^���� ,2��send+recv,n��10000��
   time(2,1)=(finish2-start2)/real(2*n)  !�ĤG���ǰe�`�@�ɶ�/�Ӧ^���� ,2��send+recv,n��10000��
   write(*,*)"time 1:",time(1,1)        
   write(*,*)"time 2:",time(2,1)
end if
   
if (rank==1)then
      
   do i=1,n  !test1
      call MPI_RECV(test1,1,MPI_INT,0,ping,MPI_COMM_WORLD,status,IERROR)
      call MPI_SEND(test1,1,MPI_INT,0,pong,MPI_COMM_WORLD,IERROR) 
   end do
   
   do i=1,n  !test2
      call MPI_RECV(test2,size(test2),MPI_int,0,ping,MPI_COMM_WORLD,status,IERROR)
      call MPI_SEND(test2,size(test2),MPI_int,0,pong,MPI_COMM_WORLD,IERROR) 
   end do

end if

if (rank==0) then
   write(*,*)"A"
   do i=1,2
      write(*,100)(A(i,j),j=1,2)
   end do
   detA=(A(2,2)*A(1,1)-A(1,2)*A(2,1)) !A����C����
   InverseA(1,1)=A(2,2)/detA      !!!!!!!!!!!!!!!!!!!!
   InverseA(1,2)=-A(1,2)/detA     !!
   InverseA(2,1)=-A(2,1)/detA     !!  �D2*2�x�}���ϯx�} 
   InverseA(2,2)=A(1,1)/detA      !!!!!!!!!!!!!!!!!!!!
 
   write(*,*)"Inverse A"
   do i=1,2
      write(*,*)(InverseA(i,j),j=1,2)
   end do

   x=matmul(InverseA,time)    !�D�Xlatency �M 1/bandwidth
   latency=x(1,1)
   bandwidth=1/x(2,1)
   write(*,*)"latency:",latency,"bandwidth:",bandwidth
end if



100 format(2f12.3)
call MPI_FINALIZE(ierror)
end program pingpong

