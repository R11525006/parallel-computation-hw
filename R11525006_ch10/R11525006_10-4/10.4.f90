program exercise4
implicit none
include 'mpif.h'

integer::rank,size,ierror   !處理器參數 
integer::status(MPI_STATUS_SIZE)
real*8::start,finish          !程式執行時間開始與結

integer,parameter::max=2000000                       !number of points
integer::area
real,allocatable,dimension(:)::x,y,z                    !generate coordinate x,y,z
!!!!!!!!!!!!!!!!!!!!!!!!!!!length
integer,parameter::s=2                               !cube edge
real,parameter::d=0.3                                !cylindrical hole diameter
!!!!!!!!!!!!!!!!!!!!!!!!!!!!generate random number
real::random                                         !function variable
integer::iseed                                       !random seed
!!!!!!!!!!!!!!!!!!!!!!!!!!!!loop index
integer::i,j,k                 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!caculation term
real*16::square_root,denominator,numerator
!!!!!!!!!!!!!!!!!!!!!!!!!!!distance between point and line
real*16::distance
!!!!!!!!!!!!!!!!!!!!!!!!!!statistics how many points satify distance > d/2
integer::n=0,gn                                      !local and global
!!!!!!!!!!!!!!!!!!!!!!!!!!portion of cube remains
real::portion


call MPI_INIT(ierror)       !初始化mpi環境
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror) !size
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror) !rank

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !開始時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

area=max/size
allocate(x(area),y(area),z(area))

iseed=rank
do i=1,area
   x(i)=random(iseed)+1  !0~2
   y(i)=random(iseed)+1  !0~2
   z(i)=random(iseed)+1  !0~2
  ! write(*,"(3f13.8)")x(i),y(i),z(i)
   

   square_root=sqrt(x(i)**2+y(i)**2+z(i)**2)
   denominator=sqrt(3.)*square_root
   numerator=x(i)+y(i)+z(i)
   
   distance=sin(acos(numerator/denominator))*square_root
   
   if (distance > d/2) then
      n=n+1
   end if
end do

call mpi_reduce(n,gn,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,IERROR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime()  !結束時間!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (rank==0) then
   write(*,*)
   write(*,99)gn,max
   portion=gn/real(area*size)*100.
   write(*,100)portion
   write(*,*)
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !總執行時間
end if

99 format("There are",I9," random points remain in the volume with",I9," random points.") 
100 format("The volume of the portion of the cube remains is",f6.2,"%")
end program exercise4
