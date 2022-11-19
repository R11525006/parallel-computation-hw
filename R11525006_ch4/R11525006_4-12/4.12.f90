program exercise12
implicit none
include 'mpif.h'
integer::rank,size,ierror   !�B�z���Ѽ�
integer::n,a,b              !b�n���W��,a�n���U��,n�n���϶�
real*8,allocatable,dimension(:)::x  !x0 ~ xn
real*8::dx                          !x(i)-x(i-1)=dx
real*8::sum,gsum                    !sum�n���϶����� , gsum�n������
real*8::f0,f1,f2,fn                 !simpson's rule 
real*8::solution                    !pi value from cauculation
integer::low_value,high_value       !���οn���϶�
integer::i
real*8::start,finish                !execution time

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !�}�l�ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

n=50
a=0
b=1

dx=real(b-a)/real(n)

allocate(x(0:n))
do i=0,n
   x(i)=real(i)*dx
end do

low_value = 1+rank*(n/2)/size       !�̧C1
 
high_value = (rank+1)*(n/2)/size    !�̰�n/2

write(*,*)"low:",low_value,"high:",high_value,"rank:",rank

sum=0
do i=low_value,high_value
   f1=real(4)/real(1+(x(2*i-1))**2)
   f2=real(4)/real(1+(x(2*i))**2)
   sum=sum+4*f1+2*f2
end do

call MPI_REDUCE(sum,gsum,1,MPI_double,MPI_SUM,0,MPI_COMM_WORLD,ierror)

if (rank==0) then
   f0=real(4)/real(1+(x(0))**2)
   fn=real(4)/real(1+(x(n))**2)
   solution=(f0-fn+gsum)/real(3*n)
   write(*,100)"The value of pi using Simpson' Rule:", solution," with f(x)=4/(1+x^2)"
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime() !�����ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !�`����ɶ�
end if

100 format(A40,F10.6,A25)

call MPI_FINALIZE(ierror)
end program exercise12
