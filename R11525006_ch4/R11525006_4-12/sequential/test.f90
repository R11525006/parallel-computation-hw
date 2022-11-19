program exerciseT
implicit none
integer::n,a,b
real*8,allocatable,dimension(:)::x
real*8::dx
real*8::sum
real*8::solution
real*8::f0,f1,f2,fn
integer::i

b=1
a=0
n=50
dx=real(b-a)/real(n)

allocate(x(0:n))
do i=0,n
   x(i)=real(i)*dx
end do

!write(*,100)(x(i),i=0,n)

f0=real(4)/real(1+(x(0))**2)
fn=real(4)/real(1+(x(n))**2)

sum=0
do i=1,n/2
   f1=real(4)/real(1+(x(2*i-1))**2)
   f2=real(4)/real(1+(x(2*i))**2)
   sum=sum+4*f1+2*f2
end do

solution=(f0-fn+sum)/real(3*n)

write(*,100)solution

100 format(F24.16)
end program

