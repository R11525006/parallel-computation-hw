program exercise
integer::i,j,k  !loop index
integer::max    !max power
real*8::stp    !2^n-1
integer*8::largest                    !sqrt(2^n-1)
!integer,allocatable,dimension(:)::n   ! record 2^n-1 is prime
integer::n
integer(kind=8)::perfect_number
!integer(kind=8),allocatable,dimension(:)::perfect_number
integer(kind=8)::test

max=100
!allocate(n(1:8))
!allocate(perfect_number(1:8))           
k=1
outer: do i=2,max

   stp=2**(i)-1
   largest=int(sqrt(stp))
   inner: do j=3,largest,2
      if (mod(int(stp),j)==0) cycle outer
   end do inner
     
   n=i
   perfect_number=(2**(i)-1)*(2**(i-1))
   k=k+1
   
   write(*,*)
   write(*,*)"number",k,"perfect number n=",n
   write(*,*)"Perfect numbers="
   write(*,100)perfect_number

   if (k >8)exit
         
end do outer

test=2147483647
write(*,*)test
100 format(I20)
end program exercise
