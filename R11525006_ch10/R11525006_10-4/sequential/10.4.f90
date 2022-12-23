program exercise4
implicit none

integer,parameter::max=100000                        !number of points
real,dimension(max)::x,y,z                           !generate coordinate x,y,z
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
integer::n=0
!!!!!!!!!!!!!!!!!!!!!!!!!!portion of cube remains
real::portion
iseed=20
do i=1,max
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

write(*,*)
write(*,*)n
portion=n/real(max)*100.

write(*,*)
write(*,100)portion
100 format("The volume of the portion of the cube remains is",f6.2,"%")
end program exercise4
