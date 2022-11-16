program test
implicit none
integer::n
integer,parameter::d=16
real*16::t


t=1.123456789123456789123456789123456789
n=selected_real_kind(p=33)
write(*,100)t
write(*,*)n

100 format(f20.15)
end program test
