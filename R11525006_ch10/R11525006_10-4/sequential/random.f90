FUNCTION random(iseed)
!
! When first call, iseed must be a large positive integer.
! iseed will be changed when exit and be used for next calling.
! The range of the generated random number is between 1 and -1
!
implicit none
integer, intent(inout) :: iseed
real :: random
iseed=mod(8121*iseed+28411, 134456)    ! 0 =< iseed < 134456
random=real(iseed)/134456.*2. -1.      ! -1 < random < 1
!
end FUNCTION random
