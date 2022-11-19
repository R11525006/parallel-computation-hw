program exercise8
implicit none
include 'mpif.h'
integer::rank,size,ierror
integer::a
integer::low_value,high_value,psize
integer::prime,first,count,gcount
integer,allocatable,dimension(:)::marked
integer::i,index
integer::cobp,gcobp  !consecutive odd integers are both prime
real*8::start,finish                !execution time

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size ,ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank ,ierror)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
start=MPI_Wtime()  !�}�l�ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!write(*,*)"rank",rank
a=1000000

low_value = 2+rank*(a-1)/size      !�C�ӳB�z�����쪺�̤p��

if (rank>0) then
   low_value = low_value-2         !���F�B�z��0�H�~�A�̤p��-2�A�ت��O���F�л\�W�@�ӳB�z��
end if                             !�Ҥ��쪺�̫��ӭȡA�Y����Ȥ�����ƫh�L���ǰe�T�����U
                                   !�@�ӳB�z���A�`�ٷ��q�ɶ��C
  
high_value = 1+(rank+1)*(a-1)/size !�C�ӳB�z�����쪺�̤j��

psize=high_value-low_value+1       !�C�ӳB�z���Ҥ��쪺�Ȫ��ƶq

!write(*,*)"low",low_value,"high",high_value,"psize",psize,"rank:",rank


allocate(marked(0:psize-1))
do i=0,psize-1
   marked(i)=0 !�B�z�ƶq�j�p���x�}��l��
end do

if (rank == 0)then
   index=0
end if

prime=2 !��Ʊq2�}�l

do while (prime*prime <= a)
   if (prime*prime > low_value) then
      first = prime*prime-low_value
      else if (mod(low_value,prime)==0) then
         first =0;
         else
            first=prime-mod(low_value,prime)
         end if
         
         do i=first,psize-1,prime
            marked(i)=1;
         end do
         
         if (rank ==0) then   !rank0�M��U�@��prime
            do i=0,psize-1                  
               if (marked(i)==0) then   !��X���Q�аO���Ʀr
                  if (i+2 > prime) then !�ӼƦr��ثe��prime�٤j
                     prime=i+2          !�q2�}�l                         
                     exit      !����������X�j��
                  end if
               end if
            end do
         end if
         !�q���Ҧ�rank prime�w��s
         call   MPI_BCAST(prime, 1, MPI_INT, 0, MPI_COMM_WORLD, IERROR)    
end do

!count =0
!do i=0,psize-1          !�p��ۤv���d�򤺦��X��prime
  ! if (marked(i)==0) then
  ! count = count +1
  ! end if
!end do

!�������X��prime
!call MPI_REDUCE(count,gcount,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierror) 

!if (rank==0) then
  ! write(*,*)"There are",gcount,"primes less than or equal to",a,"from rank",rank
!end if

cobp=0            
do i=2,psize-1          !�p��ۤv���d�򤺦��X�ӳs��_�ƹﳣ�O���
   if((marked(i)==0).and.(marked(i-2)==0)) then
      cobp=cobp+1
   end if
end do
   

!�������X��cobp
call MPI_REDUCE(cobp,gcobp,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierror) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime() !�����ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (rank==0) then
   write(*,"(A80,I5,1X,A30,I8,1X,A10,I2)")"The number of times that two consecutive odd integers are both prime:"&
              & ,gcobp,"for all integers less than:",a,"from rank:",rank
   
   write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size   !�`����ɶ�
end if


call MPI_FINALIZE(ierror)
end program exercise8
