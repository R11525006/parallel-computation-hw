program exercise9
implicit none
include 'mpif.h'
integer::rank,size,ierror
integer::a
integer::low_value,high_value,psize
integer::prime,first,count,gcount
integer,allocatable,dimension(:)::marked
integer::i,index
integer::gap,largestgap,glargestgap  !gap between a pair of cosecutive prime numbers
integer::fp,bp           
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
 !  if (marked(i)==0) then
 !  count = count +1
 !  end if
!end do

!�������X��prime
!call MPI_REDUCE(count,gcount,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD,ierror) 

!if (rank==0) then
 !  write(*,*)"There are",gcount,"primes less than or equal to",a,"from rank",rank
!end if

largestgap=0
fp=low_value !�B�z�����̧C����

do i=0,psize-1       !�p��ۤv���d�򤺨�ӽ�Ƴ̤j���t�Z
   if(marked(i)==0) then         
      bp=low_value+i      !�N�o�쪺��Ƽg�� backward prime
      gap=bp-fp           !backward prime - forward prime =gap
      fp=bp               !backward prime �ܦ� forward prime ���ѵ��U�@��prime�p��

      if(gap>largestgap) then    
         largestgap=gap            !�p�Ggap�ܤj���Nlargest gap
      end if      
    !  if (rank==1) then                                                  !���յ{�ǬO�_���~!
    !     write(*,*)"fp",fp,"bp",bp,"gap",gap,"largest",largestgap
    !  end if
   end if
end do
   
!����Ҧ��B�z�����̤j���t�Z
call MPI_REDUCE(largestgap,glargestgap,1,MPI_INT,MPI_MAX,0,MPI_COMM_WORLD,ierror)  !MPI_MAX��X�̤j�� ,MPI_MAXLOC ��X�̤j�ȩM���m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
finish=MPI_Wtime() !�����ɶ�!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!


if (rank==0) then
   write(*,"(A60,I4,1x,A30,I8,1x,A10,I2)")"The largest gap between a pair of consecutive prime numbers:"&
              & ,glargestgap,"for all integers less than:",a,"from rank:",rank

    write(*,"(A25,f10.6,A20,I3)")"execution time:",finish-start,"processor numbers:",size    !�`����ɶ�
end if


call MPI_FINALIZE(ierror)
end program exercise9
