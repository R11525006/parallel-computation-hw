#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv) 
{
	int rank;					//rank of processes
	int size;					//number of process	
	double elapsed_time;    	//execution time
	int	n;						//maximum number
	int i; 						//Loop index
	long long local_sum=0;			//sum value of each process
	long long global_sum=0;			//collect local_sum
	
	//Initialize the MPI enviroment 
 	MPI_Init(&argc, &argv); 
 
	//Start timer
	MPI_Barrier(MPI_COMM_WORLD);
	elapsed_time = -MPI_Wtime();

	// Get the rank of the processes
 	MPI_Comm_rank(MPI_COMM_WORLD, &rank); 

	// Get the number of  the process
 	MPI_Comm_size(MPI_COMM_WORLD, &size); 
 	
 	//assume n=1000
 	n=1000;
 	
 	// rank (0 ~ size-1) => value(1~size) +value(size+1~2size)+..............
 	for(i = rank+1 ;i <= n ; i += size){
	   
		local_sum +=i; 
		                               }
	
	//collect local_sum by rank 0 
 	MPI_Reduce(&local_sum,&global_sum,1,MPI_INT,MPI_SUM,0,MPI_COMM_WORLD);
 			//  local	, global , numbers , type , opetation, target , invite)
	
	//Finallize the MPI enviroment
	MPI_Finalize();
	
	//result
	if (!rank){
		local_sum = n*(n+1)/2;
		printf("\n \
			    The value of n was specified as : %d \n \
				The value obtained by summing 1+2+...+n is : %d \n \
			    The value of n(n+1)/2 is : %d \n \
				execution time = %8.6f"\
				,n,global_sum,local_sum,elapsed_time+MPI_Wtime());
	           }
	return 0;	
	
	}	
