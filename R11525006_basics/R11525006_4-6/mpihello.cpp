#include <stdio.h> 
#include <mpi.h> 
int main(int argc, char** argv) 
{   
 int rank; 				//rank of processes
 int size;				//number of process	
 int namelen; 			//name of processes
 double elapsed_time; 	//execution time	
 char 
processor_name[MPI_MAX_PROCESSOR_NAME]; 


//Initialize the MPI enviroment 
 MPI_Init(&argc, &argv); 
 
//Start timer
MPI_Barrier(MPI_COMM_WORLD);
elapsed_time = -MPI_Wtime();

// Get the rank of the processes
 MPI_Comm_rank(MPI_COMM_WORLD, 
&rank); 

// Get the number of  the process
 MPI_Comm_size(MPI_COMM_WORLD, 
&size); 

// Get the name of the processes
MPI_Get_processor_name(processor_name, 
&namelen); 

//Print off a hello world message
 printf("Hello world! I'm rank %d of %d on %s\n"
 		, rank, size, processor_name); 
 fflush(stdout); 
 
//Finalize the MPI environment 
 MPI_Finalize(); 

//Stop timer
 elapsed_time +=MPI_Wtime();
 if (!rank)
	printf("Execution time %8.6f \n",elapsed_time); 
 return 0; 
} 
