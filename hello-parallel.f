      program main
      include'mpif.h'
      CHARACTER * (mpi_max_processor_name) PROCESSOR_NAME
      INTEGER MYID,NP,NAMELENGTH,IERROR
      CALL mpi_init(IERROR)
      CALL mpi_comm_rank(mpi_comm_world,MYID,IERROR)
      CALL mpi_comm_size(mpi_comm_world,NP,IERROR)
      CALL mpi_get_processor_name(PROCESSOR_NAME,NAMELENGTH,IERROR)
      WRITE(*,*) 'HELLO WORLD PROCESS',MYID,'OF',NP,'ON',PROCESSOR_NAME
      CALL mpi_finalize(IERROR)
      END 
