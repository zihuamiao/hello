program mpi_matrix_multiply
    use mpi
    implicit none
    
    integer :: rank, size, i, j, k, ierr
    double precision :: start_time, end_time
    integer, parameter :: N = 1000
    double precision, dimension(N, N) :: a, b, c
    
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

    ! 初始化矩阵
    a = reshape([(mod(i+j, 10), i=1,N,j=1,N)], [N, N])
    b = reshape([(mod(i-j, 10), i=1,N,j=1,N)], [N, N])
    c = 0.0d0

    start_time = MPI_Wtime()

    ! 将矩阵分割成块
    integer :: block_size = N / size
    double precision, dimension(block_size, N) :: a_block, b_block, c_block
    call MPI_Alltoall(a(block_size*(rank*size:(rank+1)*size-1)+1, 1:N), &
    &block_size*N, MPI_DOUBLE_PRECISION, a_block, block_size*N, &
    &MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, ierr)
    call MPI_Alltoall(b, block_size*N, MPI_DOUBLE_PRECISION, b_block, &
    &block_size*N, MPI_DOUBLE_PRECISION, MPI_COMM_WORLD, ierr)

    ! 计算自己的块
    c_block = sum(a_block(:, :, 1) * b_block(:, :, 1:size), dim=3)

    ! 收集所有块
    call MPI_Allreduce(c_block, c, N*N, MPI_DOUBLE_PRECISION,&
    & MPI_SUM, MPI_COMM_WORLD, ierr)

    end_time = MPI_Wtime()

    ! 输出计算结果和耗时
    if (rank == 0) then
        write(*, *) "Result:"
        write(*, "(1000f10.2)") c
        write(*, *) "Time: ", end_time - start_time, " seconds"
    end if

    call MPI_Finalize(ierr)
end program mpi_matrix_multiply


