program Matrix_operation
!implicit none
USE MPI
integer, parameter::N=8!矩阵大小
real *8::Time_begin, Time_end !计算时间
integer::myid, ierr, numprocs,status(MPI_STATUS_SIZE)
integer NP !extra!NP=N/numprocs
integer, dimension(n,n) ::b,c,a
integer::buffer(1,n),a_k(2,n),c_k(2,n)
integer i, j, k,cc,ii,sender,anstype,jj
!每个进程中的动态数组，配合MPI_Comm_size使用，即可以不用事先知道进程数
!integer,dimension(:,:),allocatable::C_k
call MPI_INIT(ierr)
Time_begin=MPI_Wtime()
call MPI_Comm_Rank(MPI_COMM_WORLD, myid, ierr)!得到当前进程标识符
call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierr)!得到通讯域包含的进程数
if (myid .eq. 0) then
do i=1, N
    do j=1,N
       A(i,j)=i+j
    enddo
enddo
do i=1, N
    do j=1,N
        B(i,j)=i*j
    enddo
enddo
 c=matmul(a,b)
write(*,*) 'matrix A:'
write(*,'(8(8I10,/))')  ((a(i,j),j=1,N),i=1,N)
write(*,*) 'matrix B:'
write(*,'(8(8I10,/))')  ((b(i,j),j=1,N),i=1,N)
write(*,*) 'matrix c:'
write(*,'(8(8I10,/))')  ((c(i,j),j=1,N),i=1,N)
end if
call MPI_BCAST(b,n*n,MPI_INT,&
&0,MPI_COMM_WORLD, ierr)
!if(myid .eq. 0) print*,'numprocs',numprocs
NP=N/numprocs

if(myid .eq. 0)then

do i=1,numprocs
do j=1,np
a_k(j,:)=a(j+(i-1)*np,:)
enddo
 call MPI_SEND(a_k, np*n, MPI_int,i-1,&
 &0,MPI_COMM_WORLD, ierr)
 enddo
  print*,'numprocs',numprocs
 endif
 !  接收主进程发送过来的矩阵A一行的数据
call MPI_RECV(a_k, np*n, MPI_int,0,&
 &0,MPI_COMM_WORLD, MPI_STATUS_IGNORE,ierr)
 do i=1,np
 do j=1,n
 c_k(i,j)=0
 do k=1,n
 c_k(i,j)=c_k(i,j)+a_k(i,k)*b(k,j)
 enddo
 enddo
 enddo
 print*,'process',myid,'c_k',&
& ((c_k(i,j),j=1,N),i=1,Np)
!call mpi_allreduce(c_k,c)
!程序运行时间
!call MPI_Barrier(MPI_COMM_WORLD,ierr)
Time_end=MPI_Wtime()
if(myid .eq. 0) print*, 'Total time：', Time_end-Time_begin


call MPI_Finalize(ierr)
end program Matrix_operation

