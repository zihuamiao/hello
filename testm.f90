program main
include "mpif.h"
integer MAX_ROWS,MAX_COLS, rows, cols
parameter (MAX_ROWS=4, MAX_COLS=4)
real *8::Time_begin, Time_end !计算时间
double precision a(MAX_ROWS, MAX_COLS),b(MAX_COLS,MAX_ROWS),c(MAX_ROWS,MAX_ROWS)
double precision buffer (MAX_COLS), ans
integer myid, master, numprocs, ierr, status(MPI_STATUS_SIZE)
integer i,j,numsent, numrcvd, sender
integer anstype, row
call MPI_INIT(ierr)
Time_begin=MPI_Wtime()
call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)
master=0
rows=4
cols=4
if (myid .eq. master) then
!主进程对矩阵A和B赋初值
do i=1,cols
 do j=1,rows
 a(i,j)=i+j
 b(i,j)=i*j
 end do
end do
numsent=0
numrcvd=0
!将矩阵B发送给所有其它的从进程 通过下面的广播语句实现
call MPI_BCAST(b,cols*rows,MPI_DOUBLE_PRECISION,&
&master,MPI_COMM_WORLD, ierr)
!依次将矩阵A的各行发送给其它的numprocs-1个从进程
do i=1,min(numprocs,rows)
 do j=1,cols
! 将一行的数据取出来依次放到缓冲区中
buffer(j)=a(i,j)
 end do
! 将准备好的一行数据发送出去
 call MPI_SEND(buffer, n, MPI_DOUBLE_PRECISION,i,&
 &i,MPI_COMM_WORLD, ierr)
 numsent=numsent+1
end do
do i=1,row
!对所有的行 依次接收从进程对一行数据的计算结果
 call MPI_RECV(ans, n,MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE,&
 & MPI_ANY_TAG,MPI_COMM_WORLD, status, ierr)
 sender=status(MPI_SOURCE)
 anstype=status(MPI_TAG)
!将该行数据赋给结果数组C的相应单元
 c(i,anstype)=ans
 if (numsent .lt. rows) then
! 如果还有其它的行没有被计算 则继续发送
do j=1,cols
! 准备好新一行的数据
 buffer(j)=a(numsent+1,j)
end do
! 将该行数据发送出去
call MPI_SEND(buffer,cols, MPI_DOUBLE_PRECISION, sender,&
 & numsent+1,MPI_COMM_WORLD, ierr)
numsent=numsent+1
 
 end if
!else
! 下面为从进程的执行步骤 首先是接收数组B
call MPI_BCAST(b,cols*rows,MPI_DOUBLE_PRECISION,master,&
 & MPI_COMM_WORLD, ierr)
 ! 接收主进程发送过来的矩阵A一行的数据
90 call MPI_RECV(buffer,cols, MPI_DOUBLE_PRECISION, master,&
 &MPI_ANY_TAG, MPI_COMM_WORLD, status,ierr)
!若接收到标识为0的消息 则退出执行

! 计算一行的结果 并将结果发送给主进程
 call MPI_SEND(ans, n, MPI_DOUBLE_PRECISION, master, row,&
 &MPI_COMM_WORLD, ierr)
 goto 90
end do
!程序运行时间
call MPI_Barrier(MPI_COMM_WORLD,ierr)
Time_end=MPI_Wtime()
if(myid .eq. 0) print*, 'Total time：', Time_end-Time_begin
call MPI_FINALIZE(ierr)
end if
end program
