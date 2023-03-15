program matrix
implicit none
integer::i,j,m,n,k
integer, dimension(2,3) :: a = reshape((/1,2,3,4,5,6/), (/ 2,3/))
integer, dimension(3,2) :: b = reshape((/1,2,3,4,5,6/), (/ 3,2/))
INTEGER c(2,2)
       c=MATMUL(a,b)
write(*,*) 'matrix A:'
write(*,'(2(3I10,/))')  ((a(i,j),j=1,3),i=1,2)
write(*,*) 'matrix B:'
write(*,'(3(2I10,/))') ((b(i,j),j=1,2),i=1,3)
write(*,*) 'matrix C=A*B:'
write(*,'(2(2I10,/))')  ((c(i,j),j=1,2),i=1,2)
 end  
