program matrix
implicit none
integer, dimension(2,3) :: a = reshape((/1,2,3,4,5,6/), (/ 2,3/))
integer, dimension(3,2) :: b = reshape((/1,2,3,4,5,6/), (/ 3,2/))
integer c(2,2)
integer i,j

write(*,*) 'matrix A:'
write(*,'(2(3I10,/))')  ((a(i,j),j=1,3),i=1,2)
write(*,*) 'matrix B:'
write(*,'(3(2I10,/))') ((b(i,j),j=1,2),i=1,3)

write(*,*) 'matrix C=A*B:'

call matrixx(a,2,3,b,2,c)  
write(*,'(2(2I10,/))')  ((c(i,j),j=1,2),i=1,2)
end

subroutine matrixx(a,ar,ac,b,bc,c)  
implicit none
integer i,j,k,ar,ac,bc                       
integer a(ar,ac),b(ac,bc),c(ar,bc)
do i=1,ar 
    do j=1,bc
       c(i,j)=0
       do k=1,ac 
        c(i,j)=c(i,j)+a(i,k)*b(k,j)
        end do 
    end do 
end do
end subroutine


      
   
