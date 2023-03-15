subroutine matrixx(a,ar,ac,b,bc,c)  
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
return
end 

