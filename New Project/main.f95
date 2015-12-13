program triang
  implicit none
  real, dimension (3,3) :: a
  real, dimension (3) :: b
  integer :: n,l,i,j,k
  real :: r,p
  real, dimension (3,4) :: q
  a(1,1)=7
  a(1,2)=1
  a(1,3)=1
  a(2,1)=42
  a(2,2)=12
  a(2,3)=4
  a(3,1)=14
  a(3,2)=24
  a(3,3)=8
  b(1)=2
  b(2)=2
  b(3)=1
  n=3
  l=n+1
  i=1
  j=1
  do while (i.le.n)
     do while (j.le.n)
     q(i,j)=a(i,j)
     j=j+1
     enddo
     j=1
     i=i+1
  enddo
  
  i=1
  j=l
  do while (i.le.n)
     q(i,j)=-b(i)
     i=i+1
  enddo
  
  k=1
  do while (k.le.n)
     p=q(k,k)
     
     i=k+1
     do while(i.le.n)
        r=q(i,k)
        
        q(i,k)=0
        j=k+1
        do while(j.le.l)
        
           q(i,j)=q(i,j)-q(k,j)*(r/p)
           j=j+1
        enddo
        i=i+1
     enddo
     k=k+1
  enddo

  i=1
  j=1
   do while (i.le.n)
     do while (j.le.n)
     a(i,j)=q(i,j)
     j=j+1
     enddo
     j=1
     i=i+1
  enddo
  
  i=1
  j=l
  do while (i.le.n)
     b(i)=-q(i,j)
     i=i+1
  enddo
  
  print*, "a=",a,"b=",b
  
end program

