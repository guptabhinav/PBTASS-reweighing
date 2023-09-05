program projected_free_energy 
IMPLICIT NONE

REAL*8, ALLOCATABLE :: p(:,:,:,:)
REAL*8 :: dum, pp, norm, grid2, grid4
INTEGER*8 :: i,j,k,l, nx, ny, nz, nd
nx=100
ny=100
nz=100
nd=100
 
open(1,file='input_2d')
open(2, file='PROB_4D.dat',form='unformatted',status='old')

read(1,*) nx, ny, nz, nd
read(1,*) grid2, grid4

ALLOCATE(p(nx,ny,nz,nd))

norm=0.d0
p(nx,ny,nz,nd)=0.d0

do i=1,nx
  do j=1,ny
    do k=1,nz
      do l=1,nd
      read(2) p(i,j,k,l)
      norm = norm + p(i,j,k,l)
      end do
    end do
  end do
end do

print *, 'norm =', norm
norm = norm*grid2*grid4
print *, 'norm*area =', norm

!=========calculating projections =======================


open(10,file='PROB_2D',status='replace')
do i=1,nx
   do k=1,nz
      pp=0.d0
      do j=1,ny
        do l=1,nd
        pp=pp+p(i,j,k,l)
        end do
      end do
      write(10,*) i, k, pp/norm 
    end do
    write(10,*)
end do

end program

