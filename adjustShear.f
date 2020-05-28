program bezzola
implicit none

! Adjustment with Bezzola's method: takes depth-averaged velocity (at elevation 0.4*h) and computes near-bed
! velocity and shear stress (tau) based on the thickness of the roughness sublayer, which is calculated based
! on substrate grain size

integer,parameter :: ik=selected_int_kind(6)
integer(kind=ik) :: m,n,i,j,eStat
character(len=255) :: cworkd,vFile,hFile,subFile,v025yrfile,vStarFile,tauFile
character(len=20),allocatable :: sub(:)
real,allocatable :: x(:) ![m]
real,allocatable :: y(:) ![m]
real,allocatable :: v(:,:) ![m/s]
real,allocatable :: h(:,:) ![m]
real,allocatable :: v025yr(:,:) ![m/s]
real,allocatable :: vstar(:,:) ![m/s]
real,allocatable :: yr(:,:) ![m]
real,allocatable :: yw(:,:) ![m]
real,allocatable :: cr(:,:) ![-]
real,allocatable :: k(:) ![m]
real,parameter :: kappa=0.41

call getcwd(cworkd)

subFile=trim(cworkd)//'\crSub.txt'
vFile=trim(cworkd)//'\vMag.txt'
hFile=trim(cworkd)//'\wd.txt'
v025yrfile=trim(cworkd)//'\v025yr.txt'
vStarFile=trim(cworkd)//'\vStar.txt'
tauFile=trim(cworkd)//'\tau.txt'

open(1,action='read',file=subFile,status='old')
open(2,action='read',file=vFile,status='old')
open(3,action='read',file=hFile,status='old')
open(4,action='write',file=v025yrfile,status='replace')
open(5,action='write',file=vStarFile,status='replace')
open(10,action='write',file=tauFile,status='replace')

write(*,*)"********************************************"
write(*,*)"Bezzola's adjustment for near-bottom depth"
write(*,*)"********************************************"
write(*,*) "Working directory: "//trim(cworkd)

! get number of observation points from substrate file, stored in n
read(1,*)  ! read header line
! continue with rest of file
n=0
do
 n=n+1
 read(1,*,end=8,iostat=eStat)
end do
8 write(*,*) adjustl("No. points:"), n-1

if (eStat == -1) then
  write(*,*) "Substrate file OK"
 else
  write(*,*) "Error reading substrate file"
  stop
endif

allocate(x(n-1))
allocate(y(n-1))
allocate(sub(n-1))
allocate(k(n-1))

! read substrate file
rewind(1)
read(1,*) ! read header line
do i=1,n-1 ! continue with rest of file
 read(1,*,end=9) x(i),y(i),sub(i)
end do
9 close(1)

if (eStat == -1) then
  write(*,*) "Substrate file read"
 else
  write(*,*) "Error reading substrate file"
  stop
endif

! get  number of time steps (rows in velocity file, stored in m)
m=0
do
 m=m+1
 read(2,*,end=10)
end do
10 write(*,*) adjustl("No. time steps:"), m-1

allocate(v(m-1,n-1))
allocate(h(m-1,n-1))
allocate(v025yr(m-1,n-1))
allocate(vstar(m-1,n-1))
allocate(yr(m-1,n-1))
allocate(yw(m-1,n-1))
allocate(cr(m-1,n-1))

! read velocity and water level files, exit status controlled with variable eStat
rewind(2)
i=0
do
 i=i+1
 read(2,*,iostat=eStat,end=11) v(i,:)
end do
11 close(2)

if (eStat == -1) then
  write(*,*) "Velocity file read"
 else
  write(*,*) "Error reading velocity file"
  stop
endif

i=0
do
 i=i+1
 read(3,*,iostat=eStat,end=12) h(i,:)
end do
12 close(3)
if (eStat == -1) then
  write(*,*) "Water depth file read"
 else
  write(*,*) "Error reading water depth file"
  stop
endif

! assign k values to observation points according to their substrate, types based on Hering et al. (2003)
do i=1,n-1
 if (sub(i) == 'Makro_Megalithal') then
    k(i)=0.4
   elseif (sub(i) == 'Mesolithal') then
    k(i)=0.13
   elseif (sub(i) == 'Mikrolithal') then
    k(i)=0.04
   elseif (sub(i) == 'Akal') then
    k(i)=0.011
   elseif (sub(i) == 'Psammal') then
    k(i)=0.001003
   elseif (sub(i) == 'Argyllal') then
    k(i)=3e-06
   elseif (sub(i) == 'Technolithal') then
    k(i)=0.4
   elseif (sub(i) == 'Xylal') then
    k(i)=0.4
   elseif (sub(i) == 'CPOM' .or. sub(i) == 'FPOM') then
    k(i)=3e-06
   elseif (sub(i) == 'Pelal') then
    k(i)=0.063/2000
   elseif (sub(i) == 'subM') then
    k(i)=.3
   elseif (sub(i) == 'Em_macrophytes') then
    k(i)=.5
   elseif (sub(i) == 'LPTP') then
    k(i)=.5
   else
    write(*,*) 'Error in k assignment',i
    stop
 endif
end do
write(*,*) 'Roughness assignment:'
do i=1,n-1
 write(*,'(2f10.3,3x,a18,3x,f4.2)') x(i),y(i),sub(i),k(i)
end do

! convert depth-averaged velocity to near bottom velocity (at 0.25*yr) using the method by Bezzola (2002)
do i=1,m-1
  do j=1,n-1
   ! I introduced this arbitrary and simple correction to avoid negative near-bottom velocites at
   ! small relative submergences (h<k), see Bezzola (2002), p160
   if (h(i,j) <= 1.75*k(j)) then
     yr(i,j)=h(i,j)
   else
     yr(i,j)=k(j)*1.75
   endif

   if (h(i,j)/yr(i,j) <= 1) then
     yw(i,j)=h(i,j)
   elseif (1 < h(i,j)/yr(i,j) .and. h(i,j)/yr(i,j) <= 3.2) then
     yw(i,j)=yr(i,j)
   else
     yw(i,j)=.31*h(i,j)
   endif

   if (h(i,j)/yr(i,j) > 2) then
     cr(i,j)=sqrt(1-(yr(i,j)/h(i,j)))
   else
     cr(i,j)=sqrt(.25*h(i,j)/yr(i,j))
   endif

   if (.4*h(i,j) <= yw(i,j)) then
     vstar(i,j)=v(i,j)/(cr(i,j)*(1/kappa*log((.4*h(i,j))/yr(i,j))+8.48))
   else
     vstar(i,j)=v(i,j)/(cr(i,j)*(1/kappa*log(yw(i,j)/yr(i,j))+8.48)+((2/3)*h(i,j)/(kappa*yw(i,j))*((1-yw(i,j)/h(i,j))**&
     &(3/2)-.465)))
   endif

  end do
  ! replace NaNs (arise when reading in zero depth values) with zeroes
  where(isnan(vstar(i,:)))
   vstar(i,:)=0
  endwhere
  write(5,*) vstar(i,:)
end do

do i=1,m-1
 do j=1,n-1
   if (.25*yr(i,j) <= yw(i,j)) then
     v025yr(i,j)=vstar(i,j)*(cr(i,j)*(1/kappa*log((.25*yr(i,j))/yr(i,j))+8.48))
   else
     v025yr(i,j)=vstar(i,j)*(cr(i,j)*(1/kappa*log(yw(i,j)/yr(i,j))+8.48)+((2/3)*(h(i,j)/(kappa*yw(i,j)))*((1-yw(i,j)/h(i,j))**&
     &(3/2)-(1-.25*yr(i,j)/h(i,j))**(3/2))))
   endif
 end do
 ! replace NaNs (result from zero depth) with zeroes
 where(isnan(v025yr))
  v025yr=0
 endwhere

 ! progress indicator
 write(*,'(a,f6.0,a)',advance='no') achar(13), real(i)/(real(m)-1.0)*100.0,"% completed"
end do

! write output files v025yr.txt and tau.txt
do i=1,m-1
 do j=1,n-1
  write(4,100,advance='no') v025yr(i,j)
  write(10,100,advance='no') ((vstar(i,j))**2)*1000
 end do
 write(4,'(a)',advance='no') achar(10)
 write(10,'(a)',advance='no') achar(10)
end do
close(4)
close(10)
100 format(f15.4)

write(*,*) achar(10),"File 'v025yr.txt' written to folder: "//trim(cworkd)//"\"
write(*,*) achar(10),"File 'tau.txt' written to folder: "//trim(cworkd)//"\"

stop
end program bezzola
