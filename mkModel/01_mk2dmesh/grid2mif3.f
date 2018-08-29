      implicit real*8 (a-h,o-z)

      real*8 time
      character chr
      character*80 fgrid,fmif
      character*256 ccoord

      real*8 , allocatable :: X(:,:)
      real*8 , allocatable :: Xg(:,:)
      real*8 , allocatable :: Y(:,:)
      real*8 , allocatable :: Yg(:,:)
      real*8 , allocatable :: Z(:,:,:)

      call ReadParams(nx,ny,nz,fgrid,fmif,ccoord)

      open(9,file=trim(fgrid),SHARED)

      nxp=nx+1
      nyp=ny+1
      nzp=nz+1
      nmesh=nx*ny*nz

      allocate( X(nxp,nyp) )
      allocate( Xg(nx,ny) )
      allocate( Y(nxp,nyp) )
      allocate( Yg(nx,ny) )
      allocate( Z(nxp,nyp,nzp) )

      read(9,'(a)') chr
      read(9,*) ((X(i,j),i=1,nxp),j=1,nyp)
      read(9,'(a)') chr
      read(9,*) ((Y(i,j),i=1,nxp),j=1,nyp)
      read(9,'(a)') chr
      read(9,*) (((Z(i,j,k),i=1,nxp),j=1,nyp),k=1,nzp)

c---------------------
c open output file
c---------------------

      do j=1,ny
        do i=1,nx
          Xg(i,j)=(X(i,j)+X(i+1,j)+X(i,j+1)+X(i+1,j+1))/4.d0
          Yg(i,j)=(Y(i,j)+Y(i+1,j)+Y(i,j+1)+Y(i+1,j+1))/4.d0
        enddo
      enddo

      open(20,file=trim(fmif)//'_reg.mif')

      write(20,'(a)') 'Version 450'
      write(20,'(a)') 'Charset "WindowsJapanese"'
      write(20,'(a)') 'Delimiter ","'
      write(20,'(a)') ccoord
      write(20,'(a)') 'Columns 4'
      write(20,'(a)') '  Ip Integer'
      write(20,'(a)') '  Jp Integer'
      write(20,'(a)') '  Xc Float'
      write(20,'(a)') '  Yc Float'
      write(20,'(a)') 'Data'
      write(20,*)

      do j=1,ny
        do i=1,nx
          write(20,'(a9)') 'Region  1'
          write(20,'(a)')  '  5'
          write(20,'(2f14.5)') x(i  ,j  ),y(i  ,j  )
          write(20,'(2f14.5)') x(i+1,j  ),y(i+1,j  )
          write(20,'(2f14.5)') x(i+1,j+1),y(i+1,j+1)
          write(20,'(2f14.5)') x(i,j+1  ),y(i,j+1  )
          write(20,'(2f14.5)') x(i  ,j  ),y(i  ,j  )
          write(20,'(a)')          '    Pen (17,2,0)'
          write(20,'(a)')          '    Brush (1,0,16777215)'
          write(20,'(a10,2f14.5)') '    Center',xg(i,j),yg(i,j)
        enddo
      enddo

      close(20)


      open(20,file=trim(fmif)//'_p.mif')

      write(20,'(a)') 'Version 450'
      write(20,'(a)') 'Charset "WindowsJapanese"'
      write(20,'(a)') 'Delimiter ","'
      write(20,'(a)') ccoord
      write(20,'(a)') 'Columns 5'
      write(20,'(a)') '  Ip Integer'
      write(20,'(a)') '  Jp Integer'
      write(20,'(a)') '  Xp Float'
      write(20,'(a)') '  Yp Float'
      write(20,'(a)') '  Zp Float'
      write(20,'(a)') 'Data'
      write(20,*)

      do j=1,nyp
        do i=1,nxp
          write(20,'(a,2e)') 'Point ',X(i,j),Y(i,j)
          write(20,'(a)')  '      Symbol (33,0,8)'
        enddo
      enddo

      close(20)

      open(20,file=trim(fmif)//'_cntp.mif')

      write(20,'(a)') 'Version 450'
      write(20,'(a)') 'Charset "WindowsJapanese"'
      write(20,'(a)') 'Delimiter ","'
      write(20,'(a)') ccoord
      write(20,'(a)') 'Columns 5'
      write(20,'(a)') '  Ip Integer'
      write(20,'(a)') '  Jp Integer'
      write(20,'(a)') '  Xc Float'
      write(20,'(a)') '  Yc Float'
      write(20,'(a)') '  Zc Float'
      write(20,'(a)') 'Data'
      write(20,*)

      do j=1,ny
        do i=1,nx
          write(20,'(a,2e)') 'Point ',Xg(i,j),Yg(i,j)
          write(20,'(a)')  '      Symbol (33,0,8)'
        enddo
      enddo

      close(20)

      open(20,file=trim(fmif)//'_line.mif')

      write(20,'(a)') 'Version 450'
      write(20,'(a)') 'Charset "WindowsJapanese"'
      write(20,'(a)') 'Delimiter ","'
      write(20,'(a)') ccoord
      write(20,'(a)') 'Columns 1'
      write(20,'(a)') '  Dir Char(6)'
      write(20,'(a)') 'Data'
      write(20,*)

      do j=1,nyp
         write(20,'(a,i)') "Pline ",nxp
         do i=1,nxp
            write(20,'(2e)') X(i,j),Y(i,j)
         enddo
      enddo

      do i=1,nxp
         write(20,'(a,i)') "Pline ",nyp
         do j=1,nyp
            write(20,'(2e)') X(i,j),Y(i,j)
         enddo
      enddo

      close(20)


      open(20,file=trim(fmif)//'_reg.mid')
      do j = 1, ny
        do i = 1, nx
          write(20,'(i0,a1,i0,2(a1,f0.5))') i,',',j,',',
     -          xg(i,j),',',yg(i,j)
        enddo
      enddo
      close(20)

      open(20,file=trim(fmif)//'_p.mid')
      do j = 1, nyp
        do i = 1, nxp
          write(20,'(i0,a1,i0,3(a1,f0.5))') i,',',j,',',
     -          x(i,j),',',y(i,j),',',-z(i,j,3)
        enddo
      enddo
      close(20)

      open(20,file=trim(fmif)//'_cntp.mid')
      do j = 1, ny
        do i = 1, nx
           dtmp=z(i,j,3)+z(i+1,j,3)+z(i,j+1,3)+z(i+1,j+1,3)
           dtmp=-dtmp/4.d0
           write(20,'(i0,a1,i0,3(a1,f0.5))') i,',',j,',',
     -          x(i,j),',',y(i,j),',',dtmp
        enddo
      enddo
      close(20)

      open(20,file=trim(fmif)//'_line.mid')
      do j = 1, nyp
         if(j==1)then
            write(20,'(a)') "J0"
         elseif(j==nyp)then
            write(20,'(a)') "J1"
         else
            write(20,'(a)') "0"
         endif
      enddo
      do i = 1, nxp
         if(i==1)then
            write(20,'(a)') "I0"
         elseif(i==nxp)then
            write(20,'(a)') "I1"
         else
            write(20,'(a)') "0"
         endif
      enddo
      close(20)


      end

c-----------------------------------------------------------------------

      subroutine ReadParams(nx,ny,nz,fgrid,fmif,ccoord)
      character*80 fgrid,fmif
      character*256 ccoord
      character chr
      integer i

      open(8,file='grid2mif.txt',status='old',err=888)
      read(8,'(a)') chr
      read(8,*) ccoord
      read(8,'(a)') chr
      read(8,*) NX,NY,NZ
      read(8,'(a)') chr
      read(8,*) fgrid
      read(8,'(a)') chr
      read(8,*) fmif
      close(8)
      goto 100

  888 write(*,*) 'grid2mif.txt not found.'
      stop 1818
  100 end

