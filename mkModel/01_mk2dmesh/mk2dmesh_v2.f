      implicit real*8(a-h,o-z), integer(i-n)
      character*256 com,ofgrid,ofset,mifhead,ofmid
      integer :: mcodelat(3,2),mcodelon(3,2)
      real*8 :: lat(2),lon(2),blat(2),blon(2)
      real*8,allocatable :: xp(:,:),yp(:,:),zp(:,:,:)
      parameter( nz=3 )



!     <<<   read setfile   >>>>
      open(11,file="set_mk2dmesh_v2.dat",action="Read")
      read(11,*) com
      read(11,*) lon(1),lat(1)
      read(11,*) com
      read(11,*) lon(2),lat(2)
      read(11,*) com
      read(11,*) coeef
      read(11,*) com
      read(11,*) ofgrid
      read(11,*) com
      read(11,*) ofset
      read(11,*) com
      read(11,*) mifhead
      read(11,*) com
      read(11,*) ofmid



!     <<<   latlon and meshcode:get the start latlon   >>>>
      do i=1,2
      mcodelat(1,i)=lat(i)*60/40  ;  rest= lat(i)*60- mcodelat(1,i)*40
      mcodelat(2,i)=rest/5        ;  rest= rest     - mcodelat(2,i)*5
      mcodelat(3,i)=rest*60/30    ;

      mcodelon(1,i)=(lon(i)-100)  ;  rest= lon(i)-100-mcodelon(1,i)
      mcodelon(2,i)=rest*60/7.5   ;  rest=rest*60    -mcodelon(2,i)*7.5
      mcodelon(3,i)=rest*60/45    ;
      !write(*,*)   mcodelat(1,i),mcodelat(2,i),mcodelat(3,i)
      !write(*,*)   mcodelon(1,i),mcodelon(2,i),mcodelon(3,i)

      blon(i)=mcodelon(1,i)+100+mcodelon(2,i)*7.5/60.0   
     &                         +mcodelon(3,i)*45.0/3600.0
      blat(i)=mcodelat(1,i)/1.5+mcodelat(2,i)*5.0/60.0   
     &                         +mcodelat(3,i)*30.0/3600.0
      if(i==2) then
      blon(i)=blon(i)+45.0/3600.0
      endif
      !write(*,*) blon(i),blat(i)
      enddo

      dx=(45.0/3600.0)*coeef
      dy=(30.0/3600.0)*coeef
      nflag_lon=0
      do while(nflag_lon==0)
         blon(1)=blon(1)+dx
         if(blon(1)>=lon(1))then
           nflag_lon=1
           blon(1)=blon(1)-dx
         endif
      enddo
      nflag_lat=0
      do while(nflag_lat==0)
         blat(1)=blat(1)+dy
         if(blat(1)>=lat(1))then
           nflag_lat=1
           blat(1)=blat(1)-dy
         endif
      enddo

      nx=(lon(2)-blon(1))/dx
      ny=(lat(2)-blat(1))/dy
      restx=(lon(2)-blon(1))/dx
      resty=(lat(2)-blat(1))/dy
      if(restx/=nx*0.1) nx=nx+1
      if(resty/=ny*0.1) ny=ny+1
      sx=blon(1)
      sy=blat(1)
      !write(*,*) blon(1),blat(1)



!     <<<   create setfile   >>>
      open(111,file=trim(ofset),action='write')
      write(111,'(a)') "'Coordinate System(MIF header)'"
      write(111,'(3xa)') "'"//trim(mifhead)//"'"
      write(111,'(a)') "'NX,NY,NZ'" 
      write(111,'(3(3xi0))') nx,ny,nz
      write(111,'(a)') "'Grid File Name'" 
      write(111,'(3xa)') "'"//trim(ofgrid)//"'"
      write(111,'(a)') "'Output File Name Body (except .mif/.mid)'" 
      write(111,'(3xa)') "'"//trim(ofmid)//"'"
      close(111)



!     <<<   create grid coordinates   >>>
      nxp=nx+1
      nyp=ny+1
      nzp=nz+1
      allocate( xp(nxp,nyp),yp(nxp,nyp),zp(nxp,nyp,nzp) )

      zp(:,:,1)=-101.d0
      zp(:,:,2)=-100.d0
      zp(:,:,3)=   0.d0
      zp(:,:,4)=   1.d0

      do j=1,nyp
        do i=1,nxp
          xp(i,j)=sx+(i-1)*dx
          yp(i,j)=sy+(j-1)*dy
        enddo
      enddo

      write(*,*) ' '
      write(*,*) 'xp(  1,  1)=',xp(  1,  1)
      write(*,*) 'yp(  1,  1)=',yp(  1,  1)
      write(*,*) 'xp(nxp,nyp)=',xp(nxp,nyp)
      write(*,*) 'yp(nxp,nyp)=',yp(nxp,nyp)



!     <<< write to grid file   >>>
      com=ofgrid
      open(11,file=trim(com)//"_lonlat")
      write(11,'(a)') "   'lon_start lat_start lon_end lat_end'"
      write(11,'(4(3x,f20.15))') sx,sy,xp(nxp,nyp),yp(nxp,nyp)
      close(11)



!     <<< write to grid file   >>>
      open(10,file=trim(com))
      open(20,file=trim(com)//'.xym')
      open(11,file=trim(com)//"_NumGridReg")
      write(11,'(a)') "   'NX   NY   NZ'"
      write(11,'(3(3x,i0))') nx,ny,nz
      close(11)

      write(*,*) 'output: ',trim(com)

      dst=1000.d0*coeef

      write(10,'(a)') "'X'"
      write(20,'(a)') "'X'"
      do j=1,nyp
        do i=1,nxp
          write(10,*) xp(i,j)
          write(20,*) dst*(i-1)
        enddo
      enddo

      write(10,'(a)') "'Y'"
      write(20,'(a)') "'Y'"
      do j=1,nyp
        do i=1,nxp
          write(10,*) yp(i,j)
          write(20,*) dst*(j-1)
        enddo
      enddo

      write(10,'(a)') "'Z'"
      write(20,'(a)') "'Z'"
      do k=1,nzp
        do j=1,nyp
          do i=1,nxp
            write(10,*) zp(i,j,k)
            write(20,*) zp(i,j,k)
          enddo
        enddo
      enddo
      close(10)
      close(20)

      write(*,*) 'done.'

      stop
      end
