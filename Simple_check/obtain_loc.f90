      program obtain_loc

      implicit real*8(a-h,o-z)
      character(LEN=200),dimension(:),allocatable::infilelist
      character(LEN=200):: com,name,lon,lat,elev

      call system('find input -name "*.csv" | sort > &
     &              inputfilelist.txt')

      open(11,file="inputfilelist.txt",action="read")

      nf = 0
      do
        read(11,*,iostat=nr) com
        if(nr/=0) EXIT
        nf = nf + 1
      end do

      write(*,*) "Number of input files: ", nf
      allocate(infilelist(nf))

      rewind(11)
      do nfn = 1, nf
         read(11,'(a)') infilelist(nfn)
      end do

      close(11)

      open(12,file="obs_location.csv",action="write")
      write(12,'(a,3(",",a))') "Name","Lon","Lat","Elev"

      do nfn =1, nf!2 !########################## change it to nf
         nopen = 77 + nfn
         open(nopen,file=infilelist(nfn),action="read")
         read(nopen,*) com
         nis=index(trim(infilelist(nfn)),'-')+1
         nie=index(trim(infilelist(nfn)),'.')-1
         name=trim(infilelist(nfn)(nis:nie)) 
         !write(*,*) trim(infilelist(nfn)(nis:nie))

         read(nopen,*) com,lon,lat,elev 
         write(12,'(a,3(",",a))') trim(name),trim(lon),trim(lat),trim(elev)
      enddo

      close(12)


      end program
