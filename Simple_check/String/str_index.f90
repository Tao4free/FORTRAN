      program test_str_index

      implicit none
      
      integer::i1,i2,i3,i4
      character(len=120)::com


      com='GEO_AC_Wrank_1'
      i1=index(com,'_')
      write(*,*) i1
      i2=index(com,'W')
      write(*,*) i2
      i3=index(com,'_Wrank')
      write(*,*) i3
      i4=index(com,'_X')
      write(*,*) i4

      write(*,*) "'",trim(com(i1:i4)),"'"

      if(trim(com(i1:i4))=='') then
          write(*,*) 'YES'
      endif

      end
      
