      program y

      implicit real(a-h, o-z), integer(i-n)

      character(len=132)::com, iffac, ifflood, of
      character(len=132)::id
      real:: lat1, lon1, lat2, lon2, a, c, distance,d
      real:: lat1_d, lon1_d, lat2_D, lon2_D
      real :: pi, R 

      thold = 20.0 !km
      sd = thold / 4.0

      !pi = 4.D0*DATAN(1.D0)

      !R = 6373.0

      !lat1_d = 52.2296756
      !lon1_d = 21.0122287
      !lat2_d = 52.406374
      !lon2_d = 16.9251681
      !lat1 = ( pi * lat1_d ) / 180.0
      !lon1 = ( pi * lon1_d ) / 180.0
      !lat2 = ( pi * lat2_d ) / 180.0
      !lon2 = ( pi * lon2_d ) / 180.0

      !dlon = lon2 - lon1
      !dlat = lat2 - lat1

      !a = sin(dlat / 2.0)**2 + & 
     !&    cos(lat1) * cos(lat2) * sin(dlon / 2.0)**2
      !c = 2.0 * atan2(sqrt(a), sqrt(1.0 - a))

      !distance = R * c
      !d = haversine(lat1_d,lon1_d,lat2_d,lon2_d) ! BNA to LAX

      !write(*,*) distance,d

      open(11,file="set_getnearflood_dist_wt.dat",action="read")
      read(11,*) com, iffac, nfac
      read(11,*) com, ifflood, nflood
      read(11,*) com, of

      write(*,*) trim(iffac), trim(ifflood)

      open(12,file=trim(ifflood),action="read")
      open(22,file=trim(iffac),action="read")
      read(22,*) com
      shift = 1.0
      do ifac = 1, 5!nfac
        read(22,*) num, id, lat1, lon1
        write(*,*) num, trim(id), lat1, lon1

        lat1_d_plus  = lat1_d + shift
        lat1_d_minus = lat1_d - shift
        lon1_d_plus  = lon1_d + shift
        lon1_d_minus = lon1_d - shift

        read(12,*) com
          do iflood = 1, nflood
             read(12,*) lon2, lat2, v
             if(lat2_d < lat1_d_minus .or. lat2_d > lat1_d_plus .or. & 
     &          lon2_d < lon1_d_minus .or. lon2_d > lon1_d_plus) then
                 cycle             
             endif
             dis = haversine(lat1,lon1,lat2,lon2)
             if(dis <= thold) then
                w = exp(-1*dis**2/(2*sd**2))
                v_wt = v*w  
                write(*,*) dis, w, v, v_wt
             endif
          enddo
        rewind(12)

      enddo

      close(11)
      close(12)
      close(22)


      contains
       
          function to_radian(degree) result(rad)
              ! degrees to radians
              real,intent(in) :: degree
              real, parameter :: deg_to_rad = atan(1.0)/45 
              real :: rad
          
              rad = degree*deg_to_rad !4.D0*DATAN(1.D0)*degree/180.0 !degree*deg_to_rad
          end function to_radian
          
          function haversine(deglat1,deglon1,deglat2,deglon2) result (dist)
              ! great circle distance -- adapted from Matlab 
              real,intent(in) :: deglat1,deglon1,deglat2,deglon2
              real :: a,c,dist,dlat,dlon,lat1,lat2
              real,parameter :: radius = 6373.0 
          
              dlat = to_radian(deglat2)-to_radian(deglat1)
              dlon = to_radian(deglon2)-to_radian(deglon1)
              lat1 = to_radian(deglat1)
              lat2 = to_radian(deglat2)
              a = (sin(dlat/2.0))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2.0))**2
              c = 2*asin(sqrt(a))
              dist = radius*c
          end function haversine


      end program
