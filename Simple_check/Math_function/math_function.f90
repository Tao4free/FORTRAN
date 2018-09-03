        program main

        implicit real(kind=8) (a-h,o-z)
        
        PI=4.D0*DATAN(1.D0)
        x=45.0
  
        x_rad=45.0*PI/180.0
        s=tan(x_rad)
        write(*,*) s

        end
