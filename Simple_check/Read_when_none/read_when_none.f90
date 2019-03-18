        program read_when_none

        implicit real(a-h,o-z), integer(i-n)

        open(7,file="sample.txt",action="read")
        read(7,*,iostat=nr) i1, i2, f3
        write(*,*) i1,i2,f3

        end
