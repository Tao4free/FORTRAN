        program numberriver

        implicit real(a-h,o-z), integer(i-n)

        character(len=132)::inf, cmd, com
        integer,allocatable::rivflag(:,:),oldrivflag(:,:)
        integer:: brach(2,2,100), temp(2,4)

        nx    = 1652; ny    = 2353

        allocate(rivflag(nx,ny))
        allocate(oldrivflag(nx,ny))
        rivflag = 0
        oldrivflag = 0

        inf = "riverflag.csv"
        write(cmd,'(3a)') "wc -l < ", trim(inf), " > num" 
        !write(*,*) trim(cmd)
        CALL nc=SYSTEM(trim(cmd))
        open(7,file="num",action="read")
        read(7,*) nc
        CALL nc=SYSTEM("rm -rf num")


        nc = nc - 1
        open(11, file="riverflag.csv", action="read") 
        read(11,*) com
        do i = 1, nc
            read(11,*) ig, jg 
            !write(*,*) ig, jg 
            rivflag(ig,jg) = 1
            !write(*,*) ig, jg, rivflag(ig,jg)
            if(ig==178 .and. jg == 73) then
                write(*,*) ig, jg
            endif
        enddo

        nu = 9000

        !open(13,file="asaishi.csv",action="write")
        !open(13,file="iwaki2.csv",action="write")
        !open(13,file="hira2.csv",action="write")
        open(13,file="asaishi2.csv",action="write")
        ix  =  976; iy  = 536
        !ix  =  870; iy  = 619
        !ix  =  976; iy  = 536
        oldrivflag(ix,iy) = 1
        flag = 1
        do nnn = 1, 9999999
            nn = 0
            nfront = 0
            n1 = 0
            n2 = 0
            n3 = 0
            n4 = 0
            i = ix -1; j = iy
            if(rivflag(i,j)==1 .and. oldrivflag(i,j) /= 1) then
                nfront = nfront + 1
                n1 = 1
            endif
            !if(nnn==2898) then
            !write(*,*) i,j,rivflag(i,j), oldrivflag(i,j),n4
            !endif   
            i = ix +1; j = iy
            if(rivflag(i,j)==1 .and. oldrivflag(i,j) /= 1) then
                nfront = nfront + 1
                n2 = 1
            endif
            !if(nnn==2898) then
            !write(*,*) i,j,rivflag(i,j), oldrivflag(i,j),n4
            !endif   
            i = ix   ; j = iy + 1
            if(rivflag(i,j)==1 .and. oldrivflag(i,j) /= 1) then
                nfront = nfront + 1
                n3 = 1
            endif
            !if(nnn==2898) then
            !write(*,*) i,j,rivflag(i,j), oldrivflag(i,j),n4
            !endif   
            i = ix   ; j = iy - 1
            if(rivflag(i,j)==1 .and. oldrivflag(i,j) /= 1) then
                nfront = nfront + 1
                n4 = 1
            endif
            !if(nnn==2898) then
            !write(*,*) i,j,rivflag(i,j), oldrivflag(i,j),n4
            !endif   


            write(*,*) nnn ,n1+ n2+n3+n4

            if(nfront == 0) then
                exit
            else
                nu = nu + 1
            endif

            if(n1==1) then
                i = ix -1; j = iy
                ix = i; iy = j
                oldrivflag(ix,iy) = 1
            endif
            if(n2==1) then
                i = ix +1; j = iy
                ix = i; iy = j
                oldrivflag(ix,iy) = 1
            endif
            if(n3==1) then
                i = ix   ; j = iy + 1
                ix = i; iy = j
                oldrivflag(ix,iy) = 1
            endif
            if(n4==1) then
                i = ix   ; j = iy - 1
                ix = i; iy = j
                oldrivflag(ix,iy) = 1
            endif

            if(ix==178 .and. iy == 73) then
                write(*,*) ix, ix, nu
            endif
            write(13,'(I15,a,I15,a,I15)') ix,",", iy,",", nu
        enddo

                    
        end program
