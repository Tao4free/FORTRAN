        program main

        implicit real(kind=8) (a-h,o-z)

        real::r(2,3,5)
        real::ro(2,3,5)

        do k = 1, 5
        do j = 1, 3
        do i = 1, 2
            r(i,j,k) = i + 10*j + 100*k
        enddo
        enddo
        enddo
        write(*,'(6f10.4)') (r(:,:,k), k=1,5)
        write(*,*)

        ro=r

        !a=r(:,:,1)
        !r(:,:,:)=r(:,:,:) - a
        !write(*,*) r

        r=ro
        do k = 1, 5
        do j = 1, 3
        do i = 1, 2
            r(i,j,k) = r(i,j,k) - ro(i,j,1)
        enddo
        enddo
        enddo
        write(*,'(6f10.4)') (r(:,:,k), k=1,5)
        write(*,*) 

        r=ro
        do k = 1, 5
            r(:,:,k) = r(:,:,k) - ro(:,:,1)
        enddo
        write(*,'(6f10.4)') (r(:,:,k), k=1,5)
        write(*,*) 

        end
