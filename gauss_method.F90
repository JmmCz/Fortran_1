module gauss_method

    implicit none

    contains 

    subroutine gauss(A, X, N)

        integer (kind = 4), intent(in) :: N
        real (kind = 4), intent(out) :: A(N,N), X(N)
        integer (kind = 4) :: i, j
        real (kind = 4) :: c

        do i=1, N
            do j=1, N
                if(i .NE. j) then ! i!=j
                    c = A(i, j) / A(i, i)
                    A(:, j) = A(:, j) - c * A(:, i)
                    X(j) = X(j) - c * X(i)
                    
                    do i=1, N
                       c = 1 / A(i, i)
                       A(i, i) = 1
                       X(i) = c * X(i) 
                    end do 
           
                end if
            end do
        end do

    end subroutine

end module

program main

    use gauss_method, only: gauss

    integer (kind = 4) :: N, i, j
    real (kind = 4), allocatable :: A(:,:), X(:)

    read(*, *) N

    allocate (A(N, N))
    allocate (X(N))

    do i=1, N
        do j=1, N
            read(*, *) A(j, i)
        end do
    end do

    write(*, *) A

    do i=1, N
        read(*,*) X(i)
    end do

    call gauss(A, X, N)

    write(*, *) X

    deallocate (A)
    deallocate (X)

end program

