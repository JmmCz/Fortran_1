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
                end if
            end do
        end do

    end subroutine

end module
