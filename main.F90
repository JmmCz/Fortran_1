program main

    use gauss_method, only: gauss

    implicit none
    
    integer (kind = 4) :: i, N
    real (kind = 4), allocatable :: A(:,:), X(:)        !here changed kind
    real (kind = 4) :: h, err                           !here changed kind 

    write(*,*) "Please, type N:"

    read(*, *) N

    h = 1. / N
    
    allocate (A(N-1, N))
    allocate (X(N))

    X(:) = 0
    X(N) = 1

    A(:, :) = 0
    
    do i=1, N-1
        A(i, i) = 1 / (h*h)
    end do

    do i=1, N-1
        A(i, i+1) = -2 / (h*h)
    end do

    do i=1, N-2
        A(i, i+2) = 1 / (h*h)
    end do

    call gauss(A, X, N-1) 

    write (*, *) X
    
    do i=1, N
        err = err + abs(x(i) - real(i)/real(N))
    end do
    
    write (*, *) "Error:"
    write (*, *) err
    
    deallocate(A)
    deallocate(X)

end program 

