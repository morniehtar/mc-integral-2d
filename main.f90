module config
    implicit none

! Number of random points
    integer(8), parameter :: dots = 1d6 ! 1d9 takes around 8 seconds
! Threads count
    integer, parameter :: thr = 4
! Float comparison limiter
    real(8), parameter :: eps = 1d-9

    real(8), parameter :: pi = 4*atan(1.d0)
end module

program main
    use config
    implicit none

    abstract interface
        real(8) function fct(x, y)
        real(8), intent(in) :: x , y
        end function
    end interface

    real(8) :: rndX, rndY, integ, store
    integer :: j

    integ = 0
    !$omp parallel default(none) shared(integ) private(j, rndX, rndY) num_threads(thr)
        !$omp do schedule(static) reduction(+: integ)
        do j = 1, dots
1           rndX = rnd(0d0, 1d0)
            rndY = rnd(0d0, 1d0)
            if (crcl(rndX, rndY).lt.eps) then
                integ = integ + func(rndX, rndY)
            else
                goto 1
            end if
        end do
        !$omp end do
    !$omp end parallel
    integ = pi*integ/dots
    store = integ
    print *, "circle:           ", integ, pi/24

    integ = 0
    !$omp parallel default(none) shared(integ) private(j, rndX, rndY) num_threads(thr)
        !$omp do schedule(static) reduction(+: integ)
        do j = 1, dots
2           rndX = rnd(0d0, 0.5d0)
            rndY = rnd(0d0, 1d0)
            if (velps(rndX, rndY).lt.eps) then
                integ = integ + func(rndX, rndY)
            else
                goto 2
            end if
        end do
        !$omp end do
    !$omp end parallel
    integ = 0.5d0*pi*integ/dots
    print *, "vertical ellipse: ", store-integ, pi/24-pi/192

    integ = 0
    !$omp parallel default(none) shared(integ) private(j, rndX, rndY) num_threads(thr)
        !$omp do schedule(static) reduction(+: integ)
        do j = 1, dots
3           rndX = rnd(-1d0, 1d0)
            rndY = rnd(-1d0, 1d0)
            if (telps(rndX, rndY).lt.eps) then
                integ = integ + func(rndX, rndY)
            else
                goto 3
            end if
        end do
        !$omp end do
    !$omp end parallel
    integ = pi*integ/dots
    print *, "tilted ellipse:  ", integ, pi/24-pi*43/3072

contains

    pure real(8) function func(x, y)
        implicit none
        real(8), intent(in) :: x, y
        func = x**2*y**2
    end function func

    real(8) function rnd(bt, tp)
        implicit none
        real(8), intent(in) :: bt, tp
        real(8) :: x
        call random_number(x)
        rnd = bt + (tp-bt)*x
    end function rnd

    pure real(8) function crcl(x, y)
        implicit none
        real(8), intent(in) :: x, y
        crcl = x**2 + y**2 - 1d0
    end function crcl

    pure real(8) function velps(x, y)
        implicit none
        real(8), intent(in) :: x, y
        velps = 4d0*x**2 + y**2 - 1d0
    end function velps

    pure real(8) function helps(x, y)
        implicit none
        real(8), intent(in) :: x, y
        helps = x**2 + 4d0*y**2 - 1d0
    end function helps

    pure real(8) function telps(x, y)
        implicit none
        real(8), intent(in) :: x, y
        telps = 2d0*(x+y)**2 + (x-y)**2/2d0 - 1d0
    end function telps

end program main

