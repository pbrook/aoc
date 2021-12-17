program main
    use aoc

    implicit none

    integer :: xmin, xmax, ymin, ymax
    integer, parameter :: HIT = 0, MISS = 1, MISS_STOP = 2

    integer :: a(2)

    a = probe('test')
    call assert(a(1), 45)
    call assert(a(2), 112)

    a = probe('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function probe(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(12) :: xstr, ystr
    integer :: x, y

    open(newunit=fd, action='read', file=filename)
    read(fd, "(a12)", advance='no') xstr
    if (xstr /= "target area:") then
        error stop
    end if
    read(fd, *) xstr, ystr
    call decode_range(xstr, xmin, xmax)
    call decode_range(ystr, ymin, ymax)

    if (ymin >= 0 .or. xmin < 0) then
        error stop
    end if
    part(1) = ymin * (ymin+1) / 2
    part(2) = 0
    do x=0,xmax+1
        do y=ymin,-ymin
            if (fire(x, y)) then
                part(2) = part(2) + 1
            end if
        end do
    end do
end function

function fire(dx, dy)
    integer, value :: dx, dy
    logical :: fire
    integer :: x, y

    x = 0
    y = 0
    do
        x = x + dx
        y = y + dy
        if (dx > 0) then
            dx = dx - 1
        end if
        dy = dy - 1

        if (dx == 0 .and. x < xmin) then
            fire = .false.
            exit
        end if
        if (x > xmax .or. y < ymin) then
            fire = .false.
            exit
        end if
        if (x >= xmin .and. y <= ymax) then
            fire = .true.
            exit
        end if
    end do
end function

subroutine decode_range(s, from, to)
    character(*), intent(in) :: s
    integer, intent(out) :: from, to

    integer :: n
    integer :: pos

    n = len_trim(s)
    pos = scan(s,'.')
    if (n == len(s) .or. pos == 0 .or. s(2:2) /= '=') then
        error stop
    end if
    read (s(3:pos-1), *) from
    read (s(pos+2:), *) to
end subroutine

end program
