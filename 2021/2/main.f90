program two
    use aoc

    implicit none

    integer :: a(2)

    a = submarine('test')
    call assert(a(1), 150)
    call assert(a(2), 900)

    a = submarine('input')
    print *, "Part1:", a(1), "Part2:", a(2)
contains

function submarine(filename)
    character(*) :: filename
    integer :: submarine(2)
    integer :: fd
    character(8) :: cmd
    integer :: dist
    integer :: stat
    integer :: x
    integer :: depth
    integer :: aim

    x = 0
    depth = 0
    ! aim is the same as depth for part1
    aim = 0
    open(newunit=fd, action='read', file=filename, iostat=stat)

    do
        read (fd, *, iostat=stat) cmd, dist
        if (stat /= 0) then
            exit
        end if
        select case (cmd)
        case ('forward')
            x = x + dist
            depth = depth + aim * dist
        case ('up')
            aim = aim - dist
        case ('down')
            aim = aim + dist
        case default
            error stop
        end select
    end do
    close(fd)

    submarine = (/x * aim, x * depth/)
end function

end program
