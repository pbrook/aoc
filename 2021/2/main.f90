program one
    use aoc

    implicit none

    print *, part1('test')
    print *, part1('input')
contains

function part1(filename)
    character(*) :: filename
    integer :: part1
    integer :: fd
    character(8) :: cmd
    integer :: dist
    integer :: stat
    integer :: x
    integer :: depth

    x = 0
    depth = 0
    open(newunit=fd, action='read', file=filename, iostat=stat)

    do
        read (fd, *, iostat=stat) cmd, dist
        if (stat /= 0) then
            exit
        end if
        select case (cmd)
        case ('forward')
            x = x + dist
        case ('up')
            depth = depth - dist
        case ('down')
            depth = depth + dist
        case default
            error stop
        end select
    end do
    close(fd)

    part1 = x * depth
end function

end program
