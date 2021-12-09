program main
    use aoc

    implicit none

    integer :: a(2)

    a = lava('test')
    call assert(a(1), 15)
    !call assert(a(2), 61229)

    a = lava('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains


function lava(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    integer :: width
    integer :: height
    character :: dummy
    integer(1), allocatable :: map(:,:)
    integer :: x, y, val
    integer :: foo(2)

    width = 0
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, "(a1)", advance='no', iostat=stat) dummy
        if (stat /= 0) then
            exit
        end if
        width = width + 1
    end do
    height = 1
    do
        read (fd, *, iostat=stat) dummy
        if (stat /= 0) then
            exit
        end if
        height = height + 1
    end do

    rewind(fd)
    allocate(map(0:width+1, 0:height+1))
    map = 9
    do y=1,height
        read (fd, "(*(i1))") map(1:width, y)
    end do

    part(1) = 0
    do y=1,height
        do x=1,width
            val = map(x, y)
            if (val < map(x-1,y) .and. val < map(x+1,y) .and. val < map(x,y-1) .and. val < map(x,y+1)) then
                part(1) = part(1) + val + 1
            end if
        end do
    end do
end function

end program
