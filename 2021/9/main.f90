program main
    use aoc

    implicit none

    integer :: a(2)

    a = lava('test')
    call assert(a(1), 15)
    call assert(a(2), 1134)

    a = lava('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains


function lava(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    integer :: width
    integer :: height
    character :: dummy
    integer, allocatable :: map(:,:)
    integer, allocatable :: basin(:,:)
    integer :: x, y, val, flood
    integer :: bcount
    integer, allocatable :: bsum(:)
    logical :: again

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

    allocate(basin(0:width+1, 0:height+1))
    bcount = 0
    basin = -1
    part(1) = 0
    do y=1,height
        do x=1,width
            val = map(x, y)
            if (all(val < border(map, x,y))) then
                part(1) = part(1) + val + 1
                bcount = bcount + 1
                basin(x, y) = bcount
            else if (val /= 9) then
                basin(x, y) = 0
            end if
        end do
    end do

    allocate(bsum(bcount))
    bsum = 1
    again = .true.
    do while (again)
        again = .false.
        do y=1,height
            do x=1,width
                if (basin(x,y) /= 0) then
                    cycle
                end if
                val = maxval(border(basin, x, y))
                if (val > 0)  then
                    basin(x, y) = val
                    bsum(val) = bsum(val) + 1
                    again = .true.
                end if
            end do
        end do
    end do

    call find_big(bsum)
    call find_big(bsum(2:))
    call find_big(bsum(3:))
    ! check if the product might overflow
    if (bsum(1) > 1500) then
        error stop
    end if
    part(2) = product(bsum(1:3))
end function

subroutine find_big(a)
    integer, intent(inout) :: a(:)
    integer :: loc(1)
    integer :: big

    loc = maxloc(a)
    big = a(loc(1))
    a(loc(1)) = a(1)
    a(1) = big
end subroutine

function border(map, x, y)
    integer, allocatable, intent(in) :: map(:,:)
    integer, intent(in) :: x, y
    integer :: border(4)

    border = [map(x-1,y), map(x+1,y), map(x,y-1), map(x,y+1)]
end function

end program
