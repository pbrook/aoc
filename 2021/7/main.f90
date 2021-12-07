program main
    use aoc

    implicit none

    integer :: a(2)

    a = move('test')
    call assert(a(1), 37)
    call assert(a(2), 168)

    a = move('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function move(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat
    integer :: val
    character dummy
    integer :: ncrabs
    integer :: i
    integer, allocatable :: crabs(:)
    integer :: pos
    integer :: best, fuel
    integer :: a, b

    ncrabs = 1
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, "(a1)", iostat=stat, advance='no') dummy
        if (stat /= 0) then
            exit
        end if
        if (dummy == ',') then
            ncrabs = ncrabs + 1
        end if
    end do
    allocate(crabs(ncrabs))
    rewind(fd)
    read (fd, *) crabs
    close(fd)
    
    ! O(n^2). Don't care!
    best = huge(best)
    do i=1,ncrabs
        pos = crabs(i)
        fuel = sum(abs(crabs - pos))
        if (fuel < best) then
            best = fuel
        end if
    end do
    part(1) = best

    ! Still O(n^2). Still don't care!
    best = huge(best)
    a = minval(crabs)
    b = maxval(crabs)
    do pos=a,b
        fuel = sum(part2(abs(crabs - pos)))
        if (fuel < best) then
            best = fuel
        end if
    end do
    part(2) = best
end function

elemental function part2(dist)
    integer, intent(in) :: dist
    integer :: part2

    part2 = dist * (dist + 1) / 2
end function

end program
