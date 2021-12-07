program main
    use aoc

    implicit none

    integer :: a(2)

    a = breed('test')
    call assert(a(1), 37)
    !call assert8(a(2), 26984457539_8)

    a = breed('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function breed(filename) result(part)
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
    
    best = huge(best)
    ! O(n^2). Don't care!
    do i=1,ncrabs
        pos = crabs(i)
        fuel = sum(abs(crabs - pos))
        if (fuel < best) then
            best = fuel
        end if
    end do
    part(1) = best
end function

end program
