program main
    use aoc

    implicit none

    integer :: a(2)

    a = breed('test')
    call assert(a(1), 5934)
    !call assert(a(2), 12)

    a = breed('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function breed(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat
    integer :: fish(0:8)
    integer :: val
    character dummy
    integer :: day

    fish = 0
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, "(i1)", advance='no') val
        fish(val) = fish(val) + 1
        read (fd, "(a1)", iostat=stat, advance='no') dummy
        if (stat /= 0) then
            exit
        end if
    end do
    close(fd)
    
    do day=1,80
       val = fish(0) 
       fish(0:7) = fish(1:8)
       fish(6) = fish(6) + val
       fish(8) = val
    end do
    part(1) = sum(fish)
end function

end program
