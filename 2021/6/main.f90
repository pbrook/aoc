program main
    use aoc

    implicit none

    integer(8) :: a(2)

    a = breed('test')
    call assert8(a(1), 5934_8)
    call assert8(a(2), 26984457539_8)

    a = breed('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function breed(filename) result(part)
    character(*) :: filename
    integer(8) :: part(2)
    integer :: fd
    integer :: stat
    integer(8) :: fish(0:8)
    integer(8) :: val
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
    
    call age(fish, 80)
    part(1) = sum(fish)
    call age(fish, 256 - 80)
    part(2) = sum(fish)
end function

subroutine age(fish, days)
    integer(8) :: fish(0:8)
    integer, intent(in) :: days
    integer(8) :: new
    integer :: i

    do i=1,days
       new = fish(0)
       fish(0:7) = fish(1:8)
       fish(6) = fish(6) + new
       fish(8) = new
    end do
end subroutine

end program
