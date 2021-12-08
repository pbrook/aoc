program main
    use aoc

    implicit none

    integer :: a(2)

    a = decode('test')
    call assert(a(1), 26)
    !call assert(a(2), 168)

    a = decode('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function decode(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(7) :: signals(10)
    character :: dummy
    character(7) :: output(4)
    integer :: n

    n = 0
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, *, iostat=stat) signals, dummy, output
        if (stat /= 0) then
            exit
        end if
        if (dummy /= '|') then
            error stop
        end if
        n = n + count(part1(output))
    end do
    part(1) = n
end function

elemental function part1(s)
    character(7), intent(in) :: s
    logical :: part1
    integer :: slen

    slen = len_trim(s)
    part1 = slen == 2 .or. slen == 4 .or. slen == 3 .or. slen == 7
end function

end program
