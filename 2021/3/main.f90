program two
    use aoc

    implicit none

    call assert(part1('test'), 198)

    print *, "Part1:", part1('input')
contains

function part1(filename)
    character(*) :: filename
    integer :: part1
    integer :: fd
    integer :: stat
    character(32) :: bits
    integer :: i
    integer :: nbits
    integer, allocatable :: bitsum(:)
    integer :: nlines
    integer :: g
    integer :: e

    open(newunit=fd, action='read', file=filename, iostat=stat)

    read (fd, *, iostat=stat) bits
    nbits = len_trim(bits)
    nlines = 0
    allocate(bitsum(nbits))
    bitsum = 0
    do
        nlines = nlines + 1
        bitsum = bitsum + (/ (iachar(bits(i:i)) - iachar('0'), i=1,nbits) /)
        read (fd, *, iostat=stat) bits
        if (stat /= 0) then
            exit
        end if
    end do
    close(fd)

    g = 0
    e = 0
    do i=1,nbits
        g = g * 2
        e = e * 2
        if (bitsum(i) > (nlines / 2)) then
            g =g + 1
        else
            e = e + 1
        end if
    end do
    !print *, g, e
    part1 = g * e
end function

end program
