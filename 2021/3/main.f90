program two
    use aoc

    implicit none
    integer :: a(2)

    a = process('test')
    call assert(a(1), 198)
    call assert(a(2), 230)

    a = process('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

! Return true if the most common bit value is 1
function common_bit(bits, pos)
    character(*) :: bits(:)
    integer :: pos
    integer :: one_count
    character :: common_bit

    one_count = count(bits(:)(pos:pos) == '1')
    if (one_count * 2 >= size(bits)) then
        common_bit = '1'
    else
        common_bit = '0'
    end if
end function

function process(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat
    character(32) :: line
    character(:), allocatable :: bits(:)
    integer :: i, j
    integer :: nbits
    integer :: nlines
    integer :: g, e
    integer :: top, bot

    open(newunit=fd, action='read', file=filename, iostat=stat)
    nlines = 0
    do
        read (fd, *, iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        nlines = nlines + 1
    end do
    nbits = len_trim(line)
    rewind(fd)

    allocate(character(nbits) :: bits(nlines))
    do i = 1,nlines
        read (fd, *, iostat=stat) bits(i)
    end do
    close(fd)

    g = 0
    e = 0
    do i=1,nbits
        g = g * 2
        e = e * 2
        if (common_bit(bits, i) == '1') then
            g = g + 1
        else
            e = e + 1
        end if
    end do


    part(1) = g * e

    call partition(bits, 1, nlines, 1)
    part(2) = binstr(bits(1)) * binstr(bits(nlines))
end function

function binstr(s) result (val)
    character(*) s
    integer :: val

    read (s, "(B32)") val
end function

recursive subroutine partition(bits, top, bot, pos)
    character(:), allocatable :: bits(:)
    integer, intent(in) :: top
    integer, intent(in) :: bot
    integer, intent(in) :: pos
    character :: com
    character(len(bits)) :: tmp
    integer :: a, b

    com = common_bit(bits(top:bot), pos)
    a = top
    b = bot
    do
        do while (a < bot .and. bits(a)(pos:pos) == com)
            a = a + 1
        end do
        do while (b > top .and. bits(b)(pos:pos) /= com)
            b = b - 1
        end do
        if (a >= b) then
            exit
        end if
        tmp = bits(a)
        bits(a) = bits(b)
        bits(b) = tmp
    end do
    a = a - 1
    b = b + 1
    if (top == 1 .and. a > 1) then
        call partition(bits, top, a, pos + 1)
    end if
    if (bot == size(bits) .and. b < size(bits)) then
        call partition(bits, b, bot, pos + 1)
    end if
end subroutine

end program
