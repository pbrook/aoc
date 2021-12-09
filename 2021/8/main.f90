program main
    use aoc

    implicit none

    integer :: a(2)

    a = decode('test0')
    call assert(a(1), 0)
    call assert(a(2), 5353)

    a = decode('test')
    call assert(a(1), 26)
    call assert(a(2), 61229)

    a = decode('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains


function decode(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(7) :: signals(10)
    character :: dummy
    character(7) :: output(4)
    integer :: siglen(10)
    integer :: bits(10)
    integer :: digit(0:9)
    integer :: i
    integer :: n
    integer :: loc(1)

    part(1) = 0
    part(2) = 0
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, *, iostat=stat) signals, dummy, output
        if (stat /= 0) then
            exit
        end if
        if (dummy /= '|') then
            error stop
        end if
        part(1)= part(1) + count(find_easy(len_trim(output)) >= 0)

        digit = 0
        bits = to_bits(signals)
        siglen = len_trim(signals)
        !1 len2
        !4 len4
        !7 len3
        !8 len7
        do i=1,10
            n = find_easy(siglen(i))
            if (n >= 0) then
                siglen(i) = 0
                digit(n) = bits(i)
            end if
        end do

!9 len6 inc4
!3 len5 inc1
        do i=1,10
            if (siglen(i) == 6 .and. has_bits(bits(i), digit(4))) then
                n = 9
            else if (siglen(i) == 5 .and. has_bits(bits(i), digit(1))) then
                n = 3
            else
                n = -1
            end if
            if (n >= 0) then
                siglen(i) = 0
                digit(n) = bits(i)
            end if
        end do

!0 len6 inc1
!6 len6 !inc1
!5 len5 most9
!2 len5 !most9
        do i=1,10
            if (siglen(i) == 6) then
                if (has_bits(bits(i), digit(1))) then
                    n = 0
                else
                    n = 6
                endif
            else if (siglen(i) == 5) then
                if (most_bits(bits(i), digit(9))) then
                    n = 5
                else
                    n = 2
                end if
            else
                n = -1
            end if
            if (n >= 0) then
                siglen(i) = 0
                digit(n) = bits(i)
            end if
        end do

        if (any(digit == 0) .or. any(siglen /= 0)) then
            error stop
        end if
        n = 0
        do i=1,size(output)
            loc = findloc(digit, to_bits(output(i)))
            n = n * 10 + loc(1) - 1
        end do
        part(2) = part(2) + n
    end do
end function

function has_bits(a, b)
    integer, intent(in) :: a, b
    logical :: has_bits

    has_bits = iand(a, b) == b
end function

function most_bits(a, b)
    integer, intent(in) :: a, b
    logical :: most_bits

    most_bits = popcnt(iand(ieor(a, b), b)) == 1
end function

elemental function to_bits(s) result (mask)
    character(7), intent(in) :: s
    integer :: mask
    integer :: i
    integer :: bit

    mask = 0
    do i=1,len_trim(s)
        bit = iachar(s(i:i)) - iachar('a')
        mask = ibset(mask, bit)
    end do
end function

elemental function find_easy(slen)
    integer, intent(in) :: slen
    integer :: find_easy

    select case (slen)
    case(2)
        find_easy = 1
    case(4)
        find_easy = 4
    case(3)
        find_easy = 7
    case(7)
        find_easy = 8
    case default
        find_easy = -1
    end select
end function

end program
