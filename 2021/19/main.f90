program main
    use aoc

    implicit none

type beacon
    integer :: x, y, z
end type
type scanner
    type(beacon), allocatable :: b(:)
    logical :: matched = .false.
    type(beacon) :: offset
end type
interface operator (==)
    procedure beacon_eq
end interface
interface operator (-)
    procedure beacon_sub
end interface

    integer :: a(2)

    a = assemble('test')
    call assert(a(1), 79)
    call assert(a(2), 3621)

    a = assemble('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

elemental function beacon_eq(a, b) result(r)
    type(beacon), intent(in) :: a, b
    logical :: r
    r = a%x == b%x .and. a%y == b%y .and. a%z == b%z
end function

elemental function beacon_sub(a, b) result(r)
    type(beacon), intent(in) :: a, b
    type(beacon) :: r
    r%x = a%x - b%x
    r%y = a%y - b%y
    r%z = a%z - b%z
end function

function assemble(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(20) :: line
    integer :: nscan, nbeacon
    integer :: i, j, n
    type(scanner), allocatable :: s(:)
    integer, allocatable :: scan_order(:)
    integer :: found
    logical :: again

    open(newunit=fd, action='read', file=filename)
    nscan = 0
    do
        read (fd, *, iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        if (line == '---') then
            nscan = nscan + 1
        end if
    end do
    rewind(fd)
    allocate(s(nscan))
    n = 0
    i = 0
    do
        read (fd, *, iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        if (line == '---') then
            if (i > 0) then
                allocate(s(i)%b(n))
            end if
            i = i + 1
            n = 0
        else
            n = n + 1
        end if
    end do
    allocate(s(i)%b(n))
    rewind(fd)
    do i=1,nscan
        read (fd, *) line
        read (fd, *) s(i)%b
    end do

    s(1)%matched = .true.
    s(1)%offset = beacon(0,0,0)
    allocate(scan_order(nscan))
    scan_order(1) = 1
    found = 1
    do n=1,nscan
        if (n > found) then
            error stop
        end if
        i= scan_order(n)
        do j=1,nscan
            if (.not.s(j)%matched) then
                call match(s(i), s(j))
                if (s(j)%matched) then
                    found = found + 1
                    scan_order(found) = j
                end if
            end if
        end do
    end do
    part(1) = count_beacons(s)

    part(2) = furthest(s%offset)
end function

function furthest(b) result (far)
    type(beacon), intent(in) :: b(:)
    type(beacon) :: diff
    integer :: far, n
    integer :: i, j

    far = 0
    do i=1,size(b)
        do j=i+1,size(b)
            diff = b(i) - b(j)
            n = abs(diff%x) + abs(diff%y) + abs(diff%z)
            if (n > far) then
                far = n
            end if
        end do
    end do
end function

elemental function rotatez(b) result(r)
    type(beacon), intent(in) :: b
    type(beacon) :: r
    r = beacon(-b%y, b%x, b%z)
end function

elemental function flip(b) result(r)
    type(beacon), intent(in) :: b
    type(beacon) :: r
    r = beacon(-b%x, b%y, -b%z)
end function

elemental function roll(b) result(r)
    type(beacon), intent(in) :: b
    type(beacon) :: r
    r = beacon(b%y, b%z, b%x)
end function

subroutine permute(b, p)
    type(beacon), intent(inout) :: b(:)
    integer, intent(in) :: p

    b = rotatez(b)
    if (mod(p, 4) == 0) then
        b = flip(b)
    end if
    if (mod(p, 8) == 0) then
        b = roll(b)
    end if
end subroutine

function count_beacons(s) result(n)
    type(scanner), intent(inout) :: s(:)
    type(beacon), allocatable :: b(:)
    integer :: n
    integer :: i, j
    type(beacon) :: next
    logical :: add

    n = 0
    do i=1,size(s)
        n = n + size(s(i)%b)
    end do
    allocate(b(n))
    n = 0
    do i=1,size(s)
        do j=1,size(s(i)%b)
            next = s(i)%b(j)
            if (.not. any(b(:n) == next)) then
                n = n + 1
                b(n) = next
            end if
        end do
    end do
end function

subroutine match(s, t)
    type(scanner), intent(inout) :: s
    type(scanner), intent(inout) :: t
    type(beacon) :: offset
    integer :: i, j, p
    integer :: n

    outer: do p=1,24
        do i=1,size(s%b)
            do j=1,size(t%b)
                offset = t%b(j)-s%b(i)
                n = try_beacon(s,t,offset)
                if (n >= 12) then
                    t%matched = .true.
                    t%offset = offset
                    t%b = t%b - offset
                    exit outer
                end if
            end do
        end do
        call permute(t%b, p)
    end do outer
end subroutine

function try_beacon(s, t, b) result (r)
    type(scanner), intent(inout) :: s
    type(scanner), intent(inout) :: t
    type(beacon), intent(in) :: b
    integer :: r
    integer :: i, j

    r = 0
    do i=1,size(s%b)
        if (any(t%b-s%b(i) == b)) then
            r = r + 1
        end if
    end do
end function

end program
