program main
    use aoc

    implicit none

    logical(1), allocatable :: image(:,:)
    logical(1) :: border
    logical(1) :: algo(512)

    integer :: a(2)

    a = enhance('test')
    call assert(a(1), 35)
    call assert(a(2), 3351)

    a = enhance('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function enhance(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(512) :: algostr
    character :: c
    integer :: width
    integer :: height
    integer :: i, j

    open(newunit=fd, action='read', file=filename)

    read (fd, "(a512)") algostr
    read (fd, "()")
    width = 0
    do
        read (fd, "(a1)", advance='no', iostat=stat) c
        if (stat /= 0) then
            exit
        end if
        width = width + 1
    end do
    height = 1
    do
        read (fd, *, iostat=stat) c
        if (stat /= 0) then
            exit
        end if
        height = height + 1
    end do
    rewind(fd)

    allocate(image(width, height))
    border = .false.

    read (fd, "(a512)") algostr
    do i=1,512
        algo(i) = parse(algostr(i:i))
    end do
    read (fd, "()")
    do j=1,height
        do i=1,width
            read (fd, "(a1)", advance='no') c
            image(i, j) = parse(c)
        end do
        read (fd, "()")
    end do

    do i=1,2
        call step()
    end do

    part(1) = count(image)
    do i=3,50
        call step()
    end do
    part(2) = count(image)
    deallocate(image)
end function

elemental function parse(c)
    character, intent(in) :: c
    logical(1) :: parse

    if (c == '#') then
        parse = .true.
    else
        parse = .false.
    end if
end function

elemental function unparse(p)
    logical(1), intent(in) :: p
    character :: unparse

    if (p) then
        unparse = '#'
    else
        unparse = '.'
    end if
end function

function get_bit(x, y)
    integer, intent(in) :: x, y
    logical(1) :: val
    integer :: get_bit

    if (x < lbound(image, 1) .or. x > ubound(image, 1) .or. y < lbound(image, 2) .or. y > ubound(image, 2)) then
        val = border
    else
        val = image(x, y)
    end if
    if (val) then
        get_bit = 1
    else
        get_bit = 0
    end if
end function

function pixel(x, y)
    logical(1) :: pixel
    integer :: x, y
    integer :: idx

    idx = get_bit(x-1, y-1)
    idx = shiftl(idx, 1) + get_bit(x, y-1)
    idx = shiftl(idx, 1) + get_bit(x+1, y-1)
    idx = shiftl(idx, 1) + get_bit(x-1, y)
    idx = shiftl(idx, 1) + get_bit(x, y)
    idx = shiftl(idx, 1) + get_bit(x+1, y)
    idx = shiftl(idx, 1) + get_bit(x-1, y+1)
    idx = shiftl(idx, 1) + get_bit(x, y+1)
    idx = shiftl(idx, 1) + get_bit(x+1, y+1)
    pixel = algo(idx + 1)
end function

subroutine step()
    logical(1), allocatable :: tmp(:,:)
    integer :: w, h
    integer :: x, y

    w = ubound(image, 1) + 2
    h = ubound(image, 2) + 2
    allocate(tmp(w, h))
    do y=1,h
        do x=1,w
            tmp(x, y) = pixel(x-1, y-1)
        end do
    end do
    call move_alloc(tmp, image)
    if (border) then
        border = algo(512)
    else
        border = algo(1)
    end if
end subroutine

subroutine dump()
    integer :: i

    do i=1,ubound(image, 2)
        print "(*(a1))", unparse(image(:, i))
    end do
end subroutine

end program
