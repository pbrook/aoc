program main
    use aoc

    implicit none

type point
    integer :: x, y
end type

    integer :: a(2)

    a = origami('test')
    call assert(a(1), 17)
    !call assert(a(2), 36)

    a = origami('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function origami(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(20) :: line

    type(point), allocatable :: p(:)
    integer :: np
    integer :: i

    open(newunit=fd, action='read', file=filename)
    np = 0
    do
        read (fd, *) line
        if (line == 'fold') then
            exit
        end if
        np = np + 1
    end do
    rewind(fd)
    allocate(p(np))
    do i=1,np
        read (fd, *) p(i)%x, p(i)%y
    end do
    read (fd, *)

    read (fd, "(a20)", iostat=stat) line
    call fold(p, trim(line))
    close(fd)

    part(1) = plot(p)
end function

subroutine fold(p, line)
    type(point), allocatable, intent(inout) :: p(:)
    character(*) :: line
    integer :: pos
    integer :: i

    read (line(14:), *) pos
    if (line(:13) == 'fold along y=') then
        do i=1,size(p)
            if (p(i)%y > pos) then
                p(i)%y = 2*pos - p(i)%y
            end if
        end do
    else if (line(:13) == 'fold along x=') then
        do i=1,size(p)
            if (p(i)%x > pos) then
                p(i)%x = 2*pos - p(i)%x
            end if
        end do
    else
        error stop
    end if
end subroutine

function plot(p) result(n)
    type(point), allocatable, intent(in) :: p(:)
    integer :: n
    integer :: w, h
    integer :: i
    integer :: x
    character(:), allocatable :: grid(:)
    w = maxval(p%x)
    h = maxval(p%y)
    allocate(character(w+1) :: grid(0:h))
    grid = ' '
    n = 0
    do i=1,size(p)
        x = p(i)%x + 1
        if (grid(p(i)%y)(x:x) == ' ') then
            grid(p(i)%y)(x:x) = '#'
            n = n + 1
        end if
    end do
    do i=0,size(grid)-1
        !print *, grid(i)
    end do
end function

end program
