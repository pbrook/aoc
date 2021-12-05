program main
    use aoc

    implicit none

type vent
    integer :: x1, y1, x2, y2
    type(vent), allocatable :: next
end type vent

    integer :: a(2)

    a = map_vents('test')
    call assert(a(1), 5)
    call assert(a(2), 12)

    a = map_vents('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function map_vents(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat
    integer, allocatable :: grid(:,:)
    integer :: i
    type(vent), allocatable, target :: vents
    type(vent), allocatable, target :: tmp
    type(vent), pointer :: p
    character(2) dummy
    integer :: width, height
    integer :: a, b, w, h

    open(newunit=fd, action='read', file=filename)

    width = 0
    height = 0
    do
        allocate(tmp)
        read (fd, *, iostat=stat) tmp%x1, tmp%y1, dummy, tmp%x2, tmp%y2
        if (stat /= 0) then
            exit
        end if
        width = max(width, tmp%x1, tmp%x2)
        height = max(height, tmp%y1, tmp%y2)
        call move_alloc(vents, tmp%next)
        call move_alloc(tmp, vents)
    end do

    allocate(grid(0:width, 0:height))
    grid = 0

    p => vents
    do while (associated(p))
        if (p%x1 == p%x2) then
            a = min(p%y1, p%y2)
            b = max(p%y1, p%y2)
            do i=a,b
                grid(p%x1, i) = grid(p%x1, i) + 1
            end do
        else if (p%y1 == p%y2) then
            a = min(p%x1, p%x2)
            b = max(p%x1, p%x2)
            do i=a,b
                grid(i, p%y1) = grid(i, p%y1) + 1
            end do
        end if
        p => p%next
    end do
    part(1) = count(grid > 1)
    p => vents
    do while (associated(p))
        w = p%x2 - p%x1
        h = p%y2 - p%y1
        if (w /= 0 .and. h /= 0) then
            do i = 0,abs(w)
                a = p%x1 + sign(i, w)
                b = p%y1 + sign(i, h)
                grid(a, b) = grid(a, b) + 1
            end do
        end if
        p => p%next
    end do
    part(2) = count(grid > 1)
end function

end program
