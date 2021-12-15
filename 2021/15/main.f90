program main
    use aoc

    implicit none

    integer, allocatable :: cave(:,:)
    integer, allocatable :: risk(:,:)

    integer(8) :: a(2)

    a = chinton('test')
    call assert8(a(1), 40_8)
    !call assert8(a(2), 2188189693529_8)

    a = chinton('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function chinton(filename) result(part)
    character(*) :: filename
    integer(8) :: part(2)
    integer :: fd
    integer :: stat

    integer :: i, j
    integer :: width, height
    logical :: again

    open(newunit=fd, action='read', file=filename)
    width = 0
    do
        read(fd, "(i1)", advance='no', iostat=stat) i
        if (stat /= 0) then
            exit
        end if
        width = width + 1
    end do
    height = 1
    do
        read(fd, "(i1)", iostat=stat) i
        if (stat /= 0) then
            exit
        end if
        height = height + 1
    end do
    allocate(cave(width, height))
    rewind(fd)
    do j=1,height
        read (fd, "(*(i1))") cave(:, j)
    end do
    close(fd)
    allocate(risk(width,height))
    risk = huge(risk)
    risk(1,1) = 0
    again = .true.
    do while (again)
        again = .false.
        do j=1,height
            do i=1,width
                call push_risk(i, j, again)
            end do
        end do
    end do
    !call dump(cave)
    !call dump(risk)
    part(1) = risk(ubound(cave,1),ubound(cave,2))
    deallocate(cave)
    deallocate(risk)
end function

subroutine dump(grid)
    integer, allocatable :: grid(:,:)
    integer :: i
    do i=1,ubound(grid,2)
        print "(*(i3))", grid(:,i)
    end do
end subroutine
subroutine pull_risk(x, y, base, again)
    integer, intent(in) :: x, y
    integer, intent(in) :: base
    logical, intent(inout) :: again
    integer :: new

    new = base + cave(x, y)
    if (new < risk(x, y)) then
        risk(x, y) = new
        again = .true.
    end if
end subroutine

subroutine push_risk(x, y, again)
    integer, intent(in) :: x, y
    logical, intent(inout) :: again
    integer :: base

    base = risk(x,y)

    if (base == huge(base)) then
        return
    end if

    if (x > 1) then
        call pull_risk(x-1, y, base, again)
    end if
    if (x < ubound(cave, 1)) then
        call pull_risk(x+1, y, base, again)
    end if
    if (y > 1) then
        call pull_risk(x, y-1, base, again)
    end if
    if (y < ubound(cave, 2)) then
        call pull_risk(x, y+1, base, again)
    end if
end subroutine

end program
