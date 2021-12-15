program main
    use aoc

    implicit none

    integer, allocatable :: cave(:,:)
    integer, allocatable :: risk(:,:)

    integer(8) :: a(2)

    a = chinton('test')
    call assert8(a(1), 40_8)
    call assert8(a(2), 315_8)

    a = chinton('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function chinton(filename) result(part)
    character(*) :: filename
    integer(8) :: part(2)
    integer :: fd
    integer :: stat
    integer, allocatable :: tmp(:,:)

    integer :: i, j
    integer :: width, height

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
    rewind(fd)
    allocate(cave(width, height))
    do i=1,height
        read (fd, "(*(i1))") cave(:, i)
    end do
    close(fd)

    !call dump(cave)
    !call dump(risk)

    part(1) = calc_risk()

    call move_alloc(cave, tmp)
    allocate(cave(width*5, height*5))
    do i=1,width*5,width
        do j=1,height*5,height
            cave(i:i+width, j:j+height) = add_mod(tmp, i/width+j/height)
        end do
    end do
    !call dump(cave)
    deallocate(tmp)
    part(2) = calc_risk()

    deallocate(cave)
end function

elemental function add_mod(val, n)
    integer, intent(in) :: val, n
    integer :: add_mod
    add_mod = mod(val + n - 1, 9) + 1
end function

function calc_risk()
    integer :: calc_risk
    integer :: x, y
    logical :: again

    allocate(risk(ubound(cave,1),ubound(cave,2)))
    risk = huge(risk)
    risk(1,1) = 0
    again = .true.
    do while (again)
        again = .false.
        do y=1,ubound(cave,2)
            do x=1,ubound(cave,1)
                call push_risk(x, y, again)
            end do
        end do
    end do
    calc_risk = risk(ubound(cave,1),ubound(cave,2))
    deallocate(risk)
end function

subroutine dump(grid)
    integer, allocatable, intent(in) :: grid(:,:)
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
