program main
    use aoc

    implicit none

    integer(1), parameter :: EAST=iachar('>'), SOUTH=iachar('v'), EMPTY=iachar('.')
    integer :: a

    call test()

    call assert(herd('test'), 58)

    a = herd('input')
    print *, "Part1:", a
contains

subroutine test()
    integer(1), allocatable :: map(:,:)
    character(6), parameter :: names(*) = ['test0b', 'test0c', 'test0d', 'test0e']
    integer :: i

    call parse('test0a', map)
    do i=1,size(names)
        if (.not. step(map)) then
            error stop
        end if
        call test_eq(names(i), map)
    end do
end subroutine

subroutine test_eq(filename, map)
    character(*), intent(in) :: filename
    integer(1), intent(in) :: map(:, :)
    integer(1), allocatable :: tmp(:, :)

    call parse(filename, tmp)
    if (any(tmp /= map)) then
        print *, filename
        call dump(map)
        error stop
    end if
end subroutine

subroutine parse(filename, map)
    character(*), intent(in) :: filename
    integer(1), allocatable, intent(out) :: map(:, :)

    integer :: fd
    integer :: stat
    integer :: width, height
    integer :: x, y
    character :: c

    open(newunit=fd, action='read', file=filename)
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
        read (fd, "(a1)", iostat=stat) c
        if (stat /= 0) then
            exit
        end if
        height = height + 1
    end do
    rewind(fd)

    allocate(map(width, height))

    do y=1,height
        do x=1,width
            read (fd, "(a1)", advance='no') c
            map(x, y) = iachar(c)
        end do
        read (fd, "()")
    end do
    close(fd)
end subroutine

function herd(filename) result (steps)
    character(*), intent(in) :: filename
    integer :: steps

    integer(1), allocatable :: map(:, :)

    call parse(filename, map)

    steps = 0
    do
        !print *, steps
        !call dump(map)
        steps = steps + 1
        if (.not. step(map)) then
            exit
        end if
    end do
end function

function try(a, b, t) result (move)
    integer(1), intent(inout) :: a, b
    integer(1), intent(in) :: t
    logical :: move

    move = a == t .and. b == EMPTY
    if (move) then
        a = EMPTY
        b = t
    end if
end function

function step(map) result (moved)
    integer(1), intent(inout) :: map(:,:)
    integer(1) :: tmp(maxval(ubound(map))+1)
    logical :: moved
    integer :: x, y, last

    moved = .false.
    last = ubound(map, 1)
    do y=1,ubound(map, 2)
        tmp(1:last) = map(:,y)
        tmp(last+1) = tmp(1)
        do x=1,last
            if (tmp(x) == EAST .and. tmp(x+1) == EMPTY) then
                map(x, y) = EMPTY
                if (x == last) then
                    map(1, y) = EAST
                else
                    map(x+1, y) = EAST
                end if
                moved = .true.
            end if
        end do
    end do

    last = ubound(map, 2)
    do x=1,ubound(map, 1)
        tmp(1:last) = map(x, :)
        tmp(last+1) = tmp(1)
        do y=1,last
            if (tmp(y) == SOUTH .and. tmp(y+1) == EMPTY) then
                map(x, y) = EMPTY
                if (y == last) then
                    map(x, 1) = SOUTH
                else
                    map(x, y+1) = SOUTH
                end if
                moved = .true.
            end if
        end do
    end do

end function

subroutine dump(map)
    integer(1), intent(in) :: map(:,:)
    integer :: i

    do i=1,ubound(map, 2)
        print "(*(a1))", achar(map(:, i))
    end do
end subroutine

end program
