program main
    use aoc

    implicit none

    type cube
        logical :: on
        integer(4) :: a(3), b(3)
        type(cube), pointer :: next => null()
    end type

    integer :: a(2)

    a = reactor('test0')
    call assert(a(1), 39)
    call assert(a(2), 39)

    a = reactor('test1')
    call assert(a(1), 590784)

    a = reactor('test2')
    call assert(a(1), 474140)
    call assert(a(2), 2758514936282235)

    a = reactor('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function reactor(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    logical :: map(-50:50, -50:50, -50:50)
    type(cube), pointer :: head
    type(cube), pointer :: tail
    type(cube), pointer :: next
    type(cube), pointer :: p
    character(3) :: cmd
    character(20) :: s(3)
    character :: c
    integer :: i, j, k

    nullify(head)
    open(newunit=fd, action='read', file=filename)

    do
        read (fd, *, iostat=stat) cmd, s
        if (stat /= 0) then
            exit
        end if
        if (any(len_trim(s) == len(s))) then
            error stop
        end if
        allocate(p)
        p%on = cmd == 'on'
        do j=1,3
            do i=1,len_trim(s(j))
                select case (s(j)(i:i))
                case ('-', '0':'9')
                case default
                    s(j)(i:i) = ' '
                end select
            end do
            read (s(j), *) p%a(j), p%b(j)
        end do
        if (associated(head)) then
            tail%next => p
        else
            head => p
        end if
        tail => p
    end do


    map = .false.

    next => head
    nullify(head)
    do while (associated(next))
        p => next
        next => next%next
        call split_cubes(head, p)
        if (p%on) then
            p%next => head
            head => p
        end if
        if (any(p%a > 50) .or. any(p%b < -50)) then
        else
            if (any(p%a < -50) .or. any(p%b > 50)) then
                error stop
            end if
            do i=p%a(1),p%b(1)
                do j=p%a(2),p%b(2)
                    do k=p%a(3),p%b(3)
                        map(i, j, k) = p%on
                    end do
                end do
            end do
        end if
    end do
    part(1) = count(map)
    part(2) = count_cubes(head)
end function

function count_cubes(head) result(n)
    type(cube), pointer, intent(in) :: head
    type(cube), pointer :: p
    integer :: n

    n = 0
    p => head
    do while (associated(p))
        if (p%on) then
            call print_cube('sum', p)
            n = n + product(p%b - p%a + 1)
        end if
        p => p%next
    end do
end function

subroutine print_cube(s, p)
    character(*) :: s
    character(4) :: ls
    type(cube), pointer, intent(in) :: p
    ls = s
    !print "(a4, l1,*(i6, '..', i6, ','))", ls, p%on, p%a(1), p%b(1), p%a(2), p%b(2), p%a(3), p%b(3)
end subroutine

subroutine split_cubes(head, op)
    type(cube), pointer, intent(inout) :: head
    type(cube), pointer, intent(in) :: op
    type(cube), pointer :: p
    type(cube), pointer :: next
    type(cube), pointer :: new
    integer :: i

    call print_cube("sp", op)
    p => head
    do while (associated(p))
        if (p%on) then
            if (any(p%a > op%b) .or. any(p%b < op%a)) then
                ! No intersection
            else
                do i=1,3
                    if (p%a(i) < op%a(i) .and. p%b(i) >= op%a(i)) then
                        call print_cube("/a", p)
                        allocate(new)
                        new%on = .true.
                        new%a = p%a
                        new%b = p%b
                        new%b(i) = op%a(i) - 1
                        p%a(i) = op%a(i)
                        new%next => head
                        head => new
                        call print_cube('+', new)
                    end if
                    if (p%a(i) <= op%b(i) .and. p%b(i) > op%b(i)) then
                        call print_cube('/b', p)
                        allocate(new)
                        new%on = .true.
                        new%a = p%a
                        new%b = p%b
                        new%a(i) = op%b(i) + 1
                        p%b(i) = op%b(i)
                        new%next => head
                        head => new
                        call print_cube('+', new)
                    end if
                end do
                if (any(p%a < op%a) .or. any(p%b > op%b)) then
                    error stop
                end if
                p%on = .false.
                call print_cube('-', p)
            end if
        end if
        p => p %next
    end do
end subroutine

end program
