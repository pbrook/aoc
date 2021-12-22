program main
    use aoc

    implicit none

    type cube
        logical :: on
        integer :: a(3), b(3)
        type(cube), pointer :: next => null()
    end type

    integer :: a(2)

    a = reactor('test0')
    call assert(a(1), 39)

    a = reactor('test1')
    call assert(a(1), 590784)
    !call assert(a(2), 444356092776315)

    a = reactor('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function reactor(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    logical :: map(-50:50, -50:50, -50:50)
    type(cube), pointer :: head
    type(cube), pointer :: tail
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

    p => head
    do while (associated(p))
        if (any(p%a > 50) .or. any(p%b < -50)) then
        else
            do i=p%a(1),p%b(1)
                do j=p%a(2),p%b(2)
                    do k=p%a(3),p%b(3)
                        map(i, j, k) = p%on
                    end do
                end do
            end do
        end if
        p => p%next
    end do
    part(1) = count(map)
end function

end program
