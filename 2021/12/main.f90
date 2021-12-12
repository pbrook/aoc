program main
    use aoc

    implicit none

type cave
    character(5) :: str
    logical :: big
    integer :: visited
    type(link), allocatable :: links
end type

type link
    type(cave), pointer :: p
    type(link), allocatable :: next
end type

    integer :: a(2)

    a = explore('test')
    call assert(a(1), 10)
    call assert(a(2), 36)

    a = explore('test2')
    call assert(a(1), 19)
    call assert(a(2), 103)

    a = explore('test3')
    call assert(a(1), 226)
    call assert(a(2), 3509)

    a = explore('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains


subroutine add_link(root, p)
    type(link), allocatable, target, intent(inout) :: root
    type(cave), pointer,intent(in) :: p
    type(link), allocatable, target :: tmp

    allocate(tmp)
    tmp%p => p
    call move_alloc(root, tmp%next)
    call move_alloc(tmp, root)
end subroutine

function find_cave(caves, str) result(p)
    type(link), allocatable, target, intent(inout) :: caves
    character(*), intent(in) :: str
    type(cave), pointer :: p
    type(link), pointer :: l

    l => caves
    do while (associated(l))
        if (l%p%str == str) then
            exit
        end if
        l => l%next
    end do
    if (associated(l)) then
        p => l%p
    else
        allocate(p)
        p%str = str
        p%big = llt(str(1:1), 'a')
        p%visited = 0
        call add_link(caves, p)
    end if
end function

function explore(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(11) :: line

    type(link), allocatable, target :: caves
    type(cave), pointer :: a
    type(cave), pointer :: b
    integer :: i

    open(newunit=fd, action='read', file=filename)
    do
        read (fd, *, iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        i = scan(line, '-')
        if (i == 0) then
            error stop
        end if
        a => find_cave(caves, line(:i-1))
        b => find_cave(caves, line(i+1:))
        call add_link(a%links, b)
        call add_link(b%links, a)
    end do
    close(fd)

    a => find_cave(caves, 'start')
    part(1) = traverse(a, 0)
    part(2) = traverse(a, 1)
    call cleanup(caves)
end function

subroutine cleanup(caves)
    type(link), allocatable, target, intent(inout) :: caves
    type(link), pointer :: l

    l => caves
    do while (associated(l))
        deallocate(l%p)
        l => l%next
    end do
end subroutine

recursive function traverse(c, again) result(n)
    type(cave), pointer, intent(in) :: c
    integer, intent(in) :: again
    integer :: n
    type(link), pointer :: l
    integer :: yetagain
    logical :: visit

    if (c%str == 'end') then
        n = 1
        return
    end if
    n = 0
    l => c%links
    c%visited = c%visited + 1
    do while (associated(l))
        if (c%big .and. l%p%big) then
            error stop
        end if
        yetagain = again
        if (l%p%big) then
            visit = .true.
        else if (l%p%visited == 0) then
            visit = .true.
        else if (l%p%visited <= again .and. l%p%str /= 'start') then
            visit = .true.
            yetagain = again - 1
        else
            visit = .false.
        end if

        if (visit) then
            n = n + traverse(l%p, yetagain)
        end if
        l => l%next
    end do
    c%visited = c%visited - 1
end function

end program
