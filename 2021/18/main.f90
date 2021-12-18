program main
    use aoc

    implicit none

type node
    integer :: depth = 0
    integer :: val
    type(node), pointer :: next => NULL()
end type

    integer :: a(2)

    call test()

    a = snailsum('test0')
    call assert(a(1), 1384)

    a = snailsum('test1')
    call assert(a(1), 3488)

    a = snailsum('test2')
    call assert(a(1), 4140)
    !call assert(a(2), 112)

    a = snailsum('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

subroutine test()
    type(node), pointer :: p

    call assert(magnitude(parse_str("[[9,1],[1,9]]")), 129)
    call assert(magnitude(parse_str("[[1,2],[[3,4],5]]")), 143)
    call assert(magnitude(parse_str("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")), 1384)
    call assert(magnitude(parse_str("[[[[1,1],[2,2]],[3,3]],[4,4]]")), 445)
    call assert(magnitude(parse_str("[[[[3,0],[5,3]],[4,4]],[5,5]]")), 791)
    call assert(magnitude(parse_str("[[[[5,0],[7,4]],[5,5]],[6,6]]")), 1137)
    call assert(magnitude(parse_str("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")), 3488)
end subroutine

function snailsum(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    type(node), pointer :: val
    type(node), pointer :: newval
    character :: c

    open(newunit=fd, action='read', file=filename)
    val => parse_node(fd)
    do
       newval => parse_node(fd)
       if (.not. associated(newval%next)) then
           exit
       end if
       val => add_pair(val, newval)
    end do
    part(1) = magnitude(val)
end function

subroutine dump(val)
    type(node), pointer, intent(in) :: val
    type(node), pointer :: p
    integer :: i

    p => val%next
    do while (associated(p))
        if (p%depth > 0) then
            write (*, "(*(a1))", advance='no') ('[', i=1,p%depth)
        end if
        if (p%val >= 10) then
            write (*, "(i2)", advance='no') p%val
        else
            write (*, "(i1)", advance='no') p%val
        end if
        if (p%depth < 0) then
            write (*, "(*(a1))", advance='no') (']', i=1,-p%depth)
        end if
        if (associated(p%next)) then
            write (*, "(a1)", advance='no') ','
        end if
        p => p%next
    end do
    write (*, "()")
end subroutine

function last(val) result(p)
    type(node), pointer, intent(in) :: val
    type(node), pointer :: p

    p => val
    do while (associated(p%next))
        p => p%next
    end do
end function

function add_pair(a, b) result(val)
    type(node), pointer, intent(inout) :: a, b
    type(node), pointer :: val
    type(node), pointer :: p
    logical :: again

    p => last(a)
    p%next => b%next
    deallocate(b)
    p => last(p)
    p%depth = p%depth - 1
    p => a%next
    p%depth = p%depth + 1

    val => a
    again = .true.
    do while (again)
        again = explode(val)
        if (.not. again) then
            again = split(val)
        end if
    end do
end function

function split(val) result (again)
    type(node), pointer, intent(inout) :: val
    logical :: again
    type(node), pointer :: p, r

    again = .false.
    p => val%next
    do while (associated(p))
        if (p%val >= 10) then
            allocate(r)
            r%val = (p%val + 1) / 2
            p%val = p%val / 2
            r%next => p%next
            p%next => r
            if (p%depth > 0) then
                p%depth = p%depth + 1
                r%depth = -1
            else
                r%depth = p%depth - 1
                p%depth = 1
            end if
            again = .true.
            exit
        end if
        p => p%next
    end do
end function

recursive function mag1(p, r) result(left)
    type(node), pointer, intent(in) :: p
    type(node), pointer, intent(inout) :: r
    type(node), pointer :: tmp
    integer :: left, right
    integer :: i

    left = p%val
    do i=1,p%depth
        if (r%depth > 0) then
            tmp => r
            r => r%next
            right = mag1(tmp, r)
        else
            right = r%val
            r => r%next
        end if
        left = left * 3 + right * 2
    end do
end function


function magnitude(val)
    type(node), pointer, intent(in) :: val
    type(node), pointer :: p, r
    integer :: magnitude

    p => val%next
    r => p%next
    magnitude = mag1(p, r)
end function

function explode(val) result (again)
    type(node), pointer, intent(inout) :: val
    logical :: again
    type(node), pointer :: prev, p, r
    integer :: depth

    again = .false.
    prev => val
    p => val%next
    depth = 0
    do while (associated(p))
        depth = depth + p%depth
        if (depth > 4) then
            if (.not. associated(p%next)) then
                error stop
            end if
            prev%val = prev%val + p%val
            p%val = 0
            r => p%next
            if (associated(r%next)) then
                r%next%val = r%next%val + r%val
            end if
            if (p%depth == 1) then
                p%depth = r%depth + 1
            else if (r%depth == -1) then
                p%depth = p%depth - 1
            else
                print *, p%val, p%depth, r%depth
                error stop
            end if
            if (p%depth == 0) then
                error stop
            end if
            p%next => r%next
            p%val = 0
            deallocate(r)
            again = .true.
            exit
        end if
        prev => p
        p => p%next
    end do
end function

subroutine build_node(p, depth, c)
    type(node), pointer, intent(inout) :: p
    integer, intent(inout) :: depth
    character, intent(in) :: c

    select case (c)
    case ('[')
        depth = depth + 1
    case (',')
        if (depth /= 0) then
            error stop
        end if
    case (']')
        if (depth /= 0 .or. p%depth > 0) then
            error stop
        end if
        p%depth = p%depth - 1
    case default
        allocate(p%next)
        p => p%next
        p%depth = depth
        read (c, *) p%val
        depth = 0
    end select
end subroutine

function parse_str(s) result(head)
    character(*) :: s
    type(node), pointer :: head
    type(node), pointer :: p
    integer :: depth
    integer :: i

    depth = 0
    allocate(head)
    head%val = -1
    p => head
    do i=1,len(s)
        call build_node(p, depth, s(i:i))
    end do
end function

function parse_node(fd) result(head)
    integer, intent(in) :: fd
    type(node), pointer :: head
    type(node), pointer :: p
    character :: c
    integer :: depth
    integer :: stat

    depth = 0
    allocate(head)
    head%val = -1
    p => head
    do
        read (fd, "(a1)", advance='no', iostat=stat) c
        if (stat /= 0) then
            exit
        end if
        call build_node(p, depth, c)
    end do
end function

end program
