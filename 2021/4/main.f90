program main
    use aoc

    implicit none

type board
    integer :: grid(5,5)
    type(board), allocatable :: next
    integer :: win
end type board

    integer :: a(2)

    a = bingo('test')
    call assert(a(1), 4512)
    call assert(a(2), 1924)

    a = bingo('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function bingo(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat
    integer, allocatable :: nums(:)
    integer :: i
    character(400) :: line
    type(board), allocatable, target :: boards
    type(board), allocatable, target :: tmp
    type(board), pointer :: p
    type(board), pointer :: first
    integer :: pos(2)
    integer :: nboards
    integer :: ncount

    open(newunit=fd, action='read', file=filename)
    read (fd, '(a400)') line
    ncount = 1 + count([(line(i:i) == ',', i=1,len(line))])
    allocate(nums(ncount))
    read (line, *) nums

    nboards = 0
    do
        allocate(tmp)
        tmp%win = -1
        read (fd, *, iostat=stat) tmp%grid
        if (stat /= 0) then
            exit
        end if
        nboards = nboards + 1
        call move_alloc(boards, tmp%next)
        call move_alloc(tmp, boards)
    end do

    nullify(first)
    numloop: do i = 1,size(nums)
        if (nums(i)  == -1) then
            error stop
        end if
        p => boards
        do while (associated(p))
            pos = findloc(p%grid, nums(i))
            if (p%win == -1 .and. all(pos /= 0)) then
                p%grid(pos(1), pos(2)) = -1
                if (check(p%grid, 1) .or. check(p%grid, 2)) then
                    p%win = nums(i)
                    if (.not. associated(first)) then
                        first => p
                    end if
                    nboards = nboards - 1
                    if (nboards == 0) then
                        exit numloop
                    end if
                end if
            end if
            p => p%next
        end do
    end do numloop
    part(1) = score(first)
    part(2) = score(p)
end function

function check(grid, n)
    integer, intent(in) :: grid(5,5)
    integer, intent(in) :: n
    logical :: check

    check = any(all(grid == -1, dim=n))
end function

function score(p)
    type(board), intent(in) :: p
    integer :: score

    score = p%win * sum(p%grid, mask=p%grid /= -1)
end function

end program
