program main
    use aoc

    implicit none

type rule
    character(2) :: from
    character :: to
end type

    integer :: a(2)

    a = polymer('test')
    call assert(a(1), 1588)
    !call assert(a(2), 16)

    a = polymer('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function polymer(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(30) :: line
    type(rule), allocatable :: rules(:)
    integer :: i
    character(2) :: dummy
    integer :: cc(26)


    open(newunit=fd, action='read', file=filename)
    read (fd, *) line
    if (len(line) == len_trim(line)) then
        error stop
    end if
    read(fd, *)
    i = 0
    do
        read (fd, *, iostat=stat)
        if (stat /= 0) then
            exit
        end if
        i = i + 1
    end do
    rewind(fd)
    allocate(rules(i))
    read (fd, *)
    read (fd, *)
    do i=1,size(rules)
        read (fd, *) rules(i)%from, dummy, rules(i)%to
    end do
    close(fd)

    cc = 0
    do i=1,len_trim(line)
        call count_char(cc, line(i:i))
    end do
    do i=1,len_trim(line)-1
        call fold(rules, cc, line(i:i+1), 10)
    end do
    part(1) = maxval(cc) - minval(cc, mask=cc>0)
end function

subroutine count_char(cc, c)
    integer, intent(inout) :: cc(26)
    character, intent(in) :: c
    integer :: n

    n = iachar(c) + 1 - iachar('A')
    if (n<=0 .or. n>size(cc)) then
        error stop
    end if
    cc(n) = cc(n) + 1
end subroutine

recursive subroutine fold(rules, cc, match, depth)
    type(rule), allocatable, intent(in) :: rules(:)
    integer, intent(inout) :: cc(26)
    character(2) :: match
    integer, intent(in) :: depth
    character :: c
    integer :: i

    do i=1,size(rules)
        if (rules(i)%from == match) then
            c = rules(i)%to
            call count_char(cc, c)
            if (depth > 1) then
                call fold(rules, cc, match(1:1)//c, depth-1)
                call fold(rules, cc, c//match(2:2), depth-1)
            end if
            exit
        end if
    end do
end subroutine

end program
