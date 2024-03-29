program main
    use aoc

    implicit none

type cache
    integer(8) :: cc(26)
    logical :: valid
end type

type rule
    character(2) :: from
    character :: to
    type(cache) :: depth(40)
end type

    integer(8) :: a(2)

    a = polymer('test')
    call assert8(a(1), 1588_8)
    call assert8(a(2), 2188189693529_8)

    a = polymer('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function polymer(filename) result(part)
    character(*) :: filename
    integer(8) :: part(2)
    integer :: fd
    integer :: stat

    character(30) :: line
    type(rule), allocatable :: rules(:)
    integer :: i
    character(2) :: dummy
    integer(8) :: cc(26)


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
        rules(i)%depth(:)%valid = .false.
        call init_cc(rules(i)%depth(1)%cc, rules(i)%to)
        rules(i)%depth(1)%valid = .true.
    end do
    close(fd)

    part(1) = foldN(rules, line, 10)
    part(2) = foldN(rules, line, 40)
end function

function foldN(rules, line, depth)
    type(rule), allocatable, intent(inout) :: rules(:)
    character(*), intent(in) :: line
    integer, intent(in) :: depth
    integer(8) :: foldN
    integer(8) :: cc(26)
    integer :: i

    cc = 0
    do i=1,len_trim(line)
        call count_char(cc, line(i:i))
    end do
    do i=1,len_trim(line)-1
        call fold(rules, cc, line(i:i+1), depth)
    end do
    foldN = maxval(cc) - minval(cc, mask=cc>0)
end function

subroutine init_cc(cc, c)
    integer(8), intent(inout) :: cc(26)
    character, intent(in) :: c
    cc = 0
    call count_char(cc, c)
end subroutine

subroutine count_char(cc, c)
    integer(8), intent(inout) :: cc(26)
    character, intent(in) :: c
    integer :: n

    n = iachar(c) + 1 - iachar('A')
    if (n<=0 .or. n>size(cc)) then
        error stop
    end if
    cc(n) = cc(n) + 1
end subroutine

recursive subroutine fold(rules, cc, match, depth)
    type(rule), allocatable, intent(inout) :: rules(:)
    integer(8), intent(inout) :: cc(26)
    character(2) :: match
    integer, intent(in) :: depth
    integer(8) :: cc0(26)
    character :: c
    integer :: i

    do i=1,size(rules)
        if (rules(i)%from == match) then
            if (.not. rules(i)%depth(depth)%valid) then
                c = rules(i)%to
                call init_cc(cc0, c)
                call fold(rules, cc0, match(1:1)//c, depth-1)
                call fold(rules, cc0, c//match(2:2), depth-1)
                rules(i)%depth(depth)%cc = cc0
                rules(i)%depth(depth)%valid = .true.
            end if
            cc = cc + rules(i)%depth(depth)%cc
            exit
        end if
    end do
end subroutine

end program
