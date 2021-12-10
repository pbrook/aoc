program main
    use aoc

    implicit none

    integer :: a(2)

    a = parse('test')
    call assert(a(1), 26397)
    !call assert(a(2), 1134)

    a = parse('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains


function parse(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(200) :: line
    character :: c
    integer :: i, pos

    part(1) = 0
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, *, iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        if (len_trim(line) == len(line)) then
            error stop
        end if
        pos = 0
        do i=1,len_trim(line)
            c = line(i:i)
            select case (c)
            case ('(','[','{','<')
                pos = pos + 1
                if (pos /= i) then
                    line(pos:pos) = line(i:i)
                end if
            case default
                if (.not. match(line(pos:pos), c)) then
                    pos = -1
                    exit
                end if
                pos = pos - 1
            end select
        end do
        if (pos == -1) then
            part(1) = part(1) + part1(c)
        end if
    end do
end function

function match(a, b)
    character :: a, b
    logical :: match

    select case (a)
    case ('(')
        match = b == ')'
    case ('[')
        match = b == ']'
    case ('{')
        match = b == '}'
    case ('<')
        match = b == '>'
    case default
        error stop
    end select
end function

function part1(c)
    character :: c
    integer :: part1

    select case (c)
    case (')')
        part1 = 3
    case (']')
        part1 = 57
    case ('}')
        part1 = 1197
    case ('>')
        part1 = 25137
    case default
        error stop
    end select
end function

end program
