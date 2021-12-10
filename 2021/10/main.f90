program main
    use aoc

    implicit none

    integer(8) :: a(2)

    a = parse('test')
    call assert8(a(1), 26397_8)
    call assert8(a(2), 288957_8)

    a = parse('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains


function parse(filename) result(part)
    character(*) :: filename
    integer(8) :: part(2)
    integer :: fd
    integer :: stat

    character(200) :: line
    character :: c
    integer :: i, pos
    integer(8) :: complete(100)
    integer :: ncomplete
    integer(8) :: score

    ncomplete = 0
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
        else
            score=0
            do i=pos,1,-1
                if (score * 5 < score) then
                    error stop
                end if
                score = score * 5 + part2(line(i:i))
            end do
            if (ncomplete == size(complete)) then
                error stop
            end if
            ncomplete = ncomplete + 1
            complete(ncomplete) = huge(score)
            do i=1,ncomplete
                if (complete(i) > score) then
                    complete(i:) = eoshift(complete(i:), -1, score)
                    exit
                end if
            end do
        end if
    end do
    part(2) = complete(ncomplete/2+1)
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

function part2(c)
    character :: c
    integer :: part2

    select case (c)
    case ('(')
        part2 = 1
    case ('[')
        part2 = 2
    case ('{')
        part2 = 3
    case ('<')
        part2 = 4
    case default
        error stop
    end select
end function

end program
