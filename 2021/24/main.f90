program main
    use aoc

    implicit none

    print *, "Part1:", alu('input', '59692994994998')
    print *, "Part2:", alu('input', '16181111641521')
contains

function alu(filename, input) result(serial)
    character(*) :: filename
    character(14) :: input
    integer :: serial
    integer :: fd
    integer :: stat

    character(10) :: line
    character(3) :: insn
    character(3) :: argstr

    integer :: reg(4)
    integer :: regno
    integer :: arg
    integer :: inp

    inp = 1
    reg = 0
    open(newunit=fd, action='read', file=filename)
    do
        read (fd, "(a10)", iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        if (len(line) == len_trim(line)) then
            error stop
        end if
        insn = line(:3)
        regno = get_regno(line(5:5))
        if (insn == 'inp') then
            arg = 0
        else
            argstr = line(7:)
            select case (argstr)
            case ('w':'z')
                arg = reg(get_regno(argstr))
            case default
                read (argstr, *) arg
            end select
        end if
        select case (insn)
        case ('inp')
            read (input(inp:inp), *) reg(regno)
            inp = inp + 1
        case ('add')
            reg(regno) = reg(regno) + arg
        case ('mul')
            reg(regno) = reg(regno) * arg
        case ('div')
            reg(regno) = reg(regno) / arg
        case ('mod')
            reg(regno) = mod(reg(regno), arg)
        case ('eql')
            if (reg(regno) == arg) then
                reg(regno) = 1
            else
                reg(regno) = 0
            end if
        case default
            error stop
        end select
        !print *, line(:5), '=', reg(regno)
    end do
    close(fd)
    if (reg(4) /= 0) then
        error stop
    end if
    read (input, *) serial
end function

function get_regno(s) result (regno)
    character(*), intent(in) :: s
    integer :: regno
    regno = iachar(s) + 1 - iachar('w')
    if (regno < 1 .or. regno > 4) then
        error stop
    end if
end function

end program
