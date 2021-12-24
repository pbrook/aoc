program main
    use aoc

    implicit none

    integer, parameter :: INP=1, ADD=2,MUL=3,DIV=4,REM=5,EQL=6
    character(3), parameter :: opstr(6) = ["inp","add","mul","div","mod","eql"]
    type insn
        integer :: op
        integer :: regno
        integer :: arg
        logical :: isreg
    end type

    integer :: a(2)

    a = monad('input', 'template')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function monad(progname, templatename) result (part)
    character(*), intent(in) :: progname, templatename
    integer :: part(2)

    type(insn), allocatable :: prog(:), template(:)
    integer :: blocksize
    integer :: stack(7)
    integer :: args(3)
    integer :: offset(14)
    integer :: serial(14)
    integer :: sp
    integer :: i

    call parse(prog, progname)
    call parse(template, templatename)

    blocksize = size(template)
    if (size(prog) /= blocksize * 14) then
        error stop
    end if
    sp = 0
    do i=1,14
        call match_template(prog(i*blocksize-(blocksize-1):i*blocksize), template, args)
        select case (args(1))
        case (1) !push
            if (args(2) < 10 .or. args(3) < 0) then
                error stop
            end if
            offset(i) = args(3)
            sp = sp + 1
            stack(sp) = i
        case (26) !pop
            if (args(2) >= 0) then
                error stop
            end if
            offset(i) = offset(stack(sp)) + args(2)
            offset(stack(sp)) = -offset(i)
            sp = sp - 1
        case default
            error stop
        end select
    end do
    if (sp /= 0) then
        error stop
    end if

    serial = 9 + min(offset, 0)
    call validate(prog, serial)
    part(1) = serial_num(serial)

    serial = 1 + max(offset, 0)
    call validate(prog, serial)
    part(2) = serial_num(serial)
end function

function serial_num(serial) result (num)
    integer, intent(in) :: serial(14)
    integer :: num
    integer :: i
    num = 0
    do i=1,14
        num = num * 10 + serial(i)
    end do
end function

subroutine match_template(prog, template, args)
    type(insn), intent(in) :: prog(:), template(:)
    integer, intent(inout) :: args(:)
    integer :: i

    do i=1,size(prog)
        call match_insn(prog(i), template(i), args)
    end do
end subroutine

subroutine match_insn(p, t, args)
    type(insn), intent(in) :: p, t
    integer, intent(inout) :: args(:)

    if (p%op /= t%op .or. p%regno /= t%regno) then
        error stop
    end if
    if (t%isreg .and. t%arg < 0) then
        if (p%isreg) then
            error stop
        end if
        args(-t%arg) = p%arg
    else if ((p%isreg .neqv. t%isreg) .or. p%arg /= t%arg) then
        error stop
    end if
end subroutine

subroutine parse(prog, filename)
    type(insn), allocatable, intent(out) :: prog(:)
    character(*), intent(in) :: filename
    integer :: fd
    integer :: stat

    character(10) :: line
    character(3) :: op
    character(3) :: argstr
    type(insn) :: cmd
    integer :: proglen
    integer :: i
    integer :: ar(1)

    open(newunit=fd, action='read', file=filename)
    proglen = 0
    do
        read (fd, "(a10)", iostat=stat) line
        if (stat /= 0) then
            exit
        end if
        proglen = proglen + 1
    end do
    rewind(fd)

    allocate(prog(proglen))

    do i=1,proglen
        read (fd, "(a10)") line
        if (len(line) == len_trim(line)) then
            error stop
        end if
        ar = findloc(opstr, line(:3))
        cmd%op = ar(1)
        if (cmd%op == 0) then
            error stop
        end if
        cmd%regno = get_regno(line(5:5))
        if (cmd%op == INP) then
            argstr = '0'
        else
            argstr = line(7:)
        end if
        select case (argstr(:1))
        case ('w':'z')
            cmd%arg = get_regno(argstr)
            cmd%isreg = .true.
        case ('*')
            read (argstr(2:), *) cmd%arg
            cmd%arg = -cmd%arg
            cmd%isreg = .true.
        case default
            read (argstr, *) cmd%arg
            cmd%isreg = .false.
        end select
        prog(i) = cmd
    end do
end subroutine

subroutine validate(prog, serial)
    type(insn), target, intent(in) :: prog(:)
    integer, intent(in) :: serial(14)
    type(insn), pointer :: cmd
    integer :: reg(4)
    integer :: regno
    integer :: arg
    integer :: i
    integer :: n

    reg = 0
    n = 1
    do i=1,size(prog)
        cmd => prog(i)

        regno = cmd%regno
        if (cmd%isreg) then
            arg = reg(cmd%arg)
        else
            arg = cmd%arg
        end if
        select case (cmd%op)
        case (INP)
            reg(regno) = serial(n)
            n = n + 1
        case (ADD)
            reg(regno) = reg(regno) + arg
        case (MUL)
            reg(regno) = reg(regno) * arg
        case (DIV)
            reg(regno) = reg(regno) / arg
        case (REM)
            reg(regno) = mod(reg(regno), arg)
        case (EQL)
            if (reg(regno) == arg) then
                reg(regno) = 1
            else
                reg(regno) = 0
            end if
        case default
            error stop
        end select
        !print *, opstr(cmd%op), reg(regno)
    end do
    if (reg(4) /= 0) then
        error stop
    end if
end subroutine

function get_regno(s) result (regno)
    character(*), intent(in) :: s
    integer :: regno
    regno = iachar(s) + 1 - iachar('w')
    if (regno < 1 .or. regno > 4) then
        error stop
    end if
end function

end program
