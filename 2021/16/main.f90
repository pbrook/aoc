program main
    use aoc

    implicit none

    integer :: a(2)

    character(3), parameter :: op_name(0:7) = &
        ['add', 'mul', 'min', 'max', 'lit', 'gt ', 'lt ', 'eq ']

type buffer
    character(:), pointer :: str
    integer :: pos
    integer :: nbits
    integer :: val
    integer :: used
    integer :: versum
end type

    type(buffer) :: buf

    a = decode_str('8A004A801A8002F478')
    call assert(a(1), 16)
    a = decode_str('620080001611562C8802118E34')
    call assert(a(1), 12)
    a = decode_str('C0015000016115A2E0802F182340')
    call assert(a(1), 23)
    a = decode_str('A0016C880162017C3686B18A3D4780')
    call assert(a(1), 31)

    a = decode_str('C200B40A82')
    call assert(a(2), 3)
    a = decode_str('04005AC33890')
    call assert(a(2), 54)
    a = decode_str('880086C3E88112')
    call assert(a(2), 7)
    a = decode_str('CE00C43D881120')
    call assert(a(2), 9)
    a = decode_str('D8005AC2A8F0')
    call assert(a(2), 1)
    a = decode_str('F600BC2D8F')
    call assert(a(2), 0)
    a = decode_str('9C005AC2F8F0')
    call assert(a(2), 0)
    a = decode_str('9C0141080250320F1802104A08')
    call assert(a(2), 1)

    a = decode_str('38006F45291200')
    call assert(a(1), 9)
    call assert(a(2), 1)

    a = decode_file('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2) ! not 233391313
contains

function decode_file(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(:), allocatable, target :: str
    integer :: strlen
    character :: c

    open(newunit=fd, action='read', file=filename)
    strlen=0
    do
        read(fd, "(a1)", advance='no', iostat=stat) c
        if (stat /= 0) then
            exit
        end if
        strlen = strlen + 1
    end do
    rewind(fd)
    allocate(character(strlen) :: str)
    read (fd, *) str
    close(fd)

    part = decode_str(str)
end function

function getbits(n) result(val)
    integer, intent(in) :: n
    integer :: val
    integer :: bits
    integer :: extra
    extra = buf%nbits - n
    do while (extra < 0)
        if (buf%pos > len(buf%str)) then
            error stop
        end if
        read (buf%str(buf%pos:buf%pos), '(Z1)') bits
        !print *, "getbits <-", bits, buf%pos
        buf%pos = buf%pos + 1
        buf%val = shiftl(buf%val, 4) + bits
        extra = extra + 4
    end do
    val = shiftr(buf%val, extra)
    buf%val = ibits(buf%val, 0, extra)
    buf%nbits = extra
    buf%used = buf%used + n
    !print *, "getbits", val, n, extra, buf%used, buf%val
end function

function decode_str(str) result(part)
    character(*), target, intent(in) :: str
    integer :: part(2)

    buf = buffer(str=null(), pos=1, nbits=0, val=0, used=0, versum=0)
    buf%str => str

    !print *, buf%str
    part(2) = decode1(0)
    part(1) = buf%versum
    !print *, "Unused:", len(str) * 4 - buf%used
end function

recursive function decode1(indent) result(val)
    integer :: val
    integer :: newval
    integer, intent(in) :: indent
    integer :: ptype
    integer :: tmp
    integer :: plength
    integer :: pcount
    logical :: bitlen

    buf%versum = buf%versum + getbits(3)
    ptype = getbits(3)
    select case (ptype)
    case (4) ! literal
        val = 0
        tmp = ibset(0, 4)
        do while (ibits(tmp, 4, 1) /= 0)
            if (val > maskr(63-4, 8)) then
                error stop
            end if
            tmp = getbits(5)
            val = shiftl(val, 4) + ibits(tmp, 0, 4)
        end do
        !print *, repeat(' ', indent), 'lit ', val
    case default
        bitlen = getbits(1) == 0
        if (bitlen) then
            plength = getbits(15)
            pcount = buf%used
        else
            plength = getbits(11)
            pcount = 0
        end if
        val = decode1(indent+1)
        !print *, repeat(' ', indent), op_name(ptype), '0', val
        do
            if (bitlen) then
                if (buf%used - pcount == plength) then
                    exit
                end if
                if (buf%used - pcount > plength) then
                    error stop
                end if
            else 
                pcount = pcount + 1
                if (pcount == plength) then
                    exit
                end if
            end if

            newval = decode1(indent+1)
            select case (ptype)
            case (0) ! sum
                val = val + newval
            case (1) ! product
                val = val * newval
            case (2) ! minimum
                val = min(val, newval)
            case (3) ! maximum
                val = max(val, newval)
            case (5) ! greater
                val = bool(val > newval)
            case (6) ! less
                val = bool(val < newval)
            case (7) ! equal
                val = bool(val == newval)
            case default
                error stop
            end select
            if (val < 0) then ! partial overflow check
                error stop
            end if
            !print *, repeat(' ', indent), op_name(ptype),'N', val, newval
        end do
    end select
end function

elemental function bool(val)
    logical, intent(in) :: val
    integer :: bool
    if (val) then
        bool = 1
    else
        bool = 0
    end if
end function

end program
