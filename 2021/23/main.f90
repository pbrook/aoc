program main
    use aoc

    implicit none

    integer, parameter :: hallpos(7) = [1, 2, 4, 6, 8, 10, 11]
    integer, parameter :: homepos(8) = [3, 5, 7, 9, 3, 5, 7, 9]
    integer, parameter :: move_cost(8) = [1, 10, 100, 1000, 1, 10, 100, 1000]
    character(8), parameter :: pod_name = 'ABCDabcd'
    type pod
        integer :: pos
        logical :: bot
        logical :: home
        logical :: moved
    end type
    type cave
        type(pod) :: p(8)
        integer :: blocked(11)
    end type

    integer :: energy
    integer :: best
    integer :: steps(16)
    type(cave) :: winner(16)

    integer :: a(2)

    a = organize('test')
    call assert(a(1), 12521)
    !call assert(a(2), 39)


    a = organize('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function organize(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(15) :: line
    integer :: i, n

    type(cave) :: c

    open(newunit=fd, action='read', file=filename)

    c%p = pod(0, .false., .false., .false.)
    c%blocked = 0
    do i=1,4
        c%blocked(homepos(i)) = -2
    end do

    call check_line(fd, '#############')
    call check_line(fd, '#...........#')
    call parse_podline(c, fd, .false.)
    call parse_podline(c, fd, .true.)
    call check_line(fd, '  #########')

    !call dump(c)

    energy = 0
    best = huge(best)
    steps = 0
    call try(c)
    do i=1,16
        if (steps(i) /= 0) then
            !print *, steps(i)
            !call dump(winner(i))
        end if
    end do
    part(1) = best
    part(2) = 0
end function

subroutine check_line(fd, s)
    integer, intent(in) :: fd
    character(*), intent(in) :: s
    character(15) :: line

    call read_line(fd, line)
    if (line /= s) then
        error stop
    end if
end subroutine

subroutine parse_podline(c, fd, bot)
    type(cave), intent(inout) :: c
    integer, intent(in) :: fd
    logical, intent(in) :: bot
    character(15) :: line
    integer :: i, n
    integer :: pos

    call read_line(fd, line)

    if (.not. (bot .or. (line(:2) == '##' .and. line(12:) == '##'))) then
        error stop
    end if
    do i=3,11,2
        if (line(i:i) /= '#') then
            error stop
        end if
    end do
    do i=1,4
        pos = homepos(i)
        n = iachar(line(i*2+2:i*2+2)) + 1 - iachar('A')
        if (n < 1 .or. n > 4) then
            error stop
        end if
        if (c%p(n)%pos /= 0) then
            n = n + 4
            if (c%p(n)%pos /= 0) then
                error stop
            end if
        end if
        c%p(n)%pos = pos
        c%p(n)%bot = bot
        if (bot .and. pos == homepos(n)) then
            c%p(n)%home = .true.
            c%blocked(pos) = -3
        end if
    end do
end subroutine

recursive subroutine move_pod(c, copy, n)
    type(cave), intent(in) :: c
    type(cave), intent(inout) :: copy
    integer, intent(in) :: n
    integer :: prev_e
    integer :: dist
    integer :: depth = 0
    logical :: won(16) = .false.

    depth = depth + 1
    prev_e = energy
    dist = abs(c%p(n)%pos - copy%p(n)%pos) + 1
    if (copy%p(n)%bot) then
        dist= dist + 1
    end if
    energy = energy + dist * move_cost(n)
    if (energy < best) then
        if (depth < 3) then
            !print *, "move", int(n, kind=1), energy, depth
        end if
        if (all(copy%p%home)) then
            !print *, "Win", energy
            won(:depth) = .true.
            best = energy
        else
            call try(copy)
        end if
        if (won(depth)) then
            steps(depth) = dist
            winner(depth) = copy
            won(depth) = .false.
        end if
    else
        !print *, "bad"
    end if
    depth = depth - 1
    energy = prev_e
    copy%p(n) = c%p(n)
    copy%blocked = c%blocked
end subroutine

recursive subroutine try(c)
    type(cave), intent(in) :: c
    type(cave) :: copy
    integer :: n
    integer :: i
    integer :: pos, oldpos

    copy = c
    do n=1,8
        if (c%p(n)%home) then
            cycle
        end if
        oldpos = c%p(n)%pos
        if (c%p(n)%moved) then
            pos = homepos(n)
            i = c%blocked(pos)
            if (hall_clear(c, oldpos, pos) .and. i >= 0) then
                copy%p(n)%pos = pos
                copy%p(n)%bot = (i == 0)
                copy%p(n)%home = .true.
                copy%blocked(pos) = i + 1
                copy%blocked(oldpos) = 0
                call move_pod(c, copy, n)
            end if
        else
            if (c%p(n)%bot .and. c%blocked(c%p(n)%pos) == -2) then
                cycle
            end if
            do i=1,size(hallpos)
                pos = hallpos(i)
                if (hall_clear(c, oldpos, pos) .and. c%blocked(pos) == 0) then
                    copy%blocked(pos) = 10
                    select case (copy%blocked(c%p(n)%pos))
                    case (-2, -1)
                        copy%blocked(c%p(n)%pos) = copy%blocked(c%p(n)%pos) + 1
                    case (-3)
                        copy%blocked(c%p(n)%pos) = 1
                    case default
                        error stop
                    end select
                    copy%p(n)%pos = pos
                    copy%p(n)%moved = .true.
                    call move_pod(c, copy, n)
                end if
            end do
        end if
    end do
end subroutine

function hall_clear(c, from, to) result (clear)
    type(cave), intent(in) :: c
    integer, intent(in) :: from, to
    logical :: clear
    integer :: l, r, pos
    integer :: i

    clear = .true.
    l = min(from, to)
    r = max(from, to)
    do i=1,size(hallpos)
        pos = hallpos(i)
        if (pos > l .and. pos < r .and. c%blocked(pos) /= 0) then
            clear = .false.
            exit
        end if
    end do
end function

subroutine read_line(fd, line)
    integer, intent(in) :: fd
    character(15), intent(out) :: line
    read (fd, "(a15)") line
    if (len(line) == len_trim(line)) then
        error stop
    end if
end subroutine

subroutine dump(c)
    type(cave), intent(in) :: c
    character(13) :: s
    integer :: n, i, pos
    print "(a13)", '#############'
    s = '#...........#'
    do i=1,size(hallpos)
        pos = hallpos(i)
        do n=1,8
            if (c%p(n)%pos == pos) then
                s(pos+1:pos+1) = pod_name(n:n)
            end if
        end do
    end do
    print "(a13)", s
    s = '###.#.#.#.###'
    do i=1,size(homepos)
        pos = homepos(i)
        do n=1,8
            if (c%p(n)%pos == pos .and. .not. c%p(n)%bot) then
                s(pos+1:pos+1) = pod_name(n:n)
            end if
        end do
    end do
    print "(a13)", s
    s = '  #.#.#.#.#  '
    do i=1,size(homepos)
        pos = homepos(i)
        do n=1,8
            if (c%p(n)%pos == pos .and. c%p(n)%bot) then
                s(pos+1:pos+1) = pod_name(n:n)
            end if
        end do
    end do
    print "(a13)", s
    print "(a13)", '  #########  '
end subroutine

end program
