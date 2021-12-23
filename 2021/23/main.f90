program main
    use aoc

    implicit none

    integer, parameter :: hallpos(7) = [1, 2, 4, 6, 8, 10, 11]
    integer, parameter :: homepos(16) = [3, 5, 7, 9, 3, 5, 7, 9, 3, 5, 7, 9, 3, 5, 7, 9]
    integer, parameter :: move_cost(16) = [1, 10, 100, 1000, 1, 10, 100, 1000, 1, 10, 100, 1000, 1, 10, 100, 1000]
    character(32), parameter :: pod_name = 'ABCDabcdEFGHefgh'
    type pod
        integer :: pos
        integer :: stack
        logical :: home
        logical :: moved
    end type
    type cave
        type(pod) :: p(16)
        integer :: blocked(11)
    end type

    integer :: nlines
    integer :: npods
    integer :: energy
    integer :: best
    integer :: steps(32)
    integer :: force(32)
    integer :: forcepos(32)
    type(cave) :: winner(32)
    logical :: won(32) = .false.

    integer :: a(2)

    a = organize('test')
    call assert(a(1), 12521)
    call assert(a(2), 44169)


    a = organize('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function organize(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    character(15) :: line(4)
    integer :: i, n

    open(newunit=fd, action='read', file=filename)
    call check_line(fd, '#############')
    call check_line(fd, '#...........#')
    call read_line(fd, line(2))
    call read_line(fd, line(1))
    call check_line(fd, '  #########')
    close(fd)

    force = 0
    forcepos = 0
    part(1) = solve_cave(line(:2))

    !call forcelist(force, 'hefbEggGGBDBbfccaDFFHHdEed')
    !forcepos(:5) = [11,1,10,8,2]
    line(4) = line(2)
    line(3) = '  #D#C#B#A#'
    line(2) = '  #D#B#A#C#'
    part(2) = solve_cave(line)
end function

function csub(a, b)
    character, intent(in) :: a, b
    integer :: csub
    csub = iachar(b) + 1 - iachar(a)
end function

subroutine forcelist(a, s)
    character(*), intent(in) :: s
    integer, intent(out) :: a(:)
    integer :: i

    a = 0
    do i=1,len(s)
        a(i) = scan(pod_name, s(i:i))
    end do
end subroutine

function solve_cave(line)
    character(15), intent(in) :: line(:)
    integer :: solve_cave

    type(cave) :: c
    integer :: i, n

    nlines = size(line)

    npods = nlines * 4
    c%p = pod(0, 0, .false., .false.)
    c%blocked = 0
    do i=1,4
        c%blocked(homepos(i)) = nlines - 1
    end do
    do i=1,nlines
        call parse_podline(c, line(i), i)
    end do

    energy = 0
    best = huge(best)
    steps = 0
    won = .false.
    call try(c)

    !call dump(c)

    do i=1,size(steps)
        if (steps(i) /= 0) then
            !print *, steps(i)
            !call dump(winner(i))
        end if
    end do
    solve_cave = best
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

subroutine parse_podline(c, line, stack)
    type(cave), intent(inout) :: c
    integer, intent(in) :: stack
    character(15), intent(in) :: line
    integer :: i, n
    integer :: pos

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
        do while (c%p(n)%pos /= 0)
            n = n + 4
            if (n > npods) then
                error stop
            end if
        end do
        c%p(n)%pos = pos
        c%p(n)%stack = stack
    end do
end subroutine

recursive subroutine move_pod(c, copy, n)
    type(cave), intent(in) :: c
    type(cave), intent(inout) :: copy
    integer, intent(in) :: n
    integer :: prev_e
    integer :: dist
    integer :: depth = 0
    logical :: allow

    depth = depth + 1
    prev_e = energy
    dist = abs(c%p(n)%pos - copy%p(n)%pos) + 1 + (nlines - copy%p(n)%stack)
    energy = energy + dist * move_cost(n)
    if (force(depth) == 0) then
        allow = .true.
    else if (n /= force(depth)) then
        allow = .false.
    else
        allow = .true.
        if (forcepos(depth) /= 0 .and. copy%p(n)%pos /= forcepos(depth)) then
            allow = .false.
        end if
    end if
    if (energy < best .and. allow) then
        if (depth < 3) then
            !print *, "move ", pod_name(n:n), int(copy%p(n)%pos, kind=1), copy%p(n)%stack, depth
        end if
        if (all(copy%p(:npods)%home)) then
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

subroutine fixup(c, pos)
    type(cave), intent(inout) :: c
    integer, intent(in) :: pos
    integer :: n

    do n=1,npods
        if (homepos(n) == pos .and. c%p(n)%pos == pos) then
            c%p(n)%home = .true.
            c%blocked(pos) = -2
        end if
    end do
end subroutine

recursive subroutine try(c)
    type(cave), intent(in) :: c
    type(cave) :: copy
    integer :: n
    integer :: i
    integer :: pos, oldpos

    copy = c

    do n=1,npods
        if (copy%p(n)%home) then
            cycle
        end if
        oldpos = c%p(n)%pos
        if (c%p(n)%moved) then
            pos = homepos(n)
            i = copy%blocked(pos)
            if (hall_clear(c, oldpos, pos) .and. i < 0) then
                copy%p(n)%pos = pos
                copy%p(n)%stack = - i
                copy%p(n)%home = .true.
                copy%blocked(pos) = i - 1
                copy%blocked(oldpos) = 0
                call move_pod(c, copy, n)
            end if
        else
            if (c%p(n)%stack <= c%blocked(oldpos)) then
                cycle
            end if
            do i=1,size(hallpos)
                pos = hallpos(i)
                if (hall_clear(c, oldpos, pos) .and. c%blocked(pos) == 0) then
                    copy%blocked(pos) = 10
                    copy%blocked(oldpos) = copy%blocked(oldpos) - 1
                    copy%p(n)%pos = pos
                    copy%p(n)%moved = .true.
                    if (c%p(n)%stack == 2) then
                        call fixup(copy, oldpos)
                    end if
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
    integer :: n, i, pos, stack
    print "(a13)", '#############'
    s = '#...........#'
    do i=1,size(hallpos)
        pos = hallpos(i)
        do n=1,npods
            if (c%p(n)%pos == pos) then
                s(pos+1:pos+1) = pod_name(n:n)
            end if
        end do
    end do
    print "(a13)", s
    do stack=nlines,1,-1
        s = '###.#.#.#.###'
        do i=1,4
            pos = homepos(i)
            do n=1,npods
                if (c%p(n)%pos == pos .and. c%p(n)%stack == stack) then
                    s(pos+1:pos+1) = pod_name(n:n)
                end if
            end do
        end do
        print "(a13)", s
    end do
    s = '  #.#.#.#.#  '
    print "(a13)", '  #########  '
end subroutine

end program
