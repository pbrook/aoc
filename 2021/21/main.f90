program main
    use aoc

    implicit none

    type multiverse
        ! pos1, pos2, score1, score2
        integer :: num(10, 10, 0:20, 0:20)
        integer :: won(2)
    end type

    integer :: a(2)

    a = diracdice('test')
    call assert(a(1), 739785)
    call assert(a(2), 444356092776315)

    a = diracdice('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains

function diracdice(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    integer :: score(2)
    integer :: start(2)
    integer :: pos(2)
    integer :: dice
    integer :: nrolls
    integer :: p

    type(multiverse) :: v
    type(multiverse) :: tmp

    open(newunit=fd, action='read', file=filename)

    read (fd, "(28x i1)") start(1)
    read (fd, "(28x i1)") start(2)

    nrolls = 0
    dice = 1
    score = 0
    pos = start
    p = 1
    do
        call roll3(pos(p), score(p), dice)
        nrolls = nrolls + 3
        if (score(p) >= 1000) then
            exit
        end if
        p = 3 - p
    end do
    part(1) = nrolls * score(3-p)

    v%num = 0
    v%won = 0
    v%num(start(1), start(2), 0, 0) = 1
    do while (any(v%num > 0))
        call dirac_step(tmp, v, 1)
        call dirac_step(v, tmp, 2)
    end do
    part(2) = maxval(v%won)
end function

subroutine dirac_step(new, old, player)
    type(multiverse), intent(out) :: new
    type(multiverse), intent(in) :: old
    integer, intent(in) :: player
    integer :: p1, p2, s1, s2
    integer :: n

    new%num = 0
    new%won = old%won
    do p1=1,10
        do p2=1,10
            do s1=0,20
                do s2=0,20
                    n = old%num(p1, p2, s1, s2)
                    if (n > 0) then
                        call dirac_roll(new, player, n, p1, p2, s1, s2)
                    end if
                end do
            end do
        end do
    end do
end subroutine

subroutine add(a, b)
    integer, intent(inout) :: a
    integer, intent(in) :: b

    a = a + b
end subroutine

subroutine dirac_roll(v, player, n, p1, p2, s1, s2)
    type(multiverse), intent(inout) :: v
    integer, intent(in) :: player, n, p1, p2, s1, s2
    integer, parameter :: splits(3:9) = [1,3,6,7,6,3,1]
    integer :: a, b, c, d
    integer :: newscore
    integer :: roll
    integer :: nsplit

    do roll=3,9
        nsplit = n * splits(roll)
        if (player == 1) then
            a = plus_mod(p1, roll, 10)
            b = p2
            c = s1 + a
            d = s2
            newscore = c
        else
            a = p1
            b = plus_mod(p2, roll, 10)
            c = s1
            d = s2 + b
            newscore = d
        end if

        if (newscore >= 21) then
            call add(v%won(player), nsplit)
        else
            call add(v%num(a, b, c, d), nsplit)
        end if
    end do
end subroutine

pure function other(p)
    integer, intent(in) :: p
    integer :: other
    other = 3 - p
end function

pure function plus_mod(a, b, m)
    integer, intent(in) :: a, b, m
    integer :: plus_mod

    plus_mod = mod(a + b - 1, m) + 1
end function

subroutine roll3(pos, score, dice)
    integer, intent(inout) :: pos, score, dice
    integer :: i, n

    do i=1,3
        pos = plus_mod(pos, dice, 10)
        dice = plus_mod(dice, 1, 100)
    end do
    score = score + pos
end subroutine

end program
