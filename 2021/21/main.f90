program main
    use aoc

    implicit none

    integer :: a(2)

    a = diracdice('test')
    call assert(a(1), 739785)
    !call assert(a(2), 3351)

    a = diracdice('input')
    print *, "Part1:", a(1)
    !print *, "Part2:", a(2)
contains

function diracdice(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    integer :: score(2)
    integer :: pos(2)
    integer :: dice
    integer :: nrolls
    integer :: p

    open(newunit=fd, action='read', file=filename)

    read (fd, "(28x i1)") pos(1)
    read (fd, "(28x i1)") pos(2)

    nrolls = 0
    dice = 1
    score = 0
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
    part(2) = 0
end function

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
