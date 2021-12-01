program one
    use aoc

    implicit none
    integer, allocatable :: a(:)

    call read_ints(a, 'test')
    call assert(count_inc(a, 1), 7)
    call assert(count_inc(a, 3), 5)

    call read_ints(a, 'input')
    print *, count_inc(a, 1)
    print *, count_inc(a, 3)
contains

function count_inc(a, sz) result(res)
    integer :: a(:)
    integer :: sz
    integer :: res
    integer :: i

    res = 0

    do i = sz + 1, size(a)
        if (a(i - sz) < a(i)) then
            res = res + 1
        end if
    end do
end function

end program
