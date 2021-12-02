program one
    use aoc

    implicit none
    integer, allocatable :: a(:)

    call read_ints(a, 'test')
    call assert(count_inc(a, 1), 7)
    call assert(count_inc(a, 3), 5)

    call read_ints(a, 'input')
    print *, 'Part1:', count_inc(a, 1)
    print *, 'Part2:', count_inc(a, 3)
contains

function count_inc(a, sz) result(res)
    integer :: a(:)
    integer :: sz
    integer :: res

    res = count(a(1+sz:) > a(:size(a)-sz))
end function

end program
