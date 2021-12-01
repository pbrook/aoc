module aoc

    implicit none

contains

subroutine read_ints(a, filename)
    character(*) :: filename
    integer, allocatable, intent(out) :: a(:)
    integer :: n
    integer :: fd
    integer :: stat
    integer :: val
    integer :: i

    n = 0
    open(newunit=fd, action='read', file=filename, iostat=stat)

    do
        read (fd, *, iostat=stat) val
        if (stat /= 0) then
            exit
        end if
        n = n + 1
    end do
    allocate(a(n))
    rewind(fd)
    do i = 1, n
        read (fd, *) a(i)
    end do

    close(fd)
end subroutine

subroutine assert(a, b)
    integer :: a
    integer :: b
    if (a /= b) then
        print *, 'Got:', a, 'Expected:', b
        error stop 1
    end if
end subroutine

end module
