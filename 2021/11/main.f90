program main
    use aoc

    implicit none

    integer :: a(2)

    a = octop('test')
    call assert(a(1), 1656)
    call assert(a(2), 195)

    a = octop('input')
    print *, "Part1:", a(1)
    print *, "Part2:", a(2)
contains


function octop(filename) result(part)
    character(*) :: filename
    integer :: part(2)
    integer :: fd
    integer :: stat

    integer :: grid(0:11,0:11)
    integer :: i, n

    grid=0 
    open(newunit=fd, action='read', file=filename)
    do i=1,10
        read (fd, "(*(i1))") grid(1:10,i)
    end do
    close(fd)
    part(1) = 0
    do i=1,100
        part(1) = part(1) + step(grid)
        !print *, i
        !call dump(grid)
    end do
    do
        n = step(grid)
        if (n == 100) then
            part(2) = i
            exit
        end if
        i = i + 1
    end do
end function

subroutine dump(grid)
    integer, intent(inout) :: grid(0:,0:)
    integer :: i
    do i=1,10
        print "(*(i1))", grid(1:10,i)
    end do
end subroutine

function step(grid) result(flash)
    integer, intent(inout) :: grid(0:,0:)
    integer :: flash
    logical :: again
    integer :: x, y

    flash = 0
    grid = grid + 1
    again = .true.
    do while (again)
        again = .false.
        do x=1,10
            do y=1,10
                if (grid(x,y) > 9) then
                    grid(x,y) = -huge(grid)
                    call inc(grid(x-1,y))
                    call inc(grid(x+1,y))
                    call inc(grid(x,y-1))
                    call inc(grid(x,y+1))
                    call inc(grid(x-1,y-1))
                    call inc(grid(x+1,y-1))
                    call inc(grid(x-1,y+1))
                    call inc(grid(x+1,y+1))
                    again = .true.
                    flash = flash + 1
                end if
            end do
        end do
    end do
    where (grid < 0)
        grid = 0
    end where
end function

subroutine inc(x)
    integer, intent(inout) :: x
    x = x + 1
end subroutine
end program
