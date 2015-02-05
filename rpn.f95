! Translating algrbraic expressions to polish notation
!
! The variable names and their meanings are as follows:
!     source     the input string, in normal algebraic form
!     shier      array containing the hierarchy numbers of the input
!     opstck     'operator stack': the operators from the input
!     ohier      array containing the hierarchy numbers of the operators
!     polish     the output string, in polish notation
!
!     m          do index used in setting up shier array
!     i          pointer to index string (source and shier)
!     j          pointer to operator stack (opstck and ohier)
!     k          pointer to output string (polish)
program rpn
implicit none

integer :: i, j, k, m
character, dimension(40) :: opstck, polish, source
integer, dimension(40) :: shier, ohier

do
    shier = 0
    ohier = 0
    opstck = ' '
    polish = ' '

    read (*, "(40a)") source

    do m = 1, 40
        select case(source(m))
        case (' ')
            exit
        case ('(')
            shier(m) = 1
        case (')')
            shier(m) = 2
        case ('+', '-')
            shier(m) = 3
        case ('*', '/')
            shier(m) = 4
        case default
            shier(m) = 0
        end select
    end do

    if (m .eq. 1) then
        exit
    else if  (m .eq. 40) then
        write (*, "(1x)") 'data input in error - no blanks'
        cycle
    end if

    ohier(1) = -1
    j = 2
    k = 1

    do i = 1, m
        if (shier(i) .eq. 0) then
            polish(k) = source(i)
            k = k + 1
        else if (shier(i) .eq. 2) then
            j = j - 1
        else
            opstck(j) = source(i)
            ohier(j) = shier(i)
            j = j + 1
            cycle
        end if

        do while (ohier(j-1) .ge. shier(i + 1))
            polish(k) = opstck(j-1)
            k = k + 1
            j = j - 1
        end do
    end do

    write (*, "(1x,a7,40a,/,1x,a7,40a)") "INPUT: ", source, "RPN:   ", polish
end do
end 
