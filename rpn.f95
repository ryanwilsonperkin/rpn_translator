! RPN
! Translate algrbraic expressions to polish notation
! Program prompts for algebraic string as input
! Converts input to equivalent reverse polish notation string
! Limited to 40 characters of input
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
    ! Initialize hierarchy arrays to all 0s
    shier = 0
    ohier = 0

    ! Initialize character arrays to all blanks
    opstck = ' '
    polish = ' '

    ! Read (up to) 40 characters from input into source array
    read (*, "(40a)") source

    ! Determine hierarchy of characters in input stream
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
        case ('^')
            shier(m) = 5
        case default
            shier(m) = 0
        end select
    end do

    ! Program exits when first character of input is a blank
    if (m .eq. 1) then
        exit

    ! Error message if no blank at enf of input
    else if  (m .eq. 40) then
        write (*, "(1x)") 'data input in error - no blanks'
        cycle
    end if

    ! Initialize start of output hierarchy smaller than possible source values
    ohier(1) = -1

    ! Initialize operator stack and output string pointers
    j = 2
    k = 1

    ! Analyze source hierarchy values and build output string
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

        ! Pop higher values from operator hierarchy into output string
        do while (ohier(j-1) .ge. shier(i + 1))
            polish(k) = opstck(j-1)
            k = k + 1
            j = j - 1
        end do
    end do

    ! Print output string
    write (*, "(1x,a7,40a)") "INPUT: ", source
    write (*, "(1x,a7,40a)") "RPN:   ", polish
end do
end 
