! RPN
! Translate algrbraic expressions to polish_str notation
! Program prompts for algebraic string as input
! Converts input to equivalent reverse polish_str notation string
! Limited to 40 characters of input
!
! The variable names and their meanings are as follows:
!     source_str   the input string, in normal algebraic form
!     source_hier  array containing the hierarchy numbers of the input
!     op_stack     operator stack: the operators from the input
!     op_hier      array containing the hierarchy numbers of the operators
!     polish_str   the output string, in polish_str notation
!
!     m do index used in setting up source_hier array
!     i pointer to index string (source_str and source_hier)
!     j pointer to operator stack (op_stack and op_hier)
!     k pointer to output string (polish_str)

program rpn
implicit none

integer :: i, j, k, m
character, dimension(40) :: op_stack, polish_str, source_str
integer, dimension(40) :: source_hier, op_hier

do
    ! Initialize hierarchy arrays to all 0s
    source_hier = 0
    op_hier = 0

    ! Initialize character arrays to all blanks
    op_stack = ' '
    polish_str = ' '

    ! Read (up to) 40 characters from input into source_str array
    read (*, "(40a)") source_str

    ! Determine hierarchy of characters in input stream
    do m = 1, 40
        select case(source_str(m))
        case (' ')
            exit
        case ('(')
            source_hier(m) = 1
        case (')')
            source_hier(m) = 2
        case ('+', '-')
            source_hier(m) = 3
        case ('*', '/')
            source_hier(m) = 4
        case ('^')
            source_hier(m) = 5
        case default
            source_hier(m) = 0
        end select
    end do

    ! Program exits when first character of input is ablank
    if (m .eq. 1) then
        exit

    ! Error message if no blank at enf of input
    else if  (m .eq. 40) then
        write (*, "(1x)") 'data input in error - no blanks'
        cycle
    end if

    ! Initialize start of output hierarchy smaller than possible source_str values
    op_hier(1) = -1

    ! Initialize operator stack and output string pointers
    j = 2
    k = 1

    ! Analyze source_str hierarchy values and build output string
    do i = 1, m
        if (source_hier(i) .eq. 0) then
            polish_str(k) = source_str(i)
            k = k + 1
        else if (source_hier(i) .eq. 2) then
            j = j - 1
        else
            op_stack(j) = source_str(i)
            op_hier(j) = source_hier(i)
            j = j + 1
            cycle
        end if

        ! Pop higher values from operator hierarchy into output string
        do while (op_hier(j-1) .ge. source_hier(i + 1))
            polish_str(k) = op_stack(j-1)
            k = k + 1
            j = j - 1
        end do
    end do

    ! Print output string
    write (*, "(1x,a7,40a)") "INPUT: ", source_str
    write (*, "(1x,a7,40a)") "RPN:   ", polish_str
end do
end 
