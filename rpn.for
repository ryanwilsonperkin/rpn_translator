! Translating algrbraic expressions to polish notation
!
! The variable names and their meanings are as follows:
!     source     the input string, in normal algebraic form
!     shier      array containing the hierarchy numbers of the input
!     opstck     'operator stack': the operators from the input
!     ohier      array containing the hierarchy numbers of the operators
!     polish     the output string, in polish notation
!
!     l          do index used in initializing
!     m          do index used in setting up shier array
!     i          pointer to index string (source and shier)
!     j          pointer to operator stack (opstck and ohier)
!     k          pointer to output string (polish)

      implicit none

      integer :: i, j, k, l, m
      integer(1), dimension(40) :: source, shier, opstck, ohier, polish
      integer(1) :: blank, lparen, rparen, plus, minus, astrsk, slash
      
      blank = ichar(' ')
      lparen = ichar('(')
      rparen = ichar(')')
      plus = ichar('+')
      minus = ichar('-')
      astrsk = ichar('*')
      slash = ichar('/')

      do
!
! Initialize arrays to zero or blank, as appropriate
          shier = 0
          ohier = 0
          opstck = blank
          polish = blank
!
! Read a 'data' card
          read (*, 30) source
  30      format (40a)
!
! In the following do-loop, m points to input columns, from left to right
! First blank signals end of string (embedded blanks are not allowed)
! It is assumed that if a character is not an operator or a
! parenthesis, it is a variable.
          do m = 1, 40
              if (source(m) .eq. blank) exit
!
! Set shier(m) to zero, then change it if the character is an operator
!     shier(m) = 0
              if (source(m) .eq. lparen) shier(m) = 1
              if (source(m) .eq. rparen) shier(m) = 2
              if (source(m) .eq. plus
     1            .or. source(m) .eq. minus) shier(m) = 3
              if (source(m) .eq. astrsk 
     1            .or. source(m) .eq. slash) shier(m) = 4
          end do
!
! If normal exit is taken, the card did not contain a blank
          if (m .eq. 1) then
              exit
          else if  (m .eq. 40) then
                  write (*, "(1x)") 'data input in error - no blanks'
                  cycle
          end if
!
! Initialize hierarchy numbers to get started properly
          shier(m) = 0
          ohier(1) = -1
!
! Initialize pointers
          j = 2
          k = 1

          do i = 1, m
              if ( shier(i) .eq. 0 ) then
                  polish(k) = source(i)
                  k = k + 1
              else if ( shier(i) .eq. 2 ) then
                  j = j - 1
              else
                  opstck(j) = source(i)
                  ohier(j) = shier(i)
                  j = j + 1
                  cycle
              end if

              do while ( ohier(j-1) .ge. shier(i + 1) )
                  polish(k) = opstck(j-1)
                  k = k + 1
                  j = j - 1
              end do
          end do

          write (*, 100) "INPUT: ", source, "RPN:   ", polish
 100      format (1h ,a7, 40a1/1h , a7, 40a1)
      end do
      end 
