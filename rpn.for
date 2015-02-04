! TRANSLATING ALGRBRAIC EXPRESSIONS TO POLISH NOTATION
!
! THE VARIABLE NAMES AND THEIR MEANINGS ARE AS FOLLOWS:
!     SOURCE     THE INPUT STRING, IN NORMAL ALGEBRAIC FORM
!     SHIER      ARRAY CONTAINING THE HIERARCHY NUMBERS OF THE INPUT
!     OPSTCK     'OPERATOR STACK': THE OPERATORS FROM THE INPUT
!     OHIER      ARRAY CONTAINING THE HIERARCHY NUMBERS OF THE OPERATORS
!     POLISH     THE OUTPUT STRING, IN POLISH NOTATION
!
!     L          DO INDEX USED IN INITIALIZING
!     M          DO INDEX USED IN SETTING UP SHIER ARRAY
!     I          POINTER TO INDEX STRING (SOURCE AND SHIER)
!     J          POINTER TO OPERATOR STACK (OPSTCK AND OHIER)
!     K          POINTER TO OUTPUT STRING (POLISH)
!
!     THE OTHER VARIABLES ARE ACTUALLY CONSTANTS, AND ARE
!     DEFINED IN THE DATA STATEMENT.
!
!

      implicit none

      integer :: I, J, K, L, M
      integer(1), dimension(40) :: SOURCE, SHIER, OPSTCK, OHIER, POLISH
      integer(1) :: BLANK, LPAREN, RPAREN, PLUS, MINUS, ASTRSK, SLASH
      
      BLANK = ICHAR(' ')
      LPAREN = ICHAR('(')
      RPAREN = ICHAR(')')
      PLUS = ICHAR('+')
      MINUS = ICHAR('-')
      ASTRSK = ICHAR('*')
      SLASH = ICHAR('/')

      DO
!
! INITIALIZE ARRAYS TO ZERO OR BLANK, AS APPROPRIATE
          SHIER = 0
          OHIER = 0
          OPSTCK = BLANK
          POLISH = BLANK
!
! READ A 'DATA' CARD
          READ (*, 30) SOURCE
  30      FORMAT (40A)
!
! IN THE FOLLOWING DO-LOOP, M POINTS TO INPUT COLUMNS, FROM LEFT TO RIGHT
! FIRST BLANK SIGNALS END OF STRING (EMBEDDED BLANKS ARE NOT ALLOWED)
! IT IS ASSUMED THAT IF A CHARACTER IS NOT AN OPERATOR OR A
! PARENTHESIS, IT IS A VARIABLE.
          DO M = 1, 40
              IF (SOURCE(M) .EQ. BLANK) EXIT
!
! SET SHIER(M) TO ZERO, THEN CHANGE IT IF THE CHARACTER IS AN OPERATOR
!     SHIER(M) = 0
              IF (SOURCE(M) .EQ. LPAREN) SHIER(M) = 1
              IF (SOURCE(M) .EQ. RPAREN) SHIER(M) = 2
              IF (SOURCE(M) .EQ. PLUS
     1            .OR. SOURCE(M) .EQ. MINUS) SHIER(M) = 3
              IF (SOURCE(M) .EQ. ASTRSK 
     1            .OR. SOURCE(M) .EQ. SLASH) SHIER(M) = 4
          END DO
!
! IF NORMAL EXIT IS TAKEN, THE CARD DID NOT CONTAIN A BLANK
          IF (M .EQ. 1) THEN
              EXIT
          ELSE IF  (M .EQ. 40) THEN
                  WRITE (*,50)
  50              FORMAT (1X, 'DATA INPUT IN ERROR - NO BLANKS')
                  CYCLE
          END IF
!
! INITIALIZE HIERARCHY NUMBERS TO GET STARTED PROPERLY
  60      SHIER(M) = 0
          OHIER(1) = -1
!
! INITIALIZE POINTERS
          I = 1
          J = 2
          K = 1

          DO I = 1, M
              IF ( SHIER(I) .EQ. 0 ) THEN !GO TO 90
                  POLISH(K) = SOURCE(I)
                  K = K + 1
              ELSE IF ( SHIER(I) .EQ. 2 ) THEN !GO TO 80
                  J = J - 1
              ELSE
                  OPSTCK(J) = SOURCE(I)
                  OHIER(J) = SHIER(I)
                  J = J + 1
                  CYCLE
              END IF

              DO WHILE ( OHIER(J-1) .GE. SHIER(I + 1) )
                  POLISH(K) = OPSTCK(J-1)
                  K = K + 1
                  J = J - 1
              END DO
          END DO

          WRITE (*, 100) "INPUT: ", SOURCE, "RPN:   ", POLISH
 100      FORMAT (1H ,A7, 40A1/1H , A7, 40A1)
      END DO
      END 
