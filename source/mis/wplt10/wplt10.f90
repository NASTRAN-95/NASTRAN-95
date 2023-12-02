!*==wplt10.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE wplt10(A,Opt)
!
!     TO WRITE PLOTTER COMMANDS FOR NASTRAN GENERAL PURPOSE PLOTTER
!     REF - NASTRAN PROGRAMMER'S MANUAL P.3.4-111
!
!     REVISED  9/1990 BY G.CHAN/UNISYS
!     SEE SGINO FOR IMPLEMENTATION OF PLT1 FILE
!
!     INPUT -
!        OPT = 0 IF ARRAY A IS A PLOT COMMAND.
!        OPT = 1 IF CURRENT SERIES OF PLOT COMMANDS IS TO BE TERMINATED
!
!     OUTPUT -
!       A(1) = PLOT MODE DIGIT
!       A(2) = CONTROL DIGIT
!       A(3) = X1 = X-COORDINATE
!       A(4) = Y1 = Y-COORDINATE
!       A(5) = X2 = X-COORDINATE
!       A(6) = Y2 = Y-COORDINATE
!
!     A PLT2 FILE PLOTTER COMMAND IS OF THE FOLLOWING FORMAT
!
!         MC1111122222333334444400000000
!            WHERE M = MODE            1 BYTE
!                  C = CONTROL         1 BYTE
!                  1 = DIGIT OF X1     5 BYTES
!                  2 = ..... .. Y1     5 BYTES
!                  3 = ..... .. X2     5 BYTES
!                  4 = ..... .. Y2     5 BYTES
!                  0 = ZERO            8 BYTES
!                              ---------------
!                              TOTAL  30 BYTES
!
!     SEE SGINO FOR PLT1 FILE PLOTTER COMMAND FORMAT
!
!     /PLTDAT/
!     EDGE = SIZE OF THE BORDERS (X,Y) IN PLOTTER UNITS,    REAL - INPUT
!     PLOT = GINO FILE NAME OF THE PLOT TAPE TO BE WRITTEN,  BCD - INPUT
!     MAXCHR = PLOT TAPE BUFFER SIZE (NUMBER OF CHARACTERS), INT - INPUT
!              (AN INTEGER MULTIPLE OF THE NUMBER OF CHARACTERS
!              PER WORD ON THE COMPUTER ON WHICH THE PLOT TAPE IS
!              BEING READ)
!
   IMPLICIT NONE
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: A
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(30) , SAVE :: c , zero
   INTEGER :: c1 , i , i3 , i4 , j , k , m , n
   INTEGER , SAVE :: nc , nchr , plt2 , pzero
   INTEGER , DIMENSION(5) , SAVE :: ten
   EXTERNAL swrite
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (c1,c(1))
   DATA nchr , ten , pzero/0 , 10000 , 1000 , 100 , 10 , 1 , +0/ , plt2 , nc , zero , c/4HPLT2 , 30 , 30*0 , 30*0/
!
   IF ( Plot/=plt2 ) THEN
!
!     PLT1 FILE - NON BYTE PACKING LOGIC
!     A FORMAT OF (5(2I3,4I5)) IS COMPOSED IN SGINO
!     =============================================
!
      nc = 6
      IF ( Opt/=0 ) THEN
!
!     TERMINATE A SET OF PLOT COMMANDS
!     SEND A RECORD OF ALL ZERO-S TO SWRITE
!
         CALL swrite(Plot,zero,nc,0)
         CALL swrite(Plot,0,0,1)
      ELSE
!
!     SET UP THE MODE AND CONTROL CHARACTERS IN THE COMMAND.
!
         c1 = A(1)
         c(2) = A(2)
!
         i3 = ifix(Edge(1)+.1)
         i4 = ifix(Edge(2)+.1)
         c(3) = A(3) + i3
         c(4) = A(4) + i4
         c(5) = A(5)
         c(6) = A(6)
         IF ( c1/=4 .AND. c1/=14 ) THEN
            c(5) = A(5) + i3
            c(6) = A(6) + i4
         ENDIF
         CALL swrite(Plot,c,nc,0)
      ENDIF
!
!     PLT2 FILE - WITH BYTE PACKING LOGIC
!     A FORMAT OF (10(180A4)) IS COMPOSED IN SGINO
!     ============================================
!
   ELSEIF ( Opt==0 ) THEN
!
!     SET UP THE MODE + CONTROL CHARACTERS IN THE COMMAND.
!
      c1 = A(1)
      c(2) = A(2)
!
!     SEPARATE THE DECIMAL DIGITS OF THE X + Y COORDINATES.
!
      DO j = 1 , 4
         i = 1
         IF ( j==2 .OR. j==4 ) i = 2
         n = A(j+2)
         IF ( j<3 .OR. (c1/=4 .AND. c1/=14) ) n = n + ifix(Edge(i)+.1)
         k = 5*(j-1)
         DO i = 1 , 5
            m = n/ten(i)
!
!   . M MAY BE A -0 (UNIVAC), SET IT TO +0 FOR SURE
!
            IF ( m==0 ) m = pzero
            c(k+3) = m
            k = k + 1
            n = n - m*ten(i)
         ENDDO
      ENDDO
!
      CALL swrite(Plot,c,nc,0)
      nchr = nchr + nc
      IF ( nchr==Maxchr ) nchr = 0
!
!     TERMINATE A SET OF PLOT COMMANDS (FILL THE RECORD WITH ZERO-S).
!
   ELSEIF ( nchr==0 ) THEN
      CALL swrite(Plot,0,0,1)
   ELSE
      SPAG_Loop_1_1: DO
         CALL swrite(Plot,zero,nc,0)
         nchr = nchr + nc
         IF ( nchr==Maxchr ) THEN
            nchr = 0
            CALL swrite(Plot,0,0,1)
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
!
END SUBROUTINE wplt10
