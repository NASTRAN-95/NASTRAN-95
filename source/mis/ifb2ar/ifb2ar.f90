!*==ifb2ar.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifb2ar(Type,Ifb,Ar,L)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Type
   INTEGER :: Ifb
   INTEGER , DIMENSION(1) :: Ar
   INTEGER :: L
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(12) :: c12
   CHARACTER(8) :: c8
   CHARACTER(7) , DIMENSION(10) , SAVE :: fmt
   CHARACTER(7) :: fmtx
   CHARACTER(10) :: fmty
   CHARACTER(10) , DIMENSION(9) , SAVE :: fnt
   INTEGER :: i , ia , j , k , spag_nextblock_1
   REAL :: ra , x , xl
   INTEGER , DIMENSION(2) , SAVE :: sub , zero
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE STORES IN ARRAY AR(L+1) THE BCD VALUE OF IFB, AND
!     UPDATE THE L COUNTER
!
!     IF TYPE=1, IFB IS AN INTEGER, AND 8 DIGITS ARE USED IN AR, AND
!                L IS INCREASED BY 2 (INTEGER IS RIGHT ADJUSTED)
!     IF TYPE=2, IFB IS A REAL NUMBER, 12 DIGITS ARE USED IN AR, AND
!                L IS INCREASED BY 3
!     IF TYPE=3, IFB IS A BCD WORD, 4 LETTERS ARE USE IN AR, AND
!                L IS INCREASED BY 1
!
   !>>>>EQUIVALENCE (ia,ra)
   DATA fmt/'(F12.9)' , '(F12.8)' , '(F12.7)' , '(F12.6)' , '(F12.5)' , '(F12.4)' , '(F12.3)' , '(F12.2)' , '(F12.1)' , '(F12.0)'/
   DATA fnt/'(1X,F11.8)' , '(1X,F11.7)' , '(1X,F11.6)' , '(1X,F11.5)' , '(1X,F11.4)' , '(1X,F11.3)' , '(1X,F11.2)' , '(1X,F11.1)' , &
       &'(1X,F11.0)'/
   DATA zero/4H     , 4H 0.0/
   DATA sub/4HIFB2 , 4HAR  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         k = -1
         j = Type + 1
         IF ( j==1 .OR. j==3 ) GOTO 40
         IF ( j==2 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( j==4 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      k = k + 1
         IF ( k<0 ) THEN
            CALL mesage(-37,0,sub)
         ELSEIF ( k/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     INTEGER, RIGHT ADJUSTED
!
         WRITE (c8,99001,ERR=40) Ifb
99001    FORMAT (I8)
         READ (c8,99002) Ar(L+1) , Ar(L+2)
99002    FORMAT (2A4)
         L = L + 2
         RETURN
      CASE (3)
!
!     BCD WORD
!
         Ar(L+1) = Ifb
         L = L + 1
         RETURN
!
!     REAL NUMBER
!
 40      ia = Ifb
         x = abs(ra)
         IF ( x<1.0E-36 ) THEN
            Ar(L+1) = zero(1)
            Ar(L+2) = zero(1)
            Ar(L+3) = zero(2)
            L = L + 3
            RETURN
         ELSE
            xl = alog10(x)
            IF ( xl>-4.0 .AND. xl<10.0 ) THEN
               IF ( xl<=1.0 ) THEN
                  fmtx = fmt(1)
                  WRITE (c12,fmtx) ra
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  i = xl
                  IF ( ra<0. ) i = i + 1
                  IF ( i>0 .AND. i<=9 ) THEN
                     IF ( ra>0. .AND. xl>0. ) THEN
                        fmty = fnt(i)
                        WRITE (c12,fmty) ra
                     ELSE
                        fmtx = fmt(i)
                        WRITE (c12,fmtx) ra
                     ENDIF
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
            WRITE (c12,99003,ERR=20) ra
99003       FORMAT (1P,E12.5)
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         READ (c12,99004) (Ar(L+j),j=1,3)
99004    FORMAT (3A4)
         L = L + 3
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ifb2ar
