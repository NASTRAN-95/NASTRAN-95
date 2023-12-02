!*==tipe.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tipe(X,Y,Xyd,Chr,N,Opt)
   USE c_char94
   USE c_pltdat
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X
   REAL :: Y
   INTEGER :: Xyd
   INTEGER , DIMENSION(1) :: Chr
   INTEGER :: N
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: blank , lstchr
   INTEGER , DIMENSION(80) :: c
   INTEGER :: charx , d , i , j , l , l1 , l2 , nc
   REAL :: s
   REAL , DIMENSION(2,2) :: xy
   EXTERNAL type10
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     (X,Y) = STARTING OR ENDING POINT OF THE LINE TO BE TYPED (ALWAYS
!             LEFT-TO-RIGHT OR TOP-TO-BOTTOM.
!     XYD   = +/-1 IF X = STARTING OR ENDING POINT OF THE LINE.
!           = +/-2 IF Y = STARTING OR ENDING POINT OF THE LINE.
!     CHR   = CHARACTERS TO BE TYPED.
!     N     = NUMBER OF CHARACTERS.
!     OPT   = -1 TO INITIATE  THE TYPING MODE.
!           = +1 TO TERMINATE THE TYPING MODE.
!           =  0 TO TYPE A LINE.
!
   DATA blank , lstchr/48 , 47/
!
   IF ( Opt/=0 ) THEN
!
!     OPT = +/-1
!
      CALL type10(0,0,0,0,0,Opt)
   ELSE
!
!     OPT = 0.
!
      d = max0(iabs(Xyd),1)
      s = cntchr(d)
      IF ( Xyd==-1 .OR. Xyd==2 ) s = -s
      xy(1,1) = X
      xy(2,1) = Y
      xy(1,2) = xy(1,1)
      xy(2,2) = xy(2,1)
!
!     PRINT A MAXIMUM OF 80 CHARACTERS AT A TIME.
!
      DO j = 1 , N , 80
         IF ( Xyd<0 ) THEN
            l2 = N - j + 1
            l1 = l2 - 79
            IF ( l1<=0 ) l1 = 1
         ELSE
            l1 = j
            l2 = l1 + 79
            IF ( l2>N ) l2 = N
         ENDIF
!
         nc = 0
         DO l = l1 , l2
            spag_nextblock_1 = 1
            SPAG_DispatchLoop_1: DO
               SELECT CASE (spag_nextblock_1)
               CASE (1)
                  charx = Chr(l)
                  DO i = 1 , lstchr
                     IF ( charx==char(i) ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
                  i = blank
                  spag_nextblock_1 = 2
               CASE (2)
                  nc = nc + 1
                  c(nc) = i
                  EXIT SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_1
         ENDDO
!
!     TYPE THE -NC- CHARACTERS JUST PROCESSED.
!
         xy(d,2) = xy(d,1) + s*float(l1-1)
         CALL type10(xy(1,2),xy(2,2),Xyd,c,nc,0)
      ENDDO
   ENDIF
END SUBROUTINE tipe
