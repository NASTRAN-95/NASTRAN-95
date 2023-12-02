!*==print.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE print(X,Y,Xyd,Chr,N,Opt)
   USE c_pltdat
   USE c_system
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
   INTEGER , SAVE :: blank
   INTEGER :: blnk , charx , d , i , j , l , l1 , l2 , nc
   INTEGER , DIMENSION(80) :: c
   REAL :: s
   REAL , DIMENSION(2,2) :: xy
   EXTERNAL klshft , krshft , orf , tipe
!
! End of declarations rewritten by SPAG
!
!
!     (X,Y) = STARTING OR ENDING POINT OF THE LINE TO BE PRINTED (ALWAYS
!             LEFT-TO-RIGHT OR TOP-TO-BOTTOM).
!     CHR   = CHARACTERS TO BE PRINTED (4 PER WORD).
!     N     = NUMBER OF 4 CHARACTER WORDS.
!     XYD   = +/-1 IF X = STARTING OR ENDING POINT OF THE LINE.
!     ...   = +/-2 .. Y = ........ .. ...... ..... .. ... .....
!     OPT   = -1 TO INITIATE  THE TYPING MODE.
!     ...   = +1 .. TERMINATE ... ...... .....
!     ...   =  0 .. PRINT A LINE.
!
   DATA blank/1H /
!
   IF ( Opt/=0 ) THEN
!
!     OPT = +/-1
!
      CALL tipe(0,0,0,0,0,Opt)
   ELSE
      blnk = krshft(klshft(blank,1),1)
      d = max0(iabs(Xyd),1)
      s = cntchr(d)
      IF ( Xyd==-1 .OR. Xyd==2 ) s = -s
      xy(1,1) = X
      xy(2,1) = Y
      xy(1,2) = xy(1,1)
      xy(2,2) = xy(2,1)
!
!     SEPARATE 80 CHARACTERS AT A TIME.
!
      DO j = 1 , N , 20
         IF ( Xyd<0 ) THEN
            l2 = N - j + 1
            l1 = l2 - 19
            IF ( l1<=0 ) l1 = 1
         ELSE
            l1 = j
            l2 = l1 + 19
            IF ( l2>N ) l2 = N
         ENDIF
!
         nc = 0
         DO l = l1 , l2
            DO i = 1 , 4
               charx = krshft(Chr(l),ncpw-i)
               nc = nc + 1
               c(nc) = orf(klshft(charx,ncpw-1),blnk)
            ENDDO
         ENDDO
!
!     TYPE THE -NC- CHARACTERS JUST SEPARATED.
!
         xy(d,2) = xy(d,1) + s*float(l1-1)
         CALL tipe(xy(1,2),xy(2,2),Xyd,c,nc,0)
      ENDDO
   ENDIF
END SUBROUTINE print
