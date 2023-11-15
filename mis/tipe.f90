
SUBROUTINE tipe(X,Y,Xyd,Chr,N,Opt)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Char(60) , Model , Ploter
   REAL Cntchr(2) , Skpa(3) , Skpplt(18)
   COMMON /char94/ Char
   COMMON /pltdat/ Model , Ploter , Skpplt , Skpa , Cntchr
!
! Dummy argument declarations
!
   INTEGER N , Opt , Xyd
   REAL X , Y
   INTEGER Chr(1)
!
! Local variable declarations
!
   INTEGER blank , c(80) , charx , d , i , j , l , l1 , l2 , lstchr , nc
   REAL s , xy(2,2)
!
! End of declarations
!
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
      s = Cntchr(d)
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
            charx = Chr(l)
            DO i = 1 , lstchr
               IF ( charx==Char(i) ) GOTO 10
            ENDDO
            i = blank
 10         nc = nc + 1
            c(nc) = i
         ENDDO
!
!     TYPE THE -NC- CHARACTERS JUST PROCESSED.
!
         xy(d,2) = xy(d,1) + s*float(l1-1)
         CALL type10(xy(1,2),xy(2,2),Xyd,c,nc,0)
      ENDDO
   ENDIF
END SUBROUTINE tipe
