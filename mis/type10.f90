
SUBROUTINE type10(X,Y,Xyd,Chr,Nn,Opt)
   IMPLICIT NONE
   REAL Cntchr(6) , Cscale , Skpa(3) , Skpplt(2) , Xymax(15) , Xymin(2)
   INTEGER Pltype
   COMMON /pltdat/ Skpplt , Xymin , Xymax , Cscale , Skpa , Cntchr , Pltype
   INTEGER Nn , Opt , Xyd
   REAL X , Y
   INTEGER Chr(1)
   INTEGER a(6) , d , i , j , k , lstchr , n , optx , type
   REAL s , xy(2,2)
!
!     (X,Y) = STARTING OR ENDING POINT OF THE LINE TO BE TYPED (ALWAYS
!             LEFT-TO-RIGHT OR TOP-TO-BOTTOM)
!     XYD   = (+/-)1 IF X = STARTING OR ENDING POINT OF THE LINE
!           = (+/-)2 IF Y = STARTING OR ENDING POINT OF THE LINE
!     CHR   = CHARACTERS TO BE TYPED
!     NN    = NUMBER OF CHARACTERS
!     OPT   = -1 TO INITIATE  THE TYPING MODE
!           = +1 TO TERMINATE THE TYPING MODE
!           =  0 TO TYPE A LINE
!
   DATA a(6) , type , lstchr/0 , 4 , 48/
!
   IF ( Pltype<0 ) THEN
!
!     DRAW THE LINE OF CHARACTERS
!
      CALL drwchr(X,Y,Xyd,Chr,Nn,Opt)
   ELSE
      optx = -1
      IF ( Opt<0 ) THEN
      ELSEIF ( Opt==0 ) THEN
         a(5) = ifix(Cscale+.44)
         xy(1,1) = X
         xy(2,1) = Y
         xy(1,2) = X
         xy(2,2) = Y
         n = 1
         IF ( n<=0 ) n = 1
!
!     SCREEN OUT TRAILING BLANKS
!
         DO j = 1 , Nn
            IF ( iabs(Chr(j))/=48 ) n = j
         ENDDO
         IF ( n==1 .AND. iabs(Chr(1))==48 ) RETURN
         d = max0(iabs(Xyd),1)
         s = Cntchr(d)
         IF ( Xyd==-1 .OR. Xyd==2 ) s = -s
!
!     TYPE THE LINE
!
         DO j = 1 , n
            xy(d,2) = xy(d,1) + s*float(j-1)
            DO i = 1 , 2
               IF ( xy(i,2)+.1<Xymin(i) .OR. xy(i,2)-.1>Xymax(i) ) GOTO 20
               a(i+2) = xy(i,2) + .1
            ENDDO
!
!     MAKE SURE EACH CHARACTER IS A VALID CHARACTER (UNLESS NN.LE.0)
!
            k = j
            IF ( Xyd<0 ) k = n - j + 1
            a(2) = iabs(Chr(k))
            IF ( Nn>0 ) THEN
               IF ( a(2)==0 .OR. a(2)>lstchr ) CYCLE
               IF ( a(2)==0 ) CYCLE
            ENDIF
!
!     TYPE THE CHARACTER
!
            a(1) = type
            IF ( optx/=0 ) THEN
               a(1) = type + 10
               optx = 0
            ENDIF
            CALL wplt10(a,0)
 20      ENDDO
      ELSE
!
!     TERMINATE THE TYPING MODE
!
         CALL wplt10(a,1)
         optx = -1
      ENDIF
   ENDIF
!
END SUBROUTINE type10
