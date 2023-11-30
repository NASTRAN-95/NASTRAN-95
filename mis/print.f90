
SUBROUTINE print(X,Y,Xyd,Chr,N,Opt)
   IMPLICIT NONE
   REAL Cntchr(2) , Skpa(3) , Skpplt(20) , Skpsys(40)
   INTEGER Ncpw
   COMMON /pltdat/ Skpplt , Skpa , Cntchr
   COMMON /system/ Skpsys , Ncpw
   INTEGER N , Opt , Xyd
   REAL X , Y
   INTEGER Chr(1)
   INTEGER blank , blnk , c(80) , charx , d , i , j , l , l1 , l2 , nc
   INTEGER klshft , krshft , orf
   REAL s , xy(2,2)
   EXTERNAL klshft , krshft , orf
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
      s = Cntchr(d)
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
               charx = krshft(Chr(l),Ncpw-i)
               nc = nc + 1
               c(nc) = orf(klshft(charx,Ncpw-1),blnk)
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