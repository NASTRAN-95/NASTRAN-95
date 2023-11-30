
SUBROUTINE prompt
!
!     DRIVER FOR  INTERACTIVE MODULE - PROMPT
!
!        PROMPT    //S,N,PEXIT/S,N,PLOT1/S,N,PLOT2/S,N,XYPLOT/
!                    S,N,SCAN1/S,N,SCAN2/DUM1/DUM2/DUM3/DUM4 $
!
   IMPLICIT NONE
   INTEGER In , Intra , Ksysm(100) , Nout , Param(10) , Soln
   COMMON /blank / Param
   COMMON /system/ Ksysm
   INTEGER b , c , i , j , p , s
   EQUIVALENCE (Nout,Ksysm(2)) , (Soln,Ksysm(22)) , (In,Ksysm(4)) , (Intra,Ksysm(86))
   DATA p , s , c , b/1HP , 1HS , 1HC , 1H /
!
   Intra = iabs(Intra)
   Nout = 6
   DO i = 1 , 10
      Param(i) = 0
   ENDDO
 100  DO
!
      WRITE (Nout,99001)
!
99001 FORMAT (9X,'1. EXIT',/9X,'2. STRUCTURE PLOTS - UNDEFORMED',/9X,'3. STRUCTURE PLOTS - DEFORMED',/9X,'4. XYPLOTS',/9X,          &
             &'5. SCAN OUTPUT - SORT1',/9X,'6. SCAN OUTPUT - SORT2',//9X,'SELECT ONE OPTION FROM MENU -')
      READ (In,99002,ERR=100) j
99002 FORMAT (I1)
      IF ( j>=1 .AND. j<=6 ) THEN
         IF ( Soln==3 .AND. (j==4 .OR. j==6) ) THEN
            WRITE (Nout,99003) j , Soln
99003       FORMAT (/,' OPTION',I3,' IS NOT AVAILABLE FOR SOLUTION',I3)
         ELSE
            Param(j) = -1
            IF ( j==1 ) RETURN
            EXIT
         ENDIF
      ENDIF
   ENDDO
 200  WRITE (Nout,99004)
99004 FORMAT (/9X,'OUTPUT TO SCREEN, OR TO PRINTFILE, OR CANCEL OPTION (S/P/C) -')
   i = b
   READ (In,99005,END=300) i
99005 FORMAT (A1)
 300  IF ( i==b ) RETURN
   IF ( i==c ) THEN
!
      Param(j) = 0
      GOTO 100
   ELSEIF ( i/=p .AND. i/=s ) THEN
      GOTO 200
   ENDIF
   IF ( i==s ) Intra = mod(Intra,10)
   IF ( i==p ) Intra = mod(Intra,10) + 10
   RETURN
END SUBROUTINE prompt
