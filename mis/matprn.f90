
SUBROUTINE matprn
   IMPLICIT NONE
   INTEGER Ibuf , Nout , P1 , P2 , P3 , P4 , P5 , P6
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / P1 , P2 , P3 , P4 , P5 , P6
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm
   INTEGER i , iout , iprec , ityp , mcb(7) , ndpl , npl
!
!     MATRIX PRINT MODULE
!     WILL PRINT UP TO 5 DBi INPUT MATRICES
!     INPUT MATRICES CAN BE IN S.P, D.P, S.P.COMPLEX, OR D.P.COMPLEX
!
!     MATPRN  DB1,DB2,DB3,DB4,DB5//C,N,P1/C,N,P2/C,N,P3/C,N,P4/C,N,P5/
!                                  C,N,P6
!
!     WHERE   P1 AND P2 ARE PRINT FORMAT CONTROLS
!             P1 = 0, MATRICES PRINTED IN THEIR ORIG. PREC. (DEFAULT),
!                = 1, MATRICES PRINTED IN S.P. PREC. (e.g.  x.xxxE+xx)
!                = 2, MATRICES PRINTED IN D.P. PREC. (e.g. -x.xxxD+xx)
!                =-1, ONLY THE DIAGONAL ELEMENTS OF THE MATRIX WILL BE
!                     PRINTED IN THEIR ORIG. PRECISON
!             P2 = NO. OF DATA VALUES PRINTED PER LINE (132 DIGITS/LINE)
!                = 8 TO 14 IF MATRICES ARE PRINTED IN S.P. (DEFAULT=10)
!                = 6 TO 12 IF MATRICES ARE PRINTED IN D.P. (DEFAULT= 9)
!
!             P3, P4, P5 ARE PRINTOUT CONTROLS
!             P3 = m, MATRIX COLUMNS, 1 THRU m, WILL BE PRINTED.
!                  DEFAULT = 0, ALL MATRIX COLUMNS WILL BE PRINTED.
!                =-m, SEE P4 = -n
!             P4 = n, LAST n MATRIX COLUMNS ARE PRINTED. DEFAULT = 0
!                =-n, AND P3 = -m, EVERY OTHER n MATRIX COLUMNS WILL BE
!                  PRINTED, STARTIN FROM COLUMN m.
!             P5 = k, EACH PRINTED COLUMN WILL NOT EXCEED k LINES LONG
!                  AND THE REMAINING DATA WILL BE OMITTED.
!             P6 = LU, WHERE LU LOGICAL FILE NUMBER = 11(UT1), 12(UT2),
!                  14(INPT), 15(INT1),...,23(INT9), 24(IBM'S INPT).
!                  DEFAULT IS ZERO, SYSTEM PRINTER.
!                  IF LU IS 11 THRU 24, THE MATRIX PRINTOUT IS SAVED IN
!                  FORTRAN UNIT LU.
!
!
!     LAST REVISED BY G.CHAN/UNISYS
!     12/91, NEW MODULE PARAMETERS TO ALLOW USER SOME CONTROL OVER
!            POSSIBLY MASSIVE MATRIX PRINTOUT
!     8/92,  TO PRINT ONLY THE DIAGONAL ELEMENTS FOR POSSIBLY MATRIX
!            SINGULARITY CHECK, AND PARAMETER P6
!
!
   IF ( P1>2 .OR. P2>14 ) THEN
      WRITE (Nout,99001) Uwm , P1 , P2 , P3 , P4 , P5 , P6
99001 FORMAT (A25,', MATPRN PARAMETERS APPEAR IN ERROR.  P1,P2,P3,P4,','P5,P6 =',6I5,/5X,'P1 IS RESET TO ZERO, AND P2 TO 6 TO',     &
             &' 14 DEPENDING ON TYPE OF DATA')
!
!     CHECK THAT USER REALY WANTS TO SET P3,P4,P5, AND INSTEAD HE SETS
!     THEM TO P1,P2,P3
!
      IF ( P4==0 .AND. P5==0 .AND. P3<=50 ) THEN
         P3 = P1
         P4 = P2
         P5 = P3
         WRITE (Nout,99002) P3 , P4 , P5
99002    FORMAT (5X,'P3,P4,P5 ARE SET TO ',3I5)
      ENDIF
   ENDIF
   DO i = 1 , 5
      mcb(1) = 100 + i
      CALL rdtrl(mcb(1))
      IF ( mcb(1)<0 ) CYCLE
      IF ( P1==-1 ) GOTO 200
      ityp = mcb(5)
      ndpl = P2
      IF ( ndpl==0 ) THEN
         ndpl = 9
         IF ( mod(ityp,2)==1 ) ndpl = 10
      ENDIF
      npl = ndpl
      IF ( ityp==2 ) GOTO 100
      IF ( ityp==3 ) THEN
         ndpl = (ndpl/2)*2
         npl = ndpl
         IF ( P1>0 .AND. P1<=2 ) THEN
            IF ( P1==1 ) THEN
            ELSEIF ( P1==2 ) THEN
               GOTO 100
            ELSE
               GOTO 150
            ENDIF
         ENDIF
      ELSEIF ( ityp==4 ) THEN
         GOTO 150
      ENDIF
 50   IF ( ndpl<8 ) npl = 8
      IF ( ndpl>14 ) npl = 14
      GOTO 200
 100  IF ( ndpl<6 ) npl = 6
      IF ( ndpl>12 ) npl = 12
      GOTO 200
 150  ndpl = (ndpl/2)*2
      npl = ndpl
      IF ( P1<=0 .OR. P1>2 ) GOTO 100
      IF ( P1==1 ) GOTO 50
      IF ( P1==2 ) GOTO 100
 200  iprec = P1
      IF ( iprec/=1 .AND. iprec/=2 .AND. P1/=-1 ) THEN
         iprec = 2
         IF ( mod(ityp,2)==1 ) iprec = 1
      ENDIF
      iout = Nout
      IF ( P6>=11 .AND. P6<=24 ) iout = P6
      CALL matdum(mcb(1),iprec,npl,iout)
   ENDDO
END SUBROUTINE matprn