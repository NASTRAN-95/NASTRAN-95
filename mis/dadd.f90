
SUBROUTINE dadd
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alp(4) , Alpha(2) , Beta(2) , Bta(4) , Cde(3,12) , Core(1)
   DOUBLE PRECISION Dalp(2) , Dalpha(2) , Dbeta(2) , Dbta(2)
   INTEGER Echo , Ia(7) , Ib(7) , Ibuf , Ic(7) , Ita , Itb , Lcore , Nomat , Nout
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Alpha , Beta , Dalpha , Dbeta , Echo
   COMMON /saddx / Nomat , Lcore , Ia , Ita , Alp , Ib , Itb , Bta , Cde , Ic
   COMMON /system/ Ibuf , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Core
!
! Local variable declarations
!
   INTEGER aa(2) , bb(2) , fn(2) , i , in1 , in2 , iout1
   LOGICAL dblea , dbleb
   INTEGER korsz
   DOUBLE PRECISION one , xx , zero
   REAL x
!
! End of declarations
!
!
!     DMAP DRIVER FOR ADD--
!
!     ADD    A,B/C/V,N,ALPHA/V,N,BETA/V,N,DALPHA/V,N,DBETA/V,N,ECHO  $
!
!            MATRIX C = ALPHA*MATRIX A + BETA*MATRIX B
!
!     MATRIX C IS COMPLEX IF ANY ONE OF THE MATRIX A, MATRIX B, SCALE
!     ALPHA, OR SCLAE BETA IS COMPLEX
!
   EQUIVALENCE (Alp(1),Dalp(1)) , (Bta(1),Dbta(1))
   DATA in1 , in2 , iout1 , zero/101 , 102 , 201 , 0.0D+0/
   DATA one , xx , x/1.0D+0 , 1.0D+37 , 1.0E+37/
!
!
!     SCALE FACTORS ALPHA, DALPHA, BETA AND DBETA WERE INITIALLY SET TO
!     (1.1+37, 1.1+37) BY XMPLDD
!
!     IN THIS ROUTINE -
!     IF ALPHA, DALPHA, BETA AND DBETA ARE NOT SPECIFIED BY USER, THEY
!     WILL BE SET TO -
!     ALPHA AND DALPHA TO (1.0, 0.0), AND
!     BETA  AND DBETA  TO (1.0, 0.0), SAME DEFAULTS AS 88 AND EARLIER
!                                     NASTRAN VERSIONS.
!     NOTE - DEFAULTS WERE ALL ZEROS IN 89 NASTRAN VERSION
!
!     NOTE - THIS ROUTINE WILL CALL SADD TO DO THE ACTUAL MATRIX MULTI-
!     PLICATION, WHICH WILL AUTOMATICALLY ADJUST THE SCALE FACTORS
!     WHETHER THEY ARE S.P. OR D.P. (E.G. S.P. ALPHA AND BETA CAN BE
!     USED FOR D.P. A AND B MATRICES, AND VISE VERSA)
!
   CALL fname(iout1,fn(1))
   Lcore = korsz(Core)
   DO i = 1 , 7
      Ia(i) = 0
      Ib(i) = 0
      Ic(i) = 0
   ENDDO
   Ia(1) = in1
   Ib(1) = in2
   CALL rdtrl(Ia)
   CALL rdtrl(Ib)
   IF ( Ia(1)<0 ) Ia(1) = 0
   IF ( Ib(1)<0 ) Ib(1) = 0
   IF ( Ia(1)+Ib(1)==0 ) THEN
!
      WRITE (Nout,99001) Ufm , fn
99001 FORMAT (A23,', INPUT MATRICES NOT SPECIFIED IN ADD MODULE.',' INTENDED OUTPUT DATA BLOCK NAME =',2A4)
      GOTO 300
   ELSE
!
!     SET DEFAULT VALUES FOR THE SCALE FACTORS
!
!     WHEN AN ITEM IS .LT. X OR XX, THAT ITEM HAS INPUT FROM USER
!
      dblea = .TRUE.
      dbleb = .TRUE.
      IF ( Alpha(1)>=x .AND. Alpha(2)>=x .AND. Dalpha(1)>=xx .AND. Dalpha(2)>=xx ) THEN
         Alp(1) = 1.0
         Alp(2) = 0.0
         Alpha(1) = 1.0
         Alpha(2) = 0.0
         dblea = .FALSE.
      ENDIF
      IF ( Beta(1)>=x .AND. Beta(2)>=x .AND. Dbeta(1)>=xx .AND. Dbeta(2)>=xx ) THEN
         Bta(1) = 1.0
         Bta(2) = 0.0
         Beta(1) = 1.0
         Beta(2) = 0.0
         dbleb = .FALSE.
         IF ( .NOT.dblea ) GOTO 100
      ENDIF
!
      IF ( (Alpha(1)<x .OR. Alpha(2)<x) .AND. (Dalpha(1)<xx .OR. Dalpha(2)<xx) ) GOTO 200
      IF ( (Beta(1)<x .OR. Beta(2)<x) .AND. (Dbeta(1)<xx .OR. Dbeta(2)<xx) ) GOTO 200
!
      IF ( Dalpha(1)>xx .AND. Dalpha(2)>xx ) dblea = .FALSE.
      IF ( Dbeta(1)>xx .AND. Dbeta(2)>xx ) dbleb = .FALSE.
!
      DO i = 1 , 2
         IF ( Alpha(i)>x ) Alpha(i) = 0.0
         IF ( Dalpha(i)>xx ) Dalpha(i) = zero
         IF ( Beta(i)>x ) Beta(i) = 0.0
         IF ( Dbeta(i)>xx ) Dbeta(i) = zero
      ENDDO
!
!     MOVE ALPHA, BETA, DALPHA AND DBETA INTO ALP AND BTA ARRAYS FOR
!     MATRIX MULTIPLICATION TO BE PERFORMED IN SADD.
!
      DO i = 1 , 2
         IF ( .NOT.dblea ) Alp(i) = Alpha(i)
         IF ( .NOT.dbleb ) Bta(i) = Beta(i)
         IF ( dblea ) Dalp(i) = Dalpha(i)
         IF ( dbleb ) Dbta(i) = Dbeta(i)
      ENDDO
   ENDIF
!
 100  IF ( Echo/=0 ) THEN
      WRITE (Nout,99002) Uim , fn
99002 FORMAT (A29,', SCALE FACTORS FOR THE OUTOUT DATA BLOCK ',2A4,', IN ADD MODULE ARE -')
      IF ( .NOT.dblea ) WRITE (Nout,99003) Alp(1) , Alp(2)
99003 FORMAT (5X,'1ST S.F. = (',E12.5,1H,,E12.5,1H))
      IF ( dblea ) WRITE (Nout,99004) Dalp(1) , Dalp(2)
99004 FORMAT (5X,'3RD S.F. = (',D12.5,1H,,D12.5,1H))
      IF ( .NOT.dbleb ) WRITE (Nout,99005) Bta(1) , Bta(2)
99005 FORMAT (1H+,48X,'2ND S.F. = (',E12.5,1H,,E12.5,1H))
      IF ( dbleb ) WRITE (Nout,99006) Dbta(1) , Dbta(2)
99006 FORMAT (1H+,48X,'4TH S.F. = (',D12.5,1H,,D12.5,1H))
   ENDIF
!
!     ENSURE THAT THE MATRICES BEING ADDED ARE OF THE SAME ORDER
!
   IF ( Ia(1)/=0 .AND. Ib(1)/=0 ) THEN
      IF ( Ia(2)/=Ib(2) .OR. Ia(3)/=Ib(3) ) THEN
         CALL fname(Ia(1),aa)
         CALL fname(Ib(1),bb)
         WRITE (Nout,99007) Ufm , aa , bb , fn , Ia(2) , Ia(3) , Ib(2) , Ib(3)
99007    FORMAT (A23,' 4149, ATTEMPT TO ADD MATRICES OF UNEQUAL ORDER IN',' MODULE ADD, ',2A4,' TO ',2A4,/5X,'INTENDED OUTOUT DATA',&
                &' BLOCK NAME =',2A4,I7,' BY',I6,' TO',I7,' BY',I6)
         GOTO 300
      ENDIF
   ENDIF
   Ic(1) = iout1
   Ic(2) = Ia(2)
   Ic(3) = Ia(3)
   IF ( Ia(4)==3 ) Ic(2) = Ia(3)
   IF ( Ia(1)==0 ) THEN
      Ic(2) = Ib(2)
      Ic(3) = Ib(3)
   ENDIF
!
!     DETERMINE TYPE
!
   Ita = 3
   Itb = 3
   IF ( Alp(2)==0.0 .AND. Alp(4)==0.0 ) Ita = 1
   IF ( Bta(2)==0.0 .AND. Bta(4)==0.0 ) Itb = 1
   Ic(5) = max0(Ia(5),Ib(5),Ita,Itb)
   IF ( Ic(5)==3 .AND. (Ia(5)==2 .OR. Ib(5)==2) ) Ic(5) = 4
!
!     DETERMINE FORM
!
   Ic(4) = Ia(4)
   IF ( Ia(1)==0 ) Ic(4) = Ib(4)
   IF ( Ic(4)==1 .AND. Ic(4)==6 ) THEN
      Ic(4) = 6
      IF ( Ia(1)/=0 .AND. Ia(4)/=6 ) Ic(4) = 1
      IF ( Ib(1)/=0 .AND. Ib(4)/=6 ) Ic(4) = 1
      IF ( Ic(2)/=Ic(3) ) Ic(4) = 2
   ENDIF
   IF ( Ia(4)==3 .AND. Ib(1)/=0 ) Ic(4) = Ib(4)
   IF ( Ia(4)==3 .AND. Ib(1)==0 ) Ic(4) = Ia(4)
!
   Nomat = 2
   CALL sadd(Core,Core)
   CALL wrttrl(Ic)
   GOTO 99999
!
 200  DO i = 1 , 2
      IF ( Alpha(i)>x ) Alpha(i) = 0.0
      IF ( Dalpha(i)>xx ) Dalpha(i) = zero
      IF ( Beta(i)>x ) Beta(i) = 0.0
      IF ( Dbeta(i)>xx ) Dbeta(i) = zero
   ENDDO
   WRITE (Nout,99008) Ufm , fn , Alpha , Beta , Dalpha , Dbeta
99008 FORMAT (A23,' IN ADD MODULE. INTENDED OUTPUT DATA BLOCK =',2A4,/5X,'SCALE FACTORS ARE ERRONEOUS =',4E9.2,2X,4D10.3)
 300  CALL mesage(-61,0,0)
!
99999 RETURN
END SUBROUTINE dadd
