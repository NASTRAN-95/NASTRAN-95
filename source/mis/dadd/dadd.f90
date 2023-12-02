!*==dadd.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dadd
   USE c_blank
   USE c_saddx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: aa , bb , fn
   REAL(REAL64) , DIMENSION(2) :: dalp , dbta
   LOGICAL :: dblea , dbleb
   INTEGER :: i
   INTEGER , SAVE :: in1 , in2 , iout1
   REAL(REAL64) , SAVE :: one , xx , zero
   REAL , SAVE :: x
   EXTERNAL fname , korsz , mesage , rdtrl , sadd , wrttrl
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Alp(1),Dalp(1)) , (Bta(1),Dbta(1))
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
   lcore = korsz(core)
   DO i = 1 , 7
      ia(i) = 0
      ib(i) = 0
      ic(i) = 0
   ENDDO
   ia(1) = in1
   ib(1) = in2
   CALL rdtrl(ia)
   CALL rdtrl(ib)
   IF ( ia(1)<0 ) ia(1) = 0
   IF ( ib(1)<0 ) ib(1) = 0
   IF ( ia(1)+ib(1)==0 ) THEN
!
      WRITE (nout,99001) ufm , fn
99001 FORMAT (A23,', INPUT MATRICES NOT SPECIFIED IN ADD MODULE.',' INTENDED OUTPUT DATA BLOCK NAME =',2A4)
      CALL spag_block_3
      RETURN
   ELSE
!
!     SET DEFAULT VALUES FOR THE SCALE FACTORS
!
!     WHEN AN ITEM IS .LT. X OR XX, THAT ITEM HAS INPUT FROM USER
!
      dblea = .TRUE.
      dbleb = .TRUE.
      IF ( alpha(1)>=x .AND. alpha(2)>=x .AND. dalpha(1)>=xx .AND. dalpha(2)>=xx ) THEN
         alp(1) = 1.0
         alp(2) = 0.0
         alpha(1) = 1.0
         alpha(2) = 0.0
         dblea = .FALSE.
      ENDIF
      IF ( beta(1)>=x .AND. beta(2)>=x .AND. dbeta(1)>=xx .AND. dbeta(2)>=xx ) THEN
         bta(1) = 1.0
         bta(2) = 0.0
         beta(1) = 1.0
         beta(2) = 0.0
         dbleb = .FALSE.
         IF ( .NOT.dblea ) THEN
            CALL spag_block_1
            RETURN
         ENDIF
      ENDIF
!
      IF ( (alpha(1)<x .OR. alpha(2)<x) .AND. (dalpha(1)<xx .OR. dalpha(2)<xx) ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      IF ( (beta(1)<x .OR. beta(2)<x) .AND. (dbeta(1)<xx .OR. dbeta(2)<xx) ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
!
      IF ( dalpha(1)>xx .AND. dalpha(2)>xx ) dblea = .FALSE.
      IF ( dbeta(1)>xx .AND. dbeta(2)>xx ) dbleb = .FALSE.
!
      DO i = 1 , 2
         IF ( alpha(i)>x ) alpha(i) = 0.0
         IF ( dalpha(i)>xx ) dalpha(i) = zero
         IF ( beta(i)>x ) beta(i) = 0.0
         IF ( dbeta(i)>xx ) dbeta(i) = zero
      ENDDO
!
!     MOVE ALPHA, BETA, DALPHA AND DBETA INTO ALP AND BTA ARRAYS FOR
!     MATRIX MULTIPLICATION TO BE PERFORMED IN SADD.
!
      DO i = 1 , 2
         IF ( .NOT.dblea ) alp(i) = alpha(i)
         IF ( .NOT.dbleb ) bta(i) = beta(i)
         IF ( dblea ) dalp(i) = dalpha(i)
         IF ( dbleb ) dbta(i) = dbeta(i)
      ENDDO
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
      IF ( echo/=0 ) THEN
         WRITE (Nout,99001) uim , Fn
99001    FORMAT (A29,', SCALE FACTORS FOR THE OUTOUT DATA BLOCK ',2A4,', IN ADD MODULE ARE -')
         IF ( .NOT.Dblea ) WRITE (Nout,99002) alp(1) , alp(2)
99002    FORMAT (5X,'1ST S.F. = (',E12.5,1H,,E12.5,1H))
         IF ( Dblea ) WRITE (Nout,99003) Dalp(1) , Dalp(2)
99003    FORMAT (5X,'3RD S.F. = (',D12.5,1H,,D12.5,1H))
         IF ( .NOT.Dbleb ) WRITE (Nout,99004) bta(1) , bta(2)
99004    FORMAT (1H+,48X,'2ND S.F. = (',E12.5,1H,,E12.5,1H))
         IF ( Dbleb ) WRITE (Nout,99005) Dbta(1) , Dbta(2)
99005    FORMAT (1H+,48X,'4TH S.F. = (',D12.5,1H,,D12.5,1H))
      ENDIF
!
!     ENSURE THAT THE MATRICES BEING ADDED ARE OF THE SAME ORDER
!
      IF ( ia(1)/=0 .AND. ib(1)/=0 ) THEN
         IF ( ia(2)/=ib(2) .OR. ia(3)/=ib(3) ) THEN
            CALL fname(ia(1),Aa)
            CALL fname(ib(1),Bb)
            WRITE (Nout,99006) Ufm , Aa , Bb , Fn , ia(2) , ia(3) , ib(2) , ib(3)
99006       FORMAT (A23,' 4149, ATTEMPT TO ADD MATRICES OF UNEQUAL ORDER IN',' MODULE ADD, ',2A4,' TO ',2A4,/5X,                    &
                   &'INTENDED OUTOUT DATA',' BLOCK NAME =',2A4,I7,' BY',I6,' TO',I7,' BY',I6)
            CALL spag_block_3
            RETURN
         ENDIF
      ENDIF
      ic(1) = Iout1
      ic(2) = ia(2)
      ic(3) = ia(3)
      IF ( ia(4)==3 ) ic(2) = ia(3)
      IF ( ia(1)==0 ) THEN
         ic(2) = ib(2)
         ic(3) = ib(3)
      ENDIF
!
!     DETERMINE TYPE
!
      ita = 3
      itb = 3
      IF ( alp(2)==0.0 .AND. alp(4)==0.0 ) ita = 1
      IF ( bta(2)==0.0 .AND. bta(4)==0.0 ) itb = 1
      ic(5) = max0(ia(5),ib(5),ita,itb)
      IF ( ic(5)==3 .AND. (ia(5)==2 .OR. ib(5)==2) ) ic(5) = 4
!
!     DETERMINE FORM
!
      ic(4) = ia(4)
      IF ( ia(1)==0 ) ic(4) = ib(4)
      IF ( ic(4)==1 .AND. ic(4)==6 ) THEN
         ic(4) = 6
         IF ( ia(1)/=0 .AND. ia(4)/=6 ) ic(4) = 1
         IF ( ib(1)/=0 .AND. ib(4)/=6 ) ic(4) = 1
         IF ( ic(2)/=ic(3) ) ic(4) = 2
      ENDIF
      IF ( ia(4)==3 .AND. ib(1)/=0 ) ic(4) = ib(4)
      IF ( ia(4)==3 .AND. ib(1)==0 ) ic(4) = ia(4)
!
      nomat = 2
      CALL sadd(Core,Core)
      CALL wrttrl(ic)
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
      DO I = 1 , 2
         IF ( alpha(I)>X ) alpha(I) = 0.0
         IF ( dalpha(I)>Xx ) dalpha(I) = Zero
         IF ( beta(I)>X ) beta(I) = 0.0
         IF ( dbeta(I)>Xx ) dbeta(I) = Zero
      ENDDO
      WRITE (Nout,99001) Ufm , Fn , alpha , beta , dalpha , dbeta
99001 FORMAT (A23,' IN ADD MODULE. INTENDED OUTPUT DATA BLOCK =',2A4,/5X,'SCALE FACTORS ARE ERRONEOUS =',4E9.2,2X,4D10.3)
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      CALL mesage(-61,0,0)
   END SUBROUTINE spag_block_3
!
END SUBROUTINE dadd
