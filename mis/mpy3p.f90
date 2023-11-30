
SUBROUTINE mpy3p(Z,Iz,Dz)
   IMPLICIT NONE
   REAL Buf1 , Buf2 , Buf3 , Buf4 , Dum1(3) , Dum2(6) , E , Filea(7) , Fileb(7) , Filec(7) , Filee(7) , Scr , Scr1 , Scr2 , Scr3(7)
   INTEGER Code , Iacols , Iakj , Ibcols , Ic , Icore , Ipoint , Itrans , Itrl , Ka , Kb , Laend , Lcore , Lkore , M , N , Ncb ,    &
         & Prec , Zpntrs(22)
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , M , Dum1 , Zpntrs , Laend , Dum2 , Ka , Kb
   COMMON /mpy3tl/ Filea , Fileb , Filee , Filec , Scr1 , Scr2 , Scr , Lkore , Code , Prec , Lcore , Scr3 , Buf1 , Buf2 , Buf3 ,    &
                 & Buf4 , E
   DOUBLE PRECISION Dz(1)
   INTEGER Iz(1)
   REAL Z(1)
   DOUBLE PRECISION dfact
   REAL fact
   INTEGER i , i1 , i2 , iac , iat , ii , iii , kb2 , kj , kj2 , l , l1 , ll , llp , lp
!*****
!    PERFORMS MULTIPLICATION AND SUMMATION.
!*****
!
!
!
!
!
!
!
!
! SUBROUTINE CALL PARAMETERS
! FILES
!
!
!
   EQUIVALENCE (fact,dfact)
! OPEN CORE POINTERS
   EQUIVALENCE (Ipoint,Zpntrs(3)) , (Iacols,Zpntrs(5)) , (Itrans,Zpntrs(7)) , (Ic,Zpntrs(9)) , (Ibcols,Zpntrs(11)) ,                &
    & (Iakj,Zpntrs(21))
!*****
!    LOOP FOR ACCUMULATING SUMS.
!*****
   kj = Iakj + Ka - 1
   kj2 = (Iakj-1)/2 + Ka
   Kb = Ibcols + Prec*((Kb-1)*N-1)
   IF ( Code/=2 .AND. Icore/=1 ) THEN
!*****
!    A(T)BA CASE.
!*****
      lp = Ipoint - 1
      DO l = 1 , N
! CALCULATE FACTOR = B(LK)*A(KJ) TO BE MULTIPLIED TO NON-ZERO TERMS IN
! LTH COLUMN OF A(T)
         Kb = Kb + Prec
         lp = lp + 1
         IF ( Iz(lp)==0 ) CYCLE
         IF ( Prec==2 ) THEN
            kb2 = (Kb+1)/2
            IF ( Dz(kb2)==0.0D0 ) CYCLE
            dfact = Dz(kb2)*Dz(kj2)
         ELSE
            IF ( Z(Kb)==0.0 ) CYCLE
            fact = Z(Kb)*Z(kj)
         ENDIF
         i1 = Iz(lp)
         IF ( l/=N ) THEN
! ACCUMULATE SUMS FOR NON-ZERO TERMS IN COLUMN L OF A(T)
            l1 = l + 1
            llp = lp
            DO ll = l1 , N
               llp = llp + 1
               IF ( Iz(llp)/=0 ) GOTO 20
            ENDDO
         ENDIF
         i2 = Laend
         GOTO 40
 20      i2 = Iz(llp) - 1
 40      iac = Iacols + i1 - 2
         IF ( Prec==2 ) THEN
! DOUBLE PRECISION CASE
            iat = (Itrans-3)/2 + i1
            DO i = i1 , i2
               iac = iac + 1
               iat = iat + 1
               ii = (Ic-1)/2 + Iz(iac)
               Dz(ii) = Dz(ii) + Dz(iat)*dfact
            ENDDO
            iii = (Ic-1)/2 + 1
         ELSE
! SINGLE PRECISION CASE
            iat = Itrans + i1 - 2
            DO i = i1 , i2
               iac = iac + 1
               iat = iat + 1
               ii = Ic + Iz(iac) - 1
               Z(ii) = Z(ii) + Z(iat)*fact
            ENDDO
         ENDIF
      ENDDO
!*****
!    BA CASE.
!*****
   ELSEIF ( Prec==2 ) THEN
! DOUBLE PRECISION CASE
      ii = (Ic-1)/2
      Kb = (Kb+1)/2
      DO i = 1 , N
         ii = ii + 1
         Kb = Kb + 1
         IF ( Dz(Kb)/=0.0D0 ) Dz(ii) = Dz(ii) + Dz(Kb)*Dz(kj2)
      ENDDO
   ELSE
! SINGLE PRECISION CASE
      ii = Ic - 1
      DO i = 1 , N
         ii = ii + 1
         Kb = Kb + 1
         IF ( Z(Kb)/=0.0 ) Z(ii) = Z(ii) + Z(Kb)*Z(kj)
      ENDDO
   ENDIF
!
END SUBROUTINE mpy3p
