
SUBROUTINE sdhtf2(Ieqex,Neqex)
   IMPLICIT NONE
   REAL Ce(96) , Dum(58) , Dummy(35) , Esta(202) , Qout(3) , Rk(9) , Tgrad(3) , Tvec(32) , Zz(1)
   INTEGER Ide , Ido , Igrad(3) , Iqout(3) , Isil(32) , Ivec , Iz(1) , Name(2) , Namo(2) , Nq , Nsil
   COMMON /sdr2x4/ Dummy , Ivec
   COMMON /sdr2x7/ Ide , Isil , Nq , Nsil , Name , Rk , Ce , Dum , Ido , Namo , Tgrad , Qout
   COMMON /sdr2x8/ Tvec
   COMMON /zzzzzz/ Zz
   INTEGER Ieqex , Neqex
   INTEGER ftube , i , ihex , ihex1 , ihex2 , ihex3 , iold , ione , ip , ipt(21) , istrpt , isub1 , isub2 , itemp , ithr , itwo ,   &
         & jjj , ns
!*****
!     THIS ROUTINE CALCULATES TEMPERATURE GRADIENTS AND HEAT FLOWS
!     FOR ALL ELEMENTS IN A HEAT TRANSFER PROBLEM.
!      DATA IS OUTPUT FOR ELEMENT FORCE REQUEST ONLY.
!******
   !>>>>EQUIVALENCE (Tgrad(1),Igrad(1)) , (Qout(1),Iqout(1))
   !>>>>EQUIVALENCE (Zz(1),Iz(1)) , (Esta(1),Ide)
   DATA ihex/4HIHEX/ , ione , itwo , ithr/4H1    , 4H2    , 4H3   /
   DATA ihex1 , ihex2 , ihex3/4HHEX1 , 4HHEX2 , 4HHEX3/
   DATA ftube/4HFTUB/
   DATA iold/0/
   DATA ipt/4H   1 , 4H  E1 , 4H   4 , 4H  E2 , 4H   7 , 4H  E3 , 4H  10 , 4H  E4 , 4H  E5 , 4H  E6 , 4H  E7 , 4H  E8 , 4H  21 ,    &
       &4H  E9 , 4H  24 , 4H E10 , 4H  27 , 4H E11 , 4H  30 , 4H E12 , 4H   0/
!
   IF ( Name(1)==ftube ) THEN
!
      Ido = Ide
      itemp = Ivec + Isil(1) - 1
      Tvec(1) = Zz(itemp)
      Esta(202) = Tvec(1)*Esta(4)
      GOTO 99999
   ELSE
      DO i = 1 , 3
         Igrad(i) = 1
         Iqout(i) = 1
      ENDDO
      Ido = Ide
      Namo(1) = Name(1)
      Namo(2) = Name(2)
!
! FOR ISOPARAMETRIC SOLIDS, GET SIL NUMBER AND CONVERT TO EXTERNAL.
! STORE IT IN NAMO(2)
!
      IF ( Namo(1)==ihex ) THEN
         IF ( iold/=Ide ) THEN
            iold = Ide
            istrpt = 0
         ENDIF
         IF ( Namo(2)==ione ) Namo(1) = ihex1
         IF ( Namo(2)==itwo ) Namo(1) = ihex2
         IF ( Namo(2)==ithr ) Namo(1) = ihex3
         istrpt = istrpt + 1
         IF ( istrpt==Nsil+1 .OR. istrpt==21 ) iold = 0
         IF ( Namo(1)==ihex3 ) THEN
            Namo(2) = ipt(istrpt)
         ELSE
            IF ( Namo(1)/=ihex1 .OR. istrpt/=9 ) THEN
               IF ( Namo(1)/=ihex2 .OR. istrpt/=21 ) THEN
                  isub1 = Ieqex + 1
                  isub2 = Ieqex + Neqex - 1
                  DO jjj = isub1 , isub2 , 2
                     ns = Iz(jjj)/10
                     IF ( ns==Isil(istrpt) ) THEN
                        Namo(2) = Iz(jjj-1)
                        GOTO 50
                     ENDIF
                  ENDDO
                  CALL mesage(-30,164,Iz(jjj))
               ENDIF
            ENDIF
            Namo(2) = 0
         ENDIF
      ENDIF
 50   IF ( Nq<=0 ) THEN
         Tgrad(1) = 0.0
         Qout(1) = 0.0
         GOTO 99999
      ENDIF
   ENDIF
   DO i = 1 , Nsil
      Tvec(i) = 0.0
      ip = Isil(i)
      IF ( ip/=0 ) THEN
         itemp = Ivec + ip - 1
         Tvec(i) = Zz(itemp)
      ENDIF
   ENDDO
!***
   CALL gmmats(Ce(1),Nq,Nsil,0,Tvec(1),Nsil,1,0,Tgrad(1))
!
   CALL gmmats(Rk(1),Nq,Nq,0,Tgrad(1),Nq,1,0,Qout(1))
!
   DO i = 1 , Nq
      Qout(i) = -Qout(i)
   ENDDO
   RETURN
!
99999 RETURN
END SUBROUTINE sdhtf2