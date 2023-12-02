!*==sdhtf2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdhtf2(Ieqex,Neqex)
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ieqex
   INTEGER :: Neqex
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(202) :: esta
   INTEGER , SAVE :: ftube , ihex , ihex1 , ihex2 , ihex3 , iold , ione , ithr , itwo
   INTEGER :: i , ip , istrpt , isub1 , isub2 , itemp , jjj , ns
   INTEGER , DIMENSION(3) :: igrad , iqout
   INTEGER , DIMENSION(21) , SAVE :: ipt
   INTEGER , DIMENSION(1) :: iz
   EXTERNAL gmmats , mesage
!
! End of declarations rewritten by SPAG
!
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
   IF ( name(1)==ftube ) THEN
!
      ido = ide
      itemp = ivec + isil(1) - 1
      tvec(1) = zz(itemp)
      esta(202) = tvec(1)*esta(4)
      RETURN
   ELSE
      DO i = 1 , 3
         igrad(i) = 1
         iqout(i) = 1
      ENDDO
      ido = ide
      namo(1) = name(1)
      namo(2) = name(2)
!
! FOR ISOPARAMETRIC SOLIDS, GET SIL NUMBER AND CONVERT TO EXTERNAL.
! STORE IT IN NAMO(2)
!
      IF ( namo(1)==ihex ) THEN
         IF ( iold/=ide ) THEN
            iold = ide
            istrpt = 0
         ENDIF
         IF ( namo(2)==ione ) namo(1) = ihex1
         IF ( namo(2)==itwo ) namo(1) = ihex2
         IF ( namo(2)==ithr ) namo(1) = ihex3
         istrpt = istrpt + 1
         IF ( istrpt==nsil+1 .OR. istrpt==21 ) iold = 0
         IF ( namo(1)==ihex3 ) THEN
            namo(2) = ipt(istrpt)
         ELSE
            IF ( namo(1)/=ihex1 .OR. istrpt/=9 ) THEN
               IF ( namo(1)/=ihex2 .OR. istrpt/=21 ) THEN
                  isub1 = Ieqex + 1
                  isub2 = Ieqex + Neqex - 1
                  DO jjj = isub1 , isub2 , 2
                     ns = iz(jjj)/10
                     IF ( ns==isil(istrpt) ) THEN
                        namo(2) = iz(jjj-1)
                        GOTO 50
                     ENDIF
                  ENDDO
                  CALL mesage(-30,164,iz(jjj))
               ENDIF
            ENDIF
            namo(2) = 0
         ENDIF
      ENDIF
 50   IF ( nq<=0 ) THEN
         tgrad(1) = 0.0
         qout(1) = 0.0
         RETURN
      ENDIF
   ENDIF
   DO i = 1 , nsil
      tvec(i) = 0.0
      ip = isil(i)
      IF ( ip/=0 ) THEN
         itemp = ivec + ip - 1
         tvec(i) = zz(itemp)
      ENDIF
   ENDDO
!***
   CALL gmmats(ce(1),nq,nsil,0,tvec(1),nsil,1,0,tgrad(1))
!
   CALL gmmats(rk(1),nq,nq,0,tgrad(1),nq,1,0,qout(1))
!
   DO i = 1 , nq
      qout(i) = -qout(i)
   ENDDO
!
END SUBROUTINE sdhtf2
