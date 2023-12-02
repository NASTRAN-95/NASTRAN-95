!*==mma2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
 
SUBROUTINE mma2(Zi,Zr,Zd,Zc,Zdc)
   IMPLICIT NONE
   USE I_MMACOM
   USE C_MPYADX
   USE C_NAMES
   USE C_PACKX
   USE C_SYSTEM
   USE C_TYPE
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Zi
   REAL , DIMENSION(2) :: Zr
   REAL*8 , DIMENSION(2) :: Zd
   COMPLEX , DIMENSION(2) :: Zc
   COMPLEX*16 , DIMENSION(2) :: Zdc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iavail , ibuf1 , ibuf2 , ibuf3 , ibuf4 , indxb , indxd , itest , k , ksys58 , len , m , nac , nadens , naform ,   &
            & nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd , ncr ,        &
            & nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout , npass , sysbuf
   INTEGER , SAVE :: jbegn , jend , kone , kzero
   INTEGER , DIMENSION(3) , SAVE :: module
!
! End of declarations rewritten by SPAG
!
!
!     MMA2 PERFORMS THE MATRIX OPERATION USING METHODS 20 AND 21
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA2 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRICES "B" AND "C".
!       5.  FOR METHOD 20, CALL UNPACK TO READ COLUMNS OF MATRIX "A".
!       6.  FOR METHOD 21, CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "A"
!           INTO MEMORY IN COMPACT FORM.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout) , (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA2 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   IF ( nastor==1 .OR. ksys58==20 ) module(2) = kzero
   IF ( nastor==2 .OR. ksys58==21 ) module(2) = kone
   module(3) = jbegn
   CALL conmsg(module,3,0)
   Incru = 1
   Typei = ndtype
   Typep = ndtype
   nwdd = Nwords(ndtype)
   irfile = Filea(1)
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
   idx = 1 + nwdd*nar
   IF ( nastor==2 .OR. ksys58==21 ) THEN
!
! REDEFINE IDX AND INSURE A QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
!
      idx = 1 + nwdd*nar + nar
      itest = mod(idx,4)
      IF ( itest/=1 ) THEN
         IF ( itest==0 ) idx = idx + 1
         IF ( itest==2 ) idx = idx + 3
         IF ( itest==3 ) idx = idx + 2
      ENDIF
   ENDIF
   idx2 = ((idx+1)/2) - 1
   idx4 = (idx+1)/4
   ibuf1 = Nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   IF ( Filec(1)==0 ) THEN
      ibuf4 = ibuf2 - sysbuf
   ELSE
      ibuf3 = ibuf2 - sysbuf
      ibuf4 = ibuf3 - sysbuf
   ENDIF
   lasmem = ibuf4 - 1
   Iprow1 = 1
   Iprown = ndr
   Incrp = 1
   sign = 1.0
   CALL gopen(Filea,Zr(ibuf1),Rdrew)
   CALL gopen(Fileb,Zr(ibuf2),Rdrew)
   IF ( Filec(1)/=0 ) CALL gopen(Filec,Zr(ibuf3),Rdrew)
   CALL gopen(Filed,Zr(ibuf4),Wrtrew)
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
!
!   DETERMINE HOW MANY COLUMNS OF "B" CAN BE READ INTO MEMORY AND HOW
!   MANY COLUMNS OF "D" CAN BE HELD IN MEMORY FOR ONE PASS
!
   iavail = lasmem - idx + 1
!
!   NCOLPP  -  NUMBER OF COLUMNS OF "B" THAT CAN BE READ IN ONE PASS
!   NPASS   -  NUMBER OF PASSES NEEDED TO READ ENTIRE "B" MATRIX
!
   nwddndr = nwdd*ndr
   nwddnbr = nwdd*nbr
   ncolpp = iavail/(2+nwddnbr+nwddndr)
   IF ( ncolpp<=0 ) CALL mesage(-8,iavail+nwddnbr+nwddndr,module)
   IF ( ncolpp>nbc ) ncolpp = nbc
   npass = ((nbc-1)/ncolpp) + 1
   ibx = idx + ncolpp*nwddndr
   DO m = 1 , npass
      ipass = m
      IF ( m==npass ) ncolpp = nbc - (ncolpp*(npass-1))
      CALL rewind(Filea)
      CALL skprec(Filea,1)
      indxb = ibx
      indxd = idx
      Typeu = ndtype*Signab
      DO i = 1 , ncolpp
         Iurow1 = -1
         CALL unpack(*20,Fileb,Zr(indxb+2))
         Zi(indxb) = Iurow1
         Zi(indxb+1) = Iurown
         GOTO 40
! NULL COLUMN READ ON "B"
 20      Zi(indxb) = 0
         Zi(indxb+1) = 0
 40      indxb = indxb + nwddnbr + 2
      ENDDO
      IF ( Filec(1)==0 .OR. Signc==0 ) THEN
!
! "C" MATRIX IS NULL OR "SIGNC" IS ZERO
!
         len = idx + ncolpp*nwddndr - 1
         DO k = idx , len
            Zr(k) = 0.
         ENDDO
      ELSE
         Typeu = ndtype*Signc
         Iurow1 = 1
         Iurown = ncr
         DO i = 1 , ncolpp
            CALL unpack(*50,Filec,Zr(indxd))
            GOTO 60
!
! NULL COLUMN READ ON "C"
!
 50         len = indxd + nwddndr - 1
            DO k = indxd , len
               Zr(k) = 0.0
            ENDDO
 60         indxd = indxd + nwddndr
         ENDDO
      ENDIF
!
! PROCESS ALL OF THE COLUMNS OF "A"
!
      IF ( ksys58/=21 ) THEN
         IF ( ksys58/=20 ) THEN
            IF ( nastor==2 ) GOTO 100
         ENDIF
         IF ( ndtype==1 ) CALL mma201(Zi,Zr)
         IF ( ndtype==2 ) CALL mma202(Zi,Zd)
         IF ( ndtype==3 ) CALL mma203(Zi,Zc)
         IF ( ndtype==4 ) CALL mma204(Zi,Zd,Zdc)
         CYCLE
      ENDIF
 100  IF ( ndtype==1 ) CALL mma211(Zi,Zr)
      IF ( ndtype==2 ) CALL mma212(Zi,Zd)
      IF ( ndtype==3 ) CALL mma213(Zi,Zc)
      IF ( ndtype==4 ) CALL mma214(Zi,Zd,Zdc)
   ENDDO
   CALL close(Filea,Clsrew)
   CALL close(Fileb,Clsrew)
   CALL close(Filec,Clsrew)
   CALL close(Filed,Clsrew)
   module(3) = jend
   CALL conmsg(module,3,0)
END SUBROUTINE mma2
