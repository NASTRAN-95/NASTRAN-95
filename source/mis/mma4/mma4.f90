!*==mma4.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mma4(Zi,Zr,Zd,Zc,Zdc)
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
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ibx2 , indxd , itest , k , ksys58 , lasmems , len , nac , nadens , naform ,       &
            & nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd , ncr ,        &
            & nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout , sysbuf
   INTEGER , SAVE :: jbegn , jend , kone , kzero
   INTEGER , DIMENSION(3) , SAVE :: module
!
! End of declarations rewritten by SPAG
!
!
!     MMA4 PERFORMS THE MATRIX OPERATION USING METHODS 40 AND 41
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA4 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  PACK (IN COMPACT FORM) AS MANY COLUMNS OF THE "B" MATRIX INTO
!           MEMORY AS POSSIBLE LEAVING SPACE FOR A FULL COLUMN OF THE
!           "D" MATRIX FOR EACH COLUMN OF THE "B" MATRIX READ.
!           SEE SUBROUTINES MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRIX "C".
!       5.  FOR METHOD 40, CALL UNPACK TO READ COLUMNS OF MATRIX "A".
!       6.  FOR METHOD 41, CALL MMARC1,2,3,4 TO READ COLUMNS OF "A" INTO
!           MEMORY IN COMPACT FORM.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA4 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   module(3) = jbegn
   IF ( nastor==1 ) module(2) = kzero
   IF ( nastor==2 ) module(2) = kone
   CALL conmsg(module,3,0)
   Incru = 1
   Typei = ndtype
   Typep = ndtype
   nwdd = Nwords(ndtype)
   nwdb = Nwords(nbtype)
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
!     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
!     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
   ibx = 1 + nwdd*nar
   IF ( nastor==2 .OR. ksys58==41 ) ibx = 1 + nwdd*nar + nar
   IF ( mod(ibx,2)==0 ) ibx = ibx + 1
   ibx2 = ((ibx+1)/2)
   ibuf1 = Nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = ibuf2 - sysbuf
   ibuf4 = ibuf3 - sysbuf
   lasmems = ibuf4 - 1
!
! INSURE THAT LASMEM IS ON A QUAD WORD BOUNDARY TO ALLOW FOR COMPLEX
! DOUBLE DATA TO BE REFERENCED FOR "D" MATRIX COLUMNS
!
   itest = mod(lasmems,4)
   IF ( itest/=1 ) THEN
      IF ( itest==0 ) lasmems = lasmems - 3
      IF ( itest==2 ) lasmems = lasmems - 5
      IF ( itest==3 ) lasmems = lasmems - 6
   ENDIF
   Iprow1 = 1
   Iprown = ndr
   Incrp = 1
   CALL gopen(Filea,Zr(ibuf1),Rdrew)
   CALL gopen(Fileb,Zr(ibuf2),Rdrew)
   IF ( Filec(1)/=0 .AND. Signc/=0 ) CALL gopen(Filec,Zr(ibuf3),Rdrew)
   ipass = 0
   ircoln = 0
   nwddndr = nwdd*ndr
   CALL gopen(Filed,Zr(ibuf4),Wrtrew)
   Filed(2) = 0
   Filed(6) = 0
   Filed(7) = 0
 100  ipass = ipass + 1
   ircol1 = ircoln + 1
   ircoln = nbc
   irfile = Fileb(1)
   sign = Signab
   lasmem = lasmems - ibx
   IF ( ipass/=1 ) CALL dsspos(irfile,irpos(1),irpos(2),irpos(3))
   IF ( ndtype==1 ) CALL mmarm1(Zi(ibx),Zr(ibx),nwddndr)
   IF ( ndtype==2 ) CALL mmarm2(Zi(ibx),Zd(ibx2),nwddndr)
   IF ( ndtype==3 ) CALL mmarm3(Zi(ibx),Zc(ibx2),nwddndr)
   IF ( ndtype==4 ) CALL mmarm4(Zi(ibx),Zd(ibx2),nwddndr)
   ncolpp = ircoln - ircol1 + 1
   ibrow = ircol1 - 1
   idx = lasmem + ibx
   idx2 = ((idx+1)/2) - 1
   idx4 = (idx+1)/4
   IF ( ipass/=1 ) THEN
      CALL rewind(Filea)
      CALL skprec(Filea,1)
   ENDIF
!
! NOW READ INTO MEMORY THE "C" FILE.
! READ AS MANY COLUMNS OF THIS AS WERE READ OF THE "B" MATRIX
!
   IF ( Filec(1)==0 .OR. Signc==0 ) THEN
!
! "C" MATRIX IS NULL OR "SIGNC" IS ZERO
!
      len = idx + ncolpp*nwddndr - 1
      DO k = idx , len
         Zr(k) = 0.0
      ENDDO
   ELSE
      indxd = idx
      Typeu = ndtype*Signc
      Iurow1 = 1
      Iurown = ncr
      DO i = 1 , ncolpp
         CALL unpack(*120,Filec,Zr(indxd))
         GOTO 140
!
! NULL COLUMN READ ON "C"
!
 120     len = indxd + nwddndr - 1
         DO k = indxd , len
            Zr(k) = 0.0
         ENDDO
 140     indxd = indxd + nwddndr
      ENDDO
   ENDIF
!
! PROCESS ALL OF THE COLUMNS OF "A" MATRIX
!
   sign = 1
   IF ( ksys58/=40 ) THEN
      IF ( ksys58==41 ) GOTO 200
      IF ( nastor==2 ) GOTO 200
   ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
   IF ( ndtype==1 ) CALL mma401(Zi,Zr)
   IF ( ndtype==2 ) CALL mma402(Zi,Zd)
   IF ( ndtype==3 ) CALL mma403(Zi,Zc)
   IF ( ndtype==4 ) CALL mma404(Zi,Zd,Zdc)
   GOTO 300
 200  IF ( ndtype==1 ) CALL mma411(Zi,Zr)
   IF ( ndtype==2 ) CALL mma412(Zi,Zd)
   IF ( ndtype==3 ) CALL mma413(Zi,Zc)
   IF ( ndtype==4 ) CALL mma414(Zi,Zd,Zdc)
 300  IF ( ircoln<nbc ) GOTO 100
!
! ALL COLUMNS OF A HAVE BEEN PROCESSED, MULTIPLICATION COMPLETE
!
   CALL close(Filea,Clsrew)
   CALL close(Fileb,Clsrew)
   CALL close(Filec,Clsrew)
   CALL close(Filed,Clsrew)
   module(3) = jend
   CALL conmsg(module,3,0)
END SUBROUTINE mma4
