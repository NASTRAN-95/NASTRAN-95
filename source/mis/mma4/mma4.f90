!*==mma4.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mma4(Zi,Zr,Zd,Zc,Zdc)
   USE i_mmacom
   USE c_mpyadx
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   IMPLICIT NONE
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
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         module(3) = jbegn
         IF ( nastor==1 ) module(2) = kzero
         IF ( nastor==2 ) module(2) = kone
         CALL conmsg(module,3,0)
         incru = 1
         typei = ndtype
         typep = ndtype
         nwdd = nwords(ndtype)
         nwdb = nwords(nbtype)
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
         ibuf1 = nz - sysbuf
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
         iprow1 = 1
         iprown = ndr
         incrp = 1
         CALL gopen(filea,Zr(ibuf1),rdrew)
         CALL gopen(fileb,Zr(ibuf2),rdrew)
         IF ( filec(1)/=0 .AND. signc/=0 ) CALL gopen(filec,Zr(ibuf3),rdrew)
         ipass = 0
         ircoln = 0
         nwddndr = nwdd*ndr
         CALL gopen(filed,Zr(ibuf4),wrtrew)
         filed(2) = 0
         filed(6) = 0
         filed(7) = 0
         spag_nextblock_1 = 2
      CASE (2)
         ipass = ipass + 1
         ircol1 = ircoln + 1
         ircoln = nbc
         irfile = fileb(1)
         sign = signab
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
            CALL rewind(filea)
            CALL skprec(filea,1)
         ENDIF
!
! NOW READ INTO MEMORY THE "C" FILE.
! READ AS MANY COLUMNS OF THIS AS WERE READ OF THE "B" MATRIX
!
         IF ( filec(1)==0 .OR. signc==0 ) THEN
!
! "C" MATRIX IS NULL OR "SIGNC" IS ZERO
!
            len = idx + ncolpp*nwddndr - 1
            DO k = idx , len
               Zr(k) = 0.0
            ENDDO
         ELSE
            indxd = idx
            typeu = ndtype*signc
            iurow1 = 1
            iurown = ncr
            DO i = 1 , ncolpp
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     CALL unpack(*2,filec,Zr(indxd))
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
!
! NULL COLUMN READ ON "C"
!
 2                   len = indxd + nwddndr - 1
                     DO k = indxd , len
                        Zr(k) = 0.0
                     ENDDO
                     spag_nextblock_2 = 2
                  CASE (2)
                     indxd = indxd + nwddndr
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
         ENDIF
!
! PROCESS ALL OF THE COLUMNS OF "A" MATRIX
!
         sign = 1
         IF ( ksys58/=40 ) THEN
            IF ( ksys58==41 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nastor==2 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
         IF ( ndtype==1 ) CALL mma401(Zi,Zr)
         IF ( ndtype==2 ) CALL mma402(Zi,Zd)
         IF ( ndtype==3 ) CALL mma403(Zi,Zc)
         IF ( ndtype==4 ) CALL mma404(Zi,Zd,Zdc)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
         IF ( ndtype==1 ) CALL mma411(Zi,Zr)
         IF ( ndtype==2 ) CALL mma412(Zi,Zd)
         IF ( ndtype==3 ) CALL mma413(Zi,Zc)
         IF ( ndtype==4 ) CALL mma414(Zi,Zd,Zdc)
         spag_nextblock_1 = 4
      CASE (4)
         IF ( ircoln<nbc ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
! ALL COLUMNS OF A HAVE BEEN PROCESSED, MULTIPLICATION COMPLETE
!
         CALL close(filea,clsrew)
         CALL close(fileb,clsrew)
         CALL close(filec,clsrew)
         CALL close(filed,clsrew)
         module(3) = jend
         CALL conmsg(module,3,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mma4
