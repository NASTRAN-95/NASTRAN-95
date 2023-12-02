!*==mma2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma2(Zi,Zr,Zd,Zc,Zdc)
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
   INTEGER :: i , iavail , ibuf1 , ibuf2 , ibuf3 , ibuf4 , indxb , indxd , itest , k , ksys58 , len , m , nac , nadens , naform ,   &
            & nanzwd , nar , natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd , ncr ,        &
            & nctype , ndc , nddens , ndform , ndnzwd , ndr , ndtype , nout , npass , sysbuf
   INTEGER , SAVE :: jbegn , jend , kone , kzero
   INTEGER , DIMENSION(3) , SAVE :: module
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
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
   incru = 1
   typei = ndtype
   typep = ndtype
   nwdd = nwords(ndtype)
   irfile = filea(1)
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
   ibuf1 = nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   IF ( filec(1)==0 ) THEN
      ibuf4 = ibuf2 - sysbuf
   ELSE
      ibuf3 = ibuf2 - sysbuf
      ibuf4 = ibuf3 - sysbuf
   ENDIF
   lasmem = ibuf4 - 1
   iprow1 = 1
   iprown = ndr
   incrp = 1
   sign = 1.0
   CALL gopen(filea,Zr(ibuf1),rdrew)
   CALL gopen(fileb,Zr(ibuf2),rdrew)
   IF ( filec(1)/=0 ) CALL gopen(filec,Zr(ibuf3),rdrew)
   CALL gopen(filed,Zr(ibuf4),wrtrew)
   filed(2) = 0
   filed(6) = 0
   filed(7) = 0
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
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            ipass = m
            IF ( m==npass ) ncolpp = nbc - (ncolpp*(npass-1))
            CALL rewind(filea)
            CALL skprec(filea,1)
            indxb = ibx
            indxd = idx
            typeu = ndtype*signab
            DO i = 1 , ncolpp
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     iurow1 = -1
                     CALL unpack(*2,fileb,Zr(indxb+2))
                     Zi(indxb) = iurow1
                     Zi(indxb+1) = iurown
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
! NULL COLUMN READ ON "B"
 2                   Zi(indxb) = 0
                     Zi(indxb+1) = 0
                     spag_nextblock_2 = 2
                  CASE (2)
                     indxb = indxb + nwddnbr + 2
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
            IF ( filec(1)==0 .OR. signc==0 ) THEN
!
! "C" MATRIX IS NULL OR "SIGNC" IS ZERO
!
               len = idx + ncolpp*nwddndr - 1
               DO k = idx , len
                  Zr(k) = 0.
               ENDDO
            ELSE
               typeu = ndtype*signc
               iurow1 = 1
               iurown = ncr
               DO i = 1 , ncolpp
                  spag_nextblock_3 = 1
                  SPAG_DispatchLoop_3: DO
                     SELECT CASE (spag_nextblock_3)
                     CASE (1)
                        CALL unpack(*4,filec,Zr(indxd))
                        spag_nextblock_3 = 2
                        CYCLE SPAG_DispatchLoop_3
!
! NULL COLUMN READ ON "C"
!
 4                      len = indxd + nwddndr - 1
                        DO k = indxd , len
                           Zr(k) = 0.0
                        ENDDO
                        spag_nextblock_3 = 2
                     CASE (2)
                        indxd = indxd + nwddndr
                        EXIT SPAG_DispatchLoop_3
                     END SELECT
                  ENDDO SPAG_DispatchLoop_3
               ENDDO
            ENDIF
!
! PROCESS ALL OF THE COLUMNS OF "A"
!
            IF ( ksys58/=21 ) THEN
               IF ( ksys58/=20 ) THEN
                  IF ( nastor==2 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
               IF ( ndtype==1 ) CALL mma201(Zi,Zr)
               IF ( ndtype==2 ) CALL mma202(Zi,Zd)
               IF ( ndtype==3 ) CALL mma203(Zi,Zc)
               IF ( ndtype==4 ) CALL mma204(Zi,Zd,Zdc)
               CYCLE
            ENDIF
            spag_nextblock_1 = 2
         CASE (2)
            IF ( ndtype==1 ) CALL mma211(Zi,Zr)
            IF ( ndtype==2 ) CALL mma212(Zi,Zd)
            IF ( ndtype==3 ) CALL mma213(Zi,Zc)
            IF ( ndtype==4 ) CALL mma214(Zi,Zd,Zdc)
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL close(filea,clsrew)
   CALL close(fileb,clsrew)
   CALL close(filec,clsrew)
   CALL close(filed,clsrew)
   module(3) = jend
   CALL conmsg(module,3,0)
END SUBROUTINE mma2
