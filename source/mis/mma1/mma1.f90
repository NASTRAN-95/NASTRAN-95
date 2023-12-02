!*==mma1.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mma1(Zi,Zr,Zd,Zc,Zdc)
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
   INTEGER :: i , iavail , ibuf1 , ibuf2 , ibuf3 , ibuf4 , indx , itest , ksys58 , m , nac , nadens , naform , nanzwd , nar ,       &
            & natype , nbc , nbdens , nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc ,        &
            & nddens , ndform , ndnzwd , ndr , ndtype , nout , npass , sysbuf
   INTEGER , SAVE :: jbegn , jend , kone , kzero
   INTEGER , DIMENSION(3) , SAVE :: module
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MMA1 PERFORMS THE MATRIX OPERATION USING METHODS 10 AND 11
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA1 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
!           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  CALL UNPACK TO READ MATRICES "A" AND "C".
!       5.  FOR METHOD 10, CALL UNPACK TO READ COLUMNS OF MATRIX "B".
!       6.  FOR METHOD 11, CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "B"
!           INTO MEMORY IN COMPACT FORM.
!
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA1 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   module(3) = jbegn
   IF ( nbstor==1 ) module(2) = kzero
   IF ( nbstor==2 ) module(2) = kone
   CALL conmsg(module,3,0)
   incru = 1
   typei = ndtype
   typep = ndtype
   sign = signab
   nwdd = nwords(ndtype)
   nwdb = nwords(nbtype)
   irfile = fileb(1)
!
!   OPEN CORE ALLOCATION AS FOLLOWS:
!     Z( 1        ) = ARRAY FOR ONE COLUMN OF "B" MATRIX
!     Z( IDX      ) = ARRAY FOR ONE COLUMN OF "D" MATRIX
!     Z( IAX      ) = ARRAY FOR MULTIPLE COLUMNS OF "A" MATRIX
!        THROUGH
!     Z( LASMEM   )
!     Z( IBUF4    ) = BUFFER FOR "D" FILE
!     Z( IBUF3    ) = BUFFER FOR "C" FILE
!     Z( IBUF2    ) = BUFFER FOR "B" FILE
!     Z( IBUF1    ) = BUFFER FOR "A" FILE
!     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
!
   IF ( nbstor==1 .OR. ksys58==10 ) idx = 1 + nwdd*nbr
   IF ( nbstor==2 .OR. ksys58==11 ) THEN
!
! REDEFINE IDX AND INSURE A QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
!
      idx = 1 + nwdd*nbr + nbr
      itest = mod(idx,4)
      IF ( itest/=1 ) THEN
         IF ( itest==0 ) idx = idx + 1
         IF ( itest==2 ) idx = idx + 3
         IF ( itest==3 ) idx = idx + 2
      ENDIF
   ENDIF
   idx2 = ((idx+1)/2) - 1
   idx4 = (idx+1)/4
   iax = idx + nwdd*ndr
   ibuf1 = nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = 0
   IF ( filec(1)==0 .OR. signc==0 ) THEN
      ibuf4 = ibuf2 - sysbuf
   ELSE
      ibuf3 = ibuf2 - sysbuf
      ibuf4 = ibuf3 - sysbuf
   ENDIF
   lasmem = ibuf4 - 1
   iprow1 = 1
   iprown = ndr
   incrp = 1
   CALL gopen(filea,Zr(ibuf1),rdrew)
   CALL gopen(fileb,Zr(ibuf2),rdrew)
   SPAG_Loop_1_1: DO
!
!   DETERMINE HOW MANY COLUMNS OF A CAN BE READ INTO MEMORY IN ONE PASS
!
      iavail = lasmem - iax + 1
!
!   NCOLPP  -  NUMBER OF COLUMNS OF "A" THAT CAN BE READ IN ONE PASS
!   NPASS   -  NUMBER OF PASSES NEEDED TO READ ENTIRE "A" MATRIX
!
      ncolpp = iavail/(2+nwdd*nar)
      IF ( ncolpp>nac ) ncolpp = nac
      IF ( ncolpp<=0 ) CALL mesage(-8,iavail+nwdd*nar,module)
      npass = ((nac-1)/ncolpp) + 1
      IF ( npass==1 .OR. ibuf3/=0 ) THEN
         DO m = 1 , npass
            spag_nextblock_1 = 1
            SPAG_DispatchLoop_1: DO
               SELECT CASE (spag_nextblock_1)
               CASE (1)
                  ipass = m
                  ibrow = (m-1)*ncolpp
                  IF ( m==npass ) THEN
! LAST PASS, CREATE OUTPUT FILE
                     ncolpp = nac - ncolpp*(npass-1)
                     ofile = filed(1)
                     ifile = scrtch
                     CALL rewind(fileb)
                     CALL skprec(fileb,1)
                     CALL gopen(filed,Zr(ibuf4),wrtrew)
                     filed(2) = 0
                     filed(6) = 0
                     filed(7) = 0
                     IF ( m/=1 ) THEN
                        CALL gopen(ifile,Zr(ibuf3),rdrew)
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ELSE
!
! MULTIPLE PASSES REQUIRED, DETERMINE PROPER FILE FOR OUTPUT SO THAT
! REQUESTED OUTPUT FILE IS USED ON THE LAST PASS
!
                     itest = npass - m
                     itest = mod(itest,2)
                     IF ( itest/=0 ) THEN
                        ifile = filed(1)
                        ofile = scrtch
                     ELSE
                        ifile = scrtch
                        ofile = filed(1)
                     ENDIF
                     IF ( m==1 ) THEN
! FIRST PASS, OPEN "C" FILE IF IT EXISTS
                        CALL gopen(ofile,Zr(ibuf4),wrtrew)
                     ELSE
                        CALL rewind(fileb)
                        CALL skprec(fileb,1)
                        CALL gopen(ifile,Zr(ibuf3),rdrew)
                        CALL gopen(ofile,Zr(ibuf4),wrtrew)
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
                  ifile = filec(1)
                  IF ( signc==0 ) ifile = 0
                  IF ( ifile/=0 ) CALL gopen(ifile,Zr(ibuf3),rdrew)
                  spag_nextblock_1 = 2
               CASE (2)
                  indx = iax
                  typeu = ndtype
                  DO i = 1 , ncolpp
                     iurow1 = -1
                     CALL unpack(*2,filea,Zr(indx+2))
                     Zi(indx) = iurow1
                     Zi(indx+1) = iurown
                     indx = indx + 2 + nwdd*nar
                     CYCLE
! NULL COLUMN READ
 2                   Zi(indx) = 0
                     Zi(indx+1) = 0
                     indx = indx + 2 + nwdd*nar
                  ENDDO
                  IF ( ksys58/=10 ) THEN
                     IF ( ksys58==11 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( nbstor==2 ) THEN
                        spag_nextblock_1 = 3
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
                  IF ( ndtype==1 ) CALL mma101(Zi,Zr)
                  IF ( ndtype==2 ) CALL mma102(Zi,Zd)
                  IF ( ndtype==3 ) CALL mma103(Zi,Zc)
                  IF ( ndtype==4 ) CALL mma104(Zi,Zd,Zdc)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               CASE (3)
                  IF ( ndtype==1 ) CALL mma111(Zi,Zr)
                  IF ( ndtype==2 ) CALL mma112(Zi,Zd)
                  IF ( ndtype==3 ) CALL mma113(Zi,Zc)
                  IF ( ndtype==4 ) CALL mma114(Zi,Zd,Zdc)
                  spag_nextblock_1 = 4
               CASE (4)
                  CALL close(ifile,clsrew)
                  CALL close(ofile,clsrew)
                  EXIT SPAG_DispatchLoop_1
               END SELECT
            ENDDO SPAG_DispatchLoop_1
         ENDDO
         CALL close(filea,clsrew)
         CALL close(fileb,clsrew)
         module(3) = jend
         CALL conmsg(module,3,0)
         EXIT SPAG_Loop_1_1
      ELSE
!
! MUST ALLOCATE TWO BUFFERS FOR MULTIPLE PASSES
!
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
         lasmem = ibuf4 - 1
      ENDIF
   ENDDO SPAG_Loop_1_1
END SUBROUTINE mma1
