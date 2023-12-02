!*==mma3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
SUBROUTINE mma3(Zi,Zr,Zd,Zc,Zdc)
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
   INTEGER :: ibuf1 , ibuf2 , ibuf3 , ibuf4 , itest , ksys58 , nac , nadens , naform , nanzwd , nar , natype , nbc , nbdens ,       &
            & nbform , nbnzwd , nbr , nbtype , ncc , ncdens , ncform , ncnzwd , ncr , nctype , ndc , nddens , ndform , ndnzwd ,     &
            & ndr , ndtype , nout , sysbuf
   INTEGER , SAVE :: jbegn , jend , kone , ktwo , kzero
   INTEGER , DIMENSION(3) , SAVE :: module
   INTEGER :: spag_nextblock_1
!
! End of declarations rewritten by SPAG
!
!
!     MMA3 PERFORMS THE MATRIX OPERATION USING METHODS 30, 31 AND 32
!       (+/-)A(T & NT) * B (+/-)C = D
!
!     MMA3 IS DESIGNED AS FOLLOWS:
!       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
!       2.  PACK (IN COMPACT FORM) AS MANY COLUMNS OF "A" INTO MEMORY
!           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
!           SEE SUBROUTINES MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
!       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
!       4.  FOR METHODS 30 AND 31, CALL UNPACK TO READ MATRIX "C".
!       5.  FOR METHOD 30, CALL UNPACK TO READ COLUMNS OF MATRIX "B".
!       6.  FOR METHOD 31, CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" INTO
!           MEMORY IN COMPACT FORM.
!       7.  FOR METHOD 32, CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" AND
!           "C" INTO MEMORY IN COMPACT FORM.
!       8.  FOR METHODS 30 AND 31, CALL PACK TO WRITE "D" MATRIX.
!       9.  FOR METHOD 32, CALL BLDPK TO WRITE "D" MATRIX.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   !>>>>EQUIVALENCE (Ksystm(58),Ksys58)
   !>>>>EQUIVALENCE (Filea(2),Nac) , (Filea(3),Nar) , (Filea(4),Naform) , (Filea(5),Natype) , (Filea(6),Nanzwd) , (Filea(7),Nadens)
   !>>>>EQUIVALENCE (Fileb(2),Nbc) , (Fileb(3),Nbr) , (Fileb(4),Nbform) , (Fileb(5),Nbtype) , (Fileb(6),Nbnzwd) , (Fileb(7),Nbdens)
   !>>>>EQUIVALENCE (Filec(2),Ncc) , (Filec(3),Ncr) , (Filec(4),Ncform) , (Filec(5),Nctype) , (Filec(6),Ncnzwd) , (Filec(7),Ncdens)
   !>>>>EQUIVALENCE (Filed(2),Ndc) , (Filed(3),Ndr) , (Filed(4),Ndform) , (Filed(5),Ndtype) , (Filed(6),Ndnzwd) , (Filed(7),Nddens)
!
   DATA module/4HMMA3 , 4H     , 4H    /
   DATA kzero/1H0/
   DATA kone/1H1/
   DATA ktwo/1H2/
   DATA jbegn/4HBEGN/ , jend/3HEND/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         module(3) = jbegn
         IF ( method==30 ) module(2) = kzero
         IF ( method==31 ) module(2) = kone
         IF ( method==32 ) module(2) = ktwo
         CALL conmsg(module,3,0)
         incru = 1
         typei = ndtype
         typep = ndtype
         nwdd = nwords(ndtype)
         nwdb = nwords(nbtype)
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
         idx = 1 + nwdd*nbr
         IF ( method==31 .OR. method==32 ) THEN
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
         IF ( method==32 ) THEN
!
! FOR METHOD 32, INSURE IAX IS ON QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
!
            iax = idx + nwdd*ndr + ndr
            itest = mod(iax,4)
            IF ( itest/=1 ) THEN
               IF ( itest==0 ) iax = iax + 1
               IF ( itest==2 ) iax = iax + 3
               IF ( itest==3 ) iax = iax + 2
            ENDIF
         ENDIF
         iax2 = ((iax+1)/2)
         ibuf1 = nz - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
         lasmem = ibuf4 - 1
         lasmem = lasmem - iax
         iprow1 = 1
         iprown = ndr
         incrp = 1
         CALL gopen(filea,Zr(ibuf1),rdrew)
         CALL gopen(fileb,Zr(ibuf2),rdrew)
         ipass = 0
         ircoln = 0
         spag_nextblock_1 = 2
      CASE (2)
         ipass = ipass + 1
         ircol1 = ircoln + 1
         ircoln = nac
         irfile = filea(1)
         sign = signab
         IF ( ipass/=1 ) CALL dsspos(irfile,irpos(1),irpos(2),irpos(3))
         IF ( ndtype==1 ) CALL mmarm1(Zi(iax),Zr(iax),0)
         IF ( ndtype==2 ) CALL mmarm2(Zi(iax),Zd(iax2),0)
         IF ( ndtype==3 ) CALL mmarm3(Zi(iax),Zc(iax2),0)
         IF ( ndtype==4 ) CALL mmarm4(Zi(iax),Zd(iax2),0)
         ncolpp = ircoln - ircol1 + 1
         ibrow = ircol1 - 1
         IF ( ircoln==nac ) THEN
! LAST PASS, CREATE OUTPUT FILE
            IF ( ifile==0 ) ifile = scrtch
            IF ( ofile==filed(1) .AND. ipass/=1 ) CALL filswi(ifile,ofile)
            ofile = filed(1)
            ifile = scrtch
            CALL rewind(fileb)
            CALL skprec(fileb,1)
            CALL gopen(filed,Zr(ibuf4),wrtrew)
            filed(2) = 0
            filed(6) = 0
            filed(7) = 0
            IF ( ipass/=1 ) THEN
               CALL gopen(ifile,Zr(ibuf3),rdrew)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
            itest = mod(ipass,2)
            IF ( itest==0 ) THEN
               ifile = filed(1)
               ofile = scrtch
            ELSE
               ifile = scrtch
               ofile = filed(1)
            ENDIF
            IF ( ipass==1 ) THEN
! FIRST PASS, OPEN "C" FILE IF IT EXISTS
               CALL gopen(ofile,Zr(ibuf4),wrtrew)
            ELSE
               CALL rewind(fileb)
               CALL skprec(fileb,1)
               CALL gopen(ifile,Zr(ibuf3),rdrew)
               CALL gopen(ofile,Zr(ibuf4),wrtrew)
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         ifile = filec(1)
         IF ( signc==0 ) ifile = 0
         IF ( ifile/=0 ) CALL gopen(ifile,Zr(ibuf3),rdrew)
         spag_nextblock_1 = 3
      CASE (3)
         sign = 1
         IF ( method/=30 ) THEN
            IF ( method==31 ) THEN
               IF ( ndtype==1 ) CALL mma311(Zi,Zr)
               IF ( ndtype==2 ) CALL mma312(Zi,Zd)
               IF ( ndtype==3 ) CALL mma313(Zi,Zc)
               IF ( ndtype==4 ) CALL mma314(Zi,Zd,Zdc)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( method==32 ) THEN
               IF ( ndtype==1 ) CALL mma321(Zi,Zr)
               IF ( ndtype==2 ) CALL mma322(Zi,Zd)
               IF ( ndtype==3 ) CALL mma323(Zi,Zc)
               IF ( ndtype==4 ) CALL mma324(Zi,Zd)
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
! PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
         IF ( ndtype==1 ) CALL mma301(Zi,Zr)
         IF ( ndtype==2 ) CALL mma302(Zi,Zd)
         IF ( ndtype==3 ) CALL mma303(Zi,Zc)
         IF ( ndtype==4 ) CALL mma304(Zi,Zd,Zdc)
         spag_nextblock_1 = 4
      CASE (4)
         CALL close(ifile,clsrew)
         CALL close(ofile,clsrew)
         IF ( ircoln<nac ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
! ALL COLUMNS OF A HAVE BEEN PROCESSED, MULTIPLICATION COMPLETE
!
         CALL close(filea,clsrew)
         CALL close(fileb,clsrew)
         module(3) = jend
         CALL conmsg(module,3,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mma3
