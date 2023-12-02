!*==fbsf.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fbsf(Zs,Zd)
!
!     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND
!     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM
!     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR
!     SYSTEM OF EQUATIONS.
!
!     OPEN CORE IS DEFINED AS FOLLOWS
!
!     ZS(   1         ) - FIRST RIGHT HAND VECTOR ON FILE DBB
!                         (SIZE = NCOL*NWDS)
!                         NCOL = NUMBER OF COLUMNS (ROWS) IN LOWER
!                                TRIANGULAR MATRIX
!                         NWDS = 1, IF MATRICES ARE REAL SINGLE
!                              = 2, IF MATRICES ARE REAL DOUBLE OR
!                                COMPLEX SINGLE
!                              = 4, IF MATRICES ARE COMPLEX DOUBLE
!     ZS( NCOL*NWDS+1 ) - NEXT RIGHT HAND VECTOR
!         .
!         .               ( "KN" RIGHT HAND VECTORS WILL BE LOADED INTO
!         .               MEMORY)
!         .
!     ZS( BUF1        ) - BUFFER FOR FILE WITH RIGHT HAND VECTORS
!                         AND FOR SOLUTION VECTORS
!     ZS( BUF2        ) - BUFFER FOR FILE WITH TRIANGULAR MATRIX
!
USE C_FBSX
USE C_LOGOUT
USE C_NAMES
USE C_PACKX
USE C_SYSTEM
USE C_TYPE
USE C_UNPAKX
USE C_XMSSG
USE C_ZBLPKX
USE C_ZNTPKX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Zs
   REAL(REAL64) , DIMENSION(1) :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: begn , end
   INTEGER , DIMENSION(15) :: block
   INTEGER :: buf1 , buf2 , dbl1 , i , icdpsg , icspsg , j , k , k1 , kn , l , last , ll , ln , nbrlod , need , nl , nnn , nnndbl , &
            & npass , nterms , nvecs , nwds , opcls , oprd , opwrt , rc , switch , typeb , typel , typex , wds
   LOGICAL :: ident
   INTEGER , DIMENSION(2) , SAVE :: subnam
   REAL , DIMENSION(4) :: xs , ys
   EXTERNAL bldpk , bldpkn , close , conmsg , fbs1 , fbs2 , fbs3 , fbs4 , fwdrec , gopen , intpk , mesage , pack , rewind , unpack ,&
          & zblpki , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Dbl(2),Nl) , (Dbb(5),Typeb) , (Dbx(5),Typex) , (Xd(1),Xs(1)) , (Yd(1),Ys(1))
   DATA subnam/4HFBSF , 4H    /
   DATA begn/4HBEGN/
   DATA end/4HEND /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     GENERAL INITIALIZATION
!
         buf2 = Lcore - Sysbuf
         buf1 = buf2 - Sysbuf
         rc = Rlcmpx(typeb)
         typel = Dbl(5)
         wds = Words(typel)
         nwds = wds*nl
         nbrlod = Dbb(2)
         ident = .FALSE.
         IF ( Dbb(4)==8 ) ident = .TRUE.
         IF ( ident ) nbrlod = nl
         switch = 1
         IF ( typel==Rsp .AND. rc==2 ) switch = 2
         IF ( typel==Rdp .AND. rc==2 ) switch = 3
         dbl1 = Dbl(1)
         nnn = buf1 - 1
         nvecs = nnn/nwds
         IF ( nvecs==0 ) CALL mesage(-8,nwds-nnn,subnam)
         IF ( switch/=1 ) nvecs = nvecs/2
         npass = (nbrlod+nvecs-1)/nvecs
         subnam(2) = begn
         CALL conmsg(subnam,2,0)
         npass = (nbrlod+nvecs-1)/nvecs
         IF ( npass/=1 ) THEN
            need = nwds*nbrlod + 2*Sysbuf
            WRITE (Lout,99001) npass , need
99001       FORMAT (I4,' PASSES REQUIRED, OPEN CORE NEEDS TO BE ',I7,' TO ELIMINATE THIS')
         ENDIF
         I2 = 1
         J2 = nl
         Incr2 = 1
         I1 = 1
         J1 = nl
         Incr1 = 1
         Itype1 = typel
         Itype2 = typex
         Itype3 = Sign*typel
         Dbx(2) = 0
         Dbx(6) = 0
         Dbx(7) = 0
         nnndbl = nnn/2
         nterms = Rlcmpx(typel)*nl
         k1 = 1
         oprd = Rdrew
         opwrt = Wrtrew
         block(1) = Dbl(1)
!
!     OPEN LOWER TRIANGULAR FACTOR FILE (DBL1)
!
         CALL gopen(dbl1,Zs(buf2),Rdrew)
         DO
!
!     OPEN RIGHT HAND VECTORS FILE (DBB) AND COMPUTE EXTENT OF THIS PASS
!
            kn = min0(k1+nvecs-1,nbrlod)
            last = (kn-k1+1)*nwds
            opcls = Norew
            IF ( kn==nbrlod ) opcls = Rew
            IF ( ident ) THEN
!
!     SPECIAL CASE - GENERATE IDENTITY MATRIX
!
               DO k = 1 , nnndbl
                  Zd(k) = 0.0D+0
               ENDDO
               l = 0
               IF ( typel==2 ) THEN
                  DO k = k1 , kn
                     Zd(l+k) = 1.0D+0
                     l = l + nterms
                  ENDDO
               ELSEIF ( typel==3 ) THEN
                  DO k = k1 , kn
                     Zs(l+2*k-1) = 1.0
                     l = l + nterms
                  ENDDO
               ELSEIF ( typel==4 ) THEN
                  DO k = k1 , kn
                     Zd(l+2*k-1) = 1.0D+0
                     l = l + nterms
                  ENDDO
               ELSE
                  DO k = k1 , kn
                     Zs(l+k) = 1.0
                     l = l + nterms
                  ENDDO
               ENDIF
            ELSE
               CALL gopen(Dbb,Zs(buf1),oprd)
               IF ( switch==2 ) THEN
!
!     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP
!
                  last = 2*(kn-k1+1)*nwds
                  l = 0
                  DO k = 1 , nnndbl
                     Zd(k) = 0.0D+0
                  ENDDO
                  DO k = k1 , kn
                     icspsg = Csp*Sign
                     CALL intpk(*2,Dbb,0,icspsg,0)
                     SPAG_Loop_3_1: DO
                        CALL zntpki
                        Zs(l+Ix) = xs(1)
                        Zs(l+Ix+nl) = xs(2)
                        IF ( Eol/=0 ) EXIT SPAG_Loop_3_1
                     ENDDO SPAG_Loop_3_1
 2                   l = l + 2*nl
                  ENDDO
               ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP
!
                  last = 2*(kn-k1+1)*nwds
                  l = 0
                  DO k = 1 , nnndbl
                     Zd(k) = 0.0D+0
                  ENDDO
                  DO k = k1 , kn
                     icdpsg = Cdp*Sign
                     CALL intpk(*4,Dbb,0,icdpsg,0)
                     SPAG_Loop_3_2: DO
                        CALL zntpki
                        Zd(l+Ix) = Xd(1)
                        Zd(l+Ix+nl) = Xd(2)
                        IF ( Eol/=0 ) EXIT SPAG_Loop_3_2
                     ENDDO SPAG_Loop_3_2
 4                   l = l + 2*nl
                  ENDDO
               ELSE
!
!     NORMAL CASE - FILL CORE WITH RIGHT HAND VECTORS
!
                  DO l = 1 , last , nwds
                     CALL unpack(*6,Dbb,Zs(l))
                     CYCLE
 6                   ln = l + nwds - 1
                     DO ll = l , ln
                        Zs(ll) = 0.
                     ENDDO
                  ENDDO
               ENDIF
!
!    CLOSE RIGHT HAND VECTORS FILE (DBB).
!    START FORWARD-BACKWARD SUBSTITUTION ON RIGHT HAND VECTORS NOW IN CORE
!
               CALL close(Dbb,opcls)
            ENDIF
            CALL rewind(dbl1)
            CALL fwdrec(*20,dbl1)
!
            j = typel
            IF ( j==2 ) THEN
               CALL fbs2(block,Zs,Zs(last),nwds)
            ELSEIF ( j==3 ) THEN
               CALL fbs3(block,Zs,Zs(last),nwds)
            ELSEIF ( j==4 ) THEN
               CALL fbs4(block,Zs,Zs(last),nwds)
            ELSE
               CALL fbs1(block,Zs,Zs(last),nwds)
            ENDIF
!
!     OPEN AND PACK SOLUTION VECTORS ONTO OUTPUT FILE (DBX)
!
            CALL gopen(Dbx,Zs(buf1),opwrt)
            IF ( switch==2 ) THEN
!
!     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP, CALL BLDPK
!
               l = 0
               DO k = k1 , kn
                  CALL bldpk(Csp,typex,Dbx,0,0)
                  DO i = 1 , nl
                     ys(1) = Zs(l+i)
                     ys(2) = Zs(l+i+nl)
                     Iy = i
                     CALL zblpki
                  ENDDO
                  CALL bldpkn(Dbx,0,Dbx)
                  l = l + 2*nl
               ENDDO
            ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP, CALL BLDPK
!
               l = 0
               DO k = k1 , kn
                  CALL bldpk(Cdp,typex,Dbx,0,0)
                  DO i = 1 , nl
                     Yd(1) = Zd(l+i)
                     Yd(2) = Zd(l+i+nl)
                     Iy = i
                     CALL zblpki
                  ENDDO
                  CALL bldpkn(Dbx,0,Dbx)
                  l = l + 2*nl
               ENDDO
            ELSE
!
!     NORMAL CASE - CALL PACK
!
               DO l = 1 , last , nwds
                  CALL pack(Zs(l),Dbx,Dbx)
               ENDDO
            ENDIF
!
!     CLOSE OUTPUT FILE, AND TEST FOR MORE PASSES
!
            CALL close(Dbx,opcls)
            IF ( kn==nbrlod ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k1 = kn + 1
            oprd = Rd
            opwrt = Wrt
         ENDDO
!
!     ERROR
!
 20      CALL mesage(-2,dbl1,subnam)
         spag_nextblock_1 = 2
      CASE (2)
!
!     JOB DONE. CLOSE TRIANGULAR FACTOR FILE.
!
         CALL close(dbl1,Rew)
         subnam(2) = end
         CALL conmsg(subnam,2,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fbsf
