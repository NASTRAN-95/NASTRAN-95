!*==fbsf.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
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
   USE c_fbsx
   USE c_logout
   USE c_names
   USE c_packx
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_xmssg
   USE c_zblpkx
   USE c_zntpkx
   USE iso_fortran_env
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
         buf2 = lcore - sysbuf
         buf1 = buf2 - sysbuf
         rc = rlcmpx(typeb)
         typel = dbl(5)
         wds = words(typel)
         nwds = wds*nl
         nbrlod = dbb(2)
         ident = .FALSE.
         IF ( dbb(4)==8 ) ident = .TRUE.
         IF ( ident ) nbrlod = nl
         switch = 1
         IF ( typel==rsp .AND. rc==2 ) switch = 2
         IF ( typel==rdp .AND. rc==2 ) switch = 3
         dbl1 = dbl(1)
         nnn = buf1 - 1
         nvecs = nnn/nwds
         IF ( nvecs==0 ) CALL mesage(-8,nwds-nnn,subnam)
         IF ( switch/=1 ) nvecs = nvecs/2
         npass = (nbrlod+nvecs-1)/nvecs
         subnam(2) = begn
         CALL conmsg(subnam,2,0)
         npass = (nbrlod+nvecs-1)/nvecs
         IF ( npass/=1 ) THEN
            need = nwds*nbrlod + 2*sysbuf
            WRITE (lout,99001) npass , need
99001       FORMAT (I4,' PASSES REQUIRED, OPEN CORE NEEDS TO BE ',I7,' TO ELIMINATE THIS')
         ENDIF
         i2 = 1
         j2 = nl
         incr2 = 1
         i1 = 1
         j1 = nl
         incr1 = 1
         itype1 = typel
         itype2 = typex
         itype3 = sign*typel
         dbx(2) = 0
         dbx(6) = 0
         dbx(7) = 0
         nnndbl = nnn/2
         nterms = rlcmpx(typel)*nl
         k1 = 1
         oprd = rdrew
         opwrt = wrtrew
         block(1) = dbl(1)
!
!     OPEN LOWER TRIANGULAR FACTOR FILE (DBL1)
!
         CALL gopen(dbl1,Zs(buf2),rdrew)
         DO
!
!     OPEN RIGHT HAND VECTORS FILE (DBB) AND COMPUTE EXTENT OF THIS PASS
!
            kn = min0(k1+nvecs-1,nbrlod)
            last = (kn-k1+1)*nwds
            opcls = norew
            IF ( kn==nbrlod ) opcls = rew
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
               CALL gopen(dbb,Zs(buf1),oprd)
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
                     icspsg = csp*sign
                     CALL intpk(*2,dbb,0,icspsg,0)
                     SPAG_Loop_3_1: DO
                        CALL zntpki
                        Zs(l+ix) = xs(1)
                        Zs(l+ix+nl) = xs(2)
                        IF ( eol/=0 ) EXIT SPAG_Loop_3_1
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
                     icdpsg = cdp*sign
                     CALL intpk(*4,dbb,0,icdpsg,0)
                     SPAG_Loop_3_2: DO
                        CALL zntpki
                        Zd(l+ix) = xd(1)
                        Zd(l+ix+nl) = xd(2)
                        IF ( eol/=0 ) EXIT SPAG_Loop_3_2
                     ENDDO SPAG_Loop_3_2
 4                   l = l + 2*nl
                  ENDDO
               ELSE
!
!     NORMAL CASE - FILL CORE WITH RIGHT HAND VECTORS
!
                  DO l = 1 , last , nwds
                     CALL unpack(*6,dbb,Zs(l))
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
               CALL close(dbb,opcls)
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
            CALL gopen(dbx,Zs(buf1),opwrt)
            IF ( switch==2 ) THEN
!
!     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP, CALL BLDPK
!
               l = 0
               DO k = k1 , kn
                  CALL bldpk(csp,typex,dbx,0,0)
                  DO i = 1 , nl
                     ys(1) = Zs(l+i)
                     ys(2) = Zs(l+i+nl)
                     iy = i
                     CALL zblpki
                  ENDDO
                  CALL bldpkn(dbx,0,dbx)
                  l = l + 2*nl
               ENDDO
            ELSEIF ( switch==3 ) THEN
!
!     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP, CALL BLDPK
!
               l = 0
               DO k = k1 , kn
                  CALL bldpk(cdp,typex,dbx,0,0)
                  DO i = 1 , nl
                     yd(1) = Zd(l+i)
                     yd(2) = Zd(l+i+nl)
                     iy = i
                     CALL zblpki
                  ENDDO
                  CALL bldpkn(dbx,0,dbx)
                  l = l + 2*nl
               ENDDO
            ELSE
!
!     NORMAL CASE - CALL PACK
!
               DO l = 1 , last , nwds
                  CALL pack(Zs(l),dbx,dbx)
               ENDDO
            ENDIF
!
!     CLOSE OUTPUT FILE, AND TEST FOR MORE PASSES
!
            CALL close(dbx,opcls)
            IF ( kn==nbrlod ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k1 = kn + 1
            oprd = rd
            opwrt = wrt
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
         CALL close(dbl1,rew)
         subnam(2) = end
         CALL conmsg(subnam,2,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fbsf
