!*==bseqgp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bseqgp(Norig,Ild,Jump)
   USE c_banda
   USE c_bandb
   USE c_bandd
   USE c_bands
   USE c_bandw
   USE c_geomx
   USE c_names
   USE c_system
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Norig
   INTEGER , DIMENSION(1) :: Ild
   INTEGER :: Jump
!
! Local variable declarations rewritten by SPAG
!
   REAL :: an , ann , av1 , av2 , den
   INTEGER , DIMENSION(3) , SAVE :: eof , seqgp
   INTEGER , DIMENSION(8) :: grid
   INTEGER :: i , iecho , j , j77 , k , lpch , nlpp , nnx , nonz
   INTEGER , DIMENSION(100) :: isys
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL bckrec , bisloc , close , locate , mesage , open , orf , page1 , page2 , preloc , rdtrl , read , rewind , skpfil ,      &
          & sort , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (Ibuf,Isys(1)) , (Nlpp,Isys(9)) , (Lpch,Isys(91)) , (Iecho,Isys(19))
   DATA sub , eof , seqgp/4HSSEQ , 4HGP   , 3*2147483647 , 5301 , 53 , 4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     NORIG(I) = ORIGINAL GRID POINT CORRESPONDING TO BANDIT INTERNAL
!                LABLE I
!     ILD(I)   = NEW RESEQUENCED LABEL CORRESPONDING TO BANDIT INTERNAL
!                LABLE I
!     NN       = NUMBER OF GRID POINTS
!     NGRD     .LT.0, INSUFF. WORKING CORE, OR SCRATCH ARRAY FOR BANDIT
!
         j77 = 0
         IF ( nn<=0 .OR. ngrd<0 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     PRINT BANDIT SUMMARY.
!
         IF ( nlpp<=48 .AND. method==0 ) CALL page1
         WRITE (nout,99001)
99001    FORMAT (//53X,22H*** BANDIT SUMMARY ***,/,/72X,6HBEFORE,5X,5HAFTER)
!
         WRITE (nout,99002) obw , nbw , op , np , maxw0 , maxw1
99002    FORMAT (40X,13HBANDWIDTH (B),15X,2I10,/40X,11HPROFILE (P),17X,2I10,/40X,25HMAXIMUM WAVEFRONT (C-MAX),3X,2I10)
!
         ann = float(nn)
         av1 = float(op)/ann
         av2 = float(np)/ann
         WRITE (nout,99003) av1 , av2 , rms0 , rms1 , brms0 , brms1 , ngpts
99003    FORMAT (40X,25HAVERAGE WAVEFRONT (C-AVG),3X,2F10.3,/40X,21HRMS WAVEFRONT (C-RMS),7X,2F10.3,/40X,21HRMS BANDWITCH (B-RMS),  &
               & 7X,2F10.3,/40X,25HNUMBER OF GRID POINTS (N),15X,I8)
!
         IF ( nspts>0 ) WRITE (nout,99004) nspts
99004    FORMAT (40X,23HNUMBER OF SCALAR POINTS,17X,I8)
!
         WRITE (nout,99005) nel , neqr , neq
99005    FORMAT (40X,30HNUMBER OF ELEMENTS (NON-RIGID),10X,I8,/40X,35HNUMBER OF RIGID ELEMENTS PROCESSED*,5X,I8,/40X,               &
                &35HNUMBER OF MPC  EQUATIONS PROCESSED*,5X,I8)
!
         WRITE (nout,99006) ncm , mm , mindeg
99006    FORMAT (40X,20HNUMBER OF COMPONENTS,20X,I8,/40X,20HMAXIMUM NODAL DEGREE,20X,I8,/40X,20HMINIMUM NODAL DEGREE,20X,I8)
!
         nonz = 2*nedge + nn
         an = nn*nn
         den = float(nonz)*100./an
         WRITE (nout,99007) nedge , den , nzero , kore
99007    FORMAT (40X,22HNUMBER OF UNIQUE EDGES,18X,I8,/40X,23HMATRIX DENSITY, PERCENT,16X,F9.3,/40X,                                &
                &31HNUMBER OF POINTS OF ZERO DEGREE,9X,I8,/40X,16HBANDIT OPEN CORE,24X,I8)
!
         IF ( icrit==1 ) WRITE (nout,99008)
99008    FORMAT (40X,10HCRITERION*,25X,13HRMS WAVEFRONT)
         IF ( icrit==2 ) WRITE (nout,99009)
99009    FORMAT (40X,10HCRITERION*,29X,9HBANDWIDTH)
         IF ( icrit==3 ) WRITE (nout,99010)
99010    FORMAT (40X,10HCRITERION*,31X,7HPROFILE)
         IF ( icrit==4 ) WRITE (nout,99011)
99011    FORMAT (40X,10HCRITERION*,25X,13HMAX WAVEFRONT)
!
         IF ( method==-1 ) WRITE (nout,99012)
99012    FORMAT (40X,12HMETHOD USED*,34X,2HCM)
         IF ( method==+1 ) WRITE (nout,99013)
99013    FORMAT (40X,12HMETHOD USED*,33X,3HGPS)
         IF ( method==0 ) WRITE (nout,99014)
99014    FORMAT (40X,12HMETHOD USED*,26X,10HCM AND GPS)
!
         IF ( Jump==0 ) THEN
!
!     GENERATE SEQGP ARRAY AND OUTPUT SEQGP CARDS
!
            j = 0
            DO i = 1 , nn
               z(j+1) = Norig(i)
               z(j+2) = Ild(i)
               j = j + 2
            ENDDO
            CALL sort(0,0,2,1,z(1),j)
!
!     CHECK AGAINST ORIGINAL GRID POINT DATA, AND SEE ANY UNUSED GRIDS
!     (SUCH AS THE THIRD GRID ON CBAR CARD). IF THEY EXIST, BRING THEM
!     IN, AND RE-SORT TABLE.  (GEOM1 IS READY HERE, SEE BGRID)
!
            CALL open(*140,geom1,z(ibuf1),rd)
            nnx = nn
            IF ( nn==ngrid ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL read(*40,*40,geom1,grid,3,0,k)
         ELSE
            WRITE (nout,99024)
            WRITE (nout,99015)
99015       FORMAT (//31X,'BANDIT FINDS GRID POINT RE-SEQUENCING NOT ','NECESSARY')
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO
            CALL read(*40,*40,geom1,grid,8,0,k)
            CALL bisloc(*20,grid(1),z,2,nnx,k)
         ENDDO
 20      nn = nn + 1
         z(j+1) = grid(1)
         z(j+2) = nn
         j = j + 2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     DO THE SAME CHECK IF SCALAR POINTS ARE PRESENT
!
 40      IF ( nspts==0 ) GOTO 100
         nonz = j + 2*nspts + 2
         CALL preloc(*100,z(nonz),geom2)
         grid(1) = 5551
         grid(2) = 49
         CALL locate(*80,z(nonz),grid,k)
         spag_nextblock_1 = 3
      CASE (3)
         DO
            CALL read(*80,*80,geom2,i,1,0,k)
            CALL bisloc(*60,i,z,2,nnx,k)
         ENDDO
 60      nn = nn + 1
         z(j+1) = i
         z(j+2) = nn
         j = j + 2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 80      CALL close(geom2,rew)
 100     i = nn - nnx
         IF ( i>0 ) WRITE (nout,99016) i
99016    FORMAT (40X,29HNO. OF NON-ACTIVE GRID POINTS,11X,I8)
         spag_nextblock_1 = 4
      CASE (4)
         i = (j+7)/8
         WRITE (nout,99017) i
99017    FORMAT (40X,28HNO. OF SEQGP CARDS GENERATED,12X,I8)
         WRITE (nout,99024)
         IF ( nopch==+9 ) THEN
!
!     SPECIAL PUNCH OPTION (BANDTPCH=+9)
!     TO PUNCH OUT EXTERNAL GRIDS IN RE-SEQUENCED INTERNAL ORDER
!
            CALL sort(0,0,2,2,z(1),j)
            WRITE (nout,99018) (z(i),i=1,j,2)
99018       FORMAT (1H1,35X,59HLIST OF EXTERNAL GRID POINTS IN INTERNAL RE-SEQUENCED ORDER,/4X,31(4H----),/,(/5X,15I8))
            WRITE (lpch,99019) (z(i),i=1,j,2)
99019       FORMAT (10I7)
            j77 = -2
            CALL close(geom1,rew)
         ELSE
            IF ( nnx/=nn ) CALL sort(0,0,2,1,z(1),j)
            IF ( iecho/=-1 ) THEN
               CALL page1
               WRITE (nout,99020)
99020          FORMAT (//35X,52HS Y S T E M  G E N E R A T E D  S E Q G P  C A R D S,/)
               WRITE (nout,99021) (z(i),i=1,j)
99021          FORMAT (25X,8HSEQGP   ,8I8)
            ENDIF
            IF ( nopch<=0 ) THEN
!
!     BEEF UP INTERNAL GRID NOS. BY 1000 AS REQUIRED BY NASTRAN
!
               DO i = 2 , j , 2
                  z(i) = z(i)*1000
               ENDDO
!
!     REWIND AND SKIP FORWARDS TO THE END OF GEOM1 FILE.
!     OVERWRITE THE OLD SEQGP RECORD IF NECESSARY.
!     (WARNING - IF SEQGP IS NOT THE VERY LAST ITEM IN GEOM1 FILE, THE
!      FOLLOWING LOGIC OF INSERTING SEQGP CARDS NEEDS MODIFICATION -
!      BECAUSE GEOM1 IS IN ALPHA-NUMERIC SORTED ORDER).
!
               CALL rewind(geom1)
               CALL skpfil(geom1,+1)
               CALL skpfil(geom1,-1)
               CALL bckrec(geom1)
               CALL read(*120,*120,geom1,Norig(1),3,1,i)
               IF ( Norig(1)==seqgp(1) .AND. Norig(2)==seqgp(2) ) CALL bckrec(geom1)
               CALL close(geom1,norew)
!
!     ADD SEQGP CARDS TO THE END OF GEOM1 FILE
!     SET GEOM1 TRAILER, AND CLEAR /SYSTEM/ 76TH WORD
!
               CALL open(*140,geom1,z(ibuf1),wrt)
               CALL write(geom1,seqgp(1),3,0)
               CALL write(geom1,z(1),j,1)
               CALL write(geom1,eof(1),3,1)
!
               z(1) = geom1
               CALL rdtrl(z(1))
               i = (seqgp(2)+31)/16
               j = seqgp(2) - i*16 + 48
               z(i) = orf(z(i),two(j))
               CALL wrttrl(z(1))
               CALL close(geom1,rew)
            ELSE
               WRITE (lpch,99022) (z(i),i=1,j)
99022          FORMAT (8HSEQGP   ,8I8)
               j77 = -2
               CALL close(geom1,rew)
            ENDIF
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         DO i = 1 , kore
            z(i) = 0
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
         isys(i77) = j77
         IF ( ngrd<0 ) RETURN
         CALL page2(-2)
         WRITE (nout,99023)
99023    FORMAT (1H0,9X,45H**NO ERRORS FOUND - EXECUTE NASTRAN PROGRAM**)
         RETURN
!
!     FILE ERROR
!
 120     k = -2
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 140     k = -1
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(k,geom1,sub)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99024 FORMAT (/31X,'(* THESE DEFAULT OPTIONS CAN BE OVERRIDDEN BY THE',' NASTRAN CARD)')
END SUBROUTINE bseqgp
