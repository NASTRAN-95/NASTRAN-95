!*==mpy3oc.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mpy3oc(Z,Iz,Dz)
   IMPLICIT NONE
   USE c_mpy3cp
   USE c_mpy3tl
   USE c_mpyadx
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   REAL*8 , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf5 , file , i , iakj , iantu , ibcid , ibcols , ibntu , ic , iktbp , ilast , intbu , intbu2 , isavp , j1 , j2 ,     &
            & jbck , jfwd , jj , jj1 , kan , kbc , kbn , kc , kf , kk , kl , kn , kn2 , kt , mm , nakj , nantu , nbcid , nbcols ,   &
            & nbntu , nc , nerr , nktbp , nlast , nntbu , nntbu2 , nsavp , precm
   LOGICAL :: first3
   INTEGER , DIMENSION(2) , SAVE :: name , nams
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     OUT-OF-CORE PRODUCT.
!
!
!     MPYAD COMMON
!
!     FILES
!
!     SUBROUTINE CALL PARAMETERS
!
!     PACK
!
!     UNPACK
!
!     TERMWISE MATRIX READ
!
!     SYSTEM PARAMETERS
   !>>>>EQUIVALENCE (Isavp,Zpntrs(1)) , (Nsavp,Zpntrs(2)) , (Intbu,Zpntrs(3)) , (Nntbu,Zpntrs(4)) , (Ilast,Zpntrs(5)) , (Nlast,Zpntrs(6))&
!>>>>    & , (Intbu2,Zpntrs(7)) , (Nntbu2,Zpntrs(8)) , (Ic,Zpntrs(9)) , (Nc,Zpntrs(10)) , (Ibcols,Zpntrs(11)) , (Nbcols,Zpntrs(12)) ,    &
!>>>>    & (Ibcid,Zpntrs(13)) , (Nbcid,Zpntrs(14)) , (Ibntu,Zpntrs(15)) , (Nbntu,Zpntrs(16)) , (Iktbp,Zpntrs(17)) , (Nktbp,Zpntrs(18)) , &
!>>>>    & (Iantu,Zpntrs(19)) , (Nantu,Zpntrs(20)) , (Iakj,Zpntrs(21)) , (Nakj,Zpntrs(22)) , (A(1),Da)
   DATA name/4HMPY3 , 4HOC  /
   DATA nams/4HSCR3 , 4H    /
!
!     RECALCULATION OF NUMBER OF COLUMNS OF B ABLE TO BE PUT IN CORE.
!
   buf5 = buf4 - sysbuf
   lcore = buf5 - 1
   nk = (lcore-4*n-prec*m-(2+prec)*maxa)/(2+prec*n)
   IF ( nk<1 ) THEN
      nerr = -8
      file = 0
      CALL mesage(nerr,file,name)
   ELSE
!
!    INITIALIZATION.
!
      first1 = .TRUE.
      first2 = .TRUE.
      first3 = .FALSE.
      precm = prec*m
!
!     OPEN CORE POINTERS
!
      isavp = 1
      nsavp = ncb
      intbu = nsavp + 1
      nntbu = nsavp + ncb
      ilast = nntbu + 1
      nlast = nntbu + ncb
      intbu2 = nlast + 1
      nntbu2 = nlast + ncb
      ic = nntbu2 + 1
      nc = nntbu2 + prec*m
      ibcols = nc + 1
      nbcols = nc + prec*n*nk
      ibcid = nbcols + 1
      nbcid = nbcols + nk
      ibntu = nbcid + 1
      nbntu = nbcid + nk
      iktbp = nbntu + 1
      nktbp = nbntu + maxa
      iantu = nktbp + 1
      nantu = nktbp + maxa
      iakj = nantu + 1
      nakj = nantu + prec*maxa
      kf = nsavp
      kl = nntbu
      kn2 = nlast
      kbc = nbcols
      kbn = nbcid
      kt = nbntu
      kan = nktbp
!
!     PACK PARAMETERS
!
      typin = prec
      typout = prec
      row1 = 1
      incr = 1
!
!     UNPACK PARAMETERS
!
      utyp = prec
      urow1 = 1
      uincr = 1
!
!     MATRIX TRAILERS
!
      CALL makmcb(scr3,scr3(1),n,2,prec)
      IF ( m==n ) scr3(4) = 1
!
!     PUT B ONTO SCRATCH FILE IN UNPACKED FORM.
!
      CALL mpy3a(Z,Z,Z)
!
!     OPEN FILES AND CHECK EXISTENCE OF MATRIX E.
!
      IF ( .NOT.(code==0 .OR. .NOT.e) ) THEN
         file = filee(1)
         CALL open(*200,filee,Z(buf5),2)
         CALL fwdrec(*300,filee)
      ENDIF
      file = filea(1)
      CALL open(*200,filea,Z(buf1),0)
      CALL fwdrec(*300,filea)
      file = scr1
      CALL open(*200,scr1,Z(buf2),0)
      file = scr2
      CALL open(*200,scr2,Z(buf3),1)
      IF ( code==0 ) THEN
         file = scr3(1)
         CALL open(*200,scr3,Z(buf4),1)
         CALL write(scr3,nams,2,1)
         rowm = scr3(3)
      ELSE
         file = filec(1)
         CALL gopen(filec,Z(buf4),1)
         rowm = filec(3)
      ENDIF
!
!     PROCESS SCR2 AND SET FIRST-TIME-USED AND LAST-TIME-USED FOR EACH
!     ROW OF A.
!
      DO k = 1 , ncb
         Iz(kf+k) = 0
         Iz(kl+k) = 0
      ENDDO
      DO j = 1 , m
         k = 0
         CALL intpk(*20,filea,0,prec,0)
         DO
            CALL zntpki
            k = k + 1
            Iz(kt+k) = irow
            IF ( Iz(kf+irow)<=0 ) Iz(kf+irow) = j
            Iz(kl+irow) = j
            IF ( eol==1 ) THEN
               CALL write(scr2,Iz(iktbp),k,0)
               EXIT
            ENDIF
         ENDDO
 20      CALL write(scr2,0,0,1)
      ENDDO
      CALL close(filea,1)
      CALL open(*200,filea,Z(buf1),2)
      CALL fwdrec(*300,filea)
      CALL close(scr2,1)
      CALL open(*200,scr2,Z(buf3),0)
!
!     PROCESS COLUMNS OF A ONE AT A TIME.
!
      DO j = 1 , m
!
!     INITIALIZE SUM - ACCUMULATION MATRIX TO 0.
!
         DO i = ic , nc
            Z(i) = 0.
         ENDDO
         IF ( .NOT.(code==0 .OR. .NOT.e) ) THEN
            urown = n
            CALL unpack(*40,filee,Z(ic))
         ENDIF
!
!     PROCESS A AND PERFORM FIRST PART OF PRODUCT BA(J).
!
 40      CALL mpy3b(Z,Z,Z)
!
!     TEST IF PROCESSING IS COMPLETE
!
         IF ( iflag==0 ) GOTO 160
!
!     PROCESS REMAINING TERMS OF COLUMN J OF A.
!
!     TEST IF BCOLS IS FULL
!
 60      IF ( k2<nk ) GOTO 140
!
!     CALCULATE NEW NEXT TIME USED VALUES
!
         IF ( .NOT.(first3) ) THEN
            first2 = .FALSE.
            first3 = .TRUE.
            DO jj = 1 , j
               CALL fwdrec(*300,scr2)
            ENDDO
         ENDIF
         file = scr2
         kc = 0
         kn = kf
         DO ka = 1 , ncb
            kn = kn + 1
            IF ( j<Iz(kn) ) THEN
               kc = kc + 1
               IF ( j+1<Iz(kn) ) THEN
                  Iz(kn2+ka) = Iz(kn)
                  kc = kc + 1
                  CYCLE
               ELSEIF ( j+1>=Iz(kl+ka) ) THEN
                  Iz(kn2+ka) = 99999999
                  kc = kc + 1
                  CYCLE
               ENDIF
            ELSEIF ( j<Iz(kl+ka) ) THEN
               Iz(kn) = 0
            ELSE
               Iz(kn) = 99999999
               Iz(kn2+ka) = Iz(kn)
               kc = kc + 2
               CYCLE
            ENDIF
            Iz(kn2+ka) = 0
         ENDDO
         IF ( kc==2*ncb ) THEN
            IF ( j/=m ) CALL fwdrec(*300,scr2)
            GOTO 120
         ELSE
            jj = j + 1
         ENDIF
 80      DO
            CALL read(*300,*100,scr2,ka,1,0,kk)
            IF ( Iz(kn2+ka)<=0 ) THEN
               IF ( jj/=j+1 ) THEN
                  Iz(kn2+ka) = jj
                  kc = kc + 1
               ENDIF
               IF ( Iz(kf+ka)<=0 ) THEN
                  Iz(kf+ka) = jj
                  kc = kc + 1
               ENDIF
               IF ( kc==2*ncb ) THEN
                  mm = m - 1
                  IF ( j/=mm ) THEN
!
!     POSITION SCRATCH FILE FOR NEXT PASS THROUGH
!
                     jj = jj - j
                     j2 = j + 2
                     jj1 = jj - 1
                     IF ( j2<jj1 ) THEN
                        CALL rewind(scr2)
                        j1 = j + 1
                        DO jfwd = 1 , j1
                           CALL fwdrec(*300,scr2)
                        ENDDO
                     ELSEIF ( jj1>0 ) THEN
                        DO jbck = 1 , jj1
                           CALL bckrec(scr2)
                        ENDDO
                     ELSE
                        CALL fwdrec(*300,scr2)
                     ENDIF
                  ENDIF
                  GOTO 120
               ENDIF
            ENDIF
         ENDDO
 100     jj = jj + 1
         GOTO 80
!
!     ASSIGN NEXT TIME USED TO COLUMNS OF B IN CORE
!
 120     DO kk = 1 , nk
            i = Iz(kbc+kk)
            Iz(kbn+kk) = Iz(kf+i)
         ENDDO
!
!     ASSIGN NEXT TIME USED TO NON-ZERO TERMS IN COLUMN OF A
!
         DO kk = 1 , k
            IF ( Iz(kt+kk)==0 ) THEN
               Iz(kan+kk) = 0
            ELSE
               i = Iz(kt+kk)
               Iz(kan+kk) = Iz(kf+i)
            ENDIF
         ENDDO
 140     DO
!
!     PERFORM MULTIPLICATION AND SUMMATION FOR NEXT TERM OF COLUMN OF A
!
            CALL mpy3c(Z,Z,Z)
!
!     TEST IF PROCESSING OF BA(J) IS COMPLETE
!
            IF ( kcount==k ) EXIT
            IF ( first2 ) GOTO 60
            Iz(kbn+ltbc) = Iz(kn2+ltac)
         ENDDO
!
!     PACK COLUMN OF C OR BA.
!
 160     IF ( code==0 ) THEN
            CALL pack(Z(ic),scr3,scr3)
         ELSE
            CALL pack(Z(ic),filec,filec)
         ENDIF
      ENDDO
!
!     CLOSE FILES.
!
      CALL close(filea,2)
      CALL close(scr1,1)
      CALL close(scr2,1)
      IF ( e ) CALL close(filee,2)
      IF ( code==0 ) THEN
         CALL close(scr3,1)
         CALL wrttrl(scr3)
!
!     CALL MPYAD TO FINISH PRODUCT
!
         DO i = 1 , 7
            mfilea(i) = filea(i)
            mfileb(i) = scr3(i)
            mfilee(i) = filee(i)
            mfilec(i) = filec(i)
         ENDDO
         mt = 1
         signab = 1
         signc = 1
         mprec = prec
         mscr = scr1
         CALL mpyad(Z,Z,Z)
      ELSE
         CALL close(filec,1)
      ENDIF
   ENDIF
   GOTO 99999
!
!     ERROR MESSAGES.
!
 200  nerr = -1
   CALL mesage(nerr,file,name)
   GOTO 99999
 300  nerr = -2
   CALL mesage(nerr,file,name)
!
99999 END SUBROUTINE mpy3oc
