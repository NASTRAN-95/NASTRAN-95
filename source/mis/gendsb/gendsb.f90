!*==gendsb.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gendsb(Ncaray,Nbaray,Sg,Cg,Nfl,Nbea1,Nbea2,Ifla1,Ifla2,Dt,Dpz,Dpy)
   USE c_amgmn
   USE c_dlbdy
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ncaray
   INTEGER , DIMENSION(1) :: Nbaray
   REAL , DIMENSION(1) :: Sg
   REAL , DIMENSION(1) :: Cg
   INTEGER , DIMENSION(1) :: Nfl
   INTEGER , DIMENSION(1) :: Nbea1
   INTEGER , DIMENSION(1) :: Nbea2
   INTEGER , DIMENSION(1) :: Ifla1
   INTEGER , DIMENSION(1) :: Ifla2
   COMPLEX , DIMENSION(1) :: Dt
   COMPLEX , DIMENSION(1) :: Dpz
   COMPLEX , DIMENSION(1) :: Dpy
!
! Local variable declarations rewritten by SPAG
!
   REAL :: cgr , sgr
   INTEGER :: i , i1 , i2 , ibuf1 , ibuf2 , ibuf3 , ibuf4 , icount , ifirst , ifl , ilast , itape , iz , j , j1 , j2 , jbo , k ,    &
            & kb , ks , kt , lbo , ls , lso , lsx , nbox , nbuf , nbxr , nlt1 , nlt2 , nstrip , nyflag , nzykb , nzysv
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , dmpfil , dppsb , dpzy , dypz , dzpy , fread , gopen , mesage , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA name/4HGEND , 4HB   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!   ***   GENERATES THE INFLUENCE COEFFICIENT MATRIX  DT   USING THE
!         FOLLOWING FOUR SUBROUTINES  --  DPPS, DPZY, DZPY,  AND  DYPZ
         nbox = ntp
         lbo = 1
         lso = 1
         jbo = 1
         kb = 0
         kt = 0
         DO i = 1 , ntbe
            Dpy(i) = (0.0,0.0)
            Dt(i) = (0.0,0.0)
         ENDDO
         nbuf = 4
         IF ( ntp==0 ) nbuf = nbuf - 1
         IF ( ntz==0 ) nbuf = nbuf - 1
         IF ( nty==0 ) nbuf = nbuf - 2
         IF ( next+nbuf*sysbuf>ecore ) CALL mesage(-8,0,name)
         ibuf1 = ecore - sysbuf
         ibuf2 = ibuf1 - sysbuf
         nstrip = 0
         j2 = 0
         i2 = 0
         nyflag = 0
         IF ( ntp/=0 ) CALL gopen(scr1,z(ibuf1),1)
         IF ( ntp==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         i1 = 1
         i2 = ntp
         j1 = 1
         j2 = ntp
!  DPP-LOOP
         k = 1
!  K IS THE PANEL NUMBER ASSOCIATED WITH RECEIVING POINT  I
         ks = 1
!  KS IS THE STRIP NUMBER ASSOCIATED WITH RECEIVING POINT  I
         nbxr = Ncaray(k)
         DO i = i1 , i2
            sgr = Sg(ks)
            cgr = Cg(ks)
            CALL dppsb(ks,i,j1,j2,sgr,cgr,z(iys),z(izs),Nbaray,Ncaray,Dt,z(1))
            CALL write(scr1,Dt,2*ntp,0)
            IF ( i/=i2 ) THEN
               IF ( i==Nbaray(k) ) k = k + 1
               IF ( i==nbxr ) THEN
                  ks = ks + 1
                  nbxr = nbxr + Ncaray(k)
               ENDIF
            ENDIF
         ENDDO
         CALL write(scr1,0,0,1)
         nstrip = ks
         nzysv = 0
         DO j = j1 , j2
            Dt(j) = (0.0,0.0)
         ENDDO
         nlt1 = 0
         nlt2 = 0
         IF ( ntz==0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nty/=0 ) CALL gopen(scr4,z(ibuf2),1)
         i1 = i2 + 1
         i2 = i2 + ntz
         spag_nextblock_1 = 2
      CASE (2)
!  DPZ-LOOP    **    ALSO USED FOR GENERATING THE DPY-MATRIX  --  SEE
!  COMMENT IN  DPY-LOOP  BELOW
         kb = kb + 1
!  KB  IS THE BODY NUMBER ASSOCIATED WITH RECEIVING POINT  I
         iz = 0
         kt = kt + 1
!  KT  IS THE INDEX OF THE ARRAY OF FIRST-AND-LAST-ELEMENTS FOR THETA-1
         icount = 1
         ifl = Nfl(kb)
         nzykb = Nbea2(kb)
         ifirst = Ifla1(kt)
         ilast = Ifla2(kt)
         SPAG_Loop_1_1: DO i = i1 , i2
            DO j = j1 , j2
               Dpz(j) = (0.0,0.0)
               Dpy(j) = (0.0,0.0)
            ENDDO
            CALL dpzy(kb,iz,i,j1,j2,ifirst,ilast,z(iyb),z(izb),z(iavr),z(iarb),z(ith1a+nlt1),z(ith2a+nlt2),z(int121),z(int122),     &
                    & Nbaray,Ncaray,nzykb,Dpz,Dpy)
            IF ( nzykb==3 ) THEN
               CALL write(scr4,Dpy,2*ntp,0)
            ELSE
               CALL write(scr1,Dpz,2*ntp,0)
               IF ( nzykb/=1 ) CALL write(scr4,Dpy,2*ntp,0)
            ENDIF
            IF ( iz==Nbea1(kb) ) THEN
               iz = 0
               IF ( nzysv<=1 .AND. nzykb>=2 ) THEN
                  lbo = kb
                  lso = nstrip + lbo
                  jbo = i - Nbea1(kb) - nbox + 1
               ENDIF
               nzysv = nzykb
               IF ( i==i2 ) EXIT SPAG_Loop_1_1
               kb = kb + 1
               icount = 0
               ifl = Nfl(kb)
               nzykb = Nbea2(kb)
            ELSEIF ( iz/=ilast .OR. icount>=ifl ) THEN
               CYCLE
            ENDIF
            kt = kt + 1
            icount = icount + 1
            ifirst = Ifla1(kt)
            ilast = Ifla2(kt)
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         IF ( i2==ntbe ) THEN
            CALL write(scr1,0,0,1)
            IF ( nty/=0 ) CALL write(scr4,0,0,1)
            CALL close(scr1,1)
            CALL close(scr4,1)
            i1 = 1
            i2 = ntp
            IF ( ntz==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL gopen(scr2,z(ibuf1),1)
!  DZP-LOOP
            k = 1
!  K  IS THE PANEL NUMBER ASSOCIATED WITH RECEIVING POINT  I
            ks = 1
!  KS  IS THE STRIP NUMBER ASSOCIATED WITH RECEIVING POINT  I
            nbxr = Ncaray(k)
            kb = 0
!  HERE  KB=0  SERVES AS A FLAG INDICATING THAT THE RECEIVING POINT   I
!  IS ON A PANEL AND NOT   ON A BODY
            j1 = j2 + 1
            j2 = j2 + ntz
            DO i = i1 , i2
               ls = nstrip + 1
               sgr = Sg(ks)
               cgr = Cg(ks)
               CALL dzpy(kb,ks,ls,i,j1,j2,nyflag,sgr,cgr,fmach,z(iarb),z(inbea1),Dt)
               CALL write(scr2,Dt(j1),2*ntz,0)
               IF ( i/=i2 ) THEN
                  IF ( i==Nbaray(k) ) k = k + 1
                  IF ( i==nbxr ) THEN
                     ks = ks + 1
                     nbxr = nbxr + Ncaray(k)
                  ENDIF
               ENDIF
            ENDDO
            CALL write(scr2,0,0,1)
         ELSE
!  DPY-LOOP    **    THIS LOOP IS REDUCED TO SETTING THE CORRECT INDICES
!  AND USING THE  DPZ-LOOP  ABOVE
            IF ( ntz==0 ) CALL gopen(scr4,z(ibuf2),1)
            i1 = i2 + 1
            i2 = ntbe
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( ntz/=0 ) THEN
            IF ( ntp==0 ) CALL gopen(scr2,z(ibuf1),1)
            nyflag = 0
!  DZZ-LOOP    **    ALSO USED FOR GENERATING THE  DZY  MATRIX  --  SEE
!  COMMENT IN  DZY-LOOP  BELOW
            kb = 1
!  KB  IS THE BODY NUMBER ASSOCIATED WITH RECEIVING POINT  I
            ks = nstrip + 1
            iz = 0
            i1 = i2 + 1
            i2 = i2 + ntz
            sgr = 0.0
            cgr = 1.0
            SPAG_Loop_1_2: DO
               ls = nstrip + 1
               lsx = ls
               DO i = i1 , i2
                  ls = lsx
                  iz = iz + 1
!  KS IS THE INDEX OF THE Y  AND  Z  COORDINATES OF RECEIVING POINT I
!  IN THE  DZZ-LOOP  KS RUNS FROM  (NSTRIP+1)  THROUGH  (NSTRIP+NBZ)
!  IN THE  DZY-LOOP  KS  RUNS FROM  (NSTRIP+NB-NBY+1) THROUGH  NSTRIP+NB
                  CALL dzpy(kb,ks,ls,i,j1,j2,nyflag,sgr,cgr,fmach,z(iarb),z(inbea1),Dt)
                  CALL write(scr2,Dt(j1),2*ntz,0)
                  IF ( iz==Nbea1(kb) ) THEN
                     iz = 0
                     kb = kb + 1
                     ks = ks + 1
                  ENDIF
               ENDDO
               CALL write(scr2,0,0,1)
               IF ( nty==0 ) CALL close(scr2,1)
               IF ( nty==0 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( nyflag/=0 ) EXIT SPAG_Loop_1_2
!  DZY-LOOP    **    THIS LOOP IS REDUCED TO SETTING THE CORRECT INDICES
!  AND USING THE  DZZ-LOOP  ABOVE
               i1 = ntbe - nty + 1
               i2 = ntbe
               nyflag = 1
               kb = lbo
               ks = lso
               sgr = -1.0
               cgr = 0.0
            ENDDO SPAG_Loop_1_2
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL close(scr2,1)
         IF ( nty==0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(scr3,z(ibuf1),1)
         i1 = 1
         i2 = ntp
         j1 = ntbe - nty + 1
         j2 = ntbe
         IF ( ntp/=0 ) THEN
!  DYP-LOOP
            k = 1
            ks = 1
            kb = 0
            nbxr = Ncaray(k)
            sgr = Sg(ks)
            cgr = Cg(ks)
            DO i = i1 , i2
               CALL dypz(kb,ks,ls,i,j1,j2,nyflag,sgr,cgr,fmach,z(iarb),z(inbea1),lbo,lso,jbo,Dt)
               CALL write(scr3,Dt(j1),2*nty,0)
               IF ( i==Nbaray(k) ) k = k + 1
               IF ( i==nbxr ) THEN
                  ks = ks + 1
                  nbxr = nbxr + Ncaray(k)
                  sgr = Sg(ks)
                  cgr = Cg(ks)
               ENDIF
            ENDDO
            CALL write(scr3,0,0,1)
         ENDIF
         nyflag = 0
         iz = 0
         IF ( ntz==0 ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!  DYZ-LOOP    **    ALSO USED FOR GENERATING THE  DYY  MATRIX  --  SEE
!  COMMENT IN  DYY-LOOP  BELOW
         i1 = i2 + 1
         i2 = i2 + ntz
         ks = nstrip + 1
         kb = 1
         sgr = 0.0
         cgr = 1.0
         spag_nextblock_1 = 6
      CASE (6)
         DO i = i1 , i2
            ls = lso
            iz = iz + 1
            CALL dypz(kb,ks,ls,i,j1,j2,nyflag,sgr,cgr,fmach,z(iarb),z(inbea1),lbo,lso,jbo,Dt)
            CALL write(scr3,Dt(j1),2*nty,0)
            IF ( iz==Nbea1(kb) ) THEN
               iz = 0
               kb = kb + 1
               ks = ks + 1
            ENDIF
         ENDDO
         CALL write(scr3,0,0,1)
         spag_nextblock_1 = 7
      CASE (7)
         IF ( nyflag==0 ) THEN
!  DYY-LOOP    **    THIS LOOP IS REDUCED TO SETTING THE CORRECT INDICES
!  AND USING THE  DYZ-LOOP  ABOVE
            IF ( ntp==0 .AND. ntz==0 ) CALL gopen(scr3,z(ibuf1),1)
            i1 = ntbe - nty + 1
            i2 = ntbe
            nyflag = 1
            kb = lbo
            ks = lso
            sgr = -1.0
            cgr = 0.0
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         CALL close(scr3,1)
!
!     BUILD SCR5 WITH GEND PART OF A MATRIX
!
         i1 = 1
         i2 = ntp + ntz
         nyflag = 0
         CALL gopen(scr5,z(ibuf1),1)
         ibuf3 = ibuf2 - sysbuf
         ibuf4 = ibuf3 - sysbuf
         IF ( ntz/=0 ) CALL gopen(scr2,z(ibuf3),0)
         IF ( nty/=0 ) CALL gopen(scr3,z(ibuf4),0)
         itape = scr1
         IF ( i2==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         IF ( ntp/=0 ) CALL gopen(itape,z(ibuf2),0)
         DO i = i1 , i2
            j1 = 1
            j2 = ntp
            IF ( ntp/=0 ) CALL fread(itape,Dt,2*j2,0)
            IF ( i==ntp ) CALL fread(itape,0,0,1)
            IF ( ntz/=0 ) THEN
               j1 = j2 + 1
               j2 = j2 + ntz
               CALL fread(scr2,Dt(j1),2*ntz,0)
               IF ( i==ntp ) CALL fread(scr2,0,0,1)
            ENDIF
            IF ( nty/=0 ) THEN
               j1 = j2 + 1
               j2 = j2 + nty
               CALL fread(scr3,Dt(j1),2*nty,0)
               IF ( i==ntp ) CALL fread(scr3,0,0,1)
            ENDIF
            CALL write(scr5,Dt,2*j2,0)
         ENDDO
         IF ( nty==0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( nyflag/=0 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ntz/=0 .AND. ntp/=0 ) CALL fread(scr2,0,0,1)
         IF ( nty/=0 .AND. ntp/=0 ) CALL fread(scr3,0,0,1)
         CALL close(itape,1)
         spag_nextblock_1 = 10
      CASE (10)
         nyflag = 1
         i1 = i2 + 1
         i2 = i2 + nty
         itape = scr4
         spag_nextblock_1 = 9
      CASE (11)
         CALL write(scr5,0,0,1)
         CALL close(scr1,1)
         CALL close(scr2,1)
         CALL close(scr3,1)
         CALL close(scr4,1)
         CALL close(scr5,1)
         CALL dmpfil(scr5,z(next),ecore-next-100)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gendsb
