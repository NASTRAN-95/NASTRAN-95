
SUBROUTINE dzymat(D,Nfb,Nlb,Ntzys,Idzdy,Ntape,Xp,Beta,Iprnt,Ns,Nc,Yp,Zp,Sg,Cg,Yb,Zb,Nbea)
   IMPLICIT NONE
   REAL Ecore , Fmach , Refc , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sysbuf , Z(1)
   INTEGER Ia0 , Ia0p , Iarb , Iavr , Icg , Ics , Idelx , Iee , Ifla1 , Ifla2 , Inas , Inasb , Inb , Inbea1 , Inbea2 , Inc , Infl , &
         & Ins , Insbea , Int121 , Int122 , Iria , Isg , Ith1a , Ith2a , Ix , Ixic , Ixij , Ixis1 , Ixis2 , Ixlam , Ixle , Ixte ,   &
         & Iyb , Iyin , Iys , Izb , Izin , Izs , Kr , Mcb(7) , Nb , Nby , Nbz , Nd , Ne , Next , Nj1 , Nk1 , Np , Npot , Nrow ,     &
         & Nt0 , Ntp , Nty , Ntys , Ntz , Ntzs
   COMMON /amgmn / Mcb , Nrow , Nd , Ne , Refc , Fmach , Kr
   COMMON /dlbdy / Nj1 , Nk1 , Np , Nb , Ntp , Nbz , Nby , Ntz , Nty , Nt0 , Ntzs , Ntys , Inc , Ins , Inb , Inas , Izin , Iyin ,   &
                 & Inbea1 , Inbea2 , Insbea , Izb , Iyb , Iavr , Iarb , Infl , Ixle , Ixte , Int121 , Int122 , Izs , Iys , Ics ,    &
                 & Iee , Isg , Icg , Ixij , Ix , Idelx , Ixic , Ixlam , Ia0 , Ixis1 , Ixis2 , Ia0p , Iria , Inasb , Ifla1 , Ifla2 , &
                 & Ith1a , Ith2a , Ecore , Next , Scr1 , Scr2 , Scr3 , Scr4 , Scr5
   COMMON /system/ Sysbuf , Npot
   COMMON /zzzzzz/ Z
   REAL Beta
   INTEGER Idzdy , Iprnt , Nfb , Nlb , Ntape , Ntzys
   REAL Cg(1) , D(2,Ntzys) , Sg(1) , Xp(1) , Yb(1) , Yp(1) , Zb(1) , Zp(1)
   INTEGER Nbea(1) , Nc(1) , Ns(1)
   INTEGER by , bz , c , c1 , i , isn , ixp , nbey , nbez , ncp , nfyb , nfybm1 , nsp , p , s , s1 , yt , zt
   REAL cgr , dx , dy , dz , sgr
!
!     CALCULATION OF DZ AND DY MATRICES SLENDER BODY CALCULATIONS
!
!     D         WORKING ARRAY USED TO STORE A ROW OF DZ OR DY
!     NFB       NUMBER OF THE FIRST BODY WITH THE ORIENTATION REQUESTED
!     NLB       NUMBER OF THE LAST BODY WITH THE ORIENTATION
!               REQUESTED
!     NTZYS     NUMBER OF Z OR Y ORIENTED SLENDER BODY ELE.
!     NTAPE     I/O UNIT NUMBER WHICH THE OUTPUT MATRIX IS TO
!               BE WRITTEN ON
!     XP        X-CONTROL POINT COORDINATE OF LIFTING SURFACE
!               BOXES
!     BETA      SQRT(1.0 - M**2)
!
!
   c1 = 0
   s1 = 0
   nfyb = Nb - Nby + 1
   IF ( Np/=0 ) THEN
!
!     THIS LOOP IS FOR EACH LIFTING SURF. PANEL
!
      isn = 0
      DO p = 1 , Np
         nsp = Ns(p)
         ncp = Nc(p)
         nsp = (nsp-isn)/ncp
         isn = Ns(p)
!
!     LOOP FOR EACH STRIP IN PANEL -P-
!
         DO s = 1 , nsp
            s1 = s1 + 1
!
!     Y AND Z COORDINATE OF STRIP
!
            dy = Yp(s1)
            dz = Zp(s1)
            sgr = Sg(s1)
            cgr = Cg(s1)
!
!     LOOP FOR EACH CHORDWISE ELEMENT IN STRIP
!
            DO c = 1 , ncp
               c1 = c1 + 1
               dx = Xp(c1)
!
!     - ROWDYC -  CALCULATES ROW -C1- OF DZ OR DY
!
               CALL rowdyz(Nfb,Nlb,c1,Ntzys,D,dx,dy,dz,Beta,Idzdy,Ntape,sgr,cgr,Iprnt,Yb,Zb,Z(Iarb),Z(Insbea),Z(Ixis1),Z(Ixis2),    &
                         & Z(Ia0))
!
            ENDDO
         ENDDO
      ENDDO
   ENDIF
!
!     WE HAVE NOW CALCULATED -C1- ROWS WHICH ARE THE LIFTING SURFACES.
!     NOW, LOOP FOR THE -Z- ORIENTED BODIES
!
   IF ( Nbz>0 .AND. Ntz>0 ) THEN
      sgr = 0.0
      cgr = 1.0
      DO bz = 1 , Nbz
         dy = Yb(bz)
         dz = Zb(bz)
         nbez = Nbea(bz)
!
!     LOOP FOR EACH ELEMENT OF BODY -BZ-
!
         DO zt = 1 , nbez
            c1 = c1 + 1
            dx = Xp(c1)
!
            CALL rowdyz(Nfb,Nlb,c1,Ntzys,D,dx,dy,dz,Beta,Idzdy,Ntape,sgr,cgr,Iprnt,Yb,Zb,Z(Iarb),Z(Insbea),Z(Ixis1),Z(Ixis2),Z(Ia0))
         ENDDO
      ENDDO
   ENDIF
!
!     NOW, LOOP FOR THE -Y- ORIENTED BODIES
!
   IF ( Nb>=nfyb .AND. Nty>0 ) THEN
      ixp = Ntp
      IF ( nfyb>1 ) THEN
         nfybm1 = nfyb - 1
         DO i = 1 , nfybm1
            ixp = ixp + Nbea(i)
         ENDDO
      ENDIF
      sgr = -1.0
      cgr = 0.0
      DO by = nfyb , Nb
         dy = Yb(by)
         dz = Zb(by)
         nbey = Nbea(by)
!
!     LOOP FOR EACH ELEMENT OF BODY -BY-
!
         DO yt = 1 , nbey
            c1 = c1 + 1
            ixp = ixp + 1
            dx = Xp(ixp)
!
            CALL rowdyz(Nfb,Nlb,c1,Ntzys,D,dx,dy,dz,Beta,Idzdy,Ntape,sgr,cgr,Iprnt,Yb,Zb,Z(Iarb),Z(Insbea),Z(Ixis1),Z(Ixis2),Z(Ia0))
!
         ENDDO
      ENDDO
   ENDIF
END SUBROUTINE dzymat
