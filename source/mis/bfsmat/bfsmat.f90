!*==bfsmat.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bfsmat(Nd,Ne,Nb,Np,Ntp,Length,Ntotal,Scr1,Jf,Jl,Nas,Fmach,Yb,Zb,Ys,Zs,X,Delx,Ee,Xic,Sg,Cg,Ar,Ria,Nbea1,Nbea2,Nasb,Nsaray,&
                & Ncaray,Bfs,Avr,Cbar,A0,Xis1,Xis2,Kr,Nsbea,Nt0)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Length
   INTEGER :: Nd
   INTEGER :: Ne
   INTEGER :: Nb
   INTEGER :: Np
   INTEGER :: Ntp
   INTEGER :: Ntotal
   INTEGER :: Scr1
   INTEGER :: Jf
   INTEGER :: Jl
   INTEGER , DIMENSION(1) :: Nas
   REAL :: Fmach
   REAL , DIMENSION(1) :: Yb
   REAL , DIMENSION(1) :: Zb
   REAL , DIMENSION(1) :: Ys
   REAL , DIMENSION(1) :: Zs
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Delx
   REAL , DIMENSION(1) :: Ee
   REAL , DIMENSION(1) :: Xic
   REAL , DIMENSION(1) :: Sg
   REAL , DIMENSION(1) :: Cg
   REAL , DIMENSION(1) :: Ar
   REAL , DIMENSION(1) :: Ria
   INTEGER , DIMENSION(1) :: Nbea1
   INTEGER , DIMENSION(1) :: Nbea2
   INTEGER , DIMENSION(1) :: Nasb
   INTEGER , DIMENSION(1) :: Nsaray
   INTEGER , DIMENSION(1) :: Ncaray
   COMPLEX , DIMENSION(Length,2) :: Bfs
   REAL , DIMENSION(1) :: Avr
   REAL :: Cbar
   REAL , DIMENSION(1) :: A0
   REAL , DIMENSION(1) :: Xis1
   REAL , DIMENSION(1) :: Xis2
   REAL :: Kr
   INTEGER , DIMENSION(1) :: Nsbea
   INTEGER :: Nt0
!
! Local variable declarations rewritten by SPAG
!
   REAL :: area , beta2 , cgs , dria , dxle , dxs , dxte , dys , dzs , earg1 , earg2 , scale , scale2 , sgs , width , xaa , xeta ,  &
         & xx1 , xx2 , xxij , xyb , xzb , xzeta
   COMPLEX :: eikj1 , eikj2 , fwy , fwz
   INTEGER :: ib , ibody , ibox , icol , index , irb , irbe , irow , is , isb , isbe , isn , isp , istrip , itsb , iy , izyflg , j ,&
            & jp1 , ksp , lsb , lsbe , lsbs , msbe , nasd , nc , nrbe , ns , nsbe , nw , spag_nextblock_1
   LOGICAL :: last
   EXTERNAL fwmw , write
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     NOTE:
!     A JUMP (VIA AN ASSIGN STATEMENT) TO 200 AND A JUMP TO 1100 (ALSO
!     VIA AN ASSIGN STATEMENT), INTO THE MIDDLES OF SOME DO LOOPS, ARE
!     ACCEPTABLE ANSI 77 FORTRAN. HOWEVER, IBM COMPILER MAY COMPLAIN.
!     THIS PROBLEM IS NOW ELIMINATED (BY G.C. 9/89)
!
!
!        ND         SYMMETRY FLAG
!        NE         GROUND EFFECTS FLAG
!        NB         NUMBER OF BODIES
!        NP         NUMBER OF PANELS
!        NTP        NUMBER OF LIFTING SURFACE BOXES
!        NTOTAL     NTP + TOTAL NO. OF Y AND Z ORIENTED BODY ELEMENTS
!        LENGTH     NTOTAL + THE TOTAL NUMBER OF Z- AND Y-ORIENTED
!                   SLENDER BODY ELEMENTS
!        SCR1       FILE FOR OUTPUT
!        JF         ROW FOR FIRST ZY BODY
!        JL         ROW FOR LAST  ZY BODY
!        NAS        ARRAY CONTAINING THE NUMBER OF ASSOCIATED BODIES
!                   FOR EACH PANEL
!        FMACH      MACH NUMBER
!        YB         ARRAY OF -Y- COORDINATES OF THE BODIES
!        ZB         ARRAY OF -Z- COORDINATES OF THE BODIES
!        YS         ARRAY OF -Y- COORDINATES OF STRIPS AND BODIES
!        ZS         ARRAY OF -Z- COORDINATES OF STRIPS AND BODIES
!        X          ARRAY OF 3/4 CHORD LOCATIONS OF BOXES AND
!                            1/2 CHORD FOR BODY ELEMENTS
!        DELX       ARRAY OF LENGTHS OF BOXES AND BODY ELEMENTS
!        EE         ARRAY OF THE SEMI-WITH OF STRIPS
!        XIC        ARRAY OF 1/4 CHORD COORDINATES OF BOXES
!        SG         ARRAY OF SINE   OF STRIP DIHEDRAL ANGLE
!        CG         ARRAY OF COSINE OF STRIP DIHEDRAL ANGLE
!        AR         ARRAY OF RATIO OF MAJOR AXES OF BODIES
!        RIA        ARRAY OF RADII OF BODY ELEMENTS
!        NBEA1      ARRAY OF NUMBER OF BODY ELEMENTS PER BODY
!        NBEA2      ARRAY OF THE BODY ORIENTATION FLAGS PER BODY
!        NASB       ARRAY OF THE BODIES ASSOCIATED WITH PANELS
!        NSARAY     ARRAY OF THE NUMBER OF STRIPS PER PANEL
!        NCARAY     ARRAY OF THE NUMBER OF CHORDWISE DIV. PER PANEL
!        BFS        WORK ARRAY FOR TEMPORARY STORAGE OF THE BFS COLS.
!        AVR        ARRAY OF RADII OF BODIES
!        CBAR       REFERENCE CHORD
!        A0         ARRAY OF SLENDER BODY ELEMENT RADII
!        XIS1       ARRAY OF SLENDER BODY ELEMENT LEADING  EDGE COORD.S
!        XIS2       ARRAY OF SLENDER BODY ELEMENT TRAILING EDGE COORD.S
!        KR         REDUCED FREQUENCY
!        NSBEA      ARRAY OF THE NUMBER OF ELEMENTS PER SLENDER BODY
!
!
!
         beta2 = 1.0 - Fmach**2
         icol = 0
         ksp = 1
!
!
!     CALCULATE EACH ROW OF THE SENDING COLUMN
!
         iy = 0
         Jf = 0
         nw = Length*2
         irow = 0
!
!     --IRB-- IS THE RECEIVING BODY
!
         irb = 0
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!
!     -Y- ORIENTED BODIES AS SENDING ELEMENTS
!
 20      sgs = -1.0
         cgs = 0.0
         nasd = 0
         izyflg = 3
         ASSIGN 40 TO ibody
         spag_nextblock_1 = 2
      CASE (2)
!
!
!     *** LOOP FOR EACH INTERFERENCE BODY SENDING ELEMENT
!
         index = Ntp
!
!     --ISB-- IS THE SENDING BODY
!
         DO isb = 1 , Nb
            IF ( Nbea2(isb)/=2 ) THEN
               IF ( Nbea2(isb)/=izyflg ) THEN
                  index = index + Nbea1(isb)
                  CYCLE
               ENDIF
            ENDIF
            dys = Yb(isb)
            nsbe = Nbea1(isb)
            jp1 = 1
            last = .FALSE.
            dzs = Zb(isb)
            earg2 = 1.0
!
!     --ISBE-- IS THE ELEMENT OF THE SEND BODY
!
            DO isbe = 1 , nsbe
               earg1 = earg2
               index = index + 1
               dxs = X(index) - Delx(index)/4.0
               earg2 = Kr*Delx(index)/Cbar
!
!     CALCULATE THIS COLUMN
!
               icol = icol + 1
               eikj1 = cmplx(cos(earg1),-sin(earg1))
               eikj2 = cmplx(cos(earg2),sin(earg2))
!
               CALL fwmw(Nd,Ne,sgs,cgs,irb,dria,Ar,dxle,dxte,Yb,Zb,dxs,dys,dzs,nasd,Nasb(ksp),Kr,beta2,Cbar,Avr,fwz,fwy)
               Bfs(icol,1) = fwz*(dxte-dxle)
               Bfs(icol,2) = fwy*(dxte-dxle)
               Bfs(icol,1) = Bfs(icol,1)*scale
               Bfs(icol,2) = Bfs(icol,2)*scale
!
!
!     IS THIS THE FIRST COLUMN, YES  BRANCH
!
               IF ( isbe/=1 ) THEN
                  Bfs(icol-1,1) = Bfs(icol-1,1)*eikj1 - Bfs(icol,1)*eikj2
                  Bfs(icol-1,2) = Bfs(icol-1,2)*eikj1 - Bfs(icol,2)*eikj2
               ENDIF
            ENDDO
         ENDDO
!
!     RETURN TO CALLING POINT - EITHER Y OR Z SENDING BODY ELEM
!
!     *** GO  EITHER TO THE  Y-ORIENTED INTERFERENCE BODY ELEMENT LOOP
!         OR  TO THE LOOP FOR SLENDER BODY SENDING ELEMENTS
!
         GOTO ibody
      CASE (3)
         irb = irb + 1
         IF ( irb>Nb ) THEN
            Jl = irow
            RETURN
         ELSE
            nrbe = Nsbea(irb)
            itsb = Nbea2(irb)
!
            xyb = Yb(irb)
            xzb = Zb(irb)
            scale = 1.0
            IF ( Nd/=0 .AND. xyb==0.0 ) scale = .5
            IF ( Ne/=0 .AND. xzb==0.0 ) scale = scale*.5
!
!     --IRBE-- IS THE ELEM. OF THE REC. BODY
!
            irbe = 0
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         irbe = irbe + 1
         IF ( irbe>nrbe ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iy = iy + 1
         irow = irow + 1
         dria = A0(iy)
         dxle = Xis1(iy)
         dxte = Xis2(iy)
         xx1 = dxle
         xx2 = dxte
         xaa = dria
         icol = 0
!
!     - LIFTING SURF. BOXES AS SENDING ELEMENTS
!
         IF ( Ntp>0 ) THEN
            j = 1
            jp1 = j
            ibox = 0
            istrip = 0
            isn = 0
            ksp = 1
!
!     LOOP FOR -PANEL-
!
            DO isp = 1 , Np
               ns = Nsaray(isp)
               nc = Ncaray(isp)
               ns = (ns-isn)/nc
               isn = Nsaray(isp)
               nasd = Nas(isp)
!
!     LOOP FOR -STRIP-
!
               DO is = 1 , ns
                  istrip = istrip + 1
                  dys = Ys(istrip)
                  dzs = Zs(istrip)
                  sgs = Sg(istrip)
                  cgs = Cg(istrip)
                  width = 2.0*Ee(istrip)
!
!     LOOP FOR -BOX-
!
                  DO ib = 1 , nc
                     ibox = ibox + 1
                     dxs = Xic(ibox)
!
                     icol = icol + 1
!
                     CALL fwmw(Nd,Ne,sgs,cgs,irb,dria,Ar,dxle,dxte,Yb,Zb,dxs,dys,dzs,nasd,Nasb(ksp),Kr,beta2,Cbar,Avr,fwz,fwy)
                     Bfs(icol,1) = fwz*(dxte-dxle)
                     Bfs(icol,2) = fwy*(dxte-dxle)
                     Bfs(icol,1) = Bfs(icol,1)*scale
                     Bfs(icol,2) = Bfs(icol,2)*scale
!
                     area = width*Delx(ibox)
                     Bfs(icol,1) = Bfs(icol,1)*area
                     Bfs(icol,2) = Bfs(icol,2)*area
                  ENDDO
               ENDDO
               ksp = ksp + nasd
            ENDDO
         ENDIF
!
!     -Z-  ORIENTED BODIES AS SENDING ELEMENTS
!
         sgs = 0.0
         cgs = 1.0
         nasd = 0
         izyflg = 1
         ASSIGN 20 TO ibody
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!
!
!
!     *** LOOP FOR EACH SLENDER BODY SENDING ELEMENT
!
 40      izyflg = 1
         sgs = 0.0
         cgs = 1.0
         DO
            lsbe = 0
            DO lsb = 1 , Nb
!
!     --LSB-- IS THE INDEX OF THE SLENDER SENDING BODY
!
               IF ( Nsbea(lsb)/=0 ) THEN
                  IF ( Nbea2(lsb)/=2 ) THEN
                     IF ( Nbea2(lsb)/=izyflg ) THEN
                        lsbe = lsbe + Nsbea(lsb)
                        CYCLE
                     ENDIF
                  ENDIF
                  xeta = Yb(lsb)
                  xzeta = Zb(lsb)
                  scale2 = scale
                  msbe = Nsbea(lsb)
                  DO lsbs = 1 , msbe
                     lsbe = lsbe + 1
                     icol = icol + 1
                     xxij = .50*Xis1(lsbe) + .50*Xis2(lsbe)
                     CALL fwmw(Nd,Ne,sgs,cgs,irb,dria,Ar,dxle,dxte,Yb,Zb,xxij,xeta,xzeta,nasd,Nasb,Kr,beta2,Cbar,Avr,fwz,fwy)
                     Bfs(icol,1) = fwz*(dxte-dxle)
                     Bfs(icol,2) = fwy*(dxte-dxle)
                     Bfs(icol,1) = Bfs(icol,1)*scale2
                     Bfs(icol,2) = Bfs(icol,2)*scale2
                  ENDDO
               ENDIF
            ENDDO
            IF ( izyflg==3 ) THEN
!
               IF ( itsb==2 ) THEN
                  CALL write(Scr1,Bfs(1,2),nw,0)
                  CALL write(Scr1,Bfs(1,1),nw,0)
                  IF ( Jf==0 ) Jf = irow
                  irow = irow + 1
               ELSEIF ( itsb==3 ) THEN
                  CALL write(Scr1,Bfs(1,2),nw,0)
                  irow = irow - 1
               ELSE
                  CALL write(Scr1,Bfs(1,1),nw,0)
               ENDIF
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSE
               izyflg = 3
               sgs = -1.0
!
               cgs = 0.0
            ENDIF
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE bfsmat
