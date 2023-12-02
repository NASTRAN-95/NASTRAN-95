!*==apd1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE apd1(Fst,Ns,Fct,Nc,Ls,Lc)
   IMPLICIT NONE
   USE C_APD12C
   USE C_APD1C
   USE C_APD1D
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Fst
   INTEGER :: Ns
   REAL , DIMENSION(1) :: Fct
   INTEGER :: Nc
   LOGICAL :: Ls
   LOGICAL :: Lc
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3,3) :: acpl
   INTEGER :: acsib , back , eidb , file , i , icid , ip1 , iret , j , k , kk , nbox , ncid , ncrdp , np , nstrip , ntp , silc
   INTEGER , DIMENSION(4) :: acsix , sildx
   REAL :: aij , aij1 , cg , cj , cj1 , delx , dj , dj1 , ds , ee , f , fci1 , fsj1 , sg , xi1j , xi1j1 , xic , xij , xij1 , xlam , &
         & yj , yj1 , ys , ysp , zs
   REAL , DIMENSION(3) :: axic , rb1 , vx1 , vx2
   INTEGER , DIMENSION(5) :: ays , cid
   INTEGER , SAVE :: clsrew , rdrew , wtrew
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(2) :: ncary
   INTEGER , DIMENSION(6) :: necta
   REAL , DIMENSION(5) :: xb
   EXTERNAL apdcs , apdf , close , gmmats , gopen , iapd , mesage , read , rewind , skprec , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(2),cid(1)) , (Key(2),Np) , (Key(3),Nstrip) , (Key(4),Ntp) ,          &
!>>>>    & (Key(5),F) , (ays(1),ys) , (ays(2),zs) , (ays(3),ee) , (ays(4),sg) , (ays(5),cg) , (axic(1),xic) , (axic(2),delx) ,           &
!>>>>    & (axic(3),xlam) , (sildx(1),icid) , (sildx(3),silc) , (acsix(1),acsib) , (Z(1),Iz(1)) , (acsix(2),vx2(1)) , (necta(1),eidb)
   DATA rdrew , clsrew , wtrew/0 , 1 , 1/
   DATA name/4HAPD1 , 1H /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Key(1) = 1
         silc = Silb
!
!     IF NEW IGRID SET INITIALIZE
!
         IF ( Ls ) THEN
            np = 0
            ntp = 0
            nbox = 0
            Nasb = 0
            nstrip = 0
            CALL gopen(Scr3,Z(Buf10),wtrew)
            CALL gopen(Scr4,Z(Buf11),wtrew)
            CALL gopen(Scr5,Z(Buf12),wtrew)
         ENDIF
!
!     MAKE COORD SYSTEM AND GET POINTS IN PROPER SYSTEM
!
         CALL apdcs
         sg = S1
         cg = C1
         acsib = Mcstm
!
!     CHECK FOR ASSOCIATED BODIES
!
         SPAG_Loop_1_1: DO j = 1 , 6
            IF ( iz(Ippc+j)==0 ) EXIT SPAG_Loop_1_1
            Nasb = Nasb + 1
         ENDDO SPAG_Loop_1_1
!
!     GENERATE BOXES
!
         ncrdp = Ncrd
         np = np + 1
         fsj1 = apdf(Fst,1,Nspan)
         yj1 = fsj1*Yp4
         dj1 = fsj1*Xp4
         cj1 = (1.0-fsj1)*Xp2 + fsj1*(Xp3-Xp4)
         eidb = Eid - 1
         DO j = 1 , Ns
            yj = yj1
            dj = dj1
            cj = cj1
            fsj1 = apdf(Fst,j+1,Nspan)
            yj1 = fsj1*Yp4
            dj1 = fsj1*Xp4
            cj1 = (1.0-fsj1)*Xp2 + fsj1*(Xp3-Xp4)
            ee = .5*(yj1-yj)
            ysp = yj + ee
            nstrip = nstrip + 1
            fci1 = apdf(Fct,1,Nchord)
            xi1j = dj + fci1*cj
            xi1j1 = dj1 + fci1*cj1
            ds = 1.0/(yj1-yj)
            ys = ysp*cg + Ra1(2)
            zs = ysp*sg + Ra1(3)
            CALL write(Scr3,ays(1),5,0)
            DO i = 1 , Nc
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     ntp = ntp + 1
                     xij = xi1j
                     xij1 = xi1j1
                     fci1 = apdf(Fct,i+1,Nchord)
                     xi1j = dj + fci1*cj
                     xi1j1 = dj1 + fci1*cj1
                     aij = (1.0-Xop)*xij + Xop*xi1j
                     aij1 = (1.0-Xop)*xij1 + Xop*xi1j1
                     xic = .5*(aij+aij1) + Ra1(1)
                     xlam = (aij1-aij)*ds
                     delx = .50*(-xij+xi1j-xij1+xi1j1)
                     CALL write(Scr4,axic(1),3,0)
                     xic = xic - Ra1(1)
                     eidb = eidb + 1
                     nbox = nbox + 1
                     cid(1) = Cidbx + i + (Nc+1)*(j-1)
                     cid(2) = cid(1) + 1
                     cid(3) = cid(1) + Nc + 1
                     cid(4) = cid(3) + 1
                     cid(5) = eidb
                     ncid = cid(4)
                     Nj = Nj + 1
                     Nk = Nk + 2
                     vx1(3) = 0
                     IF ( j/=1 ) GOTO 4
                     IF ( i==1 ) THEN
                        ASSIGN 2 TO back
                        icid = cid(1)
                        vx1(1) = xij
                        vx1(2) = yj
                        kk = 1
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
 2                   ASSIGN 4 TO back
                     icid = cid(2)
                     vx1(1) = xi1j
                     vx1(2) = yj
                     kk = 1
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
 4                   IF ( i==1 ) THEN
                        ASSIGN 6 TO back
                        icid = cid(3)
                        vx1(1) = xij1
                        vx1(2) = yj1
                        kk = 1
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
 6                   ASSIGN 8 TO back
                     icid = cid(4)
                     vx1(1) = xi1j1
                     vx1(2) = yj1
                     kk = 1
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
 8                   ASSIGN 10 TO back
                     icid = cid(5)
                     vx1(1) = xic + .25*delx
                     vx1(2) = ysp
                     kk = 2
                     spag_nextblock_2 = 2
                  CASE (2)
                     CALL gmmats(acpl,3,3,0,vx1,3,1,0,vx2)
                     DO k = 1 , 3
                        vx2(k) = vx2(k) + rb1(k)
                     ENDDO
                     CALL write(Bgpa,acsix,4,0)
                     CALL write(Gpla,icid,1,0)
                     CALL write(Useta,Auset(1,kk),6,0)
                     Ncrd = Ncrd + 1
                     silc = silc + 6
                     Isiln = Isiln + 6
                     sildx(4) = Isiln
                     Luseta = silc
                     sildx(2) = 10*silc + 1
                     CALL write(Sila,silc,1,0)
                     CALL write(Scr2,Isiln,1,0)
                     CALL write(Scr2,silc,1,0)
                     CALL write(Scr1,icid,2,0)
                     GOTO back
 10                  cid(1) = iapd(i,j,Nc,ncrdp)
                     cid(2) = iapd(i+1,j,Nc,ncrdp)
                     cid(4) = iapd(i,j+1,Nc,ncrdp)
                     cid(3) = iapd(i+1,j+1,Nc,ncrdp)
                     cid(5) = cid(3) + 1
                     CALL write(Ecta,necta(1),6,0)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO
         ENDDO
         Cidbx = ncid
         ncary(1) = Nc
         ncary(2) = nbox
         CALL write(Scr5,ncary,2,0)
!
!     ADD PROPERITY CARD POINTERS FOR APD2
!
         CALL write(Scr5,Ippc,1,0)
         Silb = silc
         IF ( .NOT.Lc ) RETURN
!
!     WRITE ACPT TABLE
!
         f = X1p - Xop
         CALL write(Acpt,Key,5,0)
!
!     COPY STUFF FROM SCRATCH FILES TO ACPT
!
         file = Scr5
         k = 3
         ASSIGN 20 TO iret
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      ASSIGN 40 TO iret
         file = Scr3
         k = 5
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      ASSIGN 60 TO iret
         file = Scr4
         k = 3
         spag_nextblock_1 = 2
      CASE (2)
         CALL write(file,0,0,1)
         CALL close(file,clsrew)
         CALL gopen(file,Z(Buf12),rdrew)
         DO i = 1 , k
            DO
               CALL read(*80,*50,file,xb(1),k,0,j)
!
!     SKIP PROPERTY CARD POINTERS
!
               IF ( i/=3 .OR. file/=Scr5 ) CALL write(Acpt,xb(i),1,0)
            ENDDO
 50         CALL rewind(file)
            CALL skprec(file,1)
         ENDDO
         CALL close(file,clsrew)
         GOTO iret
 60      CALL write(Acpt,0,0,1)
         RETURN
!
!     ERROR MESAGES
!
 80      ip1 = -2
         CALL mesage(ip1,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE apd1
