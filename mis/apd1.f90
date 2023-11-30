
SUBROUTINE apd1(Fst,Ns,Fct,Nc,Ls,Lc)
   IMPLICIT NONE
   REAL Acpl(3,3) , Alzo , C1 , F , Ra1(3) , Rb1(3) , S1 , Sysbuf , Uk , Usa , X1 , X12 , X1p , X4 , X43 , Xop , Xp2 , Xp3 , Xp4 ,  &
      & Y1 , Y4 , Yp4 , Z(1) , Z1 , Z4
   INTEGER Acpt , Acsid , Auset(6,2) , Bgpa , Buf10 , Buf11 , Buf12 , Cidbx , Cp , Cstma , Ecta , Eid , Gpla , Iacs , Icpl(14) ,    &
         & Igid , Ippc , Isiln , Iz(1) , Key(5) , Lchord , Left , Lspan , Luseta , Mcstm , Nasb , Ncam2 , Nchord , Ncrd , Ncst1 ,   &
         & Ncst2 , Next , Nj , Nk , Not , Np , Nspan , Nstrip , Ntp , Pid , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Sila , Silb , Useta
   COMMON /apd12c/ Key , Auset , Usa , Uk , Ncam2 , Nasb , Ippc
   COMMON /apd1c / Eid , Pid , Cp , Nspan , Nchord , Lspan , Lchord , Igid , X1 , Y1 , Z1 , X12 , X4 , Y4 , Z4 , X43 , Xop , X1p ,  &
                 & Alzo , Mcstm , Ncst1 , Ncst2 , Cidbx , Acsid , Iacs , Silb , Ncrd , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Ecta ,    &
                 & Bgpa , Gpla , Useta , Sila , Cstma , Acpt , Buf10 , Buf11 , Buf12 , Next , Left , Isiln
   COMMON /apd1d / Icpl , Yp4 , S1 , C1 , Xp2 , Xp3 , Xp4 , Ra1
   COMMON /blank / Nk , Nj , Luseta
   COMMON /system/ Sysbuf , Not
   COMMON /zzzzzz/ Z
   LOGICAL Lc , Ls
   INTEGER Nc , Ns
   REAL Fct(1) , Fst(1)
   INTEGER acsib , acsix(4) , ays(5) , back , cid(5) , clsrew , eidb , file , i , icid , ip1 , iret , j , k , kk , name(2) , nbox , &
         & ncary(2) , ncid , ncrdp , necta(6) , rdrew , silc , sildx(4) , wtrew
   REAL aij , aij1 , axic(3) , cg , cj , cj1 , delx , dj , dj1 , ds , ee , fci1 , fsj1 , sg , vx1(3) , vx2(3) , xb(5) , xi1j ,      &
      & xi1j1 , xic , xij , xij1 , xlam , yj , yj1 , ys , ysp , zs
   REAL apdf
   INTEGER iapd
!
   !>>>>EQUIVALENCE (Icpl(3),Rb1(1)) , (Icpl(6),Acpl(1,1)) , (necta(2),cid(1)) , (Key(2),Np) , (Key(3),Nstrip) , (Key(4),Ntp) ,          &
!>>>>    & (Key(5),F) , (ays(1),ys) , (ays(2),zs) , (ays(3),ee) , (ays(4),sg) , (ays(5),cg) , (axic(1),xic) , (axic(2),delx) ,           &
!>>>>    & (axic(3),xlam) , (sildx(1),icid) , (sildx(3),silc) , (acsix(1),acsib) , (Z(1),Iz(1)) , (acsix(2),vx2(1)) , (necta(1),eidb)
   DATA rdrew , clsrew , wtrew/0 , 1 , 1/
   DATA name/4HAPD1 , 1H /
!
   Key(1) = 1
   silc = Silb
!
!     IF NEW IGRID SET INITIALIZE
!
   IF ( Ls ) THEN
      Np = 0
      Ntp = 0
      nbox = 0
      Nasb = 0
      Nstrip = 0
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
   DO j = 1 , 6
      IF ( Iz(Ippc+j)==0 ) EXIT
      Nasb = Nasb + 1
   ENDDO
!
!     GENERATE BOXES
!
   ncrdp = Ncrd
   Np = Np + 1
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
      Nstrip = Nstrip + 1
      fci1 = apdf(Fct,1,Nchord)
      xi1j = dj + fci1*cj
      xi1j1 = dj1 + fci1*cj1
      ds = 1.0/(yj1-yj)
      ys = ysp*cg + Ra1(2)
      zs = ysp*sg + Ra1(3)
      CALL write(Scr3,ays(1),5,0)
      DO i = 1 , Nc
         Ntp = Ntp + 1
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
         IF ( j/=1 ) GOTO 40
         IF ( i==1 ) THEN
            ASSIGN 20 TO back
            icid = cid(1)
            vx1(1) = xij
            vx1(2) = yj
            kk = 1
            GOTO 100
         ENDIF
 20      ASSIGN 40 TO back
         icid = cid(2)
         vx1(1) = xi1j
         vx1(2) = yj
         kk = 1
         GOTO 100
 40      IF ( i==1 ) THEN
            ASSIGN 60 TO back
            icid = cid(3)
            vx1(1) = xij1
            vx1(2) = yj1
            kk = 1
            GOTO 100
         ENDIF
 60      ASSIGN 80 TO back
         icid = cid(4)
         vx1(1) = xi1j1
         vx1(2) = yj1
         kk = 1
         GOTO 100
 80      ASSIGN 120 TO back
         icid = cid(5)
         vx1(1) = xic + .25*delx
         vx1(2) = ysp
         kk = 2
 100     CALL gmmats(Acpl,3,3,0,vx1,3,1,0,vx2)
         DO k = 1 , 3
            vx2(k) = vx2(k) + Rb1(k)
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
 120     cid(1) = iapd(i,j,Nc,ncrdp)
         cid(2) = iapd(i+1,j,Nc,ncrdp)
         cid(4) = iapd(i,j+1,Nc,ncrdp)
         cid(3) = iapd(i+1,j+1,Nc,ncrdp)
         cid(5) = cid(3) + 1
         CALL write(Ecta,necta(1),6,0)
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
   F = X1p - Xop
   CALL write(Acpt,Key,5,0)
!
!     COPY STUFF FROM SCRATCH FILES TO ACPT
!
   file = Scr5
   k = 3
   ASSIGN 200 TO iret
   GOTO 400
 200  ASSIGN 300 TO iret
   file = Scr3
   k = 5
   GOTO 400
 300  ASSIGN 500 TO iret
   file = Scr4
   k = 3
 400  CALL write(file,0,0,1)
   CALL close(file,clsrew)
   CALL gopen(file,Z(Buf12),rdrew)
   DO i = 1 , k
      DO
         CALL read(*600,*450,file,xb(1),k,0,j)
!
!     SKIP PROPERTY CARD POINTERS
!
         IF ( i/=3 .OR. file/=Scr5 ) CALL write(Acpt,xb(i),1,0)
      ENDDO
 450  CALL rewind(file)
      CALL skprec(file,1)
   ENDDO
   CALL close(file,clsrew)
   GOTO iret
 500  CALL write(Acpt,0,0,1)
   RETURN
!
!     ERROR MESAGES
!
 600  ip1 = -2
   CALL mesage(ip1,file,name)
END SUBROUTINE apd1