
SUBROUTINE pthbdy
   IMPLICIT NONE
   REAL Dum(6) , Pi , Z(1)
   INTEGER Iz(1) , Mesh(2) , Nhbdy , Nlpp , Out , Sysbuf
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Nhbdy , Mesh
   COMMON /condas/ Pi
   COMMON /system/ Sysbuf , Out , Dum , Nlpp
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   REAL af , d , e(3) , r21(3) , tem(3) , v(3) , x1 , x2 , x3 , x4 , xl , zs3
   INTEGER bgpdt , buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , cbs(20) , chbdy(2) , ect , ept , file1 , file2 , flag , geom2 ,        &
         & hbgpdt , hect , hsil , i , i1 , i2 , i3 , i4 , icore , ieq , ilft , ipn , iprt , ipv , itry , ivew , iyes , leq , line , &
         & m , n , name(2) , nb , nbgp , ncb2 , nea , nect , neq(14) , nin , nn , nngp , no , nphbdy , nrd , ns , nsil(7) , nview , &
         & oeq , osil , phbdy(2) , scr1 , scr2 , sil , trl(7) , view(2)
   INTEGER korsz
   REAL sadotb
!
!     PTHBDY MODIFIES THE SIL,ECT,EQEXIN AND BGBDT FOR CHBDY ELEMENTS
!     SO THEY CAN BE PLOTTED.
!
!     THE SIL BGPDT AND EQEXIN OUTPUT LOOKALIKES ARE ADD ON FILES
!     THE ECT OUTPUT FILE HAS THE CHBDY FLAG SET NEGATIVE
!     SO PLTSET CAN TELL THE ECTS APART IT ALSO HAS THE NEW GRID POINTS
!
   EQUIVALENCE (Iz(1),Z(1))
   DATA geom2 , ect , ept , sil , ieq , bgpdt/101 , 102 , 103 , 104 , 105 , 106/
   DATA hect , hsil , oeq , hbgpdt , scr1 , scr2/201 , 202 , 203 , 204 , 301 , 302/
   DATA iyes , no , name , nphbdy , nview , ncb2/4HYES  , 4HNO   , 4HPLNB , 4HDY   , 7 , 6 , 15/
   DATA view , chbdy , phbdy , nect/2606 , 26 , 4208 , 42 , 2502 , 25 , 15/
!
!     PRINT FLAG CHBDY FLAG
!
   iprt = 0
   IF ( Mesh(1)==iyes ) iprt = 1
   Nhbdy = -1
   line = Nlpp
!
!     INITIALIZE
!
   buf1 = korsz(Z(1)) - Sysbuf
   buf2 = buf1 - Sysbuf - 1
   buf3 = buf2 - Sysbuf
   buf4 = buf3 - Sysbuf
   buf5 = buf4 - Sysbuf
   buf6 = buf5 - Sysbuf
   CALL preloc(*2100,Z(buf1),geom2)
   CALL locate(*2100,Z(buf1),chbdy,n)
!
!     MAKE A SCRATCH FILE WITH EID AF DISLIN FOR CHBDY
!
   ipn = 0
   ivew = 0
   CALL preloc(*400,Z(buf2),ept)
   file1 = ept
   CALL locate(*200,Z(buf2),phbdy,n)
   CALL read(*2200,*100,ept,Z(1),buf3,0,n)
   GOTO 2300
 100  ipn = n
 200  ipv = ipn + 1
   nrd = buf3 - ipv
   CALL locate(*400,Z(buf2),view,n)
   CALL read(*2200,*300,ept,Z(ipv),nrd,0,n)
   GOTO 2300
 300  ivew = n
 400  CALL close(ept,1)
   CALL gopen(scr1,Z(buf2),1)
   file1 = geom2
 500  CALL read(*2200,*600,geom2,cbs,ncb2,0,n)
   tem(1) = 0.0
   tem(2) = 0.0
   IF ( ipn/=0 ) THEN
      DO i = 1 , ipn , nphbdy
         IF ( cbs(2)==Iz(i) ) THEN
            tem(1) = Z(i+2)
            EXIT
         ENDIF
      ENDDO
   ENDIF
   IF ( ivew/=0 ) THEN
      IF ( cbs(15)/=0 ) THEN
         DO i = 1 , ivew , nview
            IF ( cbs(15)==Iz(ipn+i) ) THEN
               tem(2) = Z(ipn+i+5)
               IF ( iprt/=0 ) THEN
                  IF ( line>=Nlpp ) THEN
                     line = 1
                     CALL page1
                     WRITE (Out,99001)
99001                FORMAT (1H0,17X,5HIDENT,8X,4HBETA,7X,5HGAMMA,9X,3HCAN,6X,6HCAN BE,/6X,5HCHBDY,6X,6HNUMBER,8X,4HMESH,8X,4HMESH, &
                           & 7X,5HSHADE,6X,6HSHADED,5X,7HDISLIN ,/)
                  ENDIF
                  nb = iyes
                  ns = iyes
                  IF ( Iz(ipn+i+1)==0 ) nb = no
                  IF ( Iz(ipn+i+2)==0 ) ns = no
                  line = line + 1
                  WRITE (Out,99002) cbs(1) , Iz(ipn+i) , Iz(ipn+i+3) , Iz(ipn+i+4) , nb , ns , tem(2)
99002             FORMAT (1H ,4(I10,2X),6X,A4,8X,A4,2X,1P,E10.4)
               ENDIF
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ENDIF
   CALL write(scr1,tem,2,0)
   GOTO 500
 600  CALL write(scr1,0,0,1)
   CALL close(scr1,1)
   CALL close(geom2,1)
   CALL gopen(scr1,Z(buf1),0)
   trl(1) = sil
   CALL rdtrl(trl)
   osil = trl(3)
   trl(1) = bgpdt
   CALL rdtrl(trl)
   nin = trl(2)
   nrd = 4*trl(2)
   IF ( 5*Sysbuf+nrd+50>buf1 ) GOTO 2300
!
!     FIND CHBDY CARDS COPY ECT TO CHBDY CARDS
!
   CALL gopen(ect,Z(buf2),0)
   CALL gopen(hect,Z(buf3),1)
   file1 = ect
 700  CALL read(*2100,*2100,ect,cbs,3,0,n)
   CALL write(hect,cbs,3,0)
   IF ( cbs(1)==chbdy(1) .AND. cbs(2)==chbdy(2) ) THEN
!
!     COPY SIL EQEXIN TO NEW FILES
!
      icore = buf6 - 1
      ilft = 1
      file1 = sil
      file2 = hsil
      n = buf4
      leq = 0
      GOTO 900
   ELSE
      DO
!
!     DUPE REST OF RECORD
!
         CALL read(*2200,*800,ect,Z(1),buf6-1,0,n)
         CALL write(hect,Z(1),buf6-1,0)
      ENDDO
   ENDIF
 800  CALL write(hect,Z(1),n,1)
   GOTO 700
 900  CALL gopen(file1,Z(buf6),0)
   CALL gopen(file2,Z(n),1)
   DO
      CALL read(*2200,*1000,file1,Z(ilft),icore,0,m)
      IF ( file1==ieq ) THEN
         DO i = 1 , icore , 2
            IF ( Iz(i)>leq ) leq = Iz(i)
         ENDDO
      ENDIF
      CALL write(file2,Z(ilft),icore,0)
   ENDDO
 1000 IF ( file1==ieq ) THEN
      DO i = 1 , m , 2
         IF ( Iz(i)>leq ) leq = Iz(i)
      ENDDO
   ENDIF
   CALL write(file2,Z(ilft),m,0)
   CALL close(file1,1)
   IF ( n==buf5 ) THEN
!
!     BRING IN BGPDT
!
      CALL gopen(bgpdt,Z(buf6),0)
      file1 = bgpdt
      CALL read(*2200,*2200,bgpdt,Z(1),nrd,0,n)
      CALL close(bgpdt,1)
!
!     FINALLY TIME TO GO TO WORK
!
      Nhbdy = 0
      nngp = 0
      nbgp = nrd + 1
      DO i = 1 , 24
         Iz(nrd+i) = 0
      ENDDO
      file1 = ect
      CALL gopen(scr2,Z(buf6),1)
   ELSE
      file1 = ieq
      file2 = oeq
      n = buf5
      GOTO 900
   ENDIF
 1100 CALL read(*2200,*1700,ect,cbs,nect,0,n)
   cbs(nect) = 0.0
   IF ( cbs(3)>6 ) cbs(3) = 3
   CALL read(*2200,*2200,scr1,tem,2,0,n)
   flag = cbs(3)
   Nhbdy = Nhbdy + 1
   IF ( flag==2 ) THEN
!
!     LINE
!
!
!     BGPDT DATA FOR LINE
!
      i1 = (cbs(4)-1)*4 + 2
      i2 = (cbs(5)-1)*4 + 2
      CALL samb(Z(i2),Z(i1),r21)
      xl = sadotb(r21,r21)
      IF ( xl==0.0 ) GOTO 2000
      x1 = sadotb(r21,cbs(12))
      xl = x1/xl
      e(1) = xl*r21(1)
      e(2) = xl*r21(2)
      e(3) = xl*r21(3)
      CALL samb(cbs(12),e,v)
      CALL sanorm(*2000,v)
      CALL saxb(v,r21,e)
      CALL sanorm(*2000,e)
      d = tem(2)
      af = tem(1)*.5
      Z(nbgp+1) = Z(i1) + d*v(1) - af*e(1)
      Z(nbgp+2) = Z(i1+1) + d*v(2) - af*e(2)
      Z(nbgp+3) = Z(i1+2) + d*v(3) - af*e(3)
      Z(nbgp+5) = Z(i2) + d*v(1) - af*e(1)
      Z(nbgp+6) = Z(i2+1) + d*v(2) - af*e(2)
      Z(nbgp+7) = Z(i2+2) + d*v(3) - af*e(3)
      Z(nbgp+9) = Z(i2) + d*v(1) + af*e(1)
      Z(nbgp+10) = Z(i2+1) + d*v(2) + af*e(2)
      Z(nbgp+11) = Z(i2+2) + d*v(3) + af*e(3)
      Z(nbgp+13) = Z(i1) + d*v(1) + af*e(1)
      Z(nbgp+14) = Z(i1+1) + d*v(2) + af*e(2)
      Z(nbgp+15) = Z(i1+2) + d*v(3) + af*e(3)
      Z(nbgp+17) = Z(i1) + d*v(1) + .5*r21(1)
      Z(nbgp+18) = Z(i1+1) + d*v(2) + .5*r21(2)
      Z(nbgp+19) = Z(i1+2) + d*v(3) + .5*r21(3)
      Z(nbgp+21) = Z(nbgp+17) + 2.*af*v(1)
      Z(nbgp+22) = Z(nbgp+18) + 2.*af*v(2)
      Z(nbgp+23) = Z(nbgp+19) + 2.*af*v(3)
      nngp = nngp + 6
      ns = 6
      nea = 12
      nb = 24
      m = 6
      n = 6
!
!     REV  OR  ELIP   DO NOTHING
!
      GOTO 1300
   ELSEIF ( flag==3 .OR. flag==6 ) THEN
      GOTO 1600
   ELSEIF ( flag==4 ) THEN
!
!     AREA3
!
!     BGPDT DATA FOR AREA3
!
      i1 = (cbs(4)-1)*4 + 2
      i2 = (cbs(5)-1)*4 + 2
      i3 = (cbs(6)-1)*4 + 2
      CALL samb(Z(i2),Z(i1),e)
      CALL samb(Z(i3),Z(i1),v)
      CALL saxb(e,v,e)
      CALL sanorm(*2000,e)
      CALL samb(Z(i2),Z(i1),v)
      x1 = sadotb(v,v)
      CALL samb(Z(i3),Z(i1),v)
      x2 = sadotb(v,v)
      CALL samb(Z(i3),Z(i2),v)
      x3 = sadotb(v,v)
      x1 = amax1(x1,x2)
      x1 = amax1(x1,x3)
      xl = .25*sqrt(x1)
      CALL sapb(Z(i1),Z(i2),v)
      CALL sapb(Z(i3),v,v)
      Z(nbgp+1) = v(1)/3.0
      Z(nbgp+2) = v(2)/3.0
      Z(nbgp+3) = v(3)/3.0
      Z(nbgp+5) = Z(nbgp+1) + xl*e(1)
      Z(nbgp+6) = Z(nbgp+2) + xl*e(2)
      Z(nbgp+7) = Z(nbgp+3) + xl*e(3)
      GOTO 1400
   ELSEIF ( flag==5 ) THEN
!
!     AREA4
!
!     BGPDT DATA FOR AREA4
!
      i1 = (cbs(4)-1)*4 + 2
      i2 = (cbs(5)-1)*4 + 2
      i3 = (cbs(6)-1)*4 + 2
      i4 = (cbs(7)-1)*4 + 2
      CALL samb(Z(i3),Z(i1),e)
      CALL samb(Z(i4),Z(i2),v)
      CALL saxb(e,v,e)
      CALL sanorm(*2000,e)
      CALL samb(Z(i2),Z(i1),v)
      x1 = sadotb(v,v)
      CALL samb(Z(i3),Z(i2),v)
      x2 = sadotb(v,v)
      CALL samb(Z(i4),Z(i3),v)
      x3 = sadotb(v,v)
      CALL samb(Z(i4),Z(i1),v)
      x4 = sadotb(v,v)
      x1 = amax1(x1,x2)
      x1 = amax1(x1,x3)
      x1 = amax1(x1,x4)
      xl = .25*sqrt(x1)
      CALL sapb(Z(i1),Z(i2),v)
      CALL sapb(v,Z(i3),v)
      CALL sapb(v,Z(i4),v)
      Z(nbgp+1) = .25*v(1)
      Z(nbgp+2) = .25*v(2)
      Z(nbgp+3) = .25*v(3)
      Z(nbgp+5) = Z(nbgp+1) + xl*e(1)
      Z(nbgp+6) = Z(nbgp+2) + xl*e(2)
      Z(nbgp+7) = Z(nbgp+3) + xl*e(3)
      GOTO 1400
   ELSE
!
!     POINT
!
!
!     BGPDT DATA FOR POINT
!
      i1 = (cbs(4)-1)*4 + 2
      itry = 1
      e(1) = 0.0
      e(2) = 0.0
      e(3) = 0.0
      CALL sapb(cbs(12),e,v)
      CALL sanorm(*2000,v)
      e(1) = 1.0
      DO
         xl = sadotb(v,e)
         r21(1) = e(1) - xl*v(1)
         r21(2) = e(2) - xl*v(2)
         r21(3) = e(3) - xl*v(3)
         xl = sadotb(r21,r21)
         IF ( xl>.2 ) THEN
            CALL sanorm(*1200,r21)
            EXIT
         ELSE
            IF ( itry==2 ) GOTO 2000
            itry = 2
            e(1) = 0.0
            e(2) = 1.0
         ENDIF
      ENDDO
   ENDIF
 1200 CALL saxb(v,r21,e)
   xl = 0.0
   IF ( tem(1)/=0.0 ) xl = sqrt(tem(1)/Pi)
   zs3 = .8660254
   Z(nbgp+1) = Z(i1) + xl*r21(1)
   Z(nbgp+2) = Z(i1+1) + xl*r21(2)
   Z(nbgp+3) = Z(i1+2) + xl*r21(3)
   Z(nbgp+5) = Z(i1) + xl*(.5*r21(1)+zs3*e(1))
   Z(nbgp+6) = Z(i1+1) + xl*(.5*r21(2)+zs3*e(2))
   Z(nbgp+7) = Z(i1+2) + xl*(.5*r21(3)+zs3*e(3))
   Z(nbgp+9) = Z(i1) + xl*(-.5*r21(1)+zs3*e(1))
   Z(nbgp+10) = Z(i1+1) + xl*(-.5*r21(2)+zs3*e(2))
   Z(nbgp+11) = Z(i1+2) + xl*(-.5*r21(3)+zs3*e(3))
   Z(nbgp+14) = Z(i1+1) - xl*r21(2)
   Z(nbgp+15) = Z(i1+2) - xl*r21(3)
   Z(nbgp+17) = Z(i1) + xl*(-.5*r21(1)-zs3*e(1))
   Z(nbgp+18) = Z(i1+1) + xl*(-.5*r21(2)-zs3*e(2))
   Z(nbgp+19) = Z(i1+2) + xl*(-.5*r21(3)-zs3*e(3))
   Z(nbgp+21) = Z(i1) + xl*(+.5*r21(1)-zs3*e(1))
   Z(nbgp+22) = Z(i1+1) + xl*(+.5*r21(2)-zs3*e(2))
   Z(nbgp+23) = Z(i1+2) + xl*(+.5*r21(3)-zs3*e(3))
   Z(nbgp+25) = Z(i1) + xl*v(1)
   Z(nbgp+26) = Z(i1+1) + xl*v(2)
   Z(nbgp+27) = Z(i1+2) + xl*v(3)
   nngp = nngp + 7
   ns = 7
   nea = 14
   nb = 28
   m = 7
   n = 5
 1300 nn = 1
   DO i = 1 , m
      leq = leq + 1
      nin = nin + 1
      nsil(i) = nin
      neq(nn) = leq
      neq(nn+1) = nin
      cbs(n) = nin
      nn = nn + 2
      n = n + 1
   ENDDO
   GOTO 1500
 1400 nngp = nngp + 2
   ns = 2
   nea = 4
   nb = 8
   leq = leq + 1
   nin = nin + 1
   n = 7
   IF ( flag==5 ) n = 8
   nsil(1) = nin
   neq(1) = leq
   neq(2) = nin
   cbs(n) = nin
   leq = leq + 1
   nin = nin + 1
   nsil(2) = nin
   neq(3) = leq
   neq(4) = nin
   cbs(n+1) = nin
   cbs(n+2) = nin
   cbs(n+3) = nin
   IF ( flag==4 ) cbs(n+4) = nin
!
!     ADD TO HSIL HEQEXIN  HECT
!     BGPDT
!
 1500 CALL write(hsil,nsil,ns,0)
   CALL write(oeq,neq,nea,0)
   CALL write(scr2,Z(nbgp),nb,0)
 1600 cbs(3) = -cbs(3)
   CALL write(hect,cbs,nect,0)
   GOTO 1100
!
!     END CLOSE FILES, WRITE NBGPDT, WRITE TRAILERS THEN FINISH ECT COPY
!
 1700 CALL write(hsil,0,0,1)
   CALL write(oeq,0,0,1)
   CALL write(hect,0,0,1)
   CALL write(scr2,0,0,1)
   CALL close(scr2,1)
   CALL close(scr1,1)
   CALL close(hsil,1)
   CALL close(oeq,1)
   CALL gopen(hbgpdt,Z(buf1),1)
   CALL write(hbgpdt,Z,nrd,0)
   IF ( nngp==0 ) GOTO 1900
   file1 = scr2
   CALL gopen(scr2,Z(buf6),0)
   DO
      CALL read(*2200,*1800,scr2,Z(1),buf6-1,0,n)
      CALL write(hbgpdt,Z(1),buf6-1,0)
   ENDDO
 1800 CALL write(hbgpdt,Z(1),n,1)
   CALL close(scr2,1)
 1900 CALL close(hbgpdt,1)
   trl(1) = hbgpdt
   trl(2) = nrd/4 + nngp
   CALL wrttrl(trl)
   trl(1) = oeq
   CALL wrttrl(trl)
   trl(1) = hsil
   trl(3) = nngp + osil
   CALL wrttrl(trl)
   trl(1) = ect
   CALL rdtrl(trl)
   trl(1) = hect
   CALL wrttrl(trl)
   file1 = ect
   GOTO 700
!
!     BAD GEOMETRY FOR ELEMENT
!
 2000 cbs(3) = -cbs(3)
   Nhbdy = Nhbdy - 1
   WRITE (Out,99003) Uwm , cbs(1)
99003 FORMAT (A25,', CHBDY ELEMENT',I9,' HAS NO NORMAL OR BAD GEOMETRY',' WHICH MAKES IT UNPLOTTABLE')
   GOTO 1600
!
!     RETURN OR ERROR MESSAGES
!
 2100 CALL close(ect,1)
   CALL close(hect,1)
   CALL close(geom2,1)
   RETURN
!
 2200 CALL mesage(-2,0,file1)
 2300 CALL mesage(-8,0,name)
END SUBROUTINE pthbdy
