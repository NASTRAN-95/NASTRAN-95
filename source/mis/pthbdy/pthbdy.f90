!*==pthbdy.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pthbdy
   USE c_blank
   USE c_condas
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL :: af , d , x1 , x2 , x3 , x4 , xl , zs3
   INTEGER , SAVE :: bgpdt , ect , ept , geom2 , hbgpdt , hect , hsil , ieq , iyes , ncb2 , nect , no , nphbdy , nview , oeq ,      &
                   & scr1 , scr2 , sil
   INTEGER :: buf1 , buf2 , buf3 , buf4 , buf5 , buf6 , file1 , file2 , flag , i , i1 , i2 , i3 , i4 , icore , ilft , ipn , iprt ,  &
            & ipv , itry , ivew , leq , line , m , n , nb , nbgp , nea , nin , nn , nngp , nrd , ns , osil
   INTEGER , DIMENSION(20) :: cbs
   INTEGER , DIMENSION(2) , SAVE :: chbdy , name , phbdy , view
   REAL , DIMENSION(3) :: e , r21 , tem , v
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(14) :: neq
   INTEGER , DIMENSION(7) :: nsil , trl
   EXTERNAL close , gopen , korsz , locate , mesage , page1 , preloc , rdtrl , read , sadotb , samb , sanorm , sapb , saxb , write ,&
          & wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PTHBDY MODIFIES THE SIL,ECT,EQEXIN AND BGBDT FOR CHBDY ELEMENTS
!     SO THEY CAN BE PLOTTED.
!
!     THE SIL BGPDT AND EQEXIN OUTPUT LOOKALIKES ARE ADD ON FILES
!     THE ECT OUTPUT FILE HAS THE CHBDY FLAG SET NEGATIVE
!     SO PLTSET CAN TELL THE ECTS APART IT ALSO HAS THE NEW GRID POINTS
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA geom2 , ect , ept , sil , ieq , bgpdt/101 , 102 , 103 , 104 , 105 , 106/
   DATA hect , hsil , oeq , hbgpdt , scr1 , scr2/201 , 202 , 203 , 204 , 301 , 302/
   DATA iyes , no , name , nphbdy , nview , ncb2/4HYES  , 4HNO   , 4HPLNB , 4HDY   , 7 , 6 , 15/
   DATA view , chbdy , phbdy , nect/2606 , 26 , 4208 , 42 , 2502 , 25 , 15/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     PRINT FLAG CHBDY FLAG
!
         iprt = 0
         IF ( mesh(1)==iyes ) iprt = 1
         nhbdy = -1
         line = nlpp
!
!     INITIALIZE
!
         buf1 = korsz(z(1)) - sysbuf
         buf2 = buf1 - sysbuf - 1
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         buf5 = buf4 - sysbuf
         buf6 = buf5 - sysbuf
         CALL preloc(*240,z(buf1),geom2)
         CALL locate(*240,z(buf1),chbdy,n)
!
!     MAKE A SCRATCH FILE WITH EID AF DISLIN FOR CHBDY
!
         ipn = 0
         ivew = 0
         CALL preloc(*80,z(buf2),ept)
         file1 = ept
         CALL locate(*40,z(buf2),phbdy,n)
         CALL read(*260,*20,ept,z(1),buf3,0,n)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 20      ipn = n
 40      ipv = ipn + 1
         nrd = buf3 - ipv
         CALL locate(*80,z(buf2),view,n)
         CALL read(*260,*60,ept,z(ipv),nrd,0,n)
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
 60      ivew = n
 80      CALL close(ept,1)
         CALL gopen(scr1,z(buf2),1)
         file1 = geom2
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*260,*100,geom2,cbs,ncb2,0,n)
         tem(1) = 0.0
         tem(2) = 0.0
         IF ( ipn/=0 ) THEN
            SPAG_Loop_1_1: DO i = 1 , ipn , nphbdy
               IF ( cbs(2)==iz(i) ) THEN
                  tem(1) = z(i+2)
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ENDIF
         IF ( ivew/=0 ) THEN
            IF ( cbs(15)/=0 ) THEN
               SPAG_Loop_1_2: DO i = 1 , ivew , nview
                  IF ( cbs(15)==iz(ipn+i) ) THEN
                     tem(2) = z(ipn+i+5)
                     IF ( iprt/=0 ) THEN
                        IF ( line>=nlpp ) THEN
                           line = 1
                           CALL page1
                           WRITE (out,99001)
99001                      FORMAT (1H0,17X,5HIDENT,8X,4HBETA,7X,5HGAMMA,9X,3HCAN,6X,6HCAN BE,/6X,5HCHBDY,6X,6HNUMBER,8X,4HMESH,8X,  &
                                  &4HMESH,7X,5HSHADE,6X,6HSHADED,5X,7HDISLIN ,/)
                        ENDIF
                        nb = iyes
                        ns = iyes
                        IF ( iz(ipn+i+1)==0 ) nb = no
                        IF ( iz(ipn+i+2)==0 ) ns = no
                        line = line + 1
                        WRITE (out,99002) cbs(1) , iz(ipn+i) , iz(ipn+i+3) , iz(ipn+i+4) , nb , ns , tem(2)
99002                   FORMAT (1H ,4(I10,2X),6X,A4,8X,A4,2X,1P,E10.4)
                     ENDIF
                     EXIT SPAG_Loop_1_2
                  ENDIF
               ENDDO SPAG_Loop_1_2
            ENDIF
         ENDIF
         CALL write(scr1,tem,2,0)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 100     CALL write(scr1,0,0,1)
         CALL close(scr1,1)
         CALL close(geom2,1)
         CALL gopen(scr1,z(buf1),0)
         trl(1) = sil
         CALL rdtrl(trl)
         osil = trl(3)
         trl(1) = bgpdt
         CALL rdtrl(trl)
         nin = trl(2)
         nrd = 4*trl(2)
         IF ( 5*sysbuf+nrd+50>buf1 ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     FIND CHBDY CARDS COPY ECT TO CHBDY CARDS
!
         CALL gopen(ect,z(buf2),0)
         CALL gopen(hect,z(buf3),1)
         file1 = ect
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*240,*240,ect,cbs,3,0,n)
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
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO
!
!     DUPE REST OF RECORD
!
               CALL read(*260,*120,ect,z(1),buf6-1,0,n)
               CALL write(hect,z(1),buf6-1,0)
            ENDDO
         ENDIF
 120     CALL write(hect,z(1),n,1)
         spag_nextblock_1 = 3
      CASE (4)
         CALL gopen(file1,z(buf6),0)
         CALL gopen(file2,z(n),1)
         DO
            CALL read(*260,*140,file1,z(ilft),icore,0,m)
            IF ( file1==ieq ) THEN
               DO i = 1 , icore , 2
                  IF ( iz(i)>leq ) leq = iz(i)
               ENDDO
            ENDIF
            CALL write(file2,z(ilft),icore,0)
         ENDDO
 140     IF ( file1==ieq ) THEN
            DO i = 1 , m , 2
               IF ( iz(i)>leq ) leq = iz(i)
            ENDDO
         ENDIF
         CALL write(file2,z(ilft),m,0)
         CALL close(file1,1)
         IF ( n==buf5 ) THEN
!
!     BRING IN BGPDT
!
            CALL gopen(bgpdt,z(buf6),0)
            file1 = bgpdt
            CALL read(*260,*260,bgpdt,z(1),nrd,0,n)
            CALL close(bgpdt,1)
!
!     FINALLY TIME TO GO TO WORK
!
            nhbdy = 0
            nngp = 0
            nbgp = nrd + 1
            DO i = 1 , 24
               iz(nrd+i) = 0
            ENDDO
            file1 = ect
            CALL gopen(scr2,z(buf6),1)
         ELSE
            file1 = ieq
            file2 = oeq
            n = buf5
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         CALL read(*260,*180,ect,cbs,nect,0,n)
         cbs(nect) = 0.0
         IF ( cbs(3)>6 ) cbs(3) = 3
         CALL read(*260,*260,scr1,tem,2,0,n)
         flag = cbs(3)
         nhbdy = nhbdy + 1
         IF ( flag==2 ) THEN
!
!     LINE
!
!
!     BGPDT DATA FOR LINE
!
            i1 = (cbs(4)-1)*4 + 2
            i2 = (cbs(5)-1)*4 + 2
            CALL samb(z(i2),z(i1),r21)
            xl = sadotb(r21,r21)
            IF ( xl==0.0 ) GOTO 220
            x1 = sadotb(r21,cbs(12))
            xl = x1/xl
            e(1) = xl*r21(1)
            e(2) = xl*r21(2)
            e(3) = xl*r21(3)
            CALL samb(cbs(12),e,v)
            CALL sanorm(*220,v)
            CALL saxb(v,r21,e)
            CALL sanorm(*220,e)
            d = tem(2)
            af = tem(1)*.5
            z(nbgp+1) = z(i1) + d*v(1) - af*e(1)
            z(nbgp+2) = z(i1+1) + d*v(2) - af*e(2)
            z(nbgp+3) = z(i1+2) + d*v(3) - af*e(3)
            z(nbgp+5) = z(i2) + d*v(1) - af*e(1)
            z(nbgp+6) = z(i2+1) + d*v(2) - af*e(2)
            z(nbgp+7) = z(i2+2) + d*v(3) - af*e(3)
            z(nbgp+9) = z(i2) + d*v(1) + af*e(1)
            z(nbgp+10) = z(i2+1) + d*v(2) + af*e(2)
            z(nbgp+11) = z(i2+2) + d*v(3) + af*e(3)
            z(nbgp+13) = z(i1) + d*v(1) + af*e(1)
            z(nbgp+14) = z(i1+1) + d*v(2) + af*e(2)
            z(nbgp+15) = z(i1+2) + d*v(3) + af*e(3)
            z(nbgp+17) = z(i1) + d*v(1) + .5*r21(1)
            z(nbgp+18) = z(i1+1) + d*v(2) + .5*r21(2)
            z(nbgp+19) = z(i1+2) + d*v(3) + .5*r21(3)
            z(nbgp+21) = z(nbgp+17) + 2.*af*v(1)
            z(nbgp+22) = z(nbgp+18) + 2.*af*v(2)
            z(nbgp+23) = z(nbgp+19) + 2.*af*v(3)
            nngp = nngp + 6
            ns = 6
            nea = 12
            nb = 24
            m = 6
!
!     REV  OR  ELIP   DO NOTHING
!
            n = 6
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( flag==3 .OR. flag==6 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( flag==4 ) THEN
!
!     AREA3
!
!     BGPDT DATA FOR AREA3
!
            i1 = (cbs(4)-1)*4 + 2
            i2 = (cbs(5)-1)*4 + 2
            i3 = (cbs(6)-1)*4 + 2
            CALL samb(z(i2),z(i1),e)
            CALL samb(z(i3),z(i1),v)
            CALL saxb(e,v,e)
            CALL sanorm(*220,e)
            CALL samb(z(i2),z(i1),v)
            x1 = sadotb(v,v)
            CALL samb(z(i3),z(i1),v)
            x2 = sadotb(v,v)
            CALL samb(z(i3),z(i2),v)
            x3 = sadotb(v,v)
            x1 = amax1(x1,x2)
            x1 = amax1(x1,x3)
            xl = .25*sqrt(x1)
            CALL sapb(z(i1),z(i2),v)
            CALL sapb(z(i3),v,v)
            z(nbgp+1) = v(1)/3.0
            z(nbgp+2) = v(2)/3.0
            z(nbgp+3) = v(3)/3.0
            z(nbgp+5) = z(nbgp+1) + xl*e(1)
            z(nbgp+6) = z(nbgp+2) + xl*e(2)
            z(nbgp+7) = z(nbgp+3) + xl*e(3)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
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
            CALL samb(z(i3),z(i1),e)
            CALL samb(z(i4),z(i2),v)
            CALL saxb(e,v,e)
            CALL sanorm(*220,e)
            CALL samb(z(i2),z(i1),v)
            x1 = sadotb(v,v)
            CALL samb(z(i3),z(i2),v)
            x2 = sadotb(v,v)
            CALL samb(z(i4),z(i3),v)
            x3 = sadotb(v,v)
            CALL samb(z(i4),z(i1),v)
            x4 = sadotb(v,v)
            x1 = amax1(x1,x2)
            x1 = amax1(x1,x3)
            x1 = amax1(x1,x4)
            xl = .25*sqrt(x1)
            CALL sapb(z(i1),z(i2),v)
            CALL sapb(v,z(i3),v)
            CALL sapb(v,z(i4),v)
            z(nbgp+1) = .25*v(1)
            z(nbgp+2) = .25*v(2)
            z(nbgp+3) = .25*v(3)
            z(nbgp+5) = z(nbgp+1) + xl*e(1)
            z(nbgp+6) = z(nbgp+2) + xl*e(2)
            z(nbgp+7) = z(nbgp+3) + xl*e(3)
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
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
            CALL sanorm(*220,v)
            e(1) = 1.0
            SPAG_Loop_1_3: DO
               xl = sadotb(v,e)
               r21(1) = e(1) - xl*v(1)
               r21(2) = e(2) - xl*v(2)
               r21(3) = e(3) - xl*v(3)
               xl = sadotb(r21,r21)
               IF ( xl>.2 ) THEN
                  CALL sanorm(*160,r21)
                  EXIT SPAG_Loop_1_3
               ELSE
                  IF ( itry==2 ) GOTO 220
                  itry = 2
                  e(1) = 0.0
                  e(2) = 1.0
               ENDIF
            ENDDO SPAG_Loop_1_3
         ENDIF
 160     CALL saxb(v,r21,e)
         xl = 0.0
         IF ( tem(1)/=0.0 ) xl = sqrt(tem(1)/pi)
         zs3 = .8660254
         z(nbgp+1) = z(i1) + xl*r21(1)
         z(nbgp+2) = z(i1+1) + xl*r21(2)
         z(nbgp+3) = z(i1+2) + xl*r21(3)
         z(nbgp+5) = z(i1) + xl*(.5*r21(1)+zs3*e(1))
         z(nbgp+6) = z(i1+1) + xl*(.5*r21(2)+zs3*e(2))
         z(nbgp+7) = z(i1+2) + xl*(.5*r21(3)+zs3*e(3))
         z(nbgp+9) = z(i1) + xl*(-.5*r21(1)+zs3*e(1))
         z(nbgp+10) = z(i1+1) + xl*(-.5*r21(2)+zs3*e(2))
         z(nbgp+11) = z(i1+2) + xl*(-.5*r21(3)+zs3*e(3))
         z(nbgp+14) = z(i1+1) - xl*r21(2)
         z(nbgp+15) = z(i1+2) - xl*r21(3)
         z(nbgp+17) = z(i1) + xl*(-.5*r21(1)-zs3*e(1))
         z(nbgp+18) = z(i1+1) + xl*(-.5*r21(2)-zs3*e(2))
         z(nbgp+19) = z(i1+2) + xl*(-.5*r21(3)-zs3*e(3))
         z(nbgp+21) = z(i1) + xl*(+.5*r21(1)-zs3*e(1))
         z(nbgp+22) = z(i1+1) + xl*(+.5*r21(2)-zs3*e(2))
         z(nbgp+23) = z(i1+2) + xl*(+.5*r21(3)-zs3*e(3))
         z(nbgp+25) = z(i1) + xl*v(1)
         z(nbgp+26) = z(i1+1) + xl*v(2)
         z(nbgp+27) = z(i1+2) + xl*v(3)
         nngp = nngp + 7
         ns = 7
         nea = 14
         nb = 28
         m = 7
         n = 5
         spag_nextblock_1 = 6
      CASE (6)
         nn = 1
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
         spag_nextblock_1 = 8
      CASE (7)
         nngp = nngp + 2
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
         spag_nextblock_1 = 8
      CASE (8)
!
!     ADD TO HSIL HEQEXIN  HECT
!     BGPDT
!
         CALL write(hsil,nsil,ns,0)
         CALL write(oeq,neq,nea,0)
         CALL write(scr2,z(nbgp),nb,0)
         spag_nextblock_1 = 9
      CASE (9)
         cbs(3) = -cbs(3)
         CALL write(hect,cbs,nect,0)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     END CLOSE FILES, WRITE NBGPDT, WRITE TRAILERS THEN FINISH ECT COPY
!
 180     CALL write(hsil,0,0,1)
         CALL write(oeq,0,0,1)
         CALL write(hect,0,0,1)
         CALL write(scr2,0,0,1)
         CALL close(scr2,1)
         CALL close(scr1,1)
         CALL close(hsil,1)
         CALL close(oeq,1)
         CALL gopen(hbgpdt,z(buf1),1)
         CALL write(hbgpdt,z,nrd,0)
         IF ( nngp==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         file1 = scr2
         CALL gopen(scr2,z(buf6),0)
         DO
            CALL read(*260,*200,scr2,z(1),buf6-1,0,n)
            CALL write(hbgpdt,z(1),buf6-1,0)
         ENDDO
 200     CALL write(hbgpdt,z(1),n,1)
         CALL close(scr2,1)
         spag_nextblock_1 = 10
      CASE (10)
         CALL close(hbgpdt,1)
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
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
!     BAD GEOMETRY FOR ELEMENT
!
 220     cbs(3) = -cbs(3)
         nhbdy = nhbdy - 1
         WRITE (out,99003) uwm , cbs(1)
99003    FORMAT (A25,', CHBDY ELEMENT',I9,' HAS NO NORMAL OR BAD GEOMETRY',' WHICH MAKES IT UNPLOTTABLE')
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
!     RETURN OR ERROR MESSAGES
!
 240     CALL close(ect,1)
         CALL close(hect,1)
         CALL close(geom2,1)
         RETURN
!
 260     CALL mesage(-2,0,file1)
         spag_nextblock_1 = 11
      CASE (11)
         CALL mesage(-8,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pthbdy
