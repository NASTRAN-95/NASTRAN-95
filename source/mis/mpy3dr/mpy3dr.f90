!*==mpy3dr.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mpy3dr(Z)
!
!     SECONDARY DRIVER IF MPY3DR IS CALLED BY MPY3
!     PRIMARY   DRIVER IF CALLED BY OTHERS (COMB2 AND MRED2 GROUP)
!
!     SETS UP OPEN CORE AND DETERMINES SOLUTION METHOD.
!
   IMPLICIT NONE
   USE c_logout
   USE c_mpy3cp
   USE c_mpy3tl
   USE c_mpyadx
   USE c_ntime
   USE c_system
   USE c_xmssg
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ac , aelms , ar , bc , belms , bf , br , ec , eelms , ef , er , i , iprc , ityp , ixx , jmeth , kmeth , l19 , ttg
   REAL*8 :: dd , mm , nn , pp , xx
   INTEGER , SAVE :: jbegn , jend
   INTEGER , DIMENSION(7,3) :: mcb
   INTEGER , DIMENSION(3) , SAVE :: mpy
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: rhoa , rhob , rhoe , tcol , timem1 , timem2 , timem3
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
!WKBI 4/94
   !>>>>EQUIVALENCE (Ac,Filea(2)) , (Ar,Filea(3)) , (Bc,Fileb(2)) , (Br,Fileb(3)) , (Bf,Fileb(4)) , (Ec,Filee(2)) , (Er,Filee(3)) ,      &
!>>>>    & (Ef,Filee(4))
   !>>>>EQUIVALENCE (Mcb(1,1),Filea(1))
   DATA name/4HMPY3 , 4HDR  /
   DATA mpy/4HMPY3 , 4H     , 4H    /
   DATA jbegn , jend/4HBEGN , 4HEND /
!
!     RETURN IF EITHER A OR B IS PURGED
!
   IF ( filea(1)<0 ) RETURN
   IF ( fileb(1)<0 ) RETURN
!
!     TEST FOR MATRIX COMPATABILITY.
!
   mpy(3) = jbegn
   CALL conmsg(mpy,3,0)
!
   scr(1) = scr3
   IF ( code==0 ) THEN
      IF ( bf==2 .OR. bf==7 ) THEN
!
!    ERROR MESSAGES.
!
         WRITE (nout,99001) ufm
99001    FORMAT (A23,'6551, MATRIX B IN MPY3 IS NOT SQUARE FOR A(T)BA + E',' PROBLEM.')
         CALL mesage(-37,0,name)
         GOTO 100
      ENDIF
   ENDIF
   IF ( ar/=br .AND. code==1 ) THEN
      WRITE (nout,99002) ufm
99002 FORMAT (A23,' 6552, NO. OF ROWS OF MATRIX A IN MPY3 IS UNEQUAL TO',/5X,'NO. OF ROWS OF MATRIX B FOR A(T)B + E PROBLEM.')
      CALL mesage(-37,0,name)
   ELSE
      IF ( ar/=bc .AND. code/=1 ) THEN
         WRITE (nout,99003) ufm
99003    FORMAT (A23,' 6553, NO. OF ROWS OF MATRIX A IN MPY3 IS UNEQUAL TO'/5X,'NO. OF COLUMNS OF MATRIX B FOR A(T)BA + E PROBLEM.')
         CALL mesage(-37,0,name)
         GOTO 100
      ELSE
         IF ( filee(1)<=0 ) THEN
!
            e = .FALSE.
            DO i = 1 , 7
               filee(i) = 0
            ENDDO
         ELSE
            e = .TRUE.
            IF ( code==0 ) THEN
               IF ( ef==2 .OR. ef==7 ) THEN
                  WRITE (nout,99004) ufm
99004             FORMAT (A23,' 6555, MATRIX E IN MPY3 IS NOT SQUARE FOR A(T)BA + ','E PROBLEM.')
                  CALL mesage(-37,0,name)
                  GOTO 100
               ENDIF
            ENDIF
            IF ( ec/=bc .AND. code==1 ) THEN
               WRITE (nout,99005) ufm
99005          FORMAT (A23,' 6524, NO. OF COLUMNS OF MATRIX E IN MPY3 IS UNEQUAL',' TO',/5X,                                        &
                      &'NO. OF COLUMNS OF MATRIX B FOR A(T)B + E ','PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ELSEIF ( ec/=ac .AND. code/=1 ) THEN
               WRITE (nout,99006) ufm
99006          FORMAT (A23,' 6554, NO. OF COLUMNS OF MATRIX E IN MPY3 IS UNEQUAL',/5X,                                              &
                      &'TO NO. OF COLUMNS OF MATRIX A FOR A(T)BA +E PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ELSEIF ( er/=ac .AND. code==1 ) THEN
               WRITE (nout,99007) ufm
99007          FORMAT (A23,' 6559, NO. OF ROWS OF MATRIX E IN MPY3 IS UNEQUAL TO',/5X,                                              &
                      &'NO. OF COLUMNS OF MATRIX A FOR A(T)B + E PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ELSEIF ( er/=br .AND. code==2 ) THEN
               WRITE (nout,99008) ufm
99008          FORMAT (A23,' 6556, NO. OF ROWS OF MATRIX E IN MPY3 IS UNEQUAL TO',/5X,'NO. OF ROWS OF MATRIX B FOR BA + E PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ENDIF
         ENDIF
!
!     CORE ALLOCATION.
!
         buf1 = lkore - sysbuf
         buf2 = buf1 - sysbuf
         buf3 = buf2 - sysbuf
         buf4 = buf3 - sysbuf
         lcore = buf4 - 1
         IF ( lcore<0 ) GOTO 100
!
!     IF REQUESTED CALCULATE THE OUTPUT PRECISION
!
         IF ( prec<1 .OR. prec>4 ) THEN
            iprc = 1
            ityp = 0
            DO i = 1 , 3
               IF ( mcb(5,i)==2 .OR. mcb(5,i)==4 ) iprc = 2
               IF ( mcb(5,i)>=3 ) ityp = 2
            ENDDO
            prec = ityp + iprc
            IF ( prec<=2 ) filec(5) = prec
         ENDIF
!
!     DETERMINE NK, THE NUMBER OF COLUMNS OF B MATRIX ABLE TO BE HELD
!     IN CORE.
!
         n = fileb(3)
         ncb = fileb(2)
         m = filea(2)
         d = filea(7) + 1
         maxa = filea(6)/filea(5)
!
!     (NCB SHOULD BE USED IN THE ABOVE EQUATION INSTEAD OF N. SEE
!     MPY3IC)
!
         dd = d
         nn = ncb
         mm = m
         pp = 1 + prec
         xx = dd*pp*nn*mm/10000.D0
         ixx = xx + 0.5D0
         nk = (lcore-2*ncb-ixx-prec*m-prec-(2+prec)*maxa)/(2+prec*n)
!
!     SET UP CONSTANTS IN MPYADX COMMON
!
         mscr = scr2
         mcore = lkore
         mprec = 0
         signab = 1
         signc = 1
!
!     CALCULATE PROPERTIES OF THE MATRICES
!
         rhoa = (filea(7)+1)/10000.
         rhob = (fileb(7)+1)/10000.
         rhoe = (filee(7)+1)/10000.
         aelms = ar*ac*rhoa
         belms = br*bc*rhob
         eelms = er*ec*rhoe
!
!     CALCULATE MPY3 TIME ESTIMATE - REMEMBER NO COMPLEX FOR MPY3
!
         CALL sswtch(19,l19)
         timem3 = 1.0E+10
         IF ( prec<3 ) THEN
            IF ( code/=1 ) THEN
               timem3 = (rhoa+2./float(m))*float(m)*float(n)*(float(m)+float(n))*timcon(8+prec)                                     &
                      & + (float(n)**2+float(m)**2+rhoa*float(m)*float(n)*(2.+float(m)))*timcon(5)
               timem3 = timem3/1.0E6
!WKBR 4/94 IF (L19 .NE. 0) WRITE (NOUT,50) FILEA(1),AR,AC,AELMS,RHOA,
               IF ( l19/=0 ) WRITE (lout,99009) filea(1) , ar , ac , aelms , rhoa , fileb(1) , br , bc , belms , rhob , filee(1) ,  &
                                  & er , ec , eelms , rhoe , code , lcore , nk , timem3
99009          FORMAT (50H0(A MAT  ROWS  COLS   TERMS    DENS) (B MAT  ROWS ,50H COLS   TERMS    DENS) (E MAT  ROWS  COLS   TERMS , &
                      &32H   DENS) C  CORE    NK      TIME/3(I6,I7,I6,I9,F7.4,1X),I2,I6,I6,F10.1)
!
               IF ( nk<3 .AND. code/=2 ) THEN
                  DO i = 1 , 7
                     mfilea(i) = filea(i)
                     mfilee(i) = filee(i)
                  ENDDO
                  CALL makmcb(mfileb,scr1,br,2,prec)
                  mfileb(2) = ac
                  tcol = float(belms)*float(aelms)/float(ar)/float(ac)
                  mfileb(6) = tcol + 1.0
                  mfileb(7) = tcol/br*1.0E+4
                  mfilec(1) = -1
                  mfilec(5) = prec
                  mt = 1
                  CALL mpyad(Z(1),Z(1),Z(1))
                  timem3 = timem3 + timem
               ENDIF
!
!WKBR 4/94 70 WRITE  (NOUT,80) UIM,TIMEM3
               WRITE (lout,99010) uim , timem3
99010          FORMAT (A29,' 6525, TRIPLE MULTIPLY TIME ESTIMATE FOR MPY3 = ',F10.1,' SECONDS.')
            ENDIF
         ENDIF
!
!     CALCULATE MPYAD TIME ESTIMATE FOR (AT*B)*A + E
!
         timem1 = 1.0E+10
         IF ( code/=2 ) THEN
            DO i = 1 , 7
               mfilea(i) = filea(i)
               mfileb(i) = fileb(i)
               IF ( code==1 ) mfilee(i) = filee(i)
               IF ( code/=1 ) mfilee(i) = 0
            ENDDO
            CALL makmcb(mfilec,-1,ac,2,prec)
            mt = 1
            CALL mpyad(Z(1),Z(1),Z(1))
            timem1 = timem
            IF ( code/=1 ) THEN
!
               DO i = 1 , 7
                  mfileb(i) = mfilea(i)
                  mfilea(i) = mfilec(i)
                  mfilee(i) = filee(i)
               ENDDO
               mfilea(1) = scr1
               mfilea(2) = bc
               tcol = float(belms)*float(aelms)/float(ar)/float(bc)
               mfilea(6) = tcol + 1.0
               mfilea(7) = tcol/ac*1.0E+4
               mt = 0
               CALL mpyad(Z(1),Z(1),Z(1))
               timem1 = timem1 + timem
            ENDIF
!
!WKBR 4/94  130 WRITE  (NOUT,140) UIM,TIMEM1
            WRITE (lout,99011) uim , timem1
99011       FORMAT (A29,' 6525, TRIPLE MULTIPLY TIME ESTIMATE FOR MPYAD - ','(AT*B)*A + E = ',F10.1,' SECONDS.')
         ENDIF
!
!     CALCULATE MPYAD TIME ESTIMATE FOR AT*(B*A) + E
!
         timem2 = 1.0E+10
         IF ( code/=1 ) THEN
            DO i = 1 , 7
               mfilea(i) = fileb(i)
               mfileb(i) = filea(i)
               IF ( code==2 ) mfilee(i) = filee(i)
               IF ( code/=2 ) mfilee(i) = 0
            ENDDO
            CALL makmcb(mfilec,-1,br,2,prec)
            mt = 0
            CALL mpyad(Z(1),Z(1),Z(1))
            timem2 = timem
            IF ( code/=2 ) THEN
!
               DO i = 1 , 7
                  mfilea(i) = mfileb(i)
                  mfileb(i) = mfilec(i)
                  mfilee(i) = filee(i)
               ENDDO
               mfileb(1) = scr1
               mfileb(2) = ac
               tcol = float(belms)*float(aelms)/float(ar)/float(ac)
               mfileb(6) = tcol + 1.0
               mfileb(7) = tcol/br*1.0E+4
               mt = 1
               CALL mpyad(Z(1),Z(1),Z(1))
               timem2 = timem2 + timem
            ENDIF
!
!WKBR 4/94 230 WRITE  (NOUT,240) UIM,TIMEM2
            WRITE (lout,99012) uim , timem2
99012       FORMAT (A29,' 6525, TRIPLE MULTIPLY TIME ESTIMATE FOR MPYAD - ','AT*(B*A) + E = ',F10.1,' SECONDS.')
         ENDIF
!
!     CHOOSE METHOD BASED ON THE BEST TIME ESTIMATE OR USER REQUEST
!
         CALL tmtogo(ttg)
         IF ( float(ttg)<=1.2*amin1(timem3,timem1,timem2) ) THEN
            WRITE (nout,99013) ufm
99013       FORMAT (A23,' 6558, INSUFFICIENT TIME REMAINING FOR MPY3 ','EXECUTION.')
            CALL mesage(-37,0,name)
            GOTO 100
         ELSE
            diag = andf(diag,complf(lshift(1,18)))
            kmeth = meth
            jmeth = meth
            meth = 0
            IF ( jmeth<1 .OR. jmeth>3 ) jmeth = 0
            IF ( jmeth==1 .AND. code==2 ) jmeth = 0
            IF ( jmeth==2 .AND. code==1 ) jmeth = 0
            IF ( jmeth==3 .AND. code==1 ) jmeth = 0
            IF ( jmeth/=0 ) THEN
               IF ( jmeth==1 ) GOTO 40
               IF ( jmeth==2 ) GOTO 50
               IF ( jmeth==3 ) GOTO 20
            ENDIF
            filec(4) = fileb(4)
!
            IF ( timem3>=timem1 .OR. timem3>=timem2 ) THEN
               IF ( timem1>=timem2 ) GOTO 50
               GOTO 40
            ENDIF
         ENDIF
!
!     PERFORM MULTIPLY WITH MPY3
!
 20      IF ( nk<3 ) THEN
!
!     OUT OF CORE PROCESSING FOR MPY3
!
            icore = 1
!WKBR 4/94      WRITE  (NOUT,320) UIM
            WRITE (lout,99014) uim
99014       FORMAT (A29,' 6526,  THE CENTER MATRIX IS TOO LARGE FOR',/5X,'IN-CORE PROCESSING.  OUT-OF-CORE PROCESSING WILL BE ',    &
                   &'PERFORMED.')
!
            nk = (lcore-4*ncb-prec*m-(2+prec)*maxa)/(2+prec*n)
            CALL mpy3oc(Z(1),Z(1),Z(1))
            filec(4) = fileb(4)
         ELSE
            icore = 0
            CALL mpy3ic(Z(1),Z(1),Z(1))
         ENDIF
         GOTO 200
!
!     PERFORM MULTIPLY WITH MPYAD DOING (AT * B) FIRST
!
 40      DO i = 1 , 7
            mfilea(i) = filea(i)
            mfileb(i) = fileb(i)
            IF ( code==1 ) mfilee(i) = filee(i)
            IF ( code/=1 ) mfilee(i) = 0
         ENDDO
         CALL makmcb(mfilec,scr1,ac,2,prec)
         IF ( code==1 ) mfilec(1) = filec(1)
         mt = 1
         CALL mpyad(Z(1),Z(1),Z(1))
         IF ( code/=1 ) THEN
            CALL wrttrl(mfilec)
!
            DO i = 1 , 7
               mfileb(i) = mfilea(i)
               mfilea(i) = mfilec(i)
               mfilee(i) = filee(i)
            ENDDO
            CALL makmcb(mfilec,filec(1),ac,fileb(4),prec)
            mt = 0
            CALL mpyad(Z(1),Z(1),Z(1))
         ENDIF
         DO i = 1 , 7
            filec(i) = mfilec(i)
         ENDDO
         GOTO 200
      ENDIF
!
!     PERFORM MULTIPLY WITH MPYAD DOING (B*A) FIRST
!
 50   DO i = 1 , 7
         mfilea(i) = fileb(i)
         mfileb(i) = filea(i)
         IF ( code==2 ) mfilee(i) = filee(i)
         IF ( code/=2 ) mfilee(i) = 0
      ENDDO
      CALL makmcb(mfilec,scr1,br,2,prec)
      IF ( code==2 ) mfilec(1) = filec(1)
      mt = 0
      CALL mpyad(Z(1),Z(1),Z(1))
      IF ( code/=2 ) THEN
         CALL wrttrl(mfilec)
!
         DO i = 1 , 7
            mfilea(i) = mfileb(i)
            mfileb(i) = mfilec(i)
            mfilee(i) = filee(i)
         ENDDO
         CALL makmcb(mfilec,filec(1),ac,fileb(4),prec)
         mt = 1
         CALL mpyad(Z(1),Z(1),Z(1))
      ENDIF
      DO i = 1 , 7
         filec(i) = mfilec(i)
      ENDDO
      GOTO 200
   ENDIF
 100  CALL mesage(-8,0,name)
!
!     RETURN
!
 200  diag = orf(diag,lshift(l19,18))
   meth = kmeth
   mpy(3) = jend
   CALL conmsg(mpy,3,0)
END SUBROUTINE mpy3dr
