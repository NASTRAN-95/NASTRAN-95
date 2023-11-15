
SUBROUTINE mpy3dr(Z)
!
!     SECONDARY DRIVER IF MPY3DR IS CALLED BY MPY3
!     PRIMARY   DRIVER IF CALLED BY OTHERS (COMB2 AND MRED2 GROUP)
!
!     SETS UP OPEN CORE AND DETERMINES SOLUTION METHOD.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ac , Ar , Bc , Bf , Br , Buf1 , Buf2 , Buf3 , Buf4 , Code , D , Diag , Dum1(22) , Dum2(32) , Dumcp(34) , Ec , Ef , Er ,  &
         & Filea(7) , Fileb(7) , Filec(7) , Filee(7) , Icore , Itrl , Lcore , Lkore , Lout , M , Maxa , Mcb(7,3) , Mcore , Meth ,   &
         & Mfilea(7) , Mfileb(7) , Mfilec(7) , Mfilee(7) , Mprec , Mscr , Mt , N , Ncb , Nk , Nout , Prec , Scr(7) , Scr1 , Scr2 ,  &
         & Scr3 , Signab , Signc , Sysbuf
   LOGICAL E
   REAL Timcon(16) , Timem
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /logout/ Lout
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , M , Nk , D , Maxa , Dumcp
   COMMON /mpy3tl/ Filea , Fileb , Filee , Filec , Scr1 , Scr2 , Scr3 , Lkore , Code , Prec , Lcore , Scr , Buf1 , Buf2 , Buf3 ,    &
                 & Buf4 , E
   COMMON /mpyadx/ Mfilea , Mfileb , Mfilee , Mfilec , Mcore , Mt , Signab , Signc , Mprec , Mscr , Timem
   COMMON /ntime / Timcon
   COMMON /system/ Sysbuf , Nout , Dum1 , Diag , Dum2 , Meth
   COMMON /xmssg / Ufm , Uwm , Uim
!
! Dummy argument declarations
!
   INTEGER Z(1)
!
! Local variable declarations
!
   INTEGER aelms , belms , eelms , i , iprc , ityp , ixx , jbegn , jend , jmeth , kmeth , l19 , mpy(3) , name(2) , ttg
   INTEGER andf , complf , lshift , orf
   DOUBLE PRECISION dd , mm , nn , pp , xx
   REAL rhoa , rhob , rhoe , tcol , timem1 , timem2 , timem3
   EXTERNAL andf , complf , lshift , orf
!
! End of declarations
!
!WKBI 4/94
   EQUIVALENCE (Ac,Filea(2)) , (Ar,Filea(3)) , (Bc,Fileb(2)) , (Br,Fileb(3)) , (Bf,Fileb(4)) , (Ec,Filee(2)) , (Er,Filee(3)) ,      &
    & (Ef,Filee(4))
   EQUIVALENCE (Mcb(1,1),Filea(1))
   DATA name/4HMPY3 , 4HDR  /
   DATA mpy/4HMPY3 , 4H     , 4H    /
   DATA jbegn , jend/4HBEGN , 4HEND /
!
!     RETURN IF EITHER A OR B IS PURGED
!
   IF ( Filea(1)<0 ) RETURN
   IF ( Fileb(1)<0 ) RETURN
!
!     TEST FOR MATRIX COMPATABILITY.
!
   mpy(3) = jbegn
   CALL conmsg(mpy,3,0)
!
   Scr(1) = Scr3
   IF ( Code==0 ) THEN
      IF ( Bf==2 .OR. Bf==7 ) THEN
!
!    ERROR MESSAGES.
!
         WRITE (Nout,99001) Ufm
99001    FORMAT (A23,'6551, MATRIX B IN MPY3 IS NOT SQUARE FOR A(T)BA + E',' PROBLEM.')
         CALL mesage(-37,0,name)
         GOTO 100
      ENDIF
   ENDIF
   IF ( Ar/=Br .AND. Code==1 ) THEN
      WRITE (Nout,99002) Ufm
99002 FORMAT (A23,' 6552, NO. OF ROWS OF MATRIX A IN MPY3 IS UNEQUAL TO',/5X,'NO. OF ROWS OF MATRIX B FOR A(T)B + E PROBLEM.')
      CALL mesage(-37,0,name)
   ELSE
      IF ( Ar/=Bc .AND. Code/=1 ) THEN
         WRITE (Nout,99003) Ufm
99003    FORMAT (A23,' 6553, NO. OF ROWS OF MATRIX A IN MPY3 IS UNEQUAL TO'/5X,'NO. OF COLUMNS OF MATRIX B FOR A(T)BA + E PROBLEM.')
         CALL mesage(-37,0,name)
         GOTO 100
      ELSE
         IF ( Filee(1)<=0 ) THEN
!
            E = .FALSE.
            DO i = 1 , 7
               Filee(i) = 0
            ENDDO
         ELSE
            E = .TRUE.
            IF ( Code==0 ) THEN
               IF ( Ef==2 .OR. Ef==7 ) THEN
                  WRITE (Nout,99004) Ufm
99004             FORMAT (A23,' 6555, MATRIX E IN MPY3 IS NOT SQUARE FOR A(T)BA + ','E PROBLEM.')
                  CALL mesage(-37,0,name)
                  GOTO 100
               ENDIF
            ENDIF
            IF ( Ec/=Bc .AND. Code==1 ) THEN
               WRITE (Nout,99005) Ufm
99005          FORMAT (A23,' 6524, NO. OF COLUMNS OF MATRIX E IN MPY3 IS UNEQUAL',' TO',/5X,                                        &
                      &'NO. OF COLUMNS OF MATRIX B FOR A(T)B + E ','PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ELSEIF ( Ec/=Ac .AND. Code/=1 ) THEN
               WRITE (Nout,99006) Ufm
99006          FORMAT (A23,' 6554, NO. OF COLUMNS OF MATRIX E IN MPY3 IS UNEQUAL',/5X,                                              &
                      &'TO NO. OF COLUMNS OF MATRIX A FOR A(T)BA +E PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ELSEIF ( Er/=Ac .AND. Code==1 ) THEN
               WRITE (Nout,99007) Ufm
99007          FORMAT (A23,' 6559, NO. OF ROWS OF MATRIX E IN MPY3 IS UNEQUAL TO',/5X,                                              &
                      &'NO. OF COLUMNS OF MATRIX A FOR A(T)B + E PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ELSEIF ( Er/=Br .AND. Code==2 ) THEN
               WRITE (Nout,99008) Ufm
99008          FORMAT (A23,' 6556, NO. OF ROWS OF MATRIX E IN MPY3 IS UNEQUAL TO',/5X,'NO. OF ROWS OF MATRIX B FOR BA + E PROBLEM.')
               CALL mesage(-37,0,name)
               GOTO 100
            ENDIF
         ENDIF
!
!     CORE ALLOCATION.
!
         Buf1 = Lkore - Sysbuf
         Buf2 = Buf1 - Sysbuf
         Buf3 = Buf2 - Sysbuf
         Buf4 = Buf3 - Sysbuf
         Lcore = Buf4 - 1
         IF ( Lcore<0 ) GOTO 100
!
!     IF REQUESTED CALCULATE THE OUTPUT PRECISION
!
         IF ( Prec<1 .OR. Prec>4 ) THEN
            iprc = 1
            ityp = 0
            DO i = 1 , 3
               IF ( Mcb(5,i)==2 .OR. Mcb(5,i)==4 ) iprc = 2
               IF ( Mcb(5,i)>=3 ) ityp = 2
            ENDDO
            Prec = ityp + iprc
            IF ( Prec<=2 ) Filec(5) = Prec
         ENDIF
!
!     DETERMINE NK, THE NUMBER OF COLUMNS OF B MATRIX ABLE TO BE HELD
!     IN CORE.
!
         N = Fileb(3)
         Ncb = Fileb(2)
         M = Filea(2)
         D = Filea(7) + 1
         Maxa = Filea(6)/Filea(5)
!
!     (NCB SHOULD BE USED IN THE ABOVE EQUATION INSTEAD OF N. SEE
!     MPY3IC)
!
         dd = D
         nn = Ncb
         mm = M
         pp = 1 + Prec
         xx = dd*pp*nn*mm/10000.D0
         ixx = xx + 0.5D0
         Nk = (Lcore-2*Ncb-ixx-Prec*M-Prec-(2+Prec)*Maxa)/(2+Prec*N)
!
!     SET UP CONSTANTS IN MPYADX COMMON
!
         Mscr = Scr2
         Mcore = Lkore
         Mprec = 0
         Signab = 1
         Signc = 1
!
!     CALCULATE PROPERTIES OF THE MATRICES
!
         rhoa = (Filea(7)+1)/10000.
         rhob = (Fileb(7)+1)/10000.
         rhoe = (Filee(7)+1)/10000.
         aelms = Ar*Ac*rhoa
         belms = Br*Bc*rhob
         eelms = Er*Ec*rhoe
!
!     CALCULATE MPY3 TIME ESTIMATE - REMEMBER NO COMPLEX FOR MPY3
!
         CALL sswtch(19,l19)
         timem3 = 1.0E+10
         IF ( Prec<3 ) THEN
            IF ( Code/=1 ) THEN
               timem3 = (rhoa+2./float(M))*float(M)*float(N)*(float(M)+float(N))*Timcon(8+Prec)                                     &
                      & + (float(N)**2+float(M)**2+rhoa*float(M)*float(N)*(2.+float(M)))*Timcon(5)
               timem3 = timem3/1.0E6
!WKBR 4/94 IF (L19 .NE. 0) WRITE (NOUT,50) FILEA(1),AR,AC,AELMS,RHOA,
               IF ( l19/=0 ) WRITE (Lout,99009) Filea(1) , Ar , Ac , aelms , rhoa , Fileb(1) , Br , Bc , belms , rhob , Filee(1) ,  &
                                  & Er , Ec , eelms , rhoe , Code , Lcore , Nk , timem3
99009          FORMAT (50H0(A MAT  ROWS  COLS   TERMS    DENS) (B MAT  ROWS ,50H COLS   TERMS    DENS) (E MAT  ROWS  COLS   TERMS , &
                      &32H   DENS) C  CORE    NK      TIME/3(I6,I7,I6,I9,F7.4,1X),I2,I6,I6,F10.1)
!
               IF ( Nk<3 .AND. Code/=2 ) THEN
                  DO i = 1 , 7
                     Mfilea(i) = Filea(i)
                     Mfilee(i) = Filee(i)
                  ENDDO
                  CALL makmcb(Mfileb,Scr1,Br,2,Prec)
                  Mfileb(2) = Ac
                  tcol = float(belms)*float(aelms)/float(Ar)/float(Ac)
                  Mfileb(6) = tcol + 1.0
                  Mfileb(7) = tcol/Br*1.0E+4
                  Mfilec(1) = -1
                  Mfilec(5) = Prec
                  Mt = 1
                  CALL mpyad(Z(1),Z(1),Z(1))
                  timem3 = timem3 + Timem
               ENDIF
!
!WKBR 4/94 70 WRITE  (NOUT,80) UIM,TIMEM3
               WRITE (Lout,99010) Uim , timem3
99010          FORMAT (A29,' 6525, TRIPLE MULTIPLY TIME ESTIMATE FOR MPY3 = ',F10.1,' SECONDS.')
            ENDIF
         ENDIF
!
!     CALCULATE MPYAD TIME ESTIMATE FOR (AT*B)*A + E
!
         timem1 = 1.0E+10
         IF ( Code/=2 ) THEN
            DO i = 1 , 7
               Mfilea(i) = Filea(i)
               Mfileb(i) = Fileb(i)
               IF ( Code==1 ) Mfilee(i) = Filee(i)
               IF ( Code/=1 ) Mfilee(i) = 0
            ENDDO
            CALL makmcb(Mfilec,-1,Ac,2,Prec)
            Mt = 1
            CALL mpyad(Z(1),Z(1),Z(1))
            timem1 = Timem
            IF ( Code/=1 ) THEN
!
               DO i = 1 , 7
                  Mfileb(i) = Mfilea(i)
                  Mfilea(i) = Mfilec(i)
                  Mfilee(i) = Filee(i)
               ENDDO
               Mfilea(1) = Scr1
               Mfilea(2) = Bc
               tcol = float(belms)*float(aelms)/float(Ar)/float(Bc)
               Mfilea(6) = tcol + 1.0
               Mfilea(7) = tcol/Ac*1.0E+4
               Mt = 0
               CALL mpyad(Z(1),Z(1),Z(1))
               timem1 = timem1 + Timem
            ENDIF
!
!WKBR 4/94  130 WRITE  (NOUT,140) UIM,TIMEM1
            WRITE (Lout,99011) Uim , timem1
99011       FORMAT (A29,' 6525, TRIPLE MULTIPLY TIME ESTIMATE FOR MPYAD - ','(AT*B)*A + E = ',F10.1,' SECONDS.')
         ENDIF
!
!     CALCULATE MPYAD TIME ESTIMATE FOR AT*(B*A) + E
!
         timem2 = 1.0E+10
         IF ( Code/=1 ) THEN
            DO i = 1 , 7
               Mfilea(i) = Fileb(i)
               Mfileb(i) = Filea(i)
               IF ( Code==2 ) Mfilee(i) = Filee(i)
               IF ( Code/=2 ) Mfilee(i) = 0
            ENDDO
            CALL makmcb(Mfilec,-1,Br,2,Prec)
            Mt = 0
            CALL mpyad(Z(1),Z(1),Z(1))
            timem2 = Timem
            IF ( Code/=2 ) THEN
!
               DO i = 1 , 7
                  Mfilea(i) = Mfileb(i)
                  Mfileb(i) = Mfilec(i)
                  Mfilee(i) = Filee(i)
               ENDDO
               Mfileb(1) = Scr1
               Mfileb(2) = Ac
               tcol = float(belms)*float(aelms)/float(Ar)/float(Ac)
               Mfileb(6) = tcol + 1.0
               Mfileb(7) = tcol/Br*1.0E+4
               Mt = 1
               CALL mpyad(Z(1),Z(1),Z(1))
               timem2 = timem2 + Timem
            ENDIF
!
!WKBR 4/94 230 WRITE  (NOUT,240) UIM,TIMEM2
            WRITE (Lout,99012) Uim , timem2
99012       FORMAT (A29,' 6525, TRIPLE MULTIPLY TIME ESTIMATE FOR MPYAD - ','AT*(B*A) + E = ',F10.1,' SECONDS.')
         ENDIF
!
!     CHOOSE METHOD BASED ON THE BEST TIME ESTIMATE OR USER REQUEST
!
         CALL tmtogo(ttg)
         IF ( float(ttg)<=1.2*amin1(timem3,timem1,timem2) ) THEN
            WRITE (Nout,99013) Ufm
99013       FORMAT (A23,' 6558, INSUFFICIENT TIME REMAINING FOR MPY3 ','EXECUTION.')
            CALL mesage(-37,0,name)
            GOTO 100
         ELSE
            Diag = andf(Diag,complf(lshift(1,18)))
            kmeth = Meth
            jmeth = Meth
            Meth = 0
            IF ( jmeth<1 .OR. jmeth>3 ) jmeth = 0
            IF ( jmeth==1 .AND. Code==2 ) jmeth = 0
            IF ( jmeth==2 .AND. Code==1 ) jmeth = 0
            IF ( jmeth==3 .AND. Code==1 ) jmeth = 0
            IF ( jmeth/=0 ) THEN
               IF ( jmeth==1 ) GOTO 40
               IF ( jmeth==2 ) GOTO 50
               IF ( jmeth==3 ) GOTO 20
            ENDIF
            Filec(4) = Fileb(4)
!
            IF ( timem3>=timem1 .OR. timem3>=timem2 ) THEN
               IF ( timem1>=timem2 ) GOTO 50
               GOTO 40
            ENDIF
         ENDIF
!
!     PERFORM MULTIPLY WITH MPY3
!
 20      IF ( Nk<3 ) THEN
!
!     OUT OF CORE PROCESSING FOR MPY3
!
            Icore = 1
!WKBR 4/94      WRITE  (NOUT,320) UIM
            WRITE (Lout,99014) Uim
99014       FORMAT (A29,' 6526,  THE CENTER MATRIX IS TOO LARGE FOR',/5X,'IN-CORE PROCESSING.  OUT-OF-CORE PROCESSING WILL BE ',    &
                   &'PERFORMED.')
!
            Nk = (Lcore-4*Ncb-Prec*M-(2+Prec)*Maxa)/(2+Prec*N)
            CALL mpy3oc(Z(1),Z(1),Z(1))
            Filec(4) = Fileb(4)
         ELSE
            Icore = 0
            CALL mpy3ic(Z(1),Z(1),Z(1))
         ENDIF
         GOTO 200
!
!     PERFORM MULTIPLY WITH MPYAD DOING (AT * B) FIRST
!
 40      DO i = 1 , 7
            Mfilea(i) = Filea(i)
            Mfileb(i) = Fileb(i)
            IF ( Code==1 ) Mfilee(i) = Filee(i)
            IF ( Code/=1 ) Mfilee(i) = 0
         ENDDO
         CALL makmcb(Mfilec,Scr1,Ac,2,Prec)
         IF ( Code==1 ) Mfilec(1) = Filec(1)
         Mt = 1
         CALL mpyad(Z(1),Z(1),Z(1))
         IF ( Code/=1 ) THEN
            CALL wrttrl(Mfilec)
!
            DO i = 1 , 7
               Mfileb(i) = Mfilea(i)
               Mfilea(i) = Mfilec(i)
               Mfilee(i) = Filee(i)
            ENDDO
            CALL makmcb(Mfilec,Filec(1),Ac,Fileb(4),Prec)
            Mt = 0
            CALL mpyad(Z(1),Z(1),Z(1))
         ENDIF
         DO i = 1 , 7
            Filec(i) = Mfilec(i)
         ENDDO
         GOTO 200
      ENDIF
!
!     PERFORM MULTIPLY WITH MPYAD DOING (B*A) FIRST
!
 50   DO i = 1 , 7
         Mfilea(i) = Fileb(i)
         Mfileb(i) = Filea(i)
         IF ( Code==2 ) Mfilee(i) = Filee(i)
         IF ( Code/=2 ) Mfilee(i) = 0
      ENDDO
      CALL makmcb(Mfilec,Scr1,Br,2,Prec)
      IF ( Code==2 ) Mfilec(1) = Filec(1)
      Mt = 0
      CALL mpyad(Z(1),Z(1),Z(1))
      IF ( Code/=2 ) THEN
         CALL wrttrl(Mfilec)
!
         DO i = 1 , 7
            Mfilea(i) = Mfileb(i)
            Mfileb(i) = Mfilec(i)
            Mfilee(i) = Filee(i)
         ENDDO
         CALL makmcb(Mfilec,Filec(1),Ac,Fileb(4),Prec)
         Mt = 1
         CALL mpyad(Z(1),Z(1),Z(1))
      ENDIF
      DO i = 1 , 7
         Filec(i) = Mfilec(i)
      ENDDO
      GOTO 200
   ENDIF
 100  CALL mesage(-8,0,name)
!
!     RETURN
!
 200  Diag = orf(Diag,lshift(l19,18))
   Meth = kmeth
   mpy(3) = jend
   CALL conmsg(mpy,3,0)
END SUBROUTINE mpy3dr
