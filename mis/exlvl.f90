
SUBROUTINE exlvl(Nos,Md,Name,Z,Nwds)
   IMPLICIT NONE
   INTEGER Buf(1) , Line , Nlpp , Nout
   LOGICAL Mdiup
   REAL Sysbuf , X1(6) , X2(2) , X3(34)
   COMMON /sof   / X3 , Mdiup
   COMMON /system/ Sysbuf , Nout , X1 , Nlpp , X2 , Line
   COMMON /zzzzzz/ Buf
   INTEGER Nos , Nwds
   INTEGER Md(4,1) , Name(2) , Z(2)
   INTEGER andf , lshift , orf , rshift
   INTEGER cs , hl , i , iold , iss , j , ll , m , nl , nss , ps , subr(2) , tp
   EXTERNAL andf , lshift , orf , rshift
!
!     EXLVL ADDS A SUBSTRUCTURE TO THE RESIDENT SOF FOR THE SOFIN
!     OPERATION.  IT USES THE DIT AND MDI DATA WRITTEN ON THE EXTERNAL
!     FILE BY SOFOUT TO RESTORE THE HL, CS, AND LL POINTERS IN THE MDI.
!
   DATA subr/4HEXLV , 4HL   /
!
!
!     ADD THE NEW SUBSTRUCTURE TO THE RESIDENT DIT.
!
   CALL fdsub(Name,i)
   IF ( i/=-1 ) THEN
!
!     SUBSTRUCTURE NAME WAS DUPLICATED
!
      CALL smsg(4,0,Name)
      RETURN
   ELSE
      CALL crsub(Name,i)
      IF ( Nos<=0 ) THEN
!
!     SUBSTRUCTURE ADDED TO SOF SUCCESSFULLY
!
         WRITE (Nout,99005) Name
         GOTO 200
      ELSE
         Z(1) = Name(1)
         Z(2) = Name(2)
         nss = 1
         iss = 1
      ENDIF
   ENDIF
!
!     DECODE THE OLD MDI ENTRY
!
 100  DO i = 1 , Nos
      IF ( Md(1,i)==Z(2*iss-1) .AND. Md(2,i)==Z(2*iss) ) THEN
         ps = andf(Md(3,i),1023)
         tp = andf(rshift(Md(3,i),20),1023)
         ll = rshift(Md(4,i),20)
         cs = andf(rshift(Md(4,i),10),1023)
         hl = andf(Md(4,i),1023)
         iold = i
         EXIT
      ENDIF
   ENDDO
!
!     SET NEW MDI POINTERS FOR HL, CS, AND LL IF THE SUBSTRUCTURES OF
!     THE ORIGINATING SOF WHICH ARE INDICATED THEREBY EXIST.
!
!
!     HIGHER LEVEL (HL)
!
   m = 0
   IF ( hl/=0 ) THEN
      CALL fdsub(Md(1,hl),i)
      IF ( i<=0 ) THEN
         CALL crsub(Md(1,hl),i)
         nss = nss + 1
         IF ( 2*nss>Nwds ) GOTO 300
         Z(2*nss-1) = Md(1,hl)
         Z(2*nss) = Md(2,hl)
      ENDIF
      m = i
      hl = i
   ENDIF
!
!     COMBINED SUBSTRUCTURE (CS)
!
   IF ( cs/=0 ) THEN
      CALL fdsub(Md(1,cs),j)
      IF ( j<=0 ) THEN
         CALL crsub(Md(1,cs),j)
         nss = nss + 1
         IF ( 2*nss>Nwds ) GOTO 300
         Z(2*nss-1) = Md(1,cs)
         Z(2*nss) = Md(2,cs)
      ENDIF
      m = orf(m,lshift(j,10))
      cs = j
   ENDIF
!
!     LOWER LEVEL (LL)
!
   IF ( ll/=0 ) THEN
      CALL fdsub(Md(1,ll),j)
      IF ( j<=0 ) THEN
         CALL crsub(Md(1,ll),j)
         nss = nss + 1
         IF ( 2*nss>Nwds ) GOTO 300
         Z(2*nss-1) = Md(1,ll)
         Z(2*nss) = Md(2,ll)
      ENDIF
      m = orf(m,lshift(j,20))
      ll = j
   ENDIF
!
!     UPDATE THE MDI
!
   CALL fdsub(Z(2*iss-1),j)
   CALL fmdi(j,i)
   Buf(i+1) = lshift(tp,20)
   Buf(i+2) = m
   Mdiup = .TRUE.
!
!     WRITE USER MESSAGES
!
   nl = 2
   IF ( ll/=0 ) nl = nl + 1
   IF ( cs/=0 ) nl = nl + 1
   IF ( hl/=0 ) nl = nl + 1
   IF ( ps/=0 ) nl = nl + 3
   IF ( Line+nl>Nlpp ) CALL page
   Line = Line + nl
   WRITE (Nout,99005) Z(2*iss-1) , Z(2*iss)
   IF ( hl/=0 ) THEN
      CALL fdit(hl,i)
      WRITE (Nout,99001) Buf(i) , Buf(i+1)
99001 FORMAT (5X,25HHIGHER LEVEL SUBSTRUCTURE,2X,2A4)
   ENDIF
   IF ( cs/=0 ) THEN
      CALL fdit(cs,i)
      WRITE (Nout,99002) Buf(i) , Buf(i+1)
99002 FORMAT (5X,25HCOMBINED SUBSTRUCTURE    ,6(2X,2A4))
   ENDIF
   IF ( ll/=0 ) THEN
      CALL fdit(ll,i)
      WRITE (Nout,99003) Buf(i) , Buf(i+1)
99003 FORMAT (5X,25HLOWER LEVEL SUBSTRUCTURE ,7(2X,2A4))
   ENDIF
   IF ( ps/=0 ) THEN
      WRITE (Nout,99004) Z(2*iss-1) , Z(2*iss)
99004 FORMAT (49H0*** USER INFORMATION MESSAGE 6359, SUBSTRUCTURE ,2A4,41H WAS ORIGINALLY A SECONDARY SUBSTRUCTURE./36X,            &
             &42HON THIS SOF, IT IS A PRIMARY SUBSTRUCTURE.)
   ENDIF
   iss = iss + 1
   IF ( iss<=nss ) GOTO 100
 200  RETURN
!
!     INSUFFICIENT CORE
!
 300  CALL mesage(-8,0,subr)
   RETURN
!
!     MESSAGE TEXT
!
99005 FORMAT (49H0*** USER INFORMATION MESSAGE 6347, SUBSTRUCTURE ,2A4,18H ADDED TO THE SOF.)
END SUBROUTINE exlvl