
SUBROUTINE pla32
   IMPLICIT NONE
   INTEGER Bufsz , Iestbk(100) , Ipass , Iy(30) , Iz(1) , Placnt , Plsetn
   REAL Degra , Estbk(100) , Gamma , Gammas , Pi , Radeg , S4pisq , Twopi , Xxxxxx(325) , Yyyyyy(30) , Z(1)
   COMMON /blank / Placnt , Plsetn
   COMMON /condas/ Pi , Twopi , Radeg , Degra , S4pisq
   COMMON /pla32c/ Gamma , Gammas , Ipass
   COMMON /pla32e/ Estbk
   COMMON /pla32s/ Xxxxxx
   COMMON /sout  / Yyyyyy
   COMMON /system/ Bufsz
   COMMON /zzzzzz/ Z
   REAL a , c , d , dmt , dum2(2) , fj , p(4) , t , tubsav(20) , word
   INTEGER bufr1 , bufr2 , bufr3 , casecc , clsrw , cstm , dit , eltype , eor , estnl1 , estnls , estt(7) , estwds(40) , file , i , &
         & icc , ichar(9) , icstm , idest , iflag , ihigh , iii , ilow , imat , inrw , ionles , ip(4) , ititle(3) , izmax , j ,     &
         & jdest , left , m , mpt , mused , name(2) , ncc , ncstm , neor , nn , nstwds(40) , nwdsp2(40) , nwdsrd , onles , ostrt(7) &
         & , outrw , planos(2) , setno
   INTEGER korsz
!
!     THIS ROUTINE READS THE ESTNLS DATA BLOCK CREATED IN SUBROUTINE
!     PLA31, AND CALLS THE PROPER ELEMENT ROUTINE TO COMPUTE ELEMENT
!     STRESSES.
!     ELEMENT STRESS INFORMATION IS STORED BY THE ELEMENT ROUTINE IN
!     /STROUT/.  THE ELEMENT ROUTINE ALSO UPDATES THE EST ENTRY WHICH
!     HAS BEEN COMMUNICATED TO IT VIA /PLA32E/.  NOTE THAT THIS UPDATED
!     EST ENTRY DOES NOT CONTAIN DISPLACEMENT VECTOR INFORMATION.
!
!
!     SCRATCH BLOCK USED BY ELEMENT ROUTINES (325 SINGLE PRECISION
!     CELLS)  AND OUTPUT BLOCK FOR ELEMENT STRESSES
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (Estbk(1),Iestbk(1)) , (p(1),ip(1)) , (Yyyyyy(1),Iy(1))
   DATA name/4HPLA3 , 4H2   /
   DATA ititle/4HLOAD , 4H FAC , 4HTOR /
   DATA cstm , mpt , dit , estnls , casecc/101 , 102 , 103 , 301 , 106/
   DATA onles , estnl1/201 , 202/
   DATA inrw , outrw , eor , neor , clsrw/0 , 1 , 1 , 0 , 1/
   DATA planos/1103 , 11/
!
!    1        ROD       BEAM      TUBE      SHEAR     TWIST
!    2        TRIA1     TRBSC     TRPLT     TRMEM     CONROD
!    3        ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
!    4        QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
!    5        DAMP2     DAMP3     DAMP4     VISC      MASS1
!    6        MASS2     MASS3     MASS4     CONM1     CONM2
!    7        PLOTEL    REACT     QUAD3     BAR       CONE
!    8        TRIARG    TRAPRG    TORDRG    CORE      CAP
!
   DATA estwds/21 , 0 , 20 , 0 , 0 , 38 , 0 , 0 , 27 , 21 , 0 , 0 , 0 , 0 , 0 , 32 , 32 , 37 , 43 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 0 , 0 , 0 , 50 , 0 , 0 , 0 , 0 , 0 , 0/
   DATA nstwds/5 , 0 , 5 , 0 , 0 , 17 , 0 , 0 , 8 , 5 , 0 , 0 , 0 , 0 , 0 , 8 , 17 , 17 , 17 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  &
      & 0 , 0 , 0 , 0 , 0 , 16 , 0 , 0 , 0 , 0 , 0 , 0/
   DATA nwdsp2/33 , 0 , 32 , 0 , 0 , 56 , 0 , 0 , 36 , 33 , 0 , 0 , 0 , 0 , 0 , 44 , 50 , 61 , 67 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 0 , 0 , 0 , 62 , 0 , 0 , 0 , 0 , 0 , 0/
!
!     DEFINE POSITION IN CASECC RECORD OF DESTINATION (PRINTER, PUNCH,
!     ETC.) OF ELEMENT STRESSES.
!
   DATA idest/24/
!
!
!     DETERMINE SIZE OF CORE, DEFINE BUFFERS AND INITIALIZE CORE POINTER
!     AND COUNTERS
!
   izmax = korsz(Z)
   bufr1 = izmax - Bufsz
   bufr2 = bufr1 - Bufsz
   bufr3 = bufr2 - Bufsz
   left = bufr3 - 1
   Ipass = Placnt - 1
   icstm = 0
   ncstm = 0
   DO i = 1 , 7
      ostrt(i) = 0
      estt(i) = 0
   ENDDO
!
!     ATTEMPT TO READ CSTM INTO CORE
!
   file = cstm
   CALL open(*200,cstm,Z(bufr1),inrw)
   CALL fwdrec(*1100,cstm)
   CALL read(*1100,*100,cstm,Z(icstm+1),left,eor,ncstm)
   CALL mesage(-8,0,name)
 100  left = left - ncstm
   CALL close(cstm,clsrw)
   CALL pretrs(Z(icstm+1),ncstm)
 200  imat = ncstm
!
!     COMPUTE GAMMA AND GAMMAS FROM THE PROPER PLFACT CARD
!
   file = mpt
   CALL preloc(*1000,Z(bufr1-3),mpt)
   CALL locate(*1300,Z(bufr1-3),planos,iflag)
   DO
      CALL read(*1100,*1200,mpt,setno,1,neor,iflag)
      IF ( setno==Plsetn ) THEN
         IF ( Placnt>4 ) CALL read(*1100,*1200,mpt,0,-(Placnt-4),neor,iflag)
         nwdsrd = 4
         IF ( Placnt<4 ) nwdsrd = Placnt
         CALL read(*1100,*1200,mpt,p,nwdsrd,neor,iflag)
         IF ( ip(nwdsrd)/=-1 ) THEN
            a = p(2) - p(1)
            IF ( Placnt<3 ) THEN
               Gammas = 0.0
               Gamma = a/p(1)
            ELSEIF ( Placnt==3 ) THEN
               Gammas = a/p(1)
               Gamma = (p(3)-p(2))/a
            ELSE
               word = p(3) - p(2)
               Gammas = word/a
               Gamma = (p(4)-p(3))/word
            ENDIF
         ELSE
            IF ( Placnt<3 ) THEN
               Gammas = 1.0
            ELSEIF ( Placnt==3 ) THEN
               Gammas = (p(2)-p(1))/p(1)
            ELSE
               Gammas = (p(3)-p(2))/(p(2)-p(1))
            ENDIF
            Gamma = 1.0
         ENDIF
         CALL close(mpt,clsrw)
!
!     READ MPT AND DIT FILES.  NOTE MINUS SIGN ON DIT TO TRIGGER PLA
!     FLAG.
!
         CALL premat(Iz(imat+1),Z(imat+1),Z(bufr1-3),left,mused,mpt,-dit)
         left = left - mused
         icc = ncstm + mused
!
!     READ CASECC INTO OPEN CORE
!
         file = casecc
         CALL open(*1000,casecc,Z(bufr1),inrw)
         CALL fwdrec(*1100,casecc)
         CALL read(*1100,*300,casecc,Z(icc+1),left,eor,ncc)
         CALL mesage(-8,0,name)
         EXIT
      ELSE
         DO
            CALL read(*1100,*1200,mpt,nn,1,neor,iflag)
            IF ( nn==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
 300  left = left - ncc
   CALL close(casecc,clsrw)
!
! OPEN INPUT FILE
!
   file = estnls
   CALL open(*1000,estnls,Z(bufr1),inrw)
   CALL fwdrec(*1100,estnls)
!
!     OPEN THE ELEMENT STRESS FILE FOR OUTPUT AND BUILD HEADER WHICH IS
!     NON-CHANGING.
!
   file = onles
   CALL open(*1000,onles,Z(bufr2),outrw)
   CALL fname(onles,dum2)
   CALL write(onles,dum2,2,eor)
!
!     THE FOLLOWING INDICES HAVE TO CHANGE  WHEN THERE ARE CHANGES IN
!     THE FORMAT OF THE CASECC DATA BLOCK
!
   ionles = icc + ncc
   Iz(ionles+1) = Iz(icc+18) + 100
   Iz(ionles+2) = 5
   Iz(ionles+4) = Iz(icc+1)
   Iz(ionles+5) = Iz(icc+4)
   Iz(ionles+6) = 0
   Iz(ionles+7) = 0
   Iz(ionles+8) = 0
   Iz(ionles+9) = 0
   ilow = ionles + 51
   ihigh = ionles + 146
   left = left - 146
   IF ( left<0 ) CALL mesage(-8,0,name)
   j = icc + 38
   DO i = ilow , ihigh
      j = j + 1
      Iz(i) = Iz(j)
   ENDDO
!
!     STORE LOAD FACTOR AND INTEGER IN LABEL PORTION OF OUTPUT
!
   Iz(ionles+135) = ititle(1)
   Iz(ionles+136) = ititle(2)
   Iz(ionles+137) = ititle(3)
   iii = Placnt - 1
   CALL int2al(iii,Iz(ionles+138),ichar)
!
!     DEFINE DESTINATION OF OUTPUT
!
   i = icc + idest
   jdest = Iz(i)
!
!     OPEN THE ESTNL1 FILE FOR OUTPUT.
!
   file = estnl1
   CALL open(*1000,estnl1,Z(bufr3),outrw)
   CALL fname(estnl1,dum2)
   CALL write(estnl1,dum2,2,eor)
   file = estnls
!
!     READ ELEMENT TYPE
!
 400  CALL read(*900,*1200,estnls,eltype,1,neor,iflag)
!
!     FILL IN REMAINDER OF ID RECORD FOR THE ONLES FILE
!
   Iz(ionles+3) = eltype
   Iz(ionles+10) = nstwds(eltype)
   IF ( nstwds(eltype)<=0 ) CALL mesage(-30,91,eltype)
!
!     WRITE ID RECORD FOR ONLES FILE
!
   CALL write(onles,Iz(ionles+1),146,eor)
   CALL write(estnl1,eltype,1,neor)
!
!     READ AN ENTRY FROM THE APPENDED ESTNL FILE AND CALL THE PROPER
!     ROUTINE
!
 500  CALL read(*1100,*800,estnls,Estbk,nwdsp2(eltype),neor,iflag)
!
!               1,ROD    2,BEAM    3,TUBE   4,SHEAR   5,TWIST
!             6,TRIA1   7,TRBSC   8,TRPLT   9,TRMEM 10,CONROD
!            11,ELAS1  12,ELAS2  13,ELAS3  14,ELAS4  15,QDPLT
!            16,QDMEM  17,TRIA2  18,QUAD2  19,QUAD1  20,DAMP1
!            21,DAMP2  22,DAMP3  23,DAMP4   24,VISC  25,MASS1
!            26,MASS2  27,MASS3  28,MASS4  29,CONM1  30,CONM2
!           31,PLOTEL  32,REACT  33,QUAD3    34,BAR   35,CONE
!           36,TRIARG 37,TRAPRG 38,TORDRG   39,CORE?   40,CAP?
   IF ( eltype==2 .OR. eltype==4 .OR. eltype==5 .OR. eltype==7 .OR. eltype==8 .OR. eltype==11 .OR. eltype==12 .OR. eltype==13 .OR.  &
      & eltype==14 .OR. eltype==15 .OR. eltype==20 .OR. eltype==21 .OR. eltype==22 .OR. eltype==23 .OR. eltype==24 .OR.             &
      & eltype==25 .OR. eltype==26 .OR. eltype==27 .OR. eltype==28 .OR. eltype==29 .OR. eltype==30 .OR. eltype==31 .OR.             &
      & eltype==32 .OR. eltype==33 .OR. eltype==35 .OR. eltype==36 .OR. eltype==37 .OR. eltype==38 .OR. eltype==39 .OR. eltype==40 )&
      & THEN
!
!     FATAL ERRORS
!
      CALL mesage(-30,92,eltype)
      GOTO 1000
   ELSEIF ( eltype==3 ) THEN
!
!
!     TUBE - REARRANGE ESTBK FOR THE TUBE SO THAT IT IS IDENTICAL TO THE
!            ONE FOR THE ROD
!
!     SAVE THE EST ENTRY FOR THE TUBE EXCEPT THE 4 WORDS WHICH WILL BE
!     UPDATED BY THE THE ROD ROUTINE AND THE DISPLACEMENT VECTORS
!
      DO i = 1 , 16
         tubsav(i) = Estbk(i)
      ENDDO
!
!     COMPUTE AREA, TORSIONAL INERTIA TERM AND STRESS COEFFICIENT
!
      d = Estbk(5)
      t = Estbk(6)
      dmt = d - t
      a = dmt*t*Pi
      fj = .25*a*(dmt**2+t**2)
      c = d/2.0
!
!     MOVE THE END OF THE ESTBK ARRAY DOWN ONE SLOT SO THAT ENTRIES 7
!     THRU 32 WILL BE MOVED TO POSITIONS 8 THRU 33.
!
      m = 33
      DO i = 1 , 26
         Estbk(m) = Estbk(m-1)
         m = m - 1
      ENDDO
      Estbk(5) = a
      Estbk(6) = fj
      Estbk(7) = c
      GOTO 600
   ELSEIF ( eltype==6 ) THEN
!
!     TRIA1
!
      CALL pstri1
   ELSEIF ( eltype==9 ) THEN
!
!     TRMEM
!
      CALL pstrm
   ELSEIF ( eltype==16 ) THEN
!
!     QDMEM
!
      CALL psqdm
   ELSEIF ( eltype==17 ) THEN
!
!     TRIA2
!
      CALL pstri2
   ELSEIF ( eltype==18 ) THEN
!
!     QUAD2
!
      CALL psqad2
   ELSEIF ( eltype==19 ) THEN
!
!     QUAD1
!
      CALL psqad1
   ELSEIF ( eltype==34 ) THEN
!
!     BAR
!
      CALL psbar
   ELSE
      GOTO 600
   ENDIF
   GOTO 700
!
!     ROD, CONROD
!
 600  CALL psrod
!
!     IF ELEMENT IS A TUBE, RESTORE SAVED EST ENTRY AND STORE UPDATED
!     STRESS VARIABLES IN PROPER SLOTS.
!
   IF ( eltype==3 ) THEN
      DO i = 1 , 16
         Estbk(i) = tubsav(i)
      ENDDO
      Estbk(17) = Estbk(18)
      Estbk(18) = Estbk(19)
      Estbk(19) = Estbk(20)
      Estbk(20) = Estbk(21)
   ENDIF
!
!     ALTER ELEMENT IDENTIFICATION FROM EXTERNAL (USER) IDENTIFICATION
!     TO INTERNAL ID., AND WRITE OUTPUT FILES.
!
 700  Iy(1) = 10*Iy(1) + jdest
   CALL write(onles,Iy,nstwds(eltype),neor)
   CALL write(estnl1,Estbk,estwds(eltype),neor)
   ostrt(2) = ostrt(2) + 1
   estt(2) = estt(2) + 1
   GOTO 500
!
!     WRITE EORS
!
 800  CALL write(onles,0,0,eor)
   CALL write(estnl1,0,0,eor)
   GOTO 400
!
!     CLOSE FILES AND WRITE TRAILERS
!
 900  CALL close(onles,clsrw)
   CALL close(estnl1,clsrw)
   CALL close(estnls,clsrw)
   ostrt(1) = onles
   estt(1) = estnl1
   CALL wrttrl(ostrt)
   CALL wrttrl(estt)
   RETURN
 1000 j = -1
   GOTO 1400
 1100 j = -2
   GOTO 1400
 1200 j = -3
   GOTO 1400
 1300 j = -5
 1400 CALL mesage(j,file,name)
END SUBROUTINE pla32