!*==/home/marcusmae/nasa/nastran/SPAGged/C_BITPOS.f90  created by SPAG 7.61RG at 01:00 on 21 Mar 2022
MODULE C_BITPOS
!
!     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
!     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
!
!     REVISED 7/91 BY G.CHAN/UNISYS
!     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
!     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
!
   INTEGER :: H21, H22, H23, H30, H31, H32, Ha, Hab, Hd, He, Hf, Hfe, Hfr, Hg, Hi, Hk, Hl, Hm, Hn, Hne, Ho, Hp,  &
            & Hpa, Hps, Hr, Hs, Hsa, Hsb, Hsg, Hx, Hy, Hz, U21, U22, U23, U30, U31, U32, Ua, Uab, Ud, Ue, Uf,     &
            & Ufe, Ufr, Ug, Ui, Uk, Ul, Um, Un, Une, Uo, Up, Upa, Ups, Ur, Us, Usa, Usb, Usg, Ux, Uy, Uz
!
!     -------------------     / BITPOS /     ---------------------------
!
!     BITPOS DEFINES THE BIT POSITIONS FOR THE DEGREES-OF-FREEDOM IN
!     USET, AND HOLLERITH CHARACTERS DESCRIBING DEGREES-OF-FREEDOM.
!
   DATA um/32/ , hm/2HM / , ups/16/ , hps/2HPS/ , uo/30/ , ho/2HO / , usa/15/ , hsa/2HSA/ , ur/29/ , hr/2HR / , uk/14/ , hk/2HK / , &
      & usg/23/ , hsg/2HSG/ , upa/13/ , hpa/2HPA/ , usb/22/ , hsb/2HSB/ , u21/10/ , h21/4HXXXX/ , ul/24/ , hl/2HL / , u22/11/ ,     &
       &h22/4HYYYY/ , ua/25/ , ha/2HA / , u23/12/ , h23/4HZZZZ/ , uf/26/ , hf/2HF / , ux/9/ , hx/2HX / , us/31/ , hs/2HS / , uy/8/ ,&
      & hy/2HY / , un/27/ , hn/2HN / , ufr/7/ , hfr/2HFR/ , ug/28/ , hg/2HG / , uz/6/ , hz/2HZ / , ue/21/ , he/2HE / , uab/5/ ,     &
       &hab/2HAB/ , up/20/ , hp/2HP / , ui/4/ , hi/2HI / , une/19/ , hne/2HNE/ , u30/3/ , h30/2HU3/ , ufe/18/ , hfe/2HFE/ , u31/2/ ,&
      & h31/2HU2/ , ud/17/ , hd/2HD / , u32/1/ , h32/2HU1/
!

END MODULE C_BITPOS
