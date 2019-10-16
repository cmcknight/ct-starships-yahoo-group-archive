 REM "Sonthert      1918 X6266AB-3    Ni              R4 314 Im K6 V M0 D
REM "         1         2         3
RANDOMIZE TIMER
musik$ = "MBT180o2P2P8L8GGGL2E-P24P8L8FFFL2D"

rnx = RND
'wq = .25
'DEF fnx (x) = INT(1 / ((siz + atm + hyd + pop + gov + law + tec) / (tec + siz + pop + hyd)) * x) + 1
DEF fnx (x) = INT(RND * x) + 1
'DEF fnz (x) = INT(RND * x + RND * x) + 2
'DEF fnd (x) = INT(RND * x + RND * x + RND * x) + 3
'DEF fnw (x) = INT(fnx(1) * x) + 1
'DEF fnz (x) = INT(fnx(1) * x + fnx(1) * x) + 2
'DEF fnd (x) = INT(fnx(1) * x + fnx(1) * x + fnx(x) * x) + 3
DEF fnw (x) = fnx(x)
DEF fnz (x) = fnw(x) + fnw(x)
DEF fnd (x) = fnw(x) + fnw(x) + fnw(x)
DIM val$(12)
DIM text$(10)
DIM lox(6)
val$(1) = "NO"
val$(2) = "Negligible amounts"
val$(3) = "Tiny amounts"
val$(4) = "Very small amounts"
val$(5) = "Small amounts"
val$(6) = "Mediocre amounts"
val$(7) = "Average amounts"
val$(8) = "Above average amounts"
val$(9) = "Supple amounts"
val$(10) = "Very big amounts"
val$(11) = "Huge amounts"
val$(12) = "Enormous amounts"
DIM lat(20, 20)
DIM loca(100, 100)
DIM au(19), orf(19)
DIM mass(45, 6), lumi(45, 6)
DIM nhz(10, 4), hz(10, 4)
sec$ = "Deneb.DAT": pas$ = "Vincennes"
sec$ = "Spinward.dat": pas$ = "Regina"
'sec$ = "Gateway.dat": pas$ = "Gateway"
'sec$ = "Oldexpan.dat": pas$ = "Fisher"
'sec$ = "Solomani.dat": pas$ = "Terra"
'sec$ = "Reaversd.dat": pas$ = "Caledon"
'sec$ = "Reft.dat": pas$ = "Neubayern"
'sec$ = "Corridor.dat": pas$ = "Kaasu"
sector$ = sec$
LOCATE 24, 1
INPUT name$: LOCATE 1, 1: CLS
IF name$ = "0" THEN END
IF name$ = "" THEN name$ = pas$
pas$ = name$


funf = 6
ffx = (funf * 2 + 1) ^ 2
DIM upp$(ffx)
33 OPEN sector$ FOR INPUT AS #1
INPUT #1, planets
FOR I = 1 TO planets
INPUT #1, upp$
lo1 = VAL(MID$(upp$, 15, 2))
lo2 = VAL(MID$(upp$, 17, 2))
IF LCASE$(name$) = LCASE$(LEFT$(upp$, LEN(name$))) THEN lox = VAL(MID$(upp$, 15, 4)): horiz = lo1: verti = lo2: I = planets: GOSUB 17000
NEXT: CLOSE

IF horiz = 0 AND LEN(name$) > 0 THEN name$ = LEFT$(name$, LEN(name$) - 1): GOTO 33
IF horiz = 0 THEN RUN
hmin = horiz - funf
hplu = horiz + funf
vmin = verti - funf
vplu = verti + funf

OPEN sector$ FOR INPUT AS #1
INPUT #1, planets: sx = 0
FOR I = 1 TO planets
INPUT #1, upp$
lo1 = VAL(MID$(upp$, 15, 2))
lo2 = VAL(MID$(upp$, 17, 2))
dx = 0
IF lo1 >= hmin AND lo1 < hplu THEN dx = 1
IF lo2 >= vmin AND lo2 < vplu THEN dx = dx + 1
IF dx = 2 THEN sx = sx + 1: upp$(sx) = upp$
NEXT: planets = sx
CLOSE

888 RESTORE 888
DATA  24, 21, 21, 18, 18, 15, 15, 12, 12,  9
DATA  16, 14, 14, 12, 12, 10, 10,  8,  8,  6
DATA   8,  7,  7,  6,  6,  5,  5,  4,  4,  3
DATA   0,  0,  0,  0,  0,  0,  0,  0,  0,  0
DATA  -8, -7, -7, -6, -6, -5, -5, -4, -4, -3
DATA -16,-14,-14,-12,-12,-10,-10, -8, -8, -6
DATA -24,-21,-21,-18,-18,-15,-15,-12,-12, -9
DATA -32,-28,-28,-24,-24,-20,-20,-16,-16,-12
DATA -40,-35,-35,-30,-30,-25,-25,-20,-20,-15
DATA -48,-42,-42,-36,-36,-30,-30,-24,-24,-18
DATA -56,-49,-49,-42,-42,-35,-35,-28,-28,-21
FOR I = 1 TO 11: FOR j = 1 TO 10: READ lat(I, j): NEXT: NEXT
GOSUB 4000
'print UCASE$(LEFT$(upp$, 4)); " ";

FOR I = 1 TO 6
FOR j = 0 TO 45
IF j < 5 THEN j0 = 0: j1 = 5: GOTO 9
IF j < 10 THEN j0 = 5: j1 = 10: GOTO 9
IF j < 15 THEN j0 = 10: j1 = 15: GOTO 9
IF j < 20 THEN j0 = 15: j1 = 20: GOTO 9
IF j < 25 THEN j0 = 20: j1 = 25: GOTO 9
IF j < 30 THEN j0 = 25: j1 = 30: GOTO 9
IF j < 35 THEN j0 = 30: j1 = 35: GOTO 9
IF j < 40 THEN j0 = 35: j1 = 40: GOTO 9
IF j < 45 THEN j0 = 40: j1 = 45
9 '
'PRINT USING "##"; i; : PRINT USING "## "; j;
m1 = mass(j1, I): m0 = mass(j0, I)
l1 = lumi(j1, I): l0 = lumi(j0, I)
mx = (m1 - m0) / 5 * (j - j0): lx = (l1 - l0) / 5 * (j - j0)
IF mass(j, I) = 0 THEN mass(j, I) = mass(j0, I) + mx
IF lumi(j, I) = 0 THEN lumi(j, I) = lumi(j0, I) + lx
NEXT: NEXT
GOSUB 5000

FOR I = 1 TO planets
'PRINT UCASE$(LEFT$(upp$(i), 4)); " ";
lo1 = VAL(MID$(upp$(I), 15, 2))
lo2 = VAL(MID$(upp$(I), 17, 2))
10 loca(lo1, lo2) = I
NEXT



FOR I = 1 TO ffx
IF LCASE$(LEFT$(upp$(I), LEN(name$))) = LCASE$(name$) THEN qq = I: I = ffx
NEXT
FOR j = 1 TO 32
FOR z = 1 TO 40
IF loca(j, z) = qq THEN horiz = j: vertik = z
NEXT
NEXT

mod1 = horiz - 5: mod2 = vertik - 5
IF INT(horiz / 2) <> horiz / 2 THEN mod1 = horiz - 6

IF mod1 < 0 THEN mod1 = 0
IF mod2 < 0 THEN mod2 = 0



REM****************************
REM                           *
GOSUB 3100: REM AUSARBEITUNG  *
REM                           *
REM****************************
REM GOTO 3360
100 REM Subsector Darstellung
tvz = 0: tpz = 0: CLS
LOCATE 1, 1: PRINT CHR$(201); : FOR w = 1 TO 34: PRINT CHR$(205); : NEXT
PRINT CHR$(187): FOR u = 2 TO 22: LOCATE u, 1: PRINT CHR$(186); TAB(36); CHR$(186); : NEXT
LOCATE 23, 1: PRINT CHR$(200); : FOR w = 1 TO 34: PRINT CHR$(205); : NEXT: PRINT CHR$(188);
tfm = 0
FOR I = 1 TO 8: FOR j = 1 TO 10
bb = 1: IF INT(I / 2) = I / 2 THEN bb = 0
sa = loca(I + mod1, j + mod2)
hz = j * 2 + bb + 1: ve = I * 4
IF sa <= 0 THEN LOCATE hz, ve: PRINT " . ": GOTO 77
upp$ = upp$(sa): rtx$ = " ": loy = VAL(MID$(upp$, 15, 4)): GOSUB 15000: namx$ = LEFT$(upp$(sa), 14)
l1 = VAL(MID$(upp$, 15, 2)): l2 = VAL(MID$(upp$, 17, 2))
IF a$ = "t" THEN othw = 1: GOSUB 2001: GOSUB 9000: tbase = bsp ELSE GOSUB 3900
IF a$ = "b" OR a$ = "r" THEN othw = 1: GOSUB 2001: GOSUB 9100 ELSE GOSUB 3900


LOCATE hz - 1, ve: PRINT LEFT$(namx$, 3)
LOCATE hz, ve
IF a$ = "P" OR a$ = "p" THEN PRINT pol$; bas$: GOTO 77
IF a$ = "s" THEN PRINT sox$: GOTO 77
IF a$ = "n" THEN PRINT MID$(namx$, 4, 3): GOTO 77
IF a$ = "t" THEN PRINT USING "###"; tbase / 100: GOTO 77
IF bas$ = " " THEN bas$ = "-"
IF a$ = "r" THEN GOSUB 20000: vl = btn * 2: GOSUB 16000: PRINT stp$; wwx$; vl$: GOTO 77
IF a$ = "b" THEN PRINT USING "#.#"; btn: GOTO 77
PRINT stp$; pop$; tec$

77
IF a$ = "r" THEN tvz = tvz + tvx: tpz = tpz + tvp: tvx = 0: tvp = 0
NEXT
NEXT

REM Planeten darstellung
upp$ = LEFT$(upp$(qq), 76): othw = 1: GOSUB 3100
traffic = traffic + tfm: facility = facility + tfm: tfm = 0

sg = 1: GOSUB 14000

LOCATE 1 + sg, 39: PRINT LEFT$(upp$, 32)
wt0 = btn
LOCATE 3 + sg, 39:      PRINT "Port : "; stp$;
IF traffic >= 0 THEN PRINT "-"; : vl = traffic: GOSUB 16000: PRINT vl$; "-"; : vl = facility: GOSUB 16000: PRINT vl$;
PRINT " ";
IF tv0 > 0 THEN tvz = tv0
IF tp0 > 0 THEN tpz = tp0
IF tvz > 0 THEN value = tvz: GOSUB 13000
IF tpz > 0 THEN value = tpz / 365: PRINT " t/y "; : GOSUB 13000: PRINT " P/d"
PRINT
LOCATE 5 + sg, 39:      PRINT "Route:";


IF minor > 0 THEN PRINT USING "### Minor,"; minor;
IF feeder > 0 THEN PRINT USING "### Feeder,"; feeder;
IF major > 0 THEN PRINT USING "### Major,"; major;
IF minor > 0 THEN PRINT USING "##%"; (minor + feeder + major) / planets * 100;
PRINT
wt0 = btn

LOCATE 7 + sg, 39:
IF siz = 0 THEN PRINT "Size : Asteroid Belt"
IF siz > 0 THEN PRINT USING "Size :######, km, #.## g"; siz * 1609; grav

LOCATE 9 + sg, 39:      PRINT "Atm. : "; atmx$; : IF press > 0 THEN PRINT USING "###.## bar####.#ø"; press; bse ELSE PRINT
LOCATE 11 + sg, 39:
IF hyd > 0 THEN PRINT USING "Hyd. :###, %"; hyz * 10
IF hyd = 0 THEN PRINT "Hyd. : No free water"

LOCATE 13 + sg, 39: PRINT "Pop. : "; : value = 10 ^ pop * mul: GOSUB 13000
LOCATE 15 + sg, 39:     PRINT "Gov. : "; LEFT$(gov$, 32)
LOCATE 17 + sg, 39:     PRINT USING "Law #: "; cr; : PRINT LEFT$(law$, 32)
LOCATE 19 + sg, 39:     PRINT USING "Tech : ##/## "; tec; gtec; : PRINT LEFT$(tecx$, 32)
LOCATE 21 + sg, 39:     PRINT "Bases: "; LEFT$(bax$, 32)
REM LOCATE 23 + sg, 39:     PRINT "Trade: "; stp$; "-"; tec$; " "; trade$; " Cr"; tbase; pol$
PRINT
101
GOSUB 1000
a$ = LCASE$(a$)
IF a$ = "r" OR a$ = "b" OR a$ = "p" OR a$ = "1" OR a$ = "n" OR a$ = "s" OR a$ = "t" THEN GOTO 100
IF a$ = "x" AND tv0 > 0 THEN a$ = "b": GOTO 100
IF a$ = "x" AND tv0 = 0 THEN GOSUB 30000: a$ = "1": GOTO 100
IF a$ = "2" THEN GOTO 3300: REM Temperatur
IF a$ = "3" THEN GOTO 3310: REM Worlddata
IF a$ = "4" THEN GOTO 3330: REM Government
IF a$ = "5" THEN GOTO 3360: REM Resources
IF a$ = "h" OR a$ = "H" THEN GOSUB 120: GOTO 101
IF a$ = " " THEN RUN
GOTO 101

1000 a$ = INKEY$: IF a$ = "" GOTO 1000
IF a$ = "q" THEN END
RETURN

120 REM HELP
CLS : PRINT " "; upp$: PRINT
PRINT
PRINT " 1 = Starmap, Basic data"
PRINT " 2 = Temperature"
PRINT " 3 = System + Planetary data"
PRINT " 4 = Starport, Government, Law Level, Tech Level"
PRINT " 5 = Resources": PRINT
PRINT " p = Politics"
PRINT " n = Names"
PRINT " s = Suns"
PRINT " t = Trade Values"
PRINT " r = Port, Route, Facilities"
PRINT " b = BTN"
PRINT " x = Total sector trade volume (this can last a bit)"
RETURN


3000
code = 0
IF VAL(code$) > 0 THEN code = VAL(code$): RETURN
IF code$ = "A" THEN code = 10
IF code$ = "B" THEN code = 11
IF code$ = "C" THEN code = 12
IF code$ = "D" THEN code = 13
IF code$ = "E" THEN code = 14
IF code$ = "F" THEN code = 15
IF code$ = "G" THEN code = 16
IF code$ = "H" THEN code = 17
IF code$ = "J" THEN code = 18
IF code$ = "K" THEN code = 19
IF code$ = "L" THEN code = 20
IF code$ = "M" THEN code = 21
IF code$ = "N" THEN code = 22
RETURN


















3100 'Ausarbeitung


REM 'Planetary Data
namx$ = LEFT$(upp$(qq), 14)
upp$ = MID$(upp$(qq), 20, 40)
upp$ = upp$(qq)
IF upp$ = "" THEN RUN
2001
GOSUB 3900
code$ = siz$: GOSUB 3000: siz = code
code$ = atm$: GOSUB 3000: atm = code
code$ = hyd$: GOSUB 3000: hyd = code
code$ = pop$: GOSUB 3000: pop = code
code$ = gov$: GOSUB 3000: gov = code
code$ = law$: GOSUB 3000: law = code
code$ = tec$: GOSUB 3000: tec = code


code$ = mul$: GOSUB 3000: mul = code: mul = mul + rnx - .5
code$ = asg$: GOSUB 3000: asg = code
code$ = gas$: GOSUB 3000: gas = code

capital = 0: FOR u = 1 TO LEN(tra$)
IF UCASE$(MID$(tra$, u, 2)) = "CP" THEN capital = .5
IF UCASE$(MID$(tra$, u, 2)) = "CX" THEN capital = 1
NEXT
traffic = fnz(6) - 2: traffic = 5
facility = fnz(6) - 6: facility = 1
tbase = 4000
IF stp$ = "A" THEN stpx$ = "Refined Fuel. Starship yard": tbase = tbase - 1000: valmo = 1: traffic = traffic + 2
IF stp$ = "B" THEN stpx$ = "Refined Fuel. Spaceship yard": valmo = .95
IF stp$ = "C" THEN stpx$ = "Unrefined Fuel. Repair yard": tbase = tbase + 1000: valmo = .9: traffic = traffic - 1
IF stp$ = "D" THEN stpx$ = "Unrefined fuel. No Repair facilities.": tbase = tbase + 2000: valmo = .85: traffic = traffic - 2
IF stp$ = "E" THEN stpx$ = "Frontier installation. Beacon and pad.": tbase = tbase + 3000: valmo = .8: traffic = traffic - 3
IF stp$ = "X" THEN stpx$ = "No starport.": tbase = tbase + 5000: valmo = .75: traffic = traffic - 4
valmo = valmo - (15 - tec) * .05



tt = 0
IF atm = 0 THEN atmx$ = "No atmosphere"
IF atm = 1 THEN atmx$ = "Trace"
IF atm = 2 THEN atmx$ = "Very thin, tainted": tt = 1
IF atm = 3 THEN atmx$ = "Very thin"
IF atm = 4 THEN atmx$ = "Thin, tainted": tt = 1
IF atm = 5 THEN atmx$ = "Thin"
IF atm = 6 THEN atmx$ = "Standard"
IF atm = 7 THEN atmx$ = "Standard, tainted": tt = 1
IF atm = 8 THEN atmx$ = "Dense"
IF atm = 9 THEN atmx$ = "Dense, tainted": tt = 1
IF atm = 10 THEN atmx$ = "Exotic"
IF atm = 11 THEN atmx$ = "Corrosive"
IF atm = 12 THEN atmx$ = "Insidious"
IF atm = 13 THEN atmx$ = "Dense, high"
IF atm = 14 THEN atmx$ = "Elipsoid"
IF atm = 15 THEN atmx$ = "Thin, low"
tt$ = ""
IF tt = 0 GOTO 3200
ww = fnw(6)
IF pox$ <> " " THEN code$ = pox$: GOSUB 3000: ww = code
IF ww < 3 OR ww = 12 THEN atmx$ = "Disease": tt$ = "The atmossphere contains highly contagious components. "
IF ww = 3 OR ww = 10 THEN atmx$ = "Gas Mix": tt$ = "The gas mixture is unhealthy. "
IF ww = 4 THEN atmx$ = "High Oxygen": tt$ = "The oxygen percentage is too high. "
IF ww = 5 OR ww = 9 THEN atmx$ = "Pollutants": tt$ = "The atmossphere is highly polluted. "
IF ww = 7 THEN atmx$ = "Pollutants": tt$ = "The atmossphere is polluted by industrial waste. "
IF ww = 6 OR ww = 8 THEN atmx$ = "Sulfur Compounds": tt$ = "The atmossphere is polluted by sulfur compounds. "
IF ww = 11 THEN atmx$ = "Low Oxygen": tt$ = "The oxygen percentage is too low. "
IF ww = 13 THEN atmx$ = "Radioactive": tt$ = "The atmosphere is irradiated. "
IF ww = 14 THEN atmx$ = "Dust": tt$ = "The atmosphere is polluted by dust particles. "
IF ww = 15 THEN atmx$ = "Toxic": tt$ = "The atmossphere contains higly poisonous compounds. "
IF ww = 16 THEN atmx$ = "Allergens": tt$ = "The atmossphere contains hihgly allergic compounds. "

3200 '

                         IF atm < 4 THEN cloud$ = " 00 00 10 10 20 20 20 20 20 20 20": ovc$ = "0001020304040404040404": prec$ = "0001020405050505050505"
            IF atm > 3 AND atm < 10 THEN cloud$ = " 00 00 10 10 20 30 40 50 60 70 70": ovc$ = "0001020304050607080910": prec$ = "0001020405060708091011"
IF atm > 9 AND atm < 14 OR atm = 15 THEN cloud$ = " 40 40 50 50 60 70 80 90100110120": ovc$ = "0405060708091011121314": prec$ = "0405060809101112131415"
                        IF atm = 14 THEN cloud$ = " 00 00 05 05 10 15 20 25 30 35 35": ovc$ = "0000010102020303040405": prec$ = "0000010202030304040505"
cloud = VAL(MID$(cloud$, hyd * 3 + 1, 3))
ovc = VAL(MID$(ovc$, hyd * 2 + 1, 2))
prec = VAL(MID$(prec$, hyd * 2 + 1, 2))
IF bse < -50 THEN cloud = 0: ovc = 0: prec = 0
pxl$ = ""
IF pol$ = "Wi" OR pol$ = "WI" THEN pxl$ = "Wilds"
IF pol$ = "Re" OR pol$ = "RE" THEN pxl$ = "Regency"
IF pol$ = "As" OR pol$ = "AS" THEN pxl$ = "Aslan Hierate"
IF pol$ = "Af" OR pol$ = "AF" THEN pxl$ = "Aslan Frontier World"
IF pol$ = "Cs" OR pol$ = "CS" THEN pxl$ = "Client State": traffic = traffic - 1
IF pol$ = "Da" OR pol$ = "DA" THEN pxl$ = "Darrian"
IF pol$ = "Dd" OR pol$ = "DD" THEN pxl$ = "Domain of Deneb"
IF pol$ = "Dr" OR pol$ = "DR" THEN pxl$ = "Droyne"
IF pol$ = "Fa" OR pol$ = "FA" THEN pxl$ = "Federation of Arden"
IF pol$ = "Na" OR pol$ = "NA" THEN pxl$ = "Non-Alligned": traffic = traffic - 2
IF pol$ = "Rf" OR pol$ = "RF" THEN pxl$ = "Regency Frontier"
IF pol$ = "Sf" OR pol$ = "SF" THEN pxl$ = "Senlis Federation"
IF pol$ = "Sw" OR pol$ = "SW" THEN pxl$ = "Sword Worlds"
IF sector$ = "Gateway" AND pol$ = "Sw" THEN pxl$ = "Swanfei Free Worlds"
IF pol$ = "Zh" OR pol$ = "ZH" THEN pxl$ = "Zhodani"
IF pol$ = "Vg" OR pol$ = "VG" THEN pxl$ = "Vargr"
IF pol$ = "Im" OR pol$ = "IM" THEN pxl$ = "Imperial"
IF pol$ = "Ho" OR pol$ = "HO" THEN pxl$ = "Hochiken People's Assembly"
IF pol$ = "Ga" OR pol$ = "GA" THEN pxl$ = "Galian Federation"
IF pol$ = "Pl" OR pol$ = "PL" THEN pxl$ = "Plaviam League"
IF pol$ = "Tr" OR pol$ = "TR" THEN pxl$ = "Trindel Confederacy"
IF pol$ = "Vi" OR pol$ = "VI" THEN pxl$ = "Viyard Concourse"
IF pol$ = "Ak" OR pol$ = "AK" THEN pxl$ = "Akeena Union"
IF pol$ = "Me" OR pol$ = "ME" THEN pxl$ = "Megusard Corporate"
IF pol$ = "Re" OR pol$ = "RE" THEN pxl$ = "Renkard Union"




IF pol$ = "Wi" OR pol$ = "WI" GOTO 188
IF gov = 0 THEN gov$ = "No government": peas = .5: wars = 1.5
IF gov = 1 THEN gov$ = "Company, corporation": peas = .8: wars = 1.4
IF gov = 2 THEN gov$ = "Participating democracy": peas = 1: wars = 1.5
IF gov = 3 THEN gov$ = "Self-perpetuating oligarchy": peas = .9: wars = 1.2
IF gov = 4 THEN gov$ = "Representative democracy": peas = .85: wars = 1.45
IF gov = 5 THEN gov$ = "Feudal technocracy": peas = .95: wars = 1.4
IF gov = 6 THEN gov$ = "Captive government, colony": peas = 1.5: wars = 1.5
IF gov = 7 THEN gov$ = "Balkanization": peas = 1: wars = 1.5
IF gov = 8 THEN gov$ = "Civil service beaureaucracy": peas = 1.1: wars = 1.2
IF gov = 9 THEN gov$ = "Impersonal beaureaucracy": peas = 1.15: wars = 1.2
IF gov = 10 THEN gov$ = "Charismatic dictatorship": peas = 1.2: wars = 1.5
IF gov = 11 THEN gov$ = "Non-charismatic dictator": peas = 1.1: wars = 1.2
IF gov = 12 THEN gov$ = "Charismatic oligarchy": peas = 1.2: wars = 1.5
IF gov = 13 THEN gov$ = "Religious dictatorship": peas = .75: wars = 1.5
IF gov = 14 THEN gov$ = "Religious oligarchy": peas = .75: wars = 1.5
IF gov = 15 THEN gov$ = "Totalitarian oligarchy": peas = 1.1: wars = 1.2
IF gov = 16 THEN gov$ = "Small Aslan facility": peas = .5: wars = 1.5
IF gov = 17 THEN gov$ = "Split Clan Control": peas = .5: wars = 1.5
IF gov = 18 THEN gov$ = "On world Clan Control": peas = .5: wars = 1.5
IF gov = 19 THEN gov$ = "Multi world Clan Control": peas = .5: wars = 1.5
IF gov = 20 THEN gov$ = "Major Clan Control": peas = .5: wars = 1.5
IF gov = 21 THEN gov$ = "Vassal Clan Control": peas = .5: wars = 1.5
IF gov = 22 THEN gov$ = "Major vassal Clan Control": peas = .5: wars = 1.5
pbudget = 500 * peas * (10 ^ pop * mul)
wbudget = 500 * wars * (10 ^ pop * mul)
GOTO 189

188 'wilds
IF gov = 0 THEN gov$ = "No government": peas = .5: wars = 1.5
IF gov = 1 THEN gov$ = "Tribal Government"
IF gov = 2 THEN gov$ = "Participating democracy"
IF gov = 3 THEN gov$ = "Representative democracy"
IF gov = 4 THEN gov$ = "Charismatic dictator"
IF gov = 5 THEN gov$ = "Charismatic oligarchy"
IF gov = 6 THEN gov$ = "TED"
IF gov = 7 THEN gov$ = "Mystic dictatorship"
IF gov = 8 THEN gov$ = "Totalitaran oligarchy"
IF gov = 9 THEN gov$ = "Mystic autocracy"
IF gov = 10 THEN gov$ = "Civil service bureaucracy"
IF gov = 11 THEN gov$ = "Self perpetuating Oligarchy"
IF gov = 12 THEN gov$ = "Impersonal beauraucracy"




189 '

IF law = 0 THEN law$ = "No prohibitions": cr = 0
IF law = 1 THEN law$ = "Body pistols, explosives and poison gas probibited": cr = 1
IF law = 2 THEN law$ = "Portable energy weapons prohibited": cr = 1
IF law = 3 THEN law$ = "Machineguns, automatic rifles prohibited": cr = 2
IF law = 4 THEN law$ = "Light assault weapons prohibited": cr = 2
IF law = 5 THEN law$ = "Personal concealable weapons prohibited": cr = 3
IF law = 6 THEN law$ = "All firearms exept shotguns prohibited": cr = 4
IF law = 7 THEN law$ = "Shotguns prohibited": cr = 4
IF law = 8 THEN law$ = "Long blade weapons controlled": cr = 5
IF law = 9 THEN law$ = "Possesion of weapons outside home prohibited": cr = 6
IF law > 9 THEN law$ = "Weapon possesion prohibited": cr = 6
'IF LEN(law$) > 32 THEN law$ = LEFT$(law$, 32)


IF tec = 0 THEN tecx$ = "Stone Age": gtec = 1
IF tec = 1 THEN tecx$ = "Bronze/Iron Age": gtec = 4
IF tec = 2 THEN tecx$ = "1400 - 1700": gtec = 5
IF tec = 3 THEN tecx$ = "1700 - 1860": gtec = 5.5
IF tec = 4 THEN tecx$ = "1860 - 1900": gtec = 5.9
IF tec = 5 THEN tecx$ = "1900 - 1939": gtec = 6
IF tec = 6 THEN tecx$ = "1940 - 1969": gtec = 6
IF tec = 7 THEN tecx$ = "1970 - 1979": gtec = 7
IF tec = 8 THEN tecx$ = "1980 - 2000": gtec = 8
IF tec = 9 THEN tecx$ = "Near Future": gtec = 9
IF tec = 10 THEN tecx$ = "Interstellar community": gtec = 9
IF tec = 11 THEN tecx$ = "Lower average Imperial": gtec = 9
IF tec = 12 THEN tecx$ = "Average Imperial": gtec = 10
IF tec = 13 THEN tecx$ = "Above average Imperial": gtec = 10
IF tec = 14 THEN tecx$ = "Above average Imperial": gtec = 11
IF tec = 15 THEN tecx$ = "Imperial Maximum": gtec = 12
IF tec = 16 THEN tecx$ = "Occasional Imperial": gtec = 13
IF tec < 4 THEN facility = facility - 6
IF tec > 3 AND tec < 6 THEN facility = facility - 4
IF tec > 5 AND tec < 9 THEN facility = facility - 2
IF tec > 8 AND tec < 11 THEN facility = facility - 1
IF tec > 12 AND tec < 17 THEN facility = facility + 1
IF tec > 16 THEN facility = facility + 2






tbase = tbase + tec * 100
IF bas$ = "A" THEN bax$ = "Naval + Scout Base": facility = facility + 3
IF bas$ = "S" THEN bax$ = "Scout Base": facility = facility + 2
IF bas$ = "N" THEN bax$ = "Naval Base": facility = facility + 2
IF bas$ = "Z" THEN bax$ = "Zhodani Base": facility = facility + 2
IF bas$ = "D" THEN bax$ = "Imperial Depot": facility = facility + 4
IF bas$ = "B" THEN bax$ = "Naval Base + Way Station": facility = facility + 3
IF bas$ = "W" THEN bax$ = "X-Boat Way Station": facility = facility + 3
IF bas$ = "M" THEN bax$ = "Military Base": facility = facility + 2
IF bas$ = "C" THEN bax$ = "Vargr Corsair Base"
IF bas$ = "I" THEN bax$ = "Interface World": facility = facility + 2
IF bas$ = "T" THEN bax$ = "Terminus World": facility = facility + 2












IF zon$ = "A" THEN zon$ = "Amber Zone": traffic = traffic - 2: facility = facility - 1
IF zon$ = "R" THEN zon$ = "Red Zone": traffic = traffic - 4: facility = facility - 2

IF pop < 4 THEN traffic = traffic - 4: GOTO 667
IF pop < 6 THEN traffic = traffic - 2: GOTO 667
IF pop < 7 THEN traffic = traffic - 1: GOTO 667
IF pop = 8 THEN traffic = traffic + 1: GOTO 667
IF pop = 9 THEN traffic = traffic + 3: GOTO 667
IF pop > 9 THEN traffic = traffic + 4: GOTO 667

667
agr = 0: IF (atm > 3 AND atm < 10) AND (hyd > 3 AND hyd < 9) AND (pop > 4 AND pop < 8) THEN agr = 1
ast = 0: IF siz = 0 AND atm = 0 AND hyd = 0 THEN ast = 1
bar = 0: IF pop = 0 AND gov = 0 AND law = 0 THEN bar = 1
des = 0: IF atm > 1 AND hyd = 0 THEN bar = 1
flu = 0: IF atm > 9 AND hyd > 0 THEN flu = 1
hig = 0: IF pop > 8 THEN hig = 1
ice = 0: IF atm < 2 AND hyd > 0 THEN ice = 1
ind = 0: IF (atm < 2 OR atm = 4 OR atm = 7 OR atm = 9) AND pop > 8 THEN ind = 1
low = 0: IF pop < 4 THEN low = 1
nag = 0: IF atm < 4 AND hyd < 4 AND pop > 5 THEN nag = 1
nin = 0: IF pop < 7 THEN nin = 1
por = 0: IF atm > 1 AND atm < 6 AND hyd < 4 THEN por = 1
ric = 0: IF (atm = 6 OR atm = 8) AND (pop > 5 AND pop < 9) AND (gov > 3 AND gov < 10) THEN ric = 1
vac = 0: IF atm = 0 THEN vac = 1
wat = 0: IF hyd = 10 THEN wat = 1
tmi = 1: IF agr = 1 AND atm < 8 THEN tmi = .6
exx = 0: IF siz < 1 OR atm < 3 OR atm > 9 OR ast = 1 OR des = 1 OR flu = 1 OR ice = 1 OR vac = 1 THEN exx = 1
GOSUB 9100

trade$ = ""
tdsc$ = ""
IF agr = 1 THEN trade$ = trade$ + "Ag ": tdsc$ = tdsc$ + "agricultural, ": tbase = tbase - 1000: traffic = traffic + 3
IF ast = 1 THEN trade$ = trade$ + "As ": tdsc$ = tdsc$ + "": tbase = tbase - 1000: traffic = traffic + 3
IF bar = 1 THEN trade$ = trade$ + "Ba ":  tdsc$ = tdsc$ + "barren, ": tbase = tbase + 1000: traffic = traffic - 1
IF des = 1 THEN trade$ = trade$ + "Ds ": tdsc$ = tdsc$ + "":  tbase = tbase + 1000: traffic = traffic + 1
IF flu = 1 THEN trade$ = trade$ + "Fl ": tdsc$ = tdsc$ + "":  tbase = tbase + 1000: traffic = traffic - 1
IF hig = 1 THEN trade$ = trade$ + "Hi ": tdsc$ = tdsc$ + "high population, ":  tbase = tbase - 1000: traffic = traffic + 2
IF ice = 1 THEN trade$ = trade$ + "Ic ": tdsc$ = tdsc$ + "":  tbase = tbase - 0: traffic = traffic - 1
IF ind = 1 THEN trade$ = trade$ + "In ": tdsc$ = tdsc$ + "industrial, ":  tbase = tbase - 1000: traffic = traffic + 4
IF low = 1 THEN trade$ = trade$ + "Lo ": tdsc$ = tdsc$ + "low population, ":  tbase = tbase + 1000: traffic = traffic - 1
IF nag = 1 THEN trade$ = trade$ + "Na ": tdsc$ = tdsc$ + "non-agricultural, ":  tbase = tbase + 1000
IF nin = 1 THEN trade$ = trade$ + "Ni ": tdsc$ = tdsc$ + "non-industrial, ":  tbase = tbase + 1000: traffic = traffic - 2
IF por = 1 THEN trade$ = trade$ + "Po ": tdsc$ = tdsc$ + "poor, ":  tbase = tbase - 1000: traffic = traffic - 2
IF ric = 1 THEN trade$ = trade$ + "Ri ": tdsc$ = tdsc$ + "rich, ":  tbase = tbase + 1000: traffic = traffic + 2
IF vac = 1 THEN trade$ = trade$ + "Va ": tdsc$ = tdsc$ + "":  IF ast = 0 THEN tbase = tbase + 1000
IF wat = 1 THEN trade$ = trade$ + "Wa ": tdsc$ = tdsc$ + "":  tbase = tbase - 0: traffic = traffic + 1
IF exx = 1 THEN trade$ = trade$ + "Ex ": tdsc$ = tdsc$ + "":  tbase = tbase - 0
IF tdsc$ <> "" THEN tdsc$ = LEFT$(tdsc$, (LEN(tdsc$) - 2))
facility = facility + traffic
IF othw = 1 THEN othw = 0: RETURN

agr0 = agr: ast0 = ast: bar0 = bar: des0 = des: flu0 = flu: hig0 = hig: ice0 = ice: ind0 = ind
low0 = low: nag0 = nag: nin0 = nin: por0 = por: ric0 = ric: vac0 = vac: wat0 = wat
tec0 = tec: exx0 = exx
po0$ = pol$

'Sonne
s1$ = LEFT$(sox$, 1)
mag$ = RIGHT$(sox$, 1)
s2 = VAL(MID$(sox$, 2, 1))
'IF s2 < 5 THEN s2 = 0 ELSE s2 = 5
mag = 0
mag = VAL(mag$)
IF mag = 0 AND mag$ = "V" THEN mag = 5
IF mag = 0 AND mag$ = "D" THEN mag = 6
IF mag = 0 AND mag$ = "III" THEN mag = 3
IF mag = 0 AND mag$ = "IV" THEN mag = 4
IF mag = 0 AND mag$ = "II" THEN mag = 2
mag = mag - 1
IF mag < 1 THEN mag = 5
IF mag < 0 THEN mag = 0
IF s1$ = "A" THEN solz = 0: weather = weather + 4: col$ = "Bluewhite": son$ = "blue"
IF s1$ = "F" THEN solz = 10: weather = weather + 2: col$ = "White": son$ = "white"
IF s1$ = "G" THEN solz = 20: col$ = "Yellow": son$ = "yellow"
IF s1$ = "K" THEN solz = 30: weather = weather - 1: col$ = "Orange": son$ = "orange"
IF s1$ = "M" THEN solz = 40: weather = weather - 2: col$ = "Red": son$ = "red"
solz = solz + s2
IF solz > 40 THEN solz = 40
PRINT sox$; solz; s2; mag
131 REM MAG+
IF mag > 5 THEN mag = 4
lumi = lumi(solz, mag)
smass = mass(solz, mag)
IF mag = 1 THEN stiz$ = " bright Giant"
IF mag = 2 THEN stiz$ = " Giant"
IF mag = 3 THEN stiz$ = " Subgiant"
IF mag = 4 THEN stiz$ = " Star"
IF mag = 5 THEN stiz$ = " Dwarf"











axtilt = 0: rota = 0: bse = 0: weather = 0: bsx = 0
IF datx$ = "" GOTO 2900
REM  024h025C025ø
rota = VAL(LEFT$(datx$, 3))
axtilt = VAL(MID$(datx$, 9, 3))
bse = VAL(MID$(datx$, 5, 3)): bsx = bse
2900 '
IF rota = 0 THEN rota = ((fnz(6) - 2) + (fnz(6) - 2) + (fnz(6) - 2) + (fnz(6) - 2) + 5 + (smass / au(tempzone))) * tmi
'IF press < 1 AND press > 0 THEN rota = rota * press * 3
hyz = hyd + (fnw(10) - 5) / 10
IF hyz < 0 THEN hyz = 0
IF hyz > 10 THEN hyz = 10




hvycor = 0
moltcor = 0
rockbod = 0
icebod = 0

wtyp = fnz(6)
IF siz < 5 THEN wtyp = wtyp + 1
IF siz > 5 THEN wtyp = wtyp - 2
IF atm < 4 THEN wtyp = wtyp + 1
IF atm > 5 THEN wtyp = wtyp - 2
IF wtyp < 2 THEN hvycor = 1: wtyp$ = "Heavy Core"
IF wtyp > 1 THEN moltcor = 1: wtyp$ = "Molten Core"
IF wtyp > 10 THEN rockbod = 1: wtyp$ = "Rocky Body"
IF wtyp > 14 THEN icebod = 1: wtyp$ = "Ice Body"
IF hvycor = 1 THEN density = 1.1 + fnw(115) / 100
IF moltcor = 1 THEN density = .76 + fnd(6) * .02
IF rockbod = 1 THEN density = .46 + fnd(6) * .02
IF icebod = 1 THEN density = .6 + fnd(6) * .02
IF siz = 0 THEN siz = .0001
mass = density * (siz / 8) ^ 3

grav = mass * (64 / siz ^ 2)
'Atmossphere
press = 0
ge = 1
IF atm = 1 THEN press = fnw(9) * .01: atr = 1
IF atm = 2 OR atm = 3 THEN press = .1 + fnw(30) / 100: atr = 1
IF atm = 4 OR atm = 5 THEN press = .42 + fnw(33) / 100: atr = 2: ge = 1.05
IF atm = 6 OR atm = 7 THEN press = .75 + fnw(65) / 100: atr = 2: ge = 1.1
IF atm = 8 OR atm = 9 THEN press = 1.5 + fnw(90) / 100: atr = 2: ge = 1.15
REM Exotic
IF atm <> 10 GOTO 2910
atmx$ = "Exotic"
ww = fnz(6)
IF ww < 4 THEN press = .1 + fnw(30) / 100: atr = 1
IF ww = 4 OR ww = 5 THEN press = .42 + fnw(33) / 100: atr = 2: ge = 1.05
IF ww = 6 OR ww = 7 THEN press = .75 + fnw(65) / 100: atr = 2: ge = 1.1
IF ww = 8 OR ww = 9 THEN press = 1.5 + fnw(90) / 100: atr = 2: ge = 1.15
IF ww = 10 OR ww = 11 THEN press = 2.5 + fnw(700): atr = 2: ge = 1.4
2905
IF ww = 2 OR ww = 5 OR ww = 7 OR ww = 9 OR ww = 1 THEN atmx$ = "Irritant"
IF ww = 12 THEN atmx$ = "Occasional Corrosive"

2910 REM Corrosive
IF atm <> 11 GOTO 2920
atmx$ = "Corrosive"
ww = fnz(6)
IF ww < 4 THEN press = .1 + fnw(30) / 100: atr = 1
IF ww = 4 OR ww = 5 THEN press = .42 + fnw(33) / 100: atr = 2: ge = 1.05
IF ww = 6 OR ww = 7 THEN press = .75 + fnw(65) / 100: atr = 2: ge = 1.1
IF ww = 8 OR ww = 9 THEN press = 1.5 + fnw(90) / 100: atr = 2: ge = 1.15
IF ww = 10 OR ww = 11 THEN press = 2.5 + fnw(700): atr = 2: ge = 1.4

2920 REM Insidious
IF atm <> 12 GOTO 2930
atmx$ = "Insidious"
press = 200 + fnw(700): atr = 2: ge = 2.2
2930

IF atm = 13 OR atm = 14 THEN press = 2 + fnw(750): ge = 1.15
GOSUB 9800
IF atm > 9 THEN atr = 3
IF atm = 15 THEN atr = 4
IF hyd > 4 THEN weather = weather + 1
bse = 0

txx = -273: tempzone = 0

minte = 0: maxte = 20: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -10: maxte = 40: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -20: maxte = 60: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -30: maxte = 70: GOSUB 12000: IF txx > -273 GOTO 3290

minte = -35: maxte = 80: GOSUB 12000: IF txx > -273 GOTO 3290

IF txx = -273 THEN temp = lumi * orf(0) * hz(hyd, atr) * ge - 273: tempzone = 0: bse = temp: ' IF mag > 1 THEN mag = mag - 1: GOTO 131

3290 period = SQR(au(tempzone) * au(tempzone) * au(tempzone) / smass)

IF bsx > 0 THEN bse = bsx
liv = 0: IF atm = 0 THEN liv = -3
IF atm > 3 AND atm < 10 THEN liv = 4
IF hyd = 0 THEN liv = liv - 2
IF hyd > 1 AND hyd < 9 THEN liv = liv + 1
IF bse < -20 THEN liv = liv - 1
IF bse > 30 THEN liv = liv - 1
IF s1$ = "G" OR s1$ = "K" THEN liv = liv + 1
IF s1$ = "F" OR s1$ = "A" OR s1$ = "B" THEN liv = liv - 1
IF agr = 1 THEN liv = liv + 3
life = 0: IF fnz(6) + liv > 10 THEN life = 1

'IF atm < 10 THEN RETURN

seismic = fnw(3)
IF hvco = 1 THEN seismic = seismic + fnw(2)
IF moltcor = 1 THEN seismic = seismic + fnw(3)
seismic = seismic + smass / au(tempzone)
IF axtilt > 0 GOTO 3400
ww = fnz(6)
IF ww < 4 THEN axtilt = fnz(6) - 2: weather = weather - 1: GOTO 3400
IF ww < 6 THEN axtilt = fnz(6) - 2 + 10: GOTO 3400
IF ww < 8 THEN axtilt = fnz(6) - 2 + 20: weather = weather + 1: GOTO 3400
IF ww < 10 THEN axtilt = fnz(6) - 2 + 30: weather = weather + 2: GOTO 3400
IF ww < 12 THEN axtilt = fnz(6) - 2 + 40: weather = weather + 2: GOTO 3400
IF ww = 12 THEN axtilt = 40 + fnw(50): GOTO 3400
3400 'Season
IF axtilt = 0 THEN S00 = 0: S75 = 0: S50 = 0: S25 = 11: weather = weather - 1: GOTO 3500
IF axtilt = 1 THEN S00 = 0: S75 = 0: S50 = 11: S25 = 10: GOTO 3500
IF axtilt < 4 THEN S00 = 0: S75 = 11: S50 = 10: S25 = 9: GOTO 3500
IF axtilt < 6 THEN S00 = 11: S75 = 10: S50 = 9: S25 = 8: GOTO 3500
IF axtilt < 9 THEN S00 = 10: S75 = 9: S50 = 8: S25 = 7: GOTO 3500
IF axtilt < 13 THEN S00 = 9: S75 = 8: S50 = 7: S25 = 6: GOTO 3500
IF axtilt < 17 THEN S00 = 8: S75 = 7: S50 = 6: S25 = 5: GOTO 3500
IF axtilt < 23 THEN S00 = 7: S75 = 6: S50 = 5: S25 = 4: GOTO 3500
IF axtilt < 29 THEN S00 = 6: S75 = 5: S50 = 4: S25 = 3: GOTO 3500
IF axtilt < 35 THEN S00 = 5: S75 = 4: S50 = 3: S25 = 2: GOTO 3500
IF axtilt < 45 THEN S00 = 4: S75 = 3: S50 = 2: S25 = 1: GOTO 3500
IF axtilt < 60 THEN S00 = 3: S75 = 2: S50 = 1: S25 = 0: GOTO 3500
IF axtilt < 85 THEN S00 = 2: S75 = 1: S50 = 0: S25 = 0: GOTO 3500
S00 = 1: S75 = 0: S50 = 0: S25 = 0: GOTO 3500

3500 'day & night
IF axtilt > 44 THEN weather = weather + 2
IF press < .01 THEN inc = 1: pluslim = (bse + 273) * .1: dec = 20: minlim = (bse + 273) * .8: GOTO 3600
IF press < .1 THEN inc = .9: pluslim = (bse + 273) * .3: dec = 15: minlim = (bse + 273) * .7: GOTO 3600
IF press < .41 THEN inc = .8: pluslim = (bse + 273) * .8: dec = 8: minlim = (bse + 273) * .5: GOTO 3600
IF press < .76 THEN inc = .6: pluslim = (bse + 273) * 1.5: dec = 3: minlim = (bse + 273) * .3: GOTO 3600
IF press < 1.4 THEN inc = .5: pluslim = (bse + 273) * 2.5: dec = 1: minlim = (bse + 273) * .15: GOTO 3600
IF press < 2.4 THEN inc = .4: pluslim = (bse + 273) * 4!: dec = .5: minlim = (bse + 273) * .1: GOTO 3600
                   inc = .2: pluslim = (bse + 273) * 5!: dec = .2: minlim = (bse + 273) * .05: GOTO 3600
3600

tmult = 24 / rota
jz = period * 365.25 / 4 * tmult
day = rota / 2 * inc:  IF day > pluslim THEN day = pluslim
night = rota / 2 * dec: IF night > minlim THEN night = minlim

IF atm = 0 THEN weather = 0
RETURN

3300 REM (1) Temperatur
IF bse < -50 THEN GOTO 3310
jz = period * 365.25 / 4 * tmult
CLS
PRINT " "; upp$
    : PRINT "ÚÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
PRINT USING "³ LR ³ Spring   ###### ³ Summer ######## ³ Autumn ######## ³ Winter ######## ³"; jz; jz; jz; jz;
      PRINT "ÃÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÅÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄ´";
REM:  PRINT "ÃÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÒÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄ´";
FOR I = 1 TO 11
latmod = lat(I, 11 - siz): PRINT
summer = .6 * axtilt: winter = -1 * axtilt

perz = 0
IF I >= S00 THEN perz = 1: GOTO 3700
IF I >= S75 THEN perz = .75: GOTO 3700
IF I >= S50 THEN perz = .5: GOTO 3700
IF I >= S25 THEN perz = .25
3700

sutemp = latmod + bse + summer * perz
witemp = latmod + bse + winter * perz

day = rota / 2 * inc:  IF day > pluslim THEN day = pluslim
night = rota / 2 * dec: IF night > minlim THEN night = minlim



PRINT USING "³ ## º####ø³####ø³####øº"; I; latmod + bse + day; latmod + bse; latmod + bse - night;
PRINT USING "####ø³####ø³####øº"; sutemp + day; sutemp; sutemp - night;
PRINT USING "####ø³####ø³####øº"; latmod + bse + day; latmod + bse; latmod + bse - night;
PRINT USING "####ø³####ø³####øº"; witemp + day; witemp; witemp - night;
REM IF i < 11 THEN PRINT "ÃÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄ´";
NEXT
      PRINT "ÀÄÄÄÄÁÄÄÂÄÄÁÄÄÂÄÄÁÄÄÂÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÙ"
PRINT USING "        ³     ³     ³      Length of day         ###:##"; INT(rota); (rota - INT(rota)) * 60
      PRINT "        ³     ³     ³"
PRINT USING "        ³     ³     ÀÄÄÄÄÄÄDeepest Night temp at  ##:00"; 5 / 24 * rota
PRINT USING "        ³     ÀÄÄÄÄÄÄÄÄÄÄÄÄMedium values at       ##:00 and ##:00"; 10 / 24 * rota; 20 / 24 * rota
PRINT USING "        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄHighest Noon temp at   ##:00"; 15 / 24 * rota

GOTO 101





3310 REM (2) Worlddata

CLS
GOSUB 3390: REM Seismic

PRINT datx$
IF bse = 0 AND txx <> 0 THEN INPUT "Base temperature "; bse

CLS : PRINT " "; upp$: PRINT
                      PRINT " Sun                : "; col$; stiz$; " "; sox$
                PRINT USING " Luminosity         :#####%"; lumi * 100
                PRINT USING " Solar Mass         :#####%"; smass * 100
                PRINT USING " Subjective Size    :#####%"; smass / au(tempzone) * 100
IF gas > 0 THEN PRINT USING " Gas Giants         :#####"; gas
IF asg > 0 THEN PRINT USING " Asteroid belts     :#####"; asg
PRINT
               
                PRINT USING " Orbit              :##### =####.## Mio km"; tempzone; au(tempzone) * 149.6
                PRINT USING " Axial tilt         :#####.##ø"; axtilt
                PRINT USING " Density            :#####.## "; density; : PRINT wtyp$
                PRINT USING " Seismic Factor     :#####.##"; seismic
                PRINT USING " Air pressure       :#####.## Atm, #####, m"; press; height
                PRINT USING " Weather factor     :#####%##"; ((weather + 5) / 14 * 100); weather
               ' PRINT USING " Energy absorption  :#####%"; hz(hyd, atr) * 100
IF life = 1 THEN PRINT USING " Native life        :#####"; liv
PRINT
tmult = 24 / rota
          PRINT USING " Year length        :#####.## Terra =##### planetary days"; period; period * 365.25 * tmult
PRINT
          PRINT USING " Length of day      :#####.## hours"; rota
          PRINT USING " Temperature        :#####ø Celsius"; bse
          PRINT USING " Highest Noontemp   :#####:00"; 15 / 24 * rota
          PRINT USING " Middle values      :##### und###:00"; 10 / 24 * rota; 20 / 24 * rota
          PRINT USING " Deepest nighttemp  :#####:00"; 5 / 24 * rota;
GOTO 101


3330 REM Government

GOSUB 14000
yard = wars * (10 ^ pop * mul) / 1000 * valmo / ((7 - stp) / 6)
'IF stp > 3 THEN yard = 0
IF yard < 10 THEN yard = CINT(yard): GOTO 123
IF yard < 100 THEN yard = CINT(yard / 10) * 10: GOTO 123
IF yard < 1000 THEN yard = CINT(yard / 100) * 100: GOTO 123
IF yard < 10000 THEN yard = CINT(yard / 1000) * 1000: GOTO 123
IF yard < 100000 THEN yard = INT(yard / 10000) * 10000: GOTO 123
IF yard < 1000000 THEN yard = CINT(yard / 100000) * 100000: GOTO 123
yard = CINT(yard / 1000000!) * 1000000!
123 'IF tec < 8 THEN yard = 0

CLS : PRINT " "; upp$: PRINT
                    PRINT " PORT        : "; stpx$;
IF stp$ = "A" OR stp$ = "B" OR stp$ = "C" THEN PRINT " up to "; : value = yard: GOSUB 13000: PRINT " tons" ELSE PRINT "": yard = 0
PRINT
                    PRINT " TRAFFIC     : "; traffic$; ", ca."; CINT(tx); trader$; " per "; tx$
PRINT
                    PRINT " PASSENGERS  : ca. "; : value = tpz / 365: GOSUB 13000: PRINT " per day"
PRINT
                   
                    PRINT " FACILITIES  : "; facility$
PRINT
                    PRINT " EXTRALITY   : "; extrality$
PRINT


navy = wbudget * valmo * 10
IF yard = 0 THEN navy = 0
PRINT " NAVYSIZE    :";
BattleI = 3000000! * 1000000!
BattleL = 1300000 * 1000000!
CrusArm = 100000 * 1000000!
CrusHvy = 30000 * 1000000!
CrusLgt = 15000 * 1000000!
EscDest = 8000 * 1000000!
FleeEsc = 2000 * 1000000!
DestEsc = 300 * 1000000!
Scout = 25 * 1000000!
fight = 14 * 1000000!



IF navy > BattleI AND yard >= 500000 THEN PRINT CINT(navy / BattleI); "x Dreadnoughts (500000 t)": GOTO 102
IF navy > BattleL AND yard >= 200000 THEN PRINT CINT(navy / BattleL); "x Light Battleships (200000 t)": GOTO 102
IF navy > CrusArm AND yard >= 100000 THEN PRINT CINT(navy / CrusArm); "x Armored Cruiser (100000 t)": GOTO 102
IF navy > CrusHvy AND yard >= 75000 THEN PRINT CINT(navy / CrusHvy); "x Heavy Cruiser (75000 t)": GOTO 102
IF navy > CrusLgt AND yard >= 30000 THEN PRINT CINT(navy / CrusLgt); "x Light Cruisers (30000 t)": GOTO 102
IF navy > EscDest AND yard >= 15000 THEN PRINT CINT(navy / EscDest); "x Destroyer (15000 t)": GOTO 102
IF navy > FleeEsc AND yard >= 2000 THEN PRINT CINT(navy / FleeEsc); "x FleetEscort (2000 t)": GOTO 102
IF navy > DestEsc AND yard >= 600 THEN PRINT CINT(navy / DestEsc); "x Gazelles/SDB (400 t)": GOTO 102
IF navy > Scout AND yard >= 100 THEN PRINT CINT(navy / Scout); "x Scouts (100 t)": GOTO 102
IF navy > fight AND yard > 0 THEN PRINT CINT(navy / fight); "x Fighter (20 t)": GOTO 102
PRINT " No Navy"

102 PRINT : PRINT
IF pxl$ <> " " THEN PRINT " ALIGNMENT   : "; pxl$; : IF zon$ <> " " THEN PRINT "-"; zon$ ELSE PRINT
REM IF zon$ <> " " THEN PRINT " ZONE        : "; zon$: PRINT
PRINT
                    PRINT " GOVERNMENT  : "; gov$: PRINT
             REM    PRINT " MIL. BUDGET : "; : value = pbudget * valmo: GOSUB 13000: PRINT "(Peace)   "; : value = wbudget * valmo: GOSUB 13000: PRINT "(War)": PRINT
                    PRINT " LAW LEVEL   : "; law$: PRINT
                    PRINT " TECH LEVEL  : "; tecx$: PRINT
 'IF bax$ <> "" THEN PRINT " BASES       : "; bax$: PRINT
PRINT USING " EXCHANGE    : #.## Credits"; 1 / valmo;
GOTO 101


3360 REM Resources
CLS : PRINT " "; upp$: PRINT
GOSUB 9700: text$ = tx$: GOSUB 10000: PRINT

GOSUB 9900
REM agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
REM parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 1: artfo = artfo + 0: softw = softw + 1: docum = docum + 2
GOSUB 8000: GOTO 101
GOSUB 3370: text$ = resource$: IF pop > 0 AND LEN(text$) > 15 THEN GOSUB 10000


GOTO 101

3370
resource$ = "Warenangebot: "
axa = fnz(6)
IF agric >= fnz(6) THEN res$ = "Getreide, Frchte " + val$(agric) + ", ": GOSUB 3371
IF ores >= fnz(6) THEN res$ = "Roherze " + val$(ores) + ", ": GOSUB 3371
IF radio >= fnz(6) THEN res$ = "radioaktive Stoffe " + val$(radio) + ", ": GOSUB 3371
IF cryst >= fnz(6) THEN res$ = "Kristalle " + val$(cryst) + ", ": GOSUB 3371
IF agrop >= fnz(6) THEN res$ = "verarbeitete Nahrungsmittel " + val$(agrop) + ", ": GOSUB 3371
IF metal >= fnz(6) THEN res$ = "legierte Metalle, Stahl " + val$(metal) + ", ": GOSUB 3371
IF nonme >= fnz(6) THEN res$ = "Kunststoffprodukte " + val$(nonme) + ", ": GOSUB 3371
IF parts >= fnz(6) THEN res$ = "fertige Einzelteile " + val$(parts) + ", ": GOSUB 3371
IF durab >= fnz(6) THEN res$ = "fertige Dauerwaren " + val$(durab) + ", ": GOSUB 3371
IF consu >= fnz(6) THEN res$ = "fertige Konsumgter " + val$(consu) + ", ": GOSUB 3371
IF weapo >= fnz(6) THEN res$ = "Waffen " + val$(weapo) + ", ": GOSUB 3371
IF recor >= fnz(6) THEN res$ = "Bild- und Tonaufnahmen " + val$(recor) + ", ": GOSUB 3371
IF artfo >= fnz(6) THEN res$ = "Kunstgegenst„nde " + val$(artfo) + ", ": GOSUB 3371
IF softw >= fnz(6) THEN res$ = "Software " + val$(softw) + ", ": GOSUB 3371
IF docum >= fnz(6) THEN res$ = "Dokumente " + val$(docum) + ", ": GOSUB 3371
resource$ = LEFT$(resource$, LEN(resource$) - 2) + "."
RETURN
3371 resource$ = resource$ + res$: RETURN


4000 'Solar
'CLS

RESTORE 4000
FOR I = 0 TO 9
FOR j = 1 TO 5
READ mass(I * 5, j)

NEXT
READ q$
FOR j = 1 TO 5
READ lumi(I * 5, j)
NEXT
READ q$
NEXT

DATA 14    , 12    ,  6    ,  3.2  ,  0.36,   'A0 Mass
DATA  6.85 ,  4.09 ,  4.53 ,  3.08 ,  0.27,   'A0 Lum

DATA 11    ,  9    ,  4    ,  2.1  ,  0.39,   'A5 Mass
DATA  5.4  ,  3.08 ,  2.47 ,  2    ,  0.20,   'A5 Lum

DATA 10    ,  8    ,  2.5  ,  1.7  ,  0.42,   'F0 Mass
DATA  4.95 ,  2.7  ,  2.09 ,  1.69 ,  0.13,   'F0 Lum

DATA  8.1  ,  5    ,  2    ,  1.3  ,  0.51,   'F5 Mass
DATA  4.75 ,  2.56 ,  1.86 ,  1.37 ,  0.11,   'F5 Lum

DATA  8.1  ,  2.5  ,  1.75 ,  1.04 ,  0.63,   'G0 Mass
DATA  4.86 ,  2.66 ,  1.6  ,  1.05 ,  0.09,   'G0 Lum

DATA 10    ,  3.2  ,  2    ,   .94 ,  0.73,   'G5 Mass
DATA  5.22 ,  2.94 ,  1.49 ,   .9  ,  0.09,   'G5 Lum

DATA 11    ,  4    ,  2.3  ,   .825,   .83,   'K0 Mass
DATA  5.46 ,  3.12 ,  1.47 ,   .81 ,   .08,   'K0 Lum

DATA 14    ,  5    ,  2.6  ,   .57 ,   .97,   'K5 Mass
DATA  7.04 ,  4.23 ,  1.45 ,   .53 ,   .08,   'K5 Lum

DATA 16    ,  6.3  ,  0    ,   .489,  1.11,   'M0 Mass
DATA  8.24 ,  4.65 ,  0    ,   .45 ,   .07,   'M0 Lum

DATA 16    ,  7.4  ,  0    ,   .331,  1.20,   'M5 Mass
DATA 11.05 ,  6.91 ,  0    ,   .29 ,  0.05,   'M5 Lum


DATA    .2  ,   .4  ,   .7  ,  1    ,  1.6  ,  2.8  ,  5.2  , 10    ,19.6  ,38.8  ,77.2  ,154   ,307.6 ,614.8 ,1229.2,2548 ,4915 ,9830.8,19661.2,39322
DATA 836.345,591.385,447.045,374.025,295.693,223.523,164.021,118.277,84.484,60.046,42.569,30.140,21.326,15.085,10.668,7.554,5.335, 3.772,  2.667,1.886
FOR I = 0 TO 19: READ au(I): NEXT
FOR I = 0 TO 19: READ orf(I): NEXT
RETURN
5000



RESTORE 5000
FOR I = 0 TO 10
FOR j = 1 TO 4
READ nhz(I, j), hz(I, j)
nhz(I, j) = nhz(I, j) / 1000:
hz(I, j) = hz(I, j) / 1000
NEXT
NEXT

DATA 800 , 900 , 800 , 900 , 680 , 740 , 800 , 900
DATA 744 , 829 , 811 , 900 , 646 , 697 , 811 , 900
DATA 736 , 803 , 789 , 860 , 635 , 672 , 807 , 882
DATA 752 , 811 , 799 , 860 , 644 , 676 , 817 , 883
DATA 738 , 782 , 774 , 820 , 625 , 648 , 813 , 866
DATA 753 , 789 , 747 , 780 , 599 , 613 , 809 , 850
DATA 767 , 795 , 718 , 740 , 570 , 577 , 805 , 836
DATA 782 , 802 , 687 , 700 , 537 , 539 , 800 , 821
DATA 796 , 808 , 654 , 660 , 500 , 500 , 794 , 807
DATA 810 , 814 , 619 , 620 , 500 , 500 , 787 , 793
DATA 818 , 818 , 619 , 619 , 500 , 500 , 773 , 773
RETURN

3390 REM seismic

RETURN

3900 REM String auslesen
dat$ = MID$(upp$, 14, 4)
stp$ = MID$(upp$, 20, 1)
siz$ = MID$(upp$, 21, 1)
atm$ = MID$(upp$, 22, 1)
hyd$ = MID$(upp$, 23, 1)
pop$ = MID$(upp$, 24, 1)
gov$ = MID$(upp$, 25, 1)
law$ = MID$(upp$, 26, 1)
tec$ = MID$(upp$, 28, 1)
bas$ = MID$(upp$, 31, 1)
tra$ = MID$(upp$, 33, 17)
zon$ = MID$(upp$, 49, 1)
pox$ = MID$(upp$, 50, 1)
mul$ = MID$(upp$, 52, 1)
ast$ = MID$(upp$, 53, 1)
gas$ = MID$(upp$, 54, 1)
pol$ = MID$(upp$, 56, 2)

sol$ = MID$(upp$, 59, 20)
sox$ = LEFT$(sol$, 2)
so1$ = ""
FOR r = 4 TO 6
m$ = MID$(sol$, r, 1)
IF m$ <> " " THEN so1$ = so1$ + m$ ELSE r = 7
NEXT
IF so1$ = "II" THEN mag$ = "2"
IF so1$ = "III" THEN mag$ = "3"
IF so1$ = "V" THEN mag$ = "5"
IF so1$ = "VI" OR so1$ = "D" THEN mag$ = "6": mag$ = "5"
IF so1$ = "IV" THEN mag$ = "4"
sox$ = sox$ + mag$
REM PRINT sol$, sox$; so1$, mag$, solz; s2; mag: GOSUB 1000
IF RIGHT$(upp$, 1) = "ø" THEN datx$ = RIGHT$(upp$, 12): upp$ = LEFT$(upp$, LEN(upp$) - LEN(datx$)) ELSE datx$ = ""
RETURN
REM  024h025C025ø







9000 REM Trade & Commerce
modi = 0
bsp = 5000
IF agr0 = 1 AND agr = 1 THEN modi = modi + 1
IF agr0 = 1 AND ast = 1 THEN modi = modi + 1
IF agr0 = 1 AND des = 1 THEN modi = modi + 1
IF agr0 = 1 AND hig = 1 THEN modi = modi + 1
IF agr0 = 1 AND ind = 1 THEN modi = modi + 1
IF agr0 = 1 AND low = 1 THEN modi = modi + 1
IF agr0 = 1 AND nag = 1 THEN modi = modi + 1
IF agr0 = 1 AND ric = 1 THEN modi = modi + 1

IF ast0 = 1 AND ast = 1 THEN modi = modi + 1
IF ast0 = 1 AND ind = 1 THEN modi = modi + 1
IF ast0 = 1 AND nag = 1 THEN modi = modi + 1
IF ast0 = 1 AND ric = 1 THEN modi = modi + 1
IF ast0 = 1 AND vac = 1 THEN modi = modi + 1

IF bar0 = 1 AND agr = 1 THEN modi = modi + 1
IF bar0 = 1 AND ind = 1 THEN modi = modi + 1

IF des0 = 1 AND des = 1 THEN modi = modi + 1
IF des0 = 1 AND nag = 1 THEN modi = modi + 1

IF flu0 = 1 AND flu = 1 THEN modi = modi + 1
IF flu0 = 1 AND ind = 1 THEN modi = modi + 1

IF hig0 = 1 AND hig = 1 THEN modi = modi + 1
IF hig0 = 1 AND low = 1 THEN modi = modi + 1
IF hig0 = 1 AND ric = 1 THEN modi = modi + 1

IF ice0 = 1 AND ind = 1 THEN modi = modi + 1

IF ind0 = 1 AND agr = 1 THEN modi = modi + 1
IF ind0 = 1 AND ast = 1 THEN modi = modi + 1
IF ind0 = 1 AND des = 1 THEN modi = modi + 1
IF ind0 = 1 AND flu = 1 THEN modi = modi + 1
IF ind0 = 1 AND hig = 1 THEN modi = modi + 1
IF ind0 = 1 AND ind = 1 THEN modi = modi + 1
IF ind0 = 1 AND nin = 1 THEN modi = modi + 1
IF ind0 = 1 AND por = 1 THEN modi = modi + 1
IF ind0 = 1 AND ric = 1 THEN modi = modi + 1
IF ind0 = 1 AND vac = 1 THEN modi = modi + 1
IF ind0 = 1 AND wat = 1 THEN modi = modi + 1

IF low0 = 1 AND ind = 1 THEN modi = modi + 1
IF low0 = 1 AND ric = 1 THEN modi = modi + 1

IF nag0 = 1 AND ast = 1 THEN modi = modi + 1
IF nag0 = 1 AND des = 1 THEN modi = modi + 1
IF nag0 = 1 AND vac = 1 THEN modi = modi + 1

IF nin0 = 1 AND ind = 1 THEN modi = modi + 1
IF nin0 = 1 AND nin = 1 THEN modi = modi - 1

IF por0 = 1 AND por = 1 THEN modi = modi - 1

IF ric0 = 1 AND agr = 1 THEN modi = modi + 1
IF ric0 = 1 AND des = 1 THEN modi = modi + 1
IF ric0 = 1 AND hig = 1 THEN modi = modi + 1
IF ric0 = 1 AND ind = 1 THEN modi = modi + 1
IF ric0 = 1 AND nag = 1 THEN modi = modi + 1
IF ric0 = 1 AND ric = 1 THEN modi = modi + 1

IF vac0 = 1 AND ast = 1 THEN modi = modi + 1
IF vac0 = 1 AND ind = 1 THEN modi = modi + 1
IF vac0 = 1 AND vac = 1 THEN modi = modi + 1

IF wat0 = 1 AND ind = 1 THEN modi = modi + 1
IF wat0 = 1 AND ric = 1 THEN modi = modi + 1
IF wat0 = 1 AND wat = 1 THEN modi = modi + 1




bsp = 5000 + modi * 1000
bsp = bsp + bsp * ((tec0 - tec) * .1)
RETURN

9100 REM Gurps Far Trader

IF tec < 2 THEN tlm = -.5: GOTO 9101
IF tec < 6 THEN tlm = 0: GOTO 9101
IF tec < 9 THEN tlm = .5: GOTO 9101
IF tec < 12 THEN tlm = 1: GOTO 9101
IF tec < 16 THEN tlm = 1.5: GOTO 9101
IF tec > 15 THEN tlm = 2: GOTO 9101

9101 uwtn = tlm + (pop + mul / 10) / 2

IF uwtn > 6.5 AND stp$ = "A" THEN pm = 0: GOTO 9102
IF uwtn > 6.5 AND stp$ = "B" THEN pm = -1: GOTO 9102
IF uwtn > 6.5 AND stp$ = "C" THEN pm = -1.5: GOTO 9102
IF uwtn > 6.5 AND stp$ = "D" THEN pm = -2: GOTO 9102
IF uwtn > 6.5 AND stp$ = "E" THEN pm = -2.5: GOTO 9102
IF uwtn > 6.5 AND stp$ = "X" THEN pm = -5: GOTO 9102

IF uwtn > 5.5 AND stp$ = "A" THEN pm = 0: GOTO 9102
IF uwtn > 5.5 AND stp$ = "B" THEN pm = -.5: GOTO 9102
IF uwtn > 5.5 AND stp$ = "C" THEN pm = -1: GOTO 9102
IF uwtn > 5.5 AND stp$ = "D" THEN pm = -1.5: GOTO 9102
IF uwtn > 5.5 AND stp$ = "E" THEN pm = -2: GOTO 9102
IF uwtn > 5.5 AND stp$ = "X" THEN pm = -4.5: GOTO 9102

IF uwtn > 4.5 AND stp$ = "A" THEN pm = 0: GOTO 9102
IF uwtn > 4.5 AND stp$ = "B" THEN pm = 0: GOTO 9102
IF uwtn > 4.5 AND stp$ = "C" THEN pm = -.5: GOTO 9102
IF uwtn > 4.5 AND stp$ = "D" THEN pm = -1: GOTO 9102
IF uwtn > 4.5 AND stp$ = "E" THEN pm = -1.5: GOTO 9102
IF uwtn > 4.5 AND stp$ = "X" THEN pm = -3.5: GOTO 9102

IF uwtn > 3.5 AND stp$ = "A" THEN pm = .5: GOTO 9102
IF uwtn > 3.5 AND stp$ = "B" THEN pm = 0: GOTO 9102
IF uwtn > 3.5 AND stp$ = "C" THEN pm = 0: GOTO 9102
IF uwtn > 3.5 AND stp$ = "D" THEN pm = -.5: GOTO 9102
IF uwtn > 3.5 AND stp$ = "E" THEN pm = -1: GOTO 9102
IF uwtn > 3.5 AND stp$ = "X" THEN pm = -2.5: GOTO 9102

IF uwtn > 2.5 AND stp$ = "A" THEN pm = .5: GOTO 9102
IF uwtn > 2.5 AND stp$ = "B" THEN pm = .5: GOTO 9102
IF uwtn > 2.5 AND stp$ = "C" THEN pm = 0: GOTO 9102
IF uwtn > 2.5 AND stp$ = "D" THEN pm = 0: GOTO 9102
IF uwtn > 2.5 AND stp$ = "E" THEN pm = -.5: GOTO 9102
IF uwtn > 2.5 AND stp$ = "X" THEN pm = -3: GOTO 9102

IF uwtn > 1.5 AND stp$ = "A" THEN pm = 1: GOTO 9102
IF uwtn > 1.5 AND stp$ = "B" THEN pm = .5: GOTO 9102
IF uwtn > 1.5 AND stp$ = "C" THEN pm = .5: GOTO 9102
IF uwtn > 1.5 AND stp$ = "D" THEN pm = 0: GOTO 9102
IF uwtn > 1.5 AND stp$ = "E" THEN pm = 0: GOTO 9102
IF uwtn > 1.5 AND stp$ = "X" THEN pm = -2.5: GOTO 9102

IF uwtn > .5 AND stp$ = "A" THEN pm = 1: GOTO 9102
IF uwtn > .5 AND stp$ = "B" THEN pm = 1: GOTO 9102
IF uwtn > .5 AND stp$ = "C" THEN pm = .5: GOTO 9102
IF uwtn > .5 AND stp$ = "D" THEN pm = .5: GOTO 9102
IF uwtn > .5 AND stp$ = "E" THEN pm = 0: GOTO 9102
IF uwtn > .5 AND stp$ = "X" THEN pm = 0: GOTO 9102



IF stp$ = "A" THEN pm = 1.5: GOTO 9102
IF stp$ = "B" THEN pm = 1: GOTO 9102
IF stp$ = "C" THEN pm = 1: GOTO 9102
IF stp$ = "D" THEN pm = .5: GOTO 9102
IF stp$ = "E" THEN pm = .5: GOTO 9102
IF stp$ = "X" THEN pm = 0: GOTO 9102

9102
wtn = uwtn + pm
btn = wtn
RETURN
9900 REM Resources

agric = 0: ores = 0: radio = 0: cryst = 0: compo = 0: agrop = 0: metal = 0: nonme = 0: parts = 0: durab = 0: consu = 0: weapo = 0: recor = 0: artfo = 0: softw = 0: docum = 0


IF hvycor = 1 THEN agric = agric + 1: ores = ores + 8: radio = radio + 7: cryst = cryst + 6: compo = compo + 5: agrop = agrop + 0: metal = metal + 2: nonme = nonme + 1
IF hvycor = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF moltcor = 1 THEN agric = agric + 4: ores = ores + 7: radio = radio + 5: cryst = cryst + 5: compo = compo + 6: agrop = agrop + 1: metal = metal + 0: nonme = nonme + 0
IF moltcor = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF rockbod = 1 THEN agric = agric + 4: ores = ores + 3: radio = radio + 3: cryst = cryst + 2: compo = compo + 1: agrop = agrop + 1: metal = metal + 0: nonme = nonme + 0
IF rockbod = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF icebod = 1 THEN agric = agric - 4: ores = ores + 7: radio = radio + 5: cryst = cryst + 5: compo = compo + 6: agrop = agrop + 1: metal = metal + 0: nonme = nonme + 0
IF icebod = 1 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF atm > 3 AND atm < 10 THEN agric = agric + 1: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 0: nonme = nonme + 1
IF atm > 3 AND atm < 10 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + -1

IF atm < 4 OR atm > 9 THEN agric = agric - 3: ores = ores + 1: radio = radio + 1: cryst = cryst + 0: compo = compo + 1: agrop = agrop + 0: metal = metal + 1: nonme = nonme + 1
IF atm < 4 OR atm > 9 THEN parts = parts + 1: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + -1

IF pop < 5 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 1: metal = metal - 1: nonme = nonme + 0
IF pop < 5 THEN parts = parts - 1: durab = durab - 1: consu = consu - 1: weapo = weapo - 1: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + -1

IF pop > 4 AND pop < 9 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 1: nonme = nonme + 1
IF pop > 4 AND pop < 9 THEN parts = parts + 1: durab = durab + 2: consu = consu + 1: weapo = weapo + 0: recor = recor + 1: artfo = artfo + 2: softw = softw + 1: docum = docum + 0

IF pop > 9 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 1: nonme = nonme + 1
IF pop > 9 THEN parts = parts + 2: durab = durab + 3: consu = consu + 4: weapo = weapo + 1: recor = recor + 2: artfo = artfo + 3: softw = softw + 4: docum = docum + 1

IF gov < 2 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov < 2 THEN parts = parts + -1: durab = durab + -1: consu = consu + -1: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF gov > 1 AND gov < 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov > 1 AND gov < 7 THEN parts = parts + 1: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 1: artfo = artfo + 1: softw = softw + 1: docum = docum + 1

IF gov = 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov = 7 THEN parts = parts + 2: durab = durab + 2: consu = consu + 2: weapo = weapo + 3: recor = recor + 1: artfo = artfo + 2: softw = softw + 1: docum = docum + 0

IF gov > 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF gov > 7 THEN parts = parts + 0: durab = durab + 0: consu = consu + 1: weapo = weapo + 1: recor = recor + 2: artfo = artfo + 0: softw = softw + 1: docum = docum + 4

IF law < 3 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law < 3 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF law > 2 AND law < 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law > 2 AND law < 7 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 1: artfo = artfo + 0: softw = softw + 1: docum = docum + 2

IF law > 6 AND law < 10 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law > 6 AND law < 10 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 2: artfo = artfo + 0: softw = softw + 2: docum = docum + 4

IF law > 9 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 0: metal = metal + 0: nonme = nonme + 0
IF law > 9 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + 3: artfo = artfo + 0: softw = softw + 3: docum = docum + 6

IF tec < 4 THEN agric = agric + 1: ores = ores + 1: radio = radio + 1: cryst = cryst + 1: compo = compo + 1: agrop = agrop + 1: metal = metal + -1: nonme = nonme + 0
IF tec < 4 THEN parts = parts + 0: durab = durab + 0: consu = consu + 0: weapo = weapo + 0: recor = recor + -3: artfo = artfo + 2: softw = softw - 9: docum = docum + 0

IF tec > 3 AND tec < 7 THEN agric = agric + 0: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 2: metal = metal + 2: nonme = nonme + 2
IF tec > 3 AND tec < 7 THEN parts = parts + 0: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 1: artfo = artfo + 1: softw = softw + 0: docum = docum + 1

IF tec > 6 AND tec < 12 THEN agric = agric - 1: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 0: agrop = agrop + 1: metal = metal + 4: nonme = nonme + 4
IF tec > 6 AND tec < 12 THEN parts = parts + 2: durab = durab + 2: consu = consu + 2: weapo = weapo + 1: recor = recor + 2: artfo = artfo + 1: softw = softw + 1: docum = docum + 3

IF tec > 11 THEN agric = agric - 2: ores = ores + 1: radio = radio + 1: cryst = cryst + 1: compo = compo + 1: agrop = agrop + 1: metal = metal + 5: nonme = nonme + 6
IF tec > 11 THEN parts = parts + 4: durab = durab + 3: consu = consu + 4: weapo = weapo + 2: recor = recor + 3: artfo = artfo + 1: softw = softw + 4: docum = docum + 1

IF life = 1 THEN agric = agric + 5: ores = ores + 0: radio = radio + 0: cryst = cryst + 0: compo = compo + 1: agrop = agrop + 5: metal = metal + 0: nonme = nonme + 3
IF life = 1 THEN parts = parts + 1: durab = durab + 1: consu = consu + 1: weapo = weapo + 1: recor = recor + 0: artfo = artfo + 0: softw = softw + 0: docum = docum + 0

IF life = 0 THEN compo = compo - 1

IF bse < -5 THEN agric = agric - 5: agrop = agrop - 5


RETURN

9800 REM H”henwerte
IF press > 1 THEN height = (press - 1) * -2500
IF press > .95 AND press < 1 THEN height = 0
IF press < .95 AND press >= .5 THEN height = 1000 + (.95 - press) * 10000
IF press < .5 AND press > .2 THEN height = 5500 + (.5 - press) * 22000
IF press <= .2 THEN height = 12000 + (.2 - press) * 50000
RETURN

9700 REM Beschreibung
subgr = smass / au(tempzone)
IF subgr < .25 THEN subg$ = " tiny "
IF subgr >= .25 AND subgr < .5 THEN subg$ = " very small "
IF subgr >= .5 AND subgr < .75 THEN subg$ = " small "
IF subgr >= .75 AND subgr < 1.1 THEN subg$ = " "
IF subgr >= 1.1 AND subgr < 1.3 THEN subg$ = " big "
IF subgr >= 1.3 AND subgr < 1.7 THEN subg$ = " huge "
IF subgr >= 1.7 THEN subg$ = " gigantic "
IF siz = 0 OR atm = 0 THEN subg$ = " "
sonne$ = subg$ + son$
wolk$ = "a cloudless"
IF cloud >= 10 THEN wolk$ = "a nearly cloudless"
'IF cloud >= 20 THEN wolk$ = "a scantily cloudy"
IF cloud >= 30 THEN wolk$ = "a slightly cloudy"
IF cloud >= 40 THEN wolk$ = "a lightly clouded"
IF cloud >= 50 THEN wolk$ = "a half clouded"
IF cloud >= 60 THEN wolk$ = "a heavily clouded"
IF cloud >= 70 THEN wolk$ = "an heavily overcast"
IF cloud >= 80 THEN wolk$ = "a nearly completely overcast"
IF cloud >= 90 THEN wolk$ = "a completely overcast"
tx$ = tx$ + " " + wolk$ + ". "

tx$ = "A" + sonne$ + " Sun shines from " + wolk$ + " Sky. "

wet = ((weather + 5) / 14 * 10)
IF wet < 5 THEN wind$ = ""
IF wet > 4 AND wet < 7 THEN wind$ = "The wind is light."
IF wet > 6 AND wet < 9 THEN wind$ = "The wind is medium."
IF wet > 8 AND wet < 11 THEN wind$ = "The wind is strong."
IF wet > 10 AND wet < 13 THEN wind$ = "The wind is stormy."
IF wet > 12 THEN wind$ = "A strong gale is blowing."
tx$ = tx$ + wind$
IF siz = 0 OR atm = 0 GOTO 9711
bsx = bse: GOSUB 7100: tep0$ = tep$
tx$ = tx$ + " Right now it is " + tep$
bsx = bse - night: GOSUB 7100: tep1$ = tep$
bsx = bse + day: GOSUB 7100
IF tep$ = tep0$ AND tep$ = tep1$ THEN tx$ = tx$ + ". ": GOTO 9711
IF tep$ = tep0$ THEN tx$ = tx$ + ", the night was " + tep1$ + ". ": GOTO 9711
IF tep$ <> tep0$ THEN tx$ = tx$ + ", the night was " + tep1$ + ", during the day it will get " + tep$ + ". "
9711

IF height > 12000 THEN luft$ = "There is no atmossphere to be mentioned.": GOTO 9710
IF height > 6000 THEN luft$ = "The atmossphere is too thin to be breathed without assistance.": GOTO 9710
IF height > 1000 THEN luft$ = "The atmosspheric pressure is comparable to earth conditions in an altitude of" + STR$(INT(height / 100) / 10) + " km.": GOTO 9710
IF height > -250 THEN luft$ = "The atmosspheric pressure is comparable to earth norm.": GOTO 9710
IF height > -1000 THEN luft$ = "The atmosspheric pressure is comparable to earth conditions in a depth of" + STR$(INT(-height)) + " m.": GOTO 9710
IF height > -5000 THEN luft$ = "The atmosspheric pressure is very high.": GOTO 9710
luft$ = "The atmosspheric pressure is murderous."
9710 tx$ = tx$ + luft$ + " "
IF tt$ <> "" THEN tx$ = tx$ + tt$
regen$ = "The humidity is extremly high."
IF prec < 11 THEN regen$ = "The humidity is very high."
IF prec < 9 THEN regen$ = "The humidity is normal."
IF prec < 7 THEN regen$ = "The air is quite dry."
IF prec < 4 THEN regen$ = "The air is very dry."
IF prec < 3 THEN regen$ = "The air is as dry as in a desert."
IF prec < 1 THEN regen$ = "There is no humidity at all."
IF siz > 0 AND atm > 0 THEN tx$ = tx$ + regen$ + " "

grav$ = "The gravity is overpowering"
IF gav < 2 THEN grav$ = "The gravity ist uncomfortably high"
IF gav < 1.7 THEN grav$ = "The gravity ist rather high"
IF grav < 1.2 THEN grav$ = "The gravity approaches earth values"
IF grav < .8 THEN grav$ = "The gravity is slightly lower than earth standard"
IF grav < .6 THEN grav$ = "The gravity is about half of earth standard"
IF grav < .4 THEN grav$ = "The gravity is comparible to that of Mars"
IF grav < .2 THEN grav$ = "The gravity is comparable to that of the Moon"
grav$ = grav$ + ". A standard sized human (80kg) would weigh about" + STR$(CINT(80 * grav)) + " kg"
tx$ = tx$ + grav$ + "."
3101 IF RIGHT$(namx$, 1) = " " THEN namx$ = LEFT$(namx$, LEN(namx$) - 1): GOTO 3101

IF tdsc$ <> "" THEN tx$ = tx$ + " " + namx$ + " is a " + tdsc$ + " world."

RETURN
7100
tep$ = "unbearably hot (Vacc Suit mandatory)"
IF bsx < 80 THEN tep$ = "extremly hot"
IF bsx < 60 THEN tep$ = "very hot"
IF bsx < 45 THEN tep$ = "hot"
IF bsx < 30 THEN tep$ = "warm"
IF bsx < 20 THEN tep$ = "cool"
IF bsx < 10 THEN tep$ = "chilly"
IF bsx < 5 THEN tep$ = "cold"
IF bsx < -5 THEN tep$ = "very cold"
IF bsx < -15 THEN tep$ = "very, very cold"
IF bsx < -30 THEN tep$ = "extremly cold"
IF bsx < -50 THEN tep$ = "unbearably cold (Vacc Suit mandatory)"

RETURN
 
8000
x4 = 5: nz = 21
PRINT : PRINT " Available trade goods :":  cco = 0
FOR axa = 16 TO 1 STEP -1

IF cco > x4 GOTO 8001

value = agric: value$ = "Natural Agricultural Resources"
value$ = "Wood, Meat, Spices, Fruit": GOSUB 8100: IF cco > x4 GOTO 8001

value = ores: value$ = "Natural Ores"
value$ = "Copper, Tin, Silver, Aluminium": GOSUB 8100: IF cco > x4 GOTO 8001

value = radio: value$ = "Radioactives": GOSUB 8100: IF cco > x4 GOTO 8001

value = cryst: value$ = "Crystals, Gems": GOSUB 8100: IF cco > x4 GOTO 8001

value = agrop: value$ = "Processed Agroproducts"
value$ = "Liquor, Grain, Canned food": GOSUB 8100: IF cco > x4 GOTO 8001

value = metal: value$ = "Processed Metals":
value$ = "Steel, Special Alloys": GOSUB 8100: IF cco > x4 GOTO 8001

value = nonme: value$ = "Processed Non-Metals":
value$ = "Textiles, Polymers, Pharmaceuticals": GOSUB 8100: IF cco > x4 GOTO 8001

value = parts: value$ = "Manufactured parts":
value$ = "Tools, mechanical parts": GOSUB 8100: IF cco > x4 GOTO 8001

value = durab: value$ = "Manufactured Durables"
value$ = "Aircraft, ATV, Machine tools, Farm machinery": GOSUB 8100: IF cco > x4 GOTO 8001

value = consu: value$ = "Manufactured Consumables"
value$ = "Clothing, Computers, Entertainment": GOSUB 8100: IF cco > x4 GOTO 8001

value = weapo: value$ = "Weapons": GOSUB 8100: IF cco > x4 GOTO 8001

value = recor: value$ = "Recordings": GOSUB 8100: IF cco > x4 GOTO 8001

value = artfo: value$ = "Artforms": GOSUB 8100: IF cco > x4 GOTO 8001

value = softw: value$ = "Software": GOSUB 8100: IF cco > x4 GOTO 8001

value = docum: value$ = "Documents": GOSUB 8100: IF cco > x4 GOTO 8001

8001 NEXT
RETURN
8100
IF value <> axa THEN RETURN

value = value * (traffic / 8)
IF value < 2 THEN RETURN
IF value > 12 THEN value = 12
vx$ = val$(value) + " of " + value$
PRINT : PRINT " "; vx$; SPC(67 - LEN(vx$)); : PRINT USING "+## "; 10.5 - (nz - value * 1.5): cco = cco + 1
RETURN
10000 REM Druckausgabe
FOR i2 = 0 TO 10: text$(i2) = "": NEXT

tabu0 = 78: tabu = tabu0: tx = 0:
text$(0) = text$

10001 IF tabu > 1 AND MID$(text$(tx), tabu, 1) <> " " THEN tabu = tabu - 1: GOTO 10001
leng = LEN(text$(tx)): l1 = leng - tabu
text$(tx + 1) = RIGHT$(text$(tx), l1)
text$(tx) = LEFT$(text$(tx), tabu)
tx = tx + 1
tabu = tabu0

IF LEN(text$(tx)) >= tabu THEN GOTO 10001
FOR i2 = 0 TO tx: PRINT " "; text$(i2): NEXT
RETURN

12000 REM System - Plazierung des Planetenorbits

FOR z = 0 TO 19 STEP .5
orfa = orf(INT(z))
IF INT(z) <> z THEN orfa = (orfa + orf(INT(z) + 1)) / 2
temp = lumi * orfa * hz(hyd, atr) * ge - 273
IF temp > minte AND temp < maxte AND bse = 0 THEN tempzone = z: bse = CINT(temp): txx = temp
NEXT: RETURN

13000 REM Mio Ausgabe
IF value < 100 THEN PRINT USING "##"; value; : RETURN
IF value < 1000 THEN PRINT USING "###"; value; : RETURN

IF value < 10000 THEN PRINT USING "####,"; value; : RETURN

IF value < 100000 THEN PRINT USING "#####,"; value; : RETURN
IF value < 1000000! THEN PRINT USING "######,"; value; : RETURN
   IF value < 1E+09 THEN PRINT USING "###.## Mio"; value / 1000000!; : RETURN
IF value < 1E+12 THEN PRINT USING "###.## Mia"; value / 1E+09; : RETURN
IF value < 1E+15 THEN PRINT USING "###.## Bio"; value / 1E+12; : RETURN
PRINT USING "###,.## Bia"; value / 1E+15; : RETURN

14000            sens = 0
IF cr = 0 THEN extrality$ = "Single fence with open gate"
IF cr = 1 THEN extrality$ = "Single fence with gatehouse"
IF cr = 2 THEN extrality$ = "Single fence and clear buffer zone with guards"
IF cr = 3 THEN extrality$ = "Anti-intrusion fence, random checks"
IF cr = 4 THEN extrality$ = "Double fence, Sensors, 100% checks": sens = 1
IF cr = 5 THEN extrality$ = "Nonlethal traps, patrols, random search": sens = 1
IF cr = 6 THEN extrality$ = "Lethal traps, deadly force, 100% search": sens = 1
IF sens = 0 GOTO 54
IF gtec < 5 THEN extrality$ = extrality$ + ""
IF gtec = 5 THEN extrality$ = extrality$ + ", Searchlight"
IF gtec = 6 THEN extrality$ = extrality$ + ", Radar, metal detector"

IF gtec = 7 THEN extrality$ = extrality$ + ", Radar, LLTV"
IF gtec = 8 THEN extrality$ = extrality$ + ", PESA AESA, Chem"
IF gtec = 9 OR gtec = 10 THEN extrality$ = extrality$ + ", Radscanner"
IF gtec = 11 THEN extrality$ = extrality$ + ", Densitometer, Biosniffer"
IF gtec > 11 THEN extrality$ = extrality$ + ", Neural sensor"
54






IF traffic < 0 THEN tv = 100
IF traffic = 0 THEN tv = 1000
IF traffic = 1 THEN tv = 10000
IF traffic = 2 THEN tv = 50000
IF traffic = 3 THEN tv = 100000
IF traffic = 4 THEN tv = 500000
IF traffic = 5 THEN tv = 1000000
IF traffic = 6 THEN tv = 2500000
IF traffic = 7 THEN tv = 5000000
IF traffic = 8 THEN tv = 7500000
IF traffic = 9 THEN tv = 1E+07
IF traffic = 10 THEN tv = 5E+07
IF traffic = 11 THEN tv = 1E+08
IF traffic = 12 THEN tv = 5E+08
IF traffic = 13 THEN tv = 1E+09
IF traffic = 14 THEN tv = 2.5E+09
IF traffic > 14 THEN tv = 5E+09
IF tr = 0 THEN tr = tv
IF tv0 > 0 THEN tvz = tv0
IF tp0 > 0 THEN tpz = tp0
IF tvz > 0 THEN tr = tv: tv = tvz
IF tvz < 1000 THEN traffic = -1: GOTO 14001
IF tvz < 10000 THEN traffic = 0: GOTO 14001
IF tvz < 50000 THEN traffic = 1: GOTO 14001
IF tvz < 100000 THEN traffic = 2: GOTO 14001
IF tvz < 500000 THEN traffic = 3: GOTO 14001
IF tvz < 1000000! THEN traffic = 4: GOTO 14001
IF tvz < 2500000! THEN traffic = 5: GOTO 14001
IF tvz < 5000000! THEN traffic = 6: GOTO 14001
IF tvz < 7500000! THEN traffic = 7: GOTO 14001
IF tvz < 1E+07 THEN traffic = 8: GOTO 14001
IF tvz < 5E+07 THEN traffic = 9: GOTO 14001
IF tvz < 1E+08 THEN traffic = 10: GOTO 14001
IF tvz < 5E+08 THEN traffic = 11: GOTO 14001
IF tvz < 1E+09 THEN traffic = 12: GOTO 14001
IF tvz < 2.5E+09 THEN traffic = 13: GOTO 14001
traffic = 14
14001
IF traffic < 0 THEN traffic$ = "No Traffic": tf$ = "X"
IF traffic > -1 AND traffic < 3 THEN traffic$ = "Backwater": tf$ = "E"
IF traffic > 2 AND traffic < 6 THEN traffic$ = "low traffic volume": tf$ = "D"
IF traffic > 5 AND traffic < 9 THEN traffic$ = "average traffic volume": tf$ = "C"
IF traffic > 8 AND traffic < 11 THEN traffic$ = "high traffic volume": tf$ = "B"
IF traffic > 10 AND traffic < 14 THEN traffic$ = "very high traffic volume": tf$ = "A"
IF traffic > 13 THEN traffic$ = "extreme traffic volume": tf$ = "A+"

facility = wtn * 2

IF facility < 0 THEN facility$ = "None, (abandoned, converted or destroyed)": fc$ = "X": stp = 6
IF facility > -1 AND facility < 3 THEN facility$ = "inadequate ": fc$ = "E": stp = 5
IF facility > 2 AND facility < 6 THEN facility$ = "cramped but adequate": fc$ = "D": stp = 4
IF facility > 5 AND facility < 9 THEN facility$ = "average": fc$ = "C": stp = 3
IF facility > 8 AND facility < 12 THEN facility$ = "above average": fc$ = "B": stp = 2
IF facility > 11 AND facility < 15 THEN facility$ = "excellent": fc$ = "A": stp = 1
IF facility > 14 THEN facility$ = "outstanding": fc$ = "A+": stp = 0
IF facility = 0 THEN facility$ = "Does not actually meet all requirements for starport class"
IF facility = 1 THEN facility$ = "Inadequate facilities in at least one respect."
IF facility = 2 THEN facility$ = "Bare minimum to meet all requirements for Starport class"
IF facility = 3 THEN facility$ = "Cramped, low capacitiy facilities"
IF facility = 4 THEN facility$ = "Substandard facilities or services"
IF facility = 5 THEN facility$ = "Small, crowded, expect delays for all facilities and services"
IF facility = 6 THEN facility$ = "Small but not overcrowded"
IF facility = 7 THEN facility$ = "About average"
IF facility = 8 THEN facility$ = "Better than average facilities or services"
IF facility = 9 THEN facility$ = "Spacious facilities and efficient, skilled services"
IF facility = 10 THEN facility$ = "Capable of almost anything"
IF facility = 11 THEN facility$ = "Large, cavernous facilities with plenty room for expansion"
IF facility = 12 THEN facility$ = "Excellent facilities, can meet any requirement within class."
IF facility > 12 THEN facility$ = "Outstanding, large, efficient.": IF stp$ <> "A" THEN facility$ = facility$ + " Upgrade imminent."
'tv = tv + tv * ((facility - 7) / 15)


IF traffic > 10 THEN trader$ = "bulk carrier (50kt)": cargo = 25000: GOTO 55
IF traffic > 9 THEN trader$ = "Bulk freighter (20kt)": cargo = 13042: GOTO 55
IF traffic > 8 THEN trader$ = "Type AH (5000t)": cargo = 2827: GOTO 55
IF traffic > 7 THEN trader$ = "Type TF (3000t)": cargo = 1312.5: GOTO 55
IF traffic > 6 THEN trader$ = "Type TI (2000t)": cargo = 605.5: GOTO 55
IF traffic > 5 THEN trader$ = "Type OF (1000t)": cargo = 301.5: GOTO 55
IF traffic > 3 THEN trader$ = "Type R (400t)": cargo = 146: GOTO 55
trader$ = "Type A1 (200t)": cargo = 82

55

tx = tv / 365 / cargo: tx$ = "day"
IF tx < 0 THEN tx = 0

IF tx > 3000 THEN tx = tx / 24 / 60: tx$ = "minute": RETURN
IF tx > 50 THEN tx = tx / 24: tx$ = "hour": RETURN
IF tx < .03 THEN tx = tx * 365: tx$ = "year": RETURN
IF tx < .3 THEN tx = tx * 30: tx$ = "month": RETURN
IF tx < 1 THEN tx = tx * 7: tx$ = "week": RETURN
RETURN
15000 'RETURN
FOR t = 1 TO 6
IF loy <> lox(t) GOTO 15001
rtx$ = "*"
stz$ = MID$(upp$, 20, 1)
IF stz$ = "A" OR stz$ = "B" THEN tfm = tfm + 2: rtx$ = "+2"
IF stz$ = "C" OR stz$ = "D" OR stz$ = "E" THEN tfm = tfm + 1: rtx$ = "+1"
15001 NEXT

RETURN
17000 lox(0) = lox
IF INT(horiz / 2) = horiz / 2 GOTO 17001
lox(1) = lox - 1
lox(2) = lox + 1
lox(3) = lox - 101
lox(4) = lox - 100
lox(5) = lox + 99
lox(6) = lox + 100
RETURN
17001
lox(1) = lox - 1
lox(2) = lox + 1
lox(3) = lox - 100
lox(4) = lox - 99
lox(5) = lox + 100
lox(6) = lox + 101
RETURN
16000
vl = INT(vl)
IF vl < 0 THEN vl$ = "X"
IF vl = 0 THEN vl$ = "0"
IF vl = 1 THEN vl$ = "1"
IF vl = 2 THEN vl$ = "2"
IF vl = 3 THEN vl$ = "3"
IF vl = 4 THEN vl$ = "4"
IF vl = 5 THEN vl$ = "5"
IF vl = 6 THEN vl$ = "6"
IF vl = 7 THEN vl$ = "7"
IF vl = 8 THEN vl$ = "8"
IF vl = 9 THEN vl$ = "9"
IF vl = 10 THEN vl$ = "A"
IF vl = 11 THEN vl$ = "B"
IF vl = 12 THEN vl$ = "C"
IF vl = 13 THEN vl$ = "D"
IF vl = 14 THEN vl$ = "E"
IF vl = 15 THEN vl$ = "F"
IF vl > 15 THEN vl$ = "G"
RETURN
19000
IF traffic < 1 AND facility < 1 THEN PRINT " ? "; : RETURN
PRINT stp$;
vl = traffic: GOSUB 16000: PRINT vl$;
vl = facility: GOSUB 16000: PRINT vl$
RETURN
20000 wwx = btn + wt0
    
   
kg = btn: IF wt0 < kg THEN kg = wt0
   

IF agr0 = 1 AND exx = 1 THEN wwx = wwx + .5
IF agr0 = 1 AND nag = 1 THEN wwx = wwx + .5

IF agr = 1 AND exx0 = 1 THEN wwx = wwx + .5
IF agr = 1 AND nag0 = 1 THEN wwx = wwx + .5


IF ind0 = 1 AND nin = 1 THEN wwx = wwx + .5
IF nin0 = 1 AND ind = 1 THEN wwx = wwx + .5

IF pol$ <> po0$ THEN wwx = wwx - .5

hz = ABS(l1 - horiz): vz = ABS(l2 - vertik)
entf = INT(SQR(hz ^ 2 + vz ^ 2))

dmo = 0
IF entf > 1 THEN dmo = .5
IF entf > 2 THEN dmo = 1
IF entf > 5 THEN dmo = 1.5
IF entf > 9 THEN dmo = 2
IF entf > 19 THEN dmo = 2.5
IF entf > 29 THEN dmo = 3

wwx = wwx - dmo
IF wwx > kg + 5 THEN wwx = kg + 5

tvx = 0
IF wwx > 3.5 THEN tvx = 5
IF wwx > 4 THEN tvx = 10
IF wwx > 4.5 THEN tvx = 50
IF wwx > 5 THEN tvx = 100
IF wwx > 5.5 THEN tvx = 500
IF wwx > 6 THEN tvx = 1000
IF wwx > 6.5 THEN tvx = 5000
IF wwx > 7 THEN tvx = 10000
IF wwx > 7.5 THEN tvx = 50000
IF wwx > 8 THEN tvx = 100000
IF wwx > 8.5 THEN tvx = 500000
IF wwx > 9 THEN tvx = 1000000!
IF wwx > 9.5 THEN tvx = 5000000!
IF wwx > 10 THEN tvx = 1E+07
IF wwx > 10.5 THEN tvx = 5E+07
IF wwx > 11 THEN tvx = 1E+08
IF wwx > 11.5 THEN tvx = 5E+08
tvx = tvx * .6

wwp = wwx + capital
IF ric0 = 1 OR ric = 1 THEN wwp = wwp + .5

tvp = 0
IF wwp > 5 THEN tvp = 5
IF wwp > 5.5 THEN tvp = 10
IF wwp > 6 THEN tvp = 50
IF wwp > 6.5 THEN tvp = 100
IF wwp > 7 THEN tvp = 500
IF wwp > 7.5 THEN tvp = 1000
IF wwp > 8 THEN tvp = 5000
IF wwp > 8.5 THEN tvp = 10000
IF wwp > 9 THEN tvp = 50000!
IF wwp > 9.5 THEN tvp = 100000!
IF wwp > 10 THEN tvp = 500000
IF wwp > 10.5 THEN tvp = 1000000!
IF wwp > 11 THEN tvp = 5000000!
IF wwp > 11.5 THEN tvp = 1E+07
tvp = tvp * .6

IF entf < 1 THEN tvx = 0: tvp = 0
tvq1 = tvz / 1000000!
tvq = tvx / 1000000!

'LOCATE 24, 48: PRINT USING "####,.##"; tvq1; tvq; : PRINT namx$;

IF a$ = "x" THEN RETURN
IF entf = 0 THEN wwx$ = "!": tvx = 0: RETURN
wwx$ = " ":
IF wwx >= 8 + wq - capital THEN wwx$ = "°"
IF wwx >= 9 + wq - capital THEN wwx$ = "²"
IF wwx >= 10 + wq - capital THEN wwx$ = "Û"
RETURN

30000 REM
tv0 = 0: tvz = 0
tp0 = 0: tpz = 0
OPEN sector$ FOR INPUT AS #1
INPUT #1, planets
FOR I = 1 TO planets
INPUT #1, upp$
l1 = VAL(MID$(upp$, 15, 2)): l2 = VAL(MID$(upp$, 17, 2))
othw = 1: GOSUB 2001: GOSUB 9100: GOSUB 20000
IF entf = 0 GOTO 30001
tvz = tvz + tvx: tpz = tpz + tvp
prz = I / planets * 99

tvq = tvz / 1000000!
'vq = tvx / 1000
LOCATE 24, 68: PRINT USING "####,.## ##%"; tvq; prz;

IF wwx >= 10 + wq - capital THEN major = major + 1: LOCATE 24, 2: PRINT LEFT$(upp$, 18); entf; : GOTO 30001
IF wwx >= 9 + wq - capital THEN feeder = feeder + 1: LOCATE 24, 24: PRINT LEFT$(upp$, 18); entf; : GOTO 30001
IF wwx >= 8 + wq - capital THEN minor = minor + 1: LOCATE 24, 48: PRINT LEFT$(upp$, 18); entf; :
30001 NEXT: tv0 = tvz: tp0 = tpz
'PLAY musik$

RETURN

