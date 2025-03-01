#!/usr/bin/perl -w

use strict;

use FileHandle;
use Getopt::Long;

my @check_files = ( "semant.cc", "Semant.java" );
my $grading_dir = "./grading";
my $grading_cmd = "./143publicgrading PA4";
my $just_unpack;
my $just_run;
my $verbose;
my $skip_check;

sub usage {
    print "Usage: $0 [options]\n";
    print "    Options: -dir <path>  - specifies where to unpack/run\n";
    print "                            grading scripts [default = \"$grading_dir\"]\n";
    print "             -cmd <path>  - specifies what command to run for grading\n";
    print "                            [default = \"$grading_cmd\"]\n";
    print "             -x           - unpack script and test cases, but do not run\n";
    print "             -r           - don't unpack, just run existing script\n";
    print "             -v           - enable verbose output\n";
    return "\n";
}

die usage()
    unless(GetOptions("dir=s" => \$grading_dir,
		      "cmd=s" => \$grading_cmd,
		      "x" => \$just_unpack,
		      "r" => \$just_run,
		      "v" => \$verbose,
		      "skip" => \$skip_check,
		      "check=s" => \@check_files,
		      "help" => sub { usage(); exit 0; }));

unless($skip_check) {
    print "Checking that you appear to be in the assignment directory...\n"
	if($verbose);
    my(@found) = grep({ -r $_ } @check_files);
    unless(@found) {
	die "$0: could not find any of the following files - are you running\n  this from the assignment directory?\n    " . join("\n    ", @check_files) . "\n";
    }
}

print "Creating grading directory '$grading_dir' if necessary...\n"
    if($verbose);
unless((-d $grading_dir) or
       mkdir($grading_dir)) {
    die "$0: '$grading_dir' doesn't appear to be a directory and can't be created: $!";
}

print "Changing to grading directory '$grading_dir'...\n"
    if($verbose);
unless(chdir($grading_dir)) {
    die "$0: can't change directory to '$grading_dir'...\n";
}

unless($just_run) {
    print "Unpacking grading script and test cases...\n"
	if($verbose);

    my $tar_options = $verbose ? "-zxvf" : "-zxf";
    my $fh = new FileHandle("| tar $tar_options -") ||
	die "$0: couldn't run uudecode or tar: $!\n";

    # skip the first line
    my $dummy = <DATA>;
    die unless($dummy =~ /^begin /);
    binmode($fh);
    while(defined(my $line = <DATA>)) {
	$line =~ s/\s+$//;
	last if($line =~ /^end$/);
	my $unpacked = unpack('u', $line);	
	next unless(defined($unpacked));
	print $fh $unpacked;
    }
    $fh->close;
}

unless($just_unpack) {
    print "Running command: $grading_cmd\n"
	if($verbose);

    system($grading_cmd);
}


__DATA__
begin 644 /dev/stdout
M'XL(`.75P4\``^V]VWO;R)$X.MFS#RN>EWT\CVU2OYCT2"#!JTV-)B/9GL3?
M-S.>SW9^R>[8ZX`D*"(F`08@+2D>Y=L__73U#7T#0=DB)'M0R5@$T)?J2U57
M5U=5.\VO=@XM#(->#_ZZ@UY+_LOA*]<=]%N=7GO0P>_QPZ#S%>KM'K6OOEHG
M*R]&Z*MQM%@&<S].,M+E??],P6E&[_TX#B9^[(?>PO?B,V?E)ZN;K`,&N-]J
M98X_'FTZ_JV>VW+[>/S[G3X>_]9-(I$%O_'Q'\^])$$_>D&(/J`%_E-OH"%Z
M%J[0AQ:Z.L+_KU1HFB?H0V4O"-]=UD=#_+TQ?#[ZNS_&Z9`+J=*$3U$0SOPX
M6,EYQO8\M]W\WSP(^@_"LX6_FD430OY.M+XY%I!'_\#S5?H?M#K=DOZ+@)K;
MKKQ=QM%9["TJ"-5Z^)^WA([Q#X1.R+^4:LG/JG6Z5,FW.OFWUB%_WM+/Y#="
MTRABOS`;8+]80IPT$.\0ZK-?0Y&R`7BY+16Q4PF]+7!ZM"U.CVPX#3)P:JLX
M`1?]B/XBQ>C(+7A9"G8LZ19=MNWX.\UD'"W]Y,87?0GRZ+_=UM?_KMMKE_1?
M!*3K_P>Z^L,Z_:%U=92NYR=X$;^`U_C-M'XQ/&D,7ZYB/*'1AZHWJN*$9_CM
M:13-&T.<-O'G4_QN5N>K_8>YOT(7+,\WA],Z?88,6%)`.'.C`3\NOCEDY95R
M06'@-&-_XD^#T)^0T=X%'\B7_SL:_??Z';>D_R)@>_D?V``2='Q4(0MHG3`&
MR,(E^_,9[B6TBM<^FD?1$EV@)29T98,`)>&GVVYZ"5\!_<^\(+Y,QEY\Z8SG
M.Q$#<NB_W>H;]-\MZ;\8J#]`9`(@Y,#_'C0XD7X?1>D^_M3[YS\)^6/P,+&_
M@.=O#M'82WP$"SZ*II6]/2J&ACS!\;>H'OKG.'?<.%(^0^'\*R35/N,,\#D$
M)H/VN)B+_,0;8R9"?H\8E\)(>,XDPM(&^AJ=B5_\[Q+SJA7^R;/1]XS!(2*)
M!&E),T38X(S^_AJUCU"`62!J$#:8,C#`3_3-"]HWM/QQ6ABI2M2;<DC\*<4*
MRF2%DG)$J=!#O%"?=8FMPS=TMMJ3>WMR]TVE[OL.1M?2A_ZFWI0[0\;[V7.!
M]BRMP^7UGK'!SYH\>Q1IJ3TX=3H[]C*;N\>GE3FO]O2.4&=2D#$T]"M_%G/F
M`XK6*]A^U6>-(]2"Z7&U:79E3BY7GEQ0!"9%PH8I)2),BO+B7*&4=\HHC[;]
MG_\$$AFQIK/7^">\'4MTROL#WD_2V<0ZD*#/I'_$17M4G40HC%8S_%05\Q_@
MMEE6"3<(3I.3[@B+:3>O^P/(E?_Q9E_3_[O]<O]?"!BC/WPT1(\I8\<<.L0<
M@#-WQ/@]I*P\)MWAK8(HQ`+$?.5/T`2+_*L(X?Y<!6/,V!=>"#_\.([BQ"G9
MQIT$ISGR)OX_UMX\6%W"^+=O?@N02_]=7?_?=SO]DOZ+@!O?_U^@8[+[9T+-
M;;>OA,W@-,<S/+-W6@<YX]ND_^^YG/X[F%-@^L=_>B7]%P&U>\UU$C='0=A<
M^O&\4JM4J`:OO@^[@V/TS<M73Y[]]&V#;?^#*:K33_X_4/5U6&V@W_\>U9D*
MN;X?S2<-^@I^0B)(W&B0S+2(*^3/\<[O0V6/;*Y0%5)4C^@W\B_YA^0_)MDQ
M*RD9R6Z`K/\>ENYFNY']`?+7?T/_U^X,2OHO`I31'W:'>,L?'L+R[\5GZX4?
MKA(J#7S-%`.EX/]E@=-,@L5R[N^2!>30O]MUC?T_%@I*^B\":AW%_*>SA46+
M/F$48Y8<6Q:I')$8)Q_-H_$[8<\BWN,OR_DZ$4_*)\T(!J`G/0TENYG\G-W,
MG.J3C%JR'MU1S!;K^=;E=SX:L_9'8#8)W@<3_\X@]S$64U\6@/XW6)UC6H:3
MF-O1_W8'KJ'_&93ZGT*@UE?X?S^#_S][3GF_,5ELS-];K6)&8!?L[X_I$I!R
M_4A>#!`Y"A1T^?+I#]^_??5?/S^E)?>NLZQP9JLO*RDKU:M6*C>K-]]\,>S"
M:<+$QHMY$H6[,@+-W__UC/V?6\I_A8"LVR4*70_L.XEMY@?TH8^^08,CA/\<
MPU\X#`+=[M$5/Q`VU,=(.@#O<P7R;3>RA$S`^[_U:'6Y]"EKC?W5.@YOF`_D
MT7^GK=/_H.66]E^%@$+_A`$`_?)G#,18F[QB5B@<+K@%R<F1_-8A98A75*%[
M!<^RS5!J,'3"[84L=8M%6<^?PW!NNU,_(W":TR@^]^()'Y`=R`#Y]A^Z_K??
MZI?R?R'`Z3\U]:3TB*F1NGQH=$N)=>3%_"O:6@RX[::68`&GB1G\R)O`QFY7
M)T"Y_E^=ON'_T2K]OPL!;?3A!.A9./7CV)\@$`LQ:XBF"+X'WCSX)SWWP6\N
MT"3R$[(C&$<A7D,6<`043/QP%4P#/[Z?H(F/V8(HY[2T&;N+8+'_O/$Z<M?_
M?E>G_TZO//\M!*YK_T5MOIC"K2'KQ83GIUW.IRJ%TB;LCH'3I!O_L3>?)WB<
M=G$(D$/_G;X9_P%<0DOZ+P!J`T7_[VYQ_FN?,&I(@\WJ>CFB`=?5RP=X+<OQ
M'$X[4)$S8U-L@5G7@MG41(L?%;R=!,G26XUG-W**D%;%\>%M4UMK/>XX,XXW
M0.KRTI->3_Q*&R(UJ9?=I-ZG-.G,TB2Y1*,ZY;.M2JU2>[5RQ7+5>NF6ZK4D
M=A0,)++0D`=51R4=7)I?/<%N5.Q?+!.B;YD0O$DU'O]CFPF1F7AD26S+9LZ>
MOOCY$;,G+:=N+=$V>_K2PT?/'KG$>F;INEE!2RE7,TCXZ)Q9$^$.=82=C/K*
MXRV1T6>#Z/;T7MSQLM.$9L,.'>*`T`W[3>\"<_=_'4/_VVNW2OFO"-A^_R=M
M]"J6?:"(['?!MX"WW;02M@!B_T_5;YQS%KW_:[4[NOYG`"Z!)?T7`/;1'[KN
M$#V]6,9^DH"VEFAP4P9@4_T*;2_3Y?+R:-['I?;W;H+35`9^)W7DVG]T=/O/
M;K=7VG\4`FJ($1X$A"_FW3;(`!5="<PMQ;"8<(!&0W1Z@,;#DP,T&9YN:\%A
MLP!9V*U/1L-3$:P$"1T20:&+JZ?_;[!:6"6E]+$E.$VPUMWM`7"^_:=._[U!
MOXS_6PCHHS]\.$1_#L5BGA[H`G66B_@7!TZ3\&,6`'@G[K^Y\?\&+=W_K^>V
MR_6_$*BYCY0#H-PS%GFZV,)^2\X?7/^<%_+;/.[AQS1281.SL*ZM,-<LS'::
M8CGS$0<DN('!F9`S)#6Z?*RA.R;F'6EXTI.AP<S+/,G,G.7-!D=KFC.G$C(]
M<QC=EM'U([.O7#YF2M^W#41JKNTHT-+YKC@(U)KO6<JT.9F>6<H4GD;9(^IF
M.W2ZBH/A=8<T-_<H,_>F,7VHCNGC[<:T;XSIV-)=XHQ''M..@4C-'5CZ?V8I
MD,?-W]3_`_';Z/]!3@]N[O^\W./,W)OZ_Y':_]8#\NQQL%U#D'$N;KV(X%,B
M_=LAM?]*2'R'78@!^?;?NOUGO]<MU_]"P#+ZN1$`:220<C/P)0"Q_]Z=ZS>!
M?/M/W?ZKY[JE_T<A((_^L#-$]^'I/J?[D8]&T3J<@.^7A^[CQ/>1+XX%2@[P
M^0->_Z-=WOT#D*__U\__.^WR_K]B0-+_2S'</U00N_$'7:8!/_73@0]($:@)
M0'!T*DO4JT3^;1S9$T$$]9[MXR4H^X.0EV)+<L&20!F6[U<T]NAM=^UG`4X3
MPO!3->_8FX]B+[SQH\#\]=^(_]5OE_$?"H&/\__08OZ2FQPNP"V,G`G"10O>
M$1KQWZ,C<N%"&1'X#@*Q_R$17T?1Y/*VXO_K]C]]M[3_*P:,T1_VANB)L-V)
M$);^:6Q?1+56:.Z'9ZM9*?M_&:#%_]U)';GTW];W_]UNNSS_+P2NM_[3N[R5
M:[R_1M69/Y]'U?)"[\\1S/N_N[=@_ZO+_P/7+>/_%P+VT1^Z;6`"2-P-RQ=_
MH/\#M/1B#[_P8V;<BX($38+IU(_Q-A)-XVB!HC@X"T)OSH(_E-+"'06G^?-)
M]W`:X!&)=U4'T/BF^S]<@_X[O6ZI_RL$:O?(W1_)K%+Y\;_>_GSRZD_'U;]-
M@CC$!(Y>5_=;KZM_JU8J^^PCFRUQLJ)3!OV*E$^+Y(Q^*/?YGP<XS=`_YRZ`
MMQ7_M3<PXK^VR_6_$$AE>U"CL[ANH%X'11^8W`NO'W8EJH@12X.[_6MPK5"P
MM]W<$C1PFN/+\=P/0BK@[<0,.(?^W<'`T/_W6N7^OQ#XB/@OE@ES%X*_Y*&U
M*?)+:DI\O9@JNPH38XVI(I6H!I=H267EQ!'YG`++6$.#]&Z@$VXLF$H!\5%L
M`8/D$C^V$[C=J-.<>4%\22X>=\:[N0!Z,_]WN^;];_UV:?]9#-3Z;74!Z*I\
M]OLH(G_AKGG*9\WYHK#9AY2X+&X@+W@1(A5.A[<=<'I\0WQ39@_4E$'*'4J_
M)5RT?#@G%GL5FY)3+Y:>A\JS3(L;:^0=:62S5*AA-U1?U`9;5JGB7<LSB`\S
M&BE5?VWO$,5+)GUO\9]QY0!)^E?]>U:<'S-=5K`?U7M`:V8*DTBOH*X]-Y1G
MW0UA%XB?&8BK<^LF\#;=*93AL0=9TEOZ:5&6]"9L&V=)"Z$ESSE;#"VU81\;
M1(O<9)L52<L>]RG/H2EMO]6E:>ZG:`86V43S;3(:-LOL,"F7>H&2X=6DN?6H
MQ9KI;71MILF>^/J4-HG-6IH>B0U`3V06E4\1>0Y>06;N#0Y&;6T'QMGPBVW7
M7]=<@&V^7F(!-N55\>VCEF"9;#?&N&R;2PF?^MHMD:UL7-NM3\%5H]I-Z)+!
MZ6@>E6)0.`/.&YSV(Z/-7/1)UUN>RB8=M3]I,U%K;RLZZ/)16Q$>KB4@M1]N
M6:<FKDCYKBNO\-^UCFOTM\7WM"/T$*JX(MY;Q!7IFU5<4;YG,#XM#7A`DK/0
M+!'!EB6;6YH23H:,(PGW,I@BA"E$Z&)$!D\V&WI#+=1%(;LP='-ML8AW^D!G
MB7=F'V2TRC>P4`D#X,;%.W4ZV\4[O:5W0KR3:=0FWJD-NQ7QCBP=/6-=9R3'
M;];,63DZ`X.36<3#CO"XS@F)T#4#+'!:2NF'I[(M1)U':;]_Q$+4E=;MG$5!
M78BZBKBN+T0J[:N\H-9UMZQ37_RZ[J8Z-RY^W?:6=6KJ@6Y[4Y4Y^H%N9^O.
M55;<KGS=\O567(Y^K6N&&N!RL"K5=3=H3+N?).5<0ZJK=9FTK.R\E`)D;;\0
MFO6+9241Q\:$NGFBS%9,B'E-97"A_$IF2@W9H8EMY"HU<!N-LWSBT++TL'5O
MVQ-R_A9[V]XFP3]S;ROG,O:V/86UY.]M>YJZPR;B];95ZFRUMS5+L^UM=4'C
M(_:VO3RER$?M;?O`U/).%W/6P5[/X"]<S)66"IY,XYU2BF'Z4.N9/(LK-:7%
ML->W%9DFD)A@SURL^1Y<5H+W!K827\A(IIK7GKFOGXB&BZUC[Z&]W9:]4=]&
MF-(Y+77[Y`7W!7$F\GL\8),(0L'.\+NJJ$3*^\7<E/UE`O/_.$O<V_/_`&=O
MS?^CTRW]/PL!N_\']^]H63Q`"7OT8B5=90_>$GNQTP:Y`1@DF@/4?L0%"^;\
M:0W^2E-`KM'P]``)]U+*13!>U47$_4O*R*XW"V#_N9.@3Q+DV7^9\5^[[4YY
M_E\(:/9?FI65D-"XEH)/%E6K;4@F%UPRB2)N05,3)U=A]!9BR`AA`5Z`]3&3
M<S9+)>HF4DAY^H8L%?,QQLHN219L=>E95G)OU#IM6;Y=C]95'C_U+AI-V_DQ
M:K0<TZBT"IN]D]R'7:/`4OJ[X^`TZ49WX8>K74F`N?%_VCW=_K]5VO\7`QOB
M_R`>``B)"$!3LLG=,A00B]/3.:)_>T<TM`\-"W2$ICRH/RU1`XCA0T3"1BH'
MTIQ4#*1XXZR(QRJB=56N*J67P37`:>[X\I^O\N-_MWNZ_W^_.RCEOT(`K#UD
M`5`S_[2JZ"PS)D<>Y.JT4U,8E-54J9**%M2YCBPH0HMG':+7I&,10^A*(P\/
M!1[<RP!`EK/JUB)58;(GE:<>6%X;B6*R9)B\N-JYI>EWD3<;;#&/T]X45JE;
M75Z:E3AMTFE>TC3L\TE>THE1JH2*4+3*X][+Z$0M:K<2B3VO_ZS1MJ4.=*_C
MMI&=VM*%F6DM?9B9=F,GVD..WWR4ZQ*RP&D&NPS]2R!O_0=EKZ;_:?5+_6\A
M<$W]3V"Y4CU;_2-1^BUI?\91*-^=H:IG_'](3Z9BQG:>V]5>*$?5'"Z,-^99
MKJ6LC.-<69;@8#L=MI=H:41&TDUV=K8V96&15;B];>D%$_DEV]Y:S\F-4;+5
MK=>[E;6?-D7,<A]M+&$H:T2-`HWB.DI6[8S^+FL:7ZIGQ$.5<)7'')6C=@K-
M*5X_A$Z94GG\?"T@Y[^[W/Q_M4W\#_/^OW89_[\0T$>?1/[:&/^SO`?P2P*G
M*6*\T>5A!X<`N?8?+>/^CT&_U/\5`MO'_Q2V'E@83.\$L`<$)R&%T2K&+&$>
M14MT@998[E%L0,#PHPP(=.N0WO_U/"7_@N/_M+KZ^5^_VQF4]%\$@'I15@!H
M]]N9ZE[+=,FY!M1J/R"T]9OU`3;=ISV6T-::1*ZL-/2QUVFE:S9S)(QL907G
M=NVD6&4I7ZZ%V/8]YEJ=A4KEZV\*G.8\P`Q_1Y%_*.3$_^GTS/O?6R7_+P8P
MAU(/@'N:C_L/>'H87$B>,U8'=X7]!,E/`5=Z*1:!PL]])&O%4G=%D;C6L2F&
M9[YGN16ZDZ46[DC!@:RNHGFW,&_EI>6-HCC+1TOVLGJNEJ]@=TT7JZ[-DV/E
MB3X70RC[3QI*\YPX*=T;B9/RL;W3_80X#_H;ZHNK]]<X"A/126)B:H>9-F<T
MN7=[&T)5R+YGJD/GX[1JP%9Y#,(@;6K=7IK1(8%4VO9><[F.E:F)@&@S$5P>
MM526(1H@DF4SC(%Y>C2V"5*#+0^0!EVSO`G_)0_5H+ME@3;]=!9'&P@I5.%H
M:4C+E*,-;)ZG=HXV$+8&VIB-);\NP0T>7H,;/&QEE3Q)2Q89:@]M.$LSM/90
MC-(6A).=.O:3%!4)7P5U(;UJC.RA=/QG.'&.58]G*>VG^#E"2;U-M4[46O,6
M&J7]RA!8'FL//\%;6G]#-R(=S5[%.`;.)&>W9=+SXG)N'T*>.)<&7=>V_!,W
M[;=SI<`L4Y[TEX2`>IKLNH(C**?&N-0<]VXY@7U`Y2,_;?A49I)V)05Y352.
M#F\"JZV]SK73-LWQ7*O(2(SGR^NPJE:DI\GQ/W==R:#-#$JV57"ZFXU.9SKC
MZR>B9HV9D?^V#OTWU]X8<PE`6D3LF.4'+6SD3@N,]!8A8]1$-]+IENEE]+M6
MJ24+GI.HJM=I)MNJ']K;]$/[)OO!X'P4C'[0*LV>?]M&X-MJ_DFRAATS<_X9
MI:@=K^T&-IH3N!V;P)9I0^1F;A==.7"9L:0KBYJ1/GLB*(DVC8F6,#NI-?&F
MY!D9-F?)S&3L9F2PS`X*UHE#82QO?V0P`W9M1BO+X,B,D,%QLILSF8'!:.J,
M%F3@;\,^:QRR,-?)E.)AQ]K$.0-C*[XFMO9)9L?4-.FR8VD&7+-@:,%/Q\Y&
M+3;,=+LP&U:YK,G`1V>[.HF;F/2T.C8OOQN%?[<C;7?@K%5E2;(YVEM08>JX
MJO9J&]C1([V#,Y8(@S%:L*:@R[T`>0N%9CYGVM.Y'26(E2XKDA1;1NS3$V:W
MV5BXLY?NS,7;-JNW1F#[3M<[]+E9(*Y7$QPLL9`RZC3R;EZWC,2;C%"M%6:R
M-8L4LEW,1Z,TXY5!DMGBR)T_.W.:D_62;I:3V_+_=;OZ_=^]5J>\_[L0D.U_
M8,:J)D#N%?',)38^]-T!NF!<E_ZBFR;9]H?X]+(':O%SVVTL(1N<YMQ?C;P)
M:'%OB_[;;2/^RZ!?GO\6`HK]G[19MH1W0GBF4.KGGOLG"&<3RJNK(Z3:]PDW
M?37>DWC].'U]2EY7RNA.18/3A(GMQ;NZ^Q/@^NM_M],J_?\*@93^/["8'GB1
MA[AO690LD^^'BC<\.:J,AJ>0F,XC7`)(!_#M>%0A`L!MM[&$;,#T[R5^,/'!
M6<.;TW#1-VP+EDO__;Y&_X-NI[S_LQ#(&OUA=XB>K)=S_'KE(_J![`GPB@]Y
MB)^/3Z)&E<Y`GS%(_C^$J]^"_;_%_Z_?[71*^B\"+*,_?#1$C]G*?^XE:!G[
M[X-HG<PO$4M;DOP7`TX3E-=@:H.%-V_AK_QX&24!#.R-L8)\_S\M_G.[-1B4
M^_]"8//H@Q3P/5$.(Y$`76`!(`RC%2;\]YCD<>;TK*/D#)\9.,WH/1Z(8!*$
M9]1BXN85`=>7__ME_.>"('7&Y8I^H@%`'U"?^/YF!FW7D@_4".^20S%*CQ-(
MF:@\$;A#X#1_/ND>+I*S:8!Y=;R3.H#&!YOB_[H#2O^=]J#?;I/X[X/R_*\0
MJ-UKCH(0"X$3=#BM),W_^>7U"KUY4/NE=?CH#?GG0;.)WU=_^9_JFP>OJ<5W
M$T\6/\3B`'QQ'/9Z^+HNY7K=&#H/1,+A'.\;(#46(Q*?+OW-Y#)<>1?LX0PJ
MEX0'G/G:HD2S9"S7!=C_K]9Q"&)<&/D78-A_TP)`[OIOQ/_H=TOYOQC0S__3
MP[^?@_#=T[F_G'DA/]577I5G^U\".,T=!__Z*C_^9VM@^'^WR_B?Q8`>_UOS
M_K;&H,B-_+V=Z;UP_E*N$&D+,SHU9(?F8VH&)LG&RN:U*`50%CZ;V\1/SDIL
M"9^<E=02/3DKJ1D\663)\@F5;%'MSGV;8E%?,PKU0G1B&@=:>.#)D5'-WN%1
M6%ABU84@3:0$-7<E1T>KQUM?>OIHAS<USF4]NP+=KKNK%*U%Z\S%;:3D/KT#
M>66?/#Z^_->=MVLM83O(DO]O4A#(D_\[K8%^_M]QR_B?A8!]](?M(?JS"/Q)
MTU!-O[(%P)L&'A04+_).);NP^YC!WT=K4#.<!ZN9%%64;D#D8LLSA`+!:2;!
M8CGWP:9C5SN`O/A_?3/^9[]5WO]5"-0>*>*_=MN+_?H?=<*H8N+F#8`<?<XF
M_:>!4A3A/S<HX2:4S(`PJ=BJ;DBVC`ACVTZ,,HH44KH>I"259''!@+@LM.7%
M!UDH(JK%B4LM@5IO*24$RI,1YEYU4+2[9*E%&`)O/P>#D?*DN?%I^;-0&&DH
MZ,Z`@QP<5#].B_^_4D*FV[Z&Q36"W]^^&&W<_U@\_P=K#X/_=\OSWT(`(I1N
MB/]J1/[19LO.+H(QXZM>5BQDNFT<67,1X(J`[U-%T+81P3:O<>H"D!6E2E)E
M&-ZU\ETKM8TJA^P+0K3P5#=37>\FJKO,K,X6.4B^V(-69+!7[<7&RJ>9E>M!
M++[7UECY67Y*?^?H&=5Y*P2#:]QD@E@\88D^>=VR,+2)1%EHG!P:=:W"64=@
M548&_F+`:2X"/%_"LYU9?V]C_]71SW_=\ORG&#!&?]A+K;^%P=<TCA;("PVU
MS9-25_.90WK_`[<$OGD>\!'^7ZW2_[L8L([^T.4\@!B&,'-OEI21?FGR_44`
M7O^)N`I&>C"PL]NX_ZFK[__[+;?T_RP$A/TWNB!R_1&ZY+L.^?*G'Q3C;VKX
MS:VZ+ZA--TWXPIZ0EH@^7"I72EFOG4)U,#7[H>'0O*7!^$Z!Q'\!YG^;\1]:
MQOW/G59)_X7`X2%Z'$7SDV""!DY;H<N*\-X@=`G17V"B#)D7>(LY?-QV"TKX
M%'":H7^V0]H'R*7_@;[_[W3;I?Q?"$@$+Q;N9\\_5+`X`*=I1Q6V.J?!H$28
M*!G^-3A"_R(+]I'Q]0J"R%%7,;K"=TO3\3L#7/X?>_-Y$NQ&$LBG?\/_LU/N
M_XL!:_PG9`2`JDSY%_P#?SFK>^3.5_KNK'Y6Q^\;\&5&OQR,I.^S^JS>.F@U
M#F:0[(`G+2-#W3X(_V\_)HYZ7GQ6M/VGVQKH]E^#4O]?$'R$_9=]PA1@!J;=
MVOKD8S#K6C`+PG?\4#JU`<CT[TB1S[`>DUOBVENB65D\E=JS32/LUW"EC7B8
MT8AQ;B,>;MN(VYZW)=P,.,V1-\&3+'%OS?^OU>[H]W_WW&XI_Q4"RNC#V>\S
M..^9SU$TY;;]>.MVH(7Y@:]I0*`1FD1^@N",:!R%P'/@[&<"YPFQ/Z%93\MC
MHCL)ZOW/NZDCA_X'/4/_TVV7]I_%0/U!!3U`Z-4L2!`$ZT#)+#I/$/X'").8
MU8,E&?(0S!,T\58>)6A,YN15`KP`2PO^F0\D"X4]6V%A[QWF".O$AZ_/?OK3
MTQ?/7IW\]/@I\L()>O)?/YW\^.PQ>O+LY<\GKQ[_"7(Q+'QRM08[8IYY">JB
M:.G'A&DD//P@P@R$)*/FV(F#GDW1_?E]%"2D&(HKYEDS/X1_&"/#^9G3*"T"
M<M`09C-HSGP>G<,9E3^=0J%#CA2[J(8<4(%*;&_O!7%S2M`JQNPKX%4C?[%<
M71Z@J3>'9N-"X_,@\1U6"-S*A\N@0I14!JZ;]1[R5N014D*WX6(A\QZ&9WHE
M_H4_7A-.2FY43G@M<.L)K04Z2*TF]D$$QX(E%`XO<)$'K(+`\1WBF877?]H?
M08P[V)^S$*^T=+CZJ![0)C1X';0*W.EP;$<F"4Z&\0BA,P/DD<I9-4JI!ZS3
M\6B,+J5$[',"APRL#_CLB'V$EQ341N]P0\C,(U.0##7M&HH!S#*,4!0>DI<D
M.^%SZ"\^F#/@]%A>YOT@TK',"<Z;+/UQX,V#?]+U*IJ2,M1J'#%IV2$IH1`R
M1VF/2U,7ER%E3#.1(N#"9,W8Z@?>#!Z@,R$XGR5`EC/\8>Y;VD?F0:,B3FVA
M$!+6X_`0/2'%9.*$LU:,N8X^T$D.NB!:RLL@'/MTTN%40*&R=R#3$0/>;+YK
M*5@IY"*1<]P"'^2#B)(A'A;0%GE\MB;K^8H/$1-%1M'D\H"5X;\GU!VMSV8"
MGQ"_C%E]K$$*W>'V?.")CU#K"%U)33M)"%.C&>B4`G4H4-W?\2H!>`);0\DZ
M]AF*:<NC*2N%42#.-([B&/<&14,E3!4/J$5%Y2_`N7#_`,&1WF0T@8<,8Z%-
M=ISNS%^I\YV4DDXU<J0.LZR!P+#=3Q*8U./8]_"JKY"N3T4_L3<EYX,AIK#S
M63">0;/@IA!"&91N/<%5R8*!/]8;#LOY2AE'^@V*P",M&L3J8K6,,/NAUTGC
M[$R8)%->[^\?"(<?^6,/%AE*0(1NUR-:GAB0'PBE5C+8%XMZ@Z0N<@BBP0$9
M%^($SP/A5=AJJ=(KD993JA+D*YA-[).%,)P3+L<ND,/%L$E-YW#:&&\"P1CE
M3F+MQR73'H-/B]4RI7MH.YFO6"(G_(L6$5BRPBQ!.E<\@2;`BB662KHP1A@I
MP8$FN$<(]4.!(Q^G"*)U3$H!NL%O%X#'),`+:`QHDYXP66;:A4#^&/'S"($]
M>H`'WZ?\+9I/E)61<`%<-KT`#&<G\Q0(0TX-UV9SAJ%4]HH4168?Z_,@H<[(
MHW1(TH]L'M-)SA8F6B5CK^F,36<!FTQDGF&!DAK3[#$BX`,9A&D[2($T_21F
MLU%DT)MBY\U4U&!L0V=T@`7[9#`?J)%]HY.=7^Y$ZJ7)#`J!:V"@T&\.47`$
MOR?D-V2!1\+%6-(KA6HJ]708J$$AZ3]%TH2)OU+$/P?DR)1%)0M,9^F$)V,/
M7#+VEY!D@HF+W*R7P"W!"$9$R!$D+1%(H^E4C*<BB^C3IJ&8*$DGHVR(Z5UT
M?-0XV_X9$"#\0-0MEXL>PR$;HJ=L&/'Q.DZ"]S[&'"0OOG[@U#!><LX#M,;[
MXCFEI7`B?X*9C/L(9%K*Y-+K!>MS:23%53Y,6X@E)X?/)]*/Z=W*=;BNVW;E
M*6X5'B4\#?;@/B!V`W9][M"9USB2/O"24%6\EA%SZ(PDWWCA5ZS&:<`G#^V0
MGZ*53V<);O+D,O06P3A=<Q;^&$M#0;*@'9$L,5$&HSGA73Z]Q9UU*E[`\&NX
M(M.!I7X>`8]-^$CBW*-H31D*9J]T9P$=S4K@%9/%AZY5!!]Y[8/N),H3)M:3
M9\934OE@QI8K,K_XNLI$%DK1#OH3EHI!DCEG<@`983ISJ>RC3H(#W@HL!TCM
M("(H;<AJAC<7YQY?TBE9"?9'+>[(3$G%`X=N"ED!`9G*<B](R^L!(\=U(@MK
MM/U!J-$U$8UP)^'J/3%`L;2EHO/8O(0JY4*LM>P6*K+6$@[N-NC?-OO;87^[
M[&\/)AR;"'505]&"."$TR.R@EUWQ.Z^4>4N3DT(HI(BPDNB\9@FN\)\E-:*P
M<<7;WOH3H/'?@Y">%`B=WHTJ@W/UORU7C__>ZI3Q7PJ![-$'9?!]^'R?.P",
MZ!(-QU)4E)QJH>%+'>]G!\3^=Y>7/WZUS?TO^OE/M],NZ;\0R+3_V\KLC]T)
M2:Q\\"+8@]7^XLA,>55:_-U1X/&?/#SRN_#]`<BC_]9`M__OX>\E_1<!:OQG
M@^A[7W>/4.\0_]-YT,;_--M'5V7DYR\(G&:ZXR,A2F[!_\_MZ/>_]EO]DOX+
M@0V.>"VD..N!%?">?$',!W9P49WY\WE4/2*;]R/DLJ.,TKKW<P#S_J=VX?:_
M_:XN_P_<=NG_5PA\O/VO-F&*M_\UPT!N@=G'VO]:@\'G&PNK09^RTELBPF<E
ME2+"I\$.;2AIH0H]>Y=JALA*,/@M>O-C#9&MO9F5.*LW\ZV<S6#YU^Q-D6\Y
M7Z=A'N7HG49$R+1I6DPT*9,:5\VUYBC(V)K8_U+=G!+\_R;KR)/_.AW3_ZM3
MRG^%P";YSQ#_$+H8LM@0%7K['U']R*<T]'"%&`W!80JZ("<@RMV`CU--TRFN
M=.3%\I;35:O5;AV4:TWS@!SZW6.'E,2JHG#;O7OW0<1_V$WH)P*Y_E^N$?^M
M5<;_+P9J`T7^&V3(?SS\JSY9%&F@OUGJ4SV-Q)4W6F!4Z5(8^089-6ZH[=X6
M(?I8\JI!3NWYM1*,,+!\65:"P69EM0:*OM!*LF%A?YL;$?PRIPCSG7DYT&U/
MQ1)N`9SFXI*>R>ZN#NKDT=MP_RO;_W=Z[:[;@O@?O5YY_VLAP.Y_'2<SN/\U
MF*+ZH8\<I_F2S`GG[]Y[CQJ(82;A@+!PX<=H_P'Z%9[(9:[L$5*B0[)TX%W$
M##772=PDC[APM]MICK$<V)P'HZ'C#,E'R`$OFC'4$R-:)2ZM`G9F>?4Y8+DB
M,H238%J*>Q\!:?Q/NCSOX@@PU_ZG;=[_TNZ4]%\$\,V=V&4)<SNZUX*=%FSV
M*DC;'1*A`82X*_)9WK%1$UF\&^.947DS_%T%3/_)^RB8[,[[>XO]7UN/_]G%
M#R7]%P%X'R;O__HY^S]ILFQY]8>D6-WV[H_>=?:1?&>D[R/3#=E;BG3F5G&D
MWSWD2D_:I4+:XZ?4TMI%+9:MX<66]4@/Y5[P-P/4_AMH;^1-J!5^\?'?.OK]
MW_VN6\9_*@2N9__!U>^,!S=D+1+Z`,X8)]R_X;8;5L)6X#3!#3'A`4!N(?Y/
MNS=HF_%_2OO/8J#V2#L`T"3`QWAVK.=>?+)>10MO%:GBH#)W;&<!DD"XC)9K
MZAGR=N$MV4OU1%^<"6R6#]V.14`$'X:\X_ZT7JUF73E><X6HJHF5;BJVF;>+
M69NH9\NXTE-)O\4M9VXJI9KE*1?=Y^O^:^V6I4>)XUMF!SW*ZJ#TRLRWW*)`
MZ8E!3D\HN&<=B*0^IM++>G8]%DST-%FG-IF#:AD5@'$4CCVUD/KF:BWWWF':
M>AU6\RMK5#8F:&R8!(#()D.6:\\AFVE5N%Z\)4R"/<L708JK&XW!2;_9$,L8
M$*/Y<S\\6Z6%IH/0$$W@Z-3:-DLFXJ#/,1*\2>,IRR@)`!?Q(FVBRMS:@PWM
M'=Q`>Y/U*%G%EO;F%:\U0+-;DC-O8[@D]Z^$8:UC.QZ&+GX[]Z>KMZ$?G,U&
M$<>_UFE]:H=WA&H`DV2Z9:YU),[I_T/F&1UYYV_C!D;=EHN/U4+T*S1;F5GU
M37^GG:)I8Z32]T]@I-(<!ZAGUX"GE\K&E,]9/-9,E\5G#82SD0;0.8NM!0!J
M(`-]M"S-T$=,U0>91>C/"L_5%NU.-V=,\Y0YGSZFW<UCNHT=@X4*[#W;+:AG
M54;3M2U$A-'$F,58.$V6N+@UI^F*I4SC-(_2X=8XS:.<@=Z*TSS:/):/U.[/
MI,]'VIL[1I]:,SYM%AFJ54F>MU%D-T]P_V2*5&O8?KW8S&FZ.:M'=^>KAUJ#
M8D!M?O]45J,75A"KZ66)C7CCB_G,Q>JM_SZ:KZ46U+J?S&UZ0I!4N4U/&G*5
MV_1RAJ*G=HZ9`O`VN(12>W8Z'9M-*;.YU*8\V1R+@H5O4<CF7A2TZ9V"SL0^
M!3OK%$\QU*<Z!UN`)NL6<1-N]JTGA>I?J]>I0F.LF^JU4">`:RG3UOIK%-G:
MJDC;NUI/SYLQWXUTF?/=DC)WOEOS['2^6W=C*F3,_H_"M:#9;^_[W<]^V^SX
MQ-F_?9$?._LMZZN^0EAH04]CIP,SU68:L*7?/*<R9O_FN9^]/9#!-O&OC^"&
M*9\UX<WIGCD3K1V<.=%MTSRC:.L4MPRZ;2[JB>P<=ZNB]$F=/WV-?9,J#NF5
MN)E9];U#3]K-&STL]ZPIGO?ZF[(ZF5E5\7-@LY0!B?,]]TXP]/8#X2`J>TAL
MD#WA6$92F.D^$19_B+0.PP\#;PJE)VU8^LJPV#7V_6TV#08'R*)]^QY5I7)5
MPZZA++74XG.R\A?J>8&%KFK]CMJBZSB=#+2\ZE$,3V0(,B3*(=*@UN\9[][.
M;<S(EO+&.=[U*U'GUN;RLY;KOD6RM'4J33RP86@<"J9@S(?-1>4)A]GY\H2N
M#:AL%*,`+,=;*=@EPXV(YC1Q4][\9FZ0A0'RY&&`C;MY$[)ZX%/;L5%.!LB6
ME0&LX7Q1[F#;LVW(M$D&[S^\)KUL;+.].+OR(B]/_@#D=/^FSM]0J5ULIV"3
M*C=7E?4EBP7:M@+/;=U0&QB26<:H;3@KMQ>TJ>>M/"I3.+6^K@UV</:TO>/F
M-EZ7&,E'65ZW<FR3;%.;`5VT9%L;6:BQ6_"D^7+-;1[E^/4:,N:C+)OLA]*Q
MOC&#=$%,3ITI#S[4I2?_7!O%K.;SMF[^+ED54:AOKM]NRB%^_35]IT^I/-N.
M;#PW?:L]S#EQ?*A(L!;BT(=E<X_)-D,`<G<UML/XD31)5(EZ'*W#U20ZERO4
M!/*'BJ.TSE_;FTY3:H^4_8<A)-<>]M6Q-H1C/<5'[V"M164P+ENGV$NU"+VU
M1YI>Q[J#>*@O8-E"FYEV$X>WG0_FTR2`LLE-P92^=`DFO_#:0^.D8T-S+1N&
M'317)RL*-]+:1\;V,&-MSYIHMC(LQ]'923=+7]G5;M@\VFO)$K=LHM9UA*9\
M!90F5ADO:H\D-OVIEGC7\MYQFG19AZ#N((',=F`&GN?_UVT9\5_Z;FG_70AH
M_G]:_#\SQ)YMMMA"_UD\`6U1_W(,O8VR>)P3]41\N^*(K-U1&_B#U,S\MMGL
MSJ=19+9/V`-IM'PA,%.B\'55I%Y<"RF;M5.*5$8`0@VO2X&7E)Z@EN40>JTY
MD;.!D#I.6-F:-K*2QER5\G^0>&/Z.^V"S::_OW%?1XC_EP1C,L)XV&+?O_D(
M$+G^?VTC_K,[*.__+012_[\/+/@[)@P(_:<Z_GE#2O#?'+:.*B/Q4*T>5<;B
MB<1_+N,^?U9`XG_R&QZQ@'P;\5]:/5W^Z_7+^'_%P!;T3\.]E&3]18)Z_]-N
M`L#DT7_/-?U_.V7\]T*@UE/V?[T,<5^*_RDF2^XN)#-FB]B&:.<#F9I?.8:)
M[@0A/1@:'ED;;+I/R#ES`Z>HAD"%Q^G>%3C-:7#A+.>[K(/$?]Q`_X.!3O\=
MURWE_T*@=H_$8L1TZ,U)),BE'\\KE6@)(15C5'_R[,4!&*0UT*^_HDG@H_U[
M6#!@5ZCNPU7+QW`[[00GAK2-1H5&AMM/XC'^!"GH]:<06I*F_Q=JOH:%9K])
M=^,?*GLT];]00K\TFT?J.V`WQDL24!*_K.P!MJC^[">,Z3>D2A5=]OWYGU_A
M!-^2!`X<+FNI*GNL6=\\^^G;1F6/70$+B#>=!U"S\^`/0XPTOR!V;]^;_!T:
M^?9(/`!J4F+X0=$?`OI[1)&/,"8(DDOWQ")ZNW):MI3R;9H._W\\CQ(?/?OI
MB/_$:?#OY#+!;4+UZGB)#J=I(UE_T$&X(I'YZ/U\E2O@5TZ3JOO#:!R1:T!O
MX_ZGGA'_O]4OU_]"0+W_#8.'5[03.EM&<&1(?QJW,>./[/KE$Z1$]S]!'_"#
M+7A_&=WQ#H)Y_U.W\/A?[;YY_T>Y_R\&KA7_*YNP;3=[I%F?P(H&]P+51\/3
MQE#P$%=E'4_3['*.QV:.V^ZU+P?@_M>ST%NM8W\\\\*S':C_M[C_79?_^ZUV
M*?\7`M>-_V=>`'0"&?#'"X66,^[M.3FX%!G`K*&\2O:608G_O*,Z\NE?/__K
MNMUR_2\$,N]_OQB"M>#1MA?!TUG$KH!E#U,/[VC%T\61D:^\%O[608O_N9,Z
M<NB_V^_JYW_=3K^\_Z$0J#]`BVCBSQ,4A?[A)%CX(5P%[LV)M2I8CB*/FX[B
M),A#XR`>SWT43=$4C@)\%'N3@+K<>''L72;XCX]I_YT_05["#(J2`_C^U_L)
MBOT$[AOWL80Q#][[U"CV`$VB%7SCGR:^-V&?(&,8(;Q)C6(TGOGC=[@\S%1P
MEM!'#QI\[Z$;NLK\C.DV5/<48>U$KZ@@7`PWJ"Y_TB(<"P[V0>%E6KG?'$(X
M3Y7=D:NRQ9LKJHU+*R;:OOIVM:6Q+^MJQ0[US:M#Z,A&XUKU"T]8[8(/LW4.
M#:M8;^A%0/8Z]Y.BA31$/V871Z,6BHP'R+66K(:EV*:>8"J\MM`Q:O%+C%(@
M",L-/^1U`[`[B+3DHD0E[32PHJS&$[@^SAIN&2UH;8WSUYMQQJS`(SG0>8#_
M(=2)$?(OO/%J?HGKQR0?D.O_D!=.\$],S<%BX4\";R6JY:VE7,`CA3QHI'UB
M>%)NV2UU_)_2F@;NH.I?JZ13,&[0=#S(K%4<OD8L6\;TN68A&0.Z52G'<N<;
M(RE'"S!&48X'8`X;=87(Y!WUN;]":A^C(%0_XXG&OH!.5YIU>D)RH"`&*%#;
MH'(I`'J>(BK_AE1D]3ZWYQ?=!=5BU.`O9W(YLTGG?S*D"!TBF3CL.:Z,MTLJ
M&!MO]44`T#73J9Q8K:$!_^,#K-^9)(@HL?EUI!-"[.+SEQ):&D:U#II\H\R&
M0Q9$J_^:UK]4BF3+F/J)S!SAQI%.M'9+GT$`=,ZT\&1)LWS$E*'X<-K(G@L;
M\%:2"5PPXNG#X?4FC?)RNP5ZU]LCO/^?[G+O#Y"[_S?D_TZ[5][_40ALV/\C
M>GN;9?^_!\?1L"@3TB:W<0-=]'`1Z.+K3N/K#E[N'M%[(U&'+HB@_VO@Y<NN
M!:"?TZ6_2@R,2O7`SL%I.D]>OGVYBF)_9W40^N]V,^C?[;8,_5^G6^K_"H/?
MG:XG[E=?_2?^^1]?T;]?_1][TO]@_QGP;W)YK(P22BBAA!)**.%NP^_HG__X
M?V\7C1)**.$.`O`'Q/Y^Q_[^+_W[._;]W]C??Y?R_"?[B]C?[]C?_Z5_?\?2
M_1O[^^_L[W^PO__)_B+V]SOV]W_I7\:T?L<V'[]C-?^.[5!^QW8AOT/L[W?7
M:G())?QFX/^A?_X3UO^G7V7N_TLHH80O&'[W[T]>/CG]2FP(S`3XO[])O__W
MJVPAX-^HLO#_D_(B]O<[]O=_Z=]2$"BAA!)**!I(_!__'VMO'JPN20B0PN,_
M@KN/$?^Q]/\M!FRC/^P-T;/YW#\#*V#<;"\.DBA$Y\%JACQ$XL4AB*KH5!Z3
M3B%&3VCFS5?^!$W6/EI%"/?J"B=+_(47P@]BOILXY6'^'0.GB4D]FB;C:.GO
M*@10+OUW=/_?7K]7^O\5`MKH#SM#].=P`H$\8TS,P<3'U#L-_!A=E-3^)8+3
M7,7!^-VEM^(15]LWS@;RZ+_KNAK]#URW]/\I!.RC/W1;0_3T`KQQP!N(K/;H
M!$TB/T%A!-:\)%8,D+[@%8P'\()HGE.GLJ&&9^$4<PJ<-_97ZYA5P^P,HRFB
M\2C0".(`;*Y;SO\L7)6L:FL@\C\?FAT9`N?1?Z=KQO]KE_'?"P&[_S^W]6VI
M$0#"8(&W!%)@C\?1N>3GSS[SZ'_$X#<UZ455_*;*;-II$AXK`'=KZNXS9?DX
M#C3V%?4A.(?(`;0:%GT(,&!QND)(X)!J65',B>*W2MO;`);_23]S%N"-HOBF
M@P!?7_[O]P?E^E\(I+$]*A;:(R1&'QPR,_!7H"@1\4=B'"AU'45]%CKDMEM7
M0AXX31;Z?X=.0/GR?]OP_V\-2OHO`A3Z]X1O7@N6Z(EX)%YN4W#0H?3MP4L/
M?8TF0.@5"":9'?EGE'K\J<5XU(WUS"CW$(U4!\C'>IEH+`KM$&EB9BED3`M!
M):_*!DS_'J;^G=:QF?[=5KNEZ__;@W[I_U,(++R+9!S%/CI&@RZJ5+P0;EL*
M1NN5#Q?Z3,`ME2P-1Y@)8-D;B<^(?*>7D]$8L@N?W1]!TOY?;QY`!!#^):G8
M(LW28D4B>L@01B'?YL/6P1>*B`K4CC>L(@8YR?X,/TEXD2+6(;W28J+G9N$N
M?:GV%3AJKT`YP#^BI1=C;.XG:;$57*T7GR5NFO&G*#R4T,1Y<(^L_)CJ(3"7
M$9<8D;P8+:E2O.DFV@G>X$,(]XLK6$,W5/1-.<GS(]6'1.'\DNQV9L%\@@AO
M.P`\H1&@Y^`*F&D<+5A#:*J*[;"']>"*G?3@`G`_L%N@M/3M[`SD9DF<G*IL
MQMY\GM!0$0;V\`WG(<WFT202J>_JYW&$.Q.ZL%&QW$U"RGI!7](>C*8D!#M.
M2W4[9L>=>H9Z"I(35_-1-+E,4_Z%>)_#.S3#:!'ED$B*QWJB)X5')6D2C-,T
M+X/%$B(@T`#[_`0MP9WJAV,20R>=G4G%>A,3*><5U`+A-^@JZ$%F2('J3%+'
M"'@A9(MB4BJ>QY>K&02PJ0!_IWI\W/6C&.>5>N;)>CG'[S'5T"\PKSP$60AF
M%24X%,D!FK6Y?\';5)'%1T;W<1"M$\0^L)&6$(?@(6G060@/<.;-,9YT.J4%
M_4`.(/D$E$XB*^G/1$_^#2G^FV,I>5(97X[G>`+1R9GH4S(A<Q+Y'FY^M,)X
MLDZ`3`([<U*QL>4?_*0R62^!%WCSQ-;!P"ZA9)J$SG@T#Y)5)9T#Y#X$HXKT
M.R()4#UP?(?D;51P<>=>/.'R49KY>_J!*4FI*E3V\L9OUTG:P)D7Q)?)V(LO
ME>'^$5=\B>A9\&-,Y&+<`XFTGTT)>?F$=?$*V+8QG3A$",-MP?T1K`@+8YBP
M.<PQX>]'N#H]>QI?%G"I!W,M#_`3<C2=E2^-B6')3+A>=E:JR)+S!2N8W=H:
M*18BPJ#PY/TGYW>$VXEF1E('/F?*[J0BA6,DGUCT/)E/R)=PL*F_XF3&:J3#
MS:O"&<+(G@4B\F?G4ELFJB'MP'-I%(03M4M8'G4(1+ZT\]]CTO1&<\P>Y-IF
MWB0Z3RP9/9$!T410*X3_`DH-(2(6IEIRB@GE&`("E$,:R5K(UUQI\2:W1`)%
M*;/_!_Q"S/CY>J2NFC_\^93R_W5"PN%(5)!4MEL)$[X&DEZ87U9L=RG2)>`\
MHBL^7L\171)QMTS7X9@T:1),ISXL]9BPZKBDZ-R?-"J+(`'4""N6%^^4&1,A
M`7=A*B]142'T)4+X"2/'EUJ\#OMGN+??>_.U#^G.S0'_,^D/CVB/TC&7V!@?
M]#""W9C4,Q1?NE6CB&2I)RF%XI:2JIAN:L$E)#(KZ)4NV@FWBB#PY-7,6V'D
M\%`G:`3DD,"$@K;2&:4'2$^+>9ZN8K*<A<5&=@^.GE62H$Y"NM9PE+GDZ5]X
MP/2-K!U3#M2R:#3&2X!9@9N)1<L-B`N4&6W@@:,+%&1-B=0>*YY.*F8PI+?G
M,)V:K$B8*TG%%G;:VCJ"4%I&N%Z,?"+DD-*2"A<0)]HLUV5$CPZ.Q*Q$SDA;
MJO2LU.2)9N>+E50*G#U"H\*(S")IF@FBD@XH*V1628B^A&?*2'@_`TD=VC95
MTDX)SC+)>B(P@<<@I/*%$*BE>IBL328]Z1!9%*'"MU(68$LD<,`]<]LCG[V>
M0\"7E.(A2*/EB%84+BKF4;@D9$4A9\%[7"@(N/H.R\061I369RLI_8KJ6/29
MB"D!/8(+(=*6MD][&>'.HE\0^83KAU-CS,L2E@.D96F$>.I4B,[:EC#Q3M^9
M4#%$76J8[,&%A`0O1;BY]%$?GQ?DF3(XEA"&&W,;^X$YGW3Z0;VT]I`O/*&!
ML/V$7\G)1\K(>]OZC]\Z.,T,VKE!`Z!\^U\]_L^@TRGC?Q4"&:,_[`\WKH*"
MJ9:F-I\W8/I/Q9'=7/^91__M[D#7_W<'_9+^"X':(^7^3[=BO_]3NK^S*DT8
MY0I0E_S)O`(TO3*3I=3NZVRQ7_*%FCCM(Q6CDZW183>2@@*?E7QA8B)N(@VC
MMT16XSC`"R*KDF1]2].FO(P^+X.*TZ)!Z?VA)^P74ZOK^1+Y-6Z1-ZH*/*0L
MM8$%"U':(!>+TS0"Z8F12[O_E)SJ"!S$EI+D>6A!8\;^JO>\/N2ER[>YI@B]
M5-LMDN,,0CY$R/+9?E^KA+,-;PIRBGIFZ>KML^I5L$I'6C(;DXFCHTRIK*R6
MAEN2V3L`0.L$6K.M(P#.M.?ZQ]9Z851I]!%"#:T_3BI93VG*X:9)0C?(4KZ+
MS`'52`Q`(C-K3=H+Y?&SO.K7"D[3T!'>>!VY\G][H-O_=GJE_U\A<)W[O]+;
M^=(+O5C(?FXPV&-F.[?=K!*VA&S[WYO;#.31OWG_#_Y:TG\A@*506?[OY4G;
M6;-%$;T[-G$YBJPR8H?+B(;D(SZ!1.6?2VOU<U4*&>HO"%KB*95J&A5+!K++
MZ&^Q[]FJZ;:=@GT3),1_>1/4%QBFFZ"=CK_3Q$,_<I;S'=8!-#[(IG\@?2/^
M?ZG_*P9J]YKK)&[.H[$W;XZ"L+GTXWFELG^.EWHP"MP_>?''__M+ZPU>T_$V
M.YP$,:H_>?;B`%5IDFH#_?HKF@0^VK^'T]#;.^IP[3W.'/M@QA9#AD:C0AU\
M]B?)"LK%*:@;$-PC0-/_"S5?$XIZ[8SQ]F&_2>GU0V6/9OH72IKT4[/)[[.'
M?_UYHA62EYU6HQ?B8Q&(H5>M'K'W*8[P)?3Q)U$NO+Z'ZH?XI=.$[U7<S#WF
ML43N%$'5GY[^!3T^>?ET2)J,#K\E5;P.JT<T68TGA,^PYO(D\#M-EEPF*W^!
MZM7Q$CE.<^EA]-?ANS`Z#YL_GW2;I'".Q#7RD!II/OAIYO7'LPB]KD(".-%Y
M747??@OGP\1LF"2_JNR1*XNTAK]>G<QA`H`EW7M_*#6;76Y2N2K%Q%L'>OYC
M/S6^J3IR]W\]W?]CT.Z4_I^%P/7O?S[AKI<G_`YHF$%#Z;Y$?H?++(H.O5FU
MW!+>87":-JO\8N,_`;$;_I^MDOZ+`-OH#[M#](I&?``S(9)"<:38'(R!!GX@
MMH)I^)A1>5!\)\%IVJS/"Z5_M]6QQ'\K_3\+@5KOVN>_M@E3P$%PKFHJ%Z_N
M1K64[528%)0JI+KBIWD0IAR\V8[<Y-S&<51UYL_G454N0#OWDG*/U+,U5\JE
M'+O)>>0.5K.D[2Y,Z53"G0&\_\NP)2U,_^_VND;\KWZ[E/\*@9JK&@#EF]MD
M3!>%T]HL55).:UJ?V.U#4I,`Y2A`,ZZP&%8(W;K!PA6F:#,AT&T9E.3&082*
M)<4FVZR!+&&B)*NEAY(DR][B8D.-<C]34(TZ&AG8I;_Y+[+HN@-U/IQ*LV*[
MJ>#VMIL++F_Y-4R1*(K:E+7;K&V%ZJ/M!9='-LFE\-.;3P>GR?P0=V3["9!G
M_]EIZ?R_VRKC/Q0#-;>E;@!:&=3T[#FE)#9=%,)I4[K9;&K9YA2SV=2R8Y1U
MR?ZJUI.=[8K+(6GU+%H0M2;WITS9:IBHL/6/-4S$Y&=N"^J9U=@,VM)Q$17I
MJ13+.G.+TM_<T/Y--53=C=0SZU#3(=132G^F?,UIVB!MFK'H7\H(#&0$[(OT
M0'FT+]*&(6:6&680V@9STZJ]V5#QHVTD[6U]>,-MU8=T4T/E`2XWJCL!IZD&
M!]A%'?GG?X;_1[]7KO^%@'3^)\P[GSW_4&%G@:EEYP>D$3T%B,M*K$`A%,/%
MD9GHBA[_W78[2["#<?_+S8=_W\+_4Y?_^X-NJ?\O!*RC7UX`\YL!IVF)3G##
M=>31?Z>K^W_W^OW2_J<0D.U_*F@T/.76/:='I@R`1M^=.&#QTSL8P?\:)/JK
M;B)$C8(\$`H.(/CKZ0$:#T\.T&1X*FR+N)A`-W0D#&QV"-EKEMEG9;(B2\NC
M34#M__18,S=;1^[ZWS7\OP:E_%\,?*3]'TP:;OZ'7T$>_)W$(V)?G7&TO*PW
M2N._NPU.T^UVENO1/!B?Q1`W[^SFZ\CQ_VBW3/W_H%/>_U`(U.X1KP\LT\\J
ME9,G/S[[Z>V39R^.L63__,<?G_[TZN7;[Y_]\/2X^N+IRS__\.IEM?+R\?,7
M3]E+\KM:^?'DKV_)S^-!MU()IN@7M%]#]XZ1B]X<0=PIJG\DG@35/R?>&7@#
MM*30T&\A`!?57/L7P0JYE6E0J=30)`KOK]!X@C!+&<?!DL3FB]<AC5K(YBN:
M!#&63J+XLE*)%^APBO85W-%^BG*ELB_:"!,?BO#!^WGNHWT7.0[#_@^`?<N"
M_:L9C=26X'W1>CXA-I`C'W!R,!-]YZ-D'?OH,EK'.,'$1VS6)#C1%*)LK^)+
M$FTR(E\@2"A)2_!PC![XXXN3)T^/_S;V5G(CT*^X&'0X0?=?A_?_!OWT\]R'
M8&,8&1;-\!R"E?GXSWR.O'/X25X'I`-QVDO`8`KXDA!AQ`6<Q-S&6.#R_D*S
M0^,@W&.,&PR7`882^@O(#"'%^"@LXVCLXV4"(O4%X_4<`M3AHD:Q[[T[0K$'
MP0X/`+$9B78'Z."FDWHF$2ETYN,V2/'"H1J(QQ:2D+X8&5P<?G7FKTC%8S\&
MVU(26]>CKWR(+$PCG>'AB1/?0=`6$C`;G%#(EQF>ZA2W9>R_ATAZR6H](16>
MXR$&A,[\T(\A'"=&-H2X?&2^C7R")@NSC3'Y.^8=/-8W+LY#L^`,0CJ2T80@
ME(\)$K'G5/YT\O)/QW\C<^@P1#^?=`[WR>#BL5Q,>LEZ@7]`1+K[21,Y#YI-
M&%>:VD?5_\+]=(;'PD,T5#MN(LO-XE3N"PIT7H?5-.-+&-H5&=8A>AVFU0[W
M`2%(6Z[,OVU(X[_9@S'>1!WY\1\T_^\V>`"5ZW\1<%WYGPK\XG1/COUPP6Z&
MNNTFE7`-T,__=F$&E$?_/>/^IUZWM/\I!C3[?\W(WK#^42=+KG5]IJ5-EH6]
M9.>IQN"2S194NP\ICS7TEC7PEIK)8M1PH93PY1HBF/<_%W[^YW;[^OG_H-4I
MZ;\0J+G=:SL`66?,7?``RD?,QJ3@>FD3L:X-,=>.F&:!K!A)Y^-DBZ<XDI!2
M[1Y%3$4]:J3B0:1%JX..[JI(/OZ8834-/4?L[ZD8T"WM/%U;D*!S^UBXG;31
M<%AIB17D2BN'P<Y'XO=0((JD7U*M]G!!GSN7SP8X_U7"S>]``,SE_SU=_]OK
ME_N_8J`V4-B_YNYA]Z50)\QUPE^I0J"(@)4M!-JLD!71[2/MH.<^;H%<JFR&
M*EL2RY*?[$]I0ZQ[`X@EZU&RBC,04VO0+6E;2LF:D+PQI[LAYZ98K->QB__(
M_AA'X=C+-!;/K0'Q<X7,"C+;]X4&?-7`:1J78M[X"I"K_QOH]C]]=U#:_Q4"
MQN@/'PZE*^P6401'1IY\[RO$!!'6POS"L-(4\/,$8O]KOZONQOA`'OVWNWK\
MQT&_6\I_A4#FZ$,0H&<AO1H54SR[UFUZ0`\\F<Z?W\)&[N3RM@@+A&6+DE/<
M)2#Z?^4&QYNO(Y?^.\;^K^VZ)?T7`3?A_W.9G@>"]5]E;V^O3OV"I*L`0W31
MH/%!.>!7EZ7#T"W#AO7_QNK(E?];1OQW+!.4]%\$I/3_@5$\7J(_M*Z.U(/_
M*;6];PPI._@PK4_KK0:U[KWM)I3P">`TM4M,=U!'[OK?T^-_]EKE_0_%P/7L
M?_864<18P%%E;Q1%=<X10$`@-]Z"Q@!"?Q*?X.-O47`$\@!?W$=#B%`'[T?*
M^X3%#X8O"7PA;_W$&Q]Q@:!TY-D%&/Z_M^+_H]O_]%NE_T\Q<'W_'TJHJ?./
M=@/4!=R>0!3[I>O/9P!.,XQ@V'<6_..K?/K'3[K_C]LIU_]"(*7MDEA_BV#:
M_[5OG!7DRO^N<?[7:97K?R%P3?D?+*68CL"EZSOWV,<?SZ6O;?GK8_@ZDK[6
MR?TAC>].'9:G5"3<$CC-:+V*IN02[=O:_QO^/WC_WRG/_PL!-?X'`<$%N$C/
MU/8?:+`OQAYP!K=U!"&_KBHL8RGP?W[@-.?KT8[V_1SRZ+_;T<__NYWR_+\8
MV$+__]_H@UC)I4N@Y=>G]M>/T]>GTNLG^FOV_A6X6G^H>,.3H\IH>(J3#Q\?
M52;#)T>5?P[_&R>;8O1P^7`=XRI>$W?N$'GTZL81F@:XJ#-)([DIW0RGPY4K
M*<8TA4=20/`@=-NCLWM0]O\[B@&>N_Z[NOZ_.^B6Y_^%P&/I_)_:[DS\:1#Z
MD]),YS<!1/]/;NT=1^'DENQ_>[K^K^^6]C_%@#'Z$/OSARA:@@W?A,2!2,WZ
M:`P5<KU?%,U+#O$%`)S_GX7>:AW[XYD7GNW"!>`C]'_]=KG_+P1LHS]\!%M\
M$OAW%8SF/@K7BY$?P[$^O28Z-?I-0`\0^TQF$%;"450RA\\#G";S_@C",SIZ
MMZ#_[YGZ_W+]+P8^1O\OPO!24X`T%"\6"HC^`!?E<6V@-:+O=<KY&KFH-`G>
M%3A-P;VIV_PMK/]N3]?_]?NMDOX+`=OH#_M#](*^IQL`O/#3H/^4F*E^K5SA
MOP1PFEBD._?B">?/MT#_`\/^IS]HE?K_0@#BM6P(`'8B!;2IVF:*$OW!O+O/
M>KWV=E?WT:@U6CRB4R,:12Y2C\R8-5YL(O7H&DBUMXB2D8L7#Z:S39`D$5/G
MQF\;=;0PR#N88CGQO[OM3DO8__9[\-[M=MOE_1^%0.U><YW$)`;XTH_GZ/"\
M4JO4T..76G1L'N:9QN$F:6@D;_Q#2TI#.$-(;RF:\R2(2:9S3`X^6BZ92B%(
M2$QFQH`6+)`T.;YC*2#B,Q2.\];]"P=5W2IH(7`1[@&J=L8S?_R.OZ%/#8C%
M'$ZDRB%$.*]*1`L'!><*DQN+/9U&@8;LZ748^`GB3L]P=BD&N9\LHS`ARA&H
MFO>.AU@?2$&UHRG.#S$3D-(_K'"M[&0&8<4/T"PZ]]]#P.P1!)=^'[W#LI6W
M7D4+D*M(9.K19=KS9/-T/@O&,RR'A1.(-RXPFL\E9!**",2^]N(5A.G&<U_%
M!^*)CR[A%,A;S_'N#,*MDP#:K'KPY(8`Z)7]133QC]M'J#:):)AP;[P*WON5
M?=PC8Q_4"N<0(.*X=539Q]1#8GJG@>3Q2]S8<P@7?NSB0N"!!`^'P-ER<16B
MGT;U[TY>_/'_HF]1N\$L58(IJN_#RU]:;Y#_#U0]#*KP;8]CME?92V;!='5$
MDE_!Z:Z9QY/S=(ZVR1*Q+%H[79$9;<H]9;E%E]#/[INT[BUPF+!2I#XTL_F0
M!"]3_%WEJD+B\]<1[<SY"K41[\XEWH.OT,M73YZ^>"'"]&ND_4NT!&D_>2--
MFD.-T@\QA;T.7X?5(TNQE/Q9*7BUNX0)#M0WC>;SZ!S7,LS(^7IUZ+U>O5[!
M?(1)3:/PUZ690HF>S])&=CD3*.<<3OQQ883^I5(.$)F)E%7`;%ROENM5=F$!
M1XH0B8P.P3`[8P09Q>Q!_,X"-(79?MZ,@!$NEAMJGI*DT/NO5PF<S(@B(+@_
M^X0Q2E:^-\D<$;@T8*A0O(W:25A\W$I1"-Q2<`3S:3]E]\=\BA[QMW@J2-.[
MMA_Z_B1YN0P6Q_5]L0;@^7R_>Q]A-JJ](]S\?D-D?+Y>/8WCE\MYL,),!00C
MI!73@=28@1W>',#U"MYJAMY[<>!A?I\0!@F>;G0SG-X^4=M__,/)RY?D_HPJ
M655)BN8XP514)7A9%Q]PFTO(@!V0SB8_DP,RG9-HOJ9;<#(-Z:?*_JNG+U^Q
MBAPH>?^4W=O!'N'[V^=_?O7SGU_1UU#N(2T#$A`F((T<X2D_G[2KE!GL^Q?^
M>+V"YAY7Y_@AKL)8VS-U;)D6EWB!23;FZ]KS49W!AGP]>[YQ%,W'&[+]U<W+
MAP=P.O?.\!IU.,'"A)A9T&&,L[(_>"3!<`C&B@Y>LO3'P33P\<J-*UXOX.H-
M=([;`:0(S`&SDP6P+8(<7\E:G/6*Q0<C0:FS^D>V?J>$Y#@.H;[*XA)]1ZL]
M1G68[]$2L[+')R^?OH3K0;!4E,Z/)DE8!>*:!#ZJ/A:WEI!,Z=0;(B,3K6I_
M'H0^[@A<&5[(^6+,E$9U\A5_^D94_VV#MVH\P_R+YJ<\@Q?U]=='U&04=^-9
M"#=:C.9>^`[!YX1,>_H+)/,5=`*Y>+*6+OJTTG^AYO_4FF09#/T+8[E,$[U.
MOM[/20=SI*HGX4C.H^@=$?,6W@6]@0/RD(\P%N05ZX<$F!-J'C</Y(:3>D0R
M6,,!+UY:,R`50Z)[HE_Y1]*;>W22\-J/D508D1NT[_]"21,UFV?L"VW27BH2
MP$LR'3!C(8CSD<'3=Q[CU0*N2YF(Y@[3LK%`BKD07$8".88T8Q4*9UT&_Y+9
MF7;&4=H9K$-=;FLDRWFD#=(+D`J1G('>!H0G*TY"W^_3!UP5B*'L(5W**J+S
M1;<"9K]TWC30[W^/^`.?)`^>X?_81,%2^3F)?"9CN">C1RJ5&FU6TWW3X,$%
MQ=V$*$69)SHRO_'QXW;5=,7%@[>I.(GC.2!FD@_5(Z4,$\F>%4EO,O$GA!D*
M1'MOCK9`1\E8U6K'BW6,*[]HI'/CM0.3@U308A7`*@D+W\*/0K*[XA?^.,"J
M_T#3$(JBI0'M\A(XZZD19-A+T03\$_<,*89A5J.(I3-!31F1U3)-T24II$D@
M?^R1C[3;Y?=]\C[M&-Y*(MV@Y!++`1=B<%(&P+OJUU_1/77(W#>VMVWRMK)W
MSYSK9N*N]2W,!9C\&:QAA%D"11<._96E@W$!A6O&_B)B5RXE2V_LUY,&O4>)
M;/@C6.7>>_.U+W65^T;F77R)F%*[(R\^>X]G'=DS';#>(R*2[Y/5&';1A/$$
ML(7TV450B[248'6?<)T#QDGPD.!WCNA[95G>@^NZ/+RAIJ\%%R:[I_K^6S+K
MI#'BK_A$)!GVV&*.:X*=#=I_2U;P/<Q>O/FY=RG8RWO*7_:D:=8&[H=+6"<S
MMMP?H%\8:WV#&&>_`L:K[?<R<]`,5Y7Q/,)OQ()]1*7!C*4',I'Y(%:7<P_W
M(B@GN*6H.ANH*+6_BE9BL>)964V\GRF.2B5$/*%<'J[R$J,(TC8T@XB_0[I[
M93+;#<OZI^M@/DE4A5"J/\'UB8O/1D'H8:F?3(AL43JYQ-NO1;U*LAT^EN0Y
M1`7K!@P,&0!T>(%%-_&]R;XKV_.TAT:`J(3:/65KAESX?<6D80M^G7S\F`2?
MC2!/L!L,N[D84M0VM(#M););P!/(+9A>NPEPT7#.9N43&C$^\\,-3:"?/ZT!
MN7LFBAZZN_@#Q\';,;@/CX6?8(LW>>W'L7B-?].].%%SL$4*[S3FC+L@13$D
MU""5&IVFL%-#Q\?(!93%(A=A)$%;E(`W)2TGJ(@^>0?=<+A$^K8<>H4G8C=7
M:BF:#QRBA+*\AXHLKZ&IEM?K$`03],!)D.@1WF.`1FT'K/2/1'%(6:G,0<D;
MH:LZ#^9SK@U!+-@HR4(5XO0J1M!#X8Q<9>J0N21?78E7=J8A$<;*"W!FAZ)P
M1DV-<L"NB*2ED8LM9[ZTAE&D1I"3R&VPB]?NEU34=0ZTRELNR?<8":7>'RK[
M)\^/J]]6V<J'9[^F-&X0$:9.=_"L2!"(&6KI7IJNV2P%$R^],#GF,C;;E\,[
MO#ICD:-*2A;/3&?-"4S40-N+B2DAW6II!*K_XKUI1HT_(!!=Z*:>UH.W_2]?
M/7GVT[=X`NU1#H+?IEOI%$F/;!'WM.V22,X5ZKRS5(&&OA;OR;^$Y+5>J^*$
M:==EZCS2%$?;L;UZE5RH>?\^EEE>_OSTZ9.70#%7E?TESNE/\)P%Y3LMB[.'
M=J/"=R)4V*2+(Y[G9W@VK^<>40(N_`34[/3T"M!@!S:&%KE"-S2B0JJ,2;5%
MQS<!A+6R`E/*&DJ\G"F1*UPV!O404`T6@[%8IXAS=$M!E9K[1-I/TGF-?YU%
MT20"2MQ7=.]L_W0@;YA@N_@=KX=M"\RM9%W:2T*U#3I=Z%J!-QKD*"0:XQ_[
M+Y_]\</)#R]^O(+,ZU%EC\U5JM53V!5:!7B0$,&4ZO'P+H->5T&R>',/<^X6
M5\'$_@JJ.D[?P*;I&-8I5@,HLH&=X`:OR3G>.\+_9K#,16^?LUQGRP"RX9WO
M\BQ>BM)X(20/84AQ#$I&=O=N'7<_3<(G[C)!AV>LN%_16>POT6$@:T#Q6^_\
M';K_@8[[ZWWWZCY^=X$W6PFMYO`1GO;-B?^^&:[Q8_O;W[LP_2DF.?7@,6*H
MX0G^235MKBA9!HM=EO]W[[VWR_)!!MIE^60GL<L*Z$Y@IT-,)/5/J$'2>K["
M-#TA=R>?>P%1,`,#EH[!(38Q,7[T0?>[_`/=M5_A_T`SF#!]%*5]R-IOX9?@
M+Y54]NC;?HLE)@PC]NG!#V:!(&N<@X&DO\1/R0J8(!$OF/1*-!JK"<AGQ(X2
MOCM0$)S3'1"U&"X2HSP/L*0-'YRF3,_?R'I\P0Q)/_`Z(!,1<![@,A^@\RA^
M1S7LY+@*S#9G""_^U%Z@CEDK29PF@[OB&PYMG'1PRVP7:$+<S%&$T^*BH$60
M!>%&!>\]X/.)`YVCB;Z\!X1\NEHL'2]9D.%C?)3V:N:6&PFQHR;Q8E[/5AWU
M;8J(,CO-XJ0]C%SNUJ6*$PCK[CQM2EY#/K(9K2.B15V1`_ES^(<:Z5+9F!$T
M3KV.0SB3X=U*$C'%W<8F=(MHPF84>LJ$P*(93C>Z1#\'N!$^^AGO.=`A<MO-
M5J?9ZFHB`%$WD;+8WB1!U<.S<96*`>30D%+#'[UX!#+<8TP'F,1!9O!#:-4D
M51BFC3K^%UHT<3E-13_XRQ\?T^`'CW&Z82K#O&'JPNTGGZ1_QTST,$K+@IV?
MI9M%MZ;R]D=6=ZVZTKZI\]KNX0%E:E.Q<WZOEBEM6+?`F9V)-XET<$BZ7R[-
M-K^`NZ=<24J=5KEYRN&M@S3G$K*7HOM2S,W396GDC[TUQAS8.5KZT1+C1I3<
M;_%$\T+_+?(6T1K+S*P@O.F<^*/U&2O,0==E2MJ\>+[-<!D+*1*]<(,#A[EY
M_1YLS.JO7IS\C'=QWZQB;^E0.[:XR@K.'5[(])9@K5M>-+W)(@B)A:-<LO./
M=8`7\L-W/EZ)5Q[>"1_.)][*0VUFE8X.<447>'+TVVZWN^T$DEM&-^W0K`PB
MSFJ"C.?-8PC3?(M9#@O\.E2$'2[=M-A6_`@6!*H3(5NL]!X_JI/P8KS=_@YF
MR_YW_-BKW>L1F>G09:L+.\H5O2/FU;??HH?R+FX_BH,S4(<Q+1\_FN.OF9:O
M>I2>^U*Q`;1UY!4YV(578'")&S:Z5`4])M+AG)C)IR9ZR#E&]]$OTJX0$I.#
M#YH9;^,BMC<,0DR?;^Z#G))J*QB_4"D&CJ#E-[AC&A*:YE9T'&,I"@YU](TH
M6T7"*#S\IQ]'5#5*EN@AJB)'L`D'5>F"DMTP6L?$TIQT)\LX.&N8CSC.&Y4I
MBHA/]2EB#W\?ACK5K"A)Q]XJG<1\.Q"APZ?H?@U,V^+UF-H2#M$OK<-';[Z^
MKZ7B;S^N"C"&V%791+N67*OH*Q!_+1-^3Z.-W\//[S%-UWGEBLJ%*ED:=(O"
M9L^?GKYX>N_>/5#QT$G"/H@3;9&2)N')V+CK!GJI[`?54DPY!D?Z%SCS!85V
M$]373?Y9;:;6)/SR0)2@-.F*XU17.H6I)8FZ52F:?DCQ37<:JL&,I,JYXC0@
M&`U234..F5TM)\T;4:G3_F8E"N,S25P\$@HV)>D-5<YF!5*MO86A.>PCTCDB
MI:`J0SI+N+4R]%!+=/GO229ENJJ"AC&Y%.G9P)7(S'JA>*AU&4X,+5\JY&+E
MR0/E;<0$"MF(39I`GGML.=8D(C["Z<XLC!#-X8=CS#%&_NK<]T-Y;9!M1>E`
MI<LXY=W'=.],=,C'U18D86<&5*],3@DNV:F!>!&FS%M0_Y-@HB]-)#V\(].R
M_LOEFV;8^$.*"#\U()5+AP:B\TD!A!(1$_@HGI=*4_C67Z0.Y4TF-%*B3FX_
MHJ$*QD5P%J"@K`@,8A%.C1@.C[D2^XAU(=7[I_H(]IDM[5J7K0[Y9['FO5ZI
MM)HF%R=+6^53)[*H\)KU9!=/Q2\V^S4>YU++%^-]F\DV,K6[#2S<[-&S`8)$
M?9\S](TZ?+84K%=/6&I8\8P#3H8^.2PEW$:,I[P$P(2F\WGO]V3QI=Q!*OQ`
M(7N)7XCRK.O<WCXFYJT0Q.DXDI2E*$N1A"#ABQ*24@4'"@]IR$R$F=MQ70;Q
M"E!,6>C^4951\&9-T;\9N+-E.U]UJY;$#YTS"^0),LO=T..6*LF9-I,(U)V,
M(HG[U,*06!EYP3RE_P]T=8,/I..\\%+T5TU35JI[)EK^#H[0_T(.8.EQ./`A
M=@Y-'\3),Q,DU2\'Y.I.+,J_?/S\Q5/N'H9%^K^O,8OVN'L=F+*'DP#+J9H?
M'-C*K1=^#.YFU`//H0>NK+SJM^1'YC$KP<01)XHD,28,SE()>Z$;9/+IJ,+[
MGI_XJZT4JP0[_V06&!GL+%K'].`4,[2?22;)9^CP4#Z'A<T#M2JYJE3T8W9Q
MK$^MT(GF!M#T2?CC;@?,70B:";4/(2V'Z8[[AY?BK*B+&3G"92H!G.*(ON`6
M@J(6*1L,MV)G0-"LZ?L+.0?>+J3GV\RNPT1-*!Y4]RY'=GK(19F;R"3KY9)8
M9>)Y0HTV3#2WJ-"*^HT?;U>HWU*%N#EZ\1ERAWS"4TN(,W]%*(%PUPE+U!ZF
M%B3,/F0U8Q2UHK)9A1@#$6TYE!@+MP6R`.()LIX3`PM_L5Q=`F.C%>#>];UP
M?NE4L(B"Q!8'.I_[%U#Q%5;%=0R+$IR&OSWB7]5M.;S`::B[AGLDRA`L_)@7
M(WVSJU7@B_JVDL'JJ8Z!_`*CZ=5BR7FB,?Q4#OR&IH:47$6EYZ!L-J-@DO+W
M>/[]_//CQ83VCZPHJE%E#EBP$(XF^T[A(5C%E^0<+HHG8&G`C<727RZ7'-ES
M^P#YJS$S.L*+`K$L.J`Q@RV"L)_*E4Y%-LZA@T7&5CW0JJ5&S`JVS"ZGLK<.
M,>H)(^*?GOX%+Z:JFP\;U:J^C>6&!]0)E.X&:DC:0U*[9)J;BGR4WFDE^)%4
M^>39]]_+?$.,HX$!^A6)5-PYX5=*!(?G<)![R$;T5Z*VT&88Z0DO'L]\[JPM
M/#"ID4P<C>;^@AB`K:#3%I#.6U%E%>EF*(,,/[$0MQ<#FC+)Y0BR1+#=@A)6
MS%P=Y_=`IH48=^#\2-6Q$]`90I&'S%D9=.IB)V5X14&_?2OI%GASG6/N$2-M
MC^0WJ6?2/7!.^>6;;]\TV<ZH)JG["`J2)PX[+P*''Z*+IH49'E%U*.^7U\F;
MK_<;>K&B2'3>I#:2_P-+_.O[^/]F+:8^GY>#YY^:7.S/Y)VI)*/J@T0.EX.8
M^4,><"MSW9X0%P%BG$1T9`M[[@M3&VK%1LB1U$&%*%*WO"WD;IQ4+&>S&4SZ
M&>],1U%EF'Q0A7J.94WU7<#%*%<FFCKR"WS@I&VJHH&@ZXBZ>6GH5G$\4541
M0--5C1XD1(S6P99@'/M^2-:95#7"92A"Y=Q[\.TOK3=9<ATX,>#/CFIV:$S\
MU!.0JU-2;[@KR:*1.B`8B!/]<3ITD;0.TZ65?R/-2;=)2G.`^"BC_#9M$O-N
MT6T"X3N9=5`6KJ`JNTJS@B"5RYR4*/;L`VT`8"+6)&[^!ZLHF9/'M([4'Y`<
M>>'$/SZ1&2MCF55A^"GJ(DF%KYC@X<_0R8_0BZ^>OGCVTQ]3]2U+)^3$9;I<
MT\67[KWPY$%ZVJV6=MOJSOG]I@4^>XW7*M"\S+`PNI<ZN[F">*[*F&*?!$YS
M[J^2F3>)SG<1^HM`3OPOM]4V[O_M=\K[OPJ!6E\)_P4QI6R1K9X])W^JVF11
M`EJQ>%92H*T+]E<*924B66T,LT7"D&V(C24%VA*)<?+T_)F\[XJ?;S'>XB'%
M"X#&'I5>2-EPQD3_C-CMIM*K85XA_&H]I1@E"4X4R6VR86JM"(OV?GBV4HNN
M*T\-!=5G4@0Q^>F3HHB5\+F"TTS\^?10,GZ]A?BO7?W^MWZOWRKY?Q%@&WVX
M`N*Q%X)T3+^`N'T?4MXO@[Y^8>`T>81*NM+MXAJX7/KOZ_&?>X/R_N=BX+KW
MO],[WYD`!DE?/OWA^[>O_NOGI^@#<`A(;PW[3B477$9Y1>1=`O/^A\Z-2P"Y
M\9\'.OT/W$X9_[T0J#U2]G]:N&5K9&/[A%%C&V_>OLFAC?G>38YLW++L2;)B
M4U\3LZX%,[B,@OT4W(SCQS=I^O8,>)W`4LU%<'U4L<2M/MD:S8?71/.AV%;[
MYP)'/96)*-#_^'(\]X.05I7<ROK?-?4__?+^IT)@B_M?8>&?TLM:Z->S>JM!
MKEJ5WLWHNYG\;DK?E1>[WV%PFIAC`#,#W=N.%,!Y^E^W[^K[_U9Y_ULQ@%<.
M>?T?Y"VQ^FS)N?_!>M6"N/_!T(N*3S9]J+3DFFL90'K9A*P"E<N45T=S?;27
MVA"K)V^"53N=5I[JH^EM>:(D3_PZ,3M%9`K]5+\KWFH"$D*#7"4NDPKRI+F-
M`VH30^QRG!!`;OR&BA)V"4Z3ZGWH>>W-RWX`^??_M77YK]4O[_\I!&3]3X4I
M@+AZ!WWX@"26PX&>?SGTW*F>GNN;29+U",^N>NO`W91J'(5C;U6OHH5?M:2[
M8A<)WG9/?9F`]W]@)^<LY[NK8_/]/RV@?(W^.YU!IZ3_(H#=_T,B8XE;@(3A
M]>'A(7H,\X/87($94'J#`5A+@>//61RL+M$A\T^D/@+U9S^!)S\-J5<E;D)@
M=K5_+XT@_PVX@%72FUR:_U-WOOY#XZB9NE]-P6H*+`9)X,CZH8^J^U-B&L6M
MGQX\8%'[@G`\7T]P#5,:H9S$^B6V?>$9L>X'BT-F!\E+(A$B<THCA1%//!([
M&#D\OJ-<J`]%0-3=^G?$PP#<41M'$/=V^6%_"E'&JD$X#S"O.T+<&>6*]A1$
MAJP_>?8"]Y9C[R?2"^"&C)-"RH;<:?`1[#GKKTE4Z\:O^`>QAF[LI_U(`],Q
M7'AT/JG--&S`E'@Z>'$"1WDC,#U>Q5[H1^O$84VEB&M5OX8>>3UY(`\;&;?I
M$?L)SKTB5?-(P><F$+I*X]%12[5Z$L7XWW?^98+^#ZY']-C^>`$VT=78G]*(
M6,P2&7?OE`6\(6$NR`L6<(J]H`;,S%!-"@M.W=O0WZ#HOTG!_LE[T339^4AN
MJ"B.1*T$YQ<:M!)73WT]>=BK]!(:VF`;>1*WE`WN1)0\=?K/TO_?)(_)E?]<
M7?[K=UKE^5\A\#'W/S>&TJ$?V99O<=FSF@GO.J63PU))>&O@-+T0]#4!7NB(
MV?4$QO-F%8&Y^O^!3O^#?K^D_T(@<_2'W2&S^<%"$;$%8C>U$=M\N$8P1")K
M:1;TN8+3)#=[$\:-1SWV=W`(D'O^W]/W?WBW6-)_(5`;7/O\WSIA"CC^SSV;
MR$>L:QQ0<)V\:E'>W0XM?A`@'WA8RQ/*?<V<O%H594J6W;6^4>S86BS7K[\=
MX<V?*-0599[RU]EJ]PWK_XW-L?SUWZ3_7DG_A<!U[?]@=@Q/T#>'1(0_@2TO
M-0FD\[<A9C+Z<`&IJAX:19=$:(![//QJJ<V]4P#VO\$*(A7</-USR+?_T>F_
MU^F7]G^%@$3_8M/^[#FF](LA?8MI&&;&D?5PB,0I8I\U8,<VM]V^$C8#]?^!
MT_^1-Z$^Y87;_[8-^[]!NUWZ?Q8"]M&'S?^S$,*+X56;11J`5%@(P#M_*M?#
MNI_>MC2.0K!V(>%P?,Q4](Q"V5>J"NX4.$V8V'BX=N7\_56^_9])_SW7+<]_
M"X&:V[ZV`D">,07L^SMY^_XL?$CN;L5BB+]U;K>M9G^\?6>8FWBN:^#6?^G^
M?;,S_"!3RW#*2QIL5Y)K&R'6`EYDJDK@R7$&_Q]BE&IN2_PVC313&\>A:*=<
MDBW/2,IS*C#?0GE1PB>#TYRLE]1.];;B?[3:AOZWUR_]/XL!;?2'G2'ZGCS#
M%4`>YA-^C"X@;.AB/5\%R_DEO_79J7Q"UE($O"L`YS\38FHTBB:7MZ3_Z0\,
M_8];[O\*@>OJ?[FB-U7\@KL72K5"U&QM%:_9E2H7PE`8+?&*7JI_[Q:D]`_W
M%]X2_??T^"^]]J#T_RH$=D3_%Y3X6R7-WW&0XK_(7F`W6D<>_;>,^"_]=AG_
MJ1BPG?_(AIIVEG#;6)=P4\#LOW;E^D<@7_[7_;\[9?RG@B#'_Z_GC*/E9;UQ
ME#KKT4<0\,6#<?AK.@FB*U0>"-]!D.E_-Z<_^><_G:Z^_G=[O7+]+P1J7>7X
M1SLMR;;_S`S^L%W(7A$"0@O9VTZ/"2Q!<Z7/1D`$A'K2DQKG%LXXEI?2HQP@
MMU&Q9[H6+A\9)GA+M+1\U\),,0T%<!643M7/6R*DY)(#;-C0Z=Q`1QE!CO-'
ML`PZL0TXS9]/NH?DG@T:77\'=1`;_VN=_W3[;BG_%0+,_SOU_-X_BU;TVA5R
M.Y%PUV:7=LH>V\[7P]>3KX>JI_$]40"Y[Y'?]"O*Y#?`$!?6(_-V/EX6^TX_
MEK<\[`JX_4^01.&N#H!SY;]^UXS_5=)_(5#K;9+_LFQMTMEBNP$B*RZ6N/LA
M-RZ6;(0B<F6+BG-9U-LL)/:E)UU(W)QSD)E3EX=DS"3#F3N&&HSDC4B-RJ-B
MMV/&;-U@3Y8QIW)N`K%%4"M#D&T/QOT_.Z@CW_]+U__U6F7\YV)@D__',R*`
M9;E]X'F#+L1>#5R]".W"/7T7DM[/T`V6CB%W"9PF'K[QNTMOQ;?NQ9__F?N_
M?KN4_XJ!CXO_0KXC=W/8EQ&/^\+\0:LS?SZ/JDJ>QR3A\!1O!,^5LD??G3KT
M11D=9I?@-,&L?.1-P`UT1P)`_OIOV/^X_=+_HQ"PTS]?ZUL*!W@<G2,1$701
M12*A0M$SO)SXH;B^-(G6X20M$LL).&<I`=P5H/3/8L#MP/8'()?^.P;]=\KX
M3\7`-==_3+O#DTV+/B0X+>W]/AL@]K^Q3STS@IU<_[B%_:\1_[%;RO_%@&7T
MP?G[!7T7$`^=:(K(F3^B)(^90^G!\Z6`TYRO1R+X_ZWX?[?:;2/^O]LM]7^%
M@#SZ0_=A1MB'_Y;"/LRV#_IP6O*)$DHHH8022BBAA!)***&$$DHHH83;AO\?
(^RF"BP#``P``
`
end
