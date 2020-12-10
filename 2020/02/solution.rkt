#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList

         ;; "../../eff-parse.rkt"
         ;; "parser.rkt"
         "regex-parser.rkt"
         )

;; A policy is
;; (list 'policy Range Letter)
;;
;; where a Range is
;; (list 'range Nat(Lower) Nat(Higher))
;; denotes an inclusive range

(def/copat (! valid-password-a)
  [((list 'policy (list 'range lo hi) c) pass)
   [occs <- (! CBN
               (~! CBV (~! string->list pass) % v> colist<-list % v$) % n>
               (~! cl-filter (~! equal? c)) % n>
               cl-length % n$)]
   (! and (~! <= lo occs) (~! <= occs hi))])

(def/copat (! nth l)
  [(1) (! car l)]
  [(n) [n <- (! - n 1)]
       [l <- (! cdr l)]
       (! nth l n)])

(def-thunk (! xor b1 b2)
  (cond [(! b1) (! <<v not 'o b2)]
        [else   (! b2)]))

(def/copat (! valid-password-b)
  [((list 'policy (list 'range lo hi) c) pass)
   [pass-l <- (! string->list pass)]
   [lo-c <- (! nth pass-l lo)]
   [hi-c <- (! nth pass-l hi)]
   (! xor (~! equal? lo-c c) (~! equal? hi-c c))])

(def-thunk (! count-valid-passwords valid? c)
  (! CBN
     c % n>
     (~! cl-filter (~! apply valid?)) % n>
     cl-length % n$))

(define sample-input "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
")

(define full-input
  "1-9 x: xwjgxtmrzxzmkx
4-6 r: rrrkrgr
4-5 v: vvfvvvn
5-16 m: pxmrtmbmqmcldmmm
15-16 s: bsshsszslssssslqdsss
10-12 g: gggggggggzgvg
2-7 n: dntnrng
11-14 j: xrjflbmjszzjbjjh
2-6 r: frxrrrfjnmr
6-7 h: hplhgcsphh
4-5 w: wwwwz
1-6 g: ggdggnggg
3-4 c: cccc
5-8 k: kjgmkkfwxkwqkkgfnv
14-15 h: xpwhdjhhjhrdjkhfh
6-7 g: vgggggdhgsp
1-3 r: rtdcrthphrkzxh
15-16 j: jjjjjjstjjjjhjjjjj
8-10 k: kkkkpkkqkv
1-4 s: sssssj
2-3 d: hdbmbpswddwkkr
6-7 s: ssscssnss
8-9 z: zzzzzzzftz
7-8 t: glwvkgtn
3-10 n: nnnnnnnnnnn
5-7 z: lzzzzfhj
8-9 l: llllllltn
1-2 p: dpcppp
2-5 d: hcfdltbgt
13-16 r: rrdrrqrrrrrsbrrr
7-16 l: lllltllllllllllllrll
9-14 z: vzzzpzfzdzzzzzzfczz
3-5 f: ffffb
6-8 s: csssssjrbdsgs
9-18 r: rrrrrrbrrrjzdrrrcrr
2-5 d: fdzxdj
13-14 c: cclcccccccccmc
9-10 z: zzvszszjbnzzz
5-9 s: ssqshwsss
4-11 x: hxxxxjvdxcqplp
4-6 t: mxtrtttttttttt
3-8 w: wmwwwnbmtw
5-6 x: xsczxd
2-7 w: pwvtgkwwwrpjr
7-9 w: wwwwwwwwj
3-9 h: hhhhbhhhschhh
1-4 p: tlwx
4-5 w: lhzjwwwwmv
6-7 p: ppppglprp
2-16 m: jqmmmmmzmmmmmjmbxmw
8-10 g: zhggpgrrlctggg
7-18 z: pltbcznlvtzgzczzchbz
10-11 t: jttttgtttttt
3-8 k: kkkkkkkkkkk
19-20 x: xxxxxxxxxxxxxxxxxxwr
9-16 l: llllllflllllxpllzl
10-12 m: mmjmmmmmmvms
1-4 c: cdxvxczcc
7-10 n: nnnnfnvpncn
10-17 z: zszzzrzczxzfzzzzlz
4-9 k: kkkdkkkkfk
12-13 m: mmgmmqfgmmtmm
4-6 s: tdmmcs
10-16 n: nkfpnncncnnntmtc
2-3 w: vwhwf
7-17 w: wwfwwwqhwwwwcwwww
5-8 q: tkqrtqqsqkwq
9-10 p: ptpppppppd
1-5 f: cnncnmnf
2-9 k: kkjdsnsqkgprtqb
9-11 n: nplnnnnnnnmnn
4-6 r: rrkrzrr
3-5 z: zzzzsq
17-18 r: rrrrrrrrrrrrrrrrrx
5-8 c: rcccdcclc
1-9 c: vcccccccbc
9-10 x: xxxxcbxxxm
2-5 c: cccfn
8-9 c: cccccnctc
4-8 l: flrhfzwllm
1-5 l: fllfllrklx
5-10 j: jrjkrjjjjxgjqj
5-10 v: dsxvmvvjtsc
4-6 v: bsqvdghvnzdfvjcfvvv
6-11 m: mmmmmmmmmmpm
6-9 q: qqgqkcccsgqqjspj
2-5 n: nvsrc
14-15 n: nnnnnnnnnnnbnms
3-4 m: mmbm
3-11 h: hhbhhhhhhhhhhhhhhhh
7-9 k: kkkkbknkjv
11-15 g: ggvgggfgbgvvzjgxghgg
3-4 z: zzzz
2-5 w: wwwhww
9-14 j: jjjjjjjjtjjjjw
2-4 j: jjbljsj
6-9 w: bwtkbttwqvwk
1-3 l: lllpjl
9-10 g: ggfmggggsrggggg
3-5 t: ttwzxl
5-7 k: kkwtbckkk
9-11 r: rbgrrhrrrrrr
9-12 g: ggggggsggglnggg
1-5 b: bbbwjs
6-7 m: xkjmdmm
6-8 g: gggggwgw
12-15 v: vrvvvqvvpvzvvvvrhqv
15-17 s: hsnjsrsdxpjswsxsss
9-12 r: rnhnrrsrrrrr
1-3 d: ddnd
1-11 m: mmtxkcdrvcmx
1-18 k: kxkkkskkkkkktkkmkc
5-9 h: vfxhhshhvhhbhr
2-3 k: wtrcckttcqrj
2-7 q: wnqcqqxw
1-9 s: sqsssslsssssssssss
4-6 p: mwvpppzp
1-4 v: qlvnvv
10-11 z: lzzzzzzzznt
2-14 q: qwqqqqqqqqqqqfqqr
15-20 d: dddddzddddddddlddddt
11-15 m: fvsqwbnqmbnmgcm
3-4 c: fncnfdjzcxwbpcrn
4-7 z: jsxzzszzqtwzmcznfs
5-6 x: glxxxxrxfxxwl
5-13 b: pdbvhwjwnbdwbrbbjwf
7-8 h: hhthhhvvt
8-11 f: pfffffxffpbf
5-6 n: ndtlxnnnqdmcnv
10-12 s: sssssswssdshssss
15-16 s: ssssssssssqsssss
4-5 s: sssss
3-4 g: ggggg
3-4 c: ccjv
7-12 v: rrrngfwhslsbpvbmwnn
4-10 n: nxnbnnnbxnnn
7-15 d: fggdddpgslqgdwhdndw
6-10 h: hhzzghhfdchhjnm
8-9 c: cccccccnc
9-10 h: hhhhhhhhll
2-16 w: nwfwwwzwvrbmwwww
10-13 k: kkkksjkkskqtkkmkknks
4-9 f: fffwfffbkc
4-11 v: gcfkvvvvfvv
2-5 l: llflftlmlglc
7-10 l: llldljllgsbl
10-19 g: svcsnlshppvrxzghhzg
3-8 d: dddwdzvmddd
3-5 m: kpmmjpmmmdz
13-14 n: nnnnnnnnnnnnns
3-8 b: bkbbbbbbbbbbbbbd
2-4 g: jghfggh
8-11 m: mmmmmmrmmmm
9-12 j: pbjjpjnjxjjfsjzsjps
6-7 r: rrrrrrrr
8-13 v: vvvvvvvbbvvvvvw
4-17 p: tpppppprzpspbpplppr
2-3 h: wbhhzxhllh
2-15 c: nvbcckcpccrtccwccsc
12-13 j: jjjjjjjjjjjzhj
4-10 p: dznzpnxzppl
5-8 r: jrrrsrrrrr
7-10 q: gjqjcqqqqsqfqqq
8-12 k: kkkkkkkkkkkg
3-14 m: mmtmmmmmmmmmmmmmm
3-12 t: khgttqtcltshttwqgt
14-15 b: bbbbbbbbbbbbbqb
14-15 f: fffqfmffpnffffg
5-16 j: jlghwmmrbdvdfjbj
7-8 v: vvkbvvrv
2-11 m: qmcgxcshgwmfm
11-12 m: cmmmzfmcmmzmmmmjmr
6-7 n: hnnbnnnw
4-8 m: mvdtmstm
4-7 m: bcrmmjbpmmmsdnrmm
1-4 w: wwwww
12-14 j: jjjjjjjjjjjrjhj
2-9 h: ghhczbhblx
2-11 v: vmvvvvvggvjpvchvvpc
6-7 f: ffwfflghpwfl
3-14 k: krkmjkgkzcckjbkkk
1-2 c: cfcp
15-17 z: ztzzzdzzzzzzzzzzt
4-13 p: bmrpdwphzqvppktz
4-5 j: jjvjtj
14-20 h: hvchvhhhhbhthlhhhhhh
1-3 j: kqjp
9-13 x: fsxxxkxxbpkxmhlrdtfx
13-14 w: wwpxwwwwwwgtwwcwwww
2-5 p: wppxkq
13-15 x: xxxzxxxbxxxxxxvxs
5-11 q: qvqqqqxqqqqfqqq
8-12 q: qqqqqqqvqqqq
8-10 m: mmmmmmmjmn
17-18 t: ttttrttttsttttttttt
9-13 h: hhhhhqhjlvhhqhhwh
8-9 s: ssssshsst
9-10 v: qvvtvsvcmzvvnpgvv
7-12 d: djcpndppsdddfdsdd
2-3 z: dgzwvbrzzbgw
1-4 k: kwkl
1-9 d: dkdmdsdwdzvlpv
10-11 h: hhhhhhhhhhh
9-10 f: ffffffffff
4-7 d: ssddddxp
18-19 j: jjjjjjjjjjjjjjjjjsj
7-11 m: mkmmrmmlrzsmgm
1-7 k: kkkkkkknk
4-5 w: wwwwj
6-13 s: xslmsfgsssssmslsstqd
4-5 z: zzzch
9-11 j: jjjjjjjjjjj
4-7 h: hjhhkdh
5-14 v: vvvvglvvdvvvvv
5-6 q: shqnjdqqq
1-5 q: dqqqq
5-7 z: zzznzzz
5-9 s: vwsfsxssd
4-5 f: sffffv
5-7 b: xllbbdb
1-14 j: rtzjqkpnkjlrhjcjqj
8-10 w: wwwwwwwfwxz
3-6 g: sggtlggswf
11-14 n: nnbnnncnnsgnnmknn
6-15 c: cclcnccctcrcxqmd
5-8 l: lmllxwpl
4-11 p: pppppppcpkdp
1-4 w: zwwx
1-3 g: nlfpgfzqkzn
11-12 w: vbwwgwvwrwlwwmwwwjw
4-12 j: vzfjgjrkzdrxqfh
1-2 r: frrq
1-4 k: vkkkkk
11-13 d: pndlftgdpdhld
1-4 r: prrvrrrrfrrrrr
8-9 m: fmmxmmmmmmm
1-2 x: xcpxc
4-16 n: rhbndvjnnsnfnwnnm
2-10 g: fkzwxtqmgm
11-19 p: bpppqpppppplpppphpsr
9-13 g: ggggthggggpjgqgslggq
4-5 m: mzmmxcfwmdpz
2-13 t: tttttttptttts
3-7 q: qqvqqqcqq
6-9 j: jsjjjjccjwjcjjj
7-10 r: rdlrrrrrrr
9-12 r: wrrzsrcrgcbrhqvrhlp
6-7 j: jjxjjzhxj
6-7 m: mmmmmmmm
1-3 m: mldvdmxmbk
10-11 f: ffffnffffkqf
5-17 f: ffffsfffftfffkffn
4-5 b: fbwbhvx
14-16 g: ggggggggthgggggsvd
2-3 s: cvsbw
5-10 g: qltgnfghgcrgpnzdsvng
5-6 p: ppjbpnppxg
3-8 j: lgjrzwdvdnwvrnj
2-4 c: cdcc
6-7 l: llflrhlllgklh
1-2 p: tppp
16-17 g: gggggggvggggggggg
3-14 f: fhfdffftfbggpkfgf
16-18 b: bbbbbbbbbbbbbbbbbbbb
5-10 v: thqvvkvlwvdvjzzst
13-14 p: ppppppppppppptc
11-14 t: tkttczttttsttmcnltm
3-4 w: nvxl
4-6 f: fpflfwftk
4-6 h: xhhhhh
15-17 t: tttttttttttttttlv
10-17 j: jjjjjjjjjsjjjjjjd
2-4 s: jcgs
7-9 q: qsqqjhqndsqkqqpvqqr
2-3 p: tnpplcmdpfn
2-5 c: crvmk
1-7 d: tsmzcgddnddddrdxsq
8-9 c: rlfcpctpncv
6-9 f: tkzfhffdf
13-15 s: sssmsscfvsssdsg
6-8 s: ssjsssjrscsss
7-10 f: fffffffkfff
1-6 k: jqtdhpknkkk
11-12 t: tttttttttttf
9-16 d: swjbddfpfbntmprdd
18-19 v: dvvvvvvvvvvvvvvvkvvv
2-5 j: kjjcqjj
11-12 d: dddddddcddld
14-16 f: fffffffffffffffk
4-7 x: nxxxxxxn
10-18 p: pppppppppppppppppr
8-9 k: ktkjkkkkfvklkz
9-15 h: hhhhhhhhchhhhhhhhh
18-19 v: hvkvvvvvvvvvvvvvvqc
1-8 s: xzbxgsxshgl
2-6 p: ppsjpglptchpzbdhj
6-7 n: nnnnnnnc
2-7 j: jhtjjjjj
11-12 b: vnblqbxkbbwb
4-5 z: czzzcztsz
4-9 x: wxpvxxxfxfxx
9-12 w: wwwwwwwwwgwwww
2-5 k: cntkkq
10-12 l: vljplljlllllllll
1-4 t: tfmvst
6-7 d: rddddddwd
2-6 h: bhvwhl
5-8 c: ccsgccncwgjtcbhccckk
5-6 r: rrrrmr
3-8 h: hcrhhjrbhh
2-4 s: xndshswk
2-7 m: mmrqmmlp
7-9 n: nnnbfpmnfnknxnndn
6-7 f: fvffffb
6-8 h: vhhhhhtk
6-7 j: jjjjjwjjs
1-5 r: rwrrrr
3-4 t: tlzk
2-7 k: kkkkkkk
6-11 q: rqqqqqwflqqjqq
5-7 l: llllglll
6-14 r: rrkrrxrmrrrrrc
1-8 w: lrwpzvhhcwtbrwwpwww
3-13 b: kbjhskpbrwqpbcbbkb
6-9 d: ddddbddhdddddddddmg
14-15 n: nnnnnnnntnnnnhf
2-18 k: cgjpktthdbfxrclqwzpl
5-14 w: wwwwwbfwzwcwcwz
5-7 s: tfhssgrsnbssfl
1-18 l: wdkwkdlslbllllgzfm
1-17 h: jhhhhhhhhhhhthhhhh
7-13 w: wwvwwwwwrwwww
1-3 r: rgrdrx
9-11 t: dnxckkwpttv
2-12 c: cgqxrszcckcd
4-13 q: qkchpbqqtrtdztrq
2-9 d: ddddsddddd
9-19 p: pppppgppppqpppppppp
8-9 l: lllhlllll
7-8 h: hhhhhkhvwbhmh
11-12 w: wwwwwwwwwwwnwwww
2-3 d: jhcd
13-16 n: nnnnnnnnnnnnndnnmnn
6-7 d: dllpddkdd
6-7 n: nnnnnkp
7-14 v: vvbcvrvvkmvvsv
5-7 j: jjjjcjs
4-10 l: ldllllllll
4-5 x: rxxxx
15-16 q: qqqqqqqqqqqlqqqw
13-15 m: mmmmmmmmmbmmzmbm
7-10 k: skkkkkkkkq
9-15 m: mghxpjzqdhjcbmdl
2-4 t: ttsnwzbdj
6-7 s: hqtkgtr
3-4 n: twpknn
1-3 l: wrldlsdl
5-6 l: qlxncf
4-5 s: dvsxs
7-9 t: rktsrfwttttgwtzgft
1-3 l: flsf
6-10 c: cchczccccj
1-3 k: bktw
2-4 h: zdqxgrfkfhljhqhkgmfc
16-17 s: sssssssssssssssvs
4-8 p: plpfmnhsppnpwnx
5-10 k: kkkkknkqkk
4-11 s: nsqnssssvqsm
6-14 j: jjjzjwczkjjjxjjcjjxj
3-7 t: tnnbjjhtd
7-8 l: llllllll
4-11 z: cwsvrtszxrgh
2-11 x: sxcxxxjxrxkxdxx
11-15 m: mmxmbxmmdmmmmfmmfmm
10-13 l: tllllwzlzlrbllllhnp
5-6 k: kkkjkmkkkg
4-7 f: wqqfmffffkp
1-9 b: bbbbbmbbbbhbbdzbbtn
10-12 h: hhhhdwbhhqrhhhdnxhr
1-8 r: qfpnfrrrrz
16-20 b: bbbbbbbbbbbbbbbbbbbb
1-5 c: hwqccmwcctcnclcb
6-13 c: clccccqqcccjczpgj
13-14 j: jjjjjjjjjjjjjh
2-4 j: jkfp
2-7 d: qqxdcdjqg
5-6 r: prrrrd
7-8 l: llllllll
2-3 w: nwlm
9-12 v: vnknxncvgvrww
6-16 s: sjnwfsksnszcglxs
5-17 z: zzmkzzzzzntzjzbwpdjz
12-17 j: ljjjjjjhjjwtjjjjdjwj
8-10 s: dnnjlfkxls
2-6 m: jmmqmdkjm
9-11 m: mmmcmmmfsmmm
14-16 p: ppppptpppphrpxpppp
4-15 d: dfkfxdddldddddddlddp
12-14 q: qqqqqqqqqqqwwgq
1-6 g: tgggggmggggdg
10-18 w: wwwwwwwwwbwjwwwwwt
8-9 t: ttkttcttsrgrzlftpt
3-5 j: jjjjjjjjjjjjjl
17-18 c: ccccccczcccccccccs
4-9 d: hjddddddddm
2-4 x: lxjfp
11-12 v: vvvvvvvvvvjv
1-4 z: fmzblzvzm
4-5 h: hhshhh
3-6 t: ptvrtt
7-15 k: kkkjzkckgckskpkk
12-15 w: wwwwwwwwwwwftwdw
3-5 z: jrhns
5-6 n: pxmnzn
1-3 b: bbbkbbbbbbwb
1-10 r: rrjrqrrwrrrqv
11-12 w: wwwwwwwwwwwwwww
5-6 j: jsjjjp
6-11 h: lhbbvmhwrhh
18-19 q: qqqqqqqcxqqqqqqqqxwq
17-18 l: llllllllllllllllxql
3-4 j: jjljjn
8-10 z: zzzzwnzlzggz
6-7 t: tttttft
11-12 f: ffffffffffnf
2-8 b: hqbbbbbsbb
4-5 w: stljwbxpw
1-2 z: grxfpw
6-8 w: wwwwpwwxw
8-14 z: zqzzzzzvzzzzzzgz
4-11 b: cckgrcmbpvbbw
2-9 d: xqxbdddjds
3-6 x: xxrxxx
15-16 n: nnnnnnnnnnnnnndnn
6-11 x: lqmxxxsxxfxxxxlx
1-12 s: vssssshssmsk
1-16 m: bmgkkmwmmzvmmmmmwflv
3-15 t: thkvttsltnbgztt
9-17 z: zzzzzzzzzzzzzzzzzz
4-6 f: fjwfff
3-9 d: ddddddddd
6-10 k: kjkbkkckxkknqkkdc
6-7 n: nsnckwnmn
7-13 t: ptbtjqttwwtftpntdq
7-10 r: wrhhrrrrrwrrrrvqrms
2-3 c: zcrwhtccb
5-7 m: nnwmmdmmmccv
3-5 j: rtjjmjrjl
5-6 s: dssjhsdmbksrks
16-18 c: ccccclcccclccccccc
7-16 b: qpzgqxdbssmzptrv
6-8 b: bnsbbgdbggxbbszbm
1-10 h: lrpstqvhhhhf
4-6 s: tvssppwvss
3-4 g: qqqhcd
4-7 c: qxcqzccckxcpcxcw
1-13 m: nbwdmspmmwbbmm
1-2 j: xjrczrk
9-10 w: wwwwwwwwwd
2-4 h: hhhq
7-10 j: bjmrrjsjthjnnjjxjjd
3-7 f: srfwwfvf
3-4 b: nbbzb
3-5 n: nnnnn
7-8 f: fxfnfffzpqffgdf
10-12 k: kkpkkkpnkkmkjbkk
9-10 k: jkkkkkkkwqk
4-6 z: rzzzzs
4-7 v: vvvtvvjq
6-12 z: dzjnzczzpzgzzzx
6-8 s: smsvbkswslssrdfjsc
5-7 b: bwbbbbbbr
1-3 d: kddbsmmbv
3-4 b: bbbb
5-8 r: rqrrdrrr
7-8 m: mmmmmmzm
9-11 q: xvnlfvhxqfql
3-4 w: wfwj
1-3 q: gqqcqg
1-2 q: cqlxrq
6-12 s: swwjjssjsstmqfssdh
2-4 q: bqqq
5-6 c: ccccbscgfc
14-15 x: xxcxxxxxxxxxxxm
6-12 f: fzfchfnrxfrdffhfl
14-17 g: gggggggggggggjgpgg
5-8 n: njkcngznn
10-13 d: dddlqddddvddr
2-4 c: xgcg
2-10 s: sxsnsdmszwmsss
6-12 q: qbcqspqqqkqrq
6-16 n: xnnnnnnnnnnnnnnfnnn
6-12 s: tssnssmsssssssssnhr
1-4 v: vwvvv
8-10 h: rzmhhjvqhht
1-4 t: ttjdjftfctt
1-4 k: nkkz
1-2 q: vkvqkqq
9-19 n: qxmjqxnxnblqnnqfgsrd
15-16 h: hhhhhhhhhhhhhhghz
2-5 g: jggpxdgwjpsrv
12-13 c: xccccpccrcccc
1-13 g: nfgbghqrljstggcgq
14-17 g: gggggggggggggmggggg
4-14 s: sssbsssssssssgs
1-10 d: rcxvxcgddnhwd
4-14 g: vccvdldqdgzltq
8-10 z: zzdczzvrzzzzr
5-10 t: ctgtttftqtrbt
3-5 v: vvrvx
9-10 w: wwwwwwwwwwwww
4-6 c: hgccmcccc
16-18 s: ssssssssssstssscsh
2-7 z: zzflfrtdx
4-10 v: vvlsvvvvkrc
10-16 d: ddddwddddbdgdddwd
3-4 v: gkbqbtrv
3-4 h: cjdqvwhhththvx
3-5 b: bbbkbrb
3-5 m: ptkqdgsmbcmmblwp
4-17 q: mlqqgsqqqfqkcqqxqq
6-17 m: mdvqvmmxlmzmvmmdm
1-3 r: hrrtrrrrrrrrrwlr
3-5 j: jjjjhj
11-12 q: dqpqqqqqqnzlqq
8-11 d: dddnddnddtdddd
3-16 d: dpddxtlqsqldrpddq
7-13 z: szhzmmzzzzmzzzz
6-18 h: mkxxqtbnnjgnvxnhhhc
4-5 n: nknfrhpn
16-17 d: dddddddgdddddddddd
14-16 w: wwwwwwwwwwwwwwwww
2-3 t: rttfdstqpdtg
3-4 s: gghrfxbfshqssj
1-9 j: bjjjjjjjhjjj
1-2 p: xmpgppppppp
9-14 k: vqpxrkmskfpnxq
1-5 x: xxxxx
1-14 p: bsxnwvpkphdppphpkwp
5-12 d: xdddmzddfddc
9-10 v: gvvvvvgvhnvw
2-7 x: xxxxxwgsvmqnkxv
3-15 c: ccscccccwccccccc
5-8 w: nsvgwzdf
7-14 w: wwwwwwwwwwwfwswcw
5-10 r: qrrlwcrrwz
7-12 f: hfnphfpjffstff
2-6 h: hqjhmph
3-13 x: cpxxxzcrbjxxxb
4-8 k: kkkkktkkkkfkf
7-10 g: gggghghsgwf
8-9 s: sswswssggss
6-10 l: qlllljllhlll
8-11 c: cccwccrbcbccc
5-6 x: xxxxxxg
1-3 w: cwpww
8-11 v: vvvvvvvvvwvv
1-2 b: bbgl
7-10 b: zbkbcbbbsbb
13-15 t: tttttgtttttttktttt
12-14 x: xxxxxxxxxxxxxqf
6-7 k: kmrkkwmk
4-5 m: mmxjmdrhzkmxxphjmz
2-3 t: tnvtndw
4-5 x: xxtxxtxzf
1-3 m: nwmglfscmwrjtzp
10-11 c: cnccckcckcwdc
14-19 b: tvbmrqtchwblbqbbqbb
9-10 x: fxxxxxxxjxx
4-5 g: gggggkggx
8-12 k: kkkkkfkpkqkwkkk
2-6 v: chpvkvt
4-10 r: rmbjmcrgrrfrtmblhw
4-6 n: rnnwtn
3-10 v: htclcjhjkdv
10-13 w: wwwhhwwwrwwzww
18-20 w: wwwwwwjwwwwwwwwvwtww
7-8 w: phrwbwwmc
16-17 q: qqqqqqqqqqqqqqqqx
2-5 k: kzknsxkkgtvmwlfrmrg
3-4 b: zbbbb
5-8 v: hfclvvpc
3-15 t: ctfthpmgznjnhgtth
1-6 t: httttq
5-6 w: wwxhgww
3-5 m: fmtnmmm
11-17 n: xnnchnzpsmcslvzcn
5-6 m: mmmmcm
1-7 r: rwsgcgvjmtxwkqtr
6-16 m: mmmmmmmmmmmqmmmmmm
5-10 s: sssggssssjsssmsss
9-14 p: ppfppzppnvmpqdvpp
2-4 l: wclwrlrdvh
6-17 g: gggmkglggwgpgkmglt
5-7 z: zzzbzpz
4-5 q: qbqqlzpqg
10-19 h: hhhhhhhhhrhhqhhhhhhh
6-7 m: nfnxmsmhtmhsmmmmgmsl
2-3 m: mkfmmm
8-10 j: xrjdnjjjnjjcjjjjjj
3-6 x: xwrjxhjxx
6-7 z: zzzzzzzg
8-17 k: kkkkkkkkkkkkkkkkk
6-9 n: rnnxnnbwnsnnwn
6-8 m: mkmmmsmm
4-6 q: qqfxqqq
2-8 s: hxmfbkszk
6-7 r: rrrrrrr
2-5 h: hhqzhhhh
5-15 b: bhbvnrbrbblbbhh
12-13 d: ddddddtdddvhddd
5-6 f: fmfcfwf
4-7 j: njqfxjjjqjjvdj
6-9 g: fggkggzgghrv
9-12 b: vbblbxpfrbjbgpwxb
1-8 s: tzcdrktsqjsxssfsssh
10-14 f: fffffffffcffffffffff
13-17 p: pqpppppcpbwpppnprppk
16-17 n: nnnmnnnnnnnnnnnsj
10-16 k: kkkkkkkkkkkkkkktk
8-11 w: wwwwwwwwwwm
2-5 q: qqqqqq
2-7 m: mmmmmxg
2-9 m: mmvlggdmr
3-9 s: wxdbmlcrlpnzmkfdfs
12-16 z: zlxzgznqqfzsqtwc
6-7 f: flvmfffjffbmgfzfgfp
2-3 g: cgggqgxqgv
2-3 f: rffvf
1-8 h: vhhhhhhvhhvhhhhf
6-12 x: vzdxxxgxtxzxxxqc
4-5 w: rwwwwww
7-10 r: brwrcrrkcsj
6-10 q: jvqqqqqqqmq
5-18 v: lvhvvwvvvvvlvlvvmv
6-9 z: zzvzcwlzslhlzqsf
3-6 x: sdknfxvwtcfcjgmv
8-9 l: lqcwlllsllll
1-4 k: dgklv
2-6 q: qqqptfqqqqq
4-7 m: mrmcmmmm
4-5 r: rrrblg
1-8 c: qcccccvntc
6-7 m: mzbxmmmnhmqcmmqvbmcp
2-4 t: ptvqx
9-10 s: gpssssssgss
4-18 b: bbbbbbbbbbbbbbbbbfb
4-5 f: ffffgf
3-6 l: lslglw
1-5 l: ltzlllsdlmrllkvbztvr
15-19 l: llllllllllllllnllll
10-11 j: jjjjpjjjjjncj
2-3 w: wfwgd
1-7 r: vrrrzrr
3-7 k: kkkjckkkgkbtvm
9-12 k: kkkkkrckcrkk
1-6 b: hpbjbvb
6-8 s: ssssssvss
2-4 x: mpxk
6-10 x: xxxmrwhgxxxjx
7-8 x: xxphxxxxx
6-19 n: mdrlnnpnjndqgnkfwnnn
2-5 q: qjwtbmkhtnxm
15-17 j: xjjjjjbjqjjjjjjqjjjw
5-7 x: pxxxxsxxsxx
3-6 t: ttttrp
8-11 f: fffffffffrkzf
6-8 r: glsprkbp
10-12 m: mmmmmmmmmmmsm
13-14 z: zzszzzszzbzzzplzzz
19-20 c: ccccchwccccccccccncb
7-12 z: qzggglzlhshnrwt
12-14 z: zzzzzzzzhzzwzc
1-2 s: sssl
1-3 q: fsqmwq
1-6 z: zmzztzkzznmflqffz
9-11 b: bwbbjbbbtlbbn
4-6 s: nktszz
2-5 j: nqjhjkb
7-16 l: lrlllllllmllswlsl
2-3 f: fjqvfff
7-15 w: hwxghddqwwwwwww
2-4 z: zgzfx
5-6 h: vhhjhthh
13-14 h: hhhhhhfhhhhhhhphhhh
16-17 k: kkkkkkkkkkkkkkknx
7-15 l: dllllllmlllfllf
2-6 w: mtkwwwww
2-7 k: zpkkgldk
7-9 d: htmtjddddddqvddcd
4-9 v: rvfbbwzdwcvvqv
6-11 m: mmvmbvmmmsmmlmmm
3-8 p: xpxppsgpbxp
2-6 f: vffbszhvvrdfkxc
8-11 v: lvrrlvvqqvr
5-17 h: hdhhbhhzhhhhhhsbl
8-10 f: fhgffgfrfqtf
9-12 s: ssssfssdpssswf
9-12 m: mmmmmmmmpzgmmm
7-9 l: zlmllvwlllldl
8-11 m: mmdmfmmdxbmxbmm
1-4 m: fklssmffwzgqcvdpm
13-14 c: cczcccccccccccc
5-9 f: frtkfpffsfff
5-8 f: ffffgffr
3-8 m: mmxsmmmb
1-6 m: mdmmmm
1-2 h: hhdtmhwfpt
4-14 d: dmrkdvddmdsdddbsgd
6-7 c: cxccgcccc
6-12 v: xcqvjvvsdvvvpv
11-14 d: gltkjvddwjdkrln
2-13 f: ggffllflfbfffqkfmf
10-13 c: xkvcccccrmccccc
17-19 q: qqsqqqqqqqqqqqqqfqmq
8-10 t: ttttlttwtlt
4-5 f: wffrd
2-16 v: tvhvvqvrvvvvvvqvvf
2-4 c: cccc
3-8 z: zrzzpzlzzhsdxqqfx
6-10 k: kkkkkkkkkk
5-7 r: rvrrvrz
14-15 w: wwwwwwwlwwwwncwvwjk
6-11 q: qfhxqqgqqqsvcpr
4-7 f: kflfffzff
2-14 t: rtttttptlxsttt
11-15 t: ttttttttttftbvqttt
6-10 c: cckwlchltchcczgw
3-8 s: ssssvsqnss
1-8 h: mhhhhrhhh
13-15 b: bbbbbbbbbbbbbbnb
15-17 p: dpppppppppspppcps
5-12 l: xbfjglklphtl
2-4 v: vvvvbvwpvvvrv
3-5 k: plhjkkqkskz
6-12 z: zzzzzczzzzzk
2-3 j: xvjljj
6-7 r: rlrrrrrrzrrzrrr
1-12 t: mqrkgpktbsqxg
10-13 z: pzmzzxpnzxpgzzzhzz
5-13 s: gslsskvgxssssnpj
10-14 m: mmmmmmsmmmmmmk
2-5 b: tnbdsfwdzxrkjdb
2-15 h: hnxhzhmhhhhhwwb
1-4 c: ccccccc
9-10 b: bbbbbbbbbl
4-5 j: jsjjj
3-6 n: xmmnppbn
11-12 m: mmmmmmmmmmmm
2-4 r: jrgrcpvgctrrr
3-4 g: gggr
14-17 n: nhnpnnnnznnnnnnnknnm
4-5 k: krkkf
11-17 h: hhdhhhhhhhhghhkhx
5-19 q: dqbcqqqqsqqzqqqqqqv
5-16 t: ttrkttttstwttttqdt
11-18 h: dhhhhhhhhjhtjhhhhhh
5-8 c: ccccrccfc
1-3 v: nvvnv
7-12 m: prmmzmtmmmssdmmmmt
15-16 r: rrrrrrrrrrrrrqfr
7-19 b: djbbbbqjbdtbslbgbbmb
10-13 w: wwwwwwwwwwwwww
5-15 p: mbpcjqmmhpppkfmrbcww
2-4 n: nxbjknms
5-11 s: hxhcsldmxshdksvsss
12-16 q: qqqqqqqpqcdqqtwkqvq
2-7 j: tjlpcjjjjnj
6-7 s: ssssskm
1-4 t: tttttx
5-10 q: pqmnqrlqchbjzqqvq
11-15 q: qqqqqqqqqqhqrqqrqq
3-5 k: jhkgkxkqfskg
14-15 f: ffvffmfffffffsffqm
4-5 x: xxfxhfxx
2-4 b: ktbv
3-4 t: wjtwxvxctrcttb
1-3 s: shssg
8-12 z: skkzbdpzdkzzz
1-3 f: rlfffqft
5-8 s: fsrcnjsgsmgs
7-17 v: vvvvvvvvvvvvvvvvvvv
4-5 k: kklmq
8-17 h: hhhhhlvghwkhrfzhxh
17-18 n: nnnnnnnnnnnnnnnnnn
3-10 h: mhhhhrhznwvhh
8-9 x: xmxxxxxxx
4-5 p: ppppk
11-13 t: tttttttttttjtcn
3-4 q: qqvhz
3-8 l: gbpqclnkwlhdlml
12-13 f: fffffffffffrf
3-11 s: ssjssssssslssss
6-7 b: bbbbbwbb
7-9 r: krrrrrfrj
13-14 b: bbbbbbbbbvbbbp
2-4 x: ltgbx
7-10 r: smrrrrkjrrr
3-7 n: vnqpngjdgrw
7-18 t: ttltqtsktrjgxtqhtt
2-5 v: dvvxbvvvnk
2-5 n: nnnnnpn
1-3 t: nttmtx
3-4 z: zpszz
1-12 p: pmpxphlpppppppqppp
3-4 v: qvvvvvv
9-12 g: gwgggggggtgqgg
2-4 n: knpn
3-13 d: hncbdrvpddddcfddd
5-9 s: ssnspslslsfkpskss
1-2 s: ssczgkvtlp
5-6 p: pmvpzpppkm
1-4 j: jjhr
5-6 s: ssssxl
1-3 t: ttrt
16-18 n: nnpnnnnnnnnnnnnnnnn
4-5 n: nnfnl
1-4 b: bbbb
7-8 l: nljlllkhl
11-12 n: nnnnnsnnnnnnnpn
5-10 k: kkkkckkkkhkkkkk
7-10 s: zfsssfldxsvqsxdsqs
5-6 q: qqqqqp
6-7 q: qqqqqqq
14-16 w: wwdhmwwjwwwwnwwlwwwv
8-9 l: llllllldll
12-14 c: ccrccccccccrcccccccc
1-3 n: qnwnh
1-4 l: mlll
1-3 s: sssssss
4-15 h: mhclhhhjllhhhgh
15-16 x: xxxxxxtxxxxxtxttxx
2-4 r: rmjh
6-9 z: zzzzzpszt
17-18 n: nnwnnnnnnnnnnnnnppnn
8-11 v: vmvmvlvbvvvvv
7-8 b: bbbbbbdf
2-5 k: kkdkk
8-9 d: dddsldddvxlhd
7-8 h: hhhhkhxshxhn
12-14 c: vcchpxcnmgkctpqcc
1-5 w: wwwwww
1-2 x: xdmk
1-9 p: pbfvrdpqqn
2-4 s: mtnvzhnzs
4-6 t: cttxvtttx
4-8 t: qttmtzttkwftrjk
7-9 c: cccccccccc
2-18 j: zjjqxjfqqlmjrjhjqx
2-5 q: qmkqgq
5-11 k: kkkkkkslzkkkkkrx
16-18 h: hhhhhhhhhqhthhhvhh
5-13 h: hhfhhhhhmhhshhhhhhh
10-12 r: rkrrrrrmrrrrrr
8-10 n: rnnckhmjnxmfggsgtnnn
19-20 c: qsccznccccccnccccccq
1-4 k: wtkck
7-8 s: sssmssjssss
3-5 l: llslv
3-7 t: tvgjqgjbvtgpkt
13-15 b: bbbbbbbbqbblrbbb
9-10 z: zzzzzznzzb
6-16 c: kpcnsccgccxfwccl
2-3 d: ddbd
10-16 v: vvvvqrvvsvvvvvvl
3-12 d: dzdxxhddnlkddddk
8-9 b: khbbbfbbbbb
11-12 z: zzzzzzzzzzzz
5-15 n: bnlvnnrjbnqlnnn
1-2 q: mqqpzjc
11-13 h: hhhhhhchhhhjhh
8-11 x: nxfcxbjqxdlcxxxkxxx
14-18 v: vvbqvvvvvvvvvvvvvvvv
7-8 f: pgtlfffmjffnt
10-18 m: mnkrmmmmmqmmmmmmmjm
1-4 l: lghlhcnwprlgvw
3-4 p: npxplpwl
12-15 s: sfkwnvsssvssssfsssrp
3-7 z: ztqnzlxjmzzbjfnxcw
17-18 q: qqqqqqqqqqqqqqqqqq
3-6 z: jczztzzbhmzzw
3-7 f: skffttjffqfnchfrf
4-6 m: hmqmcrmw
10-11 t: dftttqttttttl
7-13 p: spxnpgtzpppjpwppptp
8-9 q: jqxqqqqxqq
3-4 h: hhhh
10-13 z: pgzxhxxpzzssj
5-9 x: xfrdmjxmrxrqjpxbfxxx
8-17 d: ddpddddddddsgddzddn
1-4 g: vggdggggggggggggggg
10-12 m: mmdqsmwvjjmv
2-7 s: ssqsssk
13-14 n: nnvnnnnnnnnnsgnn
2-16 k: kkkkkkkkkkknkkkk
3-6 p: dprphc
2-10 k: mpkstkxkdb
2-3 s: ssxs
9-19 t: ttjttjttrtsqtmttttt
4-5 j: jrjjtsjjd
6-7 x: xxxpxjx
13-15 q: fqfqqqqhqcqfqqbqqq
1-3 z: zfgrzl
6-8 n: nnnnnphm
1-8 b: bbbbbbbbb
4-5 h: thhgj
4-8 v: vvvcvvvp
8-9 t: ktttttttt
3-4 f: wftbcfnmhkfdpjbns
6-7 v: vvvvvlv
16-17 z: zzzzhfzzzzzzkzzzwzzz
1-2 q: qqqfq
10-11 p: zpjzcppvcppppppppj
10-15 c: vlccbcgcclcczfcfcc
11-17 t: tsntktwzhsfttfwtx
1-7 h: nhhhhhb
6-13 b: bbbphnbbwbbbb
8-9 x: xgxxxlxfxx
14-16 m: mmmmmmmmmmmmmmmm
3-4 x: xxbxx
6-9 c: xkccsfccrccv
10-18 h: jphhbshwghrxpnhnlhxh
6-10 f: nlxbgvftfh
3-4 p: ppwf
1-6 s: kjwjwshcnqwxgwslvl
2-3 m: mmlk
2-3 g: gmgg
3-6 r: rrrvrmr
1-6 s: szgpksczqd
12-14 t: ttttttpjvtqwtltt
1-2 r: hrrrrvrrzj
1-13 m: xmmmmmsmmmcsnmmmmfmm
10-11 f: fffffffffsf
13-15 d: ddddddddddddhdq
7-14 k: jkkkkkkkrkmkkkk
6-9 g: ggggggggg
14-16 l: lllllllvlllllwtx
1-3 g: ggggt
3-4 m: mmkd
5-10 g: jgsgvggwggj
17-18 p: ppppppppppppppphpp
1-2 q: qqjwhq
2-6 q: qqgqqzk
11-16 t: ttwttttttctttthgt
1-5 m: zmmmsm
12-16 r: rrrrrrrrrqrgrrrj
4-5 v: xcvvghvrczpcgn
1-3 x: xbfmxxfxxf
3-5 j: hjbjm
5-14 l: lllltlllllqllll
3-4 x: xsxnxgtx
6-18 g: gggggggggggggggggd
8-12 t: tgttttqtcttttt
4-6 p: pppppd
6-7 x: xxxxxxx
3-4 c: fdcclrccccvg
11-12 v: vnvvvvzdvcrk
1-9 z: zztzqzzzzcz
1-12 v: vmvvvvvvvvvmvvvv
9-16 v: vvvvvvvvvvvvvvvf
2-7 h: xhtsfmvthhhrhdhhbbc
5-6 c: qkccccd
8-12 z: zzzzzzzjzzhbzz
2-4 n: shlnnkxn
19-20 v: vvvvvvvvvvvvvvvvvvcv
7-8 h: hhkhhhkhh
5-19 p: sxslgwvkszswqxtqpvf
5-8 m: mmmmmrmbm
2-5 j: jrnjjzjfhjdkjqjtkwk
7-13 f: ffffffsfffffwfff
6-9 w: wmwwdwwwsz
12-15 f: nsffpfsdsjsfsffk
13-16 k: kkkkkkkkkkkkkkhw
6-17 s: ssssslssssssssssss
3-5 c: srdscnncclqqcncsw
9-11 p: pkppppppppp
5-10 q: qqqqqqqqqpqq
12-14 n: nnnnnnnnnnnnnn
6-11 h: hhhhhqhhhhhvhhhh
2-12 f: fgfrpzcstfffffxff
2-12 n: nnxmnmnnnnknrpmnv
6-7 m: jmmmmcnpkm
4-6 v: mvkcwvmvvjvrvlv
18-20 h: hhrmbrhhhhlhhvhmhhhb
9-13 g: ggggggzgggsgfgxg
9-15 m: pmmxmmmmmmmmmmm
1-7 l: zlmsmlxpvvlzv
2-15 g: sslggkdglqgxpgkx
17-18 c: ccccccccccccccccmc
10-11 w: wwwwwpcwwwpwwd
3-5 w: wwwwk
8-9 f: vfbvffsfcf
4-6 r: rrrrrr
2-4 n: nrnknn
1-4 l: lxllllll
10-11 f: qdfwvfffdfvwffgfkfgf
3-4 b: xgxbdqxbfvzrl
2-8 b: pbbkbbgbxr
1-2 d: ddxdnv
4-8 d: dndfcnhd
")

(define parsed-input
  '(((policy (range 1 9) #\x) "xwjgxtmrzxzmkx")
  ((policy (range 4 6) #\r) "rrrkrgr")
  ((policy (range 4 5) #\v) "vvfvvvn")
  ((policy (range 5 16) #\m) "pxmrtmbmqmcldmmm")
  ((policy (range 15 16) #\s) "bsshsszslssssslqdsss")
  ((policy (range 10 12) #\g) "gggggggggzgvg")
  ((policy (range 2 7) #\n) "dntnrng")
  ((policy (range 11 14) #\j) "xrjflbmjszzjbjjh")
  ((policy (range 2 6) #\r) "frxrrrfjnmr")
  ((policy (range 6 7) #\h) "hplhgcsphh")
  ((policy (range 4 5) #\w) "wwwwz")
  ((policy (range 1 6) #\g) "ggdggnggg")
  ((policy (range 3 4) #\c) "cccc")
  ((policy (range 5 8) #\k) "kjgmkkfwxkwqkkgfnv")
  ((policy (range 14 15) #\h) "xpwhdjhhjhrdjkhfh")
  ((policy (range 6 7) #\g) "vgggggdhgsp")
  ((policy (range 1 3) #\r) "rtdcrthphrkzxh")
  ((policy (range 15 16) #\j) "jjjjjjstjjjjhjjjjj")
  ((policy (range 8 10) #\k) "kkkkpkkqkv")
  ((policy (range 1 4) #\s) "sssssj")
  ((policy (range 2 3) #\d) "hdbmbpswddwkkr")
  ((policy (range 6 7) #\s) "ssscssnss")
  ((policy (range 8 9) #\z) "zzzzzzzftz")
  ((policy (range 7 8) #\t) "glwvkgtn")
  ((policy (range 3 10) #\n) "nnnnnnnnnnn")
  ((policy (range 5 7) #\z) "lzzzzfhj")
  ((policy (range 8 9) #\l) "llllllltn")
  ((policy (range 1 2) #\p) "dpcppp")
  ((policy (range 2 5) #\d) "hcfdltbgt")
  ((policy (range 13 16) #\r) "rrdrrqrrrrrsbrrr")
  ((policy (range 7 16) #\l) "lllltllllllllllllrll")
  ((policy (range 9 14) #\z) "vzzzpzfzdzzzzzzfczz")
  ((policy (range 3 5) #\f) "ffffb")
  ((policy (range 6 8) #\s) "csssssjrbdsgs")
  ((policy (range 9 18) #\r) "rrrrrrbrrrjzdrrrcrr")
  ((policy (range 2 5) #\d) "fdzxdj")
  ((policy (range 13 14) #\c) "cclcccccccccmc")
  ((policy (range 9 10) #\z) "zzvszszjbnzzz")
  ((policy (range 5 9) #\s) "ssqshwsss")
  ((policy (range 4 11) #\x) "hxxxxjvdxcqplp")
  ((policy (range 4 6) #\t) "mxtrtttttttttt")
  ((policy (range 3 8) #\w) "wmwwwnbmtw")
  ((policy (range 5 6) #\x) "xsczxd")
  ((policy (range 2 7) #\w) "pwvtgkwwwrpjr")
  ((policy (range 7 9) #\w) "wwwwwwwwj")
  ((policy (range 3 9) #\h) "hhhhbhhhschhh")
  ((policy (range 1 4) #\p) "tlwx")
  ((policy (range 4 5) #\w) "lhzjwwwwmv")
  ((policy (range 6 7) #\p) "ppppglprp")
  ((policy (range 2 16) #\m) "jqmmmmmzmmmmmjmbxmw")
  ((policy (range 8 10) #\g) "zhggpgrrlctggg")
  ((policy (range 7 18) #\z) "pltbcznlvtzgzczzchbz")
  ((policy (range 10 11) #\t) "jttttgtttttt")
  ((policy (range 3 8) #\k) "kkkkkkkkkkk")
  ((policy (range 19 20) #\x) "xxxxxxxxxxxxxxxxxxwr")
  ((policy (range 9 16) #\l) "llllllflllllxpllzl")
  ((policy (range 10 12) #\m) "mmjmmmmmmvms")
  ((policy (range 1 4) #\c) "cdxvxczcc")
  ((policy (range 7 10) #\n) "nnnnfnvpncn")
  ((policy (range 10 17) #\z) "zszzzrzczxzfzzzzlz")
  ((policy (range 4 9) #\k) "kkkdkkkkfk")
  ((policy (range 12 13) #\m) "mmgmmqfgmmtmm")
  ((policy (range 4 6) #\s) "tdmmcs")
  ((policy (range 10 16) #\n) "nkfpnncncnnntmtc")
  ((policy (range 2 3) #\w) "vwhwf")
  ((policy (range 7 17) #\w) "wwfwwwqhwwwwcwwww")
  ((policy (range 5 8) #\q) "tkqrtqqsqkwq")
  ((policy (range 9 10) #\p) "ptpppppppd")
  ((policy (range 1 5) #\f) "cnncnmnf")
  ((policy (range 2 9) #\k) "kkjdsnsqkgprtqb")
  ((policy (range 9 11) #\n) "nplnnnnnnnmnn")
  ((policy (range 4 6) #\r) "rrkrzrr")
  ((policy (range 3 5) #\z) "zzzzsq")
  ((policy (range 17 18) #\r) "rrrrrrrrrrrrrrrrrx")
  ((policy (range 5 8) #\c) "rcccdcclc")
  ((policy (range 1 9) #\c) "vcccccccbc")
  ((policy (range 9 10) #\x) "xxxxcbxxxm")
  ((policy (range 2 5) #\c) "cccfn")
  ((policy (range 8 9) #\c) "cccccnctc")
  ((policy (range 4 8) #\l) "flrhfzwllm")
  ((policy (range 1 5) #\l) "fllfllrklx")
  ((policy (range 5 10) #\j) "jrjkrjjjjxgjqj")
  ((policy (range 5 10) #\v) "dsxvmvvjtsc")
  ((policy (range 4 6) #\v) "bsqvdghvnzdfvjcfvvv")
  ((policy (range 6 11) #\m) "mmmmmmmmmmpm")
  ((policy (range 6 9) #\q) "qqgqkcccsgqqjspj")
  ((policy (range 2 5) #\n) "nvsrc")
  ((policy (range 14 15) #\n) "nnnnnnnnnnnbnms")
  ((policy (range 3 4) #\m) "mmbm")
  ((policy (range 3 11) #\h) "hhbhhhhhhhhhhhhhhhh")
  ((policy (range 7 9) #\k) "kkkkbknkjv")
  ((policy (range 11 15) #\g) "ggvgggfgbgvvzjgxghgg")
  ((policy (range 3 4) #\z) "zzzz")
  ((policy (range 2 5) #\w) "wwwhww")
  ((policy (range 9 14) #\j) "jjjjjjjjtjjjjw")
  ((policy (range 2 4) #\j) "jjbljsj")
  ((policy (range 6 9) #\w) "bwtkbttwqvwk")
  ((policy (range 1 3) #\l) "lllpjl")
  ((policy (range 9 10) #\g) "ggfmggggsrggggg")
  ((policy (range 3 5) #\t) "ttwzxl")
  ((policy (range 5 7) #\k) "kkwtbckkk")
  ((policy (range 9 11) #\r) "rbgrrhrrrrrr")
  ((policy (range 9 12) #\g) "ggggggsggglnggg")
  ((policy (range 1 5) #\b) "bbbwjs")
  ((policy (range 6 7) #\m) "xkjmdmm")
  ((policy (range 6 8) #\g) "gggggwgw")
  ((policy (range 12 15) #\v) "vrvvvqvvpvzvvvvrhqv")
  ((policy (range 15 17) #\s) "hsnjsrsdxpjswsxsss")
  ((policy (range 9 12) #\r) "rnhnrrsrrrrr")
  ((policy (range 1 3) #\d) "ddnd")
  ((policy (range 1 11) #\m) "mmtxkcdrvcmx")
  ((policy (range 1 18) #\k) "kxkkkskkkkkktkkmkc")
  ((policy (range 5 9) #\h) "vfxhhshhvhhbhr")
  ((policy (range 2 3) #\k) "wtrcckttcqrj")
  ((policy (range 2 7) #\q) "wnqcqqxw")
  ((policy (range 1 9) #\s) "sqsssslsssssssssss")
  ((policy (range 4 6) #\p) "mwvpppzp")
  ((policy (range 1 4) #\v) "qlvnvv")
  ((policy (range 10 11) #\z) "lzzzzzzzznt")
  ((policy (range 2 14) #\q) "qwqqqqqqqqqqqfqqr")
  ((policy (range 15 20) #\d) "dddddzddddddddlddddt")
  ((policy (range 11 15) #\m) "fvsqwbnqmbnmgcm")
  ((policy (range 3 4) #\c) "fncnfdjzcxwbpcrn")
  ((policy (range 4 7) #\z) "jsxzzszzqtwzmcznfs")
  ((policy (range 5 6) #\x) "glxxxxrxfxxwl")
  ((policy (range 5 13) #\b) "pdbvhwjwnbdwbrbbjwf")
  ((policy (range 7 8) #\h) "hhthhhvvt")
  ((policy (range 8 11) #\f) "pfffffxffpbf")
  ((policy (range 5 6) #\n) "ndtlxnnnqdmcnv")
  ((policy (range 10 12) #\s) "sssssswssdshssss")
  ((policy (range 15 16) #\s) "ssssssssssqsssss")
  ((policy (range 4 5) #\s) "sssss")
  ((policy (range 3 4) #\g) "ggggg")
  ((policy (range 3 4) #\c) "ccjv")
  ((policy (range 7 12) #\v) "rrrngfwhslsbpvbmwnn")
  ((policy (range 4 10) #\n) "nxnbnnnbxnnn")
  ((policy (range 7 15) #\d) "fggdddpgslqgdwhdndw")
  ((policy (range 6 10) #\h) "hhzzghhfdchhjnm")
  ((policy (range 8 9) #\c) "cccccccnc")
  ((policy (range 9 10) #\h) "hhhhhhhhll")
  ((policy (range 2 16) #\w) "nwfwwwzwvrbmwwww")
  ((policy (range 10 13) #\k) "kkkksjkkskqtkkmkknks")
  ((policy (range 4 9) #\f) "fffwfffbkc")
  ((policy (range 4 11) #\v) "gcfkvvvvfvv")
  ((policy (range 2 5) #\l) "llflftlmlglc")
  ((policy (range 7 10) #\l) "llldljllgsbl")
  ((policy (range 10 19) #\g) "svcsnlshppvrxzghhzg")
  ((policy (range 3 8) #\d) "dddwdzvmddd")
  ((policy (range 3 5) #\m) "kpmmjpmmmdz")
  ((policy (range 13 14) #\n) "nnnnnnnnnnnnns")
  ((policy (range 3 8) #\b) "bkbbbbbbbbbbbbbd")
  ((policy (range 2 4) #\g) "jghfggh")
  ((policy (range 8 11) #\m) "mmmmmmrmmmm")
  ((policy (range 9 12) #\j) "pbjjpjnjxjjfsjzsjps")
  ((policy (range 6 7) #\r) "rrrrrrrr")
  ((policy (range 8 13) #\v) "vvvvvvvbbvvvvvw")
  ((policy (range 4 17) #\p) "tpppppprzpspbpplppr")
  ((policy (range 2 3) #\h) "wbhhzxhllh")
  ((policy (range 2 15) #\c) "nvbcckcpccrtccwccsc")
  ((policy (range 12 13) #\j) "jjjjjjjjjjjzhj")
  ((policy (range 4 10) #\p) "dznzpnxzppl")
  ((policy (range 5 8) #\r) "jrrrsrrrrr")
  ((policy (range 7 10) #\q) "gjqjcqqqqsqfqqq")
  ((policy (range 8 12) #\k) "kkkkkkkkkkkg")
  ((policy (range 3 14) #\m) "mmtmmmmmmmmmmmmmm")
  ((policy (range 3 12) #\t) "khgttqtcltshttwqgt")
  ((policy (range 14 15) #\b) "bbbbbbbbbbbbbqb")
  ((policy (range 14 15) #\f) "fffqfmffpnffffg")
  ((policy (range 5 16) #\j) "jlghwmmrbdvdfjbj")
  ((policy (range 7 8) #\v) "vvkbvvrv")
  ((policy (range 2 11) #\m) "qmcgxcshgwmfm")
  ((policy (range 11 12) #\m) "cmmmzfmcmmzmmmmjmr")
  ((policy (range 6 7) #\n) "hnnbnnnw")
  ((policy (range 4 8) #\m) "mvdtmstm")
  ((policy (range 4 7) #\m) "bcrmmjbpmmmsdnrmm")
  ((policy (range 1 4) #\w) "wwwww")
  ((policy (range 12 14) #\j) "jjjjjjjjjjjrjhj")
  ((policy (range 2 9) #\h) "ghhczbhblx")
  ((policy (range 2 11) #\v) "vmvvvvvggvjpvchvvpc")
  ((policy (range 6 7) #\f) "ffwfflghpwfl")
  ((policy (range 3 14) #\k) "krkmjkgkzcckjbkkk")
  ((policy (range 1 2) #\c) "cfcp")
  ((policy (range 15 17) #\z) "ztzzzdzzzzzzzzzzt")
  ((policy (range 4 13) #\p) "bmrpdwphzqvppktz")
  ((policy (range 4 5) #\j) "jjvjtj")
  ((policy (range 14 20) #\h) "hvchvhhhhbhthlhhhhhh")
  ((policy (range 1 3) #\j) "kqjp")
  ((policy (range 9 13) #\x) "fsxxxkxxbpkxmhlrdtfx")
  ((policy (range 13 14) #\w) "wwpxwwwwwwgtwwcwwww")
  ((policy (range 2 5) #\p) "wppxkq")
  ((policy (range 13 15) #\x) "xxxzxxxbxxxxxxvxs")
  ((policy (range 5 11) #\q) "qvqqqqxqqqqfqqq")
  ((policy (range 8 12) #\q) "qqqqqqqvqqqq")
  ((policy (range 8 10) #\m) "mmmmmmmjmn")
  ((policy (range 17 18) #\t) "ttttrttttsttttttttt")
  ((policy (range 9 13) #\h) "hhhhhqhjlvhhqhhwh")
  ((policy (range 8 9) #\s) "ssssshsst")
  ((policy (range 9 10) #\v) "qvvtvsvcmzvvnpgvv")
  ((policy (range 7 12) #\d) "djcpndppsdddfdsdd")
  ((policy (range 2 3) #\z) "dgzwvbrzzbgw")
  ((policy (range 1 4) #\k) "kwkl")
  ((policy (range 1 9) #\d) "dkdmdsdwdzvlpv")
  ((policy (range 10 11) #\h) "hhhhhhhhhhh")
  ((policy (range 9 10) #\f) "ffffffffff")
  ((policy (range 4 7) #\d) "ssddddxp")
  ((policy (range 18 19) #\j) "jjjjjjjjjjjjjjjjjsj")
  ((policy (range 7 11) #\m) "mkmmrmmlrzsmgm")
  ((policy (range 1 7) #\k) "kkkkkkknk")
  ((policy (range 4 5) #\w) "wwwwj")
  ((policy (range 6 13) #\s) "xslmsfgsssssmslsstqd")
  ((policy (range 4 5) #\z) "zzzch")
  ((policy (range 9 11) #\j) "jjjjjjjjjjj")
  ((policy (range 4 7) #\h) "hjhhkdh")
  ((policy (range 5 14) #\v) "vvvvglvvdvvvvv")
  ((policy (range 5 6) #\q) "shqnjdqqq")
  ((policy (range 1 5) #\q) "dqqqq")
  ((policy (range 5 7) #\z) "zzznzzz")
  ((policy (range 5 9) #\s) "vwsfsxssd")
  ((policy (range 4 5) #\f) "sffffv")
  ((policy (range 5 7) #\b) "xllbbdb")
  ((policy (range 1 14) #\j) "rtzjqkpnkjlrhjcjqj")
  ((policy (range 8 10) #\w) "wwwwwwwfwxz")
  ((policy (range 3 6) #\g) "sggtlggswf")
  ((policy (range 11 14) #\n) "nnbnnncnnsgnnmknn")
  ((policy (range 6 15) #\c) "cclcnccctcrcxqmd")
  ((policy (range 5 8) #\l) "lmllxwpl")
  ((policy (range 4 11) #\p) "pppppppcpkdp")
  ((policy (range 1 4) #\w) "zwwx")
  ((policy (range 1 3) #\g) "nlfpgfzqkzn")
  ((policy (range 11 12) #\w) "vbwwgwvwrwlwwmwwwjw")
  ((policy (range 4 12) #\j) "vzfjgjrkzdrxqfh")
  ((policy (range 1 2) #\r) "frrq")
  ((policy (range 1 4) #\k) "vkkkkk")
  ((policy (range 11 13) #\d) "pndlftgdpdhld")
  ((policy (range 1 4) #\r) "prrvrrrrfrrrrr")
  ((policy (range 8 9) #\m) "fmmxmmmmmmm")
  ((policy (range 1 2) #\x) "xcpxc")
  ((policy (range 4 16) #\n) "rhbndvjnnsnfnwnnm")
  ((policy (range 2 10) #\g) "fkzwxtqmgm")
  ((policy (range 11 19) #\p) "bpppqpppppplpppphpsr")
  ((policy (range 9 13) #\g) "ggggthggggpjgqgslggq")
  ((policy (range 4 5) #\m) "mzmmxcfwmdpz")
  ((policy (range 2 13) #\t) "tttttttptttts")
  ((policy (range 3 7) #\q) "qqvqqqcqq")
  ((policy (range 6 9) #\j) "jsjjjjccjwjcjjj")
  ((policy (range 7 10) #\r) "rdlrrrrrrr")
  ((policy (range 9 12) #\r) "wrrzsrcrgcbrhqvrhlp")
  ((policy (range 6 7) #\j) "jjxjjzhxj")
  ((policy (range 6 7) #\m) "mmmmmmmm")
  ((policy (range 1 3) #\m) "mldvdmxmbk")
  ((policy (range 10 11) #\f) "ffffnffffkqf")
  ((policy (range 5 17) #\f) "ffffsfffftfffkffn")
  ((policy (range 4 5) #\b) "fbwbhvx")
  ((policy (range 14 16) #\g) "ggggggggthgggggsvd")
  ((policy (range 2 3) #\s) "cvsbw")
  ((policy (range 5 10) #\g) "qltgnfghgcrgpnzdsvng")
  ((policy (range 5 6) #\p) "ppjbpnppxg")
  ((policy (range 3 8) #\j) "lgjrzwdvdnwvrnj")
  ((policy (range 2 4) #\c) "cdcc")
  ((policy (range 6 7) #\l) "llflrhlllgklh")
  ((policy (range 1 2) #\p) "tppp")
  ((policy (range 16 17) #\g) "gggggggvggggggggg")
  ((policy (range 3 14) #\f) "fhfdffftfbggpkfgf")
  ((policy (range 16 18) #\b) "bbbbbbbbbbbbbbbbbbbb")
  ((policy (range 5 10) #\v) "thqvvkvlwvdvjzzst")
  ((policy (range 13 14) #\p) "ppppppppppppptc")
  ((policy (range 11 14) #\t) "tkttczttttsttmcnltm")
  ((policy (range 3 4) #\w) "nvxl")
  ((policy (range 4 6) #\f) "fpflfwftk")
  ((policy (range 4 6) #\h) "xhhhhh")
  ((policy (range 15 17) #\t) "tttttttttttttttlv")
  ((policy (range 10 17) #\j) "jjjjjjjjjsjjjjjjd")
  ((policy (range 2 4) #\s) "jcgs")
  ((policy (range 7 9) #\q) "qsqqjhqndsqkqqpvqqr")
  ((policy (range 2 3) #\p) "tnpplcmdpfn")
  ((policy (range 2 5) #\c) "crvmk")
  ((policy (range 1 7) #\d) "tsmzcgddnddddrdxsq")
  ((policy (range 8 9) #\c) "rlfcpctpncv")
  ((policy (range 6 9) #\f) "tkzfhffdf")
  ((policy (range 13 15) #\s) "sssmsscfvsssdsg")
  ((policy (range 6 8) #\s) "ssjsssjrscsss")
  ((policy (range 7 10) #\f) "fffffffkfff")
  ((policy (range 1 6) #\k) "jqtdhpknkkk")
  ((policy (range 11 12) #\t) "tttttttttttf")
  ((policy (range 9 16) #\d) "swjbddfpfbntmprdd")
  ((policy (range 18 19) #\v) "dvvvvvvvvvvvvvvvkvvv")
  ((policy (range 2 5) #\j) "kjjcqjj")
  ((policy (range 11 12) #\d) "dddddddcddld")
  ((policy (range 14 16) #\f) "fffffffffffffffk")
  ((policy (range 4 7) #\x) "nxxxxxxn")
  ((policy (range 10 18) #\p) "pppppppppppppppppr")
  ((policy (range 8 9) #\k) "ktkjkkkkfvklkz")
  ((policy (range 9 15) #\h) "hhhhhhhhchhhhhhhhh")
  ((policy (range 18 19) #\v) "hvkvvvvvvvvvvvvvvqc")
  ((policy (range 1 8) #\s) "xzbxgsxshgl")
  ((policy (range 2 6) #\p) "ppsjpglptchpzbdhj")
  ((policy (range 6 7) #\n) "nnnnnnnc")
  ((policy (range 2 7) #\j) "jhtjjjjj")
  ((policy (range 11 12) #\b) "vnblqbxkbbwb")
  ((policy (range 4 5) #\z) "czzzcztsz")
  ((policy (range 4 9) #\x) "wxpvxxxfxfxx")
  ((policy (range 9 12) #\w) "wwwwwwwwwgwwww")
  ((policy (range 2 5) #\k) "cntkkq")
  ((policy (range 10 12) #\l) "vljplljlllllllll")
  ((policy (range 1 4) #\t) "tfmvst")
  ((policy (range 6 7) #\d) "rddddddwd")
  ((policy (range 2 6) #\h) "bhvwhl")
  ((policy (range 5 8) #\c) "ccsgccncwgjtcbhccckk")
  ((policy (range 5 6) #\r) "rrrrmr")
  ((policy (range 3 8) #\h) "hcrhhjrbhh")
  ((policy (range 2 4) #\s) "xndshswk")
  ((policy (range 2 7) #\m) "mmrqmmlp")
  ((policy (range 7 9) #\n) "nnnbfpmnfnknxnndn")
  ((policy (range 6 7) #\f) "fvffffb")
  ((policy (range 6 8) #\h) "vhhhhhtk")
  ((policy (range 6 7) #\j) "jjjjjwjjs")
  ((policy (range 1 5) #\r) "rwrrrr")
  ((policy (range 3 4) #\t) "tlzk")
  ((policy (range 2 7) #\k) "kkkkkkk")
  ((policy (range 6 11) #\q) "rqqqqqwflqqjqq")
  ((policy (range 5 7) #\l) "llllglll")
  ((policy (range 6 14) #\r) "rrkrrxrmrrrrrc")
  ((policy (range 1 8) #\w) "lrwpzvhhcwtbrwwpwww")
  ((policy (range 3 13) #\b) "kbjhskpbrwqpbcbbkb")
  ((policy (range 6 9) #\d) "ddddbddhdddddddddmg")
  ((policy (range 14 15) #\n) "nnnnnnnntnnnnhf")
  ((policy (range 2 18) #\k) "cgjpktthdbfxrclqwzpl")
  ((policy (range 5 14) #\w) "wwwwwbfwzwcwcwz")
  ((policy (range 5 7) #\s) "tfhssgrsnbssfl")
  ((policy (range 1 18) #\l) "wdkwkdlslbllllgzfm")
  ((policy (range 1 17) #\h) "jhhhhhhhhhhhthhhhh")
  ((policy (range 7 13) #\w) "wwvwwwwwrwwww")
  ((policy (range 1 3) #\r) "rgrdrx")
  ((policy (range 9 11) #\t) "dnxckkwpttv")
  ((policy (range 2 12) #\c) "cgqxrszcckcd")
  ((policy (range 4 13) #\q) "qkchpbqqtrtdztrq")
  ((policy (range 2 9) #\d) "ddddsddddd")
  ((policy (range 9 19) #\p) "pppppgppppqpppppppp")
  ((policy (range 8 9) #\l) "lllhlllll")
  ((policy (range 7 8) #\h) "hhhhhkhvwbhmh")
  ((policy (range 11 12) #\w) "wwwwwwwwwwwnwwww")
  ((policy (range 2 3) #\d) "jhcd")
  ((policy (range 13 16) #\n) "nnnnnnnnnnnnndnnmnn")
  ((policy (range 6 7) #\d) "dllpddkdd")
  ((policy (range 6 7) #\n) "nnnnnkp")
  ((policy (range 7 14) #\v) "vvbcvrvvkmvvsv")
  ((policy (range 5 7) #\j) "jjjjcjs")
  ((policy (range 4 10) #\l) "ldllllllll")
  ((policy (range 4 5) #\x) "rxxxx")
  ((policy (range 15 16) #\q) "qqqqqqqqqqqlqqqw")
  ((policy (range 13 15) #\m) "mmmmmmmmmbmmzmbm")
  ((policy (range 7 10) #\k) "skkkkkkkkq")
  ((policy (range 9 15) #\m) "mghxpjzqdhjcbmdl")
  ((policy (range 2 4) #\t) "ttsnwzbdj")
  ((policy (range 6 7) #\s) "hqtkgtr")
  ((policy (range 3 4) #\n) "twpknn")
  ((policy (range 1 3) #\l) "wrldlsdl")
  ((policy (range 5 6) #\l) "qlxncf")
  ((policy (range 4 5) #\s) "dvsxs")
  ((policy (range 7 9) #\t) "rktsrfwttttgwtzgft")
  ((policy (range 1 3) #\l) "flsf")
  ((policy (range 6 10) #\c) "cchczccccj")
  ((policy (range 1 3) #\k) "bktw")
  ((policy (range 2 4) #\h) "zdqxgrfkfhljhqhkgmfc")
  ((policy (range 16 17) #\s) "sssssssssssssssvs")
  ((policy (range 4 8) #\p) "plpfmnhsppnpwnx")
  ((policy (range 5 10) #\k) "kkkkknkqkk")
  ((policy (range 4 11) #\s) "nsqnssssvqsm")
  ((policy (range 6 14) #\j) "jjjzjwczkjjjxjjcjjxj")
  ((policy (range 3 7) #\t) "tnnbjjhtd")
  ((policy (range 7 8) #\l) "llllllll")
  ((policy (range 4 11) #\z) "cwsvrtszxrgh")
  ((policy (range 2 11) #\x) "sxcxxxjxrxkxdxx")
  ((policy (range 11 15) #\m) "mmxmbxmmdmmmmfmmfmm")
  ((policy (range 10 13) #\l) "tllllwzlzlrbllllhnp")
  ((policy (range 5 6) #\k) "kkkjkmkkkg")
  ((policy (range 4 7) #\f) "wqqfmffffkp")
  ((policy (range 1 9) #\b) "bbbbbmbbbbhbbdzbbtn")
  ((policy (range 10 12) #\h) "hhhhdwbhhqrhhhdnxhr")
  ((policy (range 1 8) #\r) "qfpnfrrrrz")
  ((policy (range 16 20) #\b) "bbbbbbbbbbbbbbbbbbbb")
  ((policy (range 1 5) #\c) "hwqccmwcctcnclcb")
  ((policy (range 6 13) #\c) "clccccqqcccjczpgj")
  ((policy (range 13 14) #\j) "jjjjjjjjjjjjjh")
  ((policy (range 2 4) #\j) "jkfp")
  ((policy (range 2 7) #\d) "qqxdcdjqg")
  ((policy (range 5 6) #\r) "prrrrd")
  ((policy (range 7 8) #\l) "llllllll")
  ((policy (range 2 3) #\w) "nwlm")
  ((policy (range 9 12) #\v) "vnknxncvgvrww")
  ((policy (range 6 16) #\s) "sjnwfsksnszcglxs")
  ((policy (range 5 17) #\z) "zzmkzzzzzntzjzbwpdjz")
  ((policy (range 12 17) #\j) "ljjjjjjhjjwtjjjjdjwj")
  ((policy (range 8 10) #\s) "dnnjlfkxls")
  ((policy (range 2 6) #\m) "jmmqmdkjm")
  ((policy (range 9 11) #\m) "mmmcmmmfsmmm")
  ((policy (range 14 16) #\p) "ppppptpppphrpxpppp")
  ((policy (range 4 15) #\d) "dfkfxdddldddddddlddp")
  ((policy (range 12 14) #\q) "qqqqqqqqqqqwwgq")
  ((policy (range 1 6) #\g) "tgggggmggggdg")
  ((policy (range 10 18) #\w) "wwwwwwwwwbwjwwwwwt")
  ((policy (range 8 9) #\t) "ttkttcttsrgrzlftpt")
  ((policy (range 3 5) #\j) "jjjjjjjjjjjjjl")
  ((policy (range 17 18) #\c) "ccccccczcccccccccs")
  ((policy (range 4 9) #\d) "hjddddddddm")
  ((policy (range 2 4) #\x) "lxjfp")
  ((policy (range 11 12) #\v) "vvvvvvvvvvjv")
  ((policy (range 1 4) #\z) "fmzblzvzm")
  ((policy (range 4 5) #\h) "hhshhh")
  ((policy (range 3 6) #\t) "ptvrtt")
  ((policy (range 7 15) #\k) "kkkjzkckgckskpkk")
  ((policy (range 12 15) #\w) "wwwwwwwwwwwftwdw")
  ((policy (range 3 5) #\z) "jrhns")
  ((policy (range 5 6) #\n) "pxmnzn")
  ((policy (range 1 3) #\b) "bbbkbbbbbbwb")
  ((policy (range 1 10) #\r) "rrjrqrrwrrrqv")
  ((policy (range 11 12) #\w) "wwwwwwwwwwwwwww")
  ((policy (range 5 6) #\j) "jsjjjp")
  ((policy (range 6 11) #\h) "lhbbvmhwrhh")
  ((policy (range 18 19) #\q) "qqqqqqqcxqqqqqqqqxwq")
  ((policy (range 17 18) #\l) "llllllllllllllllxql")
  ((policy (range 3 4) #\j) "jjljjn")
  ((policy (range 8 10) #\z) "zzzzwnzlzggz")
  ((policy (range 6 7) #\t) "tttttft")
  ((policy (range 11 12) #\f) "ffffffffffnf")
  ((policy (range 2 8) #\b) "hqbbbbbsbb")
  ((policy (range 4 5) #\w) "stljwbxpw")
  ((policy (range 1 2) #\z) "grxfpw")
  ((policy (range 6 8) #\w) "wwwwpwwxw")
  ((policy (range 8 14) #\z) "zqzzzzzvzzzzzzgz")
  ((policy (range 4 11) #\b) "cckgrcmbpvbbw")
  ((policy (range 2 9) #\d) "xqxbdddjds")
  ((policy (range 3 6) #\x) "xxrxxx")
  ((policy (range 15 16) #\n) "nnnnnnnnnnnnnndnn")
  ((policy (range 6 11) #\x) "lqmxxxsxxfxxxxlx")
  ((policy (range 1 12) #\s) "vssssshssmsk")
  ((policy (range 1 16) #\m) "bmgkkmwmmzvmmmmmwflv")
  ((policy (range 3 15) #\t) "thkvttsltnbgztt")
  ((policy (range 9 17) #\z) "zzzzzzzzzzzzzzzzzz")
  ((policy (range 4 6) #\f) "fjwfff")
  ((policy (range 3 9) #\d) "ddddddddd")
  ((policy (range 6 10) #\k) "kjkbkkckxkknqkkdc")
  ((policy (range 6 7) #\n) "nsnckwnmn")
  ((policy (range 7 13) #\t) "ptbtjqttwwtftpntdq")
  ((policy (range 7 10) #\r) "wrhhrrrrrwrrrrvqrms")
  ((policy (range 2 3) #\c) "zcrwhtccb")
  ((policy (range 5 7) #\m) "nnwmmdmmmccv")
  ((policy (range 3 5) #\j) "rtjjmjrjl")
  ((policy (range 5 6) #\s) "dssjhsdmbksrks")
  ((policy (range 16 18) #\c) "ccccclcccclccccccc")
  ((policy (range 7 16) #\b) "qpzgqxdbssmzptrv")
  ((policy (range 6 8) #\b) "bnsbbgdbggxbbszbm")
  ((policy (range 1 10) #\h) "lrpstqvhhhhf")
  ((policy (range 4 6) #\s) "tvssppwvss")
  ((policy (range 3 4) #\g) "qqqhcd")
  ((policy (range 4 7) #\c) "qxcqzccckxcpcxcw")
  ((policy (range 1 13) #\m) "nbwdmspmmwbbmm")
  ((policy (range 1 2) #\j) "xjrczrk")
  ((policy (range 9 10) #\w) "wwwwwwwwwd")
  ((policy (range 2 4) #\h) "hhhq")
  ((policy (range 7 10) #\j) "bjmrrjsjthjnnjjxjjd")
  ((policy (range 3 7) #\f) "srfwwfvf")
  ((policy (range 3 4) #\b) "nbbzb")
  ((policy (range 3 5) #\n) "nnnnn")
  ((policy (range 7 8) #\f) "fxfnfffzpqffgdf")
  ((policy (range 10 12) #\k) "kkpkkkpnkkmkjbkk")
  ((policy (range 9 10) #\k) "jkkkkkkkwqk")
  ((policy (range 4 6) #\z) "rzzzzs")
  ((policy (range 4 7) #\v) "vvvtvvjq")
  ((policy (range 6 12) #\z) "dzjnzczzpzgzzzx")
  ((policy (range 6 8) #\s) "smsvbkswslssrdfjsc")
  ((policy (range 5 7) #\b) "bwbbbbbbr")
  ((policy (range 1 3) #\d) "kddbsmmbv")
  ((policy (range 3 4) #\b) "bbbb")
  ((policy (range 5 8) #\r) "rqrrdrrr")
  ((policy (range 7 8) #\m) "mmmmmmzm")
  ((policy (range 9 11) #\q) "xvnlfvhxqfql")
  ((policy (range 3 4) #\w) "wfwj")
  ((policy (range 1 3) #\q) "gqqcqg")
  ((policy (range 1 2) #\q) "cqlxrq")
  ((policy (range 6 12) #\s) "swwjjssjsstmqfssdh")
  ((policy (range 2 4) #\q) "bqqq")
  ((policy (range 5 6) #\c) "ccccbscgfc")
  ((policy (range 14 15) #\x) "xxcxxxxxxxxxxxm")
  ((policy (range 6 12) #\f) "fzfchfnrxfrdffhfl")
  ((policy (range 14 17) #\g) "gggggggggggggjgpgg")
  ((policy (range 5 8) #\n) "njkcngznn")
  ((policy (range 10 13) #\d) "dddlqddddvddr")
  ((policy (range 2 4) #\c) "xgcg")
  ((policy (range 2 10) #\s) "sxsnsdmszwmsss")
  ((policy (range 6 12) #\q) "qbcqspqqqkqrq")
  ((policy (range 6 16) #\n) "xnnnnnnnnnnnnnnfnnn")
  ((policy (range 6 12) #\s) "tssnssmsssssssssnhr")
  ((policy (range 1 4) #\v) "vwvvv")
  ((policy (range 8 10) #\h) "rzmhhjvqhht")
  ((policy (range 1 4) #\t) "ttjdjftfctt")
  ((policy (range 1 4) #\k) "nkkz")
  ((policy (range 1 2) #\q) "vkvqkqq")
  ((policy (range 9 19) #\n) "qxmjqxnxnblqnnqfgsrd")
  ((policy (range 15 16) #\h) "hhhhhhhhhhhhhhghz")
  ((policy (range 2 5) #\g) "jggpxdgwjpsrv")
  ((policy (range 12 13) #\c) "xccccpccrcccc")
  ((policy (range 1 13) #\g) "nfgbghqrljstggcgq")
  ((policy (range 14 17) #\g) "gggggggggggggmggggg")
  ((policy (range 4 14) #\s) "sssbsssssssssgs")
  ((policy (range 1 10) #\d) "rcxvxcgddnhwd")
  ((policy (range 4 14) #\g) "vccvdldqdgzltq")
  ((policy (range 8 10) #\z) "zzdczzvrzzzzr")
  ((policy (range 5 10) #\t) "ctgtttftqtrbt")
  ((policy (range 3 5) #\v) "vvrvx")
  ((policy (range 9 10) #\w) "wwwwwwwwwwwww")
  ((policy (range 4 6) #\c) "hgccmcccc")
  ((policy (range 16 18) #\s) "ssssssssssstssscsh")
  ((policy (range 2 7) #\z) "zzflfrtdx")
  ((policy (range 4 10) #\v) "vvlsvvvvkrc")
  ((policy (range 10 16) #\d) "ddddwddddbdgdddwd")
  ((policy (range 3 4) #\v) "gkbqbtrv")
  ((policy (range 3 4) #\h) "cjdqvwhhththvx")
  ((policy (range 3 5) #\b) "bbbkbrb")
  ((policy (range 3 5) #\m) "ptkqdgsmbcmmblwp")
  ((policy (range 4 17) #\q) "mlqqgsqqqfqkcqqxqq")
  ((policy (range 6 17) #\m) "mdvqvmmxlmzmvmmdm")
  ((policy (range 1 3) #\r) "hrrtrrrrrrrrrwlr")
  ((policy (range 3 5) #\j) "jjjjhj")
  ((policy (range 11 12) #\q) "dqpqqqqqqnzlqq")
  ((policy (range 8 11) #\d) "dddnddnddtdddd")
  ((policy (range 3 16) #\d) "dpddxtlqsqldrpddq")
  ((policy (range 7 13) #\z) "szhzmmzzzzmzzzz")
  ((policy (range 6 18) #\h) "mkxxqtbnnjgnvxnhhhc")
  ((policy (range 4 5) #\n) "nknfrhpn")
  ((policy (range 16 17) #\d) "dddddddgdddddddddd")
  ((policy (range 14 16) #\w) "wwwwwwwwwwwwwwwww")
  ((policy (range 2 3) #\t) "rttfdstqpdtg")
  ((policy (range 3 4) #\s) "gghrfxbfshqssj")
  ((policy (range 1 9) #\j) "bjjjjjjjhjjj")
  ((policy (range 1 2) #\p) "xmpgppppppp")
  ((policy (range 9 14) #\k) "vqpxrkmskfpnxq")
  ((policy (range 1 5) #\x) "xxxxx")
  ((policy (range 1 14) #\p) "bsxnwvpkphdppphpkwp")
  ((policy (range 5 12) #\d) "xdddmzddfddc")
  ((policy (range 9 10) #\v) "gvvvvvgvhnvw")
  ((policy (range 2 7) #\x) "xxxxxwgsvmqnkxv")
  ((policy (range 3 15) #\c) "ccscccccwccccccc")
  ((policy (range 5 8) #\w) "nsvgwzdf")
  ((policy (range 7 14) #\w) "wwwwwwwwwwwfwswcw")
  ((policy (range 5 10) #\r) "qrrlwcrrwz")
  ((policy (range 7 12) #\f) "hfnphfpjffstff")
  ((policy (range 2 6) #\h) "hqjhmph")
  ((policy (range 3 13) #\x) "cpxxxzcrbjxxxb")
  ((policy (range 4 8) #\k) "kkkkktkkkkfkf")
  ((policy (range 7 10) #\g) "gggghghsgwf")
  ((policy (range 8 9) #\s) "sswswssggss")
  ((policy (range 6 10) #\l) "qlllljllhlll")
  ((policy (range 8 11) #\c) "cccwccrbcbccc")
  ((policy (range 5 6) #\x) "xxxxxxg")
  ((policy (range 1 3) #\w) "cwpww")
  ((policy (range 8 11) #\v) "vvvvvvvvvwvv")
  ((policy (range 1 2) #\b) "bbgl")
  ((policy (range 7 10) #\b) "zbkbcbbbsbb")
  ((policy (range 13 15) #\t) "tttttgtttttttktttt")
  ((policy (range 12 14) #\x) "xxxxxxxxxxxxxqf")
  ((policy (range 6 7) #\k) "kmrkkwmk")
  ((policy (range 4 5) #\m) "mmxjmdrhzkmxxphjmz")
  ((policy (range 2 3) #\t) "tnvtndw")
  ((policy (range 4 5) #\x) "xxtxxtxzf")
  ((policy (range 1 3) #\m) "nwmglfscmwrjtzp")
  ((policy (range 10 11) #\c) "cnccckcckcwdc")
  ((policy (range 14 19) #\b) "tvbmrqtchwblbqbbqbb")
  ((policy (range 9 10) #\x) "fxxxxxxxjxx")
  ((policy (range 4 5) #\g) "gggggkggx")
  ((policy (range 8 12) #\k) "kkkkkfkpkqkwkkk")
  ((policy (range 2 6) #\v) "chpvkvt")
  ((policy (range 4 10) #\r) "rmbjmcrgrrfrtmblhw")
  ((policy (range 4 6) #\n) "rnnwtn")
  ((policy (range 3 10) #\v) "htclcjhjkdv")
  ((policy (range 10 13) #\w) "wwwhhwwwrwwzww")
  ((policy (range 18 20) #\w) "wwwwwwjwwwwwwwwvwtww")
  ((policy (range 7 8) #\w) "phrwbwwmc")
  ((policy (range 16 17) #\q) "qqqqqqqqqqqqqqqqx")
  ((policy (range 2 5) #\k) "kzknsxkkgtvmwlfrmrg")
  ((policy (range 3 4) #\b) "zbbbb")
  ((policy (range 5 8) #\v) "hfclvvpc")
  ((policy (range 3 15) #\t) "ctfthpmgznjnhgtth")
  ((policy (range 1 6) #\t) "httttq")
  ((policy (range 5 6) #\w) "wwxhgww")
  ((policy (range 3 5) #\m) "fmtnmmm")
  ((policy (range 11 17) #\n) "xnnchnzpsmcslvzcn")
  ((policy (range 5 6) #\m) "mmmmcm")
  ((policy (range 1 7) #\r) "rwsgcgvjmtxwkqtr")
  ((policy (range 6 16) #\m) "mmmmmmmmmmmqmmmmmm")
  ((policy (range 5 10) #\s) "sssggssssjsssmsss")
  ((policy (range 9 14) #\p) "ppfppzppnvmpqdvpp")
  ((policy (range 2 4) #\l) "wclwrlrdvh")
  ((policy (range 6 17) #\g) "gggmkglggwgpgkmglt")
  ((policy (range 5 7) #\z) "zzzbzpz")
  ((policy (range 4 5) #\q) "qbqqlzpqg")
  ((policy (range 10 19) #\h) "hhhhhhhhhrhhqhhhhhhh")
  ((policy (range 6 7) #\m) "nfnxmsmhtmhsmmmmgmsl")
  ((policy (range 2 3) #\m) "mkfmmm")
  ((policy (range 8 10) #\j) "xrjdnjjjnjjcjjjjjj")
  ((policy (range 3 6) #\x) "xwrjxhjxx")
  ((policy (range 6 7) #\z) "zzzzzzzg")
  ((policy (range 8 17) #\k) "kkkkkkkkkkkkkkkkk")
  ((policy (range 6 9) #\n) "rnnxnnbwnsnnwn")
  ((policy (range 6 8) #\m) "mkmmmsmm")
  ((policy (range 4 6) #\q) "qqfxqqq")
  ((policy (range 2 8) #\s) "hxmfbkszk")
  ((policy (range 6 7) #\r) "rrrrrrr")
  ((policy (range 2 5) #\h) "hhqzhhhh")
  ((policy (range 5 15) #\b) "bhbvnrbrbblbbhh")
  ((policy (range 12 13) #\d) "ddddddtdddvhddd")
  ((policy (range 5 6) #\f) "fmfcfwf")
  ((policy (range 4 7) #\j) "njqfxjjjqjjvdj")
  ((policy (range 6 9) #\g) "fggkggzgghrv")
  ((policy (range 9 12) #\b) "vbblbxpfrbjbgpwxb")
  ((policy (range 1 8) #\s) "tzcdrktsqjsxssfsssh")
  ((policy (range 10 14) #\f) "fffffffffcffffffffff")
  ((policy (range 13 17) #\p) "pqpppppcpbwpppnprppk")
  ((policy (range 16 17) #\n) "nnnmnnnnnnnnnnnsj")
  ((policy (range 10 16) #\k) "kkkkkkkkkkkkkkktk")
  ((policy (range 8 11) #\w) "wwwwwwwwwwm")
  ((policy (range 2 5) #\q) "qqqqqq")
  ((policy (range 2 7) #\m) "mmmmmxg")
  ((policy (range 2 9) #\m) "mmvlggdmr")
  ((policy (range 3 9) #\s) "wxdbmlcrlpnzmkfdfs")
  ((policy (range 12 16) #\z) "zlxzgznqqfzsqtwc")
  ((policy (range 6 7) #\f) "flvmfffjffbmgfzfgfp")
  ((policy (range 2 3) #\g) "cgggqgxqgv")
  ((policy (range 2 3) #\f) "rffvf")
  ((policy (range 1 8) #\h) "vhhhhhhvhhvhhhhf")
  ((policy (range 6 12) #\x) "vzdxxxgxtxzxxxqc")
  ((policy (range 4 5) #\w) "rwwwwww")
  ((policy (range 7 10) #\r) "brwrcrrkcsj")
  ((policy (range 6 10) #\q) "jvqqqqqqqmq")
  ((policy (range 5 18) #\v) "lvhvvwvvvvvlvlvvmv")
  ((policy (range 6 9) #\z) "zzvzcwlzslhlzqsf")
  ((policy (range 3 6) #\x) "sdknfxvwtcfcjgmv")
  ((policy (range 8 9) #\l) "lqcwlllsllll")
  ((policy (range 1 4) #\k) "dgklv")
  ((policy (range 2 6) #\q) "qqqptfqqqqq")
  ((policy (range 4 7) #\m) "mrmcmmmm")
  ((policy (range 4 5) #\r) "rrrblg")
  ((policy (range 1 8) #\c) "qcccccvntc")
  ((policy (range 6 7) #\m) "mzbxmmmnhmqcmmqvbmcp")
  ((policy (range 2 4) #\t) "ptvqx")
  ((policy (range 9 10) #\s) "gpssssssgss")
  ((policy (range 4 18) #\b) "bbbbbbbbbbbbbbbbbfb")
  ((policy (range 4 5) #\f) "ffffgf")
  ((policy (range 3 6) #\l) "lslglw")
  ((policy (range 1 5) #\l) "ltzlllsdlmrllkvbztvr")
  ((policy (range 15 19) #\l) "llllllllllllllnllll")
  ((policy (range 10 11) #\j) "jjjjpjjjjjncj")
  ((policy (range 2 3) #\w) "wfwgd")
  ((policy (range 1 7) #\r) "vrrrzrr")
  ((policy (range 3 7) #\k) "kkkjckkkgkbtvm")
  ((policy (range 9 12) #\k) "kkkkkrckcrkk")
  ((policy (range 1 6) #\b) "hpbjbvb")
  ((policy (range 6 8) #\s) "ssssssvss")
  ((policy (range 2 4) #\x) "mpxk")
  ((policy (range 6 10) #\x) "xxxmrwhgxxxjx")
  ((policy (range 7 8) #\x) "xxphxxxxx")
  ((policy (range 6 19) #\n) "mdrlnnpnjndqgnkfwnnn")
  ((policy (range 2 5) #\q) "qjwtbmkhtnxm")
  ((policy (range 15 17) #\j) "xjjjjjbjqjjjjjjqjjjw")
  ((policy (range 5 7) #\x) "pxxxxsxxsxx")
  ((policy (range 3 6) #\t) "ttttrp")
  ((policy (range 8 11) #\f) "fffffffffrkzf")
  ((policy (range 6 8) #\r) "glsprkbp")
  ((policy (range 10 12) #\m) "mmmmmmmmmmmsm")
  ((policy (range 13 14) #\z) "zzszzzszzbzzzplzzz")
  ((policy (range 19 20) #\c) "ccccchwccccccccccncb")
  ((policy (range 7 12) #\z) "qzggglzlhshnrwt")
  ((policy (range 12 14) #\z) "zzzzzzzzhzzwzc")
  ((policy (range 1 2) #\s) "sssl")
  ((policy (range 1 3) #\q) "fsqmwq")
  ((policy (range 1 6) #\z) "zmzztzkzznmflqffz")
  ((policy (range 9 11) #\b) "bwbbjbbbtlbbn")
  ((policy (range 4 6) #\s) "nktszz")
  ((policy (range 2 5) #\j) "nqjhjkb")
  ((policy (range 7 16) #\l) "lrlllllllmllswlsl")
  ((policy (range 2 3) #\f) "fjqvfff")
  ((policy (range 7 15) #\w) "hwxghddqwwwwwww")
  ((policy (range 2 4) #\z) "zgzfx")
  ((policy (range 5 6) #\h) "vhhjhthh")
  ((policy (range 13 14) #\h) "hhhhhhfhhhhhhhphhhh")
  ((policy (range 16 17) #\k) "kkkkkkkkkkkkkkknx")
  ((policy (range 7 15) #\l) "dllllllmlllfllf")
  ((policy (range 2 6) #\w) "mtkwwwww")
  ((policy (range 2 7) #\k) "zpkkgldk")
  ((policy (range 7 9) #\d) "htmtjddddddqvddcd")
  ((policy (range 4 9) #\v) "rvfbbwzdwcvvqv")
  ((policy (range 6 11) #\m) "mmvmbvmmmsmmlmmm")
  ((policy (range 3 8) #\p) "xpxppsgpbxp")
  ((policy (range 2 6) #\f) "vffbszhvvrdfkxc")
  ((policy (range 8 11) #\v) "lvrrlvvqqvr")
  ((policy (range 5 17) #\h) "hdhhbhhzhhhhhhsbl")
  ((policy (range 8 10) #\f) "fhgffgfrfqtf")
  ((policy (range 9 12) #\s) "ssssfssdpssswf")
  ((policy (range 9 12) #\m) "mmmmmmmmpzgmmm")
  ((policy (range 7 9) #\l) "zlmllvwlllldl")
  ((policy (range 8 11) #\m) "mmdmfmmdxbmxbmm")
  ((policy (range 1 4) #\m) "fklssmffwzgqcvdpm")
  ((policy (range 13 14) #\c) "cczcccccccccccc")
  ((policy (range 5 9) #\f) "frtkfpffsfff")
  ((policy (range 5 8) #\f) "ffffgffr")
  ((policy (range 3 8) #\m) "mmxsmmmb")
  ((policy (range 1 6) #\m) "mdmmmm")
  ((policy (range 1 2) #\h) "hhdtmhwfpt")
  ((policy (range 4 14) #\d) "dmrkdvddmdsdddbsgd")
  ((policy (range 6 7) #\c) "cxccgcccc")
  ((policy (range 6 12) #\v) "xcqvjvvsdvvvpv")
  ((policy (range 11 14) #\d) "gltkjvddwjdkrln")
  ((policy (range 2 13) #\f) "ggffllflfbfffqkfmf")
  ((policy (range 10 13) #\c) "xkvcccccrmccccc")
  ((policy (range 17 19) #\q) "qqsqqqqqqqqqqqqqfqmq")
  ((policy (range 8 10) #\t) "ttttlttwtlt")
  ((policy (range 4 5) #\f) "wffrd")
  ((policy (range 2 16) #\v) "tvhvvqvrvvvvvvqvvf")
  ((policy (range 2 4) #\c) "cccc")
  ((policy (range 3 8) #\z) "zrzzpzlzzhsdxqqfx")
  ((policy (range 6 10) #\k) "kkkkkkkkkk")
  ((policy (range 5 7) #\r) "rvrrvrz")
  ((policy (range 14 15) #\w) "wwwwwwwlwwwwncwvwjk")
  ((policy (range 6 11) #\q) "qfhxqqgqqqsvcpr")
  ((policy (range 4 7) #\f) "kflfffzff")
  ((policy (range 2 14) #\t) "rtttttptlxsttt")
  ((policy (range 11 15) #\t) "ttttttttttftbvqttt")
  ((policy (range 6 10) #\c) "cckwlchltchcczgw")
  ((policy (range 3 8) #\s) "ssssvsqnss")
  ((policy (range 1 8) #\h) "mhhhhrhhh")
  ((policy (range 13 15) #\b) "bbbbbbbbbbbbbbnb")
  ((policy (range 15 17) #\p) "dpppppppppspppcps")
  ((policy (range 5 12) #\l) "xbfjglklphtl")
  ((policy (range 2 4) #\v) "vvvvbvwpvvvrv")
  ((policy (range 3 5) #\k) "plhjkkqkskz")
  ((policy (range 6 12) #\z) "zzzzzczzzzzk")
  ((policy (range 2 3) #\j) "xvjljj")
  ((policy (range 6 7) #\r) "rlrrrrrrzrrzrrr")
  ((policy (range 1 12) #\t) "mqrkgpktbsqxg")
  ((policy (range 10 13) #\z) "pzmzzxpnzxpgzzzhzz")
  ((policy (range 5 13) #\s) "gslsskvgxssssnpj")
  ((policy (range 10 14) #\m) "mmmmmmsmmmmmmk")
  ((policy (range 2 5) #\b) "tnbdsfwdzxrkjdb")
  ((policy (range 2 15) #\h) "hnxhzhmhhhhhwwb")
  ((policy (range 1 4) #\c) "ccccccc")
  ((policy (range 9 10) #\b) "bbbbbbbbbl")
  ((policy (range 4 5) #\j) "jsjjj")
  ((policy (range 3 6) #\n) "xmmnppbn")
  ((policy (range 11 12) #\m) "mmmmmmmmmmmm")
  ((policy (range 2 4) #\r) "jrgrcpvgctrrr")
  ((policy (range 3 4) #\g) "gggr")
  ((policy (range 14 17) #\n) "nhnpnnnnznnnnnnnknnm")
  ((policy (range 4 5) #\k) "krkkf")
  ((policy (range 11 17) #\h) "hhdhhhhhhhhghhkhx")
  ((policy (range 5 19) #\q) "dqbcqqqqsqqzqqqqqqv")
  ((policy (range 5 16) #\t) "ttrkttttstwttttqdt")
  ((policy (range 11 18) #\h) "dhhhhhhhhjhtjhhhhhh")
  ((policy (range 5 8) #\c) "ccccrccfc")
  ((policy (range 1 3) #\v) "nvvnv")
  ((policy (range 7 12) #\m) "prmmzmtmmmssdmmmmt")
  ((policy (range 15 16) #\r) "rrrrrrrrrrrrrqfr")
  ((policy (range 7 19) #\b) "djbbbbqjbdtbslbgbbmb")
  ((policy (range 10 13) #\w) "wwwwwwwwwwwwww")
  ((policy (range 5 15) #\p) "mbpcjqmmhpppkfmrbcww")
  ((policy (range 2 4) #\n) "nxbjknms")
  ((policy (range 5 11) #\s) "hxhcsldmxshdksvsss")
  ((policy (range 12 16) #\q) "qqqqqqqpqcdqqtwkqvq")
  ((policy (range 2 7) #\j) "tjlpcjjjjnj")
  ((policy (range 6 7) #\s) "ssssskm")
  ((policy (range 1 4) #\t) "tttttx")
  ((policy (range 5 10) #\q) "pqmnqrlqchbjzqqvq")
  ((policy (range 11 15) #\q) "qqqqqqqqqqhqrqqrqq")
  ((policy (range 3 5) #\k) "jhkgkxkqfskg")
  ((policy (range 14 15) #\f) "ffvffmfffffffsffqm")
  ((policy (range 4 5) #\x) "xxfxhfxx")
  ((policy (range 2 4) #\b) "ktbv")
  ((policy (range 3 4) #\t) "wjtwxvxctrcttb")
  ((policy (range 1 3) #\s) "shssg")
  ((policy (range 8 12) #\z) "skkzbdpzdkzzz")
  ((policy (range 1 3) #\f) "rlfffqft")
  ((policy (range 5 8) #\s) "fsrcnjsgsmgs")
  ((policy (range 7 17) #\v) "vvvvvvvvvvvvvvvvvvv")
  ((policy (range 4 5) #\k) "kklmq")
  ((policy (range 8 17) #\h) "hhhhhlvghwkhrfzhxh")
  ((policy (range 17 18) #\n) "nnnnnnnnnnnnnnnnnn")
  ((policy (range 3 10) #\h) "mhhhhrhznwvhh")
  ((policy (range 8 9) #\x) "xmxxxxxxx")
  ((policy (range 4 5) #\p) "ppppk")
  ((policy (range 11 13) #\t) "tttttttttttjtcn")
  ((policy (range 3 4) #\q) "qqvhz")
  ((policy (range 3 8) #\l) "gbpqclnkwlhdlml")
  ((policy (range 12 13) #\f) "fffffffffffrf")
  ((policy (range 3 11) #\s) "ssjssssssslssss")
  ((policy (range 6 7) #\b) "bbbbbwbb")
  ((policy (range 7 9) #\r) "krrrrrfrj")
  ((policy (range 13 14) #\b) "bbbbbbbbbvbbbp")
  ((policy (range 2 4) #\x) "ltgbx")
  ((policy (range 7 10) #\r) "smrrrrkjrrr")
  ((policy (range 3 7) #\n) "vnqpngjdgrw")
  ((policy (range 7 18) #\t) "ttltqtsktrjgxtqhtt")
  ((policy (range 2 5) #\v) "dvvxbvvvnk")
  ((policy (range 2 5) #\n) "nnnnnpn")
  ((policy (range 1 3) #\t) "nttmtx")
  ((policy (range 3 4) #\z) "zpszz")
  ((policy (range 1 12) #\p) "pmpxphlpppppppqppp")
  ((policy (range 3 4) #\v) "qvvvvvv")
  ((policy (range 9 12) #\g) "gwgggggggtgqgg")
  ((policy (range 2 4) #\n) "knpn")
  ((policy (range 3 13) #\d) "hncbdrvpddddcfddd")
  ((policy (range 5 9) #\s) "ssnspslslsfkpskss")
  ((policy (range 1 2) #\s) "ssczgkvtlp")
  ((policy (range 5 6) #\p) "pmvpzpppkm")
  ((policy (range 1 4) #\j) "jjhr")
  ((policy (range 5 6) #\s) "ssssxl")
  ((policy (range 1 3) #\t) "ttrt")
  ((policy (range 16 18) #\n) "nnpnnnnnnnnnnnnnnnn")
  ((policy (range 4 5) #\n) "nnfnl")
  ((policy (range 1 4) #\b) "bbbb")
  ((policy (range 7 8) #\l) "nljlllkhl")
  ((policy (range 11 12) #\n) "nnnnnsnnnnnnnpn")
  ((policy (range 5 10) #\k) "kkkkckkkkhkkkkk")
  ((policy (range 7 10) #\s) "zfsssfldxsvqsxdsqs")
  ((policy (range 5 6) #\q) "qqqqqp")
  ((policy (range 6 7) #\q) "qqqqqqq")
  ((policy (range 14 16) #\w) "wwdhmwwjwwwwnwwlwwwv")
  ((policy (range 8 9) #\l) "llllllldll")
  ((policy (range 12 14) #\c) "ccrccccccccrcccccccc")
  ((policy (range 1 3) #\n) "qnwnh")
  ((policy (range 1 4) #\l) "mlll")
  ((policy (range 1 3) #\s) "sssssss")
  ((policy (range 4 15) #\h) "mhclhhhjllhhhgh")
  ((policy (range 15 16) #\x) "xxxxxxtxxxxxtxttxx")
  ((policy (range 2 4) #\r) "rmjh")
  ((policy (range 6 9) #\z) "zzzzzpszt")
  ((policy (range 17 18) #\n) "nnwnnnnnnnnnnnnnppnn")
  ((policy (range 8 11) #\v) "vmvmvlvbvvvvv")
  ((policy (range 7 8) #\b) "bbbbbbdf")
  ((policy (range 2 5) #\k) "kkdkk")
  ((policy (range 8 9) #\d) "dddsldddvxlhd")
  ((policy (range 7 8) #\h) "hhhhkhxshxhn")
  ((policy (range 12 14) #\c) "vcchpxcnmgkctpqcc")
  ((policy (range 1 5) #\w) "wwwwww")
  ((policy (range 1 2) #\x) "xdmk")
  ((policy (range 1 9) #\p) "pbfvrdpqqn")
  ((policy (range 2 4) #\s) "mtnvzhnzs")
  ((policy (range 4 6) #\t) "cttxvtttx")
  ((policy (range 4 8) #\t) "qttmtzttkwftrjk")
  ((policy (range 7 9) #\c) "cccccccccc")
  ((policy (range 2 18) #\j) "zjjqxjfqqlmjrjhjqx")
  ((policy (range 2 5) #\q) "qmkqgq")
  ((policy (range 5 11) #\k) "kkkkkkslzkkkkkrx")
  ((policy (range 16 18) #\h) "hhhhhhhhhqhthhhvhh")
  ((policy (range 5 13) #\h) "hhfhhhhhmhhshhhhhhh")
  ((policy (range 10 12) #\r) "rkrrrrrmrrrrrr")
  ((policy (range 8 10) #\n) "rnnckhmjnxmfggsgtnnn")
  ((policy (range 19 20) #\c) "qsccznccccccnccccccq")
  ((policy (range 1 4) #\k) "wtkck")
  ((policy (range 7 8) #\s) "sssmssjssss")
  ((policy (range 3 5) #\l) "llslv")
  ((policy (range 3 7) #\t) "tvgjqgjbvtgpkt")
  ((policy (range 13 15) #\b) "bbbbbbbbqbblrbbb")
  ((policy (range 9 10) #\z) "zzzzzznzzb")
  ((policy (range 6 16) #\c) "kpcnsccgccxfwccl")
  ((policy (range 2 3) #\d) "ddbd")
  ((policy (range 10 16) #\v) "vvvvqrvvsvvvvvvl")
  ((policy (range 3 12) #\d) "dzdxxhddnlkddddk")
  ((policy (range 8 9) #\b) "khbbbfbbbbb")
  ((policy (range 11 12) #\z) "zzzzzzzzzzzz")
  ((policy (range 5 15) #\n) "bnlvnnrjbnqlnnn")
  ((policy (range 1 2) #\q) "mqqpzjc")
  ((policy (range 11 13) #\h) "hhhhhhchhhhjhh")
  ((policy (range 8 11) #\x) "nxfcxbjqxdlcxxxkxxx")
  ((policy (range 14 18) #\v) "vvbqvvvvvvvvvvvvvvvv")
  ((policy (range 7 8) #\f) "pgtlfffmjffnt")
  ((policy (range 10 18) #\m) "mnkrmmmmmqmmmmmmmjm")
  ((policy (range 1 4) #\l) "lghlhcnwprlgvw")
  ((policy (range 3 4) #\p) "npxplpwl")
  ((policy (range 12 15) #\s) "sfkwnvsssvssssfsssrp")
  ((policy (range 3 7) #\z) "ztqnzlxjmzzbjfnxcw")
  ((policy (range 17 18) #\q) "qqqqqqqqqqqqqqqqqq")
  ((policy (range 3 6) #\z) "jczztzzbhmzzw")
  ((policy (range 3 7) #\f) "skffttjffqfnchfrf")
  ((policy (range 4 6) #\m) "hmqmcrmw")
  ((policy (range 10 11) #\t) "dftttqttttttl")
  ((policy (range 7 13) #\p) "spxnpgtzpppjpwppptp")
  ((policy (range 8 9) #\q) "jqxqqqqxqq")
  ((policy (range 3 4) #\h) "hhhh")
  ((policy (range 10 13) #\z) "pgzxhxxpzzssj")
  ((policy (range 5 9) #\x) "xfrdmjxmrxrqjpxbfxxx")
  ((policy (range 8 17) #\d) "ddpddddddddsgddzddn")
  ((policy (range 1 4) #\g) "vggdggggggggggggggg")
  ((policy (range 10 12) #\m) "mmdqsmwvjjmv")
  ((policy (range 2 7) #\s) "ssqsssk")
  ((policy (range 13 14) #\n) "nnvnnnnnnnnnsgnn")
  ((policy (range 2 16) #\k) "kkkkkkkkkkknkkkk")
  ((policy (range 3 6) #\p) "dprphc")
  ((policy (range 2 10) #\k) "mpkstkxkdb")
  ((policy (range 2 3) #\s) "ssxs")
  ((policy (range 9 19) #\t) "ttjttjttrtsqtmttttt")
  ((policy (range 4 5) #\j) "jrjjtsjjd")
  ((policy (range 6 7) #\x) "xxxpxjx")
  ((policy (range 13 15) #\q) "fqfqqqqhqcqfqqbqqq")
  ((policy (range 1 3) #\z) "zfgrzl")
  ((policy (range 6 8) #\n) "nnnnnphm")
  ((policy (range 1 8) #\b) "bbbbbbbbb")
  ((policy (range 4 5) #\h) "thhgj")
  ((policy (range 4 8) #\v) "vvvcvvvp")
  ((policy (range 8 9) #\t) "ktttttttt")
  ((policy (range 3 4) #\f) "wftbcfnmhkfdpjbns")
  ((policy (range 6 7) #\v) "vvvvvlv")
  ((policy (range 16 17) #\z) "zzzzhfzzzzzzkzzzwzzz")
  ((policy (range 1 2) #\q) "qqqfq")
  ((policy (range 10 11) #\p) "zpjzcppvcppppppppj")
  ((policy (range 10 15) #\c) "vlccbcgcclcczfcfcc")
  ((policy (range 11 17) #\t) "tsntktwzhsfttfwtx")
  ((policy (range 1 7) #\h) "nhhhhhb")
  ((policy (range 6 13) #\b) "bbbphnbbwbbbb")
  ((policy (range 8 9) #\x) "xgxxxlxfxx")
  ((policy (range 14 16) #\m) "mmmmmmmmmmmmmmmm")
  ((policy (range 3 4) #\x) "xxbxx")
  ((policy (range 6 9) #\c) "xkccsfccrccv")
  ((policy (range 10 18) #\h) "jphhbshwghrxpnhnlhxh")
  ((policy (range 6 10) #\f) "nlxbgvftfh")
  ((policy (range 3 4) #\p) "ppwf")
  ((policy (range 1 6) #\s) "kjwjwshcnqwxgwslvl")
  ((policy (range 2 3) #\m) "mmlk")
  ((policy (range 2 3) #\g) "gmgg")
  ((policy (range 3 6) #\r) "rrrvrmr")
  ((policy (range 1 6) #\s) "szgpksczqd")
  ((policy (range 12 14) #\t) "ttttttpjvtqwtltt")
  ((policy (range 1 2) #\r) "hrrrrvrrzj")
  ((policy (range 1 13) #\m) "xmmmmmsmmmcsnmmmmfmm")
  ((policy (range 10 11) #\f) "fffffffffsf")
  ((policy (range 13 15) #\d) "ddddddddddddhdq")
  ((policy (range 7 14) #\k) "jkkkkkkkrkmkkkk")
  ((policy (range 6 9) #\g) "ggggggggg")
  ((policy (range 14 16) #\l) "lllllllvlllllwtx")
  ((policy (range 1 3) #\g) "ggggt")
  ((policy (range 3 4) #\m) "mmkd")
  ((policy (range 5 10) #\g) "jgsgvggwggj")
  ((policy (range 17 18) #\p) "ppppppppppppppphpp")
  ((policy (range 1 2) #\q) "qqjwhq")
  ((policy (range 2 6) #\q) "qqgqqzk")
  ((policy (range 11 16) #\t) "ttwttttttctttthgt")
  ((policy (range 1 5) #\m) "zmmmsm")
  ((policy (range 12 16) #\r) "rrrrrrrrrqrgrrrj")
  ((policy (range 4 5) #\v) "xcvvghvrczpcgn")
  ((policy (range 1 3) #\x) "xbfmxxfxxf")
  ((policy (range 3 5) #\j) "hjbjm")
  ((policy (range 5 14) #\l) "lllltlllllqllll")
  ((policy (range 3 4) #\x) "xsxnxgtx")
  ((policy (range 6 18) #\g) "gggggggggggggggggd")
  ((policy (range 8 12) #\t) "tgttttqtcttttt")
  ((policy (range 4 6) #\p) "pppppd")
  ((policy (range 6 7) #\x) "xxxxxxx")
  ((policy (range 3 4) #\c) "fdcclrccccvg")
  ((policy (range 11 12) #\v) "vnvvvvzdvcrk")
  ((policy (range 1 9) #\z) "zztzqzzzzcz")
  ((policy (range 1 12) #\v) "vmvvvvvvvvvmvvvv")
  ((policy (range 9 16) #\v) "vvvvvvvvvvvvvvvf")
  ((policy (range 2 7) #\h) "xhtsfmvthhhrhdhhbbc")
  ((policy (range 5 6) #\c) "qkccccd")
  ((policy (range 8 12) #\z) "zzzzzzzjzzhbzz")
  ((policy (range 2 4) #\n) "shlnnkxn")
  ((policy (range 19 20) #\v) "vvvvvvvvvvvvvvvvvvcv")
  ((policy (range 7 8) #\h) "hhkhhhkhh")
  ((policy (range 5 19) #\p) "sxslgwvkszswqxtqpvf")
  ((policy (range 5 8) #\m) "mmmmmrmbm")
  ((policy (range 2 5) #\j) "jrnjjzjfhjdkjqjtkwk")
  ((policy (range 7 13) #\f) "ffffffsfffffwfff")
  ((policy (range 6 9) #\w) "wmwwdwwwsz")
  ((policy (range 12 15) #\f) "nsffpfsdsjsfsffk")
  ((policy (range 13 16) #\k) "kkkkkkkkkkkkkkhw")
  ((policy (range 6 17) #\s) "ssssslssssssssssss")
  ((policy (range 3 5) #\c) "srdscnncclqqcncsw")
  ((policy (range 9 11) #\p) "pkppppppppp")
  ((policy (range 5 10) #\q) "qqqqqqqqqpqq")
  ((policy (range 12 14) #\n) "nnnnnnnnnnnnnn")
  ((policy (range 6 11) #\h) "hhhhhqhhhhhvhhhh")
  ((policy (range 2 12) #\f) "fgfrpzcstfffffxff")
  ((policy (range 2 12) #\n) "nnxmnmnnnnknrpmnv")
  ((policy (range 6 7) #\m) "jmmmmcnpkm")
  ((policy (range 4 6) #\v) "mvkcwvmvvjvrvlv")
  ((policy (range 18 20) #\h) "hhrmbrhhhhlhhvhmhhhb")
  ((policy (range 9 13) #\g) "ggggggzgggsgfgxg")
  ((policy (range 9 15) #\m) "pmmxmmmmmmmmmmm")
  ((policy (range 1 7) #\l) "zlmsmlxpvvlzv")
  ((policy (range 2 15) #\g) "sslggkdglqgxpgkx")
  ((policy (range 17 18) #\c) "ccccccccccccccccmc")
  ((policy (range 10 11) #\w) "wwwwwpcwwwpwwd")
  ((policy (range 3 5) #\w) "wwwwk")
  ((policy (range 8 9) #\f) "vfbvffsfcf")
  ((policy (range 4 6) #\r) "rrrrrr")
  ((policy (range 2 4) #\n) "nrnknn")
  ((policy (range 1 4) #\l) "lxllllll")
  ((policy (range 10 11) #\f) "qdfwvfffdfvwffgfkfgf")
  ((policy (range 3 4) #\b) "xgxbdqxbfvzrl")
  ((policy (range 2 8) #\b) "pbbkbbgbxr")
  ((policy (range 1 2) #\d) "ddxdnv")
  ((policy (range 4 8) #\d) "dndfcnhd")))

(def-thunk (! lines s)
  (! <<n
     list<-colist 'o
     sep-by #\newline 'o
     colist<-list s))

(def/copat (! unyes)
  [((list 'yes x)) (ret x)])


;; (def-thunk (! parse s)
;;   (! <<v
;;    map (~! <<v unyes 'o firstParseAll linep) 'o
;;    lines 'o
;;    string->list s))


(def-thunk (! part-a)
  (! count-valid-passwords valid-password-a (~! colist<-list parsed-input)))

(def-thunk (! part-b)
  (! count-valid-passwords valid-password-b (~! colist<-list parsed-input)))

;; (! displayall 'parsing-now)
;; (! parse full-input)
