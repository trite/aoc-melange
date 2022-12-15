type command =
  | ChangeDirectory(string)
  | ListContents;

type fileInfo = {
  name: string,
  size: int
};

type fileSystemInfo =
  | DirectoryName(string)
  | FileInfo(fileInfo);

type line =
  | Command(command)
  | FileSystemInfo(fileSystemInfo);

// let

// let insertOrUpdateFS = (_path: string, _item: fileSystem, _toUpdate: directory) =>
//   raise(Failure("TODO"));



// for prototyping and debugging
let prettyPrintLine =
  fun
  | Command(ChangeDirectory(target)) =>
    {j|cd $target|j}
  | Command(ListContents) =>
    "ls"
  | FileSystemInfo(DirectoryName(name)) =>
    {j|dir(name=$name)|j}
  | FileSystemInfo(FileInfo({name, size})) =>
    {j|file(name=$name, size=$size)|j};

let parseLine =
  fun
  | [ "$", "cd", target ] => Command(ChangeDirectory(target))
  | [ "$", "ls" ] => Command(ListContents)
  | [ "dir", name ] => FileSystemInfo(DirectoryName(name))
  | [ size, name ] when size |> Int.fromString |> Option.isSome =>
    FileSystemInfo(FileInfo({
      name,
      size:
        size
        |> Int.fromString
        |> Option.getOrThrow
    }))
  | _ => raise(Failure("Check your assumptions because they're wrong!"));

type fileSystem =
  | File(int)
  | Directory(String.Map.t(fileSystem));

type path = list(string);

// let prettyPrintPath = List.intersperse("/") >> List.String.join;
// let prettyPrintPath = p => "/" ++ List.String.joinWith("/", p);
let prettyPrintPath =
  List.reverse
  >> List.String.joinWith("/")
  >> (++)("/");

let rootPath = [];

let newDirectory = Directory(String.Map.make());

let fsInsert = (~insertAt: path, ~newItem: fileSystem, ~newItemName: string, fs: fileSystem) => {
  // let friendlyPath = insertAt |> prettyPrintPath;
  // Js.log({j|fsInsert ($friendlyPath, $newItemName)|j});

  let rec go = (remaining: path, newItem: fileSystem, newItemName: string, fs: fileSystem): fileSystem => {
    switch((remaining |> List.uncons, fs)) {
    | (Some((dirName, restOfPath)), Directory(toUpdate)) =>
      Directory(
        toUpdate
        |> String.Map.update(
          dirName,
          fun
          | None                => Some(go(restOfPath, newItem, newItemName, newDirectory))
          | Some(nextLevelDown) => Some(go(restOfPath, newItem, newItemName, nextLevelDown))
        )
      )
    | (None, Directory(toUpdate)) =>
      Directory(
        toUpdate
        |> String.Map.update(
          newItemName,
          fun
          | None => Some(newItem)
          | Some(shouldNotExist) =>
            raise(Failure(
{j|Trying to insert over another value!
Current value: $shouldNotExist
Attempting to insert
  Name: $newItemName
  Item: $newItem|j}))
        )
      )
    | _ => raise(Failure("Time to start troubleshooting!"))
    }
  }

  go(insertAt |> List.reverse, newItem, newItemName, fs)
};

let prettyPrintFS = fs => {
  let rec go = (name, depth, fs) => {
    let spaces = String.repeat(depth, "  ");

    switch(fs) {
    | File(size) => [{j|$(spaces)- $name (file, size=$size)|j}]
    | Directory(contents) =>
      contents
      |> String.Map.foldLeft(
        (acc, s, v) => List.concat(acc, go(s, depth+1, v)),
        [{j|$(spaces)- $name (dir)|j}])
    }
  };

  go("/", 0, fs)
  |> List.toArray
};

let runLines = (lines: list(line)) => {
  let runLine = (line, (currentPath, fs)) =>
    switch(line, currentPath) {
    // cd to root (modify path)
    | (Command(ChangeDirectory("/")), _) => (rootPath, fs)

    // up a directory successfully (modify path)
    | (Command(ChangeDirectory("..")), [_, ...restOfPath]) => (restOfPath, fs)

    // up a directory from root (fail)
    | (Command(ChangeDirectory("..")), []) => raise(Failure("Tried to `cd ..` from root!"))

    // down a directory (modify path)
    | (Command(ChangeDirectory(target)), current) => ([target, ...current], fs)

    // list directory contents (noop)
    | (Command(ListContents), current) => (current, fs)

    // directory info (insert into fs object)
    | (FileSystemInfo(DirectoryName(name)), current) =>
      (current, fsInsert(~insertAt=current, ~newItem=newDirectory, ~newItemName=name, fs))
    
    // file info (insert into fs object)
    | (FileSystemInfo(FileInfo({name, size})), current) =>
      (current, fsInsert(~insertAt=current, ~newItem=File(size), ~newItemName=name, fs))
    };

  let rec go = (lines: list(line), (currentPath: path, fs: fileSystem)) =>
    switch(lines |> List.uncons) {
    | Some((ln, rest)) => go(rest, runLine(ln, (currentPath, fs)))
    | None => fs
    };

  go(lines, (rootPath, newDirectory))
}

/*
$ node _build/default/src/2022/Day07.bs.js
fsInsert (/, a)
fsInsert (/, b.txt)
fsInsert (/, c.dat)
fsInsert (/, d)
fsInsert (/a, e)
fsInsert (/a, f)
fsInsert (/a, g)
fsInsert (/a, h.lst)
fsInsert (/a/e, i)
fsInsert (/d, j)
fsInsert (/d, d.log)
fsInsert (/d, d.ext)
fsInsert (/d, k)
[
  '- / (dir)',
  '  - a (dir)',
  '    - e (dir)',
  '      - i (file, size=584)',
  '    - f (file, size=29116)',
  '    - g (file, size=2557)',
  '    - h.lst (file, size=62596)',
  '  - b.txt (file, size=14848514)',
  '  - c.dat (file, size=8504156)',
  '  - d (dir)',
  '    - d.ext (file, size=5626152)',
  '    - d.log (file, size=8033020)',
  '    - j (file, size=4060174)',
  '    - k (file, size=7214296)'
]
*/

let doWork = (_description, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(
    String.splitList(~delimiter=" ")
    >> parseLine)
  |> runLines
  // |> (fun
  //     | File(_) => [||]
  //     // | Directory(m) =>
  //     //   m
  //     //   |> String.Map.get("a")
  //     //   |> Option.getOrThrow
  //     //   |> (fun
  //     //       | File(_) => [||]
  //     //       | Directory(a) => a |> String.Map.toArray))
  //     | Directory(m) => m |> String.Map.toArray)
  |> prettyPrintFS
  |> Array.forEach(Js.log);
  // |> Js.log;
  // |> List.forEach(prettyPrintLine >> Js.log); // to visualize

// Shared.File.read("data/2022/day07test.txt")
// |> doWork("Part 1 Test  ");

Shared.File.read("data/2022/day07.txt")
|> doWork("Part 1 Result");

// Shared.File.read("data/2022/day07test.txt")
// |> doWork("Part 2 Test  ");

// Shared.File.read("data/2022/day07.txt")
// |> doWork("Part 2 Result");


/* Directory structure of puzzle input for fun
$ node _build/default/src/2022/Day07.bs.js
- / (dir)
  - bzcg (dir)
    - fwmbbvj (dir)
      - dhdgrhg (dir)
        - sfpqf (file, size=296118)
        - znzszz (file, size=96182)
      - fmdbzs (dir)
        - bfcsgnhd.qdd (file, size=35391)
        - cgvwgw (dir)
          - nzmddp (dir)
            - qfzr.wmv (file, size=236634)
        - jrzcqs (dir)
          - cwhfwngj (file, size=167809)
          - mvmbff.frl (file, size=110114)
          - qgzdlv.vdh (file, size=289563)
          - vtjlsltn (dir)
            - jwj (file, size=306424)
            - qgzdlv.vdh (file, size=345576)
          - wcztrpdr.bcz (file, size=332076)
          - zwpjpt (dir)
            - jrzcqs (dir)
              - sfpqf (dir)
                - fgnljzg.zvv (file, size=329091)
        - lbd (file, size=28249)
        - qfzr.wmv (file, size=310945)
        - tgcsgnsd.tcm (file, size=135866)
      - hfczrwl (dir)
        - crdbqlvs (dir)
          - lbd (file, size=87031)
          - znzszz (file, size=2841)
        - jnplb (dir)
          - bzs (file, size=297170)
          - gqlvhlm (file, size=294997)
          - nzmddp (file, size=111010)
        - pldz (file, size=82659)
      - hzdt (dir)
        - bzqlsq.jtn (file, size=197102)
        - qgdncp (dir)
          - ndn (dir)
            - ppfmfq (dir)
              - slcmz.dfc (file, size=176104)
            - qjhq.brj (file, size=104804)
          - sfpqf.tvj (file, size=34679)
      - lchzdqv (dir)
        - pfrdwpsl.swc (file, size=267779)
      - llfhrcjr (dir)
        - jrzcqs.rgs (file, size=58864)
        - nzmddp (file, size=228412)
      - mqb (file, size=276346)
      - nzmddp (dir)
        - cdp (dir)
          - lbd (file, size=226343)
          - znzszz (dir)
            - nzmddp.tcm (file, size=241815)
            - pcvjzhzn.wdg (file, size=68255)
            - qjhq.brj (file, size=252123)
        - dqjrv (dir)
          - bmcsz.jrq (file, size=160039)
          - gnp (dir)
            - dhnwcgb (dir)
              - pff (file, size=119009)
        - qjhq.brj (file, size=171210)
      - nzmddp.hsf (file, size=314933)
      - stg (dir)
        - fntwgzz (dir)
          - lbd (file, size=289241)
        - gtbgr (dir)
          - qgzdlv.vdh (file, size=124648)
      - tgtdlct.qbw (file, size=299329)
    - hclnfzgv.gqb (file, size=286838)
    - mpsthvvc (dir)
      - wpq (file, size=164129)
    - qgzdlv.vdh (file, size=76013)
    - znzszz (file, size=186898)
    - zwmp (dir)
      - cfgg (dir)
        - rcpts.bnc (file, size=139209)
      - hdbqg (file, size=237318)
      - hstqbrr.tdt (file, size=171175)
      - lbd (file, size=306477)
      - qjhq.brj (file, size=286893)
      - sjmdcl (file, size=324660)
      - tlcl (dir)
        - cfgg.rvc (file, size=39093)
  - hrtvrp (dir)
    - bmsw (dir)
      - qgzdlv.vdh (file, size=322182)
      - sfpqf.mrs (file, size=106324)
    - cfgg.btw (file, size=63820)
    - qfzr.wmv (file, size=93558)
  - jvj (dir)
    - cfgg (dir)
      - qgzdlv.vdh (file, size=179999)
    - dppgmmgh (dir)
      - bwwnwt (dir)
        - httvdc.hpw (file, size=131362)
        - qjhq.brj (file, size=76925)
    - hdmc (dir)
      - gghj.bbc (file, size=144982)
      - jrzcqs.ssg (file, size=75348)
      - mqdqjrp (dir)
        - lbd (file, size=102764)
        - rdrgh.brw (file, size=20330)
        - shht.rnq (file, size=84239)
        - vfgmpm.jnq (file, size=284381)
      - qccm (dir)
        - bcjcbh.mbq (file, size=47778)
        - gmsqgg.ncl (file, size=265938)
        - mqb (file, size=83269)
        - qfzr.wmv (file, size=58515)
        - znzszz (file, size=330596)
      - wzs (dir)
        - qjhq.brj (file, size=319883)
    - nntzqnfb (file, size=56099)
    - sfpqf (dir)
      - ccsbqncb (dir)
        - qjhq.brj (file, size=187812)
      - cfgg (dir)
        - cfgg (dir)
          - qjhq.brj (file, size=288249)
        - hpbq (dir)
          - cfw (file, size=342631)
          - czcvlz.hqm (file, size=282426)
        - jrzcqs (dir)
          - ltr (dir)
            - qgzdlv.vdh (file, size=339178)
          - qjhq.brj (file, size=304275)
        - jrzcqs.vcq (file, size=285380)
        - lhzpnhjc (file, size=343520)
        - mqb (file, size=24392)
        - nzmddp (dir)
          - cfgg (dir)
            - cfgg (file, size=162027)
            - cfgg.frf (file, size=9838)
            - dsw.rrf (file, size=113543)
            - jhgcqp.zpv (file, size=288839)
            - znzszz.bvd (file, size=119427)
          - hnp (dir)
            - nhjhcd (file, size=215530)
          - mtzgr (dir)
            - slbt.djp (file, size=138609)
          - nrtvg (dir)
            - gtc (dir)
              - pgd (dir)
                - bsfv (dir)
                  - lwdpmn (dir)
                    - lcjq.llm (file, size=126870)
                  - nzmddp (dir)
                    - fcbrv (file, size=280315)
                    - zqfc.pbm (file, size=271478)
                  - znzszz (dir)
                    - jrzcqs.qfv (file, size=76029)
                    - nzmddp (file, size=176019)
                - lbd (file, size=29468)
              - qfzr.wmv (file, size=202523)
              - smprtrhs (dir)
                - mqb (file, size=233528)
                - qbrrj.bhz (file, size=127142)
                - qjhq.brj (file, size=148724)
              - trppqf (dir)
                - hdg (dir)
                  - sqmnmnhd (dir)
                    - mqb (file, size=102960)
                - nzmddp (dir)
                  - dbh.vsj (file, size=327098)
                  - gblvc.gps (file, size=343700)
                  - qfzr.wmv (file, size=265225)
                  - znzszz.dbv (file, size=199016)
                  - znzszz.plm (file, size=217259)
                - qfzr.wmv (file, size=183245)
            - mbwm (file, size=120784)
            - tsj.qdg (file, size=22036)
          - nzmddp (dir)
            - dprztmqp.hpv (file, size=84041)
            - jrzcqs (dir)
              - glzldmj.rvv (file, size=127029)
              - jtt.lqn (file, size=35266)
              - lbc.nlp (file, size=51031)
              - lbd (file, size=268567)
              - qgzdlv.vdh (file, size=280100)
            - zszhsnb (dir)
              - gssjn.njq (file, size=186962)
              - hcrnn (dir)
                - qfzr.wmv (file, size=188774)
              - hlf.fsq (file, size=263790)
              - hlrjcm (dir)
                - cfgg (dir)
                  - nbrlntm.znr (file, size=70451)
              - jqbwl (dir)
                - znzszz (dir)
                  - mqb (file, size=283986)
              - jrzcqs (dir)
                - whfsbm (file, size=93117)
              - nzmddp (dir)
                - nzmddp (dir)
                  - ndmsh.jln (file, size=163289)
              - nzmddp.nsr (file, size=285517)
              - tshj (dir)
                - jcrmv (dir)
                  - mqb (file, size=288613)
                - qjhq.brj (file, size=240935)
                - srddr (dir)
                  - jpqzbj.rnr (file, size=263797)
                  - jrzcqs.zjn (file, size=43879)
                  - mqb (file, size=230566)
                - znzszz (dir)
                  - ftbpgmjq (file, size=327063)
        - znzszz (dir)
          - vdddqvj (file, size=227452)
      - jrzcqs (dir)
        - qcwndswg.hrp (file, size=104553)
      - nzmddp (dir)
        - qfzr.wmv (file, size=201456)
      - sfpqf (dir)
        - smr (dir)
          - wnjnslvd (dir)
            - ftrvp.pnf (file, size=44407)
            - mjmrgsj (file, size=116039)
            - nzmddp (dir)
              - sfpqf.jjg (file, size=323730)
        - tttvzmnm (dir)
          - csvhmb (dir)
            - qgzdlv.vdh (file, size=299075)
          - dsd (file, size=286656)
          - qfzr.wmv (file, size=71993)
          - znzszz (dir)
            - qgzdlv.vdh (file, size=15914)
  - ltrqb (dir)
    - gzhrbrr (dir)
      - djfrnzfn (file, size=277882)
      - frqmdg (file, size=89527)
      - nzmddp.mwr (file, size=95596)
      - qfzr.wmv (file, size=210985)
      - qgzdlv.vdh (file, size=169282)
    - hbv (file, size=122564)
    - twf (dir)
      - mfd (dir)
        - jdgqf.tqd (file, size=300823)
    - vptsc (dir)
      - cfgg (file, size=156341)
  - msqlnht (dir)
    - dcv (dir)
      - nzmddp.rdn (file, size=201585)
      - sfpqf (file, size=344322)
      - wwdvbggs (file, size=239482)
    - ddbr.zbq (file, size=30956)
    - dht (dir)
      - bwfcs (dir)
        - tjgjdj (dir)
          - dljdshp (dir)
            - wcp.nmg (file, size=19838)
        - znzszz (dir)
          - crwt (dir)
            - nzmddp (file, size=201821)
            - qgzdlv.vdh (file, size=222044)
          - dwdjmnl (dir)
            - swg (file, size=230423)
            - vcvvq.drl (file, size=98135)
          - jrzcqs (dir)
            - zclsmlf.ljp (file, size=227320)
          - rldp (dir)
            - grjsrgcs (file, size=349401)
    - hsn (dir)
      - sfpqf.snp (file, size=256123)
    - qgzdlv.vdh (file, size=42896)
    - sfpqf (file, size=330093)
    - sfpqf.cpt (file, size=262508)
    - sth (dir)
      - cfgg (file, size=151062)
      - dscngvc (dir)
        - gshqbpw.njw (file, size=273344)
        - mqb (file, size=70631)
        - qfzr.wmv (file, size=16181)
        - rmsgtb (dir)
          - gnmf (dir)
            - npvlc.hvq (file, size=47246)
          - qgzdlv.vdh (file, size=324122)
        - sfpqf (file, size=172650)
      - glqq (dir)
        - cfgg.mvw (file, size=66844)
      - hgffpr.lqb (file, size=214524)
      - hqrrrz (dir)
        - lcq (dir)
          - qfzr.wmv (file, size=334646)
        - mqb (file, size=109656)
        - nzh.nhp (file, size=120607)
        - scn.hfh (file, size=229843)
        - shdnj.swg (file, size=149344)
        - tgwtqd.qwd (file, size=169266)
      - qjhq.brj (file, size=5900)
      - qpjnjbzg.fqz (file, size=216813)
      - rlzhq (dir)
        - lbd (file, size=316457)
        - pfl.tjh (file, size=290078)
      - sbv (dir)
        - dmrrn (file, size=158423)
        - nzmddp (dir)
          - jrzcqs (dir)
            - lsjfn (dir)
              - ptrcqt.qqz (file, size=41263)
          - nvsf (dir)
            - wzcjllln.swq (file, size=137050)
          - znzszz.mrm (file, size=194665)
        - qfzr.wmv (file, size=86125)
        - qjhq.brj (file, size=309500)
        - qlqlmqq (file, size=14673)
    - ttqqpqn (file, size=310917)
    - twgmlhtp (dir)
      - qgzdlv.vdh (file, size=284913)
      - rbhnjvrz (dir)
        - vlcm (dir)
          - nqfdtfs.hpc (file, size=71610)
          - nzmddp.mcl (file, size=53860)
  - mvs (dir)
    - cfmqw (dir)
      - qjhq.brj (file, size=236767)
    - dpj (dir)
      - wcgz (dir)
        - jrzcqs (dir)
          - gltbrdl (file, size=47261)
    - znzszz (dir)
      - lbd (file, size=63782)
  - nzmddp (dir)
    - dcbntj (file, size=297761)
    - fpnrzl (dir)
      - jrzcqs (dir)
        - lbd (file, size=117141)
        - znzszz (dir)
          - znzszz (file, size=148863)
      - mpzrrjd (dir)
        - tglsshqj.msb (file, size=287787)
    - jrzcqs (dir)
      - mqb (file, size=28680)
      - pgpctg.tpz (file, size=496)
      - zqcdfq (dir)
        - mqb (file, size=134507)
        - nzmddp (dir)
          - shhttsf (file, size=115602)
    - lbd (file, size=211161)
    - mbqjsfj (dir)
      - cfgg (file, size=8422)
    - nhnnmhj (dir)
      - qgzdlv.vdh (file, size=172791)
    - pwjwc (dir)
      - fwtnnrb (dir)
        - bpvt.gvv (file, size=294370)
        - jrzcqs (dir)
          - bjrw.wfg (file, size=291270)
        - mfvntmf (dir)
          - jclz (file, size=48260)
          - rlv (dir)
            - ddjcz (dir)
              - frtldl.rfz (file, size=135665)
          - sfpqf (file, size=46115)
          - zbs (file, size=46418)
        - qscphr (dir)
          - lrhmvmq (dir)
            - qgzdlv.vdh (file, size=198056)
      - zhdq (dir)
        - cchv (file, size=98139)
        - fqc (file, size=200761)
        - lbd (file, size=118696)
        - mqb (file, size=161939)
        - rrgtws.wjn (file, size=343033)
    - pzptv (dir)
      - hfdzv.rtz (file, size=28931)
      - jrzcqs (file, size=44106)
      - lbd (file, size=9518)
      - mjc (dir)
        - wcqv (dir)
          - fwvf (dir)
            - tqm.gdw (file, size=280201)
      - qjfgzqj (dir)
        - jrzcqs.vpb (file, size=102183)
        - nzmddp (dir)
          - jrzcqs.gpc (file, size=37882)
          - wcc.qcl (file, size=284676)
      - sfpqf (dir)
        - phmwwcnl.wth (file, size=105254)
      - vczrqhtd (dir)
        - drfvdqjc.fph (file, size=35159)
        - pfvwwzfl (dir)
          - bjwdcbpf.wwz (file, size=138311)
        - qgzdlv.vdh (file, size=206822)
        - qjhq.brj (file, size=279335)
        - zjtcbps (dir)
          - ddp.vvv (file, size=169941)
          - jrnggfr.gtf (file, size=346076)
          - rtz (file, size=50876)
      - vrnnn (file, size=90722)
      - znzszz (dir)
        - hcpmqmpv (file, size=324206)
        - jrzcqs (file, size=140802)
    - qfzr.wmv (file, size=104522)
    - qjhq.brj (file, size=80572)
    - sfpqf (dir)
      - dnj.szl (file, size=177463)
      - jrzcqs (dir)
        - glwz (dir)
          - mhrdgfp (dir)
            - lbd (file, size=171876)
          - mwttjss (dir)
            - qgzdlv.vdh (file, size=108737)
            - wndm (dir)
              - qfzr.wmv (file, size=96467)
        - nntbftjw (file, size=267334)
        - plhqw (dir)
          - mqb (file, size=247813)
        - pts (dir)
          - cfgg (dir)
            - rrhf.dtd (file, size=159819)
          - ffprmnh (file, size=247928)
          - fncv.cfd (file, size=308698)
          - tfzn (dir)
            - rbslfmpt.fwc (file, size=42960)
          - vrsh (dir)
            - jddbf (dir)
              - swbtfm (file, size=342843)
        - qcqcnqvp (dir)
          - mlnmbs (dir)
            - qgzdlv.vdh (file, size=211179)
          - mqb (file, size=121264)
          - qfzr.wmv (file, size=132437)
          - sfpqf (dir)
            - nzmddp (dir)
              - jjmc.cth (file, size=221804)
        - sfpqf (file, size=281648)
        - znzszz (dir)
          - cqq (dir)
            - qjhq.brj (file, size=37926)
          - lbd (file, size=261225)
      - mqb (file, size=242840)
      - ntcptsd (dir)
        - lmbznbsd (dir)
          - qfzr.wmv (file, size=67249)
      - pctt.flm (file, size=291995)
      - qfzr.wmv (file, size=205818)
      - rsgdd (dir)
        - lbd (file, size=198925)
      - znzszz.dws (file, size=155626)
    - znzszz (dir)
      - hhggjlpt.ncp (file, size=224738)
      - lqdrnfrt (file, size=215566)
      - lvv.bwz (file, size=257824)
      - vnqfcr.dbh (file, size=2888)
      - znzszz.rqr (file, size=129229)
  - zjvncc (dir)
    - dswznwtf (dir)
      - cfv (file, size=37067)
      - qgzdlv.vdh (file, size=186502)
    - nzmddp.pln (file, size=255036)
    - qsq (dir)
      - qfzr.wmv (file, size=149224)
    - rzbnmn (dir)
      - dhdgvg.lsj (file, size=57606)
      - fpj (dir)
        - jrzcqs.nrn (file, size=212353)
      - jrzcqs (dir)
        - ndchf (file, size=288350)
    - vbr (file, size=312907)
 */