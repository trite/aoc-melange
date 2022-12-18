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

type directory = {
  size: option(int),
  contents: String.Map.t(fileSystem)
}
and fileSystem =
  | File(int)
  | Directory(directory);

type path = list(string);

let prettyPrintPath =
  List.reverse
  >> List.String.joinWith("/")
  >> (++)("/");

let rootPath = [];

let emptyDirContents = String.Map.make();
let newDirectory = Directory({size: None, contents: emptyDirContents});

let fsInsert = (~insertAt: path, ~newItem: fileSystem, ~newItemName: string, fs: fileSystem) => {
  // let friendlyPath = insertAt |> prettyPrintPath;
  // Js.log({j|fsInsert ($friendlyPath, $newItemName)|j});

  let rec go = (remaining: path, newItem: fileSystem, newItemName: string, fs: fileSystem): fileSystem => {
    switch((remaining |> List.uncons, fs)) {
    | (Some((dirName, restOfPath)), Directory({size, contents: toUpdate})) =>
      Directory({
        size,
        contents:
          toUpdate
          |> String.Map.update(
            dirName,
            fun
            | None                => Some(go(restOfPath, newItem, newItemName, newDirectory))
            | Some(nextLevelDown) => Some(go(restOfPath, newItem, newItemName, nextLevelDown))
        )
      })
    | (None, Directory({size, contents: toUpdate})) =>
      Directory({
        size,
        contents:
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
      })
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
    | Directory({size, contents}) =>
      contents
      |> String.Map.foldLeft(
        (acc, s, v) => List.concat(acc, go(s, depth+1, v)),
        [{j|$(spaces)- $name (dir, size=$size)|j}])
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

let rec calculateDirSizes = 
  fun
  | File(size) => File(size)
  | Directory({size, contents}) =>
    switch(size) {
    | Some(_) => Directory({size, contents})
    | None => {
      let getContentsTotal = (x: String.Map.t(fileSystem)) =>
        x
        |> String.Map.foldLeft((acc, _, v: fileSystem) => 
          switch(v) {
          | File(size) => size
          | Directory({size: Some(size), _}) => size
          | Directory({size: None, _}) => 
            raise(Failure("These should all be Some now..."))
          }
          |> (+)(acc)
          , 0);

      let newContents =
        contents
        |> String.Map.map(fs => calculateDirSizes(fs));

      Directory({
        size:
          newContents
          |> getContentsTotal
          |> Option.pure,
        contents: newContents
      })
    }};

let mapFsToSize =
  fun
  | File(_) => 0
  | Directory({size, _}) =>
    switch(size) {
    | Some(x) when x <= 100000 => x
    | Some(_)
    | None => 0
    };

let flattenDirs = (fs): list(directory) => {
  let rec go = (fs) =>
    switch(fs) {
    | File(_) => []
    | Directory({size, contents}) =>
      contents
      |> String.Map.values
      |> List.flatMap(x => go(x))
      |> List.cons({size, contents: emptyDirContents})
    };

  go(fs)
};

let part1 =
  flattenDirs
  >> List.map(({size, contents: _}) => size |> Option.getOrThrow)
  >> List.filter(x => x <= 100000)
  >> List.Int.sum;

// Part 2

let getTargetAmount = fs =>
  switch(fs) {
  | File(_) => raise(Failure("This should be a directory!"))
  | Directory({size, contents:_}) =>
    (30000000 - (70000000 - (size |> Option.getOrThrow)), fs)
  };

// TODO: I feel like this exists somewhere in Relude already
//       Search more, maybe add it if not? (and equivalent mirrored)
let mapSecondTuple = (f, (x, y)) =>
  (x, y |> f);

let findSmallestFolder = ((minFolderSize, fs)) =>
    fs
    |> List.mapOption(({size, contents: _}) =>
      size
      |> Option.flatMap(s => 
        (s >= minFolderSize)
        ? Some(s)
        : None)
    )
    |> List.sortBy(Int.compare)
    |> List.head
    |> Option.getOrThrow;

let part2 =
  getTargetAmount
  >> mapSecondTuple(flattenDirs)
  >> findSmallestFolder;

// Program boundry IO

let doWork = (description, partSpecificStuff, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(
    String.splitList(~delimiter=" ")
    >> parseLine)
  |> runLines
  |> calculateDirSizes
  |> partSpecificStuff
  |> Int.toString
  |> Shared.Log.logWithDescription(description)

Shared.File.read("data/2022/day07test.txt")
|> doWork("Part 1 Test  ", part1);

Shared.File.read("data/2022/day07.txt")
|> doWork("Part 1 Result", part1);

Shared.File.read("data/2022/day07test.txt")
|> doWork("Part 2 Test  ", part2);

Shared.File.read("data/2022/day07.txt")
|> doWork("Part 2 Result", part2);

/*
$ node _build/default/src/2022/Day07.bs.js
Part 1 Test   : 95437
Part 1 Result : 1141028
Part 2 Test   : 24933642
Part 2 Result : 8278005
*/