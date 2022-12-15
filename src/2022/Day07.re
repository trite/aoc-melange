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
  let friendlyPath = insertAt |> prettyPrintPath;
  Js.log({j|fsInsert ($friendlyPath, $newItemName)|j});

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

  
//   ;

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
  |> Js.log;
  // |> List.forEach(prettyPrintLine >> Js.log); // to visualize

Shared.File.read("data/2022/day07test.txt")
|> doWork("Part 1 Test  ");

// Shared.File.read("data/2022/day07.txt")
// |> doWork("Part 1 Result");

// Shared.File.read("data/2022/day07test.txt")
// |> doWork("Part 2 Test  ");

// Shared.File.read("data/2022/day07.txt")
// |> doWork("Part 2 Result");