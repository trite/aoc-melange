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

let newDirectory = Directory(String.Map.make());

let fsInsert = (~insertAt: path, ~newItem: fileSystem, ~newItemName: string, fs: fileSystem) => {
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

  go(insertAt, newItem, newItemName, fs)
};

// let prettyPrintFS = id; // TODO: recurse filesystem and emit lines that then get joined into a list to display
// let rec prettyPrintFS = fs => {
//   fun
//   | File(size) => {j|(file, size=$size)|j}
//   | Directory()
// }
let prettyPrintFS = fs => {
  let rec go = (name, depth, fs) => {
    let spaces = String.repeat(depth-1, "  ");

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
};


// let runLine = (ln: line, fs: fileSystem) =>
  
//   ;

let doWork = (_description, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(
    String.splitList(~delimiter=" ")
    >> parseLine)
  |> List.forEach(prettyPrintLine >> Js.log); // to visualize

Shared.File.read("data/2022/day07test.txt")
|> doWork("Part 1 Test  ");

// Shared.File.read("data/2022/day07.txt")
// |> doWork("Part 1 Result");

// Shared.File.read("data/2022/day07test.txt")
// |> doWork("Part 2 Test  ");

// Shared.File.read("data/2022/day07.txt")
// |> doWork("Part 2 Result");