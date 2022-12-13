type command =
  | ChangeDirectory(string)
  | ListContents;

type file = {
  name: string,
  size: int
};

type directory = {
  name: string,
  size: option(int),
  children: list(fileSystem)
}
and fileSystem =
  | Directory(directory)
  | File(file);

type line =
  | Command(command)
  | FileSystem(fileSystem);

// for prototyping and debugging
let prettyPrintLine =
  fun
  | Command(ChangeDirectory(target)) =>
    {j|cd $target|j}

  | Command(ListContents) =>
    "ls"

  | FileSystem(Directory({name, size, children})) =>
    {j|dir(name=$name, size=$size, children=$children)|j}

  | FileSystem(File({name, size})) =>
    {j|file(name=$name, size=$size)|j}

let parseLine =
  fun
  | [ "$", "cd", target ] =>
    Command(ChangeDirectory(target))

  | [ "$", "ls" ] =>
    Command(ListContents)

  | [ "dir", name ] =>
    FileSystem(Directory({
      name,
      size: None,
      children: []
    }))

  | [ size, name ] when size |> Int.fromString |> Option.isSome =>
    FileSystem(File({
      name,
      size:
        size
        |> Int.fromString
        |> Option.getOrThrow
    }))

  | _ => raise(Failure("Check your assumptions because they're wrong!"))

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