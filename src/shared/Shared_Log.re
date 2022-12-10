let (logWithDescription) = (description, toLog) =>
  toLog
  |> (++)(description ++ " : ")
  |> Js.log;