open Core.Std
open Async.Std

type t

val create_log: ?url:Uri.t -> ?level:Log.Level.t ->
                ?tags:String.t List.t -> String.t -> String.t -> Log.t
