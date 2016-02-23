namespace Relay

open System
open System.Collections.Generic

[<AutoOpen>]
module Prelude =

  /// True if s is null or whitespace.
  let inline isEmpty s = String.IsNullOrWhiteSpace(s)
  
  /// Builds a modifiable lookup table from a sequence of key/value pairs.
  let inline rwdict (pairs : seq<'k * 'v>) : IDictionary<'k,'v> = upcast Dictionary<'k,'v>(dict pairs)

  type IDictionary<'k,'v> with
    /// Gets the value associated with the specified key, or None if key not found.
    member inline x.TryGet key =
      match x.TryGetValue key with
      | true, v -> Some v
      | _ -> None

  /// Converts the result of a TryParse() method to an Option.
  let inline tryParse (s : string) : ^o option =
    let mutable o = Unchecked.defaultof<(^o)>
    if (^o : (static member TryParse : string * ^o byref -> bool) (s, &o))
    then Some o
    else None

  /// Converts the result of a TryParse-type method to an Option. Use tryParse if possible.
  let inline toOption parse str = 
    match parse str with
    | true, v -> Some v
    | _ -> None

  /// Returns Some value, otherwise returns default.
  let inline orDefault ``default`` value = defaultArg value ``default``
  
  let inline (|Default|) defaultValue input = defaultArg input defaultValue

  type MaybeBuilder() = 
    member __.Bind(v, f) = Option.bind f v
    member __.Return v = Some v
    member __.ReturnFrom o = o
  let maybe = MaybeBuilder()
  
  type ChoiceBuilder() = 
    member __.Bind(m, f) = Option.bind f m
    member __.Return(x) = x
    member __.ReturnFrom(f) = f()
    member __.Zero() = None
    
    member __.Combine(a, b) = 
      match a with
      | Some _ -> a // if a is good, skip b
      | None -> b() // if a is bad, run b
    
    member __.Delay(f) = f
    member __.Run(f) = f()  
  let choice = new ChoiceBuilder()
  
  module List = 
    /// Converts an empty list to None, otherwise Some list.
    let toOption = 
      function
      | [] -> None
      | ls -> Some ls
  
  let (|Success|Failure|) = 
    function 
    | Choice1Of2 x -> Success x
    | Choice2Of2 x -> Failure x

  let inline Success x = Choice1Of2 x
  let inline Failure x = Choice2Of2 x
  
  let isSuccess = 
    function 
    | Success _ -> true
    | _ -> false
  
  let isFailure = 
    function 
    | Failure _ -> true
    | _ -> false

  /// Invokes a function, returning the result and elapsed TimeSpan.
  let timed f =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let v = f()
    stopwatch.Stop()
    v, stopwatch.Elapsed

[<AutoOpen>]
module Uri =

  let (|AbsoluteUri|RelativeUri|InvalidUri|) = function
  | a when Uri.IsWellFormedUriString(a, UriKind.Absolute) -> AbsoluteUri (Uri a)
  | r when Uri.IsWellFormedUriString(r, UriKind.Relative) -> RelativeUri (Uri r)
  | _ -> InvalidUri

  let toAbsoluteOption = function
  | AbsoluteUri u -> Some u
  | _ -> None

  type Uri with
    /// Gets the part of the URI before the last '/' character.
    member x.FirstPart =
      let uri = x.AbsoluteUri
      match uri.LastIndexOf '/' with
      | i when i > 0 -> uri.Substring(0, i)
      | _ -> String.Empty

    /// Gets the remainder of the URI after the last '/' character.
    member x.LastPart =
      let uri = x.AbsoluteUri
      match uri.LastIndexOf '/' with
      | i when i > 0 && uri.Length > i -> uri.Substring(i + 1)
      | _ -> String.Empty

  type Identifier = private Url of string | Urn of string
  let (|Url|Urn|NotUri|) = function
    | AbsoluteUri u when u.Scheme = "urn" -> Urn u
    | AbsoluteUri u -> Url u
    | _ -> NotUri
