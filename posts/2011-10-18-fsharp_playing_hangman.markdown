---
tags: f#, .net, programming
author: Gregor Uhlenheuer
title: F# - Playing Hangman
summary: Today I tried to program the simple game "Hangman" in F# in order to
         get more comfortable with the functional programming style and with F#
         itself.
---
Today I made some effort to implement a very simple version of the game
"[Hangman][1]" in F#. Even though I am already familiar with the general syntax
of F# and do understand some basic functional programming styles I have to
admit that it's not that easy to get rid of usual iterative programming
behaviors I am so used to.

Without further ado, this is what I came up with so far:

~~~ {.fsharp}
open System
open System.Text
open System.Text.RegularExpressions

/// Replace the non-guessed letters with an underscore
let replace (word : string) (letters : char list) =
    let sb = StringBuilder(word.Length)
    let contains c list = list |> List.exists (fun x -> x = c)
    word.ToCharArray()
    |> Array.iter (fun l -> sb.Append(if contains l letters then l else '_') |> ignore)
    sb.ToString()

/// Determine whether the word was guessed
let solved (word : string) =
    let rgx = Regex(@"^[^_]+$")
    rgx.IsMatch(word)

/// Start the Hangman game with a maximum number of attempts
/// and a given word to guess
let hangman max (word : string) =
    printfn "Hangman: %s" (replace word [])
    let rec hangman' attempts (word : string) (letters : char list) =
        if attempts = 0 then printfn "You lost the game"
        else
            printf "Attempts left %d: " attempts
            let input = Console.ReadKey(true).KeyChar
            let ls = input :: letters
            let rep = replace word ls
            if solved rep then
                printfn "%s" word
            else
                printfn "%s" rep
                hangman' (attempts-1) word ls
    hangman' max word []
~~~

I am pretty sure that there are much more elegant ways to implement this in F#.
So if you have got any remarks or suggestions on how to improve this, I am very
much interested in your opinion. So feel free to [email][2] me.

By the way, I did program on my linux machine at home using [MonoDevelop][3] with
the great [F# bindings][4] [^1] mainly
written by [Tomas Petricek][5]. Go check that out if you are running linux or MacOS
- it does work very well especially regarding that it's completely open-source.

[^1]: see <http://functional-variations.net>

[1]: http://en.wikipedia.org/wiki/Hangman_(game)
[2]: mailto:gregor@uhlenheuer.net
[3]: http://monodevelop.com
[4]: https://github.com/fsharp/fsharpbinding
[5]: http://tomasp.net
