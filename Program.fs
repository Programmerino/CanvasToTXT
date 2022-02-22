open FSharpPlus
open FSharp.Data
open System
open Helpers
open Helpers.Extensions

type CanvasResults = HtmlProvider<"data/page.html", PreferOptionals=true>

let shuffle seed xs =
    let rnd = Random(seed)
    xs |> List.sortBy (fun x -> rnd.Next())

let cleanQuestionText =
    replace "\n" " "
    >> replace "\t" " "
    >> replace "\u00a0" " "
    >> replace "  " " "
    >> String.trimWhiteSpaces

module Answer =

    type MultipleChoiceAns = { Correct: string; Wrong: string list }

    type MultipleAnswers =
        { Correct: string list
          Wrong: string list }

    type Match =
        { Prompt: string
          Correct: string
          Others: string list }

    type MatchingAnswer =
        { QuestionText: string
          Matches: Match list }


    type Answer =
        | MultipleChoiceAns of MultipleChoiceAns
        | MultipleAnswers of MultipleAnswers
        | MatchingAnswer of MatchingAnswer
        | StringPass of string

    let (|TrueFalseQuestion|MultipleChoiceQuestion|MultipleAnswersQuestion|MultipleDropdownsQuestion|MatchingQuestion|Unknown|)
        (node: HtmlNode)
        =
        let classes = node.AttributeValue "class"

        if node.HasClass "true_false_question" then
            TrueFalseQuestion
        else if node.HasClass "multiple_choice_question" then
            MultipleChoiceQuestion
        else if node.HasClass "multiple_answers_question" then
            MultipleAnswersQuestion
        else if node.HasClass "multiple_dropdowns_question" then
            MultipleDropdownsQuestion
        else if node.HasClass "matching_question" then
            MatchingQuestion
        else
            Console.WriteLine $"Unknown question type {classes}"
            Unknown

    let (|Checked|Unchecked|) (x: HtmlNode) =
        if (x.CssSelect("input") |> head)
            .HasAttribute("checked", "") then
            Checked
        else
            Unchecked


    let multipleAnswersCreate (x: HtmlNode) correct sharingCorrect : MultipleAnswers option =
        monad {

            let! _ =
                Option.assertion (
                    ((not correct) && (not sharingCorrect))
                    => (x.CssSelect(".answer") |> length = 2)
                )
                |> Option.log "Correct answers in wrong multiple answers cannot be ascertained with more than 2 answers"

            let (correct, incorrect) =
                x.CssSelect(".answer")
                |> List.ofSeq
                |> List.choose (fun x ->
                    let sans = x.CssSelect(".select_answer") |> head

                    let text =
                        (x.CssSelect ".answer_text" |> head)
                            .DirectInnerText()

                    if sharingCorrect then
                        if x.HasClass "correct_answer" then
                            Some(Choice1Of2(text))
                        else if x.HasClass "incorrect_answer" then
                            failwith "Don't know the semantics of this (yet...)"
                            Some(Choice2Of2(text))
                        else
                            Some(Choice2Of2(text))
                    else
                        match sans with
                        | Checked when correct -> Some(Choice1Of2(text))
                        | Unchecked when (not correct) -> Some(Choice1Of2(text))
                        | Unchecked when correct -> Some(Choice2Of2(text))
                        | Checked when (not correct) -> Some(Choice2Of2(text))
                        | _ -> failwith "impossible")
                |> List.partitionMap id

            return { Correct = correct; Wrong = incorrect }
        }

    let multipleChoiceCreate (x: HtmlNode) correct sharingCorrect : MultipleChoiceAns option =
        monad {
            let! ans = multipleAnswersCreate x correct sharingCorrect

            let! _ =
                Option.assertion (ans.Correct |> length = 1)
                |> Option.log "Multiple choice question must have exactly one correct answer"

            return
                { Correct = ans.Correct |> head
                  Wrong = ans.Wrong }
        }


    let multipleDropdownsCreate (x: HtmlNode) correct sharingCorrect : string option =
        monad {
            let! _ =
                Option.assertion (not sharingCorrect)
                |> Option.log "sharingCorrect not implemented for multipleDropdownsCreate"

            let! _ =
                Option.assertion correct
                |> Option.log "Multiple dropdowns question must be correct"

            let correctAnswers =
                x.CssSelect(".answers .select_answer label")
                |> Seq.map (fun x ->
                    x.AttributeValue "for"
                    |> String.replace "answer-" "")

            let! qtext =
                x.CssSelect(".question_text")
                |> tryHead
                |> Option.log "No question text element found"

            let (str, inputs) =
                qtext.Descendants()
                |> Seq.fold
                    (fun (str, look, ignoreNext, inputs) x ->
                        if x.HasClass "question_input" then
                            let options = x.CssSelect("option")

                            (str
                             ++ (options
                                 |> Seq.find (fun x ->
                                     correctAnswers
                                     |> Seq.contains (x.AttributeValue("value"))))
                                 .InnerText()
                             ++ " (",
                             (options |> rev |> head).AttributeValue "value",
                             2,
                             inputs)
                        else if (x.Attributes()
                                 |> Seq.exists (fun x -> x.Name() = "value")) then
                            if x.AttributeValue "value" = "" then
                                (str, look, 2, inputs + 1)
                            else if look = x.AttributeValue "value" then
                                (str ++ $"{x.InnerText()})", "", 1, inputs + 1)
                            else
                                (str ++ $"{x.InnerText()}/", look, 2, inputs + 1)
                        else if (x.Elements() |> length = 0) && (ignoreNext < 1) then
                            (str
                             ++ (x.InnerText()
                                 |> String.replace "[\"" ""
                                 |> String.replace "\", \"" ""
                                 |> String.replace "\"]" " "),
                             "",
                             ignoreNext - 1,
                             inputs)
                        else
                            (str, look, ignoreNext - 1, inputs)

                        )
                    ("", "", -1, 0)
                |> (fun (x, _, _, y) -> x, y)


            // let! _ =
            //     Option.assertion (inputs <> 0)
            //     |> Option.log
            //         "Dropdown question had no inputs. Creating true/false questions out of available information"

            match inputs with
            | 0 ->
                printfn "Dropdown question had no inputs. Creating \"bracket\" questions out of available information"

                qtext.Descendants()
                |> Seq.fold
                    (fun (str, skipNext) x ->
                        match (x.Name(), skipNext) with
                        | ("span", _) -> ($"{str}[{x.DirectInnerText()}]", true)
                        | (_, false) -> ($"{str}{x.DirectInnerText()}", false)
                        | _ -> ($"{str}", false))
                    ("", false)
                |> fst
            | _ -> str
            |> cleanQuestionText

        }

    let matchingCreate (x: HtmlNode) correct sharingCorrect : MatchingAnswer option =
        monad {
            let! _ =
                Option.assertion (not sharingCorrect)
                |> Option.log "sharingCorrect not implemented for matching"

            let! _ =
                Option.assertion correct
                |> Option.log "Matching question must be correct"

            let! qtextElem =
                x.CssSelect(".question_text")
                |> tryHead
                |> Option.log "No question text element found"

            let qtext = qtextElem.InnerText()

            let matches =
                x.CssSelect(".matching_answer")
                |> map (fun x ->
                    (x.CssSelect("div")
                     |> map (fun x -> x.DirectInnerText())
                     |> sum
                     |> String.trimWhiteSpaces,
                     x.CssSelect(".question_input option")
                     |> map (fun x -> x.DirectInnerText(), x.HasAttribute("selected", ""))
                     |> toList
                     |> Option.asserttap (length >> (<) 0)
                     |> Option.log "No options found for matching answer, skipping"))
                |> choose (fun (x, y) ->
                    match y with
                    | Some y -> Some(x, y)
                    | None -> None)
                |> map (fun (x, y) ->
                    { Prompt = x
                      Correct =
                        y
                        |> List.find (fun (_, selected) -> selected)
                        |> (fun (x, _) -> x)
                      Others =
                        y
                        |> List.filter (fun (_, selected) -> not selected)
                        |> map (fun (x, _) -> x) })

            { QuestionText = cleanQuestionText qtext
              Matches = matches |> Seq.toList }
        }


    let createAnswer (x: HtmlNode) correct showingAnswers : Answer option =
        match x with
        | TrueFalseQuestion
        | MultipleChoiceQuestion ->
            multipleChoiceCreate x correct showingAnswers
            |> map (MultipleChoiceAns)
        | MultipleAnswersQuestion ->
            multipleAnswersCreate x correct showingAnswers
            |> map (MultipleAnswers)
        | MultipleDropdownsQuestion ->
            multipleDropdownsCreate x correct showingAnswers
            |> map (StringPass)
        | MatchingQuestion ->
            matchingCreate x correct showingAnswers
            |> map (MatchingAnswer)
        | Unknown -> None

    let listAnswers (x: Answer) : String list =
        match x with
        | MultipleChoiceAns x -> x.Correct :: x.Wrong
        | MultipleAnswers x -> x.Correct ++ x.Wrong
        | StringPass x -> failwith "listAnswers unimplemented for StringPass"
        | MatchingAnswer x -> failwith "listAnswers unimplemented for MatchingAnswer"
        |> shuffle Environment.TickCount

open Answer

let toAscii x =
    x
    |> String.replace "↓" "_down"
    |> String.replace "↑" "^"

module Question =
    type Question =
        { QuestionText: string
          Answer: Answer
          Correct: bool }

    let questionName (x: HtmlNode) =
        x
            .CssSelect(".question_name")
            .Head.DirectInnerText()

    let pointCorrectness (x: HtmlNode) : bool option =
        monad {
            let! userPointsElem =
                x.CssSelect ".user_points"
                |> tryHead
                |> Option.log "Could not find user points"

            let! userPoints =
                userPointsElem.DirectInnerText()
                |> String.replace "pts" ""
                |> String.replace " " ""
                |> Option.protect float
                |> Option.log $"Could not parse user points {userPointsElem.DirectInnerText()}"

            let! questionPointsElem =
                x.CssSelect(".question_points")
                |> tryHead
                |> Option.log "Could not find question points"

            let! questionPoints =
                questionPointsElem.DirectInnerText()
                |> String.replace " " ""
                |> String.replace "/" ""
                |> Option.protect float
                |> Option.log "Could not parse question points"

            userPoints = questionPoints
        }


    let toQuestion (x: HtmlNode) showingAnswers : Question option =
        monad {
            let! correct =
                (if x.HasClass "correct" then
                     Some true
                 else if x.HasClass "incorrect"
                         || x.HasClass "partial_credit" then
                     Some false
                 else
                     None)
                |> Option.log $"Couldn't directly determine correctness of {questionName (x)}, assessing points"
                |> (fun y ->
                    match y with
                    | Some _ -> y
                    | None -> pointCorrectness x)

            let! _ =
                Option.assertion (x.CssSelect("img") |> length = 0)
                |> Option.log $"{questionName (x)} must not have images"


            let! questionTextElem =
                x.CssSelect(".question_text")
                |> tryHead
                |> Option.log "Could not find question text"

            let questionText =
                questionTextElem.InnerText() |> cleanQuestionText

            let! answer =
                createAnswer x correct showingAnswers
                |> Option.log $"Could not create answers for question {questionName (x)}"

            return
                { QuestionText = questionText
                  Answer = answer
                  Correct = correct }
        }

    let rng = Random()

    let stringify (x: Question) : String list =
        let trueFalse =
            [ "True"; "False" ]
            |> shuffle Environment.TickCount
            |> intercalate "/"

        match x.Answer with
        | MultipleChoiceAns y ->
            let possibleAnswersStr = listAnswers x.Answer |> intercalate "/"

            match x.Correct with
            | true ->
                [ $"{x.QuestionText} ({possibleAnswersStr})? {y.Correct}" ]
                ++ (if rng.Next(1, 10) > 9 then
                        [ $"\n{x.QuestionText}: {y.Correct} ({trueFalse})? True" ]
                    else
                        [ "" ])
            | false -> [ $"{x.QuestionText}: {y.Correct} ({trueFalse})? False" ]
        | MultipleAnswers y ->
            (y.Correct |> List.map (fun x -> x, true))
            ++ (y.Wrong |> List.map (fun x -> x, false))
            |> List.map (fun (z, correct) ->
                let answer = if correct then "True" else "False"
                $"{x.QuestionText}: {z} (True/False)? {answer}")
        | StringPass y -> [ y ]
        | MatchingAnswer y ->
            y.Matches
            |> List.map

                (fun { Prompt = prompt
                       Correct = correct
                       Others = others } ->
                    let answers =
                        correct :: others |> shuffle Environment.TickCount

                    let possibleAnswersStr = answers |> intercalate "/"

                    $"{y.QuestionText}: {prompt} -> {correct}"
                    ++ if (length answers <> 1) then
                           $" ({possibleAnswersStr})"
                       else
                           "")

let vowels =
    set [ 'a'
          'e'
          'i'
          'o'
          'u'
          'A'
          'E'
          'I'
          'O'
          'U' ]

// Based on https://rosettacode.org/wiki/Remove_vowels_from_a_string#F.23
let justVowelsAndParens n =
    n
    |> Seq.filter (fun n ->
        vowels.Contains n
        || [ '('; ')' ] |> Seq.contains n)
    |> Array.ofSeq
    |> String

let inline vowelCount x =
    let vowels =
        set [ 'a'
              'e'
              'i'
              'o'
              'u'
              'A'
              'E'
              'I'
              'O'
              'U' ]

    x |> filter (fun x -> vowels.Contains x) |> length

// 350 character limit
let truncate (x: string) : string option =
    let strlen = (length x)

    if (strlen > 350) then
        let vcount = vowelCount x

        if (strlen - vcount > 350) then
            None
        else
            let removeN = (strlen - 350)

            let removeN =
                if (justVowelsAndParens x).IndexOf '(' < removeN then
                    9999
                else
                    removeN

            x
            |> Seq.fold
                (fun (i, str) x ->
                    if (i < removeN) && (vowels.Contains x) then
                        (i + 1, str.ToString())
                    else
                        (i, str ++ x.ToString()))
                (0, "")
            |> snd
            |> Some
    else
        Some x

[<EntryPoint>]
let main argv =
    monad {
        let doc =
            (Seq.initInfinite (fun _ -> Console.In.ReadLine())
             |> takeWhile ((<>) null)
             |> intersperse "\n"
             |> sum
             |> String.trim [ '\"' ]
             |> CanvasResults.Parse)
                .Html

        let! questions =
            doc.CssSelect("#questions")
            |> tryHead
            |> Option.log "Could not find questions element"

        let! showingAnswers =
            if questions.HasClass("show_correct_answers") then
                Some(true)
            else if questions.HasClass("suppress_correct_answers") then
                Some(false)
            else
                None
                |> Option.log "Could not determine if correct answers are being shown"

        doc.CssSelect ".display_question"
        |> map (fun x -> x, Question.toQuestion x showingAnswers)
        |> List.choose (fun (x1, x2) ->
            x2
            |> Option.log $"###Could not parse question {Question.questionName (x1)}")
        |> bind Question.stringify
        |> List.map (toAscii)
        |> choose (
            truncate
            >> Option.log "###Question could not be reduced to fit size requirements"
        )
        |> filter ((<>) "")
        |> List.iter Console.WriteLine

        0
    }
    |> (function
    | Some x -> x
    | None -> failwith "None value for main execution")
