module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { source : String
    , evaluated : Result Error Expression
    }


execute : String -> Result Error Expression
execute source =
    case parse source of
        Ok parsed ->
            case eval initialEnv parsed of
                Ok ( evaluated, _ ) ->
                    Ok evaluated

                Err err ->
                    Err err

        Err err ->
            Err err


discriminantSample : String
discriminantSample =
    """(begin
  (define a 1)
  (define b 4)
  (define c 8)
  (define
    discriminant
    (lambda
      (a b c)
      (- (* b b) (* 4 a c))))
  (discriminant a b c))
"""


initialModel : Model
initialModel =
    Model discriminantSample (execute discriminantSample)


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = Run
    | UpdateSource String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( { model | evaluated = execute model.source }, Cmd.none )

        UpdateSource newSource ->
            ( { model | source = newSource }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-primary" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ] [ text "Lispy Interpreter" ]
                    , h2 [ class "subtitle" ] [ text "Tiny LISP Interpreter written in Elm." ]
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text "Program" ]
                    , div [ class "control" ]
                        [ textarea [ class "textarea", onInput UpdateSource, value model.source, rows 15 ] [ text model.source ]
                        ]
                    ]
                ]
            , div [ class "column" ]
                [ div [ class "field" ]
                    [ label [ class "label" ] [ text "Evaluated" ]
                    , div [ class "control" ]
                        [ textarea
                            [ class "textarea"
                            , rows 15
                            , value
                                (case model.evaluated of
                                    Ok exp ->
                                        expressionToString exp

                                    Err err ->
                                        errorToString err
                                )
                            ]
                            [ case model.evaluated of
                                Ok exp ->
                                    text (expressionToString exp)

                                Err err ->
                                    text (errorToString err)
                            ]
                        ]
                    ]
                ]
            ]
        , button [ class "button is-primary", onClick Run ] [ text "Run" ]
        , h3 [ class "title is-3" ] [ text "Samples" ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ button [ class "button is-info", onClick (UpdateSource discriminantSample) ] [ text "Discriminant" ]
                ]
            , div [ class "column" ]
                [ button [ class "button is-info", onClick (UpdateSource fibonacciSample) ] [ text "Fibonacci" ]
                ]
            , div [ class "column" ]
                [ button [ class "button is-info", onClick (UpdateSource reverseListSample) ] [ text "Reverse List" ] ]
            , div [ class "column" ]
                [ button [ class "button is-info", onClick (UpdateSource fibonacciListSample) ] [ text "Fibonacci List" ] ]
            ]
        , div []
            [ h3 [ class "title is-3" ] [ text "Description" ]
            , div [ class "content" ] [ text "This is a tiny Lisp interpreter written in ", a [ href "https://elm-lang.org/", target "_blank" ] [ text "Elm" ], text "." ]
            , div [ class "content" ] [ text "The tiny Lisp is based on ", a [ href "https://norvig.com/lispy.html", target "_blank" ] [ text "Lispy, which is described by Peter Norvig" ], text ". But this implementation has several restrictions such as no floating point numbers." ]
            , div [ class "content" ] [ text "Source code is available at ", a [ href "https://github.com/tomotakatakahashi/lispy-elm" ] [ text "GitHub" ], text "." ]
            ]
        ]


fibonacciSample : String
fibonacciSample =
    """(begin
  (define n 7)
  (define
    fibonacci
    (lambda
      (n)
      (if
        (or (= n 0) (= n 1))
        1
        (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
  (fibonacci n))
"""


reverseListSample : String
reverseListSample =
    """(begin
  (define lst (quote (1 2 3 4 5 6 7 8 9 10)))
  (define reverseListHelper
    (lambda (ret arg)
      (if (not arg) ret
        (reverseListHelper (cons (car arg) ret) (cdr arg)))))
  (define reverseList
    (lambda (lst) (reverseListHelper nil lst)))
  (reverseList lst))
"""


fibonacciListSample : String
fibonacciListSample =
    """(begin
  (define listSize 20)
  (define reverseListHelper
    (lambda (ret arg)
      (if (not arg) ret
        (reverseListHelper (cons (car arg) ret) (cdr arg)))))
  (define reverseList
    (lambda (lst) (reverseListHelper nil lst)))
  (define reversedFibonacciList
    (lambda (size)
      (if (<= size 0) (quote ())
        (if (= size 1) (quote (1))
          (if (= size 2) (quote (1 1))
            (begin
              (define prev (reversedFibonacciList (- size 1)))
              (cons (+ (car prev) (car (cdr prev))) prev)))))))
  (define fibonacciList (lambda (size) (reverseList (reversedFibonacciList size))))
  (fibonacciList listSize))
"""


errorToString : Error -> String
errorToString error =
    case error of
        ParseError err ->
            "Parse Error: " ++ err

        EvalError err ->
            "Evaluation Error: " ++ err


expressionToString : Expression -> String
expressionToString exp =
    case exp of
        Constant val ->
            String.fromInt val

        Symbol sym ->
            sym

        Function _ ->
            "Function"

        ExpressionList lst ->
            "( " ++ String.join " " (List.map expressionToString lst) ++ " )"

        LambdaExpression vars valExp ->
            "Lambda " ++ expressionToString (ExpressionList vars) ++ " " ++ expressionToString valExp

        TrueExpression ->
            "#t"



-- Lispy implementation


eval : Environment -> Expression -> Result Error ( Expression, Environment )
eval env exp =
    case exp of
        Symbol var ->
            symbol env var

        Constant _ ->
            Ok ( exp, env )

        ExpressionList lst ->
            case lst of
                (Symbol "quote") :: tail ->
                    quote env tail

                (Symbol "if") :: tail ->
                    handleIf env tail

                (Symbol "define") :: tail ->
                    define env tail

                (Symbol "set!") :: tail ->
                    set env tail

                (Symbol "lambda") :: tail ->
                    lambda env tail

                (Symbol "begin") :: tail ->
                    begin env tail

                (Symbol procName) :: args ->
                    proc env procName args

                (ExpressionList [ Symbol "lambda", ExpressionList paramsList, lambdaExpression ]) :: args ->
                    case handleLambdaExecution paramsList lambdaExpression env args of
                        Ok evaluatedExpression ->
                            Ok ( evaluatedExpression, env )

                        Err err ->
                            Err err

                [] ->
                    Ok ( nil, env )

                _ ->
                    Err (EvalError ("list should start with a proc" ++ expressionToString exp))

        Function _ ->
            Ok ( exp, env )

        LambdaExpression _ _ ->
            Ok ( exp, env )

        TrueExpression ->
            Ok ( exp, env )


quote : Environment -> List Expression -> Result Error ( Expression, Environment )
quote env lst =
    case lst of
        [ exp ] ->
            Ok ( exp, env )

        _ ->
            Err (EvalError "quote takes just 1 argument")


lambda : Environment -> List Expression -> Result Error ( Expression, Environment )
lambda env lst =
    case lst of
        [ ExpressionList vars, exp ] ->
            Ok ( LambdaExpression vars exp, env )

        _ ->
            Err (EvalError "Invalid lambda expression")


symbol : Environment -> String -> Result Error ( Expression, Environment )
symbol env var =
    case getVar env var of
        Just exp_ ->
            Ok ( exp_, env )

        Nothing ->
            Err (EvalError ("Symbol Not Found: " ++ var))


begin : Environment -> List Expression -> Result Error ( Expression, Environment )
begin env exps =
    case exps of
        [] ->
            Ok ( nil, env )

        [ exp ] ->
            eval env exp

        head :: tail ->
            case eval env head of
                Ok ( _, newEnv ) ->
                    begin newEnv tail

                Err error ->
                    Err error


proc : Environment -> String -> List Expression -> Result Error ( Expression, Environment )
proc env procName args =
    let
        maybeFunc =
            getVar env procName

        evaledArgsResult =
            evalList env args
    in
    case ( maybeFunc, evaledArgsResult ) of
        ( Just (Function func), Ok ( evaledArgs, newEnv ) ) ->
            case func evaledArgs of
                Ok ok ->
                    Ok ( ok, newEnv )

                Err error ->
                    Err error

        ( Just (LambdaExpression params exp), Ok ( evaluatedArgs, newEnv ) ) ->
            case handleLambdaExecution params exp newEnv evaluatedArgs of
                Ok evaledExp ->
                    Ok ( evaledExp, newEnv )

                Err error ->
                    Err error

        ( Nothing, _ ) ->
            Err (EvalError ("Symbol " ++ procName ++ " not found"))

        ( _, Err error ) ->
            Err error

        ( _, Ok _ ) ->
            Err (EvalError "Not a function or lambda expression")


handleLambdaExecution : List Expression -> Expression -> Environment -> List Expression -> Result Error Expression
handleLambdaExecution params exp env args =
    if List.length params /= List.length args then
        Err (EvalError "params and args do not have same length")

    else
        let
            varsResult =
                paramsAndArgsToVars params args
        in
        case varsResult of
            Ok vars ->
                case eval (Environment { vars = vars, outer = Just env }) exp of
                    Ok ( ret, _ ) ->
                        Ok ret

                    Err error ->
                        Err error

            Err error ->
                Err error


paramsAndArgsToVars : List Expression -> List Expression -> Result Error Variables
paramsAndArgsToVars params args =
    let
        allSymbol =
            List.all
                (\a ->
                    case a of
                        Symbol _ ->
                            True

                        _ ->
                            False
                )
                params

        varsPairList =
            List.map2
                (\a b ->
                    case a of
                        Symbol sym ->
                            ( sym, b )

                        _ ->
                            ( "", b )
                )
                params
                args

        vars =
            Dict.fromList varsPairList
    in
    if allSymbol then
        Ok vars

    else
        Err (EvalError "Parameters of a lambda expression must not contain a not symbol")


evalList : Environment -> List Expression -> Result Error ( List Expression, Environment )
evalList env lst =
    case lst of
        [] ->
            Ok ( [], env )

        head :: tail ->
            case eval env head of
                Ok ( evaledHead, newEnv ) ->
                    case evalList newEnv tail of
                        Ok ( evaledTail, lastEnv ) ->
                            Ok ( evaledHead :: evaledTail, lastEnv )

                        Err error ->
                            Err error

                Err error ->
                    Err error


define : Environment -> List Expression -> Result Error ( Expression, Environment )
define env exps =
    case exps of
        [ Symbol var, exp ] ->
            case getVar env var of
                Just _ ->
                    Err (EvalError "Defining already defined variable")

                Nothing ->
                    case eval env exp of
                        Ok ( evaledExp, newEnv ) ->
                            Ok ( nil, setVar newEnv var evaledExp )

                        Err error ->
                            Err error

        [ _, exp ] ->
            Err (EvalError "First argument for define should be a symbol")

        _ ->
            Err (EvalError "Too many or few expressions for DEFINE")


set : Environment -> List Expression -> Result Error ( Expression, Environment )
set env exps =
    case exps of
        [ Symbol var, exp ] ->
            case getVar env var of
                Just _ ->
                    case eval env exp of
                        Ok ( evaledExp, newEnv ) ->
                            Ok ( nil, setVar newEnv var evaledExp )

                        Err error ->
                            Err error

                Nothing ->
                    Err (EvalError "Setting an undefined variable.")

        [ _, exp ] ->
            Err (EvalError "First argument of set! should be a symbol")

        _ ->
            Err (EvalError "Too many of few expressions for set!")


parse : String -> Result Error Expression
parse src =
    let
        split =
            src
                |> String.replace "\n" ""
                |> String.replace "\t" ""
                |> String.replace "(" " ( "
                |> String.replace ")" " ) "
                |> String.split " "
                |> List.filter (\el -> el /= "")

        parsedResult =
            parseSplit split
    in
    case parsedResult of
        Ok parsed ->
            Ok (ExpressionList parsed)

        Err err ->
            Err err


parseSplit : List String -> Result Error (List Expression)
parseSplit split =
    case split of
        "(" :: tail ->
            let
                parsedResult =
                    parseFindClosing tail
            in
            case parsedResult of
                Ok ( exps, resid ) ->
                    if List.length resid == 0 then
                        Ok exps

                    else
                        Err (ParseError "insufficient )")

                Err error ->
                    Err error

        [] ->
            Err (ParseError "unexpected EOF found")

        _ ->
            Err (ParseError "Program should start with (")


parseFindClosing : List String -> Result Error ( List Expression, List String )
parseFindClosing lst =
    case lst of
        [] ->
            Err (ParseError "insufficient )")

        ")" :: tail ->
            Ok ( [], tail )

        "(" :: tail ->
            let
                parsedTail =
                    parseFindClosing tail
            in
            case parsedTail of
                Err error ->
                    Err error

                Ok ( exps, resid ) ->
                    case parseFindClosing resid of
                        Ok ( exps_, resid_ ) ->
                            Ok ( ExpressionList exps :: exps_, resid_ )

                        Err err ->
                            Err err

        head :: tail ->
            let
                atomHead =
                    atom head

                parsedTailResult =
                    parseFindClosing tail
            in
            case parsedTailResult of
                Err error ->
                    Err error

                Ok ( exps, resid ) ->
                    Ok ( atomHead :: exps, resid )


atom : String -> Expression
atom str =
    if str == "#t" then
        TrueExpression

    else if str == "nil" then
        nil

    else
        case String.toInt str of
            Just n ->
                Constant n

            Nothing ->
                Symbol str


handleIf : Environment -> List Expression -> Result Error ( Expression, Environment )
handleIf env lst =
    case lst of
        [ test, conseq, alt ] ->
            case eval env test of
                Ok ( ExpressionList [], _ ) ->
                    eval env alt

                Ok _ ->
                    eval env conseq

                Err err ->
                    Err err

        _ ->
            Err (EvalError "If takes just three arguments")


type Error
    = ParseError String
    | EvalError String


type Expression
    = Symbol String
    | Constant Int
    | Function (List Expression -> Result Error Expression)
    | ExpressionList (List Expression)
    | LambdaExpression (List Expression) Expression
    | TrueExpression


nil : Expression
nil =
    ExpressionList []


type Environment
    = Environment
        { outer : Maybe Environment
        , vars : Variables
        }


type alias Variables =
    Dict String Expression


plus : List Expression -> Result Error Expression
plus lst =
    case lst of
        [] ->
            Ok (Constant 0)

        (Constant head) :: tail ->
            let
                sumTailResult =
                    plus tail
            in
            case sumTailResult of
                Ok (Constant sumTail) ->
                    Ok (Constant (sumTail + head))

                _ ->
                    Err (EvalError "summation of not numbers")

        _ ->
            Err (EvalError "summation of not numbers")


minus : List Expression -> Result Error Expression
minus lst =
    case lst of
        [] ->
            Ok (Constant 0)

        [ Constant val ] ->
            Ok (Constant -val)

        [ Constant val1, Constant val2 ] ->
            Ok (Constant (val1 - val2))

        _ ->
            Err (EvalError "Error in minus")


prod : List Expression -> Result Error Expression
prod lst =
    case lst of
        [] ->
            Ok TrueExpression

        [ Constant val ] ->
            Ok (Constant val)

        (Constant headVal) :: tail ->
            case prod tail of
                Err err ->
                    Err err

                Ok (Constant tailProd) ->
                    Ok (Constant (headVal * tailProd))

                Ok _ ->
                    Err (EvalError "Args for * should be constants")

        _ ->
            Err (EvalError "Args for * should be constants")


divide : List Expression -> Result Error Expression
divide lst =
    case lst of
        [] ->
            Err (EvalError "/ should have args")

        [ Constant val ] ->
            Ok (Constant val)

        [ Constant a, Constant b ] ->
            Ok (Constant (a // b))

        _ ->
            Err (EvalError "/ should 2 numerical arguments")


not : List Expression -> Result Error Expression
not lst =
    case lst of
        [ ExpressionList [] ] ->
            Ok TrueExpression

        [ a ] ->
            Ok nil

        _ ->
            Err (EvalError "not should take 1 argument")


gt : List Expression -> Result Error Expression
gt lst =
    case lst of
        [ Constant a, Constant b ] ->
            Ok
                (if a > b then
                    TrueExpression

                 else
                    nil
                )

        _ ->
            Err (EvalError "> should take 2 numerical arguments")


leq : List Expression -> Result Error Expression
leq lst =
    case lst of
        [ Constant a, Constant b ] ->
            Ok
                (if a <= b then
                    TrueExpression

                 else
                    nil
                )

        _ ->
            Err (EvalError "<= should take 2 numerical arguments")


geq : List Expression -> Result Error Expression
geq lst =
    case lst of
        [ Constant a, Constant b ] ->
            Ok
                (if a >= b then
                    TrueExpression

                 else
                    nil
                )

        _ ->
            Err (EvalError ">= should take 2 numerical arguments")


lt : List Expression -> Result Error Expression
lt lst =
    case lst of
        [ Constant a, Constant b ] ->
            Ok
                (if a < b then
                    TrueExpression

                 else
                    nil
                )

        _ ->
            Err (EvalError "< should take 2 numerical arguments")


or : List Expression -> Result Error Expression
or lst =
    case lst of
        [] ->
            Ok nil

        head :: tail ->
            case ( head, or tail ) of
                ( ExpressionList [], Ok (ExpressionList []) ) ->
                    Ok nil

                _ ->
                    Ok TrueExpression


and : List Expression -> Result Error Expression
and lst =
    case lst of
        [] ->
            Ok TrueExpression

        head :: tail ->
            case ( head, and tail ) of
                ( ExpressionList [], _ ) ->
                    Ok nil

                ( _, Ok (ExpressionList []) ) ->
                    Ok nil

                _ ->
                    Ok TrueExpression


length : List Expression -> Result Error Expression
length lst =
    case lst of
        [ ExpressionList a ] ->
            Ok (Constant (List.length lst))

        _ ->
            Err (EvalError "Invalid argument for length")


equal : List Expression -> Result Error Expression
equal lst =
    case lst of
        [ a, b ] ->
            if a == b then
                Ok TrueExpression

            else
                Ok nil

        _ ->
            Err (EvalError "The number of arguments for = should be 2")


cons : List Expression -> Result Error Expression
cons lst =
    case lst of
        [ a, ExpressionList b ] ->
            Ok (ExpressionList (a :: b))

        _ ->
            Err (EvalError "Invalid types of arguments for cons")


car : List Expression -> Result Error Expression
car lst =
    case lst of
        [ ExpressionList (head :: tail) ] ->
            Ok head

        [ ExpressionList [] ] ->
            Err (EvalError "car is not for an empty list")

        _ ->
            Err (EvalError "car is for a list")


cdr : List Expression -> Result Error Expression
cdr lst =
    case lst of
        [ ExpressionList (head :: tail) ] ->
            Ok (ExpressionList tail)

        [ ExpressionList [] ] ->
            Err (EvalError "cdr is not for an empty list")

        _ ->
            Err (EvalError "cdr is for non-empty lists")


list : List Expression -> Result Error Expression
list lst =
    Ok (ExpressionList lst)


isList : List Expression -> Result Error Expression
isList lst =
    case lst of
        [ ExpressionList someList ] ->
            Ok TrueExpression

        _ ->
            Ok nil


isSymbol : List Expression -> Result Error Expression
isSymbol lst =
    case lst of
        [ Symbol _ ] ->
            Ok TrueExpression

        [ _ ] ->
            Ok nil

        _ ->
            Err (EvalError "symbol? should be used for one argument")


initialEnv : Environment
initialEnv =
    Environment { outer = Nothing, vars = initialVars }


initialVars : Variables
initialVars =
    Dict.fromList
        [ ( "+", Function plus )
        , ( "-", Function minus )
        , ( "*", Function prod )
        , ( "/", Function divide )
        , ( "not", Function not )
        , ( ">", Function gt )
        , ( ">=", Function geq )
        , ( "<", Function lt )
        , ( "<=", Function leq )
        , ( "or", Function or )
        , ( "and", Function and )
        , ( "=", Function equal )
        , ( "length", Function length )
        , ( "cons", Function cons )
        , ( "car", Function car )
        , ( "cdr", Function cdr )
        , ( "list", Function list )
        , ( "list?", Function isList )
        , ( "symbol?", Function isSymbol )
        ]


getVar : Environment -> String -> Maybe Expression
getVar env key =
    case env of
        Environment env_ ->
            case Dict.get key env_.vars of
                Just exp ->
                    Just exp

                Nothing ->
                    case env_.outer of
                        Just outer ->
                            getVar outer key

                        Nothing ->
                            Nothing


setVar : Environment -> String -> Expression -> Environment
setVar env key val =
    case env of
        Environment env_ ->
            Environment { outer = env_.outer, vars = Dict.insert key val env_.vars }
