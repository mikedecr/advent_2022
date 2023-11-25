# extremely type-system style

#==========================#
#=    functional utils    =#
#==========================#

# turns map(f, x) into mapping(f)(x). helpful for x |> mapping(f)
# (purrr::map signature is map(x, f) which is easy for |> but unidiomatic in F.P.)
mapping(fn::Function)::Function = (x::AbstractArray -> map(fn, x))

# lift a fn from args... domain to tuple/array of args... domain
# (similar to deprecated purrr::lift)
unpack(f)::Function = (t::Union{Tuple, AbstractArray} -> f(t...))

#================#
#=    part 1    =#
#================#

# shocking how "English" the problem becomes with these enums
@enum Move Rock Paper Scissors
@enum Outcome Win Lose Tie

function char_to_move(c::Char)::Move
    c in ['A' 'X'] && return Rock
    c in ['B' 'Y'] && return Paper
    c in ['C' 'Z'] && return Scissors
end

function str_to_moves(s::String)::Tuple{Move, Move}
    str_moves = split(s)
    @assert length(str_moves) == 2
    Tuple(map(char_to_move ∘ only, str_moves))
end

function compute_outcome(theirs::Move, mine::Move)::Outcome
    (theirs, mine) in [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)] && return Win
    theirs == mine && return Tie
    return Lose
end

function score_move(x::Move)::Int
    x == Rock && return 1
    x == Paper && return 2
    # don't check Scissors, it's an enum
    return 3
end

function score_outcome(x::Outcome)::Int
    x == Win && return 6
    x == Tie && return 3
    # don't check Lose
    return 0
end

function score_round(theirs::Move, mine::Move)::Int
    outcome = compute_outcome(theirs, mine)
    score_outcome(outcome) + score_move(mine)
end

function part1(guide::Vector)::Int
    guide |>
        mapping(str_to_moves) |>
        mapping(unpack(score_round)) |>
        sum
end



#================#
#=    part 2    =#
#================#

function char_to_outcome(c::Char)::Outcome
    c == 'X' && return Lose
    c == 'Y' && return Tie
    c == 'Z' && return Win
end

function infer_move(theirs::Move, outcome::Outcome)
    # this could be memoized for speed
    papers = [(Rock, Win), (Scissors, Lose), (Paper, Tie)]
    rocks = [(Rock, Tie), (Scissors, Win), (Paper, Lose)]
    (theirs, outcome) in papers && return Paper 
    (theirs, outcome) in rocks && return Rock
    # (theirs, outcome) in scissors && return Scissors 
    return Scissors
end

function parse_move_and_outcome(str::String)::Tuple{Move, Outcome}
    move_char, outcome_char = split(str) |> mapping(only)
    return char_to_move(move_char), char_to_outcome(outcome_char)
end

function score_round_with_inferred_move(theirs::Move, outcome::Outcome)
    mine = infer_move(theirs, outcome)
    score_outcome(outcome) + score_move(mine)
end

function part2(guide::Vector)::Int
    guide |> 
        mapping(parse_move_and_outcome) |>
        mapping(unpack(score_round_with_inferred_move)) |>
        sum
end


#==============#
#=    main    =#
#==============#

data = readlines(ARGS[1])
println("Part 1: " * string(part1(data)))
println("Part 2: " * string(part2(data)))

