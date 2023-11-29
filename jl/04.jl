using Functors: fmap

# # λs galore
splat(f::Function) = args -> f(args...)
partial(f::Function, a...) = (b...) -> f(a..., b...)
apply(f::Function) = partial(fmap, f)
split_at(pattern::AbstractString) = x::AbstractString -> split(x, pattern)

# I could have been so much meaner...
# splat = f -> a -> f(a...)
# partial = (f, a...) -> (b...) -> f(a..., b...)
# apply = f -> x -> fmap(f, x)
# split_at = s -> x -> split(x, s)

#================#
#=    part 1    =#
#================#

# "a-b, c-d" -> range(a, b), range(c, d)
IntRange = UnitRange{Int64}
function parse_ranges(line::String)::Tuple{IntRange, IntRange}
    line |> 
        split_at(",") |>
        apply(split_at("-")) |>
        apply(x -> parse(Int, x)) |>
        apply(vect -> range(first(vect), last(vect))) |>
        Tuple
end

function range_contained(inner::UnitRange, outer::UnitRange)::Bool
    first(outer) <= first(inner) <= last(inner) <= last(outer)
end

function either_range_contained(a::UnitRange, b::UnitRange)::Bool
    range_contained(a, b) || range_contained(b, a)
end

function part1(data::Vector{String})::Int
    f = line -> line |>
        parse_ranges |>
        splat(either_range_contained)
    sum(map(f, data))
end

#================#
#=    part 2    =#
#================#

function part2(data::Vector{String})::Int
    f = line -> line |>
        parse_ranges |>
        splat(intersect) |>
        length |>
        >=(1)
    sum(map(f, data))
end

#============#
#=    do    =#
#============#

data = readlines(ARGS[1]);
println("Part 1: " * string(part1(data)))
println("Part 2: " * string(part2(data)))

