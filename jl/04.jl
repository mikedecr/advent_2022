
using PartialFunctions: $
using Functors: fmap


function rpartial(f, b...)::Function 
    (a...) -> f(a..., b...) 
end

function partial(f, a...)::Function 
    (b...) -> f(a..., b...)
end

function unpack(f::Function) 
    t -> f(t...)
end
#================#
#=    part 1    =#
#================#

function parse_ranges(line::String)::Tuple{UnitRange{Int64}, UnitRange{Int64}}
    line |> 
        rpartial(split, ",") |>
        partial(fmap, rpartial(split, "-")) |>
        partial(fmap, partial(parse, Int)) |>
        partial(map, (v -> range(first(v), last(v)))) |>
        Tuple
end

function range_contained(inner::UnitRange, outer::UnitRange)
    (first(outer) <= first(inner)) && (last(outer) >= last(inner))
end

function either_range_contained(a::UnitRange, b::UnitRange)
    range_contained(a, b) || range_contained(b, a)
end

function part1(data)
    f = unpack(either_range_contained) ∘ parse_ranges
    sum(map(f, data))
end

#================#
#=    part 2    =#
#================#

function part2(data)
    f = >=(1) ∘ length ∘ (reduce $ intersect) ∘ parse_ranges
    sum(map(f, data))
end

data = readlines(ARGS[1]);
println("Part 1: " * string(part1(data)))
println("Part 2: " * string(part2(data)))
