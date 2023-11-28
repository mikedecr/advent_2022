using PartialFunctions: $
using IterTools: partition

#================#
#=    part 1    =#
#================#

function divide_rucksack_to_compartments(rucksack::String)::Tuple{String, String}
    half_n = length(rucksack) ÷ 2
    first(rucksack, half_n), last(rucksack, half_n)
end

function item_priority(item::Char)
    prios = Dict(zip(vcat('a':'z', 'A':'Z'), 1:56))
    get(prios, item, nothing)
end

function part1(sacks::AbstractVector)::Int
    f = item_priority ∘ only ∘ (reduce $ intersect) ∘ divide_rucksack_to_compartments
    sum(map(f, sacks))
end


#==================#
#=    2nd part    =#
#==================#

function part2(sacks::Vector{String})::Int
    f = item_priority ∘ only ∘ (reduce $ intersect)
    grps = partition(sacks, 3)
    sum(map(f, grps))
end

#==============#
#=    main    =#
#==============#

data = readlines(ARGS[1])
println("Part 1: " * (part1(data) |> string))
println("Part 2: " * (part2(data) |> string))

