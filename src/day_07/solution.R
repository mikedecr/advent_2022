box::use(purrr[compose, partial])
box::use(fns = ./src/fns)


test = c(
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
)

cmds = test

chars = function(x) strsplit(x, '')[[1]]
is_cd = function(x) all(chars(x)[1:5] == chars("$ cd "))
is_ls = function(x) all(chars(x)[1:4] == chars("$ ls"))
is_new_cd = function(x) is_cd(x) & last_word(x) != '..'

last_word = function(w) {
    stringr::str_split(w, ' ', simplify = TRUE) |>
    (function(x) tail(x[1, ], 1))()
}
first_word = function(w) {
    stringr::str_split(w, ' ', simplify = TRUE) |>
    (function(x) head(x[1, ], 1))()
}


size_of = function(cmds, dirname) {
    print(paste('getting size of', dirname))
    ii = which(cmds == paste("$ cd", dirname)) + 1
    cmd = cmds[ii]
    size = 0
    while (!is_cd(cmd) || ii > length(cmds)) {
        print(paste('while', cmd))
        if (is_ls(cmd)) {
            # do nothing
        }
        else if (first_word(cmd) == 'dir') {
            size = size + size_of(cmds, last_word(cmd))
        }
        else {
            filesize = as.integer(first_word(cmd))
            size = size + filesize
            print(paste('updated size of', dirname, 'by', filesize, 'to', size))
        }
        if (cmd == tail(cmds, 1)) break
        ii = ii + 1
        cmd = cmds[ii]
    }
    print(paste('done with', dirname, 'size =', size))
    return(size)
}

directory_sizes = function(cmds) {
    sizes = list()
    for (cmd_n in seq_along(cmds)) {
        cmd = cmds[cmd_n]
        print(paste('found', last_word(cmd)))
        if (is_new_cd(cmd)) {
            if (last_word(cmd) %in% names(sizes)) {
                print(paste('already computed', last_word(cmd)))
            }
            else {
                sizes[[last_word(cmd)]] = size_of(cmds, last_word(cmd))
            }
        }
    }
    return(sizes)
}

directory_sizes(test) |>
    unlist() |>
    (function(x) x * (x < 100000))() |>
    sum()


directory_sizes(fns$advent_data(year=2022, day=7))


unlist(sizes) |>
    ( function(x) x * (x <= 100000) )() |>
    sum()


compute_sizes = function(cmds, cmd_n) {
    size = 0
    it = cmds[cmd_n]
    while (!is_cd(it)) {
        if (first_word(it) == 'dir') {
            
        }
    }
}


in seq_along(cmds)
for (com in coms) {
    if (is_cd(com)) {
        wd = last_word(com)
    }
    if (is_ls(com)) {
        sizes[[wd]] = walk_file_sizes()
    
    }
}



last_word('$ cd xyaz')


depth = 0
for (com in coms) {
    if (! is_cd(com)) next
    print(depth)
    print(last_word(com))
    # if ((depth) == 0 && last_word(com) != '..') {
    #     tops = c(tops, )
    # }
    if (last_word(com) == '/') {
        depth = 0
    } else if (last_word(com) == '..') {
        depth = depth - 1
    }
    else {
        depth = depth + 1
    }
    # if (depth == 0) {
    # }
}


dir_moves = 

chars(test[1])


lapply(test, function(x) strsplit(x, ' ')[[1]])





- identify top directories
- for each directory, call function
    - for files, sum
    - for dirs, call return function on dir

A = list[[/]]
if $: parse_command(cmd, arg)

if ls:
    dir x = A[[]]



