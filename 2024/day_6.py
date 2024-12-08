example:str="""....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""


def get_file_contents() -> str:
    with open("day_6_input.txt", "r") as file:
        return file.read()
    

import numpy as np

def read(input:str)->np.matrix:
    return np.matrix([[x for x in line] for line in input.splitlines()])


def find_start(input:np.matrix)->tuple[int,int]:
    for x in range(input.shape[0]):
        for y in range(input.shape[1]):
            if input[x,y] == "^":
                input[x,y] = "X"
                return x,y
    raise ValueError("No start found")

def move(matrix:np.matrix, pos:tuple[int,int], direction:tuple[int,int])->tuple[int,int]:
    new_pos = (pos[0] + direction[0], pos[1] + direction[1])
    if not (0 <= new_pos[0] < matrix.shape[0] and 0 <= new_pos[1] < matrix.shape[1]):
        raise ValueError("Out of bounds")
    if matrix[new_pos] != "#":
        matrix[pos] = "X"
        return new_pos
    return None

def solve(input:np.matrix)->int:
    start = find_start(input)
    print(f"start: {start}")
    direction = (-1,0)
    new_pos = start
    while True:
        try:
            pos = move(input,new_pos,direction)
            if pos is None:
                # turn right 
                direction = (direction[1],-direction[0])
            else:
                new_pos = pos
        except ValueError:
            print("Out of bounds")
            break
    return sum([x.count("X") for x in input.tolist()]) + 1

print(f"example: {solve(read(example))}")
print(f"example: {solve(read(get_file_contents()))}")