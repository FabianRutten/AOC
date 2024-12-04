example = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""

def get_file_contents() -> str:
    with open("day_4_input.txt", "r") as file:
        return file.read()

import numpy as np

def read(input:str)->np.array:
    return np.array([[x for x in line] for line in input.splitlines()])

def find_xmas_from_x(index:tuple[int,int], array:np.array)->int:
    search_directions = [(0,1),(1,0),(0,-1),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
    retval = 0
    for x, y in search_directions:
        new_x1, new_y1 = index[0] + x, index[1] + y
        new_x2, new_y2 = index[0] + 2 * x, index[1] + 2 * y
        new_x3, new_y3 = index[0] + 3 * x, index[1] + 3 * y
        if 0 <= new_x1 < array.shape[0] and 0 <= new_y1 < array.shape[1]:
            if array[new_x1, new_y1] == "M":
                if 0 <= new_x2 < array.shape[0] and 0 <= new_y2 < array.shape[1]:
                    if array[new_x2, new_y2] == "A":
                        if 0 <= new_x3 < array.shape[0] and 0 <= new_y3 < array.shape[1]:
                            if array[new_x3, new_y3] == "S":
                                retval += 1
    return retval

def solve(input:np.array)->int:
    retval = 0
    for x in range(input.shape[0]):
        for y in range(input.shape[1]):
            if input[x,y] == "X":
                retval += find_xmas_from_x((x,y),input)
    return retval


print(f"example: {solve(read(example))}")
print(f"input: {solve(read(get_file_contents()))}")

def find_x_mas_from_a(index:tuple[int,int], array:np.array)->int:
    # search diagonal tl-br
    tl_br = False
    if index[0] - 1 >= 0 and index[1] - 1 >= 0 and index[0] + 1 < array.shape[0] and index[1] + 1 < array.shape[1]:
        if array[index[0] - 1, index[1] - 1] == "M" and array[index[0] + 1, index[1] + 1] == "S":
            tl_br = True
        if array[index[0] - 1, index[1] - 1] == "S" and array[index[0] + 1, index[1] + 1] == "M":
            tl_br = True

    if tl_br:
        # search diagonal tr-bl
        if index[0] - 1 >= 0 and index[1] + 1 < array.shape[1] and index[0] + 1 < array.shape[0] and index[1] - 1 >= 0:
            if array[index[0] - 1, index[1] + 1] == "M" and array[index[0] + 1, index[1] - 1] == "S":
                return 1
            if array[index[0] - 1, index[1] + 1] == "S" and array[index[0] + 1, index[1] - 1] == "M":
                return 1
    return 0

def solve_better(input:np.array)->int:
    retval = 0
    for x in range(input.shape[0]):
        for y in range(input.shape[1]):
            if input[x,y] == "A":
                retval += find_x_mas_from_a((x,y),input)
    return retval

print(f"example: {solve_better(read(example))}")
print(f"input: {solve_better(read(get_file_contents()))}")