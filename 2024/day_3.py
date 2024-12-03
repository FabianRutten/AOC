
def get_file_contents() -> str:
    with open("day_3_input.txt", "r") as file:
        return file.read()


import re

example:str="xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"



def read(input:str)->list[tuple[int,int]]:
    return [(int(x),int(y)) for x,y in re.findall(r"mul\((\d+),(\d+)\)",input)]

def solve_muls(ls:list[tuple[int,int]])->int:
    return sum([x*y for x,y in ls])

print(f"example: {solve_muls(read(example))}")


    
print(f"input: {solve_muls(read(get_file_contents()))}")


def read_better(input:str)->list[tuple[int,int]]:
    matches = re.findall(r"(do\(\))|(don't\(\))|mul\((\d+),(\d+)\)", input)
    enabled = True
    retval: list[tuple[int,int]] = []
    for match in matches:
        if match[0] == "do()":
            enabled = True
        elif match[1] == "don't()":
            enabled = False
        elif enabled:
            x,y = match[2],match[3]
            retval.append((int(x),int(y)))
    return retval

print(f"example: {solve_muls(read_better(example))}")
print(f"input: {solve_muls(read_better(get_file_contents()))}")