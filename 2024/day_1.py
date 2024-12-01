example = """3   4
4   3
2   5
1   3
3   9
3   3"""

def read(input:str)->tuple[list[int],list[int]]:
    lines:list[str] = input.split('\n')
    print(f"amount of lines: {len(lines)}")
    col1:list[int] = []
    col2:list[int] = []
    for line in lines:
        if not line:
            continue
        str1,str2 = line.split("   ")
        col1.append(int(str1))
        col2.append(int(str2))
    return col1,col2

def get_distances(col1:list[int], col2:list[int]) -> list[int]:
    retval:list[int] = col1
    for i,num in enumerate(col1):
        retval[i] = abs(num-col2[i])
    return retval

def solve_part_1(input:str)->int:
    col1,col2 = read(input)
    col1.sort()
    col2.sort()
    print(sum(get_distances(col1,col2)))

def get_file_contents() -> str:
    with open("day_1_input.txt", "r") as file:
        return file.read()
    
solve_part_1(get_file_contents())

def solve_part_2(input:str)->int:
    col1,col2 = read(input)
    for i,num in enumerate(col1):
        col1[i] = num * col2.count(num)
    print(sum(col1))

solve_part_2(get_file_contents())