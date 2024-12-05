example:str="""47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""

def get_file_contents() -> str:
    with open("day_5_input.txt", "r") as file:
        return file.read()

def read(input:str)->tuple[str,str]:
    #split on empty line
    parts = input.split("\n\n")
    return parts[0],parts[1]

def rules_from_input(input:str) -> list[tuple[int,int]]:
    return [(int(x.split("|")[0]),int(x.split("|")[1])) for x in input.splitlines()]

def check_rules(rules:list[tuple[int,int]], nums:list[int])->bool:
    for i, num in enumerate(nums):
        for rule in rules:
            if rule[0][1] == num:
                if nums[i:].count(rule[0][0]) != 0:
                    return False
    return True
            
def pages_to_nums(pages:str)->list[int]:
    return [int(x) for x in pages.split(",")]
            
def get_center_value(nums:list[int])->int:
    return nums[len(nums)//2]

def solve(input:tuple[str,str])->int:
    all_rules,all_pages =read(input)
    rules = [rules_from_input(x) for x in all_rules.splitlines()]
    pages = [pages_to_nums(x) for x in all_pages.splitlines()]
    return sum([get_center_value(x) for x in pages if check_rules(rules,x)])

print(f"example: {solve(example)}")
print(f"input: {solve(get_file_contents())}")

def fix_with_rules(rules:list[tuple[int,int]], nums:list[int])->list[int]:
    print(f"Initial nums: {nums}")
    for i, num in enumerate(nums):
        print(f"Checking num: {num} at index: {i}")
        for rule in rules:
            print(f"Applying rule: {rule}")
            if rule[0][1] == num:
                print(f"Rule matches num: {num}")
                if nums[i:].count(rule[0][0]) != 0:
                    print(f"Rule {rule} is applicable, modifying nums: {nums}")
                    # swap m
                    j = nums.index(rule[0][0],i)
                    h = nums.index(rule[0][1],i)
                    print(f"Swapping {nums[h]}, at {h} with {rule[0][0]}")
                    print(f"Swapping {nums[j]}, at {j} with {num}")
                    nums[h] = rule[0][0]
                    print(f"half_swap: {nums}")
                    nums[j] = num
                    print(f"Modified nums: {nums}")
                    continue
                print(f"Rule {rule} is not applicable")
    print(f"Final nums: {nums}")
    return nums

def solve_better(input:tuple[str,str])->int:
    all_rules,all_pages =read(input)
    rules = [rules_from_input(x) for x in all_rules.splitlines()]
    pages = [pages_to_nums(x) for x in all_pages.splitlines()]
    faulty_pages = [x for x in pages if not check_rules(rules,x)]
    return sum([get_center_value(fix_with_rules(rules,x)) for x in faulty_pages])

print(f"example: {solve_better(example)}")
print(f"input: {solve_better(get_file_contents())}")