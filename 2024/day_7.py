from itertools import product

example:str="""190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"""

class Operator:
    def __init__(self, func, symbol):
        self.func = func
        self.symbol = symbol

    def __call__(self, a, b):
        return self.func(a, b)

    def __str__(self):
        return self.symbol
    
    def __repr__(self):
        return self.symbol

multiply = Operator(lambda a, b: a * b, '*')
add = Operator(lambda a, b: a + b, '+')
concat = Operator(lambda a, b: int(str(a) + str(b)), '||')

operators = [multiply, add]

def get_file_contents() -> str:
    with open("day_7_input.txt", "r") as file:
        return file.read()

class Equation:
    def __init__(self, line:str):
        split = line.split(" ")
        self.value = int(split[0].strip(":"))
        self.numbers = [int(x) for x in split[1:]]
        self.solution_sum = 0

    def find_solutions(self):
        #print(f"Equation: {self.value} {self.numbers} ==========") 
        operator_number = self.numbers.__len__() - 1
        # Generate all combinations of operators for length a-1
        combinations = list(product(operators, repeat=operator_number))
        #print(f"combinations: {combinations}")
        for combination in combinations:
            evaluation = self.evaluate(combination)
            #print(f"combination {combination} evaluates to {evaluation}")
            if evaluation == self.value:
                #print(f"matched {combination}, to {self.value}, adding to sum: {self.solution_sum}")
                self.solution_sum += self.value
                # part 1
                return self.value
        return self.solution_sum
        
    def evaluate(self, operators):
        operators = list(operators)
        numbers = self.numbers.copy()
        while numbers.__len__() > 1:
            for i, operator in enumerate(operators):
                # first multiply
                if operator == multiply:
                    numbers[i] = operator(numbers[i], numbers[i+1])
                    numbers.pop(i+1)
                    operators.pop(i)
                    break
                # then add
                elif operator == add:
                    numbers[i] = operator(numbers[i], numbers[i+1])
                    numbers.pop(i+1)
                    operators.pop(i)
                    break
                elif operator == concat:
                    numbers[i] = int(str(numbers[i]) + str(numbers[i+1]))
                    numbers.pop(i+1)
                    operators.pop(i)
                    break
                else:
                    raise ValueError("A yo")
        assert numbers.__len__() == 1
        return numbers[0]



def read(input:str)->list[Equation]:
    return [Equation(x) for x in input.splitlines()]

solve = lambda input: sum([x.find_solutions() for x in read(input)])

example_solved = solve(example)
print(f"example: {example_solved}, expected 3749, so {'correct' if example_solved == 3749 else 'incorrect'}")
print(f"part 1: {solve(get_file_contents())}")


operators = [multiply, add, concat]
example_solved = solve(example)
print(f"example: {example_solved}, expected 11387, so {'correct' if example_solved == 11387 else 'incorrect'}")
print(f"part 2: {solve(get_file_contents())}")