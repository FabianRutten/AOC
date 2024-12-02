def get_file_contents() -> str:
    with open("day_2_input.txt", "r") as file:
        return file.read()
    
example: str = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

def read(input:str)->list[list[int]]:
    lines:list[str] = input.split('\n')
    print(f"amount of lines: {len(lines)}")
    retval:list[list[int]] = []
    for line in lines:
        if not line:
            continue
        retval.append([int(x) for x in line.split(" ")])
    return retval

def check_report(report:list[int])->int:
    asc:bool = report[0] < report[1]
    for i in range(1, len(report)):
        if asc and report[i-1] < report[i]:
            if abs(report[i-1] - report[i]) <= 3:
                continue
        elif not asc and report[i-1] > report[i]:
            if abs(report[i-1] - report[i]) <= 3:
                continue
        return False
    return True
    
def check_reports(reports:list[list[int]])->int:
    retval:int = 0
    for report in reports:
        if check_report(report):
            retval += 1
    return retval

print(f"example: {check_reports(read(example))}")
print(f"input: {check_reports(read(get_file_contents()))}")


def check_report_tolerant(report:list[int])->int:
    retval:list[bool] = [check_report(report)]
    for i,_ in enumerate(report):
        report_copy:list[int] = report[:i] + report[i+1:]
        retval.append(check_report(report_copy))
    return any(retval)


def check_reports_tolerant(reports:list[list[int]])->int:
    retval:int = 0
    for report in reports:
        if check_report_tolerant(report):
            retval += 1
    return retval

print(f"example: {check_reports_tolerant(read(example))}")
print(f"input: {check_reports_tolerant(read(get_file_contents()))}")