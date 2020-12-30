import sys

usage = "Usage: ./main <input>"


def dx(char):
    if char == '^':
        return (0, 1)
    elif char == 'v':
        return (0, -1)
    elif char == '>':
        return (1, 0)
    elif char == '<':
        return (-1, 0)
    else:
        assert False, "Unexpected char {}".format(char)


def sum_tuple(t1, t2):
    return (t1[0] + t2[0], t1[1] + t2[1])


def solution(data):
    santa_pos = (0, 0)
    robo_pos = (0, 0)
    map = {(0, 0): 1}

    for i, char in enumerate(data):
        if i % 2 == 0:
            santa_pos = sum_tuple(santa_pos, dx(char))
        else:
            robo_pos = sum_tuple(robo_pos, dx(char))

        pos = santa_pos if i % 2 == 0 else robo_pos

        if pos in map:
            map[pos] += 1
        else:
            map[pos] = 1

    return len(map.keys())


def main(argv):
    assert (len(argv) == 2), usage
    data = open(argv[1], 'r').read()
    print(solution(data))


if __name__ == "__main__":
    main(sys.argv)
