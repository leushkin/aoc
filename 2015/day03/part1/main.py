import sys

usage = "Usage: ./main <input>"

def solution(data):
  curr = (0,0)
  map = {curr: 0}

  for char in data:
    if char == '^':
      curr = (curr[0], curr[1] + 1)
    elif char == 'v':
      curr = (curr[0], curr[1] - 1)
    elif char == '>':
      curr = (curr[0] + 1, curr[1])
    elif char == '<':
      curr = (curr[0] - 1, curr[1])
    else:
      assert False, "Unexpected char {}".format(char)

    if curr in map:
      map[curr] += 1;
    else:
      map[curr] = 1;

  return len(map.keys())


def main(argv):
  assert (len(argv) == 2), usage
  data = open(argv[1], 'r').read()
  print(solution(data))


if __name__ == "__main__":
  main(sys.argv)