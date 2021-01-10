const fs = require('fs')

class Operator {
  constructor(name, l, r, debug) {
    this.name = name
    this.left = l
    this.right = r
    this.debug = debug
  }
}

const data = fs.readFileSync('./input.txt', { encoding: "utf-8"})

function opToFn(op, l, r) {
  switch (op) {
    case "OR": return l | r
    case "AND": return l & r
    case "RSHIFT": return l >> r
    case "LSHIFT": return l << r
    default:
      throw new Error(`Unsupported operator ${op}`)
  }
}

function tokenize(input) {
  const [id, expr] = input.split(" -> ").reverse()

  if (expr.split(" ").length === 3) {
    const [l, op, r] = expr.split(" ");
    const left = parseInt(l, 10) || l
    const right = parseInt(r, 10) || r

    switch (op) {
      case "OR": return [id, new Operator("OR", left, right, { id })]
      case "AND": return [id, new Operator("AND", left, right, { id })]
      case "RSHIFT": return [id, new Operator("RSHIFT", left, right, { id })]
      case "LSHIFT": return [id, new Operator("LSHIFT", left, right, { id })]
      default:
        throw new Error(`Unsupported operator ${op}`)
    }
  }

  if (expr.startsWith("NOT")) {
    const r = expr.replace("NOT ", "");
    const right = parseInt(r, 10) || r;

    return [id, new Operator("NOT", null, right, { id })]
  }

  if (!isNaN(parseInt(expr, 10))) {
    return [id, parseInt(expr, 10)]
  }

  if (expr.length == 2) {
    return [id, expr]
  }

  throw new Error(`Cannot parse input ${input}`)
}

const map = Object.fromEntries(data.split('\n').map(tokenize))

function buildAST(map, key) {
  const current = map[key];

  if (current instanceof Operator) {
    switch (current.name) {
      case 'NOT':
      case 'OR':
      case 'AND':
      case 'RSHIFT':
      case 'LSHIFT': {
        if (typeof current.left === 'string') {
          current.left = buildAST(map, current.left)
        }

        if (typeof current.right === 'string') {
          current.right = buildAST(map, current.right)
        }

        return current;
      }
      default:
        throw new Error(`Unsupported operator ${current.name}`)
    }
  }

  if (typeof current === 'string') {
    return buildAST(map, current);
  }

  if (typeof current === 'number') {
    return current;
  }

  throw new Error(`Cannot walk throught ${current}`)
}

function exec(ast) {
  if (typeof ast === 'number') {
    return ast
  }

  if (ast instanceof Operator) {
    switch(ast.name) {
      case 'NOT': {
        if (typeof ast.right !== 'number') {
          ast.right = exec(ast.right)
        }

        return ~ast.right
      }
      case 'OR':
      case 'AND':
      case 'RSHIFT':
      case 'LSHIFT': {
        if (typeof ast.left !== 'number') {
          ast.left = exec(ast.left)
        }

        if (typeof ast.right !== 'number') {
          ast.right = exec(ast.right)
        }

        return opToFn(ast.name, ast.left, ast.right)
      }
      default:
        throw new Error(`Unsupported operator ${ast.name}`)
    }
  }

  throw new Error(`Cannot exec ast: ${ast}`)
}

const ast = buildAST(map, 'a')
const result = exec(ast)

console.log(result)
